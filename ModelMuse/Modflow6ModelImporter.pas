unit Modflow6ModelImporter;

interface

uses
  System.Classes, System.IOUtils, Vcl.Dialogs, System.SysUtils, System.UITypes,
  Mf6.SimulationNameFileReaderUnit, System.Math, Mf6.CustomMf6PersistentUnit,
  ScreenObjectUnit, DataSetUnit, System.Generics.Collections;

  // The first name in NameFiles must be the name of the groundwater flow
  // simulation name file (mfsim.nam). Any additional names must be associated
  // transport simulation name files (mfsim.nam)

type
  TModflow6Importer = class(TObject)
  private
    FErrorMessages: TStringList;
    FSimulation: TMf6Simulation;
    FFlowModel: TModel;
    FAllTopCellsScreenObject: TScreenObject;
    FModelNameFile: string;
    procedure ImportFlowModelTiming;
    procedure ImportSimulationOptions;
    procedure ImportSolutionGroups;
    function ImportFlowModel: Boolean;
    procedure ImportDis(Package: TPackage);
    procedure ImportDisV(Package: TPackage);
    procedure UpdateLayerStructure(NumberOfLayers: Integer);
    procedure CreateAllTopCellsScreenObject;
    function GetAllTopCellsScreenObject: TScreenObject;
    property AllTopCellsScreenObject: TScreenObject read GetAllTopCellsScreenObject;
    procedure AssignRealValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: TDArray2D);
    procedure AssignIntegerValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: TIArray2D);
    procedure AssignBooleanValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: TBArray2D);
    procedure AssignIDomain(IDOMAIN: TIArray3D; NumberOfLayers: Integer);
    procedure AssignBOTM(BOTM: TDArray3D);
    procedure AssignTOP(TOP: TDArray2D);
    procedure ImportIc(Package: TPackage);
    procedure Assign3DRealDataSet(DsName: string; Data: TDArray3D);
    procedure Assign3DIntegerDataSet(DsName: string; Data: TIArray3D);
    procedure ImportOc(Package: TPackage);
    procedure ImportGwfObs(Package: TPackage);
    procedure ImportNpf(Package: TPackage);
    procedure ImportTvk(Package: TPackage);
  public
    procedure ImportModflow6Model(NameFiles, ErrorMessages: TStringList);
  end;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, GoPhastTypes, frmSelectFlowModelUnit,
  Mf6.TDisFileReaderUnit, ModflowTimeUnit, ModflowOptionsUnit,
  Mf6.AtsFileReaderUnit, ModflowPackageSelectionUnit, ModflowOutputControlUnit,
  Mf6.NameFileReaderUnit, Mf6.DisFileReaderUnit, LayerStructureUnit,
  UndoItems, FastGEO, AbstractGridUnit, ValueArrayStorageUnit,
  InterpolationUnit, GIS_Functions, RbwParser, DataSetNamesUnit,
  Mf6.DisvFileReaderUnit, ModflowIrregularMeshUnit, Mf6.IcFileReaderUnit,
  Mf6.OcFileReaderUnit, Mf6.ObsFileReaderUnit, Modflow6ObsUnit,
  Mf6.NpfFileReaderUnit, Mf6.TvkFileReaderUnit, ModflowTvkUnit;

resourcestring
  StrTheNameFileSDoe = 'The name file %s does not exist.';

procedure TModflow6Importer.AssignBooleanValuesToCellCenters(
  DataArray: TDataArray; ScreenObject: TScreenObject; ImportedData: TBArray2D);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesB
    + '("' + DataArray.Name + '")';
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtBoolean;
  ImportedValues.Values.Count := Model.RowCount * Model.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
//      APoint := Grid.TwoDElementCenter(ColIndex, RowIndex);
//      if (FImporter.FImportParameters.Outline = nil)
//        or FImporter.FImportParameters.Outline.PointInside(APoint) then
      begin
        ImportedValues.Values.BooleanValues[PointIndex] :=
          ImportedData[RowIndex, ColIndex];;
        Inc(PointIndex);
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.Values.CacheData;
end;

procedure TModflow6Importer.AssignIntegerValuesToCellCenters(
  DataArray: TDataArray; ScreenObject: TScreenObject; ImportedData: TIArray2D);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesI;
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtInteger;
  ImportedValues.Values.Count := Model.RowCount * Model.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
//      APoint := Grid.TwoDElementCenter(ColIndex, RowIndex);
//      if (FImporter.FImportParameters.Outline = nil)
//        or FImporter.FImportParameters.Outline.PointInside(APoint) then
      begin
        ImportedValues.Values.IntValues[PointIndex] :=
          ImportedData[RowIndex, ColIndex];
        Inc(PointIndex);
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.Values.CacheData;
end;

procedure TModflow6Importer.AssignRealValuesToCellCenters(DataArray: TDataArray;
  ScreenObject: TScreenObject; ImportedData: TDArray2D);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + DataArray.Name + '")';
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := Model.RowCount * Model.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
//      APoint := Grid.TwoDElementCenter(ColIndex, RowIndex);
//      if (FImporter.FImportParameters.Outline = nil)
//        or FImporter.FImportParameters.Outline.PointInside(APoint) then
      begin
        ImportedValues.Values.RealValues[PointIndex] :=
          ImportedData[RowIndex, ColIndex];
        Inc(PointIndex);
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.Values.CacheData;
end;

procedure TModflow6Importer.CreateAllTopCellsScreenObject;
var
  UndoCreateScreenObject: TCustomUndo;
  RowIndex: Integer;
  ColIndex: Integer;
  APoint: TPoint2D;
//  UpdateOutline: Boolean;
//  FoundFirst: Boolean;
  Model: TPhastModel;
//  Grid: TCustomModelGrid;
begin
  Assert(FAllTopCellsScreenObject = nil);
    Model := frmGoPhast.PhastModel;
//    Grid := Model.Grid;
    FAllTopCellsScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    FAllTopCellsScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(FAllTopCellsScreenObject);
    FAllTopCellsScreenObject.ElevationCount := ecZero;
//    if FImporter.FImportParameters.AssignmentMethod = camInterpolate then
//    begin
//      FAllTopCellsScreenObject.SetValuesByInterpolation := True;
//    end
//    else
//    begin
      FAllTopCellsScreenObject.SetValuesOfIntersectedCells := True;
//    end;
    FAllTopCellsScreenObject.EvaluatedAt := eaBlocks;
    FAllTopCellsScreenObject.Visible := False;
    FAllTopCellsScreenObject.Capacity := Model.RowCount * Model.ColumnCount;
//    UpdateOutline := (FImporter.FImportParameters.Outline <> nil)
//      and (FImporter.FImportParameters.FirstCol = 0)
//      and (FImporter.FImportParameters.LastCol = 0)
//      and (FImporter.FImportParameters.FirstRow = 0)
//      and (FImporter.FImportParameters.LastRow = 0);
//    FoundFirst := False;
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        APoint := Model.TwoDElementCenter(ColIndex, RowIndex);

//        if (FImporter.FImportParameters.Outline = nil)
//          or FImporter.FImportParameters.Outline.PointInside(APoint) then
        begin
          FAllTopCellsScreenObject.AddPoint(APoint, True);
//          if UpdateOutline then
//          begin
//            if FoundFirst then
//            begin
//              if FImporter.FImportParameters.FirstCol > ColIndex + 1 then
//              begin
//                FImporter.FImportParameters.FirstCol := ColIndex + 1;
//              end;
//              if FImporter.FImportParameters.LastCol < ColIndex + 1 then
//              begin
//                FImporter.FImportParameters.LastCol := ColIndex + 1;
//              end;
//              if FImporter.FImportParameters.FirstRow > RowIndex + 1 then
//              begin
//                FImporter.FImportParameters.FirstRow := RowIndex + 1;
//              end;
//              if FImporter.FImportParameters.LastRow < RowIndex + 1 then
//              begin
//                FImporter.FImportParameters.LastRow := RowIndex + 1;
//              end;
//            end
//            else
//            begin
//              FoundFirst := True;
//              FImporter.FImportParameters.FirstCol := ColIndex + 1;
//              FImporter.FImportParameters.LastCol := ColIndex + 1;
//              FImporter.FImportParameters.FirstRow := RowIndex + 1;
//              FImporter.FImportParameters.LastRow := RowIndex + 1;
//            end;
//          end;
        end;
      end;
    end;
    FAllTopCellsScreenObject.Name := 'Imported_Arrays';
    FAllTopCellsScreenObject.SectionStarts.CacheData;
end;

function TModflow6Importer.GetAllTopCellsScreenObject: TScreenObject;
begin
  if FAllTopCellsScreenObject = nil then
  begin
    CreateAllTopCellsScreenObject;
  end;
  result := FAllTopCellsScreenObject;
end;

procedure TModflow6Importer.ImportDis(Package: TPackage);
var
  Dis: TDis;
  XOrigin: Extended;
  YOrigin: Extended;
  GridAngle: Extended;
  Model: TPhastModel;
  MfOptions: TModflowOptions;
  ColumnPositions: TOneDRealArray;
  RowPositions: TOneDRealArray;
  Delr: TDArray1D;
  Position: Extended;
  ColIndex: Integer;
  Delc: TDArray1D;
  AngleToLL: Extended;
  DistanceToLL: Extended;
  RowIndex: Integer;
  TOP: TDArray2D;
  BOTM: TDArray3D;
  NumberOfLayers: Integer;
  IDOMAIN: TIArray3D;
begin
  Model := frmGoPhast.PhastModel;
  MfOptions := Model.ModflowOptions;

  Dis := Package.Package as TDis;
  MfOptions.LengthUnit := Dis.Options.LENGTH_UNITS;
  MfOptions.WriteBinaryGridFile := not Dis.Options.NOGRB;

  XOrigin := Dis.Options.XORIGIN;
  YOrigin := Dis.Options.YORIGIN;
  GridAngle := Dis.Options.ANGROT * Pi / 180;

  Delr := Dis.GridData.DELR;
  SetLength(ColumnPositions, Length(Delr) + 1);
  Delc := Dis.GridData.DELC;
  SetLength(RowPositions, Length(Delc) + 1);

  if GridAngle = 0 then
  begin
    Position := XOrigin;
    ColumnPositions[0] := Position;
    for ColIndex := 0 to Length(Delr) - 1 do
    begin
      Position := Position + Delr[ColIndex];
      ColumnPositions[ColIndex+1] := Position;
    end;

    Position := YOrigin;
    RowPositions[Length(RowPositions)-1] := Position;
    for RowIndex := 0 to Length(Delc) - 1 do
    begin
      Position := Position + Delc[RowIndex];
      RowPositions[Length(RowPositions) - RowIndex -2] := Position;
    end;
  end
  else
  begin
    AngleToLL := ArcTan2(YOrigin, XOrigin);
    DistanceToLL := Sqrt(Sqr(XOrigin) + Sqr(YOrigin));

    Position := DistanceToLL * Cos(AngleToLL - GridAngle);
    ColumnPositions[0] := Position;
    for ColIndex := 0 to Length(Delr) - 1 do
    begin
      Position := Position + Delr[ColIndex];
      ColumnPositions[ColIndex+1] := Position;
    end;

    Position := DistanceToLL * Sin(AngleToLL - GridAngle);
    RowPositions[Length(RowPositions)-1] := Position;
    for RowIndex := 0 to Length(Delc) - 1 do
    begin
      Position := Position + Delc[RowIndex];
      RowPositions[Length(RowPositions) - RowIndex - 2] := Position;
    end;
  end;

  TOP := Dis.GridData.TOP;
  BOTM  := Dis.GridData.BOTM;
  IDOMAIN := Dis.GridData.IDOMAIN;
  NumberOfLayers := Length(BOTM);
  UpdateLayerStructure(NumberOfLayers);

  Model.ModflowGrid.BeginGridChange;
  try
    Model.ModflowGrid.GridAngle := GridAngle;
    Model.ModflowGrid.ColumnPositions := ColumnPositions;
    Model.ModflowGrid.RowPositions := RowPositions;
  finally
    Model.ModflowGrid.EndGridChange;
  end;
  AssignTOP(TOP);
  AssignBOTM(BOTM);
  AssignIDomain(IDOMAIN, NumberOfLayers);

end;

procedure TModflow6Importer.ImportDisV(Package: TPackage);
var
  Model: TPhastModel;
  MfOptions: TModflowOptions;
  Disv: TDisv;
  XOrigin: Extended;
  YOrigin: Extended;
  GridAngle: Extended;
  Mesh3D: TModflowDisvGrid;
  Mesh2D: TModflowIrregularGrid2D;
  CellCorners: TModflowNodes;
  Verticies: TDisvVertices;
  Index: Integer;
  Vertex: TVertex;
  Node: TModflowNode;
  Cells: TDisvCells;
  ModelCells: TModflowIrregularCell2DCollection;
  Cell: TDisvCell;
  IrregularCell: TModflowIrregularCell2D;
  NodeIndex: Integer;
  NodeNumber: Integer;
  ModelNode: TIntegerItem;
  TOP: TDArray2D;
  BOTM: TDArray3D;
  IDOMAIN: TIArray3D;
  APoint: TPoint2D;
  NumberOfLayers: Integer;
  function ConvertLocation(X, Y: Extended): TPoint2D;
  begin
    if GridAngle = 0 then
    begin
      result.x := XOrigin + X;
      result.Y := YOrigin + Y;
    end
    else
    begin
      result.x := XOrigin + X * Cos(GridAngle);
      result.Y := YOrigin + Y * Sin(GridAngle);
    end;
  end;
begin
  Model := frmGoPhast.PhastModel;
  MfOptions := Model.ModflowOptions;

  Model.Mf6GridType := mgtLayered;

  Disv := Package.Package as TDisv;
  MfOptions.LengthUnit := Disv.Options.LENGTH_UNITS;
  MfOptions.WriteBinaryGridFile := not Disv.Options.NOGRB;

  XOrigin := Disv.Options.XORIGIN;
  YOrigin := Disv.Options.YORIGIN;
  GridAngle := Disv.Options.ANGROT * Pi / 180;
  
  NumberOfLayers := Disv.Dimensions.NLay;
  UpdateLayerStructure(NumberOfLayers);

  Mesh3D := Model.DisvGrid;
  Mesh2D := Mesh3D.TwoDGrid;
  CellCorners := Mesh2D.CellCorners;
  
  Verticies := Disv.Verticies;
  CellCorners.Capacity := Verticies.Count;
  for Index := 0 to Verticies.Count - 1 do
  begin
    Vertex := Verticies[Index];
    Node := CellCorners.Add;
    APoint := ConvertLocation(Vertex.xv, Vertex.yv);
    Node.X := APoint.x;
    Node.Y := APoint.y;
    Node.Number := Vertex.iv -1; 
  end;

  Cells := Disv.Cells;
  ModelCells := Mesh2D.Cells;

  ModelCells.Capacity := Cells.Count;
  for Index := 0 to Cells.Count - 1 do
  begin
    Cell := Cells[Index];
    IrregularCell := ModelCells.Add;
    IrregularCell.ElementNumber := Cell.icell2d -1;
    APoint := ConvertLocation(Cell.xc, Cell.yc);
    IrregularCell.X := APoint.x;
    IrregularCell.Y := APoint.y; 
    IrregularCell.NodeNumbers.Capacity := Cell.ncvert;
    for NodeIndex := 0 to Cell.ncvert - 1 do
    begin
      NodeNumber := Cell.icvert[NodeIndex] -1;
      ModelNode := IrregularCell.NodeNumbers.Add;
      ModelNode.Value := NodeNumber;
    end;
  end;

  Mesh3D.Loaded;

  TOP := Disv.GridData.TOP;
  BOTM := Disv.GridData.BOTM;
  IDOMAIN := Disv.GridData.IDOMAIN;
  
  AssignTOP(TOP);
  AssignBOTM(BOTM);
  AssignIDomain(IDOMAIN, NumberOfLayers);

end;

function TModflow6Importer.ImportFlowModel: Boolean;
var
  NameFile: TFlowNameFile;
  Options: TFlowNameFileOptions;
  Packages: TFlowPackages;
  Model: TPhastModel;
  MfOptions: TModflowOptions;
  OC: TModflowOutputControl;
  PackageIndex: Integer;
  APackage: TPackage;
begin
  result := True;
  if FFlowModel <> nil then
  begin
    Model := frmGoPhast.PhastModel;

    NameFile := FFlowModel.FName as TFlowNameFile;

    Options := NameFile.NfOptions;
    MfOptions := Model.ModflowOptions;
    MfOptions.NewtonMF6 := Options.NEWTON;
    MfOptions.UnderRelaxationMF6 := Options.UNDER_RELAXATION;
    if Options.PRINT_INPUT then
    begin
      OC := Model.ModflowOutputControl;
      OC.PrintInputCellLists := True;
      OC.PrintInputArrays := True;
    end;

    Packages := NameFile.NfPackages;

    for PackageIndex := 0 to Packages.Count - 1 do
    begin
      APackage := Packages[PackageIndex];
      if APackage.FileType = 'DIS6' then
      begin
        ImportDis(APackage);
        break
      end
      else if APackage.FileType = 'DISV6' then
      begin
        ImportDisV(APackage);
        break;
      end
      else if APackage.FileType = 'DISU6' then
      begin
        MessageDlg('ModelMuse can not import DISU models.', mtError, [mbOK], 0);
        result := False;
        Exit
      end
      else
      begin
        Continue;
      end;
    end;

    for PackageIndex := 0 to Packages.Count - 1 do
    begin
      APackage := Packages[PackageIndex];
      if (APackage.FileType = 'DIS6')
        or (APackage.FileType = 'DISV6')
        or (APackage.FileType = 'DISU6')
        then
      begin
        Continue;
      end;

      if APackage.FileType = 'IC6' then
      begin
        ImportIc(APackage);
      end
      else if APackage.FileType = 'OC6' then
      begin
        ImportOc(APackage)
      end
      else if APackage.FileType = 'OBS6' then
      begin
        ImportGwfObs(APackage)
      end
      else if APackage.FileType = 'NPF6' then
      begin
        ImportNpf(APackage);
      end
      else if APackage.FileType = 'HFB6' then
      begin
//        HfbReader := THfb.Create(APackage.FileType);
//        HfbReader.Dimensions := FDimensions;
//        APackage.Package := HfbReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'STO6' then
      begin
//        StoReader := TSto.Create(APackage.FileType);
//        StoReader.Dimensions := FDimensions;
//        APackage.Package := StoReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'CSUB6' then
      begin
//        CSubReader := TCSub.Create(APackage.FileType);
//        CSubReader.Dimensions := FDimensions;
//        APackage.Package := CSubReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'BUY6' then
      begin
//        BuyReader := TBuy.Create(APackage.FileType);
//        BuyReader.Dimensions := FDimensions;
//        APackage.Package := BuyReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'VSC6' then
      begin
//        VscReader := TVsc.Create(APackage.FileType);
//        VscReader.Dimensions := FDimensions;
//        APackage.Package := VscReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'CHD6' then
      begin
//        ChdReader := TChd.Create(APackage.FileType);
//        ChdReader.Dimensions := FDimensions;
//        APackage.Package := ChdReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'WEL6' then
      begin
//        WelReader := TWel.Create(APackage.FileType);
//        WelReader.Dimensions := FDimensions;
//        APackage.Package := WelReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'DRN6' then
      begin
//        DrnReader := TDrn.Create(APackage.FileType);
//        DrnReader.Dimensions := FDimensions;
//        APackage.Package := DrnReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'GHB6' then
      begin
//        GhbReader := TGhb.Create(APackage.FileType);
//        GhbReader.Dimensions := FDimensions;
//        APackage.Package := GhbReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'RIV6' then
      begin
//        RivReader := TRiv.Create(APackage.FileType);
//        RivReader.Dimensions := FDimensions;
//        APackage.Package := RivReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'RCH6' then
      begin
//        RchReader := TRch.Create(APackage.FileType);
//        RchReader.Dimensions := FDimensions;
//        APackage.Package := RchReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'EVT6' then
      begin
//        EvtReader := TEvt.Create(APackage.FileType);
//        EvtReader.Dimensions := FDimensions;
//        APackage.Package := EvtReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'MAW6' then
      begin
//        MawReader := TMaw.Create(APackage.FileType);
//        MawReader.Dimensions := FDimensions;
//        APackage.Package := MawReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'SFR6' then
      begin
//        SfrReader := TSfr.Create(APackage.FileType);
//        SfrReader.Dimensions := FDimensions;
//        APackage.Package := SfrReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'LAK6' then
      begin
//        LakReader := TLak.Create(APackage.FileType);
//        LakReader.Dimensions := FDimensions;
//        APackage.Package := LakReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'UZF6' then
      begin
//        UzfReader := TUzf.Create(APackage.FileType);
//        UzfReader.Dimensions := FDimensions;
//        APackage.Package := UzfReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'MVR6' then
      begin
//        MovReader := TMvr.Create(APackage.FileType);
//        APackage.Package := MovReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'GNC6' then
      begin
//        GncReader := TGnc.Create(APackage.FileType);
//        GncReader.Dimensions := FDimensions;
//        APackage.Package := GncReader;
//        APackage.ReadPackage(Unhandled);
      end
      else if APackage.FileType = 'GWF6-GWF6' then
      begin
//        GwfGwfReader := TGwfGwf.Create(APackage.FileType);
//        GwfGwfReader.Dimensions := FDimensions;
//        GwfGwfReader.FDimensions2 := FDimensions;
//        APackage.Package := GwfGwfReader;
//        APackage.ReadPackage(Unhandled);
      end

    end;

  end;
end;

procedure TModflow6Importer.ImportFlowModelTiming;
var
  PhastModel: TPhastModel;
  MFStressPeriods: TModflowStressPeriods;
  StressPeriods: TTDis;
  MfOptions: TModflowOptions;
  TimeUnits: string;
  ValidUnits: TStringList;
  MfTimeUnit: Integer;
  SPIndex: Integer;
  SPData: TPeriod;
  MfStressPeriod: TModflowStressPeriod;
  StartTime: double;
  AtsIndex: Integer;
  AtsPeriod: TAtsPeriod;
begin
  StressPeriods := FSimulation.Timing.TDis;


  PhastModel := frmGoPhast.PhastModel;
  MfOptions := PhastModel.ModflowOptions;
  TimeUnits := UpperCase(StressPeriods.Options.TimeUnits);
  if StressPeriods.Options.StartDate <> '' then
  begin
    MfOptions.Description.Add('Start Date = ' + StressPeriods.Options.StartDate);
    FErrorMessages.Add('Warning: The start date of the model has been added as a comment to the model description')
  end;

  ValidUnits := TStringList.Create;
  try
    ValidUnits.Add('UNKNOWN');
    ValidUnits.Add('SECONDS');
    ValidUnits.Add('MINUTES');
    ValidUnits.Add('HOURS');
    ValidUnits.Add('DAYS');
    ValidUnits.Add('YEARS');
    MfTimeUnit := ValidUnits.IndexOf(TimeUnits);
    if MfTimeUnit < 0 then
    begin
      MfTimeUnit := 0;
    end;
    MfOptions.TimeUnit := MfTimeUnit;
  finally
    ValidUnits.Free;
  end;


  MFStressPeriods := PhastModel.ModflowStressPeriods;
  MFStressPeriods.Capacity := StressPeriods.Dimensions.NPER;
  StartTime := 0.0;
  for SPIndex := 0 to StressPeriods.PeriodData.Count - 1 do
  begin
    SPData := StressPeriods.PeriodData[SPIndex];
    MfStressPeriod := MFStressPeriods.Add;
    MfStressPeriod.StartTime := StartTime;
    StartTime := StartTime + SPData.PerLen;
    MfStressPeriod.EndTime := StartTime;
    MfStressPeriod.PeriodLength := SPData.PerLen;
    MfStressPeriod.TimeStepMultiplier := SPData.TSMult;

    if SPData.NSTP > 1 then
    begin
      if SPData.TSMULT = 1 then
      begin
        MfStressPeriod.MaxLengthOfFirstTimeStep :=
          SPData.PERLEN / SPData.NSTP;
      end
      else
      begin
        MfStressPeriod.MaxLengthOfFirstTimeStep :=
          SPData.PERLEN * (SPData.TSMULT - 1)
          / (IntPower(SPData.TSMULT, SPData.NSTP) - 1);
      end;
    end
    else
    begin
      MfStressPeriod.MaxLengthOfFirstTimeStep := MfStressPeriod.PeriodLength;
    end;
  end;

  if StressPeriods.Ats <> nil then
  begin
    for AtsIndex := 0 to StressPeriods.Ats.Count - 1 do
    begin
      AtsPeriod := StressPeriods.Ats.AtsPeriod[AtsIndex];
      if AtsPeriod.iperats <= 0 then
      begin
        FErrorMessages.Add('ATS period data for iperats <= 0 is skipped ')
      end
      else if AtsPeriod.iperats > MFStressPeriods.Count then
      begin
        FErrorMessages.Add('ATS period data for iperats > NPER is skipped ')
      end
      else
      begin
        MfStressPeriod := MFStressPeriods[AtsPeriod.iperats-1];
        MfStressPeriod.AtsUsed := True;
        MfStressPeriod.AtsInitialStepSize := AtsPeriod.dt0;
        MfStressPeriod.AtsMinimumStepSize := AtsPeriod.dtmin;
        MfStressPeriod.AtsMaximumStepSize := AtsPeriod.dtmax;
        MfStressPeriod.AtsAdjustmentFactor := AtsPeriod.dtadj;
        MfStressPeriod.AtsFailureFactor := AtsPeriod.dtfailadj;
      end;
    end;
  end;
end;

procedure TModflow6Importer.ImportGwfObs(Package: TPackage);
var
  Obs: TObs;
  Model: TPhastModel;
  Mf6ObservationUtility: TMf6ObservationUtility;
  FileIndex: Integer;
  ObsFile: TObsFile;
  ObsIndex: Integer;
  Observation: TObservation;
  ScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  Modflow6Obs: TModflow6Obs;
  APoint: TPoint2D;
  CellId: TCellId;
begin
  Obs := Package.Package as TObs;
  Model := frmGoPhast.PhastModel;
  Mf6ObservationUtility := Model.ModflowPackages.Mf6ObservationUtility;
  Mf6ObservationUtility.IsSelected := True;
  if Obs.Options.Digits > 0 then
  begin
    Mf6ObservationUtility.OutputFormat := ofText;
    Mf6ObservationUtility.Digits := Obs.Options.Digits;
  end
  else
  begin
    Mf6ObservationUtility.OutputFormat := ofBinary;
  end;

  for FileIndex := 0 to Obs.FileCount - 1 do
  begin
    ObsFile := Obs[FileIndex];
    for ObsIndex := 0 to ObsFile.Count - 1 do
    begin
      Observation := ObsFile[ObsIndex];

      if AnsiSameText(Observation.ObsType, 'head')
        or AnsiSameText(Observation.ObsType, 'drawdown') then
      begin

        ScreenObject := TScreenObject.CreateWithViewDirection(
          Model, vdTop, UndoCreateScreenObject, False);
        ScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

        Model.AddScreenObject(ScreenObject);
        ScreenObject.ElevationCount := ecOne;
        ScreenObject.SetValuesOfIntersectedCells := True;
        ScreenObject.EvaluatedAt := eaBlocks;
        ScreenObject.Visible := False;
        ScreenObject.Capacity := 1;

        CellId := Observation.CellId1;
        if Model.DisvUsed then
        begin
          Dec(CellId.Column);
          CellId.Row := 0;
        end
        else
        begin
          Dec(CellId.Column);
          Dec(CellId.Row);
        end;
        Assert(Observation.IdType1 = itCell);
        APoint := Model.TwoDElementCenter(CellId.Column, CellId.Row);
        ScreenObject.AddPoint(APoint, True);
        ScreenObject.ElevationFormula := Format('LayerCenter(%d)', [CellId.Layer]);

        ScreenObject.CreateMf6Obs;
        Modflow6Obs := ScreenObject.Modflow6Obs;
        Modflow6Obs.Name := Observation.ObsName;
        if AnsiSameText(Observation.ObsType, 'head') then
        begin
          Modflow6Obs.General := [ogHead];
        end
        else
        begin
          Modflow6Obs.General := [ogDrawdown];
        end;

      end
      else if AnsiSameText(Observation.ObsType, 'flow-ja-face') then
      begin
        FErrorMessages.Add(Format('ModelMuse could not import the observation "%s" because it is a flow-ja-face observation', [Observation.ObsName] ));
      end
      else
      begin
        FErrorMessages.Add(Format('ModelMuse could not import the observation "%s" because it is not a recognized type', [Observation.ObsName] ));
      end;

{
  TObservation = record
    ObsName: string;
    ObsType: string;
    IdType1: TIdType;
    IdType2: TIdType;
    CellId1: TCellId;
    CellId2: TCellId;
    Num1: Integer;
    Num2: Integer;
    FloatNum1: Extended;
    FloatNum2: Extended;
    Name1: string;
    Name2: string;
    procedure Initialize;
  end;
}
    end;
  end;
end;

procedure TModflow6Importer.ImportIc(Package: TPackage);
var
  IC: TIc;
begin
  IC := Package.Package as TIc;
  Assign3DRealDataSet(rsModflow_Initial_Head, IC.GridData.STRT);
end;

procedure TModflow6Importer.ImportModflow6Model(NameFiles, ErrorMessages: TStringList);
var
  FileIndex: Integer;
  OutFile: string;
  ListFile: TStringList;
  PhastModel: TPhastModel;
  ModelIndex: Integer;
  AModel: TModel;
  FlowModelNames: TStringList;
  FlowModelImported: Boolean;
  frmSelectFlowModel: TfrmSelectFlowModel;
  ExchangeIndex: Integer;
  Exchange: TExchange;
begin
  FErrorMessages := ErrorMessages;
  for FileIndex := 0 to NameFiles.Count - 1 do
  begin
    if not TFile.Exists(NameFiles[FileIndex]) then
    begin
      Beep;
      MessageDlg(Format(StrTheNameFileSDoe, [NameFiles[FileIndex]]), mtError, [mbOK], 0);
      Exit;
    end;
  end;
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.Clear;
  frmGoPhast.Caption := StrModelName;
  frmGoPhast.sdSaveDialog.FileName := '';
  PhastModel.ModelSelection := msModflow2015;

  FlowModelImported := False;
  for FileIndex := 0 to NameFiles.Count - 1 do
  begin
    FSimulation := TMf6Simulation.Create('Simulation');
    try
      FSimulation.ReadSimulation(NameFiles[FileIndex]);
      OutFile := ChangeFileExt(NameFiles[FileIndex], '.lst');
      if TFile.Exists(OutFile) then
      begin
        ListFile := TStringList.Create;
        try
          ListFile.LoadFromFile(OutFile);
          if ListFile.Count > 0 then
          begin
            ErrorMessages.Add('The following errors were encountered when reading ' + NameFiles[FileIndex]);
            ErrorMessages.AddStrings(ListFile);
            ErrorMessages.Add('');
          end;
        finally
          ListFile.Free;
        end;
      end
      else
      begin
        ErrorMessages.Add(OutFile + ' does not exist.')
      end;

      for ExchangeIndex := 0 to FSimulation.Exchanges.Count - 1 do
      begin
        Exchange := FSimulation.Exchanges[ExchangeIndex];
        if not AnsiSameText(Exchange.ExchangeType, 'GWF6-GWT6') then
        begin
          ErrorMessages.Add('The following error was encountered when reading ' + NameFiles[FileIndex]);
          ErrorMessages.Add('ModelMuse does not currently support MODFLOW 6 exchanges');
          break;
        end;
      end;

      FlowModelNames := TStringList.Create;
      try
        for ModelIndex := 0 to FSimulation.Models.Count - 1 do
        begin
          AModel := FSimulation.Models[ModelIndex];
          if AModel.ModelType = 'GWF6' then
          begin
            FlowModelNames.Add(AModel.NameFile)
          end;
        end;
        if FlowModelImported and (FlowModelNames.Count > 0) then
        begin
          ErrorMessages.Add('The following error was encountered when reading ' + NameFiles[FileIndex]);
          ErrorMessages.Add('Another flow model name file was already in another simulation name file');
          Exit;
        end;
        FModelNameFile := '';
        if FlowModelNames.Count > 1 then
        begin
          frmSelectFlowModel := TfrmSelectFlowModel.Create(nil);
          try
            frmSelectFlowModel.rgFlowModels.Items := FlowModelNames;
            frmSelectFlowModel.rgFlowModels.ItemIndex := 0;
            if frmSelectFlowModel.ShowModal = mrOK then
            begin
              FModelNameFile := frmSelectFlowModel.rgFlowModels.Items[frmSelectFlowModel.rgFlowModels.ItemIndex];
            end
            else
            begin
              Exit;
            end;
          finally
            frmSelectFlowModel.Free
          end;
        end
        else
        begin
          FModelNameFile := FlowModelNames[0];
        end;
        if FModelNameFile <> '' then
        begin
          FFlowModel := FSimulation.Models.GetModelByNameFile(FModelNameFile);
        end
        else
        begin
          FFlowModel := nil;
        end;
        ImportSimulationOptions;
        ImportFlowModelTiming;
        ImportSolutionGroups;
        if not ImportFlowModel then
        begin
          Exit;
        end;

      finally
        FlowModelNames.Free
      end;

    finally
      FSimulation.Free;
      FSimulation := nil;
    end;
  end;
  PhastModel.Exaggeration := frmGoPhast.DefaultVE;
  frmGoPhast.RestoreDefault2DView1Click(nil);
end;

procedure TModflow6Importer.ImportNpf(Package: TPackage);
var
  Npf: TNpf;
  Model: TPhastModel;
  NpfPackage: TNpfPackage;
  Options: TNpfOptions;
  GridData: TNpfGridData;
  DataArray: TDataArray;
  TvkIndex: Integer;
begin
  Model := frmGoPhast.PhastModel;
  Npf := Package.Package as TNpf;

  NpfPackage := Model.ModflowPackages.NpfPackage;
  Options := Npf.Options;
  if Options.ALTERNATIVE_CELL_AVERAGING <> '' then
  begin
    if Options.ALTERNATIVE_CELL_AVERAGING = 'LOGARITHMIC' then
    begin
      NpfPackage.CellAveraging := caLogarithmic;
    end
    else if Options.ALTERNATIVE_CELL_AVERAGING = 'AMT-LMK' then
    begin
      NpfPackage.CellAveraging := caArithLog;
    end
    else if Options.ALTERNATIVE_CELL_AVERAGING = 'AMT-HMK' then
    begin
      NpfPackage.CellAveraging := caArithHarm;
    end
    else
    begin
      FErrorMessages.Add(Format('Unrecognized ALTERNATIVE_CELL_AVERAGING option %s in NPF package',
        [Options.ALTERNATIVE_CELL_AVERAGING]))
    end;
  end;
  NpfPackage.UseSaturatedThickness := Options.THICKSTRT;
  NpfPackage.TimeVaryingVerticalConductance := Options.VARIABLECV;
  NpfPackage.Dewatered := Options.DEWATERED;
  NpfPackage.Perched := Options.PERCHED;
  Model.ModflowWettingOptions.WettingActive := Options.REWET.Used;
  if Options.REWET.Used then
  begin
    Model.ModflowWettingOptions.WettingFactor := Options.REWET.WETFCT;
    Model.ModflowWettingOptions.WettingIterations := Options.REWET.IWETIT;
    Model.ModflowWettingOptions.WettingEquation := Options.REWET.IHDWET;
  end;
  NpfPackage.UseXT3D := Options.XT3D;
  NpfPackage.Xt3dOnRightHandSide := Options.RHS;
  NpfPackage.SaveSpecificDischarge := Options.SAVE_SPECIFIC_DISCHARGE;
  NpfPackage.SaveSaturation := Options.SAVE_SATURATION;
  NpfPackage.UseHorizontalAnisotropy := Options.K22OVERK;
  NpfPackage.UseVerticalAnisotropy := Options.K33OVERK;

  GridData := Npf.GridData;
  Assign3DIntegerDataSet(KCellType, GridData.ICELLTYPE);

  Assign3DRealDataSet(rsKx, GridData.K);

  if GridData.K22 <> nil then
  begin
    if NpfPackage.UseHorizontalAnisotropy then
    begin
      Assign3DRealDataSet(KKyOverKx, GridData.K22);
    end
    else
    begin
      Assign3DRealDataSet(rsKy, GridData.K22);
    end;
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKy);
    DataArray.Formula := rsKx;
  end;

  if GridData.K33 <> nil then
  begin
    if NpfPackage.UseVerticalAnisotropy then
    begin
      Assign3DRealDataSet(KKzOverKx, GridData.K33);
    end
    else
    begin
      Assign3DRealDataSet(rsKz, GridData.K33);
    end;
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKz);
    DataArray.Formula := rsKx;
  end;

  if GridData.ANGLE1 <> nil then
  begin
    Assign3DRealDataSet(KXT3DAngle1, GridData.ANGLE1);
  end;

  if GridData.ANGLE2 <> nil then
  begin
    Assign3DRealDataSet(KXT3DAngle2, GridData.ANGLE2);
  end;

  if GridData.ANGLE3 <> nil then
  begin
    Assign3DRealDataSet(KXT3DAngle3, GridData.ANGLE3);
  end;

  if GridData.WETDRY <> nil then
  begin
    Assign3DRealDataSet(rsWetDry, GridData.WETDRY);
  end;

  if Npf.Count > 0 then
  begin
    Model.ModflowPackages.TvkPackage.IsSelected := True;
    for TvkIndex := 0 to Npf.Count - 1 do
    begin
      ImportTvk(Npf[TvkIndex])
    end;
  end;
end;

procedure TModflow6Importer.ImportOc(Package: TPackage);
var
  OC: TOc;
  Model: TPhastModel;
  OutputControl: TModflowOutputControl;
begin
  OC := Package.Package as TOc;
  Model := frmGoPhast.PhastModel;
  OutputControl := Model.ModflowOutputControl;

  if OC.Options.BudgetFile then
  begin
    OutputControl.SaveCellFlows := csfBinary
  end;
  if OC.Options.BudgetCsvFile then
  begin
    OutputControl.SaveBudgetCSV := True
  end;
  if OC.Options.HeadFile then
  begin
    OutputControl.HeadOC.SaveInExternalFile := True
  end;
  if OC.Options.ConcentrationFile then
  begin
    OutputControl.ConcentrationOC.SaveInExternalFile := True
  end;
end;

procedure TModflow6Importer.Assign3DIntegerDataSet(DsName: string;
  Data: TIArray3D);
var
  Formula: string;
  Model: TPhastModel;
  LayerIndex: Integer;
  FirstValue: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Uniform: Boolean;
  DataArrayName: string;
  DataArray: TDataArray;
  Interpolator: TNearestPoint2DInterpolator;
  ScreenObject: TScreenObject;
begin
  Formula := 'CaseI(Layer';
  Model := frmGoPhast.PhastModel;
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstValue := Data[LayerIndex - 1, 0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstValue = Data[LayerIndex - 1, RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Format('Imported_%s_%d', [DsName, LayerIndex]);
    Formula := Formula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
      DataArrayName, '0', DataArrayName, [dcType], rdtInteger, eaBlocks, dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s', [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      DataArray.Formula := IntToStr(FirstValue);
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignIntegerValuesToCellCenters(DataArray, ScreenObject, Data[LayerIndex - 1]);
    end;
  end;
  Formula := Formula + ')';
  Formula := Format('IfI(Layer > %d, 0, %s)', [Model.LayerCount, Formula]);
  DataArray := Model.DataArrayManager.GetDataSetByName(DsName);
  DataArray.Formula := Formula;
end;

procedure TModflow6Importer.Assign3DRealDataSet(DsName: string; Data: TDArray3D);
var
  Formula: string;
  Model: TPhastModel;
  LayerIndex: Integer;
  FirstValue: Double;
  RowIndex: Integer;
  ColIndex: Integer;
  Uniform: Boolean;
  DataArrayName: string;
  DataArray: TDataArray;
  Interpolator: TNearestPoint2DInterpolator;
  ScreenObject: TScreenObject;
begin
  Formula := 'CaseR(Layer';
  Model := frmGoPhast.PhastModel;
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstValue := Data[LayerIndex - 1, 0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstValue = Data[LayerIndex - 1, RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Format('Imported_%s_%d', [DsName, LayerIndex]);
    Formula := Formula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
      DataArrayName, '0', DataArrayName, [dcType], rdtDouble, eaBlocks, dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s', [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      DataArray.Formula := FortranFloatToStr(FirstValue);
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignRealValuesToCellCenters(DataArray, ScreenObject, Data[LayerIndex - 1]);
    end;
  end;
  Formula := Formula + ')';
  Formula := Format('IfR(Layer > %d, 1, %s)', [Model.LayerCount, Formula]);
  DataArray := Model.DataArrayManager.GetDataSetByName(DsName);
  DataArray.Formula := Formula;
end;

procedure TModflow6Importer.AssignTOP(TOP: TDArray2D);
var
  Model: TPhastModel; 
  ColIndex: Integer; 
  RowIndex: Integer;
  Uniform: Boolean;
  FirstValue: Double;
  DataArrayName: string;
  DataArray: TDataArray;
  ScreenObject: TScreenObject;
begin
  Model := frmGoPhast.PhastModel;
  Uniform := True;
  FirstValue := TOP[0, 0];
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
      Uniform := FirstValue = TOP[RowIndex, ColIndex];
      if not Uniform then
      begin
        break;
      end;
    end;
  end;
  DataArrayName := Model.LayerStructure[0].DataArrayName;
  DataArray := Model.DataArrayManager.GetDataSetByName(DataArrayName);
  if Uniform then
  begin
    DataArray.Formula := FortranFloatToStr(FirstValue);
  end
  else
  begin
    ScreenObject := AllTopCellsScreenObject;
    AssignRealValuesToCellCenters(DataArray, ScreenObject, TOP);
  end;
end;

procedure TModflow6Importer.AssignBOTM(BOTM: TDArray3D);
var
  Model: TPhastModel; 
  LayerIndex: Integer;
  Uniform: Boolean; 
  FirstValue: Double; 
  DataArrayName: string; 
  DataArray: TDataArray; 
  ScreenObject: TScreenObject;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Model := frmGoPhast.PhastModel;
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstValue := BOTM[LayerIndex - 1, 0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstValue = BOTM[LayerIndex - 1, RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Model.LayerStructure[LayerIndex].DataArrayName;
    DataArray := Model.DataArrayManager.GetDataSetByName(DataArrayName);
    if Uniform then
    begin
      DataArray.Formula := FortranFloatToStr(FirstValue);
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignRealValuesToCellCenters(DataArray, ScreenObject, BOTM[LayerIndex - 1]);
    end;
  end;
end;

procedure TModflow6Importer.AssignIDomain(IDOMAIN: TIArray3D; NumberOfLayers: Integer);
var
  Uniform: Boolean; 
  DataArrayName: string; 
  DataArray: TDataArray; 
  ScreenObject: TScreenObject;
  Model: TPhastModel; 
  ColIndex: Integer; 
  RowIndex: Integer; 
  LayerIndex: Integer;
  IDomainFormula: string;
  FirstIntValue: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  ActiveFormula: string;
  Active: TBArray2D;
  FirstBoolValue: Boolean;
begin
  Model := frmGoPhast.PhastModel;
  if IDOMAIN = nil then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    DataArray.Formula := '1';
    DataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    DataArray.Formula := 'True';
    Exit;
  end;
  IDomainFormula := 'CaseI(Layer';
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstIntValue := IDOMAIN[LayerIndex - 1, 0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstIntValue = IDOMAIN[LayerIndex - 1, RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Format('Imported_IDOMAIN_%d', [LayerIndex]);
    IDomainFormula := IDomainFormula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
      DataArrayName, '0', DataArrayName, [dcType], rdtInteger, eaBlocks,
      dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s',
      [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      DataArray.Formula := IntToStr(FirstIntValue);
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignIntegerValuesToCellCenters(DataArray, ScreenObject, IDOMAIN[LayerIndex - 1]);
    end;
  end;
  IDomainFormula := IDomainFormula + ')';
  IDomainFormula := Format('IfI(Layer > %d, 1, %s)', [NumberOfLayers, IDomainFormula]);
  DataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  DataArray.Formula := IDomainFormula;

  ActiveFormula := 'CaseB(Layer';
  SetLength(Active, Model.RowCount, Model.ColumnCount);
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Active[RowIndex, ColIndex] := IDOMAIN[LayerIndex - 1, RowIndex, ColIndex] <> 0;
      end;
    end;
    Uniform := True;
    FirstBoolValue := Active[0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstBoolValue = Active[RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Format('Imported_Active_%d', [LayerIndex]);
    ActiveFormula := ActiveFormula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray, 
      DataArrayName, 'True', DataArrayName, [dcType], rdtBoolean, eaBlocks, 
      dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s', 
      [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      if FirstBoolValue then
      begin
        DataArray.Formula := 'True';
      end
      else
      begin
        DataArray.Formula := 'False';
      end;
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignBooleanValuesToCellCenters(DataArray, ScreenObject, Active);
    end;
  end;
  ActiveFormula := ActiveFormula + ')';
  ActiveFormula := Format('IfB(Layer > %d, True, %s)', [NumberOfLayers, ActiveFormula]);
  DataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  DataArray.Formula := ActiveFormula;
end;

procedure TModflow6Importer.UpdateLayerStructure(NumberOfLayers: Integer);
var
  Model: TPhastModel;
  TopLayer: TLayerGroup;
  LayerIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  Model := frmGoPhast.PhastModel;
  Model.LayerStructure.BeginUpdate;
  try
    TopLayer := Model.LayerStructure.Add;
    TopLayer.AquiferName := kModelTop;
    for LayerIndex := 1 to NumberOfLayers do
    begin
      LayerGroup := Model.LayerStructure.Add;
      LayerGroup.AquiferName := Format('Layer %d', [LayerIndex]);
    end;
    Model.ModflowGrid.LayerCount := NumberOfLayers;
    Model.DisvGrid.LayerCount := NumberOfLayers;
  finally
    Model.LayerStructure.EndUpdate;
  end;
end;

procedure TModflow6Importer.ImportSimulationOptions;
var
  Model: TPhastModel;
  SmsPkg: TSmsPackageSelection;
  OC: TModflowOutputControl;
begin
  Model := frmGoPhast.PhastModel;
  SmsPkg := Model.ModflowPackages.SmsPackage;
  SmsPkg.ContinueModel := FSimulation.Options.ContinueOption;
  if FSimulation.Options.NoCheckOption then
  begin
    SmsPkg.CheckInput := ciDontCheck
  end;
  case FSimulation.Options.MemPrint of
    Mf6.SimulationNameFileReaderUnit.mpNone:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpNone;
      end;
    Mf6.SimulationNameFileReaderUnit.mpSummary:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpSummary;
      end;
    Mf6.SimulationNameFileReaderUnit.mpAll:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpAll;
      end;
  end;
  SmsPkg.MaxErrors := FSimulation.Options.MaxErrors;
  if FSimulation.Options.PrintInputOption then
  begin
    OC := Model.ModflowOutputControl;
    OC.PrintInputCellLists := True;
  end;
end;

procedure TModflow6Importer.ImportSolutionGroups;
var
  GroupIndex: Integer;
  Group: TSolutionGroup;
  SolutionIndex: Integer;
  Solution: TSolution;
  ModelIndex: Integer;
  Model: TPhastModel;
begin
  // for now, this just imports Mxiter.
  if FFlowModel = nil then
  begin
    Exit;
  end;
  Model := frmGoPhast.PhastModel;
  for GroupIndex := 0 to FSimulation.SolutionGroupCount - 1 do
  begin
    Group := FSimulation.SolutionGroups[GroupIndex];
    for SolutionIndex := 0 to Group.Count - 1 do
    begin
      Solution := Group.Solutions[SolutionIndex];
      for ModelIndex := 0 to Solution.FSolutionModelNames.Count - 1 do
      begin
        if AnsiSameText(Solution.FSolutionModelNames[ModelIndex], FFlowModel.ModelName) then
        begin
          Model.ModflowPackages.SmsPackage.SolutionGroupMaxIteration
            := Group.Mxiter
        end;
      end;
    end;
  end;
end;

procedure TModflow6Importer.ImportTvk(Package: TPackage);
var
  Tvk: TTvk;
  APeriod: TTvkPeriodData;
  Model: TPhastModel;
  LastTime: Double;
  StartTime: Double;
  PeriodIndex: Integer;
  BoundIndex: Integer;
  TvkBound: TTimeVariableCell;
  KScreenObject: TScreenObject;
  Item: TTvkItem;
  CellId: TCellId;
  KDictionary: TDictionary<string, TScreenObject>;
  AScreenObject: TScreenObject;
  Index: Integer;
  UndoCreateScreenObject: TCustomUndo;
  APoint: TPoint2D;
begin
  Model := frmGoPhast.PhastModel;
  LastTime := Model.ModflowStressPeriods.Last.EndTime;

  KDictionary := TDictionary<string, TScreenObject>.Create;
  try


    Tvk := Package.Package as TTvk;
    for PeriodIndex := 0 to Tvk.Count - 1 do
    begin
      APeriod := Tvk[PeriodIndex];
      StartTime := Model.ModflowStressPeriods[APeriod.Period-1].StartTime;
      if KScreenObject <> nil then
      begin
        Item := KScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
        Item.EndTime := StartTime;
      end;
      KScreenObject := nil;
      for AScreenObject in KDictionary.Values do
      begin
        Item := AScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
        Item.EndTime := StartTime;
      end;
      KDictionary.Clear;
      for BoundIndex := 0 to APeriod.Count - 1 do
      begin
        TvkBound := APeriod[BoundIndex];
        if TvkBound.VariableName = 'K' then
        begin
          if TvkBound.ValueType = vtNumeric then
          begin
            if KScreenObject = nil then
            begin
              KScreenObject := TScreenObject.CreateWithViewDirection(
                Model, vdTop, UndoCreateScreenObject, False);
              KScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

              Model.AddScreenObject(KScreenObject);
              KScreenObject.ElevationCount := ecOne;
              KScreenObject.SetValuesOfIntersectedCells := True;
              KScreenObject.EvaluatedAt := eaBlocks;
              KScreenObject.Visible := False;

              KScreenObject.CreateTvkBoundary;
              AScreenObject := KScreenObject
            end;
          end
          else
          begin

          end;

//            CellId := TvkBound.CellId;
//            if Model.DisvUsed then
//            begin
//              Dec(CellId.Column);
//              CellId.Row := 0;
//            end
//            else
//            begin
//              Dec(CellId.Column);
//              Dec(CellId.Row);
//            end;
//            APoint := Model.TwoDElementCenter(CellId.Column, CellId.Row);
//            KScreenObject.AddPoint(APoint, True);
//            KScreenObject.ElevationFormula := Format('LayerCenter(%d)', [CellId.Layer]);
//
//            Item := KScreenObject.ModflowTvkBoundary.Values.Add as TTvkItem;
//            Item.StartTime := StartTime;
//            Item.EndTime := LastTime;

        end
        else if TvkBound.VariableName = 'K22' then
        begin

        end
        else if TvkBound.VariableName = 'K33' then
        begin

        end
        else
        begin

        end;
      end;
    end;
  finally
    KDictionary.Free;
  end;

end;

end.
