unit ModflowGridUnit;

interface

uses System.UITypes,
  Windows, // included to allow inlining of "Beep" function.
  Types, // included to allow inlining of "Point" function.
  SysUtils, Classes, Controls, Graphics, Forms,
  GR32, // TBitmap32 and TFloatRect are declared in GR32.
  GoPhastTypes, AbstractGridUnit, ZoomBox2, OpenGL, DataSetUnit,
  FastGEO;

type
  TModflowGrid = class(TCustomModelGrid)
  private
    FLayerElevations: TThreeDRealArray;
    // @name contains the locations of the cell corners, cell centers
    // on the top and bottom surfaces, and the centers of the edges of the
    // top and bottom surfaces.  It does not contain the location of the
    // center of the cell.  It is used for drawing the cell in
    // front, side and 3D Views.
    // The points are in the grid coordinate system not the
    // model coordinate system.
    //
    // Dimensions: 2*ColCount+1, 2*RowCount+1, LayerCount+1.
    CellPoints: T3DRealPointArray3;
    FCellElevationsNeedUpdating: boolean;
    FCellPointsNeedUpdating: boolean;
    FUpdatingElevations: Boolean;
    procedure SetLayerElevations(const Value: TThreeDRealArray);
    // @name calculates @link(CellPoints).
    procedure UpdateCellPoints;
    procedure IdentifyDividedUnitBoundaries(
      out LayerBoundaries: TOneDIntegerArray; out DividedUnits: Boolean);
    procedure SetLayerLineWidth(var LineWidth: single; LayerIndex: Integer;
      var UnitIndex: Integer; DividedUnits: Boolean;
      LayerBoundaries: TOneDIntegerArray);
    function GetLayerElevations: TThreeDRealArray;
    procedure Write1DArray(const ArrayName: string; const AnArray: TOneDRealArray;
      const DiscretizationWriter: TObject; const Reverse: boolean);
    procedure WriteALayerArray(const DiscretizationWriter: TObject;
      const LayerIndex: integer; const Comment: string; const MF6_ArrayName: string);
    function IsLayerUniform(const LayerIndex: integer): boolean;
    procedure CheckSizeRatios(const AnArray: TOneDRealArray;
      const WarningRoot: string);
    procedure GetMinAndMax(const AnArray: TOneDRealArray; out MinValue,
      MaxValue: double; out MinIndex, MaxIndex: integer);
    procedure DrawOrdinaryFrontColumns(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2; FrontPoints: T2DRealPointArray);
    procedure DrawOrdinaryFrontLayers(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2; FrontPoints: T2DRealPointArray);
    procedure DrawOrdinarySideRows(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2; SidePoints: T2DRealPointArray);
    procedure DrawOrdinarySideLayers(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2; SidePoints: T2DRealPointArray);
  protected
    // @name creates an OpenGL display list using @link(FCellsGLIndex)
    // to show colored grid cells or elements.
    procedure RecordColoredGrid; override;
    // @name creates an OpenGL display list using @link(FrontGridGLIndex)
    // to show the grid on the front view of the model.
    procedure RecordFront; override;
    // @name creates an OpenGL display list using @link(GridShellGLIndex)
    // to show the grid shell.
    procedure RecordShell; override;
    // @name creates an OpenGL display list using @link(SideGridGLIndex)
    // to show the grid on the side view of the model.
    procedure RecordSide; override;
    // @name creates an OpenGL display list using @link(TopGridGLIndex)
    // to show the grid on the top view of the model.
    procedure RecordTop; override;
    {@name draws a front view of the grid on TBitmap32.}
    procedure DrawFront(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); override;
    {@name draws a side view of the grid on TBitmap32.}
    procedure DrawSide(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); override;
    procedure DrawTop(const BitMap: TPersistent;
      const ZoomBox: TQRbwZoomBox2); override;
    // @name sets Elevations to a 3D array of real numbers.  Each
    // number in the array is the elevation of one corner of a cell or
    // element.
    //
    // This procedure would probably have to be modified or replaced for
    // grids such as that in MODFLOW in which elevations can vary
    // within a layer.
    procedure GetCellCornerElevations(const EvalAt: TEvaluatedAt;
      out Elevations: TThreeDRealArray); override;
    // See @link(CellElevation).
    function GetCellElevation(const CellID: TZeroBasedID): real;
      override;
    // See @link(CellThickness).
    function GetCellThickness(const Column, Row, Layer: integer): real;
      override;
    // See @link(CellElevation).
    procedure SetCellElevation(const CellID: TZeroBasedID;
      const Value: real); override;
    // See @link(CellThickness).
    procedure SetCellThickness(const Column, Row, Layer: integer;
      const Value: real); override;
    function GetTwoDCellElevations(const Col, Row: integer): TOneDRealArray; override;
    procedure SetFrontContourDataSet(const Value: TDataArray); override;
    procedure SetSideContourDataSet(const Value: TDataArray); override;
    procedure SetThreeDContourDataSet(const Value: TDataArray); override;
    procedure SetTopContourDataSet(const Value: TDataArray); override;
    // See @link(SideDataSet).
    procedure SetSideDataSet(const Value: TDataArray); override;
    // See @link(ThreeDDataSet).
    procedure SetThreeDDataSet(const Value: TDataArray); override;
    // See @link(TopDataSet).
    procedure SetTopDataSet(const Value: TDataArray); override;
    // See @link(FrontDataSet).
    procedure SetFrontDataSet(const Value: TDataArray); override;
    function GetElevationsNeedUpdating: Boolean; override;
  public
    procedure UpdateCellElevations;
    // @name is used when the grid has changed so that the grid elevations
    // will be updated.
    procedure NotifyGridChanged(Sender: TObject);
    {Copies the properties of Source into self.  Only those properties that
     normally would be saved to file are copied.}
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    function ThreeDElementCorner(const Column, Row, Layer: integer):
      TPoint3D; override;
    // @name is the elevations of the top and bottom of the model layers
    // @name is accessed as @name[Col, Row, Layer].
    property LayerElevations: TThreeDRealArray read GetLayerElevations
      write SetLayerElevations;
    function LayerThickness(Layer, Row, Column: Integer): double;
    // @name has the cell outlines for the MODFLOW cells as
    // viewed from the front.  The dimensions of the result will be
    // @link(ColumnCount)*2+1, @link(LayerCount)+1
    // The outline for a cell will be the points at
    // @orderedList(
    //   @item([Column*2, Layer])
    //   @item([Column*2+1, Layer])
    //   @item([Column*2+2, Layer])
    //   @item([Column*2, Layer+1])
    //   @item([Column*2+1, Layer+1])
    //   @item([Column*2+2, Layer+1])
    // )
    function FrontCellPoints(Row: integer): T2DRealPointArray;
    // @name has the cell outlines for the MODFLOW cells as
    // viewed from the side.  The dimensions of the result will be
    // @link(RowCount)*2+1, @link(LayerCount)+1
    // The outline for a cell will be the points at
    // @orderedList(
    //   @item([Row*2, Layer])
    //   @item([Row*2+1, Layer])
    //   @item([Row*2+2, Layer])
    //   @item([Row*2, Layer+1])
    //   @item([Row*2+1, Layer+1])
    //   @item([Row*2+2, Layer+1])
    // )
    function SideCellPoints(Col: integer): T2DRealPointArray;
    function HighestElevation: real; override;
    function LowestElevation: real; override;
    function GetContainingLayer(ACol, ARow: integer; const AZPosition: real): integer; override;
    function NearestLayerPosition(ACol, ARow: integer; const AZPosition: real;
      const First: integer = -1; const Last: integer = -1): integer;
    procedure WriteDELR(const DiscretizationWriter: TObject);
    procedure WriteDELC(const DiscretizationWriter: TObject);
    procedure WriteTOP(const DiscretizationWriter: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure WriteBOTM(const DiscretizationWriter: TObject;
      const Model: TBaseModel);
    procedure CheckColumnWidths;
    procedure CheckRowHeights;
    procedure CheckRowToColumnRatios;
    Procedure CheckElevations;
    property ElevationsNeedUpdating: Boolean read GetElevationsNeedUpdating;
    function OkLocation(const DataSet: TDataArray;
      const CellID: TZeroBasedID): boolean; override;
    property UpdatingElevations: Boolean read FUpdatingElevations;
  end;

implementation

uses Vcl.Dialogs, GR32_Polygons, BigCanvasMethods, frmGoPhastUnit,
  ModelMuseUtilities, LayerStructureUnit, PhastModelUnit, frmErrorsAndWarningsUnit,
  IsosurfaceUnit, CustomModflowWriterUnit, DataArrayManagerUnit,
  DataSetNamesUnit;

resourcestring
  StrColumns = 'columns';
  StrRows = 'rows';
  StrTheWidthOfOneOr = 'The width of one or more %s is zero.';
  StrColumnDRow = 'Column = %0:d; Row = %1:d; Layer = %2:d';
  StrTheRatioBetweenTh = 'The ratio between the widths of two adjacent %s ex' +
  'ceeds the recommended maximum of 1.5';
  StrOneOrMoreCellsHa = 'One or more cells have ratios of row to column widt' +
  'h that exceed the recommended maximum of 10.';
  StrColumn0dRow1 = 'Column %0:d, Row %1:d';
  StrTheTopOfOneOrMo = 'The top of one or more cells is equal to or below its bottom.';
  StrThereIsADiscrepan = 'There is a discrepancy between the number of layer' +
  's in the MODFLOW grid and the number of layers specified via the MODFLOW ' +
  'Layer Groups. You need to change the MODFLOW Layer Groups so that there ' +
  'are %d layers.';

procedure ReadReal2DArray(const Reader: TReader;
  var Positions: TTwoDRealArray; const Count1, Count2: integer);
var
  Index: integer;
  CurrentCount: integer;
begin
  with Reader do
  begin
    if Count1 = 0 then
    begin
      CurrentCount := 4;
      SetLength(Positions, CurrentCount);
      ReadListBegin;
      Index := 0;
      while not EndOfList do
      begin
        ReadRealArray(Reader, Positions[Index], Count2);
        Inc(Index);
        if Index = CurrentCount then
        begin
          CurrentCount := CurrentCount * 2;
          SetLength(Positions, CurrentCount);
        end;
      end;
      ReadListEnd;
      SetLength(Positions, Index);
    end
    else
    begin
      SetLength(Positions, Count1);
      ReadListBegin;
      for Index := 0 to Count1 - 1 do
      begin
        ReadRealArray(Reader, Positions[Index], Count2);
      end;
      ReadListEnd;
    end;
  end;
end;

procedure ReadReal3DArray(const Reader: TReader;
  var Positions: TThreeDRealArray; const Count1, Count2, Count3: integer);
var
  Index: integer;
  CurrentCount: integer;
begin
  with Reader do
  begin
    if Count1 = 0 then
    begin
      CurrentCount := 4;
      SetLength(Positions, CurrentCount);
      ReadListBegin;
      Index := 0;
      while not EndOfList do
      begin
        ReadReal2DArray(Reader, Positions[Index], Count2, Count3);
        Inc(Index);
        if Index = CurrentCount then
        begin
          CurrentCount := CurrentCount * 2;
          SetLength(Positions, CurrentCount);
        end;
      end;
      ReadListEnd;
      SetLength(Positions, Index);
    end
    else
    begin
      SetLength(Positions, Count1);
      ReadListBegin;
      for Index := 0 to Count1 - 1 do
      begin
        ReadReal2DArray(Reader, Positions[Index], Count2, Count3);
      end;
      ReadListEnd;
    end;
  end;
end;

procedure WriteReal2DArray(const Writer: TWriter;
  const Positions: TTwoDRealArray);
var
  Count: integer;
  Index: integer;
begin
  with Writer do
  begin
    Count := Length(Positions);
    WriteListBegin;
    for Index := 0 to Count - 1 do
    begin
      WriteRealArray(Writer, Positions[Index]);
    end;
    WriteListEnd;
  end;
end;

procedure WriteReal3DArray(const Writer: TWriter;
  const Positions: TThreeDRealArray);
var
  Count: integer;
  Index: integer;
begin
  with Writer do
  begin
    Count := Length(Positions);
    WriteListBegin;
    for Index := 0 to Count - 1 do
    begin
      WriteReal2DArray(Writer, Positions[Index]);
    end;
    WriteListEnd;
  end;
end;

{ TModflowGrid }

procedure TModflowGrid.Assign(Source: TPersistent);
var
  SourceGrid: TModflowGrid;
begin
  if Source is TModflowGrid then
  begin
    SourceGrid := TModflowGrid(Source);
    LayerElevations := SourceGrid.LayerElevations;
  end;
  inherited Assign(Source);
end;

procedure TModflowGrid.UpdateCellElevations;
var
  LayerGroupIndex: integer;
  LayerGroup: TLayerGroup;
  DataArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: integer;
  UnitBottomIndex, UnitTopIndex: integer;
  Fraction: real;
  UnitBottom, UnitTop, UnitHeight: Real;
  FractionIndex: Integer;
  DataArrayManager: TDataArrayManager;
  Model: TCustomModel;
  LayerFractions: TDoubleDynArray;
  ChildModel: TChildModel;
  Discretization: TChildDiscretizationCollection;
  CheckIndex: Integer;
  UnitThickness: Real;
  LayerTop: Real;
  LayerBottom: Real;
  LocalThreeDDataSet: TDataArray;
  LocalThreeDContourDataSet: TDataArray;
  LocalSideDataSet: TDataArray;
  LocalSideContourDataSet: TDataArray;
  LocalTopDataSet: TDataArray;
  LocalTopContourDataSet: TDataArray;
  LocalFrontDataSet: TDataArray;
  LocalFrontContourDataSet: TDataArray;
begin
  if not FCellElevationsNeedUpdating then
  begin
    Exit;
  end;
  if not (frmGoPhast.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if (ColumnCount <= 0) or (RowCount <= 0) or (LayerCount <= 0) then
  begin
    Exit;
  end;
  if FUpdatingElevations then
  begin
    Exit;
  end;

  Model := self.Model as TCustomModel;
  if Model.LayerCount <> LayerCount then
  begin
    Beep;
    MessageDlg(Format(StrThereIsADiscrepan, [LayerCount]), mtError, [mbOK], 0);
    FCellElevationsNeedUpdating := False;
    Exit;
  end;
  Assert(Model.LayerCount = LayerCount);

  LocalThreeDDataSet := ThreeDDataSet;
  LocalThreeDContourDataSet := ThreeDContourDataSet;
  LocalSideDataSet := SideDataSet;
  LocalSideContourDataSet := SideContourDataSet;
  LocalTopDataSet := TopDataSet;
  LocalTopContourDataSet := TopContourDataSet;
  LocalFrontDataSet := FrontDataSet;
  LocalFrontContourDataSet := FrontDataSet;
  try
    ThreeDDataSet := nil;
    ThreeDContourDataSet := nil;
    SideDataSet := nil;
    SideContourDataSet := nil;
    TopDataSet := nil;
    TopContourDataSet := nil;
    FrontDataSet := nil;
    FrontDataSet := nil;
    FUpdatingElevations := True;
    BeginLayerChange;
    try
      LayerIndex := -1;
      UnitTopIndex := -1;
      SetLength(FLayerElevations, ColumnCount, RowCount, LayerCount+1);

      DataArrayManager := Model.DataArrayManager;
      for LayerGroupIndex := 0 to
        Model.LayerStructure.Count -1 do
      begin
        LayerGroup := Model.LayerStructure.
          Items[LayerGroupIndex] as TLayerGroup;
        if not Model.LayerGroupUsed(LayerGroup) then
        begin
          Continue;
        end;
        if LayerGroup.RunTimeSimulated then
        begin
          LayerFractions := Model.LayerFractions(LayerGroup);
          UnitBottomIndex := UnitTopIndex + Length(LayerFractions) + 1;
        
          // Check that LayerFractions is valid.
          if Length(LayerFractions) >= 2 then
          begin
            for CheckIndex := 0 to Length(LayerFractions) - 2 do
            begin
              Assert(LayerFractions[CheckIndex] > LayerFractions[CheckIndex+1]);
            end;
          end;
        end
        else
        begin
          UnitBottomIndex := UnitTopIndex + 1;
          LayerFractions := nil;
        end;
        DataArray := DataArrayManager.GetDataSetByName(LayerGroup.DataArrayName);
        Assert(DataArray <> nil);
        DataArray.Initialize;
        for ColIndex := 0 to ColumnCount - 1 do
        begin
          for RowIndex := 0 to RowCount - 1 do
          begin
            CellElevation[ZeroBasedID(UnitBottomIndex,RowIndex,ColIndex)] :=
              DataArray.RealData[0, RowIndex, ColIndex];
          end;
        end;
        DataArrayManager.AddDataSetToCache(DataArray);
        DataArrayManager.CacheDataArrays;
        Inc(LayerIndex);
        if LayerGroup.RunTimeSimulated then
        begin
          for FractionIndex := 0 to Length(LayerFractions) - 1 do
          begin
            Fraction := LayerFractions[FractionIndex];
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              for RowIndex := 0 to RowCount - 1 do
              begin
                UnitTop :=    CellElevation[ZeroBasedID(UnitTopIndex,RowIndex,ColIndex)];
                UnitBottom := CellElevation[ZeroBasedID(UnitBottomIndex,RowIndex,ColIndex)];
                UnitHeight := UnitTop - UnitBottom;

                CellElevation[ZeroBasedID(LayerIndex,RowIndex,ColIndex)] :=
                  UnitBottom + UnitHeight*Fraction;
              end;
            end;
            Inc(LayerIndex);
          end;
        end;
        if (Model is TChildModel) then
        begin
          ChildModel := TChildModel(Model);
          if LayerGroup = ChildModel.Discretization.BottomLayerGroup then
          begin
            Discretization := ChildModel.Discretization;
            if ((LayerGroupIndex < Model.LayerStructure.Count -1)
              or (Discretization.BottomLayerInUnit < LayerGroup.LayerCount -1)) then
            begin
              for ColIndex := 0 to ColumnCount - 1 do
              begin
                for RowIndex := 0 to RowCount - 1 do
                begin
                  UnitTop := CellElevation[ZeroBasedID(UnitTopIndex,RowIndex,ColIndex)];
                  UnitBottom := CellElevation[ZeroBasedID(UnitBottomIndex,RowIndex,ColIndex)];
                  UnitThickness := UnitTop - UnitBottom;
                  LayerTop := UnitTop - UnitThickness/LayerGroup.LayerCount
                    * Discretization.BottomLayerInUnit;
                  LayerBottom := UnitTop - UnitThickness/LayerGroup.LayerCount
                    * (Discretization.BottomLayerInUnit+1);

                  if (Model.ModelSelection = msModflowLGR) then
                  begin
                    CellElevation[ZeroBasedID(UnitBottomIndex,RowIndex,ColIndex)] :=
                      (LayerTop + LayerBottom)/2;
                  end
                  else
                  begin
                    CellElevation[ZeroBasedID(UnitBottomIndex,RowIndex,ColIndex)] :=
                      LayerBottom;
                  end;
                end;
              end;
            end;
          end;
        end;
        UnitTopIndex := UnitBottomIndex;
        Assert(LayerIndex = UnitTopIndex);
      end;
    finally
      EndLayerChange;
      FUpdatingElevations := False;
    end;
    FCellElevationsNeedUpdating := False;
  finally
    ThreeDDataSet := LocalThreeDDataSet;
    ThreeDContourDataSet := LocalThreeDContourDataSet;
    SideDataSet := LocalSideDataSet;
    SideContourDataSet := LocalSideContourDataSet;
    TopDataSet := LocalTopDataSet;
    TopContourDataSet := LocalTopContourDataSet;
    FrontDataSet := LocalFrontDataSet;
    FrontDataSet := LocalFrontContourDataSet;
  end;
end;

procedure TModflowGrid.UpdateCellPoints;
var
  ColIndex: Integer;
  ColWeight1: Real;
  ColWeight2: Real;
  ColCenter: Real;
  ColEdge: Real;
  LayerIndex: Integer;
  RowIndex: Integer;
  RowWeight1: Real;
  RowWeight2: Real;
  RCenter: Real;
  RowEdge: Real;
  CIndex1, CIndex2: integer;
  RIndex1, RIndex2: integer;
begin
  if not FCellPointsNeedUpdating then Exit;
  CellPoints := nil;
  if (ColumnCount <= 0) or (RowCount <= 0) or (LayerCount <= 0) then
  begin
    Exit;
  end;
  UpdateCellElevations;
  SetLength(CellPoints, ColumnCount*2+1, RowCount*2+1, LayerCount+1);

  for ColIndex := 0 to ColumnCount do
  begin
    if ColIndex < ColumnCount then
    begin
      CIndex1:= ColIndex;
    end
    else
    begin
      CIndex1:= ColIndex-1;
    end;
    ColWeight2 := ColumnWidth[CIndex1];

    if ColIndex > 0 then
    begin
      CIndex2 := ColIndex-1;
    end
    else
    begin
      CIndex2 := 0;
    end;
    ColWeight1 := ColumnWidth[CIndex2];

    if ColIndex < ColumnCount then
    begin
      ColCenter := ColumnCenter(ColIndex);
    end
    else
    begin
      ColCenter := -1000000;
    end;
    ColEdge := ColumnPosition[ColIndex];

    for RowIndex := 0 to RowCount do
    begin
      if RowIndex < RowCount then
      begin
        RIndex1 := RowIndex;
      end
      else
      begin
        RIndex1 := RowIndex-1;
      end;
      RowWeight2 := RowWidth[RIndex1];

      if RowIndex > 0 then
      begin
        RIndex2 := RowIndex-1;
      end
      else
      begin
        RIndex2 := 0;
      end;
      RowWeight1 := RowWidth[RIndex2];

      if RowIndex < RowCount then
      begin
        RCenter := RowCenter(RowIndex);
      end
      else
      begin
        RCenter := -1000000;
      end;
      RowEdge := RowPosition[RowIndex];

      for LayerIndex := 0 to LayerCount do
      begin
        if ColIndex < ColumnCount then
        begin
          if RowIndex < RowCount then
          begin
            // Cell center
            CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex].X := ColCenter;
            CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex].Y := RCenter;
            CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex].Z :=
              CellElevation[ZeroBasedID(LayerIndex,RowIndex,ColIndex)];
          end;
          // column center
          CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex].X := ColCenter;
          CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex].Y := RowEdge;
          CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex].Z :=
            ((CellElevation[ZeroBasedID(LayerIndex,RIndex1,ColIndex)]*RowWeight1)
            + (CellElevation[ZeroBasedID(LayerIndex,RIndex2,ColIndex)]*RowWeight2))
            /(RowWeight1+RowWeight2);
        end;
        if RowIndex < RowCount then
        begin
          // Row center
          CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex].X := ColEdge;
          CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex].Y := RCenter;
          CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex].Z :=
            ((CellElevation[ZeroBasedID(LayerIndex,RowIndex,CIndex1)]*ColWeight1)
            + (CellElevation[ZeroBasedID(LayerIndex,RowIndex,CIndex2)]*ColWeight2))
            /(ColWeight1+ColWeight2);
        end;
        // cell corner
        CellPoints[ColIndex*2,RowIndex*2,LayerIndex].X :=ColEdge;
        CellPoints[ColIndex*2,RowIndex*2,LayerIndex].Y :=RowEdge;
        CellPoints[ColIndex*2,RowIndex*2,LayerIndex].Z :=
          (((CellElevation[ZeroBasedID(LayerIndex,RIndex1,CIndex1)]*ColWeight1
          +CellElevation[ZeroBasedID(LayerIndex,RIndex1,CIndex2)]*ColWeight2)
          /(ColWeight1+ColWeight2))*RowWeight1
          +
          ((CellElevation[ZeroBasedID(LayerIndex,RIndex2,CIndex1)]*ColWeight1
          +CellElevation[ZeroBasedID(LayerIndex,RIndex2,CIndex2)]*ColWeight2)
          /(ColWeight1+ColWeight2))*RowWeight2)
          /(RowWeight1+RowWeight2);
      end;
    end;
  end;
  FCellPointsNeedUpdating := False;
end;

procedure TModflowGrid.WriteBOTM(const DiscretizationWriter: TObject;
  const Model: TBaseModel);
var
  LocalModel: TCustomModel;
  LayerIndex: integer;
  MF6_Arrayname: string;
begin
  LocalModel := Model as TCustomModel;
  for LayerIndex := 1 to LayerCount do
  begin
    if LayerIndex = 1 then
    begin
      MF6_Arrayname := 'BOTM';
    end
    else
    begin
      MF6_Arrayname := '';
    end;
    WriteALayerArray(DiscretizationWriter, LayerIndex,
      'BOTM, ' + LocalModel.ModflowLayerBottomDescription(LayerIndex-1),
      MF6_Arrayname);
  end;
end;

procedure TModflowGrid.Write1DArray(const ArrayName: string;
  const AnArray: TOneDRealArray; const DiscretizationWriter: TObject;
  const Reverse: boolean);
var
  DisWriter: TCustomModflowWriter;
  Index: Integer;
  NewLineNeeded: boolean;
begin
  DisWriter := DiscretizationWriter as TCustomModflowWriter;
  if IsUniform(AnArray) then
  begin
    if Reverse then
    begin
      DisWriter.WriteConstantU2DREL(ArrayName, -AnArray[0], mat1D, ArrayName);
    end
    else
    begin
      DisWriter.WriteConstantU2DREL(ArrayName, AnArray[0], mat1D, ArrayName);
    end;
  end
  else
  begin
    NewLineNeeded := False;
    DisWriter.WriteU2DRELHeader(ArrayName, mat1D, ArrayName);
//    if Model.ModelSelection = msModflow2015 then
//    begin
//      DisWriter.WriteString('      ');
//    end;
    for Index := 0 to Length(AnArray) - 1 do
    begin
      if (Model.ModelSelection = msModflow2015) and not NewLineNeeded then
      begin
        DisWriter.WriteString('      ');
      end;
      if Reverse then
      begin
        DisWriter.WriteFloat(-AnArray[Index]);
      end
      else
      begin
        DisWriter.WriteFloat(AnArray[Index]);
      end;
      if ((Index + 1) mod 10) = 0 then
      begin
        DisWriter.NewLine;
        NewLineNeeded := False;
      end
      else
      begin
        NewLineNeeded := True;
      end;
    end;
    if NewLineNeeded then
    begin
      DisWriter.NewLine;
    end;
  end;
end;

procedure TModflowGrid.WriteDELC(const DiscretizationWriter: TObject);
begin
  Write1DArray('DELC', RowWidths, DiscretizationWriter, True);
end;

procedure TModflowGrid.WriteDELR(const DiscretizationWriter: TObject);
begin
  Write1DArray('DELR', ColWidths, DiscretizationWriter, False);
end;

function TModflowGrid.IsLayerUniform(const LayerIndex: integer): boolean;
var
  Elevations: TThreeDRealArray;
  TestValue: double;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Elevations := LayerElevations;
  result := True;
  TestValue := Elevations[0,0,LayerIndex];
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      result := Elevations[ColIndex,RowIndex,LayerIndex] = TestValue;
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TModflowGrid.WriteALayerArray(const DiscretizationWriter: TObject;
  const LayerIndex: integer; const Comment: string; const MF6_ArrayName: string);
var
  DisWriter: TCustomModflowWriter;
  Elevations: TThreeDRealArray;
  RowIndex: Integer;
  ColIndex: Integer;
  NewLineNeeded: boolean;
  UseGsflowFormat: Boolean;
begin
  UseGsflowFormat := (Model as TCustomModel).UseGsflowFormat and (Model.ModelSelection <> msModflow2015);
  Elevations := LayerElevations;
  DisWriter := DiscretizationWriter as TCustomModflowWriter;
  if IsLayerUniform(LayerIndex) then
  begin
    DisWriter.WriteConstantU2DREL(Comment, Elevations[0,0,LayerIndex],
      matStructured, MF6_ArrayName);
  end
  else
  begin
//    if LayerIndex <= 1 then
//    begin
      DisWriter.WriteU2DRELHeader(Comment, matStructured, MF6_ArrayName);
//    end
//    else
//    begin
//      DisWriter.WriteU2DRELHeader(Comment, mat1D, MF2015_ArrayName);
//    end;
    for RowIndex := 0 to RowCount - 1 do
    begin
      NewLineNeeded := False;
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        if (Model.ModelSelection = msModflow2015) and not NewLineNeeded then
        begin
          DisWriter.WriteString('      ');
        end;
        DisWriter.WriteFloat(Elevations[ColIndex, RowIndex, LayerIndex]);
        if ((ColIndex + 1) mod 10) = 0 then
        begin
          if not UseGsflowFormat then
          begin
            DisWriter.NewLine;
          end;
          NewLineNeeded := False;
        end
        else
        begin
          NewLineNeeded := True;
        end;
      end;
      if NewLineNeeded then
      begin
        if not UseGsflowFormat then
        begin
          DisWriter.NewLine;
        end;
      end;
      if UseGsflowFormat then
      begin
        DisWriter.NewLine;
      end;
    end;
  end;
end;

procedure TModflowGrid.WriteTOP(const DiscretizationWriter: TObject);
begin
  WriteALayerArray(DiscretizationWriter, 0, 'TOP', 'TOP');
end;

function TModflowGrid.FrontCellPoints(Row: integer): T2DRealPointArray;
var
  ColIndex: Integer;
  LayerIndex: Integer;
begin
  result := nil;
  if (ColumnCount < 0) or (RowCount < 0) or (LayerCount < 0) then
  begin
    Exit;
  end;
  UpdateCellPoints;
  if CellPoints = nil then
  begin
    Exit;
  end;
  SetLength(result, ColumnCount*2+1, LayerCount+1);
  for ColIndex := 0 to ColumnCount*2 do
  begin
    for LayerIndex := 0 to LayerCount do
    begin
      result[ColIndex, LayerIndex].X :=
        CellPoints[ColIndex, Row*2+1, LayerIndex].X;
      result[ColIndex, LayerIndex].Y :=
        CellPoints[ColIndex, Row*2+1, LayerIndex].Z;
    end;
  end;
end;

procedure TModflowGrid.NotifyGridChanged(Sender: TObject);
var
  ChildIndex: Integer;
  LocalModel: TPhastModel;
  ChildModel: TChildModel;
begin
  if frmGoPhast.PhastModel <> nil then
  begin
    if (frmGoPhast.PhastModel <> nil)
      and ((csDestroying in frmGoPhast.PhastModel.ComponentState)
      or frmGoPhast.PhastModel.Clearing) then
    begin
      Exit;
    end;

    frmGoPhast.PhastModel.InvalidateSegments;
  end;
  FCellElevationsNeedUpdating:= True;
  FCellPointsNeedUpdating:= True;
  NeedToRecalculateCellColors;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.ModflowGrid.NotifyGridChanged(Sender);
      end;
    end;
  end;
  frmGoPhast.InvalidateAllViews;
end;

function TModflowGrid.OkLocation(const DataSet: TDataArray;
  const CellID: TZeroBasedID): boolean;
var
  IDomain: TDataArray;
begin
  result := inherited;
  if result and DataSet.Limits.ActiveOnly
    and (Model.ModelSelection = msModflow2015) then
  begin
    IDomain := (Model as TCustomModel).
      DataArrayManager.GetDataSetByName(K_IDOMAIN);
    result := IDomain.IntegerData[CellID.Layer, CellID.Row, CellID.Column] > 0;
  end;
end;

procedure TModflowGrid.CheckSizeRatios(const AnArray: TOneDRealArray;
  const WarningRoot: string);
var
  Index: Integer;
  WarningString: string;
  ErrorString: string;
  Ratio: double;
  Start: Integer;
  Stop: Integer;
begin
  ErrorString := Format(StrTheWidthOfOneOr, [WarningRoot]);
  for Index := 0 to Length(AnArray) - 1 do
  begin
    if AnArray[Index] = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, ErrorString, IntToStr(Index+1));
    end;
  end;

  if Model is TChildModel then
  begin
    // don' check the outside rows and columns in child models.
    Start := 1;
    Stop := Length(AnArray) - 1
  end
  else
  begin
    Start := 0;
    Stop := Length(AnArray)
  end;

  WarningString := Format(StrTheRatioBetweenTh, [WarningRoot]);
  for Index := Start+1 to Stop - 1 do
  begin
    if (AnArray[Index-1] <> 0) and (AnArray[Index] <> 0) then
    begin
      Ratio := AnArray[Index-1]/AnArray[Index];
      if (Ratio > 1.5) or (Ratio < 1/1.5) then
      begin
        frmErrorsAndWarnings.AddWarning(Model, WarningString,
          IntToStr(Index) + ', ' + IntToStr(Index+1));
      end;
    end;
  end;
end;

procedure TModflowGrid.CheckColumnWidths;
begin
  CheckSizeRatios(ColWidths, StrColumns);
end;

procedure TModflowGrid.CheckElevations;
var
  Elevations: TThreeDRealArray;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerIndex: integer;
  ErrorString: string;
  Active: TDataArray;
  DataArrayManager: TDataArrayManager;
  LocalModel: TCustomModel;
  ActiveAbove: Boolean;
  ActiveBelow: Boolean;
  CellsAllZeroThickness: array of Boolean;
  InvalidLayers: TStringList;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrAllTheActiveCells);
  ErrorString := StrTheTopOfOneOrMo;
  Elevations := LayerElevations;
  LocalModel := Model as TCustomModel;
  DataArrayManager := LocalModel.DataArrayManager;
  Active := DataArrayManager.GetDataSetByName(rsActive);
  Active.Initialize;
  SetLength(CellsAllZeroThickness, LayerCount);
  for LayerIndex := 1 to LayerCount - 1 do
  begin
    CellsAllZeroThickness[LayerIndex] := True;
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        if (Elevations[ColIndex, RowIndex, LayerIndex] >
          Elevations[ColIndex, RowIndex, LayerIndex-1]) then
        begin
          if Active.BooleanData[LayerIndex-1, RowIndex, ColIndex] then
          begin
            frmErrorsAndWarnings.AddError(Model,ErrorString,
              Format(StrColumnDRow, [ColIndex+1, RowIndex+1, LayerIndex]));
          end
          else if not LocalModel.IsLayerSimulated(LayerIndex-1) then
          begin
            if LayerIndex -2 >= 0 then
            begin
              ActiveAbove := Active.BooleanData[LayerIndex-2, RowIndex, ColIndex];
            end
            else
            begin
              ActiveAbove := False;
            end;
            ActiveBelow := Active.BooleanData[LayerIndex, RowIndex, ColIndex];
            if ActiveAbove and ActiveBelow then
            begin
              frmErrorsAndWarnings.AddError(Model,ErrorString,
                Format(StrColumnDRow, [ColIndex+1, RowIndex+1, LayerIndex]));
            end;
          end;
        end
        else if (Elevations[ColIndex, RowIndex, LayerIndex] =
          Elevations[ColIndex, RowIndex, LayerIndex-1])
          and Active.BooleanData[LayerIndex-1, RowIndex, ColIndex] then
        begin
          if (Model.ModelSelection <> msModflow2015) then
          begin
            frmErrorsAndWarnings.AddError(Model,ErrorString,
            Format(StrColumnDRow, [ColIndex+1, RowIndex+1, LayerIndex]));
          end;
        end
        else
        begin
          CellsAllZeroThickness[LayerIndex] := False;
        end;
      end;
    end;
  end;

  InvalidLayers := TStringList.Create;
  try
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      if CellsAllZeroThickness[LayerIndex] then
      begin
        InvalidLayers.Add(IntToStr(LayerIndex+1));
      end;
    end;
    if InvalidLayers.Count > 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrAllTheActiveCells,
        InvalidLayers.CommaText);
    end;
  finally
    InvalidLayers.Free;
  end;

  DataArrayManager.AddDataSetToCache(Active);
  DataArrayManager.CacheDataArrays;
end;

procedure TModflowGrid.DrawOrdinarySideLayers(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2; SidePoints: T2DRealPointArray);
var
  APoint: TPoint2D;
  LineColor: TColor32;
  LineWidth: Single;
  UnitIndex: Integer;
  LocalEvalAt: TEvaluatedAt;
  DividedUnits: Boolean;
  LayerBoundaries: TOneDIntegerArray;
  Points: array of TPoint;
  LayerIndex: Integer;
  Column: Integer;
  RowIndex: Integer;
  LocalLineWidth: single;
  function IsActive: boolean;
  begin
    result := ((LayerIndex < LayerCount) and
      IsElementActive(LayerIndex,RowIndex, Column))
      or ((LayerIndex > 0)
      and IsElementActive(LayerIndex-1,RowIndex, Column))
  end;
  function IsEdge: boolean;
  begin
    result := ((LayerIndex < LayerCount) and
      IsElementActive(LayerIndex,RowIndex, Column))
      <> ((LayerIndex > 0)
      and IsElementActive(LayerIndex-1,RowIndex, Column))
  end;
begin
  if GridLineDrawingChoice in [gldcActive, gldcActiveEdge] then
  begin
    SetLength(Points, 3);
  end
  else
  begin
    SetLength(Points, Length(SidePoints));
  end;
  IdentifyDividedUnitBoundaries(LayerBoundaries, DividedUnits);
  SetLocalEvalAt(vdSide, LocalEvalAt);
  UnitIndex := 0;
  for LayerIndex := 0 to Length(SidePoints[0]) - 1 do
  begin
    SetLayerLineWidth(LineWidth, LayerIndex, UnitIndex, DividedUnits,
      LayerBoundaries);
    SetLayerLineColor(LayerIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll:
        begin
          for RowIndex := 0 to Length(SidePoints) - 1 do
          begin
            APoint := SidePoints[RowIndex, LayerIndex];
            Points[RowIndex] := Convert2D_SidePoint(ZoomBox, APoint);
          end;
          DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
        end;
      gldcExterior:
        begin
          if (LayerIndex <> 0) and (LayerIndex <> LayerCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          for RowIndex := 0 to Length(SidePoints) - 1 do
          begin
            APoint := SidePoints[RowIndex, LayerIndex];
            Points[RowIndex] := Convert2D_SidePoint(ZoomBox, APoint);
          end;
          DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
        end;
      gldcActive:
        begin
          Column := SelectedColumn;
          if Column >= ColumnCount then
          begin
            Dec(Column);
          end;
          for RowIndex := 0 to RowCount - 1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              APoint := SidePoints[RowIndex*2, LayerIndex];
              Points[0] := Convert2D_SidePoint(ZoomBox, APoint);
              APoint := SidePoints[RowIndex*2+1, LayerIndex];
              Points[1] := Convert2D_SidePoint(ZoomBox, APoint);
              APoint := SidePoints[RowIndex*2+2, LayerIndex];
              Points[2] := Convert2D_SidePoint(ZoomBox, APoint);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth, Points, True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Column := SelectedColumn;
          if Column >= ColumnCount then
          begin
            Dec(Column);
          end;
          for RowIndex := 0 to RowCount - 1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              APoint := SidePoints[RowIndex*2, LayerIndex];
              Points[0] := Convert2D_SidePoint(ZoomBox, APoint);
              APoint := SidePoints[RowIndex*2+1, LayerIndex];
              Points[1] := Convert2D_SidePoint(ZoomBox, APoint);
              APoint := SidePoints[RowIndex*2+2, LayerIndex];
              Points[2] := Convert2D_SidePoint(ZoomBox, APoint);
              DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
            end;
          end;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TModflowGrid.DrawOrdinarySideRows(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2; SidePoints: T2DRealPointArray);
var
  RowIndex: Integer;
  LayerIndex: Integer;
  APoint: TPoint2D;
  LineColor: TColor32;
  LineWidth: Single;
  LocalEvalAt: TEvaluatedAt;
  Points: array of TPoint;
  Column: Integer;
  LocalLineWidth: single;
  function IsActive: boolean;
  begin
    result := ((RowIndex < RowCount) and
      IsElementActive(LayerIndex,RowIndex, Column))
      or ((RowIndex > 0)
      and IsElementActive(LayerIndex,RowIndex-1, Column))
  end;
  function IsEdge: boolean;
  begin
    result := ((RowIndex < RowCount) and
      IsElementActive(LayerIndex,RowIndex, Column))
      <> ((RowIndex > 0)
      and IsElementActive(LayerIndex,RowIndex-1, Column));
  end;
begin
  if GridLineDrawingChoice in [gldcActive, gldcActiveEdge] then
  begin
    SetLength(Points, 2);
  end
  else
  begin
    SetLength(Points, Length(SidePoints[0]));
  end;
  SetLocalEvalAt(vdSide, LocalEvalAt);
  for RowIndex := 0 to RowCount do
  begin
    if (RowIndex mod 10 = 0) or (RowIndex = RowCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetRowLineColor(RowIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll:
        begin
          for LayerIndex := 0 to Length(SidePoints[0]) - 1 do
          begin
            APoint := SidePoints[RowIndex * 2, LayerIndex];
            Points[LayerIndex] := Convert2D_SidePoint(ZoomBox, APoint);
          end;
          DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
        end;
      gldcExterior:
        begin
          if (RowIndex <> 0) and (RowIndex <> RowCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          for LayerIndex := 0 to Length(SidePoints[0]) - 1 do
          begin
            APoint := SidePoints[RowIndex * 2, LayerIndex];
            Points[LayerIndex] := Convert2D_SidePoint(ZoomBox, APoint);
          end;
          DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
        end;
      gldcActive:
        begin
          Column := SelectedColumn;
          if Column >= ColumnCount then
          begin
            Dec(Column);
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              APoint := SidePoints[RowIndex * 2, LayerIndex];
              Points[0] := Convert2D_SidePoint(ZoomBox, APoint);
              APoint := SidePoints[RowIndex * 2, LayerIndex+1];
              Points[1] := Convert2D_SidePoint(ZoomBox, APoint);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth, Points, True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Column := SelectedColumn;
          if Column >= ColumnCount then
          begin
            Dec(Column);
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              APoint := SidePoints[RowIndex * 2, LayerIndex];
              Points[0] := Convert2D_SidePoint(ZoomBox, APoint);
              APoint := SidePoints[RowIndex * 2, LayerIndex+1];
              Points[1] := Convert2D_SidePoint(ZoomBox, APoint);
              DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
            end;
          end;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TModflowGrid.DrawOrdinaryFrontLayers(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2; FrontPoints: T2DRealPointArray);
var
  LayerIndex: Integer;
  Points: array of TPoint;
  LayerBoundaries: TOneDIntegerArray;
  DividedUnits: Boolean;
  LocalEvalAt: TEvaluatedAt;
  UnitIndex: Integer;
  LineWidth: Single;
  LineColor: TColor32;
  ColIndex: Integer;
  APoint: TPoint2D;
  Row: Integer;
  LocalLineWidth: single;
  function IsActive: boolean;
  begin
    result := ((LayerIndex < LayerCount) and
      IsElementActive(LayerIndex,Row, ColIndex))
      or ((LayerIndex > 0)
      and IsElementActive(LayerIndex-1,Row, ColIndex))
  end;
  function IsEdge: boolean;
  begin
    result := ((LayerIndex < LayerCount) and
      IsElementActive(LayerIndex,Row, ColIndex))
      <> ((LayerIndex > 0)
      and IsElementActive(LayerIndex-1,Row, ColIndex));
  end;
begin
  if GridLineDrawingChoice in [gldcActive, gldcActiveEdge] then
  begin
    SetLength(Points, 3);
  end
  else
  begin
    SetLength(Points, Length(FrontPoints));
  end;
  IdentifyDividedUnitBoundaries(LayerBoundaries, DividedUnits);
  SetLocalEvalAt(vdFront, LocalEvalAt);
  UnitIndex := 0;
  for LayerIndex := 0 to Length(FrontPoints[0]) - 1 do
  begin
    SetLayerLineWidth(LineWidth, LayerIndex, UnitIndex, DividedUnits,
      LayerBoundaries);
    SetLayerLineColor(LayerIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll:
        begin
          for ColIndex := 0 to Length(FrontPoints) - 1 do
          begin
            APoint := FrontPoints[ColIndex, LayerIndex];
            Points[ColIndex] := Convert2D_FrontPoint(ZoomBox, APoint);
          end;
          DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
        end;
      gldcExterior:
        begin
          if (LayerIndex <> 0) and (LayerIndex <> (Length(FrontPoints[0]) - 1))
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          for ColIndex := 0 to Length(FrontPoints) - 1 do
          begin
            APoint := FrontPoints[ColIndex, LayerIndex];
            Points[ColIndex] := Convert2D_FrontPoint(ZoomBox, APoint);
          end;
          DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
        end;
      gldcActive:
        begin
          Row := SelectedRow;
          if Row >= RowCount then
          begin
            Dec(Row);
          end;
          for ColIndex := 0 to ColumnCount-1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              APoint := FrontPoints[ColIndex*2, LayerIndex];
              Points[0] := Convert2D_FrontPoint(ZoomBox, APoint);
              APoint := FrontPoints[ColIndex*2+1, LayerIndex];
              Points[1] := Convert2D_FrontPoint(ZoomBox, APoint);
              APoint := FrontPoints[ColIndex*2+2, LayerIndex];
              Points[2] := Convert2D_FrontPoint(ZoomBox, APoint);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth, Points, True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Row := SelectedRow;
          if Row >= RowCount then
          begin
            Dec(Row);
          end;
          for ColIndex := 0 to ColumnCount-1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              APoint := FrontPoints[ColIndex*2, LayerIndex];
              Points[0] := Convert2D_FrontPoint(ZoomBox, APoint);
              APoint := FrontPoints[ColIndex*2+1, LayerIndex];
              Points[1] := Convert2D_FrontPoint(ZoomBox, APoint);
              APoint := FrontPoints[ColIndex*2+2, LayerIndex];
              Points[2] := Convert2D_FrontPoint(ZoomBox, APoint);
              DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TModflowGrid.DrawOrdinaryFrontColumns(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2; FrontPoints: T2DRealPointArray);
var
  LineWidth: Single;
  LineColor: TColor32;
  APoint: TPoint2D;
  ColIndex: Integer;
  LocalEvalAt: TEvaluatedAt;
  Points: array of TPoint;
  LayerIndex: Integer;
  Row: Integer;
  LocalLineWidth: Extended;
  function IsActive: boolean;
  begin
    result := ((ColIndex < ColumnCount) and
      IsElementActive(LayerIndex,Row, ColIndex))
      or ((ColIndex > 0)
      and IsElementActive(LayerIndex,Row, ColIndex-1))
  end;
  function IsEdge: boolean;
  begin
    result := ((ColIndex < ColumnCount) and
      IsElementActive(LayerIndex,Row, ColIndex))
      <> ((ColIndex > 0)
      and IsElementActive(LayerIndex,Row, ColIndex-1));
  end;
begin
  SetLocalEvalAt(vdFront, LocalEvalAt);
  if GridLineDrawingChoice in [gldcActive, gldcActiveEdge] then
  begin
    SetLength(Points, 2);
  end
  else
  begin
    SetLength(Points, Length(FrontPoints[0]));
  end;
  for ColIndex := 0 to ColumnCount do
  begin
    if (ColIndex mod 10 = 0) or (ColIndex = ColumnCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetColumnLineColor(ColIndex, LocalEvalAt, LineColor, LineWidth);
    case GridLineDrawingChoice of
      gldcAll:
        begin
          for LayerIndex := 0 to Length(FrontPoints[0]) - 1 do
          begin
            APoint := FrontPoints[ColIndex * 2, LayerIndex];
            Points[LayerIndex] := Convert2D_FrontPoint(ZoomBox, APoint);
          end;
          DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
        end;
      gldcExterior:
        begin
          if (ColIndex <> 0) and (ColIndex <> ColumnCount)
            and (LineColor = clBlack32) then
          begin
            Continue;
          end;
          for LayerIndex := 0 to Length(FrontPoints[0]) - 1 do
          begin
            APoint := FrontPoints[ColIndex * 2, LayerIndex];
            Points[LayerIndex] := Convert2D_FrontPoint(ZoomBox, APoint);
          end;
          DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
        end;
      gldcActive:
        begin
          Row := SelectedRow;
          if Row >= RowCount then
          begin
            Dec(Row);
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            if IsActive then
            begin
              if IsEdge then
              begin
                LocalLineWidth := ThickGridLineThickness;
              end
              else
              begin
                LocalLineWidth := LineWidth;
              end;
              APoint := FrontPoints[ColIndex*2, LayerIndex];
              Points[0] := Convert2D_FrontPoint(ZoomBox, APoint);
              APoint := FrontPoints[ColIndex*2, LayerIndex+1];
              Points[1] := Convert2D_FrontPoint(ZoomBox, APoint);
              DrawBigPolyline32(BitMap, LineColor, LocalLineWidth, Points, True);
            end;
          end;
        end;
      gldcActiveEdge:
        begin
          if LineColor = clBlack32 then
          begin
            LineWidth := OrdinaryGridLineThickness;
          end
          else
          begin
            LineWidth := ThickGridLineThickness;
          end;
          Row := SelectedRow;
          if Row >= RowCount then
          begin
            Dec(Row);
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            if ((LineColor <> clBlack32) and IsActive)
              or IsEdge then
            begin
              APoint := FrontPoints[ColIndex*2, LayerIndex];
              Points[0] := Convert2D_FrontPoint(ZoomBox, APoint);
              APoint := FrontPoints[ColIndex*2, LayerIndex+1];
              Points[1] := Convert2D_FrontPoint(ZoomBox, APoint);
              DrawBigPolyline32(BitMap, LineColor, LineWidth, Points, True);
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TModflowGrid.CheckRowHeights;
begin
  CheckSizeRatios(RowWidths, StrRows);
end;

procedure TModflowGrid.GetMinAndMax(const AnArray: TOneDRealArray;
  Out MinValue, MaxValue: double; out MinIndex, MaxIndex: integer);
var
  Index: integer;
begin
  Assert(Length(AnArray) > 0);
  MinValue := AnArray[0];
  MaxValue := AnArray[0];
  MinIndex:= 0;
  MaxIndex := 0;
  for Index := 1 to Length(AnArray) - 1 do
  begin
    if MaxValue < AnArray[Index] then
    begin
      MaxValue := AnArray[Index];
      MaxIndex := Index;
    end;
    if MinValue > AnArray[Index] then
    begin
      MinValue := AnArray[Index];
      MinIndex := Index;
    end;
  end;
end;

procedure TModflowGrid.CheckRowToColumnRatios;
var
  MinCol, MaxCol, MinRow, MaxRow: double;
  MinColIndex, MaxColIndex, MinRowIndex, MaxRowIndex: integer;
  WarningString: string;
begin
  GetMinAndMax(ColWidths, MinCol, MaxCol, MinColIndex, MaxColIndex);
  GetMinAndMax(RowWidths, MaxRow, MinRow, MaxRowIndex, MinRowIndex);
  MaxRow := -MaxRow;
  MinRow := -MinRow;
  WarningString := StrOneOrMoreCellsHa;
  if (MinCol <> 0) and (MaxRow/MinCol > 10) then
  begin
    frmErrorsAndWarnings.AddWarning(Model, WarningString,
      Format(StrColumn0dRow1, [MinColIndex+1, MaxRowIndex+1]));
//      'Column ' + IntToStr(MinColIndex+1) + ', '
//      + 'Row ' + IntToStr(MaxRowIndex+1));
  end;
  if (MinRow <> 0) and (MaxCol/MinRow > 10) then
  begin
    frmErrorsAndWarnings.AddWarning(Model, WarningString,
      Format(StrColumn0dRow1, [MaxColIndex+1, MinRowIndex+1]));
//      'Column ' + IntToStr(MaxColIndex+1) + ', '
//      + 'Row ' + IntToStr(MinRowIndex+1));
  end;
end;

constructor TModflowGrid.Create(Model: TBaseModel);
begin
  inherited;
  FCellElevationsNeedUpdating:= True;
  FCellPointsNeedUpdating:= True;
  LayerDirection := ldTopToBottom;
  RowDirection := rdNorthToSouth;
end;

procedure TModflowGrid.DrawFront(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  FrontPoints: T2DRealPointArray;
  LayerIndex: Integer;
  CellOutline: TPointArray;
  AColor, NewColor: TColor;
  PriorLayer: integer;
  ColumnIndex: integer;
  ColumnLimit, LayerLimit: integer;
  P: TPolygon32;
  MultiplePolygons: boolean;
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if ZoomBox.Height <= 2 then
  begin
    Exit;
  end;
  // for the time being, don't worry about coloring the grid.
  FrontPoints := FrontCellPoints(SelectedRow);
  if FrontPoints = nil then
  begin
    Exit;
  end;
  P := nil;
  MultiplePolygons := False;

  if (FrontDataSet <> nil) and (FrontDataSet.EvaluatedAt = eaBlocks) then
  begin
    case FrontDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColumnLimit := ColumnCount - 1;
          LayerLimit := LayerCount - 1;
        end;
      eaNodes:
        begin
          ColumnLimit := ColumnCount;
          LayerLimit := LayerCount;
        end;
    else
      begin
        Assert(False);
        ColumnLimit := -1;
        LayerLimit := -1;
      end;
    end;

    SetLength(CellOutline, 6);
    for ColumnIndex := 0 to ColumnLimit do
    begin
      if LayerCount > 0 then
      begin
        PriorLayer := 0;
        AColor := FrontCellColors[ColumnIndex, 0];
        NewColor := AColor;
        for LayerIndex := 1 to LayerLimit do
        begin
          NewColor := FrontCellColors[ColumnIndex, LayerIndex];
          if (NewColor <> AColor) then
          begin
            if AColor <> clWhite then
            begin
              case FrontDataSet.EvaluatedAt of
                eaBlocks:
                  begin
                    CellOutline[0] := Convert2D_FrontPoint(ZoomBox, FrontPoints[ColumnIndex*2,PriorLayer]);
                    CellOutline[1] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2+1,PriorLayer]);
                    CellOutline[2] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2+2,PriorLayer]);
                    CellOutline[3] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2+2,LayerIndex]);
                    CellOutline[4] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2+1,LayerIndex]);
                    CellOutline[5] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2,LayerIndex]);
                  end;
                eaNodes:
                  begin
                    Assert(False);
                  end;
              else
                begin
                  Assert(False);
                end;
              end;
              DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                0, CellOutline, P, MultiplePolygons, True);
            end;
            AColor := NewColor;
            PriorLayer := LayerIndex;

          end;
        end;

        if NewColor <> clWhite then
        begin
          case FrontDataSet.EvaluatedAt of
            eaBlocks:
              begin
                CellOutline[0] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2,PriorLayer]);
                CellOutline[1] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2+1,PriorLayer]);
                CellOutline[2] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2+2,PriorLayer]);
                CellOutline[3] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2+2,LayerCount]);
                CellOutline[4] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2+1,LayerCount]);
                CellOutline[5] := Convert2D_FrontPoint(ZoomBox,FrontPoints[ColumnIndex*2,LayerCount]);
              end;
            eaNodes:
              begin
                Assert(False);
              end;
          else
            begin
              Assert(False);
            end;
          end;
          DrawBigPolygon32(BitMap, Color32(NewColor), Color32(NewColor),
            0, CellOutline, P, MultiplePolygons, True);
        end;
      end;
    end;
  end;
  DrawOrdinaryFrontLayers(BitMap, ZoomBox, FrontPoints);
  DrawOrdinaryFrontColumns(BitMap, ZoomBox, FrontPoints);
  DrawFrontContours(ZoomBox, BitMap);
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.ModelSelection in [msModflowLGR , msModflowLGR2,
       msModflowFmp, msModflowOwhm2] then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          if (SelectedRow >= ChildModel.FirstRow)
            and (SelectedRow <= ChildModel.LastRow) then
          begin
            ChildModel.ModflowGrid.DrawFront(BitMap, ZoomBox);
          end;
        end;
      end;
    end;
  end;
end;

function TModflowGrid.SideCellPoints(Col: integer): T2DRealPointArray;
var
  RowIndex: Integer;
  LayerIndex: Integer;
begin
  result := nil;
  if (ColumnCount < 0) or (RowCount < 0) or (LayerCount < 0) then
  begin
    Exit;
  end;
  UpdateCellPoints;
  if CellPoints = nil then
  begin
    Exit;
  end;
  SetLength(result, RowCount*2+1, LayerCount+1);
  for RowIndex := 0 to RowCount*2 do
  begin
    for LayerIndex := 0 to LayerCount do
    begin
      result[RowIndex, LayerIndex].X :=
        CellPoints[Col*2+1, RowIndex, LayerIndex].Y;
      result[RowIndex, LayerIndex].Y :=
        CellPoints[Col*2+1, RowIndex, LayerIndex].Z;
    end;
  end;
end;


procedure TModflowGrid.DrawSide(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  SidePoints: T2DRealPointArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  CellOutline: TPointArray;
  AColor, NewColor: TColor;
  PriorLayer: integer;
  RowLimit, LayerLimit: integer;
  P: TPolygon32;
  MultiplePolygons: boolean;
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if ZoomBox.Width <= 2 then
  begin
    Exit;
  end;

  // for the time being, don't worry about coloring the grid.
  SidePoints := SideCellPoints(SelectedColumn);
  if SidePoints = nil then
  begin
    Exit;
  end;
  P := nil;
  MultiplePolygons := False;

  if (SideDataSet <> nil) and (SideDataSet.EvaluatedAt = eaBlocks) then
  begin
    case SideDataSet.EvaluatedAt of
      eaBlocks:
        begin
          RowLimit := RowCount - 1;
          LayerLimit := LayerCount - 1;
        end;
      eaNodes:
        begin
          RowLimit := RowCount;
          LayerLimit := LayerCount;
        end;
    else
      begin
        Assert(False);
        RowLimit := -1;
        LayerLimit := -1;
      end;
    end;

    SetLength(CellOutline, 6);
    for RowIndex := 0 to RowLimit do
    begin
      if LayerCount > 0 then
      begin
        PriorLayer := 0;
        AColor := SideCellColors[RowIndex, 0];
        NewColor := AColor;
        for LayerIndex := 1 to LayerLimit do
        begin
          NewColor := SideCellColors[RowIndex, LayerIndex];
          if (NewColor <> AColor) then
          begin
            if AColor <> clWhite then
            begin
              case SideDataSet.EvaluatedAt of
                eaBlocks:
                  begin
                    CellOutline[0] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2,PriorLayer]);
                    CellOutline[1] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2+1,PriorLayer]);
                    CellOutline[2] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2+2,PriorLayer]);
                    CellOutline[3] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2+2,LayerIndex]);
                    CellOutline[4] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2+1,LayerIndex]);
                    CellOutline[5] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2,LayerIndex]);
                  end;
                eaNodes:
                  begin
                    Assert(False);
                  end;
              else
                begin
                  Assert(False);
                end;
              end;
              DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                0, CellOutline, P, MultiplePolygons, True);
            end;
            AColor := NewColor;
            PriorLayer := LayerIndex;
          end;
        end;

        if NewColor <> clWhite then
        begin
          case SideDataSet.EvaluatedAt of
            eaBlocks:
              begin
                CellOutline[0] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2,PriorLayer]);
                CellOutline[1] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2+1,PriorLayer]);
                CellOutline[2] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2+2,PriorLayer]);
                CellOutline[3] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2+2,LayerCount]);
                CellOutline[4] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2+1,LayerCount]);
                CellOutline[5] := Convert2D_SidePoint(ZoomBox, SidePoints[RowIndex*2,LayerCount]);
              end;
            eaNodes:
              begin
                Assert(False);
              end;
          else
            begin
              Assert(False);
            end;
          end;
          DrawBigPolygon32(BitMap, Color32(NewColor), Color32(NewColor),
            0, CellOutline, P, MultiplePolygons, True);
        end;
      end;
    end;
  end;
  DrawOrdinarySideLayers(BitMap, ZoomBox, SidePoints);
  DrawOrdinarySideRows(BitMap, ZoomBox, SidePoints);
  DrawSideContours(ZoomBox, BitMap);
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.ModelSelection in [msModflowLGR, msModflowLGR2,
       msModflowFmp, msModflowOwhm2] then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          if (SelectedColumn >= ChildModel.FirstCol)
            and (SelectedColumn <= ChildModel.LastCol) then
          begin
            ChildModel.ModflowGrid.DrawSide(BitMap, ZoomBox);
          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.DrawTop(const BitMap: TPersistent;
  const ZoomBox: TQRbwZoomBox2);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModelItem: TChildModelItem;
begin
  inherited;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.ModelSelection in [msModflowLGR, msModflowLGR2,
      msModflowFmp, msModflowOwhm2] then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModelItem := LocalModel.ChildModels[ChildIndex];
        if ChildModelItem.ChildModel <> nil then
        begin
          ChildModelItem.ChildModel.ModflowGrid.DrawTop(BitMap, ZoomBox);
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.GetCellCornerElevations(const EvalAt: TEvaluatedAt;
  out Elevations: TThreeDRealArray);
var
  CCount, RCount, LCount: integer;
  ColIndex, RowIndex, LayerIndex: integer;
  CIndex, RIndex: integer;
  Elev1, Elev2, Elev3, Elev4: double;
  Weight1, Weight2, Weight3, Weight4: double;
begin
  case EvalAt of
    eaBlocks:
      begin
        CCount := ColumnCount + 1;
        RCount := RowCount + 1;
        LCount := LayerCount + 1;
        SetLength(Elevations, CCount, RCount, LCount);
        for ColIndex := 0 to CCount - 1 do
        begin
          if ColIndex = ColumnCount then
          begin
            CIndex := ColumnCount-1;
          end
          else
          begin
            CIndex := ColIndex;
          end;
          for RowIndex := 0 to RCount - 1 do
          begin
            if RowIndex = RowCount then
            begin
              RIndex := RowCount-1;
            end
            else
            begin
              RIndex := RowIndex;
            end;
            for LayerIndex := 0 to LCount - 1 do
            begin
              Elev1 := CellElevation[ZeroBasedID(LayerIndex, RIndex, CIndex)];
              Weight1 := ColumnWidth[CIndex];
              Weight3 := RowWidth[RIndex];
              if ColIndex > 0 then
              begin
                Elev2 := CellElevation[ZeroBasedID(LayerIndex, RIndex, ColIndex-1)];
                Weight2 := ColumnWidth[ColIndex-1];
              end
              else
              begin
                Elev2 := Elev1;
                Weight2 := Weight1;
              end;
              if RowIndex > 0 then
              begin
                Elev3 := CellElevation[ZeroBasedID(LayerIndex, RowIndex-1, CIndex)];
                Weight4 := RowWidth[RowIndex-1];
              end
              else
              begin
                Elev3 := Elev1;
                Weight4 := Weight3;
              end;
              if (ColIndex > 0) and (RowIndex > 0) then
              begin
                Elev4 := CellElevation[ZeroBasedID(LayerIndex, RowIndex-1, ColIndex-1)];
              end
              else
              begin
                Elev4 := Elev3;
              end;
              // bilinear interpolation.
              Elevations[ColIndex, RowIndex, LayerIndex] :=
                ((Weight3*(Elev1*Weight1 + Elev2*Weight2)/(Weight1 + Weight2))
                + (Weight4*(Elev3*Weight1 + Elev4*Weight2)/(Weight1 + Weight2)))
                / (Weight3 + Weight4);
            end;
          end;
        end;
      end;
    eaNodes:
      begin
        CCount := ColumnCount + 2;
        RCount := RowCount + 2;
        LCount := LayerCount + 2;
        SetLength(Elevations, CCount, RCount, LCount);
        // The corners of the interior _cells_ will just be the elevations.
        for ColIndex := 1 to CCount - 2 do
        begin
          for RowIndex := 1 to RCount - 2 do
          begin
            for LayerIndex := 0 to LCount - 1 do
            begin
              Elevations[ColIndex, RowIndex, LayerIndex] :=
                CellElevation[ZeroBasedID(LayerIndex, RowIndex-1, ColIndex-1)];
            end;
          end;
        end;
        // use linear interpolation along edges.
        for ColIndex := 1 to CCount - 2 do
        begin
          Weight1 := ColumnWidth[ColIndex];
          Weight2 := ColumnWidth[ColIndex-1];
          for LayerIndex := 0 to LCount - 1 do
          begin
            Elevations[ColIndex, 0, LayerIndex] :=
              (CellElevation[ZeroBasedID(LayerIndex, 0, ColIndex)]*Weight1
              + CellElevation[ZeroBasedID(LayerIndex, 0, ColIndex-1)]*Weight2)
              /(Weight1+Weight2);
            Elevations[ColIndex, RowCount + 1, LayerIndex] :=
              (CellElevation[ZeroBasedID(LayerIndex, RowCount-1, ColIndex)]*Weight1
              + CellElevation[ZeroBasedID(LayerIndex, RowCount-1, ColIndex-1)]*Weight2)
              /(Weight1+Weight2);
          end;
        end;
        for RowIndex := 1 to RCount - 2 do
        begin
          Weight1 := RowWidth[RowIndex];
          Weight2 := RowWidth[RowIndex-1];
          for LayerIndex := 0 to LCount - 1 do
          begin
            Elevations[0, RowIndex, LayerIndex] :=
              (CellElevation[ZeroBasedID(LayerIndex, RowIndex, 0)]*Weight1
              + CellElevation[ZeroBasedID(LayerIndex, RowIndex-1, 0)]*Weight2)
              /(Weight1+Weight2);
            Elevations[ColumnCount+1, RowIndex, LayerIndex] :=
              (CellElevation[ZeroBasedID(LayerIndex, RowIndex, ColumnCount-1)]*Weight1
              + CellElevation[ZeroBasedID(LayerIndex, RowIndex-1, ColumnCount-1)]*Weight2)
              /(Weight1+Weight2);
          end;
        end;
        // corners of mesh elevations are the elevations of the nearest cells.
        for LayerIndex := 0 to LCount - 1 do
        begin
          Elevations[0, 0, LayerIndex] := CellElevation[ZeroBasedID(LayerIndex, 0, 0)];
          Elevations[ColumnCount, 0, LayerIndex] :=
            CellElevation[ZeroBasedID(LayerIndex, 0, ColumnCount-1)];
          Elevations[0, RowCount, LayerIndex] :=
            CellElevation[ZeroBasedID(LayerIndex, RowCount-1, 0)];
          Elevations[ColumnCount, RowCount, LayerIndex] :=
            CellElevation[ZeroBasedID(LayerIndex, RowCount-1, ColumnCount-1)];
        end;
      end;
  else
    Assert(False);
  end;
end;

function TModflowGrid.GetCellElevation(const CellID: TZeroBasedID): real;
begin
  if (ColumnCount > 0) and (RowCount > 0) and (LayerCount > 0)  then
  begin
    if (FLayerElevations = nil)
      or (Length(FLayerElevations) <> ColumnCount)
      or (Length(FLayerElevations[0]) <> RowCount)
      or (Length(FLayerElevations[0,0]) <> LayerCount+1)
      then
    begin
      SetLength(FLayerElevations,ColumnCount,RowCount,LayerCount+1);
      UpdateCellElevations;
    end;
  end
  else
  begin
    result := 0;
    Exit;
  end;
  Assert((CellID.Column >= 0) and (CellID.Row >= 0) and (CellID.Layer >= 0));
  Assert((CellID.Column < ColumnCount) and (CellID.Row < RowCount) and (CellID.Layer < LayerCount+1));

  result := FLayerElevations[CellID.Column, CellID.Row, CellID.Layer];
end;

function TModflowGrid.GetCellThickness(const Column, Row, Layer: integer): real;
begin
  UpdateCellElevations;
  result := FLayerElevations[Column, Row, Layer]
    - FLayerElevations[Column, Row, Layer+1];
end;

function TModflowGrid.GetContainingLayer(ACol, ARow: integer;
  const AZPosition: real): integer;
begin
  result := GetContainingColumnOrRow(GetTwoDCellElevations(ACol, ARow), AZPosition);
end;

function TModflowGrid.GetElevationsNeedUpdating: Boolean;
begin
  result := FCellElevationsNeedUpdating;
end;

function TModflowGrid.GetLayerElevations: TThreeDRealArray;
begin
  UpdateCellElevations;
  result := FLayerElevations;
end;

function TModflowGrid.GetTwoDCellElevations(const Col,
  Row: integer): TOneDRealArray;
begin
  result := LayerElevations[Col, Row];
end;

function TModflowGrid.HighestElevation: real;
var
  ColIndex, RowIndex: integer;
begin
  UpdateCellElevations;
  if (ColumnCount > 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    result := CellElevation[ZeroBasedID(0,0,0)];
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        if result < CellElevation[ZeroBasedID(0,RowIndex,ColIndex)] then
        begin
          result := CellElevation[ZeroBasedID(0,RowIndex,ColIndex)]
        end;
      end;
    end;
  end
  else
  begin
    result := 1;
  end;
end;

procedure TModflowGrid.RecordColoredGrid;
var
  CellColors: TCellColors;
  LayerLength, RowLength, ColLength: integer;
  LayerIndex, RowIndex, ColIndex: integer;
  Red, Green, Blue: GLubyte;
  X, Y, Z: single;
//  XPositions, YPositions: TOneDRealArray;
//  ZPositions: TThreeDRealArray;
//  Index: integer;
  APoint: T3DRealPoint;
  procedure DrawLeftSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Top Center point
    APoint := CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Front point
    APoint := CellPoints[ColIndex*2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Front point
    APoint := CellPoints[ColIndex*2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Center point
    APoint := CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Back point
    APoint := CellPoints[ColIndex*2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Back point
    APoint := CellPoints[ColIndex*2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawRightSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Top Center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Front point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Front point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Back point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Back point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawBackSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Top Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Right point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Right point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Left point
    APoint := CellPoints[ColIndex*2,  RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Left point
    APoint := CellPoints[ColIndex*2,  RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawFrontSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Top Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Right point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Right point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Left point
    APoint := CellPoints[ColIndex*2  ,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Left point
    APoint := CellPoints[ColIndex*2  ,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawTopSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right back point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Center back point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left back point
    APoint := CellPoints[ColIndex*2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left point
    APoint := CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left front point
    APoint := CellPoints[ColIndex*2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // center front point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // right front point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawBottomSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right back point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Center back point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left back point
    APoint := CellPoints[ColIndex*2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left point
    APoint := CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left front point
    APoint := CellPoints[ColIndex*2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // center front point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // right front point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
var
  SelectedLayer, SelectedRow, SelectedColumn: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Assert(ThreeDDataSet <> nil);

    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;

    if frmGoPhast.PhastModel.ThreeDTimeList <> nil then
    begin
      SelectedLayer := frmGoPhast.Grid.SelectedLayer;
      SelectedRow := frmGoPhast.Grid.SelectedRow;
      SelectedColumn := frmGoPhast.Grid.SelectedColumn;
      try
        frmGoPhast.PhastModel.UpdateThreeDTimeDataSet(frmGoPhast.PhastModel.ThreeDTimeList,
          frmGoPhast.PhastModel.ThreeDDisplayTime);
        if frmGoPhast.PhastModel.TopTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateTopTimeDataSet(frmGoPhast.PhastModel.TopTimeList,
            frmGoPhast.PhastModel.TopDisplayTime);
        end;
        if frmGoPhast.PhastModel.FrontTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateFrontTimeDataSet(frmGoPhast.PhastModel.FrontTimeList,
            frmGoPhast.PhastModel.FrontDisplayTime);
        end;
        if frmGoPhast.PhastModel.SideTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateSideTimeDataSet(frmGoPhast.PhastModel.SideTimeList,
            frmGoPhast.PhastModel.SideDisplayTime);
        end;
      finally
        frmGoPhast.Grid.SelectedLayer := SelectedLayer;
        frmGoPhast.Grid.SelectedRow := SelectedRow;
        frmGoPhast.Grid.SelectedColumn := SelectedColumn;
      end;
    end
    else
    begin
      ThreeDDataSet.Initialize;
    end;

    if ThreeDDataSet = nil then
    begin
      Exit;
    end;

    glNewList(CellsGLIndex, GL_COMPILE);
    Assert(ThreeDDataSet.EvaluatedAt = eaBlocks);
    LayerLength := ThreeDDataSet.LayerCount;
    RowLength := ThreeDDataSet.RowCount;
    ColLength := ThreeDDataSet.ColumnCount;

    SetLength(CellColors, LayerLength, RowLength, ColLength);

    Update3DCellColors(CellColors);

    for LayerIndex := 0 to LayerLength - 1 do
    begin
      for RowIndex := 0 to RowLength - 1 do
      begin
        for ColIndex := 0 to ColLength - 1 do
        begin
          if CellColors[LayerIndex, RowIndex, ColIndex] = clWhite then
            Continue;

          ExtractColorComponents(CellColors[LayerIndex, RowIndex, ColIndex],
            Red, Green, Blue);

          glColor3ub(Red, Green, Blue);

          if (ColIndex = ColLength - 1) or
            (CellColors[LayerIndex, RowIndex, ColIndex + 1] = clWhite) then
          begin
            DrawRightSide;
          end;
          if (ColIndex = 0) or
            (CellColors[LayerIndex, RowIndex, ColIndex - 1] = clWhite) then
          begin
            DrawLeftSide;
          end;
          if (RowIndex = RowLength - 1) or
            (CellColors[LayerIndex, RowIndex + 1, ColIndex] = clWhite) then
          begin
            DrawFrontSide;
          end;
          if (RowIndex = 0) or
            (CellColors[LayerIndex, RowIndex - 1, ColIndex] = clWhite) then
          begin
            DrawBackSide;
          end;
          if (LayerIndex = LayerLength - 1) or
            (CellColors[LayerIndex + 1, RowIndex, ColIndex] = clWhite) then
          begin
            DrawBottomSide;
          end;
          if (LayerIndex = 0) or
            (CellColors[LayerIndex - 1, RowIndex, ColIndex] = clWhite) then
          begin
            DrawTopSide;
          end;
        end;
      end;
    end;
    glEndList;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TModflowGrid.RecordFront;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
  APoint: T3DRealPoint;
  LayerBoundaries: TOneDIntegerArray;
  DividedUnits: Boolean;
  LineWidth: single;
  UnitIndex: Integer;
  GridLine: TGLCoordArray;
  PointIndex: integer;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glNewList(FrontGridGLIndex, GL_COMPILE);
  try
    // Draw grid lines on selected Row.
    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;

    IdentifyDividedUnitBoundaries(LayerBoundaries, DividedUnits);

    RowIndex := DisplayRow;
    if (RowIndex >= 0) and (RowIndex <= RowCount) then
    begin
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex = ColumnCount) or (ColumnIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINES);
        LayerIndex := 0;
        APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
        X := APoint.X;
        Y := APoint.Y;
        Z := APoint.Z;
        glVertex3f(X, Y, Z);
        LayerIndex := LayerCount;
        APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
        X := APoint.X;
        Y := APoint.Y;
        Z := APoint.Z;
        glVertex3f(X, Y, Z);
        glEnd;
      end;

      SetLength(GridLine, ColumnCount*2+1);
      glVertexPointer(3, GL_FLOAT, 0, GridLine);
      UnitIndex := 0;
      for LayerIndex := 0 to LayerCount do
      begin
        SetLayerLineWidth(LineWidth, LayerIndex, UnitIndex,
          DividedUnits, LayerBoundaries);
        if LineWidth <> ThinLine then
        begin
          LineWidth := ThickLine;
        end;
        glLineWidth(LineWidth);

        PointIndex := 0;
//        glBegin(GL_LINE_STRIP);
        for ColumnIndex := 0 to ColumnCount do
        begin
          APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
//          X := APoint.X;
//          Y := APoint.Y;
//          Z := APoint.Z;
//          glVertex3f(X, Y, Z);

          GridLine[PointIndex].X := APoint.X;
          GridLine[PointIndex].Y := APoint.Y;
          GridLine[PointIndex].Z := APoint.Z;
          Inc(PointIndex);

          if ColumnIndex < ColumnCount then
          begin
            APoint := CellPoints[ColumnIndex*2+1,RowIndex*2,LayerIndex];
//            X := APoint.X;
//            Y := APoint.Y;
//            Z := APoint.Z;
//            glVertex3f(X, Y, Z);

            GridLine[PointIndex].X := APoint.X;
            GridLine[PointIndex].Y := APoint.Y;
            GridLine[PointIndex].Z := APoint.Z;
            Inc(PointIndex);
          end;
        end;
//        glEnd;
        glDrawArrays(GL_LINE_STRIP, 0, PointIndex);
      end;
    end;
  finally
    glEndList;
  end;
end;

procedure TModflowGrid.RecordShell;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
  APoint: T3DRealPoint;
begin
  glNewList(GridShellGLIndex, GL_COMPILE);
  try
    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;
    glLineWidth(ThinLine);
    glColor3f(0.0, 0.0, 0.0);
    // Draw left side;
    glBegin(GL_LINE_LOOP);
    ColumnIndex := 0;
    LayerIndex := 0;
    for RowIndex := 0 to Length(CellPoints[0]) -1 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := Length(CellPoints[0,0])-1;
    for RowIndex := Length(CellPoints[0]) -1 downto 0 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw right side;
    glBegin(GL_LINE_LOOP);
    ColumnIndex := Length(CellPoints)-1;
    LayerIndex := 0;
    for RowIndex := 0 to Length(CellPoints[0]) -1 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := Length(CellPoints[0,0])-1;
    for RowIndex := Length(CellPoints[0]) -1 downto 0 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw front side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    RowIndex := 0;
    for ColumnIndex := 0 to Length(CellPoints) -1 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := Length(CellPoints[0,0])-1;
    for ColumnIndex := Length(CellPoints) -1 downto 0 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw back side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    RowIndex := Length(CellPoints[0])-1;
    for ColumnIndex := 0 to Length(CellPoints) -1 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := Length(CellPoints[0,0])-1;
    for ColumnIndex := Length(CellPoints) -1 downto 0 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    glEnd;
  finally
    glEndList;
  end;
end;

procedure TModflowGrid.RecordSide;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
  APoint: T3DRealPoint;
  LayerBoundaries: TOneDIntegerArray;
  DividedUnits: Boolean;
  LineWidth: single;
  UnitIndex: Integer;
  GridLine: TGLCoordArray;
  PointIndex: integer;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glNewList(SideGridGLIndex, GL_COMPILE);
  try
    // Draw grid lines on selected column.
    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;
    IdentifyDividedUnitBoundaries(LayerBoundaries, DividedUnits);

    ColumnIndex := DisplayColumn;
    if (ColumnIndex >= 0) and (ColumnIndex <= ColumnCount) then
    begin
//    Vertical lines
      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex = RowCount) or (RowIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINES);

        LayerIndex := 0;
        APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
        X := APoint.X;
        Y := APoint.Y;
        Z := APoint.Z;
        glVertex3f(X, Y, Z);
        LayerIndex := LayerCount;
        APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
        X := APoint.X;
        Y := APoint.Y;
        Z := APoint.Z;
        glVertex3f(X, Y, Z);
        glEnd;
      end;
      // "Horizontal" lines
      SetLength(GridLine, RowCount*2+1);
      glVertexPointer(3, GL_FLOAT, 0, GridLine);
      UnitIndex := 0;
      for LayerIndex := 0 to LayerCount do
      begin
        SetLayerLineWidth(LineWidth, LayerIndex, UnitIndex,
          DividedUnits, LayerBoundaries);
        if LineWidth <> ThinLine then
        begin
          LineWidth := ThickLine;
        end;
        glLineWidth(LineWidth);
//        glBegin(GL_LINE_STRIP);
        PointIndex := 0;
        for RowIndex := 0 to RowCount do
        begin
          APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
//          X := APoint.X;
//          Y := APoint.Y;
//          Z := APoint.Z;
//          glVertex3f(X, Y, Z);

          GridLine[PointIndex].X := APoint.X;
          GridLine[PointIndex].Y := APoint.Y;
          GridLine[PointIndex].Z := APoint.Z;
          Inc(PointIndex);

          if RowIndex < RowCount then
          begin
            APoint := CellPoints[ColumnIndex*2,RowIndex*2+1,LayerIndex];
//            X := APoint.X;
//            Y := APoint.Y;
//            Z := APoint.Z;
//            glVertex3f(X, Y, Z);

            GridLine[PointIndex].X := APoint.X;
            GridLine[PointIndex].Y := APoint.Y;
            GridLine[PointIndex].Z := APoint.Z;
            Inc(PointIndex);
          end;
        end;
//        glEnd;
        glDrawArrays(GL_LINE_STRIP, 0, PointIndex);
      end;
    end;
  finally
    glEndList;
  end;
end;

procedure TModflowGrid.RecordTop;
var
//  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
  APoint: T3DRealPoint;
  GridLine: TGLCoordArray;
  PointIndex: integer;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glNewList(TopGridGLIndex, GL_COMPILE);
  try
  // Draw grid lines on selected layer.
    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;

    LayerIndex := DisplayLayer;
    if (LayerIndex >= 0) and (LayerIndex <= LayerCount) then
    begin
      SetLength(GridLine, RowCount*2+1);
      glVertexPointer(3, GL_FLOAT, 0, GridLine);
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex = ColumnCount) or (ColumnIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        PointIndex := 0;
//        glBegin(GL_LINE_STRIP);
        for RowIndex := 0 to RowCount do
        begin
          APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
//          X := APoint.X;
//          Y := APoint.Y;
//          Z := APoint.Z;
//          glVertex3f(X, Y, Z);

          GridLine[PointIndex].X := APoint.X;
          GridLine[PointIndex].Y := APoint.Y;
          GridLine[PointIndex].Z := APoint.Z;
          Inc(PointIndex);

          if RowIndex < RowCount then
          begin
            APoint := CellPoints[ColumnIndex*2,RowIndex*2+1,LayerIndex];
//            X := APoint.X;
//            Y := APoint.Y;
//            Z := APoint.Z;
//            glVertex3f(X, Y, Z);

            GridLine[PointIndex].X := APoint.X;
            GridLine[PointIndex].Y := APoint.Y;
            GridLine[PointIndex].Z := APoint.Z;
            Inc(PointIndex);
          end;
        end;
//        glEnd;
        glDrawArrays(GL_LINE_STRIP, 0, PointIndex);
      end;

      SetLength(GridLine, ColumnCount*2+1);
      glVertexPointer(3, GL_FLOAT, 0, GridLine);
      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex = RowCount) or (RowIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        PointIndex := 0;
//        glBegin(GL_LINE_STRIP);
        for ColumnIndex := 0 to ColumnCount do
        begin
          APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
//          X := APoint.X;
//          Y := APoint.Y;
//          Z := APoint.Z;
//          glVertex3f(X, Y, Z);

          GridLine[PointIndex].X := APoint.X;
          GridLine[PointIndex].Y := APoint.Y;
          GridLine[PointIndex].Z := APoint.Z;
          Inc(PointIndex);

          if ColumnIndex < ColumnCount then
          begin
            APoint := CellPoints[ColumnIndex*2+1,RowIndex*2,LayerIndex];
//            X := APoint.X;
//            Y := APoint.Y;
//            Z := APoint.Z;
//            glVertex3f(X, Y, Z);

            GridLine[PointIndex].X := APoint.X;
            GridLine[PointIndex].Y := APoint.Y;
            GridLine[PointIndex].Z := APoint.Z;
            Inc(PointIndex);
          end;
        end;
//        glEnd;
        glDrawArrays(GL_LINE_STRIP, 0, PointIndex);
      end;
    end;

  finally
    glEndList;
  end;
end;

procedure TModflowGrid.SetCellElevation(const CellID: TZeroBasedID;
  const Value: real);
begin
  FLayerElevations[CellID.Column, CellID.Row, CellID.Layer] := Value;
  LayersChanged;
end;

procedure TModflowGrid.SetCellThickness(const Column, Row, Layer: integer;
  const Value: real);
var
  Delta: double;
begin
  Delta := Value - CellThickness[Column, Row, Layer];
  if Delta <> 0 then
  begin
    LayerElevations[Column, Row, Layer+1] := LayerElevations[Column, Row, Layer+1] + Delta;
    LayersChanged;
  end;
end;

procedure TModflowGrid.SetFrontContourDataSet(const Value: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  inherited;
  if (Value <> nil) and (Value.Name = '') then
  begin
    Exit;
  end;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataArray := GetChildDataArray(Value, ChildModel);
          if (Value <> nil) and (ChildDataArray <> nil) then
          begin
            ChildDataArray.ContourLimits := Value.ContourLimits;
            ChildDataArray.Contours := Value.Contours;
          end;
          ChildModel.Grid.FrontContourDataSet := ChildDataArray;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.SetFrontDataSet(const Value: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  inherited;
  if (Value <> nil) and (Value.Name = '') then
  begin
    Exit;
  end;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataArray := GetChildDataArray(Value, ChildModel);
          if (Value <> nil) and (ChildDataArray <> nil) then
          begin
            ChildDataArray.Limits := Value.Limits;
          end;
          ChildModel.Grid.FrontDataSet := ChildDataArray;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.SetLayerElevations(const Value: TThreeDRealArray);
var
  FirstLength: integer;
  SecondLength: integer;
  ThirdLength: integer;
begin
  if Value = nil then
  begin
    FirstLength := 0;
  end
  else
  begin
    FirstLength := Length(Value);
  end;
  if FirstLength <= 0 then
  begin
    SecondLength := 0
  end
  else
  begin
    SecondLength := Length(Value[0]);
  end;
  if SecondLength <= 0 then
  begin
    ThirdLength := 0
  end
  else
  begin
    ThirdLength := Length(Value[0,0]);
  end;
  LayerCount := ThirdLength-1;
  FLayerElevations := Value;
  SetLength(FLayerElevations, FirstLength, SecondLength, ThirdLength);
  LayersChanged;
end;

function TModflowGrid.ThreeDElementCorner(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  Z1: Real;
  Z2: Real;
  Distance1: Real;
  Distance2: Real;
  TotalDistance: Real;
  Z3: Real;
  Z4: Real;
begin
  result.X := ColumnPosition[Column];
  result.Y := RowPosition[Row];

  if Column = ColumnCount then
  begin
    if Row = RowCount then
    begin
      result.Z := CellElevation[ZeroBasedID(Layer, Row-1, Column-1)];
      Exit;
    end
    else
    begin
      Z1 := CellElevation[ZeroBasedID(Layer, Row, Column-1)];
    end;
  end
  else
  begin
    if Row = RowCount then
    begin
      Z1 := CellElevation[ZeroBasedID(Layer, Row-1, Column)];
      if Column > 0 then
      begin
        Z2 := CellElevation[ZeroBasedID(Layer, Row-1, Column-1)];
        Distance1 := ColumnWidth[Column];
        Distance2 := ColumnWidth[Column-1];
        TotalDistance := Distance1 + Distance2;
        if TotalDistance = 0 then
        begin
          Z1 := (Z1+Z2)/2;
        end
        else
        begin
          Z1 := Z1 + Distance1/TotalDistance*(Z2-Z1);
        end;
      end;
      result.Z := Z1;
      Exit;
    end
    else
    begin
      Z1 := CellElevation[ZeroBasedID(Layer, Row, Column)];
      if Column > 0 then
      begin
        Z2 := CellElevation[ZeroBasedID(Layer, Row, Column-1)];
        Distance1 := ColumnWidth[Column];
        Distance2 := ColumnWidth[Column-1];
        TotalDistance := Distance1 + Distance2;
        if TotalDistance = 0 then
        begin
          Z1 := (Z1+Z2)/2;
        end
        else
        begin
          Z1 := Z1 + Distance1/TotalDistance*(Z2-Z1);
        end;
      end;
    end;
  end;

  if Row > 0 then
  begin
    if Column = ColumnCount then
    begin
      Z3 := CellElevation[ZeroBasedID(Layer, Row-1, Column-1)];
    end
    else
    begin
      Z3 := CellElevation[ZeroBasedID(Layer, Row-1, Column)];
      if Column > 0 then
      begin
        Z4 := CellElevation[ZeroBasedID(Layer, Row-1, Column-1)];
        Distance1 := ColumnWidth[Column];
        Distance2 := ColumnWidth[Column-1];
        TotalDistance := Distance1 + Distance2;
        if TotalDistance = 0 then
        begin
          Z3 := (Z3+Z4)/2;
        end
        else
        begin
          Z3 := Z3 + Distance1/TotalDistance*(Z4-Z3);
        end;
      end;
    end;
    Distance1 := RowWidth[Row];
    Distance2 := RowWidth[Row-1];
    TotalDistance := Distance1 + Distance2;
    if TotalDistance = 0 then
    begin
      result.Z := (Z1+Z3)/2;
    end
    else
    begin
      result.Z := Z1 + Distance1/TotalDistance*(Z3-Z1);
    end;
  end
  else
  begin
    result.Z := Z1;
  end;
end;

procedure TModflowGrid.SetLayerLineWidth(var LineWidth: single;
  LayerIndex: Integer; var UnitIndex: Integer; DividedUnits: Boolean;
  LayerBoundaries: TOneDIntegerArray);
begin
  if Model is TChildModel then
  begin
    LineWidth := OrdinaryGridLineThickness;
  end
  else if DividedUnits then
  begin
    if LayerBoundaries[UnitIndex] = LayerIndex then
    begin
      LineWidth := ThickGridLineThickness;
      Inc(UnitIndex);
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
  end
  else
  begin
    if (LayerIndex mod 10 = 0) or (LayerIndex = LayerCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
  end;
end;

procedure TModflowGrid.SetSideContourDataSet(const Value: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  inherited;
  if (Value <> nil) and (Value.Name = '') then
  begin
    Exit;
  end;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataArray := GetChildDataArray(Value, ChildModel);
          if (Value <> nil) and (ChildDataArray <> nil) then
          begin
            ChildDataArray.ContourLimits := Value.ContourLimits;
            ChildDataArray.Contours := Value.Contours;
          end;
          ChildModel.Grid.SideContourDataSet := ChildDataArray;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.SetSideDataSet(const Value: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  inherited;
  if (Value <> nil) and (Value.Name = '') then
  begin
    Exit;
  end;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataArray := GetChildDataArray(Value, ChildModel);
          if (Value <> nil) and (ChildDataArray <> nil) then
          begin
            ChildDataArray.Limits := Value.Limits;
          end;
          ChildModel.Grid.SideDataSet := ChildDataArray;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.SetThreeDContourDataSet(const Value: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  inherited;
  if (Value <> nil) and (Value.Name = '') then
  begin
    Exit;
  end;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataArray := GetChildDataArray(Value, ChildModel);
          if (Value <> nil) and (ChildDataArray <> nil) then
          begin
            ChildDataArray.ContourLimits := Value.ContourLimits;
            ChildDataArray.Contours := Value.Contours;
          end;
          ChildModel.Grid.ThreeDContourDataSet := ChildDataArray;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.SetThreeDDataSet(const Value: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  inherited;
  if (Value <> nil) and (Value.Name = '') then
  begin
    Exit;
  end;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataArray := GetChildDataArray(Value, ChildModel);
          if (Value <> nil) and (ChildDataArray <> nil) then
          begin
            ChildDataArray.Limits := Value.Limits;
          end;
          ChildModel.Grid.ThreeDDataSet := ChildDataArray;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.SetTopContourDataSet(const Value: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  inherited;
  if (Value <> nil) and (Value.Name = '') then
  begin
    Exit;
  end;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataArray := GetChildDataArray(Value, ChildModel);
          if (Value <> nil) and (ChildDataArray <> nil) then
          begin
            ChildDataArray.ContourLimits := Value.ContourLimits;
            ChildDataArray.Contours := Value.Contours;
          end;
          ChildModel.Grid.TopContourDataSet := ChildDataArray;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.SetTopDataSet(const Value: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  inherited;
  if (Value <> nil) and (Value.Name = '') then
  begin
    Exit;
  end;
  if Model is TPhastModel then
  begin
    LocalModel := TPhastModel(Model);
    if LocalModel.LgrUsed then
    begin
      for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataArray := GetChildDataArray(Value, ChildModel);
          if (Value <> nil) and (ChildDataArray <> nil) then
          begin
            ChildDataArray.Limits := Value.Limits;
          end;
          ChildModel.Grid.TopDataSet := ChildDataArray;
        end;
      end;
    end;
  end;
end;

procedure TModflowGrid.IdentifyDividedUnitBoundaries(
  out LayerBoundaries: TOneDIntegerArray; out DividedUnits: Boolean);
var
  LayerGroup: TLayerGroup;
  UnitIndex2: Integer;
  UnitCount: Integer;
begin
  with frmGoPhast.PhastModel do
  begin
    DividedUnits := LayerStructure.LayerCount + 1 <> LayerStructure.Count;
  end;
  if DividedUnits then
  begin
    SetLength(LayerBoundaries, frmGoPhast.PhastModel.LayerStructure.Count);
    UnitCount := -1;
    for UnitIndex2 := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure.Items[UnitIndex2] as TLayerGroup;
      UnitCount := UnitCount + LayerGroup.LayerCount;
      LayerBoundaries[UnitIndex2] := UnitCount;
    end;
  end
  else
  begin
    SetLength(LayerBoundaries, 0);
  end;
end;

function TModflowGrid.LayerThickness(Layer, Row, Column: Integer): double;
begin
  Result := LayerElevations[Column, Row, Layer]
    - LayerElevations[Column, Row, Layer+1];
end;

function TModflowGrid.LowestElevation: real;
var
  ColIndex, RowIndex: integer;
begin
  UpdateCellElevations;
  if (ColumnCount > 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    result := CellElevation[ZeroBasedID(LayerCount,0,0)];
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        if result > CellElevation[ZeroBasedID(LayerCount,RowIndex,ColIndex)] then
        begin
          result := CellElevation[ZeroBasedID(LayerCount,RowIndex,ColIndex)]
        end;
      end;
    end;
  end
  else
  begin
    result := 1;
  end;
end;

function TModflowGrid.NearestLayerPosition(ACol, ARow: integer;
  const AZPosition: real; const First: integer = -1; const Last: integer = -1): integer;
begin
  result := NearestColumnOrRow(GetTwoDCellElevations(ACol, ARow), AZPosition, First, Last);
end;

end.
