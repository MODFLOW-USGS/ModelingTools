unit ModflowDiscretizationWriterUnit;

interface

uses System.Classes, SysUtils, CustomModflowWriterUnit, PhastModelUnit;

type
  TModflowDiscretizationWriter = class(TCustomModflowWriter)
  private
//    FNameOfFile: string;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteIDomain;
    procedure CheckConnectivity;
  public
    class function Extension: string; override;
    procedure WriteFile(const AFileName: string);
  end;

  TMf6DisvWriter = class(TCustomModflowWriter)
  private
//    FNameOfFile: string;
    procedure WriteDataSet0;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WriteGridData;
    procedure WriteVertices;
    procedure WriteCells;
    procedure CheckElevations;
    procedure CheckConnectivity;
  public
    class function Extension: string; override;
    procedure WriteFile(const AFileName: string);
  end;

resourcestring
  StrInvalidSelectionOf = 'Invalid selection of time unit';
  StrTheFarmProcessReq = 'The farm process requires that the time unit be se' +
  't to days if rooting depth or consumptive use is to be calculated from cli' +
  'mate data.';

implementation

uses ModflowUnitNumbers, frmProgressUnit, Forms,
  ModflowOptionsUnit, GoPhastTypes, ModflowPackageSelectionUnit,
  frmErrorsAndWarningsUnit, FastGEO, DataSetUnit, ModflowIrregularMeshUnit,
  MeshRenumberingTypes, ModflowTimeUnit, System.Math, System.IOUtils,
  DataSetNamesUnit;

resourcestring
  StrWritingDiscretizati = 'Writing Discretization Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
  StrCheckingColumnWi = '  Checking column widths.';
  StrCheckingRowHeigh = '  Checking row height.';
  StrCheckingRowToCo = '  Checking row to column size ratios.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
//  StrWritingDataSet6 = '  Writing Data Set 6.';
  StrCheckingElevation = '  Checking elevations.';
  StrDIS8 = 'DIS6';
  StrWritingDISVPackage = 'Writing DISV package input';
  StrWritingOptions = 'Writing Options';
  StrWritingDimensions = 'Writing Dimensions';
  StrWritingVertices = 'Writing Vertices';
  StrWritingCells = 'Writing Cells';
  StrOverlappingLayers = 'Overlapping layers';
  StrTheBottomOfCell = 'The bottom of cell, %0:d, in layer %1:d (%2:g), is a' +
  'bove the bottom of the cell in layer %3:d (%4:g).';
  StrNoActiveCells = 'No active cells';
  StrMODFLOW6RequiresT = 'MODFLOW 6 requires that you must have some active ' +
  'cells in your model.';
//  StrWritingDataSet7 = '  Writing Data Set 7.';
  StrThereIsNoHydrauli = 'At least one of the following active cells is '
    + 'hydraulically isolated.';
  StrLayerRowColumn = '(Layer, Row, Column) = (%0:d, %1:d, %2:d) (Number of ' +
    'connected cells = %3:d) and (Layer, Row, Column) = (%4:d, %5:d, %6:d)';
  StrLayerCell = '(Layer, Cell) = (%0:d, %1:d) (Number of ' +
    'connected cells = %2:d) and (Layer, Cell) = (%3:d, %4:d)';

{ TModflowDiscretizationWriter }

procedure TModflowDiscretizationWriter.CheckConnectivity;
var
  DataArray: TDataArray;
  ActiveCells: array of array of array of integer;
//  MFLayer: Integer;
  Queue: TActiveCellQueue;
  FoundFirst: Boolean;
  ACell: TActiveCell;
  MfLayerCount: integer;
  LayerIndex: Integer;
  RowIndex: integer;
  ColIndex: integer;
  NewCell: TActiveCell;
  FirstCol: Integer;
  FirstRow: Integer;
  FirstLayer: Integer;
  CellCount: Integer;
  LIndex: Integer;
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  DataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  DataArray.Initialize;
  MfLayerCount := Model.ModflowLayerCount;
  SetLength(ActiveCells, DataArray.LayerCount, DataArray.RowCount, DataArray.ColumnCount);
  Queue := TActiveCellQueue.Create(True);
  try
    FoundFirst := False;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      begin
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            ActiveCells[LayerIndex,RowIndex,ColIndex] :=
              Sign(DataArray.IntegerData[LayerIndex,RowIndex,ColIndex]);

            if not FoundFirst
              and (ActiveCells[LayerIndex,RowIndex,ColIndex] = 1) then
            begin
              ACell := TActiveCell.Create;
              ACell.MFLayer := LayerIndex;
              ACell.Layer := LayerIndex;
              ACell.Row := RowIndex;
              ACell.Column := ColIndex;
              Queue.Enqueue(ACell);
              FoundFirst := True;
              ActiveCells[LayerIndex,RowIndex,ColIndex] := 2;
            end;
          end;
        end;
      end;
    end;
    repeat
      CellCount := 1;
      FirstCol := -1;
      FirstRow := -1;
      FirstLayer := -1;
      FoundFirst := False;
      While Queue.Count > 0 do
      begin
        ACell := Queue.Peek;
        if not FoundFirst then
        begin
          FirstCol := ACell.Column;
          FirstRow := ACell.Row;
          FirstLayer := ACell.Layer;
          FoundFirst := True;
        end;
        if ACell.Layer > 0 then
        begin
          for LIndex := ACell.Layer-1 downto 0 do
          begin
            if (ActiveCells[LIndex,ACell.Row,ACell.Column] = 1) then
            begin
              NewCell := TActiveCell.Create;
              NewCell.Layer := LIndex;
              NewCell.Row := ACell.Row;
              NewCell.Column := ACell.Column;
              Queue.Enqueue(NewCell);
              ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
              Inc(CellCount);
              break;
            end
            else if (ActiveCells[LIndex,ACell.Row,ACell.Column] = 0) then
            begin
              break;
            end;
          end;
        end;
        if ACell.Layer < MfLayerCount-1 then
        begin
          for LIndex := ACell.Layer+1 to MfLayerCount - 1 do
          begin
            if (ActiveCells[LIndex,ACell.Row,ACell.Column] = 1) then
            begin
              NewCell := TActiveCell.Create;
              NewCell.Layer := LIndex;
              NewCell.Row := ACell.Row;
              NewCell.Column := ACell.Column;
              Queue.Enqueue(NewCell);
              ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
              Inc(CellCount);
              break;
            end
            else if (ActiveCells[LIndex,ACell.Row,ACell.Column] = 0) then
            begin
              break;
            end;
          end;
        end;
        if ACell.Row > 0 then
        begin
          if (ActiveCells[ACell.Layer,ACell.Row-1,ACell.Column] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.Layer := ACell.Layer;
            NewCell.Row := ACell.Row-1;
            NewCell.Column := ACell.Column;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        if ACell.Row < DataArray.RowCount-1 then
        begin
          if (ActiveCells[ACell.Layer,ACell.Row+1,ACell.Column] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.Layer := ACell.Layer;
            NewCell.Row := ACell.Row+1;
            NewCell.Column := ACell.Column;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        if ACell.Column > 0 then
        begin
          if (ActiveCells[ACell.Layer,ACell.Row,ACell.Column-1] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.Layer := ACell.Layer;
            NewCell.Row := ACell.Row;
            NewCell.Column := ACell.Column-1;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        if ACell.Column < DataArray.ColumnCount-1 then
        begin
          if (ActiveCells[ACell.Layer,ACell.Row,ACell.Column+1] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.Layer := ACell.Layer;
            NewCell.Row := ACell.Row;
            NewCell.Column := ACell.Column+1;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        Queue.Dequeue;
      end;
      if FoundFirst then
      begin
        for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
        begin
          begin
            for RowIndex := 0 to DataArray.RowCount - 1 do
            begin
              for ColIndex := 0 to DataArray.ColumnCount - 1 do
              begin
                if (ActiveCells[LayerIndex,RowIndex,ColIndex] = 1) then
                begin
                  frmErrorsAndWarnings.AddWarning(Model, StrThereIsNoHydrauli,
                    Format(StrLayerRowColumn,
                    [FirstLayer+1, FirstRow+1, FirstCol+1, CellCount,
                    LayerIndex+1, RowIndex+1, ColIndex+1]));
                  ACell := TActiveCell.Create;
                  ACell.Layer := LayerIndex;
                  ACell.Layer := LayerIndex;
                  ACell.Row := RowIndex;
                  ACell.Column := ColIndex;
                  Queue.Enqueue(ACell);
                  ActiveCells[LayerIndex,RowIndex,ColIndex] := 2;
                  break;
                end;
              end;
              if Queue.Count > 0 then
              begin
                Break;
              end;
            end;
          end;
          if Queue.Count > 0 then
          begin
            Break;
          end;
        end;
      end;
    until Queue.Count = 0;
  finally
    Queue.Free;
  end;
end;

class function TModflowDiscretizationWriter.Extension: string;
begin
  result := '.dis';
end;

procedure TModflowDiscretizationWriter.WriteFile(const AFileName: string);
var
  FTYPE: string;
  SpeciesIndex: Integer;
  AGwtFileName: string;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSelectionOf);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTooManyStressPeri);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTimeStepToShort);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoActiveCells);

  if Model.ModelSelection = msModflow2015 then
  begin
    FTYPE := StrDIS8;
  end
  else
  begin
    FTYPE := StrDIS;
  end;
  if Model.PackageGeneratedExternally(FTYPE) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  if Model.ModelSelection = msModflow2015 then
  begin
    WriteToNameFile(FTYPE, -1, FNameOfFile, foInput, Model, False, 'DIS');
  end
  else
  begin
    WriteToNameFile(FTYPE, Model.UnitNumbers.UnitNumber(FTYPE),
      FNameOfFile, foInput, Model);
  end;
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingDiscretizati);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;

    // data set 2
    if Model.ModelSelection <> msModflow2015 then
    begin
      frmProgressMM.AddMessage(StrWritingDataSet2);
      Model.WriteLAYCB(self);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;

    if Model.ModelSelection = msModflow2015 then
    begin
      NewLine;
      WriteBeginGridData;
    end;

    // data set 3
    frmProgressMM.AddMessage(StrWritingDataSet3);
    Model.ModflowGrid.WriteDELR(self);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // data set 4
    frmProgressMM.AddMessage(StrWritingDataSet4);
    Model.ModflowGrid.WriteDELC(self);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrCheckingColumnWi);
    Model.ModflowGrid.CheckColumnWidths;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrCheckingRowHeigh);
    Model.ModflowGrid.CheckRowHeights;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrCheckingRowToCo);
    Model.ModflowGrid.CheckRowToColumnRatios;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // data set 5
    frmProgressMM.AddMessage(StrWritingDataSet5);
    Model.ModflowGrid.WriteTOP(self);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Model.DataArrayManager.CacheDataArrays;

    // data set 6
    frmProgressMM.AddMessage(StrWritingDataSet6);
    Model.ModflowGrid.WriteBOTM(self, Model);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrCheckingElevation);
    Model.ModflowGrid.CheckElevations;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Model.DataArrayManager.CacheDataArrays;

    WriteIDomain;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Model.DataArrayManager.CacheDataArrays;

    if Model.ModelSelection = msModflow2015 then
    begin
      WriteEndGridData;
    end;

    // data set 7
    if Model.ModelSelection <> msModflow2015 then
    begin
      frmProgressMM.AddMessage(StrWritingDataSet7);
      Model.ModflowFullStressPeriods.WriteStressPeriods(self);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;

  finally
    CloseFile;
  end;

  CheckConnectivity;

  if Model.GwtUsed then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      if Model.MobileComponents[SpeciesIndex].UsedForGWT then
      begin
        AGwtFileName := GwtFileName(FNameOfFile, SpeciesIndex);
        TFile.Copy(FNameOfFile, AGwtFileName, True);
        WriteToGwtNameFile(FTYPE, AGwtFileName, SpeciesIndex);
      end;
    end;
  end;

end;

procedure TModflowDiscretizationWriter.WriteIDomain;
var
  IDomainDataSet: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    frmProgressMM.AddMessage('  Writing IDOMAIN');

    IDomainDataSet := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);

    WriteMf6_DataSet(IDomainDataSet, 'IDOMAIN');

    for LayerIndex := 0 to IDomainDataSet.LayerCount - 1 do
    begin
      for RowIndex := 0 to IDomainDataSet.RowCount - 1 do
      begin
        for ColIndex := 0 to IDomainDataSet.ColumnCount - 1 do
        begin
          if IDomainDataSet.IntegerData[LayerIndex,RowIndex,ColIndex] > 0 then
          begin
            Exit;
          end;
        end;
      end;
    end;

    frmErrorsAndWarnings.AddError(Model, StrNoActiveCells, StrMODFLOW6RequiresT);
  end;
end;

procedure TModflowDiscretizationWriter.WriteDataSet0;
var
  GridAngle: Real;
  ActiveCellCount: Integer;
  DataArray: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  procedure WriteCorner(const CornerDesc: string; APoint: TPoint2D);
  begin
    WriteCommentLine(CornerDesc + ' (' + FortranFloatToStr(APoint.x)
      + ', ' + FortranFloatToStr(APoint.y) + ')');
  end;
begin
  WriteCommentLine('Discretization File created on ' + DateToStr(Now) + ' by '
    + Model.ProgramName
    + ' version ' + IModelVersion + '.');
  if WritingTemplate then
  begin
    WriteCommentLine('(and then modified by a parameter estimation program.)');
  end;
  WriteCommentLines(Model.ModflowOptions.Description);

  WriteCorner('Upper left corner:', Model.Grid.TwoDElementCorner(0,0));
  WriteCorner('Lower left corner:', Model.Grid.TwoDElementCorner(
    0,Model.Grid.RowCount));
  WriteCorner('Upper right corner:', Model.Grid.TwoDElementCorner(
    Model.Grid.ColumnCount,0));
  WriteCorner('Lower right corner:', Model.Grid.TwoDElementCorner(
    Model.Grid.ColumnCount,Model.Grid.RowCount));
  GridAngle := Model.Grid.GridAngle * 180 / Pi;
  WriteCommentLine('Grid angle (in degrees counterclockwise): ' + FortranFloatToStr(GridAngle));
  if Model.ModelSelection = msModflow2015 then
  begin
    ActiveCellCount := 0;
    DataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    DataArray.Initialize;
    for LayerIndex := 0 to Model.LayerCount - 1 do
    begin
        for RowIndex := 0 to Model.RowCount - 1 do
        begin
          for ColIndex := 0 to Model.ColumnCount - 1 do
          begin
            if DataArray.IntegerData[LayerIndex, RowIndex, ColIndex] > 0 then
            begin
              Inc(ActiveCellCount);
            end;
          end;
        end;
    end;
    WriteCommentLine(Format('Number of active cells = %d.', [ActiveCellCount]));
  end;

end;

procedure TModflowDiscretizationWriter.WriteDataSet1;
var
  ModflowOptions: TModflowOptions;
  FarmProcess: TFarmProcess;
  GridAngle: double;
  APoint: TPoint2D;
  OriginCorner: TPoint2D;
begin
  ModflowOptions := Model.ModflowOptions;
  if Model.ModelSelection = msModflow2015 then
  begin
    WriteBeginOptions;

    if ModflowOptions.LengthUnit <> 0 then
    begin
      WriteString('  LENGTH_UNITS ');
      case ModflowOptions.LengthUnit of
        0:
          begin
            // WriteString('UNKNOWN');
          end;
        1:
          begin
            WriteString('FEET');
          end;
        2:
          begin
            WriteString('METERS');
          end;
        3:
          begin
            WriteString('CENTIMETERS');
          end;
        else
          Assert(False);
      end;
      NewLine;
    end;

    if not ModflowOptions.WriteBinaryGridFile then
    begin
      WriteString('NOGRB');
      NewLine;
    end
    else
    begin
      Model.AddModelOutputFile(FNameOfFile + '.grb');
    end;

    OriginCorner := Model.Grid.TwoDElementCorner(0,Model.Grid.RowCount);
    WriteString('  XORIGIN');
    WriteFloat(OriginCorner.x);
    NewLine;

    WriteString('  YORIGIN');
    WriteFloat(OriginCorner.y);
    NewLine;

    GridAngle := Model.Grid.GridAngle * 180 / Pi;
    WriteString('  ANGROT');
    WriteFloat(GridAngle);
    NewLine;

    WriteEndOptions;

    WriteBeginDimensions;

    WriteString('  NLAY ');
    WriteInteger(Model.ModflowLayerCount);
    NewLine;

    WriteString('  NROW ');
    WriteInteger(Model.ModflowGrid.RowCount);
    NewLine;

    WriteString('  NCOL ');
    WriteInteger(Model.ModflowGrid.ColumnCount);
    NewLine;
    WriteEndDimensions;

    Exit;
  end;
  WriteInteger(Model.ModflowLayerCount);
  WriteInteger(Model.ModflowGrid.RowCount);
  WriteInteger(Model.ModflowGrid.ColumnCount);
  WriteInteger(Model.ModflowFullStressPeriods.Count);
  if (Model.ModelSelection = msModflowFmp)
    and Model.ModflowPackages.FarmProcess.IsSelected then
  begin
    FarmProcess := Model.ModflowPackages.FarmProcess;
    if (FarmProcess.RootingDepth = rdCalculated)
      or (FarmProcess.ConsumptiveUse = cuCalculated) then
    begin
      if ModflowOptions.TimeUnit <> 4 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrInvalidSelectionOf,
          StrTheFarmProcessReq)
      end;
    end;
  end;

  { TODO -cOWHMV2 : Does OWHM V2 require daily time units for calculated rooting depth or consumptive use? }

  WriteInteger(ModflowOptions.TimeUnit);
  WriteInteger(ModflowOptions.LengthUnit);
  if (Model.ModelSelection in [msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ]) then
  begin
    APoint := Model.Grid.TwoDElementCorner(0,0);
    WriteFloat(APoint.x);
    WriteFloat(APoint.y);
    GridAngle := Model.Grid.GridAngle * 180 / Pi;
    WriteFloat(GridAngle);
    WriteString(' CORNERCOORD');
    if Model.ModflowOutputControl.PrintInputArrays then
    begin
      WriteString(' PRINTCOORD');
    end;
  end;

  WriteString(' # NLAY, NROW, NCOL, NPER, ITMUNI, LENUNI');
  if (Model.ModelSelection in [msModflowFmp
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
  ]) then
  begin
    WriteString(' XFIRSTCORD YFIRSTCORD GRIDROTATION COORD_OPTIONS');
  end;

  NewLine;
end;

{ TMf6DisvWriter }

procedure TMf6DisvWriter.CheckConnectivity;
var
  DataArray: TDataArray;
  ActiveCells: array of array of array of integer;
  Queue: TActiveCellQueue;
  FoundFirst: Boolean;
  ACell: TActiveCell;
  MfLayerCount: integer;
  LayerIndex: Integer;
  NewCell: TActiveCell;
  FirstCol: Integer;
  FirstLayer: Integer;
  CellCount: Integer;
  DisvGrid: TModflowDisvGrid;
  CellList: TMFIrregularCell2D_List;
  CellIndex: Integer;
  TwoDCell: TModflowIrregularCell2D;
  NeighborIndex: Integer;
  NeighborCell: TModflowIrregularCell2D;
  LIndex: Integer;
begin
  DisvGrid := Model.DisvGrid;
  DataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  DataArray.Initialize;
  MfLayerCount := Model.ModflowLayerCount;
  SetLength(ActiveCells, DataArray.LayerCount, DataArray.RowCount,
    DataArray.ColumnCount);

  CellList := TMFIrregularCell2D_List.Create;
  Queue := TActiveCellQueue.Create(True);
  try
    FoundFirst := False;
    for LayerIndex := 0 to DisvGrid.LayerCount - 1 do
    begin
      for CellIndex := 0 to DisvGrid.TwoDGrid.ElementCount - 1 do
      begin
        ActiveCells[LayerIndex,0,CellIndex] :=
          Sign(DataArray.IntegerData[LayerIndex,0,CellIndex]);
        if not FoundFirst
          and (ActiveCells[LayerIndex,0,CellIndex] > 0) then
        begin
          ACell := TActiveCell.Create;
//          ACell.MFLayer := LayerIndex;
          ACell.Layer := LayerIndex;
          ACell.Row := 0;
          ACell.Column := CellIndex;
          Queue.Enqueue(ACell);
          FoundFirst := True;
          ActiveCells[LayerIndex,0,CellIndex] := 2;
        end;
      end;
    end;

    repeat
      CellCount := 1;
      FirstCol := -1;
      FirstLayer := -1;
      FoundFirst := False;
      While Queue.Count > 0 do
      begin
        ACell := Queue.Peek;
        if not FoundFirst then
        begin
          FirstCol := ACell.Column;
          FirstLayer := ACell.Layer;
          FoundFirst := True;
        end;
        if ACell.Layer > 0 then
        begin
          for LIndex := ACell.Layer -1 downto 0 do
          begin
            if (ActiveCells[LIndex,ACell.Row,ACell.Column] in [0,2]) then
            begin
              break
            end;
            if (ActiveCells[LIndex,ACell.Row,ACell.Column] = 1) then
            begin
              NewCell := TActiveCell.Create;
  //            NewCell.MFLayer := ACell.MFLayer-1;
              NewCell.Layer := LIndex;
              NewCell.Row := ACell.Row;
              NewCell.Column := ACell.Column;
              Queue.Enqueue(NewCell);
              ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
              Inc(CellCount);
              break;
            end;
          end;
        end;
        if ACell.Layer < MfLayerCount-1 then
        begin
          for LIndex := ACell.Layer+1 to MfLayerCount - 1 do
          begin
            if (ActiveCells[LIndex,ACell.Row,ACell.Column] in [0,2]) then
            begin
              break;
            end;
            if (ActiveCells[LIndex,ACell.Row,ACell.Column] = 1) then
            begin
              NewCell := TActiveCell.Create;
              NewCell.Layer := LIndex;
              NewCell.Row := ACell.Row;
              NewCell.Column := ACell.Column;
              Queue.Enqueue(NewCell);
              ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
              Inc(CellCount);
              break;
            end;
          end;
        end;

        TwoDCell := DisvGrid.TwoDGrid.Cells[ACell.Column];
        TwoDCell.GetSharedEdgeNeighbors(CellList);
        for NeighborIndex := 0 to CellList.Count - 1 do
        begin
          NeighborCell := CellList[NeighborIndex];
          if (ActiveCells[ACell.Layer,0,NeighborCell.ElementNumber] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.Layer := ACell.Layer;
//            NewCell.Layer := ACell.Layer;
            NewCell.Row := 0;
            NewCell.Column := NeighborCell.ElementNumber;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.Layer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;

        Queue.Dequeue;
      end;
      if FoundFirst then
      begin

        for LayerIndex := 0 to DisvGrid.LayerCount - 1 do
        begin
          for CellIndex := 0 to DisvGrid.TwoDGrid.ElementCount - 1 do
          begin
            if (ActiveCells[LayerIndex,0,CellIndex] = 1) then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrThereIsNoHydrauli,
                Format(StrLayerCell,
                [FirstLayer+1, FirstCol+1, CellCount,
                LayerIndex+1, CellIndex+1]));
              ACell := TActiveCell.Create;
              ACell.Layer := LayerIndex;
//              ACell.Layer := LayerIndex;
              ACell.Row := 0;
              ACell.Column := CellIndex;
              Queue.Enqueue(ACell);
              ActiveCells[LayerIndex,0,CellIndex] := 2;
              break;
            end;
          end;
          if Queue.Count > 0 then
          begin
            Break;
          end;
        end
      end;
    until Queue.Count = 0;
  finally
    Queue.Free;
    CellList.Free;
  end;
end;

procedure TMf6DisvWriter.CheckElevations;
var
  LayerIndex: Integer;
  ALayer: TModflowIrregularLayer;
  DisvGrid: TModflowDisvGrid;
  CellIndex: Integer;
  IDomainDataArray: TDataArray;
  PriorLayer: Integer;
  TopLayer: TModflowIrregularLayer;
  PriorElevation: Double;
  NextElevation: Double;
  PriorElevationAssigned: Boolean;
  CellsAllZeroThickness: array of Boolean;
  InvalidLayers: TStringList;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrAllTheActiveCells);
  DisvGrid := Model.DisvGrid;
  IDomainDataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);

  SetLength(CellsAllZeroThickness, DisvGrid.Layers.Count);
  for LayerIndex := 0 to DisvGrid.Layers.Count -1 do
  begin
    CellsAllZeroThickness[LayerIndex] := True;
  end;

  PriorLayer := 0;
  TopLayer := DisvGrid.Layers[0].Layer;
  for CellIndex := 0 to TopLayer.Count - 1 do
  begin
    PriorElevation := 0;
    PriorElevationAssigned := False;
    if IDomainDataArray.IntegerData[0,0,CellIndex] > 0 then
    begin
      PriorElevation := TopLayer[CellIndex].Top;
      PriorElevationAssigned := True;
    end;
    for LayerIndex := 0 to DisvGrid.Layers.Count -1 do
    begin
      ALayer := DisvGrid.Layers[LayerIndex].Layer;
      if IDomainDataArray.IntegerData[LayerIndex,0,CellIndex] > 0 then
      begin
        if not PriorElevationAssigned then
        begin
          PriorElevation := ALayer[CellIndex].Top;
          PriorElevationAssigned := True;
        end;
        NextElevation := ALayer[CellIndex].Bottom;
        if (NextElevation > PriorElevation) then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrOverlappingLayers,
            Format(StrTheBottomOfCell,
            [CellIndex+1, LayerIndex+1, NextElevation, PriorLayer+1, PriorElevation]));
        end
        else
        begin
          CellsAllZeroThickness[LayerIndex] := False;
        end;
        PriorLayer := LayerIndex;
        PriorElevation := NextElevation;
      end;
    end;
  end;
  InvalidLayers := TStringList.Create;
  try
    for LayerIndex := 0 to DisvGrid.Layers.Count - 1 do
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
end;

class function TMf6DisvWriter.Extension: string;
begin
  result := '.disv';
end;

procedure TMf6DisvWriter.WriteCells;
var
  TwoDGrid: TModflowIrregularGrid2D;
  CellIndex: Integer;
  ACell: TModflowIrregularCell2D;
  ALocation: TPoint2D;
  NodeIndex: Integer;
  ANode: INode;
begin
  WriteString('BEGIN CELL2D');
  NewLine;
  try
    TwoDGrid := Model.DisvGrid.TwoDGrid;
    for CellIndex := 0 to TwoDGrid.Cells.Count - 1 do
    begin
      ACell := TwoDGrid.Cells[CellIndex];
      WriteString(' ');
      WriteInteger(CellIndex+1);
      ALocation := ACell.Location;
      WriteFloat(ALocation.x);
      WriteFloat(ALocation.y);
      WriteInteger(ACell.NodeCount);
      for NodeIndex := 0 to ACell.NodeCount - 1 do
      begin
        ANode := ACell.Nodes[NodeIndex];
        WriteInteger(ANode.NodeNumber+1);
      end;
      NewLine;
    end;

  finally
    WriteString('END CELL2D');
    NewLine;
  end;
end;

procedure TMf6DisvWriter.WriteDataSet0;
var
  ActiveCellCount: Integer;
  DataArray: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  WriteCommentLine('Discretization File created on ' + DateToStr(Now) + ' by '
    + Model.ProgramName
    + ' version ' + IModelVersion + '.');
  if WritingTemplate then
  begin
    WriteCommentLine('(and then modified by a parameter estimation program.)');
  end;
  WriteCommentLines(Model.ModflowOptions.Description);
  ActiveCellCount := 0;
  DataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  DataArray.Initialize;
  for LayerIndex := 0 to Model.LayerCount - 1 do
  begin
      for RowIndex := 0 to Model.RowCount - 1 do
      begin
        for ColIndex := 0 to Model.ColumnCount - 1 do
        begin
          if DataArray.IntegerData[LayerIndex, RowIndex, ColIndex] > 0 then
          begin
            Inc(ActiveCellCount);
          end;
        end;
      end;
  end;
  WriteCommentLine(Format('Number of active cells = %d.', [ActiveCellCount]));
end;

procedure TMf6DisvWriter.WriteDimensions;
var
  DisvGrid: TModflowDisvGrid;
  nlay: Integer;
  TwoDGrid: TModflowIrregularGrid2D;
  ncpl: Integer;
  nvert: Integer;
begin
  Assert(Model.Mf6GridType = mgtLayered);
  DisvGrid := Model.DisvGrid;
  nlay := DisvGrid.Layers.Count;
  TwoDGrid := DisvGrid.TwoDGrid;
  ncpl := TwoDGrid.ElementCount;
  nvert := TwoDGrid.NodeCount;
  WriteBeginDimensions;
  try
    WriteString('  NLAY');
    WriteInteger(nlay);
    NewLine;

    WriteString('  NCPL');
    WriteInteger(ncpl);
    NewLine;

    WriteString('  NVERT');
    WriteInteger(nvert);
    NewLine;

  finally
    WriteEndDimensions;
  end;
end;

procedure TMf6DisvWriter.WriteFile(const AFileName: string);
var
  FTYPE: string;
  SpeciesIndex: Integer;
  AGwtFileName: string;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrOverlappingLayers);
  FTYPE := 'DISV6';
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  Assert( Model.ModelSelection = msModflow2015);
  WriteToNameFile(FTYPE, -1, FNameOfFile, foInput, Model, False, 'DISV');
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingDISVPackage);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;

    frmProgressMM.AddMessage(StrWritingOptions);
    WriteOptions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDimensions);
    WriteDimensions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingGridData);
    WriteGridData;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingVertices);
    WriteVertices;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingCells);
    WriteCells;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

  finally
    CloseFile;
  end;

  if Model.GwtUsed then
  begin
    for SpeciesIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      if Model.MobileComponents[SpeciesIndex].UsedForGWT then
      begin
        AGwtFileName := GwtFileName(FNameOfFile, SpeciesIndex);
        TFile.Copy(FNameOfFile, AGwtFileName, True);
        WriteToGwtNameFile(FTYPE, AGwtFileName, SpeciesIndex);
      end;
    end;
  end;
end;

procedure TMf6DisvWriter.WriteGridData;
var
  DisvGrid: TModflowDisvGrid;
  Elevation: double;
  CellIndex: Integer;
  IPRN: Integer;
  ALayer: TModflowIrregularLayer;
  LayerIndex: Integer;
  IDomainDataSet: TDataArray;
  NeedNewLine: Boolean;
begin
  DisvGrid := Model.DisvGrid;
  WriteBeginGridData;
  try
    frmProgressMM.AddMessage('  Writing TOP');
    IPRN := IPRN_Real;
    WriteString('  TOP');
    NewLine;
    if DisvGrid.IsLayerUniform(0) then
    begin
      Elevation := DisvGrid.Layers[0].Layer[0].Top;
      WriteString('    CONSTANT ');
      WriteFloat(Elevation);
      NewLine;
    end
    else
    begin
      WriteString('    INTERNAL ');
      if IPRN >= 0 then
      begin
        WriteString('IPRN ');
        WriteInteger(IPRN);
      end;
      NewLine;

      NeedNewLine := True;
      ALayer := DisvGrid.Layers[0].Layer;
      for CellIndex := 0 to ALayer.Count - 1 do
      begin
        Elevation := ALayer[CellIndex].Top;
        WriteFloat(Elevation);
        if ((CellIndex + 1) mod 10) = 0 then
        begin
          NewLine;
          NeedNewLine := False;
        end
        else
        begin
          NeedNewLine := True;
        end;
      end;
      if NeedNewLine then
      begin
        NewLine;
      end;
    end;

    frmProgressMM.AddMessage('  Writing BOTM');
    WriteString('  BOTM LAYERED');
    NewLine;
    for LayerIndex := 0 to DisvGrid.Layers.Count -1 do
    begin
      if DisvGrid.IsLayerUniform(LayerIndex+1) then
      begin
        Elevation := DisvGrid.Layers[LayerIndex].Layer[0].Bottom;
        WriteString('    CONSTANT ');
        WriteFloat(Elevation);
        NewLine;
      end
      else
      begin
        WriteString('    INTERNAL ');
        if IPRN >= 0 then
        begin
          WriteString('IPRN ');
          WriteInteger(IPRN);
        end;
        NewLine;

        NeedNewLine := True;
        ALayer := DisvGrid.Layers[LayerIndex].Layer;
        for CellIndex := 0 to ALayer.Count - 1 do
        begin
          Elevation := ALayer[CellIndex].Bottom;
          WriteFloat(Elevation);
          if ((CellIndex + 1) mod 10) = 0 then
          begin
            NewLine;
            NeedNewLine := False;
          end
          else
          begin
            NeedNewLine := True;
          end;
        end;
        if NeedNewLine then
        begin
          NewLine;
        end;
      end;
    end;

    frmProgressMM.AddMessage('  Writing IDOMAIN');
    IDomainDataSet := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    WriteMf6_DataSet(IDomainDataSet, 'IDOMAIN LAYERED');
  finally
    WriteEndGridData;
  end;

  CheckElevations;
  CheckConnectivity;
end;

procedure TMf6DisvWriter.WriteOptions;
var
  ModflowOptions: TModflowOptions;
begin
  ModflowOptions := Model.ModflowOptions;
  WriteBeginOptions;
  try
    if ModflowOptions.LengthUnit <> 0 then
    begin
      WriteString('  LENGTH_UNITS ');
      case ModflowOptions.LengthUnit of
        0:
          begin
            // WriteString('UNKNOWN');
          end;
        1:
          begin
            WriteString('FEET');
          end;
        2:
          begin
            WriteString('METERS');
          end;
        3:
          begin
            WriteString('CENTIMETERS');
          end;
        else
          Assert(False);
      end;
      NewLine;
    end;

    if not ModflowOptions.WriteBinaryGridFile then
    begin
      WriteString('NOGRB');
      NewLine;
    end
    else
    begin
      Model.AddModelOutputFile(FNameOfFile + '.grb');
    end;
    // XORIGIN not supported at this time.
    // YORIGIN not supported at this time.
    // ANGROT not supported at this time.

  finally
    WriteEndOptions;
  end;
end;

procedure TMf6DisvWriter.WriteVertices;
var
  TwoDGrid: TModflowIrregularGrid2D;
  NodeIndex: Integer;
  ANode: INode;
  ALocation: TPoint2D;
begin
  WriteString('BEGIN VERTICES');
  NewLine;
  try
    TwoDGrid := Model.DisvGrid.TwoDGrid;
    for NodeIndex := 0 to TwoDGrid.NodeCount - 1 do
    begin
      ANode := TwoDGrid.Nodes[NodeIndex];
      WriteString(' ');
      WriteInteger(NodeIndex+1);
      ALocation := ANode.Location;
      WriteFloat(ALocation.x);
      WriteFloat(ALocation.Y);
      NewLine;
    end;

  finally
    WriteString('END VERTICES');
    NewLine;
    NewLine;
  end;

end;

end.
