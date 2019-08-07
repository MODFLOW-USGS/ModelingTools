unit ModflowBasicWriterUnit;

interface

uses SysUtils, PhastModelUnit, DataSetUnit, CustomModflowWriterUnit;

type
  TCustomBasicWriter = class(TCustomModflowWriter)
  protected
    XSECTION: boolean;
    FNameOfFile: string;
    procedure CheckStartingHeads;
    procedure WriteStartingHeads;
    procedure WriteDataSet(const DataSetName: string; DataArray: TDataArray);
  end;

  TModflowBasicWriter = class(TCustomBasicWriter)
  private
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure CheckConnectivity;
    procedure CheckAtLeastOneActiveCellOnSimulatedLayers(
      ExportArray: TDataArray);
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

resourcestring
  StrFileForTheInitial = 'File for the initial heads does not exist.';
  StrWrongExtension = 'File for the initial heads has wrong extension.';
  StrCheckingStarting = '  Checking starting heads.';

implementation

uses frmErrorsAndWarningsUnit, ModflowUnitNumbers, frmProgressUnit, Forms,
  RbwParser, GoPhastTypes, ModflowOptionsUnit, Generics.Collections;

resourcestring
  StrTheFileSDoesNot = 'The file %s does not exist. This file is specified in '
  + 'the "Model|MODFLOW Options" dialog box on the "Options" tab';
  StrTheFile1sMustH = 'The file %0:s must have an extension equal to "%1:s". This file is specified in '
  + 'the "Model|MODFLOW Options" dialog box on the "Options" tab';
  StrLayer0dRow1 = 'Layer: %0:d; Row: %1:d; Column: %2:d';
  StrWritingBasicPackag = 'Writing Basic Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
  StrInitialHeadIsBelo = 'Initial Head is below or equal to the bottom of the layer.';
  StrThereIsNoHydrauli = 'At least one of the following active cells is '
    + 'hydraulically isolated.';
  StrLayerRowColumn = '(Layer, Row, Column) = (%0:d, %1:d, %2:d) (Number of ' +
    'connected cells = %3:d) and (Layer, Row, Column) = (%4:d, %5:d, %6:d)';
  StrCheckingModelConne = 'Checking model connectivity';
  StrOneOrMoreLayersC = 'One or more layers contain no active cells.';
  StrThereAreNoActive = 'There are no active cells on layer %d';

{ TModflowBasicWriter }

procedure TModflowBasicWriter.CheckConnectivity;
var
  DataArray: TDataArray;
  ActiveCells: array of array of array of integer;
  MFLayer: Integer;
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
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  DataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  DataArray.Initialize;
  MfLayerCount := Model.ModflowLayerCount;
  SetLength(ActiveCells, MfLayerCount, DataArray.RowCount, DataArray.ColumnCount);
  Queue := TActiveCellQueue.Create(True);
  try
    FoundFirst := False;
    MFLayer := -1;
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        Inc(MFLayer);
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            ActiveCells[MFLayer,RowIndex,ColIndex] :=
              Ord(DataArray.BooleanData[LayerIndex,RowIndex,ColIndex]);
            if not FoundFirst
              and (ActiveCells[MFLayer,RowIndex,ColIndex] <> 0) then
            begin
              ACell := TActiveCell.Create;
              ACell.MFLayer := MFLayer;
              ACell.Layer := LayerIndex;
              ACell.Row := RowIndex;
              ACell.Column := ColIndex;
              Queue.Enqueue(ACell);
              FoundFirst := True;
              ActiveCells[MFLayer,RowIndex,ColIndex] := 2;
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
        if ACell.MFLayer > 0 then
        begin
          if (ActiveCells[ACell.MFLayer-1,ACell.Row,ACell.Column] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.MFLayer := ACell.MFLayer-1;
            NewCell.Layer := Model.ModflowLayerToDataSetLayer(NewCell.MFLayer+1);
            NewCell.Row := ACell.Row;
            NewCell.Column := ACell.Column;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.MFLayer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        if ACell.MFLayer < MfLayerCount-1 then
        begin
          if (ActiveCells[ACell.MFLayer+1,ACell.Row,ACell.Column] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.MFLayer := ACell.MFLayer+1;
            NewCell.Layer := Model.ModflowLayerToDataSetLayer(NewCell.MFLayer+1);
            NewCell.Row := ACell.Row;
            NewCell.Column := ACell.Column;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.MFLayer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        if ACell.Row > 0 then
        begin
          if (ActiveCells[ACell.MFLayer,ACell.Row-1,ACell.Column] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.MFLayer := ACell.MFLayer;
            NewCell.Layer := ACell.Layer;
            NewCell.Row := ACell.Row-1;
            NewCell.Column := ACell.Column;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.MFLayer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        if ACell.Row < DataArray.RowCount-1 then
        begin
          if (ActiveCells[ACell.MFLayer,ACell.Row+1,ACell.Column] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.MFLayer := ACell.MFLayer;
            NewCell.Layer := ACell.Layer;
            NewCell.Row := ACell.Row+1;
            NewCell.Column := ACell.Column;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.MFLayer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        if ACell.Column > 0 then
        begin
          if (ActiveCells[ACell.MFLayer,ACell.Row,ACell.Column-1] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.MFLayer := ACell.MFLayer;
            NewCell.Layer := ACell.Layer;
            NewCell.Row := ACell.Row;
            NewCell.Column := ACell.Column-1;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.MFLayer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        if ACell.Column < DataArray.ColumnCount-1 then
        begin
          if (ActiveCells[ACell.MFLayer,ACell.Row,ACell.Column+1] = 1) then
          begin
            NewCell := TActiveCell.Create;
            NewCell.MFLayer := ACell.MFLayer;
            NewCell.Layer := ACell.Layer;
            NewCell.Row := ACell.Row;
            NewCell.Column := ACell.Column+1;
            Queue.Enqueue(NewCell);
            ActiveCells[NewCell.MFLayer,NewCell.Row,NewCell.Column] := 2;
            Inc(CellCount);
          end;
        end;
        Queue.Dequeue;
      end;
      if FoundFirst then
      begin
//        FoundFirst := False;
        MFLayer := -1;
        for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
        begin
          if Model.IsLayerSimulated(LayerIndex) then
          begin
            Inc(MFLayer);
            for RowIndex := 0 to DataArray.RowCount - 1 do
            begin
              for ColIndex := 0 to DataArray.ColumnCount - 1 do
              begin
                if (ActiveCells[MFLayer,RowIndex,ColIndex] = 1) then
                begin
                  frmErrorsAndWarnings.AddWarning(Model, StrThereIsNoHydrauli,
                    Format(StrLayerRowColumn,
                    [FirstLayer+1, FirstRow+1, FirstCol+1, CellCount,
                    LayerIndex+1, RowIndex+1, ColIndex+1]));
                  ACell := TActiveCell.Create;
                  ACell.MFLayer := MFLayer;
                  ACell.Layer := LayerIndex;
                  ACell.Row := RowIndex;
                  ACell.Column := ColIndex;
                  Queue.Enqueue(ACell);
                  ActiveCells[MFLayer,RowIndex,ColIndex] := 2;
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

procedure TCustomBasicWriter.CheckStartingHeads;
var
  DataArray: TDataArray;
  LayerIndex, RowIndex, ColIndex: Integer;
  Head, Bottom: double;
  ErrorString: string;
  ActiveArray: TDataArray;
  Active: boolean;
begin
  if Model.DisvUsed then
  begin
    Exit;
  end;
  ErrorString := StrInitialHeadIsBelo;
  DataArray := Model.DataArrayManager.GetDataSetByName(rsModflow_Initial_Head);
  DataArray.Initialize;
  ActiveArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  ActiveArray.Initialize;

  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      for RowIndex := 0 to DataArray.RowCount - 1 do
      begin
        for ColIndex := 0 to DataArray.ColumnCount - 1 do
        begin
          Active := ActiveArray.BooleanData[LayerIndex, RowIndex, ColIndex];
          if Active then
          begin
            Head := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
            Bottom := Model.ModflowGrid.CellElevation[ColIndex, RowIndex, LayerIndex+1];
            if Bottom >= Head then
            begin
              frmErrorsAndWarnings.AddWarning(Model, ErrorString,
                Format(StrLayer0dRow1,
                 [LayerIndex+1, RowIndex+1, ColIndex+1]));
            end;
          end;
        end;
      end;
    end;
  end;
  Model.DataArrayManager.AddDataSetToCache(DataArray);
  Model.DataArrayManager.AddDataSetToCache(ActiveArray);

end;

constructor TModflowBasicWriter.Create(AModel: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  XSECTION := (Model.ModflowGrid.RowCount = 1)
    and not Model.ModflowPackages.ModPath.IsSelected;
end;

class function TModflowBasicWriter.Extension: string;
begin
  result := '.bas';
end;

procedure TModflowBasicWriter.WriteDataSet0;
begin
  WriteCommentLines(Model.ModflowOptions.Description);
  WriteCommentLine('Basic Package file created on ' + DateToStr(Now) + ' by '
    + Model.ProgramName
    + ' version ' + IModelVersion + '.');
end;

procedure TModflowBasicWriter.WriteDataSet1;
const
  FREE = True;
var
  CHTOCH: boolean;
  PRINTTIME: boolean;
  ModflowOptions: TModflowOptions;
  StopError: Boolean;
  StopErrorCriterion: Double;
//  SHOWPROGRESS: boolean;
begin
  ModflowOptions := Model.ModflowOptions;
  CHTOCH := ModflowOptions.ComputeFluxesBetweenConstantHeadCells;
  PRINTTIME := ModflowOptions.PrintTime;
  StopError := ModflowOptions.StopError;
  StopErrorCriterion := ModflowOptions.StopErrorCriterion;
//  SHOWPROGRESS := PhastModel.ModflowOptions.ShowProgress;
  if FREE then
  begin
    WriteString('FREE ');
  end;
  if XSECTION then
  begin
    WriteString('XSECTION ');
  end;
  if CHTOCH then
  begin
    WriteString('CHTOCH ');
  end;
  if PRINTTIME then
  begin
    WriteString('PRINTTIME ');
  end;
  if StopError then
  begin
    WriteString('STOPERROR ');
    WriteFloat(StopErrorCriterion);
  end;
//  if SHOWPROGRESS then
//  begin
//    WriteString('SHOWPROGRESS ');
//  end;
  WriteString(' # OPTIONS');
  NewLine;
end;

procedure TModflowBasicWriter.WriteDataSet2;
var
  DataArray: TDataArray;
  DataSetName: string;
  LocalChildModel: TChildModel;
  PhastModel: TPhastModel;
  EdgeValue: Integer;
  ExportArray: TDataArray;
  TempArray: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DummyAnnotation: string;
begin
  TempArray := nil;
  try
    DataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    if {(Model.ModelSelection = msModflowLGR) and} (Model is TChildModel) then
    begin
      DummyAnnotation := 'none';
      LocalChildModel := TChildModel(Model);
      EdgeValue := LocalChildModel.EdgeIndex;

      DataArray.Initialize;
      TempArray := TDataArray.Create(Model);
      TempArray.DataType := rdtInteger;
      TempArray.EvaluatedAt := eaBlocks;
      TempArray.Orientation := dso3D;
      TempArray.UpdateDimensions(DataArray.LayerCount,
        DataArray.RowCount, DataArray.ColumnCount, True);

      TempArray.UpToDate := True;
      // force internal arrays to be resized.
      TempArray.IntegerData[0,0,0];

      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        for RowIndex := 1 to DataArray.RowCount - 2 do
        begin
          for ColIndex := 1 to DataArray.ColumnCount - 2 do
          begin
            if DataArray.BooleanData[LayerIndex, RowIndex, ColIndex] then
            begin
              TempArray.IntegerData[LayerIndex, RowIndex, ColIndex] := 1;
            end
            else
            begin
              TempArray.IntegerData[LayerIndex, RowIndex, ColIndex] := 0;
            end;
            TempArray.Annotation[LayerIndex, RowIndex, ColIndex] := DummyAnnotation;
          end;
          TempArray.IntegerData[LayerIndex, RowIndex, 0] := EdgeValue;
          TempArray.IntegerData[LayerIndex, RowIndex,
            DataArray.ColumnCount - 1] := EdgeValue;
        end;
        for ColIndex := 0 to DataArray.ColumnCount - 1 do
        begin
          TempArray.IntegerData[LayerIndex, 0, ColIndex] := EdgeValue;
          TempArray.IntegerData[LayerIndex,
            DataArray.RowCount - 1, ColIndex] := EdgeValue;
        end;
      end;

      PhastModel := LocalChildModel.ParentModel as TPhastModel;
      if (LocalChildModel.Discretization.BottomLayerGroup <>
        PhastModel.LayerStructure[PhastModel.LayerStructure.Count-1])
        or (LocalChildModel.Discretization.BottomLayerGroup.LayerCount-1
        <> LocalChildModel.Discretization.BottomLayerInUnit) then
      begin
        for RowIndex := 1 to DataArray.RowCount - 2 do
        begin
          for ColIndex := 1 to DataArray.ColumnCount - 2 do
          begin
            TempArray.IntegerData[DataArray.LayerCount - 1,
              RowIndex, ColIndex] := EdgeValue;
          end;
        end;
      end;
      TempArray.UpToDate := True;
      ExportArray := TempArray;
    end
    else
    begin
      ExportArray := DataArray;
    end;
    DataSetName := 'IBOUND';
    WriteDataSet(DataSetName, ExportArray);
    CheckAtLeastOneActiveCellOnSimulatedLayers(ExportArray);
    Model.DataArrayManager.AddDataSetToCache(DataArray);
    Model.DataArrayManager.CacheDataArrays;
  finally
    TempArray.Free;
  end;
end;

procedure TModflowBasicWriter.CheckAtLeastOneActiveCellOnSimulatedLayers(
  ExportArray: TDataArray);
var
  LayerIndex: Integer;
  ActivePresent: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      ActivePresent := False;
      for RowIndex := 0 to ExportArray.RowCount - 1 do
      begin
        for ColIndex := 0 to ExportArray.ColumnCount - 1 do
        begin
          if ExportArray.DataType = rdtBoolean then
          begin
            if ExportArray.BooleanData[LayerIndex, RowIndex, ColIndex] then
            begin
              ActivePresent := True;
              Break;
            end;
          end
          else
          begin
            Assert(ExportArray.DataType = rdtInteger);
            if ExportArray.IntegerData[LayerIndex, RowIndex, ColIndex] <> 0 then
            begin
              ActivePresent := True;
              Break;
            end;
        end;
        end;
        if ActivePresent then
        begin
          break;
        end;
      end;
      if not ActivePresent then
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrOneOrMoreLayersC,
          Format(StrThereAreNoActive, [LayerIndex+1]));
      end;
    end;
  end;
end;

procedure TModflowBasicWriter.WriteDataSet3;
begin
  WriteFloat(Model.ModflowOptions.HNoFlow);
  WriteString(' # HNOFLO');
  NewLine;
end;

procedure TCustomBasicWriter.WriteStartingHeads;
var
  DataArray: TDataArray;
  DataSetName: string;
  RelativeFileName: string;
  UnitNumber: Integer;
  LayerIndex: Integer;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    WriteBeginGridData;
  end;
  if Model.ModflowOptions.InitialHeadFileName = '' then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsModflow_Initial_Head);
    DataSetName := 'STRT';
    WriteDataSet(DataSetName, DataArray);
    Model.DataArrayManager.AddDataSetToCache(DataArray);
    Model.DataArrayManager.CacheDataArrays;
  end
  else
  begin
    if not FileExists(Model.ModflowOptions.InitialHeadFileName) then
    begin
//      frmErrorsAndWarnings.AddError(Model, StrFileForTheInitial, 'The file '
//        + Model.ModflowOptions.InitialHeadFileName
//        + ' does not exist.');
      frmErrorsAndWarnings.AddError(Model, StrFileForTheInitial,
        Format(StrTheFileSDoesNot,
        [Model.ModflowOptions.InitialHeadFileName]));
//      Exit;
    end;
    RelativeFileName := ExtractRelativePath(
      IncludeTrailingPathDelimiter(ExtractFileDir(FNameOfFile)),
      Model.ModflowOptions.InitialHeadFileName);
    UnitNumber := Model.UnitNumbers.UnitNumber(BAS_InitialHeads);
    if SameText(ExtractFileExt(Model.ModflowOptions.InitialHeadFileName), StrBhd) then
    begin
      WriteToNameFile(StrDATABINARY, UnitNumber, RelativeFileName,
        foInputAlreadyExists, Model, True);
    end
    else
    begin
//      frmErrorsAndWarnings.AddError(Model, StrWrongExtension, 'The file '
//        + Model.ModflowOptions.InitialHeadFileName
//        + ' must have an extension equal to "' + StrBhd + '".');
      frmErrorsAndWarnings.AddError(Model, StrWrongExtension,
        Format(StrTheFile1sMustH,
          [Model.ModflowOptions.InitialHeadFileName, StrBhd]));
      Exit;
    end;

    if XSECTION then
    begin
      WriteString('EXTERNAL');
      WriteInteger(UnitNumber);
      WriteInteger(1);
      WriteString(' (BINARY)');
      WriteInteger(IPRN_Real);
      NewLine;
    end
    else
    begin
      for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
      begin
        if Model.IsLayerSimulated(LayerIndex) then
        begin
          WriteString('EXTERNAL');
          WriteInteger(UnitNumber);
          WriteInteger(1);
          WriteString(' (BINARY)');
          WriteInteger(IPRN_Real);
          NewLine;
        end;
      end;
    end;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    WriteEndGridData;
  end;

end;

procedure TModflowBasicWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileForTheInitial);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrWrongExtension);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrThereIsNoHydrauli);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrOneOrMoreLayersC);

    if Model.ModelSelection = msModflow2015 then
    begin
      Exit;
    end;

    if Model.PackageGeneratedExternally(StrBAS) then
    begin
      Exit;
    end;

    FNameOfFile := FileName(AFileName);
    frmProgressMM.AddMessage(StrCheckingModelConne);
    CheckConnectivity;

    WriteToNameFile(StrBAS, Model.UnitNumbers.UnitNumber(StrBAS),
      FNameOfFile, foInput, Model);
    OpenFile(FNameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingBasicPackag);
      frmProgressMM.AddMessage(StrWritingDataSet0);
      WriteDataSet0;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet1);
      WriteDataSet1;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet2);
      WriteDataSet2;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet3);
      WriteDataSet3;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet4);
      WriteStartingHeads;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrCheckingStarting);
      CheckStartingHeads;
    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TCustomBasicWriter.WriteDataSet(const DataSetName: string;
  DataArray: TDataArray);
var
  LayerIndex: Integer;
begin
  Assert(DataArray <> nil);
  if XSECTION then
  begin
    WriteCrossSectionArray(DataArray, DataSetName, DataSetName);
  end
  else
  begin
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        if LayerIndex = 0 then
        begin
          WriteArray(DataArray, LayerIndex,
            DataSetName + ' ' + Model.ModflowLayerBottomDescription(LayerIndex),
            StrNoValueAssigned, DataSetName);
        end
        else
        begin
          WriteArray(DataArray, LayerIndex,
            DataSetName + ' ' + Model.ModflowLayerBottomDescription(LayerIndex),
            StrNoValueAssigned, '');
        end;
      end;
    end;
  end;
end;

end.

