{@abstract(The main purpose of @name is to define
  @link(TfrmImportDistributedData) which is used to import zone data into
  @link(TMultiValueScreenObject)s.)

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit frmImportDistributedDataUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons,
  Grids, RbwDataGrid4, ExtCtrls, ArgusDataEntry;  

type
  {@abstract(@name is used to import zone data into
    @link(TMultiValueScreenObject)s.)}
  TfrmImportDistributedData = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes the @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TComboBox;
    // @name specifies the direction from which
    // the imported zones will be viewed.
    comboViewDirection: TComboBox;
    // @name: TLabel;
    // @name displays "Higher X'".
    lblHigherX: TLabel;
    // @name: TLabel;
    // @name displays "Higher Y'".
    lblHigherY: TLabel;
    // @name: TLabel;
    // @name displays "Higher Z'".
    lblHigherZ: TLabel;
    // @name: TLabel;
    // @name displays "Import to ".
    lblImportTo: TLabel;
    // @name: TLabel;
    // @name displays "Lower X'".
    lblLowerX: TLabel;
    // @name: TLabel;
    // @name displays "Lower Y'".
    lblLowerY: TLabel;
    // @name: TLabel;
    // @name displays "Lower Z'".
    lblLowerZ: TLabel;
    // @name: TOpenDialog;
    // @name is used to select data file for importing.
    OpenDialogFile: TOpenDialog;
    // @name: TPanel;
    // @name holds the buttons on the bottom of the dialog box.
    pnlBottom: TPanel;
    // @name: TPanel;
    // @name holds the controls in the top part of the dialog box.
    pnlTop: TPanel;
    // @name: TRbwDataEntry;
    // @name is used to specify the higher X' coordinate of a zone.
    rdeHigherX: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the higher Y' coordinate of a zone.
    rdeHigherY: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the higher Z' coordinate of a zone.
    rdeHigherZ: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the lower X' coordinate of a zone.
    rdeLowerX: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the lower Y' coordinate of a zone.
    rdeLowerY: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the lower Z' coordinate of a zone.
    rdeLowerZ: TRbwDataEntry;
    dgDataSets: TRbwDataGrid4;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name calls @link(GetDataSets) and invalidates @link(dgDataSets).
    procedure comboViewDirectionChange(Sender: TObject);
    // @name uses @link(OpenDialogFile) to specify a zone file.
    procedure dgDataSetsButtonClicked(Sender: TObject; ACol, ARow: Integer);
    // @name initialized @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name calls @link(ResizeColumns);
    procedure FormResize(Sender: TObject);
  private
    // @name enables the @link(btnOK) button when the entered data is valid.
    procedure EnableOK;
    // sets the lower and higher, X, Y, and Z coordinates
    // to the extend of the grid.
    procedure GetData;
    // @name fills @link(dgDataSets) with the names of the @link(TDataArray)s
    // to which data can be imported in the current model view.
    procedure GetDataSets;
    // @name imports the data specified by the user into
    // @link(TMultiValueScreenObject)s.
    procedure SetData;
    // @name resizes the column with the file names in @link(dgDataSets).
    procedure ResizeColumns;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, DataSetUnit, ScreenObjectUnit, UndoItemsScreenObjects,
  GoPhastTypes, UndoItems, GIS_Functions, RbwParser, FastGEO, PhastModelUnit,
  ModelMuseUtilities;

resourcestring
  StrDataSets = 'Data Sets';
  StrFilePath = 'File Path';

{$R *.dfm}

procedure TfrmImportDistributedData.EnableOK;
var
  ShouldEnable: boolean;
  Val1, Val2: double;
  Index: integer;
  FileSpecified: boolean;
begin
  ShouldEnable := True;
  FileSpecified := false;
  for Index := 1 to dgDataSets.RowCount - 1 do
  begin
    if (dgDataSets.Cells[1, Index] <> '') then
    begin
      FileSpecified := true;
      if not FileExists(dgDataSets.Cells[1, Index]) then
      begin
        ShouldEnable := False;
      end;
    end;
  end;

  if not FileSpecified then
  begin
    ShouldEnable := false;
  end;

  Val1 := StrToFloat(rdeLowerX.Text);
  Val2 := StrToFloat(rdeHigherX.Text);
  if Val2 < Val1 then
  begin
    ShouldEnable := False;
  end;
  Val1 := StrToFloat(rdeLowerY.Text);
  Val2 := StrToFloat(rdeHigherY.Text);
  if Val2 < Val1 then
  begin
    ShouldEnable := False;
  end;
  Val1 := StrToFloat(rdeLowerZ.Text);
  Val2 := StrToFloat(rdeHigherZ.Text);
  if Val2 < Val1 then
  begin
    ShouldEnable := False;
  end;
  btnOK.Enabled := ShouldEnable;
end;

procedure TfrmImportDistributedData.GetDataSets;
var
  Index: integer;
  DataSet: TDataArray;
  Row: integer;
  UseDataSet: boolean;
  DataArrayManager: TDataArrayManager;
begin
  Row := 0;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  dgDataSets.RowCount := DataArrayManager.DataSetCount + 1;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
    if DataSet.DataType in [rdtDouble, rdtInteger] then
    begin
      UseDataSet := False;
      case DataSet.Orientation of
        dsoTop, dso3D:
          begin
            UseDataSet := comboViewDirection.ItemIndex = 0;
          end;
        dsoFront:
          begin
            UseDataSet := comboViewDirection.ItemIndex = 1;
          end;
        dsoSide:
          begin
            UseDataSet := comboViewDirection.ItemIndex = 2;
          end;
      else
        Assert(False);
      end;
      if UseDataSet then
      begin
        Inc(Row);
        dgDataSets.Cells[0, Row] := DataSet.Name;
        dgDataSets.Objects[0, Row] := DataSet;
        dgDataSets.Cells[1, Row] := '';
      end;
    end;
  end;
  dgDataSets.RowCount := Row + 1;
end;

procedure TfrmImportDistributedData.ResizeColumns;
const
  ScrollWidth = 32;
var
  TotalColWidths: integer;
  Index: integer;
begin
  TotalColWidths := 0;
  for Index := 0 to dgDataSets.ColCount -1 do
  begin
    TotalColWidths := TotalColWidths + dgDataSets.ColWidths[Index];
  end;
  dgDataSets.ColWidths[1] := dgDataSets.ColWidths[1] +
    (dgDataSets.Width - TotalColWidths) - ScrollWidth;
end;

procedure TfrmImportDistributedData.GetData;
begin
  with frmGoPhast.PhastModel.PhastGrid do
  begin
    if ColumnCount > 0 then
    begin
      rdeLowerX.Text := FloatToStr(ColumnPosition[0]);
      rdeHigherX.Text := FloatToStr(ColumnPosition[ColumnCount]);
    end;
    if RowCount > 0 then
    begin
      rdeLowerY.Text := FloatToStr(RowPosition[0]);
      rdeHigherY.Text := FloatToStr(RowPosition[RowCount]);
    end;
    if LayerCount > 0 then
    begin
      rdeLowerZ.Text := FloatToStr(LayerElevations[0]);
      rdeHigherZ.Text := FloatToStr(LayerElevation[LayerCount]);
    end;
  end;
  GetDataSets;
  ResizeColumns;
end;

procedure TfrmImportDistributedData.SetData;
var
  AScreenObject: TMultiValueScreenObject;
  UndoItem: TCustomUndo;
  Index: integer;
  DataSet: TDataArray;
  BlockDataSetFiles, NodeDataSetFiles: TStringList;
  FirstColumn, LastColumn: integer;
  FirstRow, LastRow: integer;
  FirstLayer, LastLayer: integer;
  LowerX, UpperX, LowerY, UpperY, LowerZ, UpperZ: double;
  DataSetIndex: integer;
  DataValues: TStringList;
  LineIndex: integer;
  ALine: string;
  RealValues: TOneDRealArray;
  IntegerValues: TOneDIntegerArray;
  ValueIndex: integer;
  ItemIndex: integer;
  RealItem: TRealDataListItem;
  IntegerItem: TIntegerDataListItem;
  DataSetPosition: integer;
  MinLayWidth: double;
  LayerIndex: integer;
  ViewDirection: TViewDirection;
  MinColWidth: double;
  MinRowWidth: double;
  ColIndex, RowIndex: integer;
  procedure AddPointsToScreenObject;
  var
    APoint: TPoint2D;
  begin
    AScreenObject.Capacity := 5;
    case ViewDirection of
      vdTop:
        begin
          with frmGoPhast.PhastModel.PhastGrid do
          begin
            APoint.X := LowerX - MinColWidth;
            APoint.Y := LowerY - MinRowWidth;
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
            AScreenObject.AddPoint(APoint, False);

            APoint.X := LowerX - MinColWidth;
            APoint.Y := UpperY + MinRowWidth;
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
            AScreenObject.AddPoint(APoint, False);

            APoint.X := UpperX + MinColWidth;
            APoint.Y := UpperY + MinRowWidth;
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
            AScreenObject.AddPoint(APoint, False);

            APoint.X := UpperX + MinColWidth;
            APoint.Y := LowerY - MinRowWidth;
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
            AScreenObject.AddPoint(APoint, False);

            APoint.X := LowerX - MinColWidth;
            APoint.Y := LowerY - MinRowWidth;
            APoint := RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
            AScreenObject.AddPoint(APoint, False);
          end;
          AScreenObject.HigherElevationFormula := FortranFloatToStr(UpperZ +
            MinLayWidth);
          AScreenObject.LowerElevationFormula := FortranFloatToStr(LowerZ -
            MinLayWidth);
        end;
      vdFront:
        begin
          with frmGoPhast.PhastModel.PhastGrid do
          begin
            APoint.X := LowerX - MinColWidth;
            APoint.Y := LowerZ - MinLayWidth;
            AScreenObject.AddPoint(APoint, False);

            APoint.X := LowerX - MinColWidth;
            APoint.Y := UpperZ + MinLayWidth;
            AScreenObject.AddPoint(APoint, False);

            APoint.X := UpperX + MinColWidth;
            APoint.Y := UpperZ + MinLayWidth;
            AScreenObject.AddPoint(APoint, False);

            APoint.X := UpperX + MinColWidth;
            APoint.Y := LowerZ - MinLayWidth;
            AScreenObject.AddPoint(APoint, False);

            APoint.X := LowerX - MinColWidth;
            APoint.Y := LowerZ - MinLayWidth;
            AScreenObject.AddPoint(APoint, False);
          end;
          AScreenObject.HigherElevationFormula := FortranFloatToStr(UpperY +
            MinRowWidth);
          AScreenObject.LowerElevationFormula := FortranFloatToStr(LowerY -
            MinRowWidth);
        end;
      vdSide:
        begin
          with frmGoPhast.PhastModel.PhastGrid do
          begin
            APoint.Y := LowerY - MinRowWidth;
            APoint.X := LowerZ - MinLayWidth;
            AScreenObject.AddPoint(APoint, False);

            APoint.Y := LowerY - MinRowWidth;
            APoint.X := UpperZ + MinLayWidth;
            AScreenObject.AddPoint(APoint, False);

            APoint.Y := UpperY + MinRowWidth;
            APoint.X := UpperZ + MinLayWidth;
            AScreenObject.AddPoint(APoint, False);

            APoint.Y := UpperY + MinRowWidth;
            APoint.X := LowerZ - MinLayWidth;
            AScreenObject.AddPoint(APoint, False);

            APoint.Y := LowerY - MinRowWidth;
            APoint.X := LowerZ - MinLayWidth;
            AScreenObject.AddPoint(APoint, False);
          end;
          AScreenObject.HigherElevationFormula := FortranFloatToStr(UpperX +
            MinColWidth);
          AScreenObject.LowerElevationFormula := FortranFloatToStr(LowerX -
            MinColWidth);
        end;
    else
      Assert(False);
    end;

  end;
  procedure GetItems(const Items: TStringList);
  var
    CharIndex: integer;
    FoundStart: boolean;
    StartPos: integer;
    ItemIndex: integer;
    AnItem: string;
    TimesPosition: integer;
  begin
    if Length(ALine) = 0 then
      Exit;
    FoundStart := false;
    StartPos := -1;
    // make a rough estimate of the number of items in the line.
    Items.Capacity := Length(ALine) div 5;
    // read each character and see if it starts a new item.
    for CharIndex := 1 to Length(ALine) do
    begin
      if CharInSet(ALine[CharIndex], [' ', #9, ',']) then
      begin
        // character is a separator
        if FoundStart then
        begin
          // add the prior item to Items;
          Items.Add(Copy(ALine, StartPos, CharIndex - StartPos));
          FoundStart := False;
        end;
      end
      else
      begin
        if not FoundStart then
        begin
          // mark the beginning of this item.
          StartPos := CharIndex;
          FoundStart := true;
        end;
      end;
    end;
    // If required, add the last item.
    if not CharInSet(ALine[Length(ALine)], [' ', #9, ',']) then
    begin
      if FoundStart then
      begin
        Items.Add(Copy(ALine, StartPos, Length(ALine) - StartPos + 1));
      end;
    end;

    // make sure that each '*' symbols are in the same
    // items as the things it multiplies.
    for ItemIndex := Items.Count - 1 downto 0 do
    begin
      AnItem := Items[ItemIndex];
      TimesPosition := Pos('*', AnItem);
      if TimesPosition = 0 then
      begin
        Continue
      end
      else if TimesPosition = 1 then
      begin
        if AnItem = '*' then
        begin
          Assert(ItemIndex < Items.Count - 1);
          Assert(ItemIndex > 0);
          Items[ItemIndex - 1] := Items[ItemIndex - 1]
            + AnItem + Items[ItemIndex + 1];
          Items.Delete(ItemIndex + 1);
          Items.Delete(ItemIndex);
        end
        else
        begin
          Assert(ItemIndex > 0);
          Items[ItemIndex - 1] := Items[ItemIndex - 1] + AnItem;
          Items.Delete(ItemIndex);
        end;
      end
      else if TimesPosition = Length(AnItem) then
      begin
        Assert(ItemIndex < Items.Count - 1);
        Assert(ItemIndex >= 0);
        Items[ItemIndex] := AnItem + Items[ItemIndex + 1];
      end;
    end;
  end;
  procedure RealLineValues(out RealValues: TOneDRealArray);
  var
    Items: TStringList;
    ItemIndex: integer;
    Counts: TOneDIntegerArray;
    Values: TOneDRealArray;
    AnItem: string;
    TimesPosition: integer;
    FinalSize: integer;
    Count: integer;
    RepeatIndex: integer;
    ValueIndex: integer;
  begin
    FinalSize := 0;
    Items := TStringList.Create;
    try
      GetItems(Items);
      SetLength(Counts, Items.Count);
      SetLength(Values, Items.Count);
      for ItemIndex := 0 to Items.Count - 1 do
      begin
        AnItem := Items[ItemIndex];
        TimesPosition := Pos('*', AnItem);
        if TimesPosition >= 1 then
        begin
          Count := StrToInt(Copy(AnItem, 1, TimesPosition - 1));
          AnItem := Copy(AnItem, TimesPosition + 1, MAXINT);
        end
        else
        begin
          Count := 1;
        end;
        Counts[ItemIndex] := Count;
        Inc(FinalSize, Count);
        Values[ItemIndex] := StrToFloat(AnItem);
      end;
    finally
      Items.Free;
    end;
    SetLength(RealValues, FinalSize);
    ValueIndex := 0;
    for ItemIndex := 0 to Length(Counts) - 1 do
    begin
      for RepeatIndex := 1 to Counts[ItemIndex] do
      begin
        RealValues[ValueIndex] := Values[ItemIndex];
        Inc(ValueIndex);
      end;
    end;
  end;
  procedure IntegerLineValues(out IntegerValues: TOneDIntegerArray);
  var
    Items: TStringList;
    ItemIndex: integer;
    Counts: TOneDIntegerArray;
    Values: TOneDIntegerArray;
    AnItem: string;
    TimesPosition: integer;
    FinalSize: integer;
    Count: integer;
    RepeatIndex: integer;
    ValueIndex: integer;
  begin
    FinalSize := 0;
    Items := TStringList.Create;
    try
      GetItems(Items);
      SetLength(Counts, Items.Count);
      SetLength(Values, Items.Count);
      for ItemIndex := 0 to Items.Count - 1 do
      begin
        AnItem := Items[ItemIndex];
        TimesPosition := Pos('*', AnItem);
        if TimesPosition >= 1 then
        begin
          Count := StrToInt(Copy(AnItem, 1, TimesPosition - 1));
          AnItem := Copy(AnItem, TimesPosition + 1, MAXINT);
        end
        else
        begin
          Count := 1;
        end;
        Counts[ItemIndex] := Count;
        Inc(FinalSize, Count);
        Values[ItemIndex] := StrToInt(AnItem);
      end;
    finally
      Items.Free;
    end;
    SetLength(IntegerValues, FinalSize);
    ValueIndex := 0;
    for ItemIndex := 0 to Length(Counts) - 1 do
    begin
      for RepeatIndex := 1 to Counts[ItemIndex] do
      begin
        IntegerValues[ValueIndex] := Values[ItemIndex];
        Inc(ValueIndex);
      end;
    end;
  end;
  procedure AssignLimits(const Item: TCustomDataListItem; const DataSet: TDataArray);
  begin
    Item.FirstCol := FirstColumn;
    Item.FirstRow := FirstRow;
    Item.FirstLay := FirstLayer;
    Item.LastCol := LastColumn;
    Item.LastRow := LastRow;
    Item.LastLay := LastLayer;
    case DataSet.Orientation of
      dsoTop:
        begin
          Item.FirstLay := 0;
          Item.LastLay := 0;
        end;
      dsoFront:
        begin
          Item.FirstRow := 0;
          Item.LastRow := 0;
        end;
      dsoSide:
        begin
          Item.FirstCol := 0;
          Item.LastCol := 0;
        end;
      dso3D:
        begin
          // do nothing
        end;
    else
      Assert(False);
    end;
  end;
begin
  UndoItem := nil;
  BlockDataSetFiles := TStringList.Create;
  NodeDataSetFiles := TStringList.Create;
  try
    // Make lists of the data sets for which values will be imported.
    for Index := 1 to dgDataSets.RowCount - 1 do
    begin
      if (dgDataSets.Cells[1, Index] <> '')
        and FileExists(dgDataSets.Cells[1, Index]) then
      begin
        DataSet := dgDataSets.Objects[0, Index] as TDataArray;

        case DataSet.EvaluatedAt of
          eaBlocks:
            begin
              BlockDataSetFiles.AddObject(dgDataSets.Cells[1, Index], DataSet);
            end;
          eaNodes:
            begin
              NodeDataSetFiles.AddObject(dgDataSets.Cells[1, Index], DataSet);
            end;
        else
          Assert(False);
        end;
      end;
    end;
    // establish the values of the lower and upper boundaries
    // of the zone to be imported.
    LowerX := StrToFloat(rdeLowerX.Text);
    UpperX := StrToFloat(rdeHigherX.Text);
    LowerY := StrToFloat(rdeLowerY.Text);
    UpperY := StrToFloat(rdeHigherY.Text);
    LowerZ := StrToFloat(rdeLowerZ.Text);
    UpperZ := StrToFloat(rdeHigherZ.Text);
    // create a screen object for importing data by block
    ViewDirection := TViewDirection(comboViewDirection.ItemIndex);

    with frmGoPhast.PhastModel.PhastGrid do
    begin
      MinColWidth := ColumnWidth[0];
      for ColIndex := 1 to ColumnCount - 1 do
      begin
        if MinColWidth > ColumnWidth[ColIndex] then
        begin
          MinColWidth := ColumnWidth[ColIndex];
        end;
      end;
      MinColWidth := MinColWidth / 100;

      MinRowWidth := RowWidth[0];
      for RowIndex := 1 to RowCount - 1 do
      begin
        if MinRowWidth > RowWidth[RowIndex] then
        begin
          MinRowWidth := RowWidth[RowIndex];
        end;
      end;
      MinRowWidth := MinRowWidth / 100;

      MinLayWidth := LayerThickness[0];
      for LayerIndex := 1 to LayerCount - 1 do
      begin
        if MinLayWidth > LayerThickness[LayerIndex] then
        begin
          MinLayWidth := LayerThickness[LayerIndex];
        end;
      end;
      MinLayWidth := MinLayWidth / 100;
    end;

    if BlockDataSetFiles.Count > 0 then
    begin
      AScreenObject :=
        TMultiValueScreenObject.CreateWithViewDirection(frmGoPhast.PhastModel,
        ViewDirection, UndoItem, True);
      try
        frmGoPhast.PhastModel.AddScreenObject(AScreenObject);
        // determine the range of columns, rows, and layers for which data
        // will be imported.
        AScreenObject.ElevationCount := ecTwo;
        AScreenObject.EvaluatedAt := eaBlocks;
        AScreenObject.SetValuesOfEnclosedCells := True;
        with frmGoPhast.PhastModel.PhastGrid do
        begin
          FirstColumn := NearestColumnCenter(LowerX);
          if FirstColumn < 0 then
          begin
            FirstColumn := 0;
          end;
          if FirstColumn > ColumnCount-1 then
          begin
            FirstColumn := ColumnCount-1;
          end;
          if ColumnCenter(FirstColumn) < LowerX then
          begin
            Inc(FirstColumn);
          end;
          {if (FirstColumn < ColumnCount-1)
            and (ColumnPosition[FirstColumn + 1]) < LowerX) then
          begin
            Inc(FirstColumn);
          end;  }

          LastColumn := NearestColumnCenter(UpperX);
          if LastColumn < 0 then
          begin
            LastColumn := 0;
          end;
          if LastColumn > ColumnCount-1 then
          begin
            LastColumn := ColumnCount-1;
          end;
          if ColumnCenter(LastColumn) > UpperX then
          begin
            Dec(LastColumn);
          end;
          {if (LastColumn < ColumnCount)
            and ((ColumnPosition[LastColumn]
            + ColumnPosition[LastColumn - 1]) / 2 > UpperX) then
          begin
            Dec(LastColumn);
          end; }

          FirstRow := NearestRowCenter(LowerY);
          if FirstRow < 0 then
          begin
            FirstRow := 0;
          end;
          if FirstRow > RowCount-1 then
          begin
            FirstRow := RowCount-1;
          end;
          if RowCenter(FirstRow) < LowerY then
          begin
            Inc(FirstRow);
          end;
          {if (FirstRow < RowCount)
            and ((RowPosition[FirstRow]
            + RowPosition[FirstRow + 1]) / 2 < LowerY) then
          begin
            Inc(FirstRow);
          end; }

          LastRow := NearestRowCenter(UpperY);
          if LastRow < 0 then
          begin
            LastRow := 0;
          end;
          if LastRow > RowCount-1 then
          begin
            LastRow := RowCount-1;
          end;
          if RowCenter(LastRow) > UpperY then
          begin
            Dec(LastRow);
          end;
          {if (LastRow < RowCount)
            and ((RowPosition[LastRow]
            + RowPosition[LastRow - 1]) / 2 > UpperY) then
          begin
            Dec(LastRow);
          end;  }

          FirstLayer := NearestLayerCenter(LowerZ);
          if FirstLayer < 0 then
          begin
            FirstLayer := 0;
          end;
          if FirstLayer > LayerCount-1 then
          begin
            FirstLayer := LayerCount-1;
          end;
          if LayerCenter(FirstLayer) < LowerZ then
          begin
            Inc(FirstLayer);
          end;
          {if (FirstLayer < LayerCount)
            and ((LayerElevation[FirstLayer]
            + LayerElevation[FirstLayer + 1]) / 2 < LowerZ) then
          begin
            Inc(FirstLayer);
          end; }

          LastLayer := NearestLayerCenter(UpperZ);
          if LastLayer < 0 then
          begin
            LastLayer := 0;
          end;
          if LastLayer > LayerCount-1 then
          begin
            LastLayer := LayerCount-1;
          end;
          if LayerCenter(LastLayer) > UpperZ then
          begin
            Dec(LastLayer);
          end;
          {if (LastLayer < LayerCount)
            and ((LayerElevation[LastLayer]
            + LayerElevation[LastLayer - 1]) / 2 > UpperZ) then
          begin
            Dec(LastLayer);
          end;    }

        end;
        // Add points for each block in the zone
        AddPointsToScreenObject;

        // add data values for each block in the zone.
        DataValues := TStringList.Create;
        try
          for DataSetIndex := 0 to BlockDataSetFiles.Count - 1 do
          begin
            ValueIndex := 0;
            try
              DataValues.LoadFromFile(BlockDataSetFiles[DataSetIndex]);
            except on EFOpenError do
              begin
                CantOpenFileMessage(BlockDataSetFiles[DataSetIndex]);
                Exit;
              end;
            end;

            DataSet := BlockDataSetFiles.Objects[DataSetIndex] as TDataArray;
            case DataSet.DataType of
              rdtDouble:
                begin
                  RealItem := AScreenObject.RealValues.Add as TRealDataListItem;
                  RealItem.DataSetName := DataSet.Name;
                  AssignLimits(RealItem, DataSet);
                  RealItem.Length :=
                    (RealItem.LastLay - RealItem.FirstLay + 1)
                    * (RealItem.LastRow - RealItem.FirstRow + 1)
                    * (RealItem.LastCol - RealItem.FirstCol + 1);

                  for LineIndex := 0 to DataValues.Count - 1 do
                  begin
                    ALine := DataValues[LineIndex];
                    RealLineValues(RealValues);
                    for ItemIndex := 0 to Length(RealValues) - 1 do
                    begin
                      Assert(ValueIndex < RealItem.Length);
                      RealItem.Values[ValueIndex] := RealValues[ItemIndex];
                      Inc(ValueIndex);
                    end;
                  end;
                  DataSetPosition := AScreenObject.AddDataSet(DataSet);
                  Assert(DataSetPosition >= 0);
                  AScreenObject.DataSetFormulas[DataSetPosition] :=
                    rsListRealValue
                    + '("' + DataSet.Name + '")';
                end;
              rdtInteger:
                begin
                  IntegerItem := AScreenObject.IntegerValues.Add as
                    TIntegerDataListItem;
                  IntegerItem.DataSetName := DataSet.Name;
                  AssignLimits(IntegerItem, DataSet);
                  IntegerItem.Length :=
                    (IntegerItem.LastLay - IntegerItem.FirstLay + 1)
                    * (IntegerItem.LastRow - IntegerItem.FirstRow + 1)
                    * (IntegerItem.LastCol - IntegerItem.FirstCol + 1);
                  for LineIndex := 0 to DataValues.Count - 1 do
                  begin
                    ALine := DataValues[LineIndex];
                    IntegerLineValues(IntegerValues);
                    for ItemIndex := 0 to Length(IntegerValues) - 1 do
                    begin
                      Assert(ValueIndex < IntegerItem.Length);
                      IntegerItem.Values[ValueIndex] :=
                        IntegerValues[ItemIndex];
                      Inc(ValueIndex);
                    end;
                  end;
                  DataSetPosition := AScreenObject.AddDataSet(DataSet);
                  Assert(DataSetPosition >= 0);
                  AScreenObject.DataSetFormulas[DataSetPosition] :=
                    rsListIntegerValue
                    + '("' + DataSet.Name + '")';
                end;
            else
              Assert(False);
            end;

          end;
        finally
          DataValues.Free;
        end;
        (UndoItem as TUndoCreateScreenObject).SetPostSelection;
        frmGoPhast.UndoStack.Submit(UndoItem);
        frmGoPhast.frameTopView.PreviousScreenObjects.Add(AScreenObject);
      except
        frmGoPhast.PhastModel.RemoveScreenObject(AScreenObject);
        AScreenObject.Free;
        UndoItem.Free;
        raise;
      end;
    end;
    // create a screen object for importing data by block
    if NodeDataSetFiles.Count > 0 then
    begin
      AScreenObject :=
        TMultiValueScreenObject.CreateWithViewDirection(frmGoPhast.PhastModel,
        ViewDirection, UndoItem, True);
      try
        frmGoPhast.PhastModel.AddScreenObject(AScreenObject);
        AScreenObject.ElevationCount := ecTwo;
        AScreenObject.EvaluatedAt := eaNodes;
        AScreenObject.SetValuesOfEnclosedCells := True;
        // determine the range of columns, rows, and layers for which data
        // will be imported.
        with frmGoPhast.PhastModel.PhastGrid do
        begin
          FirstColumn := NearestColumnPosition(LowerX);
          if FirstColumn < 0 then
          begin
            FirstColumn := 0;
          end;
          if FirstColumn > ColumnCount then
          begin
            FirstColumn := ColumnCount;
          end;
          if ColumnPosition[FirstColumn] < LowerX then
          begin
            Inc(FirstColumn);
          end;

          LastColumn := NearestColumnPosition(UpperX);
          if LastColumn < 0 then
          begin
            LastColumn := 0;
          end;
          if LastColumn > ColumnCount then
          begin
            LastColumn := ColumnCount;
          end;
          if ColumnPosition[LastColumn] > UpperX then
          begin
            Dec(LastColumn);
          end;

          FirstRow := NearestRowPosition(LowerY);
          if FirstRow < 0 then
          begin
            FirstRow := 0;
          end;
          if FirstRow > RowCount then
          begin
            FirstRow := RowCount;
          end;
          if RowPosition[FirstRow] < LowerY then
          begin
            Inc(FirstRow);
          end;

          LastRow := NearestRowPosition(UpperY);
          if LastRow < 0 then
          begin
            LastRow := 0;
          end;
          if LastRow > RowCount then
          begin
            LastRow := RowCount;
          end;
          if RowPosition[LastRow] > UpperY then
          begin
            Dec(LastRow);
          end;

          FirstLayer := NearestLayerPosition(LowerZ);
          if FirstLayer < 0 then
          begin
            FirstLayer := 0;
          end;
          if FirstLayer > LayerCount then
          begin
            FirstLayer := LayerCount;
          end;
          if LayerElevation[FirstLayer] < LowerZ then
          begin
            Inc(FirstLayer);
          end;

          LastLayer := NearestLayerPosition(UpperZ);
          if LastLayer < 0 then
          begin
            LastLayer := 0;
          end;
          if LastLayer > LayerCount then
          begin
            LastLayer := LayerCount;
          end;
          if LayerElevation[LastLayer] > UpperZ then
          begin
            Dec(LastLayer);
          end;
        end;
        AddPointsToScreenObject;

        // add data values for each node in the zone.
        DataValues := TStringList.Create;
        try
          for DataSetIndex := 0 to NodeDataSetFiles.Count - 1 do
          begin
            ValueIndex := 0;
            try
              DataValues.LoadFromFile(NodeDataSetFiles[DataSetIndex]);
            except on EFOpenError do
              begin
                CantOpenFileMessage(NodeDataSetFiles[DataSetIndex]);
                Exit;
              end;
            end;

            DataSet := NodeDataSetFiles.Objects[DataSetIndex] as TDataArray;
            case DataSet.DataType of
              rdtDouble:
                begin
                  RealItem := AScreenObject.RealValues.Add as TRealDataListItem;
                  RealItem.DataSetName := DataSet.Name;
                  AssignLimits(RealItem, DataSet);
                  RealItem.Length :=
                    (RealItem.LastLay - RealItem.FirstLay + 1)
                    * (RealItem.LastRow - RealItem.FirstRow + 1)
                    * (RealItem.LastCol - RealItem.FirstCol + 1);
                  for LineIndex := 0 to DataValues.Count - 1 do
                  begin
                    ALine := DataValues[LineIndex];
                    RealLineValues(RealValues);
                    for ItemIndex := 0 to Length(RealValues) - 1 do
                    begin
                      Assert(ValueIndex < RealItem.Length);
                      RealItem.Values[ValueIndex] := RealValues[ItemIndex];
                      Inc(ValueIndex);
                    end;
                  end;
                  DataSetPosition := AScreenObject.AddDataSet(DataSet);
                  Assert(DataSetPosition >= 0);
                  AScreenObject.DataSetFormulas[DataSetPosition] :=
                    rsListRealValue
                    + '("' + DataSet.Name + '")';
                end;
              rdtInteger:
                begin
                  IntegerItem := AScreenObject.IntegerValues.Add as
                    TIntegerDataListItem;
                  IntegerItem.DataSetName := DataSet.Name;
                  AssignLimits(IntegerItem, DataSet);
                  IntegerItem.Length :=
                    (IntegerItem.LastLay - IntegerItem.FirstLay + 1)
                    * (IntegerItem.LastRow - IntegerItem.FirstRow + 1)
                    * (IntegerItem.LastCol - IntegerItem.FirstCol + 1);
                  for LineIndex := 0 to DataValues.Count - 1 do
                  begin
                    ALine := DataValues[LineIndex];
                    IntegerLineValues(IntegerValues);
                    for ItemIndex := 0 to Length(IntegerValues) - 1 do
                    begin
                      Assert(ValueIndex < IntegerItem.Length);
                      IntegerItem.Values[ValueIndex] :=
                        IntegerValues[ItemIndex];
                      Inc(ValueIndex);
                    end;
                  end;
                  DataSetPosition := AScreenObject.AddDataSet(DataSet);
                  Assert(DataSetPosition >= 0);
                  AScreenObject.DataSetFormulas[DataSetPosition] :=
                    rsListIntegerValue
                    + '("' + DataSet.Name + '")';
                end;
            else
              Assert(False);
            end;
          end;
        finally
          DataValues.Free;
        end;
        (UndoItem as TUndoCreateScreenObject).SetPostSelection;
        frmGoPhast.UndoStack.Submit(UndoItem);
        frmGoPhast.frameTopView.PreviousScreenObjects.Add(AScreenObject);
      except
        frmGoPhast.PhastModel.RemoveScreenObject(AScreenObject);
        AScreenObject.Free;
        UndoItem.Free;
        raise;
      end;
    end;
  finally
    BlockDataSetFiles.Free;
    NodeDataSetFiles.Free;
  end;
end;

procedure TfrmImportDistributedData.FormCreate(Sender: TObject);
begin
  inherited;
  dgDataSets.FixedColor := Color;
//  dgDataSets.ButtonColor := Color;
  dgDataSets.Cells[0, 0] := StrDataSets;
  dgDataSets.Cells[1, 0] := StrFilePath;
  GetData;
end;

procedure TfrmImportDistributedData.dgDataSetsButtonClicked(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  if OpenDialogFile.Execute then
  begin
    dgDataSets.Cells[ACol, ARow] := OpenDialogFile.FileName;
    EnableOK;
  end;
end;

procedure TfrmImportDistributedData.btnOKClick(Sender: TObject);
begin
  inherited;
  Hide;
  SetData;
end;

procedure TfrmImportDistributedData.comboViewDirectionChange(
  Sender: TObject);
begin
  inherited;
  GetDataSets;
  dgDataSets.Invalidate;
end;

procedure TfrmImportDistributedData.FormResize(Sender: TObject);
begin
  inherited;
  ResizeColumns
end;

end.

