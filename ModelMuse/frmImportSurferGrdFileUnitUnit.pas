unit frmImportSurferGrdFileUnitUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomImportSimpleFileUnit, StdCtrls,
  Buttons, ExtCtrls, Grids, RbwDataGrid4, SurferGridFileReaderUnit,
  frmImportShapefileUnit, FastGEO, AbstractGridUnit,
  GoPhastTypes, MeshRenumberingTypes;

type
  {@abstract(@name is the command used to import
    Surfer Grid files or reverse the import.)}
  TUndoImportGrdFile = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TfrmImportSurferGrdFile = class(TfrmCustomImportSimpleFile)
    rdgLimits: TRbwDataGrid4;
    Label1: TLabel;
    rgFilterMethod: TRadioGroup;
    lblConvert: TLabel;
    comboFromUnits: TComboBox;
    lblTo: TLabel;
    comboToUnits: TComboBox;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure rgEvaluatedAtClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
  private
    FGrd6: TSurfer6Grid;
    FFileType: TSurferFileType;
    FGrd7: TSurferRaster7File2;
    FGrid: TCustomModelGrid;
//    FMesh: TSutraMesh3D;
    ImportMethod: TImportMethod;
    EvalAt: TEvaluatedAt;
    FMesh: IMesh3D;
    procedure HandleARasterPoint(Sender: TObject; APoint: TPoint3D);
    procedure SetData;
    { Private declarations }
  public
    function GetData: boolean;
    { Public declarations }
  end;

var
  frmImportSurferGrdFile: TfrmImportSurferGrdFile;

implementation

uses
  frmGoPhastUnit, DataSetUnit, ScreenObjectUnit, ModelMuseUtilities,
  UndoItems, ValueArrayStorageUnit, GIS_Functions,
  frmImportAsciiRasterUnit, DataSetNamesUnit;

resourcestring
  StrImportSurferGridF = 'import Surfer grid file';
  StrX = 'X';
  StrY = 'Y';
  StrZ = 'Z';
  StrMinimum = 'Minimum';
  StrMaximum = 'Maximum';
  CommentStr = 'Minimum X %0g' + sLineBreak
    + 'Maximum X %1g' + sLineBreak
    + 'Minimum Y %2g' + sLineBreak
    + 'Maximum Y %3g' + sLineBreak
    + 'Minimum Z %4g' + sLineBreak
    + 'Maximum Z %5g' + sLineBreak;
  StrImportedFromSurfer = 'Imported from Surfer Grid files';
  StrNumberOfColumnsRo = 'Number of Columns/Rows';
  StrDeltaXY = 'Delta X/Y';
  StrThereWasA0sE = 'There was a "%0:s" error reading the Surfer grid file. ' +
  'The error message was "%1:s."';

const
  StrGrdZ = '_Grd_Z';

{$R *.dfm}

var
  FilterIndex : integer = 0;

function ConvertPoint(const SurferPoint: TSurferPoint): TPoint2D;
begin
  result.X := SurferPoint.X;
  result.Y := SurferPoint.Y;
end;


procedure TfrmImportSurferGrdFile.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmImportSurferGrdFile.FormCreate(Sender: TObject);
begin
  inherited;
  rdgLimits.BeginUpdate;
  try
    rdgLimits.Cells[1,0] := StrX;
    rdgLimits.Cells[2,0] := StrY;
    rdgLimits.Cells[3,0] := StrZ;
    rdgLimits.Cells[0,1] := StrMinimum;
    rdgLimits.Cells[0,2] := StrMaximum;
    rdgLimits.Cells[0,3] := StrNumberOfColumnsRo;
    rdgLimits.Cells[0,4] := StrDeltaXY;
  finally
    rdgLimits.EndUpdate;
  end;
end;


procedure TfrmImportSurferGrdFile.FormDestroy(Sender: TObject);
begin
  inherited;
  FGrd7.Free;
end;

function TfrmImportSurferGrdFile.GetData: boolean;
begin
  UpdateEvalAt;
  OpenDialogFile.FilterIndex := FilterIndex;
  result := OpenDialogFile.Execute;
  if result then
  begin
    try
      FFileType := SurferFileType(OpenDialogFile.FileName);
    except
      on E: EGrdReadError do
      begin
        result := False;
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
        Exit;
      end;
      on E: EFOpenError do
      begin
        result := False;
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
    try
      case FFileType of
        sft6:
          begin
            ReadSurfer6GrdFile(OpenDialogFile.FileName, FGrd6);
            rdgLimits.BeginUpdate;
            try
              rdgLimits.Cells[1,1] := FloatToStr(FGrd6.Header.Xlo);
              rdgLimits.Cells[1,2] := FloatToStr(FGrd6.Header.Xhi);
              rdgLimits.Cells[2,1] := FloatToStr(FGrd6.Header.Ylo);
              rdgLimits.Cells[2,2] := FloatToStr(FGrd6.Header.Yhi);
              rdgLimits.Cells[3,1] := FloatToStr(FGrd6.Header.Zlo);
              rdgLimits.Cells[3,2] := FloatToStr(FGrd6.Header.Zhi);
              rdgLimits.Cells[1,3] := IntToStr(FGrd6.Header.nx);
              rdgLimits.Cells[2,3] := IntToStr(FGrd6.Header.ny);
              rdgLimits.Cells[1,4] := FloatToStr((FGrd6.Header.Xhi - FGrd6.Header.Xlo)/FGrd6.Header.nx);
              rdgLimits.Cells[2,4] := FloatToStr((FGrd6.Header.Yhi - FGrd6.Header.Ylo)/FGrd6.Header.ny);
            finally
              rdgLimits.EndUpdate;
            end;
          end;
        sft7:
          begin
            FGrd7 := TSurferRaster7File2.Create(OpenDialogFile.FileName);
  //          ReadSurfer7GrdFile(OpenDialogFile.FileName, FGrd7);
            rdgLimits.BeginUpdate;
            try
              rdgLimits.Cells[1,1] := FloatToStr(FGrd7.Header.xLL);
              rdgLimits.Cells[1,2] := FloatToStr(FGrd7.Header.xLL
                + FGrd7.Header.xSize * FGrd7.Header.nCol);
              rdgLimits.Cells[2,1] := FloatToStr(FGrd7.Header.yLL);
              rdgLimits.Cells[2,2] := FloatToStr(FGrd7.Header.yLL
                + FGrd7.Header.ySize * FGrd7.Header.nRow);
              rdgLimits.Cells[3,1] := FloatToStr(FGrd7.Header.zMin);
              rdgLimits.Cells[3,2] := FloatToStr(FGrd7.Header.zMax);
              rdgLimits.Cells[1,3] := IntToStr(FGrd7.Header.nCol);
              rdgLimits.Cells[2,3] := IntToStr(FGrd7.Header.nRow);
              rdgLimits.Cells[1,4] := FloatToStr(FGrd7.Header.xSize);
              rdgLimits.Cells[2,4] := FloatToStr(FGrd7.Header.ySize);
            finally
              rdgLimits.EndUpdate;
            end;
          end;
        sftAscii:
          begin
            ReadSurferAsciiFile(OpenDialogFile.FileName, FGrd6);
            rdgLimits.BeginUpdate;
            try
              rdgLimits.Cells[1,1] := FloatToStr(FGrd6.Header.Xlo);
              rdgLimits.Cells[1,2] := FloatToStr(FGrd6.Header.Xhi);
              rdgLimits.Cells[2,1] := FloatToStr(FGrd6.Header.Ylo);
              rdgLimits.Cells[2,2] := FloatToStr(FGrd6.Header.Yhi);
              rdgLimits.Cells[3,1] := FloatToStr(FGrd6.Header.Zlo);
              rdgLimits.Cells[3,2] := FloatToStr(FGrd6.Header.Zhi);
              rdgLimits.Cells[1,3] := IntToStr(FGrd6.Header.nx);
              rdgLimits.Cells[2,3] := IntToStr(FGrd6.Header.ny);
              rdgLimits.Cells[1,4] := FloatToStr((FGrd6.Header.Xhi - FGrd6.Header.Xlo)/FGrd6.Header.nx);
              rdgLimits.Cells[2,4] := FloatToStr((FGrd6.Header.Yhi - FGrd6.Header.Ylo)/FGrd6.Header.ny);
            finally
              rdgLimits.EndUpdate;
            end;
          end;
        else Assert(False);
      end;
    except on E: Exception do
      begin
        result := False;
        Beep;
        MessageDlg(Format(StrThereWasA0sE, [E.ClassName, E.message]), mtError,
          [mbOK], 0);
        Exit;
      end;
    end;
    GetDataSets;
    comboDataSets.ItemIndex := 0;
    comboInterpolators.ItemIndex := 1;
    FilterIndex := OpenDialogFile.FilterIndex;
  end;
end;

procedure TfrmImportSurferGrdFile.HandleARasterPoint(Sender: TObject;
  APoint: TPoint3D);
var
  Point2D: TPoint2D;
begin
  if (FGrid <> nil) and (FGrid.GridAngle <> 0) then
  begin
    Point2D.x := APoint.X;
    Point2D.y := APoint.Y;
    Point2D := FGrid.
      RotateFromRealWorldCoordinatesToGridCoordinates(Point2D);
    APoint.x := Point2D.x;
    APoint.y := Point2D.y;
  end;
  HandleAPoint(APoint, ImportMethod, EvalAt, FGrid, FMesh);
end;

procedure TfrmImportSurferGrdFile.rgEvaluatedAtClick(Sender: TObject);
var
  NodeElemString: string;
//  EvalAt: TEvaluatedAt;
  CenterString: string;
begin
  inherited;
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msSutra22, msSutra30, msSutra40:
      begin
        case EvalAt of
          eaBlocks:
            begin
              NodeElemString := StrElement;
              CenterString := StrElementCenter
            end;
          eaNodes:
            begin
              NodeElemString := StrCell;
              CenterString := StrNode;
            end;
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
      :
      begin
        NodeElemString := StrCell;
        CenterString := StrCellCenter
      end;
    else Assert(False);
  end;
  rgFilterMethod.Items[Ord(imLowest)] :=
    Format(StrLowestPointInS, [NodeElemString]);
  rgFilterMethod.Items[Ord(imHighest)] :=
    Format(StrHighestPointInS, [NodeElemString]);
  rgFilterMethod.Items[Ord(imAverage)] :=
    Format(StrAverageOfPointsIn, [NodeElemString]);
  rgFilterMethod.Items[Ord(imClosest)] :=
    Format(StrPointClosestToS, [CenterString]);
end;

procedure TfrmImportSurferGrdFile.SetData;
const
  BlankValue = 1.70141e+38;
  Epsilon = 1e-8;
var
  DataSetName: string;
  DataSet: TDataArray;
  ScreenObjectList: TList;
  Undo: TUndoImportGrdFile;
  Root: string;
  ExistingObjectCount: integer;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  PointIndex: Integer;
  Item: TValueArrayItem;
  Formula: string;
  NewDataSets: TList;
  Position: integer;
  Count: Integer;
  PointCount: Integer;
  YIndex: Integer;
  XIndex: Integer;
  UsedIndex: Integer;
  Z: Double;
  APoint3D: TSurferPoint;
  ConvertUnits: boolean;
  FromUnits: TSupportedLengthConv;
  ToUnits: TSupportedLengthConv;
  APoint2D: TPoint2D;
  function ConvertPoint2(ASurferPoint: TSurferPoint): TPoint3D;
  begin
    Result.x := ASurferPoint.X;
    Result.Y := ASurferPoint.Y;
    Result.z := ASurferPoint.Z;
  end;
begin
  ConvertUnits := (comboFromUnits.ItemIndex > 0) and (comboToUnits.ItemIndex > 0);
  if ConvertUnits then
  begin
    FromUnits := TSupportedLengthConv(comboFromUnits.ItemIndex-1);
    ToUnits := TSupportedLengthConv(comboToUnits.ItemIndex-1);
  end
  else
  begin
    FromUnits := slcMeter;
    ToUnits := slcMeter;
  end;

  LocalModel := frmGoPhast.PhastModel;
  frmGoPhast.PhastModel.BeginScreenObjectUpdate;
  frmGoPhast.CanDraw := False;
  try
    NewDataSets := TList.Create;
    try
      MakeNewDataSet(NewDataSets, StrGrdZ, strDefaultClassification + '|' + StrImportedFromSurfer,
        comboDataSets.ItemIndex = 0);
      DataSetName := comboDataSets.Text;
      DataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(DataSetName);
      case FFileType of
        sft6, sftAscii:
          begin
            DataSet.Comment := Format(CommentStr,
              [FGrd6.Header.Xlo, FGrd6.Header.Xhi,
              FGrd6.Header.Ylo, FGrd6.Header.Yhi,
              FGrd6.Header.Zlo, FGrd6.Header.Zhi]);
          end;
        sft7:
          begin
            DataSet.Comment := Format(CommentStr,
              [FGrd7.Header.xLL,
              FGrd7.Header.xLL + FGrd7.Header.xSize* FGrd7.Header.nCol,
              FGrd7.Header.yLL,
              FGrd7.Header.yLL + FGrd7.Header.ySize* FGrd7.Header.nRow,
              FGrd7.Header.zMin, FGrd7.Header.zMax]);
          end
        else Assert(False);
      end;
      Assert(DataSet <> nil);
      ScreenObjectList := TList.Create;
      //MultipleParts := false;
      try
        Undo := TUndoImportGrdFile.Create;
        try
          try
            if rgFilterMethod.ItemIndex <> 4 then
            begin
              ImportMethod := TImportMethod(rgFilterMethod.ItemIndex);
              FGrid := LocalModel.Grid;
              FMesh := LocalModel.Mesh3D;
              if not InitializeArrays(ImportMethod) then
              begin
                Exit;
              end;
              GetDiscretizationMinMax
            end;


            Root := TScreenObject.ValidName(
              ExtractFileRoot(OpenDialogFile.FileName));
            ScreenObjectList.Capacity := 1;
            ExistingObjectCount :=
              frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

            AScreenObject :=
              TScreenObject.CreateWithViewDirection(
              frmGoPhast.PhastModel, vdTop,
              UndoCreateScreenObject, False);
            AScreenObject.Comment := 'Imported from ' + OpenDialogFile.FileName +' on ' + DateTimeToStr(Now);
            if ExistingObjectCount > 0 then
            begin
              AScreenObject.Name := Root + '_'+ IntToStr(ExistingObjectCount);
            end
            else
            begin
              AScreenObject.Name := Root;
            end;
            AScreenObject.SetValuesOfIntersectedCells
              := cbIntersectedCells.Checked;
            AScreenObject.SetValuesByInterpolation
              := cbInterpolation.Checked;
            AScreenObject.ElevationCount := ecZero;
            AScreenObject.EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
            AScreenObject.Visible := False;


            Position := -1;
            case FFileType of
              sft6, sftAscii:
                begin
                  if rgFilterMethod.ItemIndex = 4 then
                  begin
                    AScreenObject.Capacity := FGrd6.Header.nx * FGrd6.Header.ny;
                    for PointIndex := 0 to Length(FGrd6.Points) - 1 do
                    begin
                      APoint3D := FGrd6.Points[PointIndex];
                      if Abs(APoint3D.Z - BlankValue)/BlankValue >= Epsilon then
                      begin
                        APoint2D := ConvertPoint(FGrd6.Points[PointIndex]);
                        if ConvertUnits then
                        begin
                          APoint2D := ConvertPoint2D(APoint2D, FromUnits, ToUnits);
                        end;
                        AScreenObject.AddPoint(APoint2D, True);
                      end;
                    end;
                    ScreenObjectList.Add(AScreenObject);
                    Position := AScreenObject.AddDataSet(DataSet);
                    Assert(Position >= 0);

                    Item := AScreenObject.
                      ImportedValues.Add as TValueArrayItem;
                    Item.Name := DataSet.Name;
                    Item.Values.DataType := DataSet.DataType;
                    Item.Values.Count := FGrd6.Header.nx * FGrd6.Header.ny;

                    UsedIndex := 0;
                    for PointIndex := 0 to Length(FGrd6.Points) - 1 do
                    begin
                      Z := FGrd6.Points[PointIndex].Z;
                      if Abs(Z - BlankValue)/BlankValue >= Epsilon then
                      begin
                        Item.Values.RealValues[UsedIndex] := Z;
                        Inc(UsedIndex);
                      end;
                    end;
                    Item.Values.Count := UsedIndex;
                    Item.CacheData;
                  end
                  else
                  begin
                    ScreenObjectList.Add(AScreenObject);
                    Position := AScreenObject.AddDataSet(DataSet);
                    Assert(Position >= 0);
                    for PointIndex := 0 to Length(FGrd6.Points) - 1 do
                    begin
                      APoint3D := FGrd6.Points[PointIndex];
                      if Abs(APoint3D.Z - BlankValue)/BlankValue >= Epsilon then
                      begin
                        HandleARasterPoint(nil, ConvertPoint2(APoint3D));
                      end;
                    end;
                  end;
                end;
              sft7:
                begin
                  if rgFilterMethod.ItemIndex = 4 then
                  begin
                    Count := 0;
                    for YIndex := 0 to FGrd7.YCount - 1 do
                    begin
                      for XIndex := 0 to FGrd7.XCount - 1 do
                      begin
                        if FGrd7.Z[XIndex,YIndex] < FGrd7.Header.BlankValue then
                        begin
                          Inc(Count);
                        end;
                      end;
                    end;
//                    for PointIndex := 0 to Length(FGrd7.Points) - 1 do
//                    begin
//                      if FGrd7.Points[PointIndex].Z < FGrd7.Header.BlankValue then
//                      begin
//                        Inc(Count);
//                      end;
//                    end;

//                    SetLength(X, Surfer7Grid.Header.nCol);
//                    for ColIndex := 0 to Surfer7Grid.Header.nCol - 1 do
//                    begin
//                      X[ColIndex] := Surfer7Grid.Header.xLL
//                        + ColIndex * Surfer7Grid.Header.xSize
//                    end;
//
//                    SetLength(Y, Surfer7Grid.Header.nRow);
//                    for RowIndex := 0 to Surfer7Grid.Header.nRow - 1 do
//                    begin
//                      Y[RowIndex] := Surfer7Grid.Header.yLL
//                        + RowIndex * Surfer7Grid.Header.ySize
//                    end;


                    AScreenObject.Capacity := Count;
                    for YIndex := 0 to FGrd7.YCount - 1 do
                    begin
                      for XIndex := 0 to FGrd7.XCount - 1 do
                      begin
                        if FGrd7.Z[XIndex,YIndex] < FGrd7.Header.BlankValue then
                        begin
//                          Inc(Count);
//                          APoint2D.x := X[XIndex];
                          APoint2D := ConvertPoint(FGrd7.Points[XIndex,YIndex]);
                          if ConvertUnits then
                          begin
                            APoint2D := ConvertPoint2D(APoint2D, FromUnits, ToUnits);
                          end;
                          AScreenObject.AddPoint(APoint2D, True);
                        end;
                      end;
                    end;
//                    for PointIndex := 0 to Length(FGrd7.Points) - 1 do
//                    begin
//                      if FGrd7.Points[PointIndex].Z < FGrd7.Header.BlankValue then
//                      begin
//                        AScreenObject.AddPoint(ConvertPoint(FGrd7.Points[PointIndex]), True);
//                      end;
//                    end;
                    ScreenObjectList.Add(AScreenObject);
                    Position := AScreenObject.AddDataSet(DataSet);
                    Assert(Position >= 0);

                    Item := AScreenObject.
                      ImportedValues.Add as TValueArrayItem;
                    Item.Name := DataSet.Name;
                    Item.Values.DataType := DataSet.DataType;
                    Item.Values.Count := Count;

                    PointCount := 0;
                    for YIndex := 0 to FGrd7.YCount - 1 do
                    begin
                      for XIndex := 0 to FGrd7.XCount - 1 do
                      begin
                        if FGrd7.Z[XIndex,YIndex] < FGrd7.Header.BlankValue then
                        begin
                          Item.Values.RealValues[PointCount] :=
                            FGrd7.Z[XIndex, YIndex];
                          Inc(PointCount);
                        end;
                      end;
                    end;
                    Item.CacheData;
                  end


//                    for PointIndex := 0 to Length(FGrd7.Points) - 1 do
//                    begin
//                      if FGrd7.Points[PointIndex].Z < FGrd7.Header.BlankValue then
//                      begin
//                        Item.Values.RealValues[PointCount] := FGrd7.Points[PointIndex].Z;
//                        Inc(PointCount);
//                      end;
//                    end;
//                    Item.CacheData;
//                end;
                  else
                  begin
                    ScreenObjectList.Add(AScreenObject);
                    Position := AScreenObject.AddDataSet(DataSet);
                    Assert(Position >= 0);

                    for YIndex := 0 to FGrd7.YCount - 1 do
                    begin
                      for XIndex := 0 to FGrd7.XCount - 1 do
                      begin
                        if FGrd7.Z[XIndex,YIndex] < FGrd7.Header.BlankValue then
                        begin
                          HandleARasterPoint(nil, ConvertPoint2(FGrd7.Points[XIndex, YIndex]));
                        end;
                      end;
                    end;


  //                    for PointIndex := 0 to Length(FGrd7.Points) - 1 do
  //                    begin
  //                      if FGrd7.Points[PointIndex].Z < FGrd7.Header.BlankValue then
  //                      begin
  //                        HandleARasterPoint(nil, ConvertPoint2(FGrd7.Points[PointIndex]));
  //                      end;
  //                    end;
                  end;
                end;

              else Assert(False);
            end;

            if rgFilterMethod.ItemIndex <> 4 then
            begin
              ComputeAverage(ImportMethod);

              Item := AScreenObject.
                ImportedValues.Add as TValueArrayItem;
              Item.Name := DataSet.Name;
              Item.Values.DataType := DataSet.DataType;
              if LocalModel.Grid <> nil then
              begin
                case EvalAt of
                  eaBlocks:
                    begin
                      AScreenObject.Capacity := FGrid.ColumnCount
                        * FGrid.RowCount;
                    end;
                  eaNodes:
                    begin
                      AScreenObject.Capacity :=
                        (FGrid.ColumnCount+1)
                        * (FGrid.RowCount + 1);
                    end;
                  else
                    Assert(False);
                end;
              end
              else
              begin
                case EvalAt of
                  eaBlocks:
                    begin
                      AScreenObject.Capacity := FMesh.Mesh2DI.ElementCount;
                    end;
                  eaNodes:
                    begin
                      AScreenObject.Capacity := FMesh.Mesh2DI.NodeCount;
                    end;
                  else
                    Assert(False);
                end;
              end;

              Item.Values.Count := AScreenObject.Capacity;
              AssignPointsAndValues(FGrid, AScreenObject, Item{, DataSetIndex <> 0});
            end;

            Formula := rsObjectImportedValuesR + '("' + DataSet.Name + '")';
            AScreenObject.DataSetFormulas[Position] := Formula;

            Undo.StoreNewScreenObjects(ScreenObjectList);
            Undo.StoreNewDataSets(NewDataSets);
            frmGoPhast.UndoStack.Submit(Undo);
            Undo := nil;
            frmGoPhast.PhastModel.AddFileToArchive(OpenDialogFile.FileName);
          finally
            Undo.Free;
          end;
        except
          raise;
        end;
      finally
        ScreenObjectList.Free;
      end;
    finally
      NewDataSets.Free
    end;
  finally
    frmGoPhast.CanDraw := True;
    frmGoPhast.PhastModel.EndScreenObjectUpdate;
  end;

end;

{ TUndoImportGrdFile }

function TUndoImportGrdFile.Description: string;
begin
  result := StrImportSurferGridF;
end;

end.
