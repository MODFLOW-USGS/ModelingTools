unit frmImportTprogsUnit;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Mask, JvExMask, JvToolEdit, ArgusDataEntry, Buttons, SsButtonEd,
  RbwStringTreeCombo, DataSetUnit, ExtCtrls,
  frmImportShapefileUnit, GoPhastTypes, FastGEO;

type
  TUndoImportTprogs = class(TUndoImportShapefile)
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TfrmImportTprogs = class(TfrmCustomGoPhast)
    fedTprogs: TJvFilenameEdit;
    lblTprogs: TLabel;
    rdeXOrigin: TRbwDataEntry;
    lblXOrigin: TLabel;
    rdeDeltaX: TRbwDataEntry;
    lblDeltaX: TLabel;
    lblYOrigin: TLabel;
    lblDeltaY: TLabel;
    rdeYOrigin: TRbwDataEntry;
    rdeDeltaY: TRbwDataEntry;
    lblZOrigin: TLabel;
    lblDeltaZ: TLabel;
    rdeZOrigin: TRbwDataEntry;
    rdeDeltaZ: TRbwDataEntry;
    lblAngle: TLabel;
    rdeAngle: TRbwDataEntry;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbVisible: TCheckBox;
    rgDataType: TRadioGroup;
    rgFileType: TRadioGroup;
    rgEvaluatedAt: TRadioGroup;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
  private
    NCOL, NROW, NLAY: integer;
    RealData: array of array of array of single;
    IntegerData: array of array of array of integer;
    XYArray: array of array of TPoint2D;
    ZArray: TOneDRealArray;
    FDataSets: TList;
    NewDataSetName3D: string;
    procedure GetData;
    procedure SetData;
    function ReadTProgFile: boolean;
    procedure MakeNewDataSets(NewDataSets: TList);
    procedure MakeNewScreenObject(ScreenObjectList: TList);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportTprogs: TfrmImportTprogs;

implementation

{$R *.dfm}

uses frmGoPhastUnit, RbwParser,
  AbstractGridUnit, IOUtils, PhastModelUnit, InterpolationUnit,
  ScreenObjectUnit, ValueArrayStorageUnit, UndoItems, ModelMuseUtilities,
  GIS_Functions, SutraMeshUnit;

resourcestring
  StrSDoesNotExist = '%s does not exist.';
  StrUnableToReadS = 'Unable to read %s';
  StrCreatedFromTPROGS = 'Created from T-PROGS binary grid file';
  StrImportedFromS = 'Imported from %0:s. '
    + 'Origin (X, Y, Z) = (%1:s, %2:s, %3:s). '
    + 'Delta X = %4:s. Delta Y = %5:s. Delta Z = %6:s. '
    + 'Angle = %7:s.';
  StrImportedFromSElev = 'Imported from %0:s for an elevation of %1:g';
  StrImportTPROGSBinar = 'import T-PROGS binary grid file';
  StrNoBgrFileHasBee = 'No .bgr file has been specified.';
  StrThereWasAnErrorR = 'There was an error reading %s';
  StrYouMustCreateAGr = 'You must create a grid before you can import a TPro' +
  'gs file.';
  StrYouMustCreateAMe = 'You must create a mesh before you can import a TPro' +
  'gs file.';

procedure TfrmImportTprogs.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmImportTprogs.FormCreate(Sender: TObject);
begin
  inherited;
  FDataSets := TList.Create;
  GetData;
end;

procedure TfrmImportTprogs.FormDestroy(Sender: TObject);
begin
  inherited;
  FDataSets.Free
end;

procedure TfrmImportTprogs.GetData;
var
  GridAngle: Double;
  Origin: TPoint2D;
  MeshLimits: TGridLimit;
  MinX: Double;
  MinY: Double;
  MinZ: Double;
  Mesh: TSutraMesh3D;
  procedure ShowGridError;
  begin
    Beep;
    MessageDlg(StrYouMustCreateAGr, mtError, [mbOK], 0);
    ModalResult := mrCancel;
  end;
  procedure ShowMeshError;
  begin
    Beep;
    MessageDlg(StrYouMustCreateAMe, mtError, [mbOK], 0);
    ModalResult := mrCancel;
  end;
begin
  GridAngle := 0;
  MinX := 0;
  MinY := 0;
  MinZ := 0;
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        rgEvaluatedAt.ItemIndex := 1;
        if frmGoPhast.Grid <> nil then
        begin
          if (frmGoPhast.Grid.LayerCount < 1) or
            (frmGoPhast.Grid.RowCount < 1) or
            (frmGoPhast.Grid.ColumnCount < 1) then
          begin
            ShowGridError;
            Exit;
          end;
          GridAngle := frmGoPhast.Grid.GridAngle;
          Origin := frmGoPhast.Grid.TwoDElementCorner(0, 0);
          MinX := Origin.X;
          MinY := Origin.Y;
          MinZ := frmGoPhast.Grid.LowestElevation;
        end
        else
        begin
          ShowGridError;
          Exit;
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msModflow2015, msModflowOwhm2:
      begin
        rgEvaluatedAt.Enabled := False;
        if frmGoPhast.Grid <> nil then
        begin
          if (frmGoPhast.Grid.LayerCount < 1) or
            (frmGoPhast.Grid.RowCount < 1) or
            (frmGoPhast.Grid.ColumnCount < 1) then
          begin
            ShowGridError;
            Exit;
          end;
          GridAngle := frmGoPhast.Grid.GridAngle;
          Origin := frmGoPhast.Grid.TwoDElementCenter(0,
            frmGoPhast.Grid.RowCount-1);
          MinX := Origin.X;
          MinY := Origin.Y;
          MinZ := frmGoPhast.Grid.LowestElevation;
        end
        else
        begin
          ShowGridError;
          Exit;
        end
      end;
    msSutra22, msSutra30, msSutra40:
      begin
        rgEvaluatedAt.ItemIndex := 1;
        GridAngle := 0;
        if frmGoPhast.PhastModel.Mesh <> nil then
        begin
          Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
          if Mesh.Mesh2D.Nodes.Count = 0 then
          begin
            ShowMeshError;
            Exit;
          end;
          MeshLimits := Mesh.MeshLimits(vdFront, 0);
          MinX := MeshLimits.MinX;
          MinZ := MeshLimits.MinZ;

          MeshLimits := Mesh.MeshLimits(vdTop, 0);
          MinY := MeshLimits.MinY;
        end
        else
        begin
          ShowMeshError;
          Exit;
        end;
      end;
    msFootPrint:
      begin
        Assert(False);
      end;
  else
    Assert(False);
  end;
  rdeAngle.Text := FloatToStr(GridAngle/pi*180);
  rdeXOrigin.Text := FloatToStr(MinX);
  rdeYOrigin.Text := FloatToStr(MinY);
  rdeZOrigin.Text := FloatToStr(MinZ);

end;

function TfrmImportTprogs.ReadTProgFile: boolean;
var
  BgrFile: TFileStream;
  DimensionCount: integer;
//  index: integer;
  LayerIndex: integer;
  RowIndex: integer;
  ColIndex: integer;
  StartPosition: integer;
  EndOfRecordLength: integer;
  EndOfFileLength: integer;
  XOffSets: TOneDRealArray;
  YOffSets: TOneDRealArray;
  XOrigin: Double;
  YOrigin: Double;
  ZOrigin: Double;
  DeltaX: Double;
  DeltaY: Double;
  DeltaZ: Double;
  Angle: Double;
  CosAngle: Double;
  SinAngle: Double;
  SizeToRead: integer;
  AnInt: Integer;
//  AByte: Byte;
  ByteArray: array of Byte;
  BytePosition: Integer;
begin
  result := False;
  if fedTprogs.FileName = '' then
  begin
    Beep;
    MessageDlg(StrNoBgrFileHasBee, mtError,
      [mbOK], 0);
    Exit;
  end;
  if not FileExists(fedTprogs.FileName) then
  begin
    Beep;
    MessageDlg(Format(StrSDoesNotExist, [fedTprogs.FileName]), mtError,
      [mbOK], 0);
    Exit;
  end;
  BgrFile := nil;
  try
    try
      BgrFile := TFile.OpenRead(fedTprogs.FileName);
    except
      on E: Exception do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
    StartPosition := 0;
    EndOfRecordLength := 0;
    EndOfFileLength := 0;
    case rgFileType.ItemIndex of
      0:
        begin
          StartPosition := 4;
          EndOfRecordLength := 8;
          EndOfFileLength := 4;
        end;
      1:
        begin
          StartPosition := 0;
          EndOfRecordLength := 0;
          EndOfFileLength := 0;
        end;
    end;

    BgrFile.Position := StartPosition;
    BgrFile.Read(DimensionCount, SizeOf(DimensionCount));
    if not(DimensionCount in [2, 3]) then
    begin
      Beep;
      MessageDlg(Format(StrUnableToReadS, [fedTprogs.FileName]), mtError,
        [mbOK], 0);
      Exit;
      // ModalResult := mrNone;
      // Exit;
    end;
    BgrFile.Position := BgrFile.Position + EndOfRecordLength;
    BgrFile.Read(NCOL, SizeOf(NCOL));
    BgrFile.Read(NROW, SizeOf(NROW));
    if DimensionCount = 2 then
    begin
      NLAY := 1;
    end
    else
    begin
      BgrFile.Read(NLAY, SizeOf(NLAY));
    end;

    XOrigin := StrToFloat(rdeXOrigin.Text);
    YOrigin := StrToFloat(rdeYOrigin.Text);
    ZOrigin := StrToFloat(rdeZOrigin.Text);

    DeltaX := StrToFloat(rdeDeltaX.Text);
    DeltaY := StrToFloat(rdeDeltaY.Text);
    DeltaZ := StrToFloat(rdeDeltaZ.Text);

    Angle := StrToFloat(rdeAngle.Text)/180*pi;

    SetLength(XOffSets, NCOL);
    SetLength(YOffSets, NROW);
    for ColIndex := 0 to NCOL - 1 do
    begin
      XOffSets[ColIndex] := DeltaX * ColIndex;
    end;

    for RowIndex := 0 to NROW - 1 do
    begin
      YOffSets[RowIndex] := DeltaY * RowIndex;
    end;

    SetLength(XYArray, NROW, NCOL);
    SetLength(ZArray, NLAY);

    if Angle = 0 then
    begin
      for RowIndex := 0 to NROW - 1 do
      begin
        for ColIndex := 0 to NCOL - 1 do
        begin
          XYArray[RowIndex, ColIndex].x := XOrigin + XOffSets[ColIndex];
          XYArray[RowIndex, ColIndex].y := YOrigin + YOffSets[RowIndex];
        end;
      end;
    end
    else
    begin
      CosAngle := Cos(Angle);
      SinAngle := Sin(Angle);
      for RowIndex := 0 to NROW - 1 do
      begin
        for ColIndex := 0 to NCOL - 1 do
        begin
          XYArray[RowIndex, ColIndex].x := XOrigin + CosAngle * XOffSets[ColIndex]
            - SinAngle * YOffSets[RowIndex];
          XYArray[RowIndex, ColIndex].y := YOrigin + SinAngle * XOffSets[ColIndex]
            + CosAngle * YOffSets[RowIndex];
        end;
      end;
    end;

    SetLength(XOffSets, 0);
    SetLength(YOffSets, 0);

    for LayerIndex := 0 to NLAY - 1 do
    begin
      ZArray[LayerIndex] := ZOrigin + LayerIndex * DeltaZ;
    end;

    BgrFile.Position := BgrFile.Position + EndOfRecordLength;
    case rgDataType.ItemIndex of
      0:
        begin
          if (BgrFile.Size - BgrFile.Position- EndOfFileLength <>
            NLAY * NROW * NCOL * SizeOf(single) ) then
          begin
            Beep;
            MessageDlg(Format(StrThereWasAnErrorR,
              [fedTprogs.FileName]), mtError, [mbOK], 0);
            Exit;
          end;
          SetLength(RealData, NLAY, NROW, NCOL);
          for LayerIndex := 0 to NLAY - 1 do
          begin
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                BgrFile.Read(RealData[LayerIndex, RowIndex, ColIndex],
                  SizeOf(single));
              end;
            end;
          end;
        end;
      1:
        begin
          if (BgrFile.Size - BgrFile.Position - EndOfFileLength =
            NLAY * NROW * NCOL * SizeOf(Integer) ) then
          begin
            SizeToRead := SizeOf(Integer);
          end
          else if (BgrFile.Size - BgrFile.Position - EndOfFileLength =
            NLAY * NROW * NCOL * SizeOf(byte) ) then
          begin
            SizeToRead := SizeOf(byte);
            SetLength(ByteArray, NLAY * NROW * NCOL);
            BgrFile.Read(ByteArray[0], SizeOf(SizeToRead)*NLAY * NROW * NCOL);
          end
          else
          begin
            Beep;
            MessageDlg(Format(StrThereWasAnErrorR,
              [fedTprogs.FileName]), mtError, [mbOK], 0);
            Exit;
          end;
//          if (BgrFile.Size - BgrFile.Position - EndOfFileLength <>
//            NLAY * NROW * NCOL * SizeOf(byte) ) then
//          begin
//            Beep;
//            MessageDlg(Format(StrThereWasAnErrorR,
//              [fedTprogs.FileName]), mtError, [mbOK], 0);
//            Exit;
//          end;
          SetLength(IntegerData, NLAY, NROW, NCOL);

          BytePosition := 0;
          for LayerIndex := 0 to NLAY - 1 do
          begin
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                if SizeToRead = SizeOf(Integer) then
                begin
                  BgrFile.Read(AnInt, SizeOf(SizeToRead));
                end
                else
                begin
                  AnInt := ByteArray[BytePosition];
                  Inc(BytePosition);
                end;
                IntegerData[LayerIndex, RowIndex, ColIndex] := AnInt;
              end;
            end;
          end;
        end;
    end;
  finally
    BgrFile.Free
  end;
  result := True;
end;

procedure TfrmImportTprogs.MakeNewDataSets(NewDataSets: TList);
var
  MaxLayerStringLength: integer;
  DataSetRoot: string;
  LayerIndex: Integer;
  Suffix: string;
  NewFormula: string;
  NewDataType: TRbwDataType;
  Interpolator: TNearestPoint2DInterpolator;
  NewDataSetName: string;
  DataSet: TDataArray;
  FormulaBuilder: TStringBuilder;
  OldDecimalSeparator: Char;
  ADataArray: TDataArray;
begin
  NewDataType := TRbwDataType(rgDataType.ItemIndex);
  case NewDataType of
    rdtDouble: NewFormula := '0.';
    rdtInteger: NewFormula := '0';
    else Assert(False);
  end;
  MaxLayerStringLength := Length(IntToStr(NLAY));
  DataSetRoot := GenerateNewName
    (ChangeFileExt(ExtractFileName(fedTprogs.FileName), ''), nil, '_');

  try
    for LayerIndex := 1 to NLAY do
    begin
      Suffix := IntToStr(LayerIndex);
      if Length(Suffix) < MaxLayerStringLength then
      begin
        Suffix := StringOfChar('0', MaxLayerStringLength-Length(Suffix)) + Suffix;
      end;
      NewDataSetName := GenerateNewName(DataSetRoot+'_'+Suffix, nil, '_');

      DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(
        TDataArray, NewDataSetName, NewFormula, NewDataSetName, [], NewDataType,
        TEvaluatedAt(rgEvaluatedAt.ItemIndex), dsoTop,
        strDefaultClassification + '|' + StrCreatedFromTPROGS);
      FDataSets.Add(DataSet);

      NewDataSets.Add(DataSet);

      DataSet.Units := '';

      Interpolator := TNearestPoint2DInterpolator.Create(nil);
      try
        DataSet.TwoDInterpolator := Interpolator;
      finally
        Interpolator.Free;
      end;

      frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);

      DataSet.Comment := Format(StrImportedFromSElev,
        [fedTprogs.FileName, ZArray[LayerIndex-1]]);
    end;

    NewDataSetName3D := GenerateNewName(DataSetRoot, nil, '_');


    OldDecimalSeparator := FormatSettings.DecimalSeparator;
    try
      FormatSettings.DecimalSeparator := '.';
      FormulaBuilder := TStringBuilder.Create;
      try
        case rgEvaluatedAt.ItemIndex of
          0:
            begin
              FormulaBuilder.Append('Case(Closest(LayerCenter, ');
            end;
          1:
            begin
              FormulaBuilder.Append('Case(Closest(LayerBoundaryPosition, ');
            end;
        end;

        for LayerIndex := 0 to Length(ZArray) - 1 do
        begin
          FormulaBuilder.Append(ZArray[LayerIndex]);
          if LayerIndex < Length(ZArray) - 1 then
          begin
            FormulaBuilder.Append(', ');
          end;
        end;
        FormulaBuilder.Append('), ');
        for LayerIndex := 0 to FDataSets.Count - 1 do
        begin
          ADataArray := FDataSets[LayerIndex];
          FormulaBuilder.Append(ADataArray.Name);
          if LayerIndex < FDataSets.Count - 1 then
          begin
            FormulaBuilder.Append(', ');
          end;
        end;
        FormulaBuilder.Append(')');
        NewFormula := FormulaBuilder.ToString;
      finally
        FormulaBuilder.Free;
      end;
    finally
      FormatSettings.DecimalSeparator := OldDecimalSeparator;
    end;

    DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(
      TDataArray, NewDataSetName3D, NewFormula, NewDataSetName3D, [], NewDataType,
      TEvaluatedAt(rgEvaluatedAt.ItemIndex), dso3D,
      strDefaultClassification + '|' + StrCreatedFromTPROGS);

    NewDataSets.Add(DataSet);
    frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);
    DataSet.Comment := Format(StrImportedFromS, [fedTprogs.FileName,
      rdeXOrigin.Text, rdeYOrigin.Text, rdeZOrigin.Text,
      rdeDeltaX.Text, rdeDeltaY.Text, rdeDeltaZ.Text, rdeAngle.Text]);

  finally
  end;

end;

procedure TfrmImportTprogs.MakeNewScreenObject(ScreenObjectList: TList);
var
  UndoCreateScreenObject: TCustomUndo;
  Root: string;
  ExistingObjectCount: Integer;
  Item: TValueArrayItem;
  NewDataType: TRbwDataType;
  ValuesCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  AScreenObject: TScreenObject;
  LayerIndex: Integer;
  ItemIndex: Integer;
  ADataArray: TDataArray;
  Position: integer;
  Formula: string;
begin
  NewDataType := TRbwDataType(rgDataType.ItemIndex);
  UndoCreateScreenObject := nil;
  AScreenObject := TScreenObject.CreateWithViewDirection(
    frmGoPhast.PhastModel, vdTop, UndoCreateScreenObject, False);
  AScreenObject.Comment := 'Imported from ' + fedTprogs.FileName +' on ' + DateTimeToStr(Now);
  ScreenObjectList.Add(AScreenObject);
  AScreenObject.ElevationCount := ecZero;
  Root := TScreenObject.ValidName(
    ExtractFileRoot(fedTprogs.FileName)+ '_');
  ExistingObjectCount := frmGoPhast.PhastModel.
    NumberOfLargestScreenObjectsStartingWith(Root);
  AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
  AScreenObject.SetValuesByInterpolation := True;
  AScreenObject.EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  AScreenObject.Visible := cbVisible.Checked;

  AScreenObject.Capacity := NROW*NCOL;
//  AScreenObject.ch
  for RowIndex := 0 to NROW - 1 do
  begin
    for ColIndex := 0 to NCOL - 1 do
    begin
      AScreenObject.AddPoint(XYArray[RowIndex,ColIndex],True);
    end;
  end;

  ValuesCount := NROW*NCOL;
  Assert(FDataSets.Count = NLAY);
  for LayerIndex := 0 to FDataSets.Count - 1 do
  begin
    Item := AScreenObject.ImportedValues.Add as TValueArrayItem;
    ADataArray := FDataSets[LayerIndex];
    Item.Name := ADataArray.Name;
    Item.Values.DataType := NewDataType;
    Item.Values.Count := ValuesCount;
    ItemIndex := 0;
    for RowIndex := 0 to NROW - 1 do
    begin
      for ColIndex := 0 to NCOL - 1 do
      begin
        case NewDataType of
          rdtDouble:
            begin
              Item.Values.RealValues[ItemIndex] :=
                RealData[LayerIndex,RowIndex, ColIndex];
            end;
          rdtInteger:
            begin
              Item.Values.IntValues[ItemIndex] :=
                IntegerData[LayerIndex,RowIndex, ColIndex];
            end;
          else Assert(False);
        end;
        Inc(ItemIndex);
      end;
    end;
    Position := AScreenObject.AddDataSet(ADataArray);
    case ADataArray.DataType of
      rdtDouble: Formula := rsObjectImportedValuesR
        + '("' + ADataArray.Name + '")';
      rdtInteger: Formula := rsObjectImportedValuesI
        + '("' + ADataArray.Name + '")';
      else Assert(False);
    end;
    AScreenObject.DataSetFormulas[Position] := Formula;

  end;
end;

procedure TfrmImportTprogs.SetData;
var
  DataSetList: TList;
  Undo: TUndoImportTprogs;
  ScreenObjectList: TList;
begin
  if ReadTProgFile then
  begin
    DataSetList := TList.Create;
    ScreenObjectList := TList.Create;
    try
      Undo:= TUndoImportTprogs.Create;
      try
        MakeNewDataSets(DataSetList);
        MakeNewScreenObject(ScreenObjectList);
        Undo.StoreNewDataSets(DataSetList);
        Undo.StoreNewScreenObjects(ScreenObjectList);
        frmGoPhast.UndoStack.Submit(Undo);
        frmGoPhast.PhastModel.AddFileToArchive(fedTprogs.FileName);
      except on E: Exception do
        begin
          Undo.Free;
        end;
      end;



    finally
      DataSetList.Free;
      ScreenObjectList.Free;
    end;


  end
  else
  begin
    ModalResult := mrNone;
  end;

end;

{ TUndoImportTprogs }

function TUndoImportTprogs.Description: string;
begin
  result := StrImportTPROGSBinar;
end;

end.
