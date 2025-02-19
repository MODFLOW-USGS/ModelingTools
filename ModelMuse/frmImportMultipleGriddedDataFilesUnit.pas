unit frmImportMultipleGriddedDataFilesUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls, frameGridUnit, frmImportShapefileUnit;

type
  TUndoImportMultipleGriddedData = class(TUndoImportShapefile)
  protected
    function Description: string; override;
  end;

  TfrmImportMultipleGriddedDataFiles = class(TfrmCustomGoPhast)
    frameGridFiles: TframeGrid;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dlgOpenFiles: TOpenDialog;
    btnOpenFiles: TButton;
    lblModel: TLabel;
    comboModel: TComboBox;
    rgEvaluatedAt: TRadioGroup;
    procedure btnOpenFilesClick(Sender: TObject);
    procedure frameGridFilesGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);

    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure frameGridFilesGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
  private
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportMultipleGriddedDataFiles: TfrmImportMultipleGriddedDataFiles;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, DataSetUnit, GoPhastTypes, RbwParser,
  ScreenObjectUnit, AbstractGridUnit, UndoItems, FastGEO,
  ValueArrayStorageUnit, GIS_Functions, MeshRenumberingTypes, DataSetNamesUnit;

resourcestring
  StrThereWasAnErrorR = 'There was an error reading %s. Please check that th' +
  'e file contains properly formatted data.';
  StrCreatedFromTextFi = 'Created from text file';
  StrErrorDoesNotExist = 'There was an error reading %s. Please check that t' +
  'he file exists and that you are allowed to open it.';
  StrImportedFrom0sO = 'Imported from %0:s on %1:s.';

{$R *.dfm}

procedure TfrmImportMultipleGriddedDataFiles.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmImportMultipleGriddedDataFiles.btnOpenFilesClick(Sender: TObject);
begin
  inherited;
  dlgOpenFiles.FileName := '';
  dlgOpenFiles.Options := dlgOpenFiles.Options + [ofAllowMultiSelect];
  if dlgOpenFiles.Execute then
  begin
    if dlgOpenFiles.Files.Count > 1 then
    begin
      frameGridFiles.Grid.DistributeText(0, 1, dlgOpenFiles.Files.Text);
    end
    else
    begin
      frameGridFiles.Grid.Cells[0, 1] := dlgOpenFiles.FileName;
    end;

  end;
end;

procedure TfrmImportMultipleGriddedDataFiles.FormCreate(Sender: TObject);
begin
  inherited;
  FillComboBoxWithModels(comboModel);
  rgEvaluatedAt.Enabled := frmGoPhast.ModelSelection
    in [msPhast, msSutra22, msSutra30, msSutra40];
  frameGridFiles.Grid.Cells[0,0] := 'Files';
end;

procedure TfrmImportMultipleGriddedDataFiles.frameGridFilesGridButtonClick(
  Sender: TObject; ACol, ARow: Integer);
var
  FileIndex: Integer;
begin
  inherited;
  dlgOpenFiles.FileName := frameGridFiles.Grid.Cells[ACol, ARow];
//  dlgOpenFiles.Options := dlgOpenFiles.Options - ofAllowMultiSelect];
  if dlgOpenFiles.Execute then
  begin
    if dlgOpenFiles.Files.Count > 0 then
    begin
      frameGridFiles.seNumber.AsInteger := ARow + dlgOpenFiles.Files.Count -1;
      for FileIndex := 0 to dlgOpenFiles.Files.Count - 1 do
      begin
        frameGridFiles.Grid.Cells[ACol, ARow + FileIndex] := dlgOpenFiles.Files[FileIndex];
      end;
    end;
  end;
end;

procedure TfrmImportMultipleGriddedDataFiles.frameGridFilesGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameGridFiles.seNumber.asInteger := frameGridFiles.Grid.RowCount -1;
end;

procedure TfrmImportMultipleGriddedDataFiles.SetData;
var
  NewDataSets: TList;
  Undo: TUndoImportMultipleGriddedData;
  NewScreenObjectList: TList;
  FileIndex: Integer;
  FileNames: TStringList;
  NewName: string;
  DataSet: TDataArray;
  UndoCreateScreenObject: TCustomUndo;
  AScreenObject: TScreenObject;
  Grid: TCustomModelGrid;
  RowIndex: Integer;
  ColIndex: Integer;
  APoint: TPoint2D;
  Values: array of double;
  AFile: TextFile;
  ValueIndex: Integer;
  ImportedItem: TValueArrayItem;
  FormulaIndex: Integer;
  AModel: TCustomModel;
  Mesh: IMesh2D;
  NewCapacity: Integer;
  ElementIndex: Integer;
begin
  AModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;

  Mesh := nil;
  Grid := AModel.Grid;
  if Grid = nil then
  begin
    Mesh := AModel.Mesh3D.Mesh2DI;
  end;

  NewDataSets := TList.Create;
  NewScreenObjectList := TList.Create;
  FileNames := TStringList.Create;
  try
    for FileIndex := 1 to frameGridFiles.seNumber.AsInteger do
    begin
      if FileExists(frameGridFiles.Grid.Cells[0,FileIndex]) then
      begin
        FileNames.Add(frameGridFiles.Grid.Cells[0,FileIndex]);
      end;
    end;

    AScreenObject := TScreenObject.CreateWithViewDirection(
      frmGoPhast.PhastModel, vdTop,
      UndoCreateScreenObject, False);
    AScreenObject.Comment := 'Imported from the following files on '
      + DateTimeToStr(Now) + sLineBreak + FileNames.Text;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.Visible := false;
    AScreenObject.ElevationCount := ecZero;
    AScreenObject.EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);

    if comboModel.ItemIndex > 0 then
    begin
      AScreenObject.UsedModels.UsedWithAllModels := False;
      AScreenObject.UsedModels.AddModel(AModel);
    end;

    NewScreenObjectList.Add(AScreenObject);

    if Grid <> nil then
    begin
      if rgEvaluatedAt.ItemIndex = 0 then
      begin
        NewCapacity := Grid.RowCount * Grid.ColumnCount;
        AScreenObject.Capacity := NewCapacity;
        AScreenObject.BeginUpdate;
        try
          for RowIndex := 0 to Grid.RowCount - 1 do
          begin
            for ColIndex := 0 to Grid.ColumnCount - 1 do
            begin
              APoint := Grid.TwoDElementCenter(ColIndex,RowIndex);
              AScreenObject.AddPoint(APoint, True);
            end;
          end;
        finally
          AScreenObject.EndUpdate;
        end;
      end
      else
      begin
        NewCapacity := (Grid.RowCount+1) * (Grid.ColumnCount+1);
        AScreenObject.Capacity := NewCapacity;
        AScreenObject.BeginUpdate;
        try
          for RowIndex := 0 to Grid.RowCount do
          begin
            for ColIndex := 0 to Grid.ColumnCount do
            begin
              APoint := Grid.TwoDElementCorner(ColIndex,RowIndex);
              AScreenObject.AddPoint(APoint, True);
            end;
          end;
        finally
          AScreenObject.EndUpdate;
        end;
      end;
    end
    else
    begin
      if rgEvaluatedAt.ItemIndex = 0 then
      begin
        NewCapacity := Mesh.ElementCount;
        AScreenObject.Capacity := NewCapacity;
        AScreenObject.BeginUpdate;
        try
          for ElementIndex := 0 to Mesh.ElementCount - 1 do
          begin
            APoint := Mesh.ElementsI2D[ElementIndex].Center;
            AScreenObject.AddPoint(APoint, True);
          end;
        finally
          AScreenObject.EndUpdate;
        end;
      end
      else
      begin
        NewCapacity := Mesh.NodeCount;
        AScreenObject.Capacity := NewCapacity;
        AScreenObject.BeginUpdate;
        try
          for ElementIndex := 0 to Mesh.NodeCount - 1 do
          begin
            APoint := Mesh.NodesI2D[ElementIndex].Location;
            AScreenObject.AddPoint(APoint, True);
          end;
        finally
          AScreenObject.EndUpdate;
        end;
      end;
    end;


    SetLength(Values, AScreenObject.Count);
    for FileIndex := 0 to FileNames.Count - 1 do
    begin
      NewName := FileNames[FileIndex];

      try
        AssignFile(AFile, NewName);
        try
          Reset(AFile);
//          ValueIndex := 0;
          for ValueIndex := 0 to AScreenObject.Count - 1 do
          begin
//            for ColIndex := 0 to Grid.ColumnCount - 1 do
//            begin
              try
                read(AFile, Values[ValueIndex]);
              except on EInOutError do
                begin
                  Beep;
                  MessageDlg(Format(StrThereWasAnErrorR, [NewName]), mtError, [mbOK], 0);
                  AScreenObject.Deleted := True;
                  Exit;
                end;
              end;
//              Inc(ValueIndex);
//            end;
          end;
        finally
          CloseFile(AFile);
        end;
      except on E: EInOutError do
        begin
          Beep;
          MessageDlg(Format(StrErrorDoesNotExist, [NewName]), mtError, [mbOK], 0);
          AScreenObject.Deleted := True;
          Exit;
        end;
      end;

      NewName := ChangeFileExt(ExtractFileName(NewName), '');
      NewName := GenerateNewName(NewName);

      DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(
        TDataArray, NewName, '0', NewName, [], rdtDouble,
        AScreenObject.EvaluatedAt, dsoTop,
        strDefaultClassification + '|' + StrCreatedFromTextFi);
      DataSet.Comment := Format(StrImportedFrom0sO,
        [FileNames[FileIndex], DateTimeToStr(Now)]);

      NewDataSets.Add(DataSet);

      DataSet.Units := '';
      frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);
      if comboModel.ItemIndex > 0 then
      begin
        DataSet := AModel.DataArrayManager.GetDataSetByName(DataSet.Name);
        AModel.UpdateDataArrayDimensions(DataSet);
      end;

      ImportedItem := AScreenObject.ImportedValues.Add;
      ImportedItem.Name := NewName;
      ImportedItem.Values.DataType := rdtDouble;
      ImportedItem.Values.Count := AScreenObject.Count;
      for ValueIndex := 0 to Length(Values) - 1 do
      begin
        ImportedItem.Values.RealValues[ValueIndex] := Values[ValueIndex];
      end;


      FormulaIndex := AScreenObject.AddDataSet(DataSet);
      AScreenObject.DataSetFormulas[FormulaIndex] :=
        rsObjectImportedValuesR + '("' + DataSet.Name + '")';
    end;

    Undo := TUndoImportMultipleGriddedData.Create;

    Undo.StoreNewScreenObjects(NewScreenObjectList);
    Undo.StoreNewDataSets(NewDataSets);


    frmGoPhast.UndoStack.Submit(Undo)
  finally
    NewDataSets.Free;
    NewScreenObjectList.Free;
    FileNames.Free;
  end;
end;

{ TUndoImportMultipleGriddedData }

function TUndoImportMultipleGriddedData.Description: string;
begin
  result := 'import multiple gridded data files';
end;

end.
