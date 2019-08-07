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
    procedure btnOpenFilesClick(Sender: TObject);
    procedure frameGridFilesGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);

    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
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
  ScreenObjectUnit, AbstractGridUnit, UndoItems, FastGEO, IOUtils,
  ValueArrayStorageUnit, GIS_Functions;

resourcestring
  StrThereWasAnErrorR = 'There was an error reading %s. Please check that th' +
  'e file contains properly formatted data.';
  StrCreatedFromTextFi = 'Created from text file';

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
    frameGridFiles.Grid.DistributeText(0, 1, dlgOpenFiles.Files.Text);
  end;
end;

procedure TfrmImportMultipleGriddedDataFiles.FormCreate(Sender: TObject);
begin
  inherited;
  FillComboBoxWithModels(comboModel);
  frameGridFiles.Grid.Cells[0,0] := 'Files';
end;

procedure TfrmImportMultipleGriddedDataFiles.frameGridFilesGridButtonClick(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  dlgOpenFiles.FileName := frameGridFiles.Grid.Cells[ACol, ARow];
  dlgOpenFiles.Options := dlgOpenFiles.Options - [ofAllowMultiSelect];
  if dlgOpenFiles.Execute then
  begin
    frameGridFiles.Grid.Cells[ACol, ARow] := dlgOpenFiles.FileName;
  end;
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
begin
  AModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  Grid := AModel.Grid;
  Assert(Grid <> nil);

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

    if comboModel.ItemIndex > 0 then
    begin
      AScreenObject.UsedModels.UsedWithAllModels := False;
      AScreenObject.UsedModels.AddModel(AModel);
    end;

    NewScreenObjectList.Add(AScreenObject);

    AScreenObject.Capacity := Grid.RowCount * Grid.ColumnCount;
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

    SetLength(Values, AScreenObject.Capacity);
    for FileIndex := 0 to FileNames.Count - 1 do
    begin
      NewName := FileNames[FileIndex];

      AssignFile(AFile, NewName);
      try
        Reset(AFile);
        ValueIndex := 0;
        for RowIndex := 0 to Grid.RowCount - 1 do
        begin
          for ColIndex := 0 to Grid.ColumnCount - 1 do
          begin
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
            Inc(ValueIndex);
          end;
        end;
      finally
        CloseFile(AFile);
      end;

      NewName := ExtractFileName(NewName);
      NewName := GenerateNewName(NewName);

      DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(
        TDataArray, NewName, '0', NewName, [], rdtDouble,
        eaBlocks, dsoTop,
        strDefaultClassification + '|' + StrCreatedFromTextFi);

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
