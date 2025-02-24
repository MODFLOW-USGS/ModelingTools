unit frmImportFootprintResultsUnit;

interface

uses System.UITypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.CheckLst, JvDialogs, FootPrintUtilities,
  DataSetUnit, ScreenObjectUnit;

type
  TFootprintDataTypes = (fpdtWithdrawals, fpdtCode);

  TfrmImportFootprintResults = class(TfrmCustomGoPhast)
    chklstDataToImport: TCheckListBox;
    lblColorMesh: TLabel;
    rgDisplayChoice: TRadioGroup;
    comboColorContourGrid: TComboBox;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblDataToImport: TLabel;
    dlgOpenFootprintFile: TJvOpenDialog;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure chklstDataToImportClickCheck(Sender: TObject);
  private
    FWithdrawal: TTwoDRealArray;
    FCode: TTwoDIntArray;
    FWithdrawalDataSet: TDataArray;
    FCodeDataSet: TDataArray;
    FScreenObject: TScreenObject;
//    FAlreadyAsked: Boolean;
//    FCreateNewDataSet: Boolean;
    procedure GetData;
    procedure SetData;
    function ReadResultFile: boolean;
    procedure CreateWithdrawalDataSet(NewDataSets: TList);
    procedure CreateCodeDataSet(NewDataSets: TList);
    procedure CreateObject;
    procedure AssignWithdrawalValues;
    procedure AssignCodeValues;
    procedure UpdateColorContourList;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportFootprintResults: TfrmImportFootprintResults;

implementation

uses
  frmGoPhastUnit, FootprintFileUnit, PhastModelUnit, RbwParser,
  GoPhastTypes, System.IOUtils, UndoItems, AbstractGridUnit,
  ValueArrayStorageUnit, GIS_Functions, frmImportShapefileUnit,
  FootprintGridUnit, frmDisplayDataUnit, frmGridValueUnit, Undo,
  FootprintPropertiesUnit, DataSetNamesUnit;

{$R *.dfm}

const
  KFootprint_Code = 'Footprint_Code';
  KDistributedWithdrawals = 'Distributed_Withdrawals';

resourcestring
  StrDistributedWithdrawals = 'Distributed_Withdrawals';
  StrFootprint_Code = 'Footprint_Code';
  StrReadFrom0sOn = 'read from: "%0:s" on %1:s'
    + sLineBreak + 'File last modified on: %2:s';
  StrTheNumberOfRowsO = 'The number of rows or columns in the results file d' +
  'oes not match the number in the model grid.';
  StrImportFootprintRes = 'Import WellFootprint results';
  StrModelResultsFootpr = 'Model Results|WellFootprint';

type
  TUndoImportFootprintResults = class(TUndoImportShapefile)
  strict private
    FDisplayDataSet: TDataArray;
    FDisplayChoice: TDisplayChoice;
    FOldTimeList: TCustomTimeList;
    FOldTopDataSet: TDataArray;
    FOldTopContourDataSet: TDataArray;
  private
    procedure SetDisplayChoice(const Value: TDisplayChoice);
    procedure SetDisplayDataSet(const Value: TDataArray);
  protected
    function Description: string; override;
  public
    constructor Create;
    property DisplayDataSet: TDataArray read FDisplayDataSet write SetDisplayDataSet;
    property DisplayChoice: TDisplayChoice read FDisplayChoice write SetDisplayChoice;
    procedure DoCommand; override;
    procedure Undo; override;
  end;


procedure TfrmImportFootprintResults.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmImportFootprintResults.chklstDataToImportClickCheck(
  Sender: TObject);
begin
  inherited;
  UpdateColorContourList;
end;

procedure TfrmImportFootprintResults.FormCreate(Sender: TObject);
begin
  inherited;
//  FColorContourList := TColorContourList.Create;
  chklstDataToImport.CheckAll(cbChecked, False);
  UpdateColorContourList;

  GetData;
end;

procedure TfrmImportFootprintResults.GetData;
var
  FileName: string;
  FootprintProperties: TFootprintProperties;
begin
   FileName := frmGoPhast.GetFootprintInputFileName;

   FootprintProperties := frmGoPhast.PhastModel.FootprintProperties;
   if FootprintProperties.SaveResultsBinary then
   begin
     FileName := ChangeFileExt(FileName, '.fpb')
   end
   else if FootprintProperties.SaveResultsText then
   begin
     FileName := ChangeFileExt(FileName, '.fpt')
   end
   else
   begin
     FileName := '';
   end;

   dlgOpenFootprintFile.FileName := FileName;
  if not dlgOpenFootprintFile.Execute then
  begin
    ModalResult := mrCancel;
    Exit;
  end;

  if comboColorContourGrid.Items.Count > 2 then
  begin
    comboColorContourGrid.ItemIndex := 2;
  end;
end;

procedure TfrmImportFootprintResults.SetData;
var
  NewDataSets: TList;
  NewScreenObjects: TList;
  Undo: TUndoImportFootprintResults;
  DisplayChoice: TDisplayChoice;
  DataArray: TDataArray;
  DataSetName: string;
  ItemIndex: Integer;
//  CreateNewDataSet: Boolean;
begin
  if (not chklstDataToImport.Checked[Ord(fpdtWithdrawals)])
    and (not chklstDataToImport.Checked[Ord(fpdtCode)]) then
  begin
    Exit;
  end;

  try
    if ReadResultFile then
    begin
      NewDataSets := TList.Create;
      try
  //      FAlreadyAsked := False;
//        CreateNewDataSet := True;
        CreateWithdrawalDataSet(NewDataSets);
        CreateCodeDataSet(NewDataSets);

        NewScreenObjects := TList.Create;
        try
          CreateObject;
          NewScreenObjects.Add(FScreenObject);


          if comboColorContourGrid.ItemIndex >= 0 then
          begin
            DataSetName := comboColorContourGrid.Items[
              comboColorContourGrid.ItemIndex];
            ItemIndex := chklstDataToImport.Items.IndexOf(DataSetName);
            if ItemIndex >= 0 then
            begin
              case ItemIndex of
                0:
                  begin
                    DataArray := FWithdrawalDataSet;
                  end;
                1:
                  begin
                    DataArray := FCodeDataSet;
                  end;
                else
                  begin
                    DataArray := nil;
                    Assert(False);
                  end;
              end;
            end
            else
            begin
              DataArray := nil;
            end;
          end
          else
          begin
            DataArray := nil;
          end;

          AssignWithdrawalValues;
          AssignCodeValues;
          DisplayChoice := TDisplayChoice(rgDisplayChoice.ItemIndex);
          Undo := TUndoImportFootprintResults.Create;
          try
            Undo.StoreNewScreenObjects(NewScreenObjects);
            Undo.StoreNewDataSets(NewDataSets);
            Undo.DisplayChoice := DisplayChoice;
            Undo.DisplayDataSet := DataArray;
            frmGoPhast.UndoStack.Submit(Undo)
          except
            Undo.Free;
            raise
          end;

        finally
          NewScreenObjects.Free;
        end;

      finally
        NewDataSets.Free;
      end;
    end;
  except
    on E: EAbortingImport do
    begin
      Beep;
      MessageDlg(E.message, mtInformation, [mbOK], 0);
    end;
  end;
end;

procedure TfrmImportFootprintResults.UpdateColorContourList;
var
  Selected: string;
  SelectedPostion: Integer;
  index: Integer;
begin
  Selected := comboColorContourGrid.Text;
  comboColorContourGrid.Items.Clear;
  comboColorContourGrid.Items.Capacity := chklstDataToImport.Items.Count + 1;
  comboColorContourGrid.Items.Add(StrNone);
  for index := 0 to chklstDataToImport.Items.Count - 1 do
  begin
    if chklstDataToImport.Checked[index] then
    begin
      comboColorContourGrid.Items.Add(chklstDataToImport.Items[index]);
    end;
  end;
  SelectedPostion := comboColorContourGrid.Items.IndexOf(Selected);
  if SelectedPostion >= 0 then
  begin
    comboColorContourGrid.ItemIndex := SelectedPostion;
  end
  else
  begin
    comboColorContourGrid.ItemIndex := 0;
  end;
end;

function TfrmImportFootprintResults.ReadResultFile: boolean;
var
  Extension: string;
  FootprintFile: TFootPrintFile;
  TextFile: TStreamReader;
  BinaryFile: TFileStream;
  RowCount: Integer;
  ColumnCount: Integer;
begin
  Result := True;
  FootprintFile := TFootPrintFile.Create;
  try
    RowCount := frmGoPhast.Grid.RowCount;
    ColumnCount := frmGoPhast.Grid.ColumnCount;
    Extension := LowerCase(ExtractFileExt(dlgOpenFootprintFile.FileName));
    if Extension = '.fpb' then
    begin
      if chklstDataToImport.Checked[Ord(fpdtWithdrawals)] then
      begin
        BinaryFile := nil;
        try
          FootprintFile.ReadBinaryRealArray(dlgOpenFootprintFile.FileName,
            BinaryFile, FWithdrawal, StrDistributedWithdraw);
          if (Length(FWithdrawal) <> RowCount)
            or (Length(FWithdrawal[0]) <> ColumnCount) then
          begin
            Result := False;
            MessageDlg(StrTheNumberOfRowsO, mtError, [mbOK], 0);
            Exit;
          end;
        finally
          BinaryFile.Free;
        end;
      end;
      if chklstDataToImport.Checked[Ord(fpdtCode)] then
      begin
        BinaryFile := nil;
        try
          FootprintFile.ReadBinaryIntArray(dlgOpenFootprintFile.FileName,
            BinaryFile, FCode, StrFootprintCode);
          if (Length(FCode) <> RowCount)
            or (Length(FCode[0]) <> ColumnCount) then
          begin
            Result := False;
            MessageDlg(StrTheNumberOfRowsO, mtError, [mbOK], 0);
            Exit;
          end;
        finally
          BinaryFile.Free;
        end;
      end;
    end
    else
    begin
      Assert(Extension = '.fpt');
      TextFile := nil;
      try
        FootprintFile.ReadTextRealArray(dlgOpenFootprintFile.FileName,
          TextFile, FWithdrawal, StrDistributedWithdraw);
        if (Length(FWithdrawal) <> RowCount)
          or (Length(FWithdrawal[0]) <> ColumnCount) then
        begin
          Result := False;
          MessageDlg(StrTheNumberOfRowsO, mtError, [mbOK], 0);
          Exit;
        end;
        if chklstDataToImport.Checked[Ord(fpdtCode)] then
        begin
          FootprintFile.ReadTextIntArray(dlgOpenFootprintFile.FileName,
            TextFile, FCode, StrFootprintCode);
          if (Length(FCode) <> RowCount)
            or (Length(FCode[0]) <> ColumnCount) then
          begin
            Result := False;
            MessageDlg(StrTheNumberOfRowsO, mtError, [mbOK], 0);
            Exit;
          end;
        end;
      finally
        TextFile.Free;
      end;
    end;
  finally
    FootprintFile.Free;
  end;
end;

procedure TfrmImportFootprintResults.CreateWithdrawalDataSet(NewDataSets: TList);
var
  NewName: string;
  Classification: string;
  NewFormula: string;
  NewDataType: TRbwDataType;
  CreateNewDataSet: Boolean;
begin
  CreateNewDataSet := True;
  if chklstDataToImport.Checked[Ord(fpdtWithdrawals)] then
  begin
    NewName := chklstDataToImport.Items[Ord(fpdtWithdrawals)];
    NewName := StringReplace(NewName, ' ', '_', [rfReplaceAll]);
    FWithdrawalDataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(NewName);
    if  FWithdrawalDataSet <> nil then
    begin
      CreateNewDataSet := AskUserIfNewDataSet;
//      AskIfNewDataSet(FAlreadyAsked, CreateNewDataSet);
    end;
    if CreateNewDataSet then
    begin
      NewName := GenerateNewName(NewName);
      NewDataType := rdtDouble;
      NewFormula := '0.';
      Classification := StrModelResultsFootpr;
      FWithdrawalDataSet := frmGoPhast.PhastModel.DataArrayManager.
        CreateNewDataArray(TDataArray, NewName, NewFormula, NewName, [],
        NewDataType, eaBlocks, dsoTop, Classification);
      NewDataSets.Add(FWithdrawalDataSet);
      (FWithdrawalDataSet.Limits.RealValuesToSkip.Add as TSkipReal).RealValue := 0;
    end;
    FWithdrawalDataSet.Comment := Format(StrReadFrom0sOn,
      [dlgOpenFootprintFile.FileName, DateTimeToStr(Now),
      DateTimeToStr(TFile.GetLastWriteTime(dlgOpenFootprintFile.FileName))]);
    frmGoPhast.PhastModel.UpdateDataArrayDimensions(FWithdrawalDataSet);
    FWithdrawalDataSet.Units := '';
  end;
end;

procedure TfrmImportFootprintResults.CreateCodeDataSet(NewDataSets: TList);
var
  NewDataType: TRbwDataType;
  Classification: string;
  NewName: string;
  NewFormula: string;
  CreateNewDataSet: Boolean;
begin
  CreateNewDataSet := False;
  if chklstDataToImport.Checked[Ord(fpdtCode)] then
  begin
    NewName := chklstDataToImport.Items[Ord(fpdtCode)];
    NewName := StringReplace(NewName, ' ', '_', [rfReplaceAll]);
    FCodeDataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(NewName);
    if  FCodeDataSet <> nil then
    begin
      CreateNewDataSet := AskUserIfNewDataSet;
//      AskIfNewDataSet(FAlreadyAsked, CreateNewDataSet);
    end
    else
    begin
      CreateNewDataSet := True;
    end;
    if CreateNewDataSet then
    begin
      NewName := GenerateNewName(NewName);
      NewDataType := rdtInteger;
      NewFormula := '0';
      Classification := StrModelResultsFootpr;
      FCodeDataSet := frmGoPhast.PhastModel.DataArrayManager.
        CreateNewDataArray(TDataArray, NewName, NewFormula, NewName, [],
        NewDataType, eaBlocks, dsoTop, Classification);
      NewDataSets.Add(FCodeDataSet);
      (FCodeDataSet.Limits.IntegerValuesToSkip.Add as TSkipInteger).IntegerValue := 0;
      (FCodeDataSet.Limits.IntegerValuesToSkip.Add as TSkipInteger).IntegerValue := 1;
    end;
    FCodeDataSet.Comment := Format(StrReadFrom0sOn,
      [dlgOpenFootprintFile.FileName, DateTimeToStr(Now),
      DateTimeToStr(TFile.GetLastWriteTime(dlgOpenFootprintFile.FileName))]);
    frmGoPhast.PhastModel.UpdateDataArrayDimensions(FCodeDataSet);
    FCodeDataSet.Units := '';
  end;
end;

procedure TfrmImportFootprintResults.CreateObject;
var
  UndoCreateScreenObject: TCustomUndo;
  ColIndex: Integer;
  Grid: TCustomModelGrid;
  RowIndex: Integer;
begin
  FScreenObject := TScreenObject.CreateWithViewDirection(frmGoPhast.PhastModel, vdTop, UndoCreateScreenObject, False);
  FScreenObject.Comment := 'Imported from ' + dlgOpenFootprintFile.FileName +' on ' + DateTimeToStr(Now);
  FScreenObject.SetPropertiesOfIntersectedCells := True;
  FScreenObject.EvaluatedAt := eaBlocks;
  FScreenObject.Visible := False;
  FScreenObject.ElevationCount := ecZero;
  Grid := frmGoPhast.Grid;
  FScreenObject.Capacity := Grid.RowCount * Grid.ColumnCount;
  for RowIndex := 0 to Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Grid.ColumnCount - 1 do
    begin
      FScreenObject.AddPoint(Grid.TwoDElementCenter(ColIndex, RowIndex), True);
    end;
  end;
end;

procedure TfrmImportFootprintResults.AssignCodeValues;
var
  DataSetPosition: Integer;
  RowIndex: Integer;
  ImportedItem: TValueArrayItem;
  ColIndex: Integer;
  index: Integer;
  Values: TValueArrayStorage;
begin
  if chklstDataToImport.Checked[Ord(fpdtCode)] then
  begin
    ImportedItem := FScreenObject.ImportedValues.Add;
    ImportedItem.Name := FCodeDataSet.Name;
    Values := ImportedItem.Values;
    Values.DataType := rdtInteger;
    Values.Count := Length(FCode) * Length(FCode[0]);
    index := 0;
    //        ImportedItem.
    for RowIndex := 0 to Length(FCode) - 1 do
    begin
      for ColIndex := 0 to Length(FCode[0]) - 1 do
      begin
        Values.IntValues[index] := FCode[RowIndex, ColIndex];
        Inc(index);
      end;
    end;
    DataSetPosition := FScreenObject.AddDataSet(FCodeDataSet);
    FScreenObject.DataSetFormulas[DataSetPosition] :=
      rsObjectImportedValuesI + '("' + FCodeDataSet.Name + '")';
  end;
end;

procedure TfrmImportFootprintResults.AssignWithdrawalValues;
var
  DataSetPosition: Integer;
  RowIndex: Integer;
  ImportedItem: TValueArrayItem;
  ColIndex: Integer;
  index: Integer;
  Values: TValueArrayStorage;
begin
  if chklstDataToImport.Checked[Ord(fpdtWithdrawals)] then
  begin
    ImportedItem := FScreenObject.ImportedValues.Add;
    ImportedItem.Name := FWithdrawalDataSet.Name;
    Values := ImportedItem.Values;
    Values.DataType := rdtDouble;
    Values.Count := Length(FWithdrawal) * Length(FWithdrawal[0]);
    index := 0;
    //        ImportedItem.
    for RowIndex := 0 to Length(FWithdrawal) - 1 do
    begin
      for ColIndex := 0 to Length(FWithdrawal[0]) - 1 do
      begin
        Values.RealValues[index] := FWithdrawal[RowIndex, ColIndex];
        Inc(index);
      end;
    end;
    DataSetPosition := FScreenObject.AddDataSet(FWithdrawalDataSet);
    FScreenObject.DataSetFormulas[DataSetPosition] :=
      rsObjectImportedValuesR + '("' + FWithdrawalDataSet.Name + '")';
  end;
end;

{ TUndoImportFootprintResults }

constructor TUndoImportFootprintResults.Create;
var
//  SutraMesh: TSutraMesh3D;
  PhastModel: TPhastModel;
  Grid: TFootprintGrid;
begin
  inherited;
  PhastModel := frmGoPhast.PhastModel;
  Grid := PhastModel.FootPrintGrid;
  FOldTimeList := PhastModel.ThreeDTimeList;
//  FOld3DDataSet := PhastModel.ThreeDDataSet;
  FOldTopDataSet := PhastModel.TopDataSet;
  FOldTopContourDataSet := Grid.TopContourDataSet;
//  FOld3DContourDataSet := SutraMesh.ThreeDContourDataSet;
end;

function TUndoImportFootprintResults.Description: string;
begin
  result := StrImportFootprintRes;
end;

procedure TUndoImportFootprintResults.DoCommand;
var
  PhastModel: TPhastModel;
  FootPrintGrid: TFootprintGrid;
begin
  inherited;
  if FDisplayDataSet = nil then
  begin
    Exit;
  end;
  case DisplayChoice of
    dcColor:
      begin
        PhastModel := frmGoPhast.PhastModel;
        PhastModel.ThreeDTimeList := nil;
        PhastModel.ThreeDDataSet := FDisplayDataSet;
        PhastModel.TopDataSet := FDisplayDataSet;

        UpdateFrmDisplayData;
        UpdateFrmGridValue;
      end;
    dcContour:
      begin
        PhastModel := frmGoPhast.PhastModel;
        FootPrintGrid := PhastModel.FootPrintGrid;
        FootPrintGrid.TopContourDataSet := FDisplayDataSet;
        FootPrintGrid.ThreeDContourDataSet := FDisplayDataSet;
        UpdateFrmDisplayData;
        UpdateFrmGridValue;
      end;
    dcNone:;// do nothing
    else Assert(False);
  end;
end;

procedure TUndoImportFootprintResults.SetDisplayChoice(
  const Value: TDisplayChoice);
begin
  FDisplayChoice := Value;
end;

procedure TUndoImportFootprintResults.SetDisplayDataSet(
  const Value: TDataArray);
begin
  FDisplayDataSet := Value;
end;

procedure TUndoImportFootprintResults.Undo;
var
  PhastModel: TPhastModel;
  FootprintGrid: TFootprintGrid;
begin
  inherited;
  if (FDisplayDataSet = nil) or (DisplayChoice = dcNone) then
  begin
    Exit;
  end;

  PhastModel := frmGoPhast.PhastModel;
  FootprintGrid := PhastModel.FootprintGrid;
  PhastModel.ThreeDTimeList := FOldTimeList;
//  PhastModel.ThreeDDataSet := FOld3DDataSet;
  PhastModel.TopDataSet := FOldTopDataSet;
  FootprintGrid.TopContourDataSet := FOldTopContourDataSet;
//  FootprintGrid.ThreeDContourDataSet := FOld3DContourDataSet;

  UpdateFrmDisplayData;
  UpdateFrmGridValue;

end;

end.
