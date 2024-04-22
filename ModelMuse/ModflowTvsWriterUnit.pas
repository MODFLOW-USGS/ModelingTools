unit ModflowTvsWriterUnit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, CustomModflowWriterUnit,
  ModflowBoundaryDisplayUnit, ModflowPackageSelectionUnit, Vcl.Dialogs,
  System.UITypes;

type
  TModflowTvs_Writer = class(TCustomTransientWriter)
  private
    procedure WriteOptions;
    procedure WriteStressPeriods;
    procedure WriteFileInternal;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    procedure Evaluate; override;
  public
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    function WriteFile(const AFileName: string): string;
  end;

implementation

uses
  frmProgressUnit, ScreenObjectUnit, ModflowTvsUnit, CellLocationUnit,
  ModflowCellUnit, GoPhastTypes, frmErrorsAndWarningsUnit, RbwParser,
  DataSetUnit, DataSetNamesUnit;

resourcestring
  StrEvaluatingTVSPacka = 'Evaluating TVS Package data.';
  StrTVSPackageSkipped = 'TVS package skipped.';
  StrTheTimeVaryingSto = 'The Time-Varying Storage package wa' +
  's not included in the model because no time-varying storage' +
  ' values have been defined.';

{ TModflowTvs_Writer }

procedure TModflowTvs_Writer.Evaluate;
var
  StartTime: Double;
  EndTime: Double;
  Dummy: TStringList;
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Boundary: TTvsBoundary;
begin
  inherited;
  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  EndTime := Model.ModflowFullStressPeriods.Last.Endtime;
  Dummy := TStringList.Create;
  try
    for ObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      AScreenObject := Model.ScreenObjects[ObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if not AScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := AScreenObject.ModflowTvsBoundary;
      if (Boundary = nil) or not Boundary.Used then
      begin
        Continue;
      end;

      frmProgressMM.AddMessage(Format(StrEvaluatingS, [AScreenObject.Name]));
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Boundary.GetCellValues(Values, Dummy, Model, self);
      FTimeSeriesNames.AddStrings(Boundary.Mf6TimeSeriesNames);
    end;

  finally
    Dummy.Free;
  end;

end;

class function TModflowTvs_Writer.Extension: string;
begin
  result := '.tvs';
end;

function TModflowTvs_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.TvsPackage;
end;

procedure TModflowTvs_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  SSLists: TModflowBoundListOfTimeLists;
  SYLists: TModflowBoundListOfTimeLists;
  SSList: TModflowBoundaryDisplayTimeList;
  SYList: TModflowBoundaryDisplayTimeList;
  TimeIndex: Integer;
  SSArray: TModflowBoundaryDisplayDataArray;
  SYArray: TModflowBoundaryDisplayDataArray;
  CellList: TValueCellList;
  TempList: TList;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  Evaluate;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  if Values.Count = 0 then
  begin
    SetTimeListsUpToDate(TimeLists);
    Exit;
  end;

  SSLists := TModflowBoundListOfTimeLists.Create;
  SYLists := TModflowBoundListOfTimeLists.Create;
  TempList := TList.Create;
  try
    try
      SSList := TimeLists[0];
      SYList := TimeLists[1];
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        TempList.Clear;
        SSArray := SSList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        TempList.Add(SSArray);
        SSArray.AddMethod := vamReplace;
        if SYList <> nil then
        begin
          SYArray := SYList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          TempList.Add(SYArray);
          SYArray.AddMethod := vamReplace;
        end
        else
        begin
          SYArray := nil;
        end;
        CellList := Values[TimeIndex];
        try
          UpdateCellDisplay(CellList, TempList, []);
        finally
          CellList.Cache;
          SSArray.UpToDate := True;
          if SYArray <> nil then
          begin
            SYArray.UpToDate;
          end;
        end;
      end;
    except on E: EInvalidTime do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  finally
    SSLists.Free;
    SYLists.Free;
    TempList.Free;
    Model.InvalidateAllDynamicLists;
  end
end;

function TModflowTvs_Writer.WriteFile(const AFileName: string): string;
begin
  result := '';
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;

  frmProgressMM.AddMessage(StrEvaluatingTVSPacka);
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  Evaluate;

  if Values.Count = 0 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrTVSPackageSkipped,
      StrTheTimeVaryingSto);
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  result := FNameOfFile;
  Model.AddModelInputFile(FNameOfFile);

  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FInputFileName := FNameOfFile;
  WriteFileInternal;

  if Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;

end;

procedure TModflowTvs_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteTemplateHeader;
    WriteDataSet0;
    WriteOptions;
    WriteStressPeriods;
  finally
    CloseFile;
  end;
end;

procedure TModflowTvs_Writer.WriteOptions;
begin
  WriteBeginOptions;
  if not (Package as TTvsPackage).Enable_Storage_Change_Integration then
  begin
    WriteString('  DISABLE_STORAGE_CHANGE_INTEGRATION');
    NewLine;
  end;
  PrintListInputOption;
  WriteTimeSeriesFiles(FInputFileName);
  WriteEndOptions
end;

procedure TModflowTvs_Writer.WriteStressPeriods;
var
  StressPeriodIndex: Integer;
  Cells: TValueCellList;
  CellIndex: Integer;
  ACell: TTvs_Cell;
  AScreenObject: TScreenObject;
  ConvertibleDataSet: TDataArray;
  procedure WriteCellID;
  begin
    WriteInteger(ACell.CellLocation.Layer+1);
    if not Model.DisvUsed then
    begin
      WriteInteger(ACell.CellLocation.Row+1);
    end;
    WriteInteger(ACell.CellLocation.Column+1);
  end;
begin
  ConvertibleDataSet := Model.DataArrayManager.GetDataSetByName(KConvertible);
  Assert(ConvertibleDataSet <> nil);
  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count -1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage(Format(
      '    Writing TVK Stress Period %d', [StressPeriodIndex+1]));

    WriteBeginPeriod(StressPeriodIndex);

    Cells := Values[StressPeriodIndex];

    for CellIndex := 0 to Cells.Count - 1 do
    begin
      ACell := Cells[CellIndex] as TTvs_Cell;

      if ACell.Used[SSPosition] then
      begin
        WriteCellID;
        WriteString(' SS');
        WriteValueOrFormula(ACell, SSPosition);
        NewLine;
      end;

      if ConvertibleDataSet.BooleanData[ACell.Layer, ACell.Row, ACell.Column]
        and ACell.Used[SYPosition] then
      begin
        WriteCellID;
        WriteString(' SY');
        WriteValueOrFormula(ACell, SYPosition);
        NewLine;
      end;

    end;

    WriteEndPeriod;
  end;
end;

end.
