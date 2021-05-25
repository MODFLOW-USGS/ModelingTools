unit ModflowMnw1Writer;

interface

uses
  System.UITypes, CustomModflowWriterUnit, Classes, PhastModelUnit,
  ModflowPackageSelectionUnit, Forms, ModflowBoundaryDisplayUnit,
  ModflowMnw1Unit, Generics.Collections, Vcl.Dialogs, Winapi.Windows;

type
  TModflowMNW1_Writer = class(TCustomTransientWriter)
  private
//    FNameOfFile: string;
    FMnwPackage: TMnw1Package;
    FWells: TList<TMnw1Boundary>;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSetX;
    procedure WriteDataSet3a;
    procedure WriteDataSet3b;
    procedure WriteDataSet3c;
    procedure WriteStressPeriods;
    procedure WriteFileInternal;
  protected
    procedure Evaluate; override;
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

procedure ClearMnw1FileNames;

implementation

uses
  frmErrorsAndWarningsUnit, frmProgressUnit, ScreenObjectUnit,
  Contnrs, ModflowUnitNumbers, GoPhastTypes, ModflowTimeUnit,
  SysUtils, ModflowCellUnit, DataSetUnit;

resourcestring
  StrWritingMNW1Package = 'Writing MNW1 Package input.';
  StrInvalidDirectoryIn = 'Invalid directory in MNW1 package';
  StrTheDirectoryForWell = 'The directory for the well output file for the M' +
  'NW1 package does not exist.';
  StrTheDirectoryForTh = 'The directory for the QSUM output file for the MNW' +
  '1 package does not exist.';
  StrTheDirectoryForByNode = 'The directory for the by-node output file for ' +
  'the MNW1 package does not exist.';
  StrDuplicateMNW1Outpu = 'Duplicate MNW1 output file name';
  StrTheQSUMFileNameH = 'The QSUM file name has already been used.';
  StrTheBYNODEFileName = 'The BYNODE file name has already been used.';
  StrTheWEL1FileNameH = 'The WEL1 file name has already been used.';
  StrPESTDelimiterConfl = 'PEST delimiter conflict';
  StrSCanNotBeUsed = '"%s" can not be used as a PEST delimiter because it is' +
  ' used in a different way in the MNW1 package';

var
  Mnw1OutputNames: TStringList;

procedure AddMnw1OutputName(AFileName: string);
begin
  Mnw1OutputNames.Add(AFileName);
end;

procedure ClearMnw1FileNames;
begin
  Mnw1OutputNames.Clear;
end;

{ TModflowMNW1_Writer }

constructor TModflowMNW1_Writer.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FMnwPackage := Package as TMnw1Package;
  FWells := TList<TMnw1Boundary>.Create;

end;

destructor TModflowMNW1_Writer.Destroy;
begin
  FWells.Free;
  inherited;
end;

procedure TModflowMNW1_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TMnw1Boundary;
  Dummy: TStringList;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    Dummy := TStringList.Create;
    try
      frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidDirectoryIn);
      for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
      begin
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        if not ScreenObject.UsedModels.UsesModel(Model) then
        begin
          Continue;
        end;
        Boundary := ScreenObject.ModflowMnw1Boundary;
        if (Boundary = nil) or not Boundary.Used then
        begin
          Continue;
        end;

        FWells.Add(Boundary);
        Boundary.GetCellValues(Values, Dummy, Model, self);
      end;
    finally
      Dummy.Free;
    end;

  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowMNW1_Writer.Extension: string;
begin
  result := '.mnw1';
end;

function TModflowMNW1_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.Mnw1Package;
end;

procedure TModflowMNW1_Writer.UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
var
  DataArrayList: TList;
  TimeIndex: Integer;
  TimeListIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  CellList: TValueCellList;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  try
    DataArrayList := TList.Create;
    try
      Evaluate;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if FWells.Count = 0 then
      begin
        SetTimeListsUpToDate(TimeLists);
        Exit;
      end;

      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        DisplayTimeList := TimeLists[TimeListIndex];
        // Values.Count can be zero if no objects define the boundary condition.
        if (Values.Count <> 0) or (DisplayTimeList.Count = 0) then
        begin
          Assert(Values.Count = DisplayTimeList.Count);
        end;
      end;

      // For each stress period, transfer values from
      // the cells lists to the data arrays.
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        CellList := Values[TimeIndex];
        if CellList.Count > 0 then
        begin
          DataArrayList.Clear;
          for TimeListIndex := 0 to TimeLists.Count - 1 do
          begin
            DisplayTimeList := TimeLists[TimeListIndex];
            DataArray := DisplayTimeList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArrayList.Add(DataArray);
          end;
          UpdateCellDisplay(CellList, DataArrayList,
            [DesiredPumpingRatePosition..ReactivationPumpingRatePosition]);
        end;
      end;

      SetTimeListsUpToDate(TimeLists);
    finally
      DataArrayList.Free;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

end;

procedure TModflowMNW1_Writer.WriteFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingMNW1Package);
    WriteTemplateHeader;

    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;

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

    WriteDataSetX;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 3a.');
    WriteDataSet3a;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 3b.');
    WriteDataSet3b;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 3c.');
    WriteDataSet3c;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 4 and 5.');
    WriteStressPeriods;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

procedure TModflowMNW1_Writer.WriteDataSet1;
var
  MXMNW: integer;
  IWL2CB: Integer;
  IWELPT: Integer;
  NOMOITER: Integer;
  kspref: Integer;
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  CountCells(MXMNW);
  GetFlowUnitNumber(IWL2CB);
  if IWL2CB < 0 then
  begin
    IWL2CB := - Model.UnitNumbers.UnitNumber(StrLIST)
  end;
  if Model.ModflowOutputControl.PrintInputCellLists then
  begin
    IWELPT := 0;
  end
  else
  begin
    IWELPT := 1;
  end;
  NOMOITER := FMnwPackage.MaxMnwIterations;
  kspref := 1;
  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count-1 do
  begin
    StressPeriod := Model.ModflowFullStressPeriods[StressPeriodIndex];
    if StressPeriod.DrawDownReference then
    begin
      kspref := StressPeriodIndex+2;
      break;
    end;
  end;

  WriteInteger(MXMNW);
  WriteInteger(IWL2CB);
  WriteInteger(IWELPT);
  WriteInteger(NOMOITER);
  WriteString(' REF');
  WriteInteger(kspref);
  WriteString(' # Data Set 1: MXMNW IWL2CB IWELPT [NOMOITER] REFerence SP: kspref');
  NewLine;
end;

procedure TModflowMNW1_Writer.WriteDataSet2;
var
  LOSSTYPE: string;
  PLossMNW: Double;
begin
  case FMnwPackage.LossType of
    mlt1Skin: LOSSTYPE := 'SKIN ';
    mlt1Linear: LOSSTYPE := 'LINEAR ';
    mlt1NonLinear: LOSSTYPE := 'NONLINEAR ';
  end;
  WriteString(LOSSTYPE);
  if FMnwPackage.LossType = mlt1NonLinear then
  begin
    PLossMNW := FMnwPackage.LossExponent;
    WriteFloat(PLossMNW);
    WriteString(' # Data Set 2: LOSSTYPE, PLossMNW');
  end
  else
  begin
    WriteString(' # Data Set 2: LOSSTYPE');
  end;
  NewLine
end;

procedure TModflowMNW1_Writer.WriteDataSet3a;
var
  WellFileName: string;
  iunw1: integer;
  FileDir: string;
begin
  WellFileName := FMnwPackage.WellFileName;
  if WellFileName <> '' then
  begin
    FileDir := ExtractFileDir(WellFileName);
    if not DirectoryExists(FileDir) then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidDirectoryIn,
        StrTheDirectoryForWell);
    end;
    WellFileName := ExtractRelativePath(FNameOfFile, WellFileName);
    WellFileName := StringReplace(WellFileName, ' ', '_', [rfReplaceAll]);
    iunw1 := Model.UnitNumbers.UnitNumber(StrMnw1WellOutput);
    WriteString('FILE:');
    WriteString(WellFileName);
    WellFileName := ExpandFileName(WellFileName);

    if Mnw1OutputNames.IndexOf(WellFileName) >= 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrDuplicateMNW1Outpu,
        StrTheWEL1FileNameH);
    end
    else
    begin
      AddMnw1OutputName(WellFileName);
    end;

    WriteString(' WEL1:');
    WriteInteger(iunw1);
    NewLine;
    if not WritingTemplate then
    begin
      WriteToNameFile(StrData, iunw1, WellFileName, foOutput, Model);
    end;
  end;
end;

procedure TModflowMNW1_Writer.WriteDataSet3b;
var
  ByNodeFileName: string;
  iunby: integer;
  FileDir: string;
begin
  ByNodeFileName := FMnwPackage.ByNodeFileName;
  if ByNodeFileName <> '' then
  begin
    FileDir := ExtractFileDir(ByNodeFileName);
    if not DirectoryExists(FileDir) then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidDirectoryIn,
        StrTheDirectoryForByNode);
    end;
    ByNodeFileName := ExtractRelativePath(FNameOfFile, ByNodeFileName);
    ByNodeFileName := StringReplace(ByNodeFileName, ' ', '_', [rfReplaceAll]);
    iunby := Model.UnitNumbers.UnitNumber(StrMnw1ByNode);
    WriteString('FILE:');
    WriteString(ByNodeFileName);
    ByNodeFileName := ExpandFileName(ByNodeFileName);

    if Mnw1OutputNames.IndexOf(ByNodeFileName) >= 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrDuplicateMNW1Outpu,
        StrTheBYNODEFileName);
    end
    else
    begin
      AddMnw1OutputName(ByNodeFileName);
    end;

    WriteString(' BYNODE:');
    WriteInteger(iunby);
    if FMnwPackage.ByNodePrintFrequency = mpfAll then
    begin
      WriteString(' ALLTIME');
    end;
    NewLine;
    if not WritingTemplate then
    begin
      WriteToNameFile(StrData, iunby, ByNodeFileName, foOutput, Model);
    end;
  end;
end;

procedure TModflowMNW1_Writer.WriteDataSet3c;
var
  QSumFileName: string;
  iunqs: integer;
  FileDir: string;
begin
  QSumFileName := FMnwPackage.QSumFileName;
  if QSumFileName <> '' then
  begin
    FileDir := ExtractFileDir(QSumFileName);
    if not DirectoryExists(FileDir) then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidDirectoryIn,
        StrTheDirectoryForTh);
    end;
    QSumFileName := ExtractRelativePath(FNameOfFile, QSumFileName);
    QSumFileName := StringReplace(QSumFileName, ' ', '_', [rfReplaceAll]);
    iunqs := Model.UnitNumbers.UnitNumber(StrMnw1QSum);
    WriteString('FILE:');
    WriteString(QSumFileName);
    QSumFileName := ExpandFileName(QSumFileName);

    if Mnw1OutputNames.IndexOf(QSumFileName) >= 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrDuplicateMNW1Outpu,
        StrTheQSUMFileNameH);
    end
    else
    begin
      AddMnw1OutputName(QSumFileName);
    end;

    WriteString(' QSUM:');
    WriteInteger(iunqs);
    if FMnwPackage.QSumPrintFrequency = mpfAll then
    begin
      WriteString(' ALLTIME');
    end;
    NewLine;
    if not WritingTemplate then
    begin
      WriteToNameFile(StrData, iunqs, QSumFileName, foOutput, Model);
    end;
  end;
end;

procedure TModflowMNW1_Writer.WriteDataSetX;
begin
  WriteString('PREFIX: FILEPREFIX');
  NewLine;
end;

procedure TModflowMNW1_Writer.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrMNW1) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrDuplicateMNW1Outpu);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrPESTDelimiterConfl);

  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(StrMNW1, Model.UnitNumbers.UnitNumber(StrMNW1),
    FNameOfFile, foInput, Model);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  WriteFileInternal;

  if Model.PestUsed and FPestParamUsed then
  begin
    FNameOfFile := FNameOfFile + '.tpl';
    WritePestTemplateLine(FNameOfFile);
    WritingTemplate := True;
    WriteFileInternal;
  end;
end;

procedure TModflowMNW1_Writer.WriteStressPeriods;
var
  TimeIndex: Integer;
  List: TValueCellList;
  ITMP: integer;
  CellIndex: Integer;
  Cell: TMnw1Cell;
  PriorCell: TMnw1Cell;
  Rw: double;
  NextCell: TMnw1Cell;
  PestDelimiters: TSysCharSet;
begin
  if Model.PestUsed then
  begin
    PestDelimiters := [Model.PestProperties.TemplateCharacter,
      Model.PestProperties.ExtendedTemplateCharacter,
      Model.PestProperties.ArrayTemplateCharacter];
  end
  else
  begin
    PestDelimiters := [];
  end;
  for TimeIndex := 0 to Values.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage(Format(StrWritingStressPer, [TimeIndex+1]));
    GetITMP(ITMP, TimeIndex, List);

    if (ITMP = 0) then
    begin
      frmErrorsAndWarnings.AddWarning(Model,
        Format(StrNoBoundaryConditio1, [Package.PackageIdentifier]),
        Format(StrStressPeriod0d, [TimeIndex+1]));
    end;

    // data set 4;
    WriteInteger(ITMP);
    WriteString(' # Data Set 4, Stress period ' + IntToStr(TimeIndex+1) + ': ITMP');
    NewLine;

    if ITMP > 0 then
    begin
      PriorCell := nil;
      for CellIndex := 0 to List.Count - 1 do
      begin
        Cell := List[CellIndex] as TMnw1Cell;
        WriteInteger(Model.DataSetLayerToModflowLayer(Cell.Layer));
        WriteInteger(Cell.Row+1);
        WriteInteger(Cell.Column+1);
//        WriteFloat(Cell.DesiredPumpingRate);
        WriteValueOrFormula(Cell, DesiredPumpingRatePosition);


        if (PriorCell <> nil) and (Cell.ConductanceMethod <> mcmFixed)
          and (PriorCell.ScreenObject = Cell.ScreenObject) then
        begin
          WriteString(' MN');
        end;
//        WriteFloat(Cell.WaterQuality);
        WriteValueOrFormula(Cell, WaterQualityPosition);
//        Rw := 0;
        case Cell.ConductanceMethod of
          mcmRadius:
            begin
//              Rw := Cell.WellRadius;
              WriteValueOrFormula(Cell, WellRadiusPosition);
            end;
          mcmFixed:
            begin
              Rw := 0;
              WriteFloat(Rw);
            end;
          mcmConductance:
            begin
//              Rw := -Cell.Conductance;
              WriteValueOrFormula(Cell, ConductancePosition, 0, True);
            end
          else Assert(False);
        end;
//        WriteFloat(Rw);
//        WriteFloat(Cell.SkinFactor);
        WriteValueOrFormula(Cell, SkinFactorPosition);
//        WriteFloat(Cell.LimitingWaterLevel);
        WriteValueOrFormula(Cell, LimitingWaterLevelPosition);
//        WriteFloat(Cell.ReferenceElevation);
        WriteValueOrFormula(Cell, ReferenceElevationPosition);
        if Cell.WaterLevelLimitType = mwlltRelative then
        begin
          WriteString(' DD');
        end;
        WriteInteger(Cell.WaterQualityGroup);
        if FMnwPackage.LossType = mlt1NonLinear then
        begin
          WriteString(' Cp: ');
//          WriteFloat(Cell.NonLinearLossCoefficient);
          WriteValueOrFormula(Cell, NonLinearLossCoefficientPosition);
          if {':' in PestDelimiters} CharInSet(':', PestDelimiters) then
          begin
            Exclude(PestDelimiters, ':');
            frmErrorsAndWarnings.AddError(Model, StrPESTDelimiterConfl,
              Format(StrSCanNotBeUsed, [':']));
          end;
        end;
        if Cell.PumpingLimitType in [mpltAbsolute, mpltPercent] then
        begin
          if Cell.PumpingLimitType = mpltAbsolute then
          begin
            WriteString(' QCUT');
          end
          else if Cell.PumpingLimitType = mpltPercent then
          begin
            WriteString(' Q-%CUT:');
            if {':' in PestDelimiters} CharInSet(':', PestDelimiters) then
            begin
              Exclude(PestDelimiters, ':');
              frmErrorsAndWarnings.AddError(Model, StrPESTDelimiterConfl,
                Format(StrSCanNotBeUsed, [':']));
            end;
            if {'%' in PestDelimiters} CharInSet('%', PestDelimiters) then
            begin
              Exclude(PestDelimiters, '%');
              frmErrorsAndWarnings.AddError(Model, StrPESTDelimiterConfl,
                Format(StrSCanNotBeUsed, ['%']));
            end;
          end
          else
          begin
            Assert(False);
          end;
//          WriteFloat(Cell.MinimumPumpingRate);
          WriteValueOrFormula(Cell, MinimumPumpingRatePosition);
//          WriteFloat(Cell.MaximumPumpingRate);
          WriteValueOrFormula(Cell, ReactivationPumpingRatePosition);
        end;

        if (Cell.Site <> '') then
        begin
          WriteString(' SITE:');
          WriteString(Cell.Site);
          if {':' in PestDelimiters} CharInSet(':', PestDelimiters) then
          begin
            Exclude(PestDelimiters, ':');
            frmErrorsAndWarnings.AddError(Model, StrPESTDelimiterConfl,
              Format(StrSCanNotBeUsed, [':']));
          end;
        end;

        if CellIndex < List.Count - 1 then
        begin
          NextCell := List[CellIndex+1] as TMnw1Cell;
          if (Cell.ConductanceMethod <> mcmFixed)
          and (NextCell.ScreenObject = Cell.ScreenObject) then
          begin
            WriteString(' DEFAULT');
          end;
        end;

        WriteString(' # Data Set 5');
        NewLine;
        PriorCell := Cell;
      end;
    end;
  end;
end;

initialization
  Mnw1OutputNames := TStringList.Create;

finalization
  Mnw1OutputNames.Free;

end.
