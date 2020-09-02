unit ModflowEVT_WriterUnit;

interface

uses System.UITypes, Winapi.Windows, SysUtils, Classes, Contnrs,
  RbwParser, CustomModflowWriterUnit,
  ScreenObjectUnit, ModflowBoundaryUnit, ModflowPackageSelectionUnit,
  OrderedCollectionUnit, ModflowCellUnit, PhastModelUnit,
  ModflowBoundaryDisplayUnit, Vcl.Dialogs;

Type
  TModflowEVT_Writer = class(TCustomTransientArrayWriter)
  private
    NPEVT: integer;
    NEVTOP: integer;
    FDepthSurface: TList;
    FEvtPackage: TEvtPackageSelection;
    FTimeIndex: Integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To10;
    procedure WriteCells(CellList: TValueCellList; const DataSetIdentifier,
      VariableIdentifiers: string);
    procedure WriteEvapotranspirationSurface(CellList: TValueCellList);
    procedure WriteExtinctionDepth(CellList: TValueCellList);
  protected
    function CellType: TValueCellType; override;
    function Prefix: string; override;
    class function Extension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    procedure Evaluate; override;
    procedure HandleMissingArrayData; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses ModflowUnitNumbers, ModflowTransientListParameterUnit,
  frmErrorsAndWarningsUnit, DataSetUnit, ModflowEvtUnit, GoPhastTypes, 
  frmProgressUnit, Forms;

resourcestring
  StrEvapotranspirationH = 'Evapotranspiration has not been defined in one o' +
  'r more stress periods';

{ TModflowEVT_Writer }

function TModflowEVT_Writer.CellType: TValueCellType;
begin
  result := TEvt_Cell
end;

constructor TModflowEVT_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FDepthSurface := TObjectList.Create;
end;

destructor TModflowEVT_Writer.Destroy;
begin
  FDepthSurface.Free;
  inherited;
end;

procedure TModflowEVT_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TEvtBoundary;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrEvapotranspirationD_EVT);
  frmErrorsAndWarnings.BeginUpdate;
  try
    inherited;
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowEvtBoundary;
      if Boundary <> nil then
      begin
        Boundary.GetEvapotranspirationLayerCells(FLayers, Model);
        Boundary.GetEvapotranspirationSurfaceDepthCells(FDepthSurface, Model);
        Boundary.EvtSurfDepthCollection.ClearBoundaries(Model);
        Boundary.EvapotranspirationLayers.ClearBoundaries(Model);
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowEVT_Writer.Extension: string;
begin
  result := '.evt';
end;

function TModflowEVT_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowEvtBoundary;
end;

procedure TModflowEVT_Writer.HandleMissingArrayData;
begin
  inherited;
  frmErrorsAndWarnings.AddWarning(Model, StrEvapotranspirationH,
    Format(StrStressPeriodD, [FTimeIndex+1]));
end;

function TModflowEVT_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.EvtPackage;
end;

function TModflowEVT_Writer.ParameterType: TParameterType;
begin
  result := ptEVT;
end;

function TModflowEVT_Writer.Prefix: string;
begin
  result := 'ET'
end;

resourcestring
//  ErrorRoot = 'One or more %s parameters have been eliminated '
//    + 'because there are no cells associated with them.';
  EtSurfaceError = 'The ET Surface is undefined in the EVT package.';
  EtSurfaceErrorMessage = 'No objects define the ET Surface in the EVT package.';
  EtDepthError = 'The ET Depth is undefined in the EVT package.';
  EtDepthErrorMessage = 'No objects define the ET Depth in the EVT package.';
  StrNoEvapotranspiratio = 'No evapotranspiration defined';
  StrTheEvapotranspirati = 'The Evapotranspiration package is active but no ' +
  'evapotranspiration has been defined for any stress period.';
  StrWritingEVTPackage = 'Writing EVT Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
  StrWritingDataSets5to10 = '  Writing Data Sets 5 to 10.';
  StrWritingStressP = '    Writing Stress Period %d';
  StrNoParametersHaveB = 'No parameters have been defined for the Evapotrans' +
  'piration package for the following Stress periods.';

procedure TModflowEVT_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  List: TValueCellList;
  ParameterValues: TList;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  DepthSurfaceCellList: TValueCellList;
  EvapRateTimes: TModflowBoundaryDisplayTimeList;
  EvapotranspirationSurfaceTimes : TModflowBoundaryDisplayTimeList;
  EvapotranspirationDepthTimes : TModflowBoundaryDisplayTimeList;
  EvapotranspirationLayerTimes : TModflowBoundaryDisplayTimeList;
  NPEVT: integer;
  ParamDefArrays: TList;
  EvapRateArray: TModflowBoundaryDisplayDataArray;
  EvapSurfArray: TModflowBoundaryDisplayDataArray;
  EvapDepthArray: TModflowBoundaryDisplayDataArray;
  EvapLayerArray: TModflowBoundaryDisplayDataArray;
  DefArrayList: TList;
  Index: Integer;
  ATimeList: TModflowBoundaryDisplayTimeList;
const
  D7PNameIname = '';
  D7PName = '';
begin
  try
    frmErrorsAndWarnings.BeginUpdate;
    try
      frmErrorsAndWarnings.RemoveErrorGroup(Model, EtSurfaceError);
      frmErrorsAndWarnings.RemoveErrorGroup(Model, EtDepthError);
      if not Package.IsSelected then
      begin
        UpdateNotUsedDisplay(TimeLists);
        Exit;
      end;
      ParameterValues := TList.Create;
      try
        Evaluate;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        ClearTimeLists(Model);
        ParamDefArrays := TObjectList.Create;
        try
          EvaluateParameterDefinitions(ParamDefArrays, StrOneOrMoreSParam, umAssign);
          NPEVT := ParameterCount;
          NEVTOP := Ord(Model.ModflowPackages.EvtPackage.LayerOption) + 1;
          EvapRateTimes := TimeLists[0];
          EvapotranspirationSurfaceTimes := TimeLists[1];
          EvapotranspirationDepthTimes := TimeLists[2];
          EvapotranspirationLayerTimes := TimeLists[3];

          if Values.Count = 0 then
          begin
            SetTimeListsUpToDate(TimeLists);
            Exit;
          end;
          for TimeIndex := 0 to Values.Count - 1 do
          begin
            EvapRateArray := EvapRateTimes[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            EvapSurfArray := EvapotranspirationSurfaceTimes[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            EvapDepthArray := EvapotranspirationDepthTimes[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            if EvapotranspirationLayerTimes = nil then
            begin
              EvapLayerArray := nil;
            end
            else
            begin
              EvapLayerArray := EvapotranspirationLayerTimes[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
            end;

            ParametersUsed := TStringList.Create;
            try
              RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
                ParametersUsed, ParameterValues, True);
              List := Values[TimeIndex];
    //          List.CheckRestore;

              // data set 6
              if FDepthSurface.Count > 0 then
              begin
                DepthSurfaceCellList := FDepthSurface[TimeIndex];
    //            DepthSurfaceCellList.CheckRestore;
                AssignTransient2DArray(EvapSurfArray, 0, DepthSurfaceCellList, 0,
                  rdtDouble, umAssign);
              end
              else
              begin
                DepthSurfaceCellList := nil;
                EvapSurfArray.UpToDate := True;
                frmErrorsAndWarnings.AddError(Model, EtSurfaceError,
                  EtSurfaceErrorMessage);
              end;
              EvapSurfArray.CacheData;

              if NPEVT = 0 then
              begin
                // data set 7
                AssignTransient2DArray(EvapRateArray, 0, List, 0, rdtDouble, umAssign);
              end
              else
              begin
                // data set 8
                DefArrayList := ParamDefArrays[TimeIndex];
                UpdateTransient2DArray(EvapRateArray, DefArrayList);
              end;
              Model.AdjustDataArray(EvapRateArray);
              EvapRateArray.CacheData;

              // data set 9
              if DepthSurfaceCellList <> nil then
              begin
                AssignTransient2DArray(EvapDepthArray, 1, DepthSurfaceCellList, 0,
                  rdtDouble, umAssign);
              end
              else
              begin
                EvapDepthArray.UpToDate := True;
                frmErrorsAndWarnings.AddError(Model, EtDepthError,
                  EtDepthErrorMessage);
              end;
              EvapDepthArray.CacheData;

              // data set 10
              if EvapLayerArray <> nil then
              begin
                if (Model.ModflowPackages.EvtPackage.
                  LayerOption = loSpecified)
                  and not Model.ModflowPackages.EvtPackage.
                  TimeVaryingLayers and (ParameterCount > 0)  then
                begin
                  List.Cache;
                  RetrieveParametersForStressPeriod(D7PNameIname, D7PName, 0,
                    ParametersUsed, ParameterValues, True);
                  List := Values[0];
    //              List.CheckRestore;
                end;
                UpdateLayerDisplay(List, ParameterValues, TimeIndex,
                  EvapLayerArray);
                EvapLayerArray.CacheData;
              end;
              List.Cache;
              if DepthSurfaceCellList <> nil then
              begin
                DepthSurfaceCellList.Cache;
              end;
            finally
              ParametersUsed.Free;
            end;
          end;
          for Index := 0 to TimeLists.Count - 1 do
          begin
            ATimeList := TimeLists[Index];
            if ATimeList <> nil then
            begin
              ATimeList.SetUpToDate(True);
            end;
          end;
        finally
          ParamDefArrays.Free;
        end;
      finally
        ParameterValues.Free;
      end;
    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TModflowEVT_Writer.WriteDataSets3And4;
const
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NCLU';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Condfact';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, StrOneOrMoreSParam, umAssign,
    FEvtPackage.MultiplierArrayNames, FEvtPackage.ZoneArrayNames);
end;

procedure TModflowEVT_Writer.WriteDataSet1;
begin
  NPEVT := ParameterCount;
  if NPEVT > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPEVT);
    WriteString(' # PARAMETER NPEVT');
    NewLine;
  end;
end;

procedure TModflowEVT_Writer.WriteDataSet2;
var
  IEVTCB: integer;
begin
  NEVTOP := Ord(Model.ModflowPackages.EvtPackage.LayerOption) + 1;
  GetFlowUnitNumber(IEVTCB);

  WriteInteger(NEVTOP);
  WriteInteger(IEVTCB);
  WriteString(' # DataSet 2: NEVTOP IEVTCB');
  NewLine;
end;

procedure TModflowEVT_Writer.WriteDataSets5To10;
const
  D7PName =      ' # Data Set 8: PARNAM IEVTPF';
  D7PNameIname = ' # Data Set 8: PARNAM Iname IEVTPF';
  DS5 = ' # Data Set 5: INSURF INEVTR INEXDP INIEVT';
  DataSetIdentifier = 'Data Set 7:';
  VariableIdentifiers = 'EVTR';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowEVT_Writer.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, EtSurfaceError);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, EtDepthError);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrEvapotranspirationH);

  if not Package.IsSelected then
  begin
    Exit
  end;
  FEvtPackage := Package as TEvtPackageSelection;
  if Model.PackageGeneratedExternally(StrEVT) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;


  FEvtPackage.MultiplierArrayNames.Clear;
  FEvtPackage.ZoneArrayNames.Clear;
  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(StrEVT, Model.UnitNumbers.UnitNumber(StrEVT),
    FNameOfFile, foInput, Model);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  ClearTimeLists(Model);
  OpenFile(FileName(AFileName));
  try
    frmProgressMM.AddMessage(StrWritingEVTPackage);
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

    frmProgressMM.AddMessage(StrWritingDataSets3and4);
    WriteDataSets3And4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets5to10);
    WriteDataSets5To10;
  finally
    CloseFile;
//    Clear;
  end;
end;

procedure TModflowEVT_Writer.WriteCells(CellList: TValueCellList;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := DataSetIdentifier + ' ' + VariableIdentifiers;
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, umAssign, True, Dummy, VariableIdentifiers);
end;

procedure TModflowEVT_Writer.WriteEvapotranspirationSurface(CellList: TValueCellList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := 'Data Set 6: SURF';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, umAssign, False, Dummy, 'SURF');
end;

procedure TModflowEVT_Writer.WriteExtinctionDepth(CellList: TValueCellList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 1;
  Comment := 'Data Set 9: EXDP';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, umAssign, False, Dummy, 'EXDP');
end;

procedure TModflowEVT_Writer.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
var
  NP: Integer;
  EtRateList, PriorEtRateList: TValueCellList;
  ParameterValues: TValueCellList;
  ParamIndex: Integer;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  INEVTR, INIEVT: Integer;
  INSURF: Integer;
  INEXDP: Integer;
  DepthSurfaceCellList, PriorListDepthSurfaceCellList: TValueCellList;
  Comment: string;
begin
  inherited;
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoParametersHaveB);
  ParameterValues := TValueCellList.Create(CellType);
  try
    ParameterValues.OwnsObjects := False;
    Comment := 'Data Set 10: IEVT';
    if Values.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrNoEvapotranspiratio,
        StrTheEvapotranspirati);
    end;
    for TimeIndex := 0 to Values.Count - 1 do
    begin
      FTimeIndex := TimeIndex;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      frmProgressMM.AddMessage(Format(StrWritingStressP, [TimeIndex+1]));
      ParametersUsed := TStringList.Create;
      try
        RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
          ParametersUsed, ParameterValues, True);
        NP := ParametersUsed.Count;
        EtRateList := Values[TimeIndex];
        // data set 5;
        if (TimeIndex > 0) and (FDepthSurface.Count > 0) then
        begin
          PriorListDepthSurfaceCellList := FDepthSurface[TimeIndex-1];
          DepthSurfaceCellList := FDepthSurface[TimeIndex];
          if PriorListDepthSurfaceCellList.AreRealValuesIdentical(DepthSurfaceCellList, 0) then
          begin
            INSURF := -1;
//            DepthSurfaceCellList.Cache;
          end
          else
          begin
            INSURF := 1;
          end;
          PriorListDepthSurfaceCellList.Cache;
        end
        else
        begin
          INSURF := 1;
        end;

        if NPEVT > 0 then
        begin
          INEVTR := NP;
        end
        else
        begin
          if (TimeIndex > 0) then
          begin
            PriorEtRateList := Values[TimeIndex-1];
            if PriorEtRateList.AreRealValuesIdentical(EtRateList, 0) then
            begin
              INEVTR := -1;
//              EtRateList.Cache;
            end
            else
            begin
              INEVTR := 1;
            end;
            PriorEtRateList.Cache;
          end
          else
          begin
            INEVTR := 1;
          end;
        end;

        if (TimeIndex > 0) and (FDepthSurface.Count > 0) then
        begin
          PriorListDepthSurfaceCellList := FDepthSurface[TimeIndex-1];
          DepthSurfaceCellList := FDepthSurface[TimeIndex];
          if PriorListDepthSurfaceCellList.AreRealValuesIdentical(DepthSurfaceCellList, 1) then
          begin
            INEXDP := -1;
//            DepthSurfaceCellList.Cache;
          end
          else
          begin
            INEXDP := 1;
          end;
          PriorListDepthSurfaceCellList.Cache;
        end
        else
        begin
          INEXDP := 1;
        end;

        if NEVTOP = 2 then
        begin
          INIEVT := 1;
        end
        else
        begin
          INIEVT := -1;
        end;

        WriteInteger(INSURF);
        WriteInteger(INEVTR);
        WriteInteger(INEXDP);
        WriteInteger(INIEVT);
        WriteString(DS5 + ' Stress period ' + IntToStr(TimeIndex+1));
        NewLine;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          EtRateList.Cache;
          Exit;
        end;

        // data set 6
        if INSURF > 0 then
        begin
          if FDepthSurface.Count > 0 then
          begin
            DepthSurfaceCellList := FDepthSurface[TimeIndex];
            WriteEvapotranspirationSurface(DepthSurfaceCellList);
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              EtRateList.Cache;
              Exit;
            end;
            if (INEXDP < 0) and (TimeIndex = Values.Count - 1) then
            begin
              DepthSurfaceCellList.Cache;
            end;
          end
          else
          begin
//            DepthSurfaceCellList := nil;
            frmErrorsAndWarnings.AddError(Model, EtSurfaceError,
              EtSurfaceErrorMessage);
          end;

        end;
        if NPEVT = 0 then
        begin
          // data set 7
          if INEVTR > 0 then
          begin
            WriteCells(EtRateList, DataSetIdentifier, VariableIdentifiers);
          end;
        end
        else
        begin
          // data set 8
          if ParametersUsed.Count = 0 then
          begin
            frmErrorsAndWarnings.AddError(Model, StrNoParametersHaveB,
              IntToStr(TimeIndex+1));
          end;
          for ParamIndex := 0 to ParametersUsed.Count - 1 do
          begin
            WriteString(ParametersUsed[ParamIndex]);
            NewLine;
          end;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          EtRateList.Cache;
          Exit;
        end;

        // data set 9
        if INEXDP > 0 then
        begin
          if FDepthSurface.Count > 0 then
          begin
            DepthSurfaceCellList := FDepthSurface[TimeIndex];
            WriteExtinctionDepth(DepthSurfaceCellList);
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              EtRateList.Cache;
              Exit;
            end;
            if  (TimeIndex = Values.Count - 1) then
            begin
              DepthSurfaceCellList.Cache;
            end;
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model, EtDepthError,
              EtDepthErrorMessage);
          end;
        end;

        // data set 10
        WriteLayerSelection(EtRateList, ParameterValues, TimeIndex, Comment, 'IEVT');
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          EtRateList.Cache;
          Exit;
        end;
        EtRateList.Cache;
      finally
        ParametersUsed.Free;
      end;
    end;
  finally
    ParameterValues.Free;
  end;
end;

end.

