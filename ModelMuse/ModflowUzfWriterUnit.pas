unit ModflowUzfWriterUnit;

interface

uses System.UITypes,Winapi.Windows, SysUtils, Classes, Contnrs, RbwParser,
  PhastModelUnit,
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, OrderedCollectionUnit, ModflowBoundaryDisplayUnit,
  Vcl.Dialogs;

type
  TModflowUzfWriter = class(TCustomTransientWriter)
  private
    NUZGAG: integer;
    IUZFOPT: integer;
    IRUNFLG: integer;
    FEtDemand: TList;
    FEExtinctionDepths: TList;
    FExtinctionWaterContent : TList;
//    FNameOfFile: string;
    UzfPackage: TUzfPackageSelection;
    procedure CountGages;
    procedure WriteDataSet1a;
    procedure WriteDataSet1b;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet4B;
    procedure WriteDataSet5;
    procedure WriteDataSet6a;
    procedure WriteDataSet6b;
    procedure WriteDataSet7;
    procedure WriteDataSet8(GageStart: integer);
    procedure WriteStressPeriods; reintroduce;
    procedure WriteInfiltrationRates(CellList: TList);
    procedure WritePotentialEtRates(CellList: TList);
    procedure WriteExtinctionDepth(CellList: TList);
    procedure WriteExtinctionWaterContent(CellList: TList);
    procedure WriteGagesToNameFile(const AFileName: string; var GageStart: integer);
    procedure WriteFileInternal(GageStart: Integer);
  protected
    procedure Evaluate; override;
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    procedure GetFlowUnitNumber(var UnitNumber: Integer); override;
    // @name is the file extension used for the observation input file.
    class function ObservationExtension: string; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
    // @name is the file extension used for the observation output file.
    class function ObservationOutputExtension: string; override;
  end;

implementation

uses ModflowUnitNumbers, DataSetUnit, ModflowUzfUnit, frmErrorsAndWarningsUnit,
  frmProgressUnit, ModflowCellUnit, Forms, GoPhastTypes,
  ModflowOutputControlUnit, PestParamRoots;

resourcestring
  StrUnspecifiedUZFData = 'Unspecified UZF data';
  StrTheInfiltrationRat = 'The infiltration rate in the UZF package was not ' +
  'defined in the following stress periods.';
  StrTheETDemandRateI = 'The ET demand rate in the UZF package was not defin' +
  'ed in the following stress periods.';
  StrTheETExtinctionDe = 'The ET extinction depth in the UZF package was not' +
  ' defined in the following stress periods.';
  StrTheETExtinctionWa = 'The ET extinction water content in the UZF package' +
  ' was not defined in the following stress periods.';
  StrNoTransientDataI = 'No transient data (infiltration and/or evapotranspi' +
  'ration) has been defined.';
  StrEvaluatingUZFPacka = 'Evaluating UZF Package data.';
  StrEvaluatingS = '    Evaluating %s.';
  StrWritingUZFPackage = 'Writing UZF Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
  StrWritingDataSet4b = '  Writing Data Set 4b.';
//  StrWritingDataSet5 = '  Writing Data Set 5.';
  StrWritingDataSet6 = '  Writing Data Set 6a.';
  StrWritingDataSet6b = '  Writing Data Set 6b.';
//  StrWritingDataSet7 = '  Writing Data Set 7.';
//  StrWritingDataSet8 = '  Writing Data Set 8.';
  StrWritingDataSets9to16 = '  Writing Data Sets 9 to 16.';
  StrWritingStressP = '    Writing Stress Period %d';
  StrInvalidS = 'Invalid %s';
  StrWhenTheUZFPackage = 'When the UZF package is used, %s should be greater' +
  ' than zero for any cells where unsaturated flow should occur.';
  StrUnsupportedOptionI = 'Unsupported option in the UZF package';
  StrTheOptionIsOnly = 'The %s option is only supported in MODFLOW-NWT versio' +
  'n 1.1 or later and MODFLOW-2005 1.12 or later.';
  StrRow0dColumn1 = 'Row %0:d; Column %1:d';
  StrErrorInUZFOptions = 'Error in UZF options';
  StrInTheUZFPackageT = 'In the UZF package the option to hydraulic conducti' +
  'vity from the flow package can only be used if there are convertible laye' +
  'rs.';

{ TModflowUzfWriter }

constructor TModflowUzfWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FEtDemand := TObjectList.Create;
  FEExtinctionDepths := TObjectList.Create;
  FExtinctionWaterContent := TObjectList.Create;
end;

destructor TModflowUzfWriter.Destroy;
begin
  FEtDemand.Free;
  FEExtinctionDepths.Free;
  FExtinctionWaterContent.Free;
  inherited;
end;

procedure TModflowUzfWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TUzfBoundary;
  NoAssignmentErrorRoot: string;
begin
  NoAssignmentErrorRoot := Format(StrNoBoundaryConditio,
    [Package.PackageIdentifier]);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoAssignmentErrorRoot);
    frmProgressMM.AddMessage(StrEvaluatingUZFPacka);
    CountGages;

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
      Boundary := ScreenObject.ModflowUzfBoundary;
      if Boundary <> nil then
      begin
        frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
        if not ScreenObject.SetValuesOfEnclosedCells
          and not ScreenObject.SetValuesOfIntersectedCells then
        begin
          frmErrorsAndWarnings.AddError(Model,
            NoAssignmentErrorRoot, ScreenObject.Name, ScreenObject);
        end;
        Boundary.GetCellValues(Values, nil, Model, self);
        if Model.ModflowPackages.UzfPackage.SimulateET then
        begin
          Boundary.GetEvapotranspirationDemandCells(FEtDemand, Model);
          Boundary.GetExtinctionDepthCells(FEExtinctionDepths, Model);
          Boundary.GetWaterContentCells(FExtinctionWaterContent, Model);
        end;
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowUzfWriter.Extension: string;
begin
  result := '.uzf';
end;

procedure TModflowUzfWriter.GetFlowUnitNumber(var UnitNumber: Integer);
begin
  case Model.ModflowOutputControl.SaveCellFlows of
    csfNone:
      begin
        UnitNumber := 0;
      end;
    csfBinary:
      begin
        UnitNumber := -Model.UnitNumbers.UnitNumber(StrCBC);
      end;
    csfListing:
      begin
        // UZF does not provide an option to save values to the
        // listing file.
        UnitNumber := 0;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

class function TModflowUzfWriter.ObservationExtension: string;
begin
  result := '.ob_uzf';
end;

class function TModflowUzfWriter.ObservationOutputExtension: string;
begin
  result := '.ob_uzf_out';
end;

procedure TModflowUzfWriter.WriteFileInternal(GageStart: Integer);
begin
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingDataSet0);

    WriteTemplateHeader;

    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1a;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet1b;
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
    WriteDataSet4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet4b);
    WriteDataSet4B;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet5);
    WriteDataSet5;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet6);
    WriteDataSet6a;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet6b);
    WriteDataSet6b;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet7);
    WriteDataSet7;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet8);
    WriteDataSet8(GageStart);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets9to16);
    WriteStressPeriods;
  finally
    CloseFile;
  end;
end;

function TModflowUzfWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.UzfPackage;
end;

procedure TModflowUzfWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  TimeIndex: Integer;
  Infiltration: TModflowBoundaryDisplayTimeList;
  EtDemand: TModflowBoundaryDisplayTimeList;
  ExtinctionDepth: TModflowBoundaryDisplayTimeList;
  WaterContent: TModflowBoundaryDisplayTimeList;
  InfiltrationArray: TModflowBoundaryDisplayDataArray;
  EtDemandArray: TModflowBoundaryDisplayDataArray;
  ExtinctionDepthArray: TModflowBoundaryDisplayDataArray;
  WaterContentArray: TModflowBoundaryDisplayDataArray;
  CellList: TValueCellList;
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

  try
    frmErrorsAndWarnings.BeginUpdate;
    try
      frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUnspecifiedUZFData);
      frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheInfiltrationRat);
      frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETDemandRateI);
      frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETExtinctionDe);
      frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETExtinctionWa);

      Infiltration := TimeLists[0];
      if Model.ModflowPackages.UzfPackage.SimulateET then
      begin
        EtDemand := TimeLists[1];
        ExtinctionDepth := TimeLists[2];
        WaterContent := TimeLists[3];
      end
      else
      begin
        EtDemand := nil;
        ExtinctionDepth := nil;
        WaterContent := nil;
      end;
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        InfiltrationArray := Infiltration[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        CellList := Values[TimeIndex];
//        CellList.CheckRestore;
        AssignTransient2DArray(InfiltrationArray, 0, CellList, 0,
          rdtDouble, Model.ModflowPackages.UzfPackage.AssignmentMethod);
        Model.AdjustDataArray(InfiltrationArray);
        CellList.Cache;

        if Model.ModflowPackages.UzfPackage.SimulateET then
        begin
          EtDemandArray := EtDemand[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          if TimeIndex < FEtDemand.Count then
          begin
            CellList := FEtDemand[TimeIndex];
    //        CellList.CheckRestore;
            AssignTransient2DArray(EtDemandArray, 0, CellList, 0,
              rdtDouble, umAssign);
            Model.AdjustDataArray(EtDemandArray);
            CellList.Cache;
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model,
              StrTheETDemandRateI, IntToStr(TimeIndex+1));
          end;

          ExtinctionDepthArray := ExtinctionDepth[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          if TimeIndex < FEExtinctionDepths.Count then
          begin
            CellList := FEExtinctionDepths[TimeIndex];
    //        CellList.CheckRestore;
            AssignTransient2DArray(ExtinctionDepthArray, 0, CellList, 0,
              rdtDouble, umAssign);
            CellList.Cache;
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model,
              StrTheETExtinctionDe, IntToStr(TimeIndex+1));
          end;


          WaterContentArray := WaterContent[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          if TimeIndex <  FExtinctionWaterContent.Count then
          begin
            CellList := FExtinctionWaterContent[TimeIndex];
    //        CellList.CheckRestore;
            AssignTransient2DArray(WaterContentArray, 0, CellList, 0,
              rdtDouble, umAssign);
            CellList.Cache;
          end
          else
          begin
            frmErrorsAndWarnings.AddError(Model,
              StrTheETExtinctionWa, IntToStr(TimeIndex+1));
          end;
        end;
      end;
      SetTimeListsUpToDate(TimeLists);
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

procedure TModflowUzfWriter.WriteGagesToNameFile(const AFileName: string;
  var GageStart: integer);
var
  FileRoot: string;
  Index: Integer;
begin
  GageStart := Model.ParentModel.UnitNumbers.SequentialUnitNumber+1;
  FileRoot := ChangeFileExt(AFileName, '');
  for Index := 0 to NUZGAG - 1 do
  begin
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDATA, Model.ParentModel.UnitNumbers.SequentialUnitNumber, FileRoot
        + IntToStr(Index + 1) + '.uzfg', foOutput, Model);
    end;
  end;
end;

procedure TModflowUzfWriter.WritePotentialEtRates(CellList: TList);
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
  Comment := '# Data Set 12: PET';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, umAssign, True, Dummy, 'PET');
end;

procedure TModflowUzfWriter.WriteExtinctionDepth(CellList: TList);
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
  Comment := '# Data Set 14: EXTDP';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, umAssign, False, Dummy, 'EXTDP');
end;

procedure TModflowUzfWriter.WriteExtinctionWaterContent(CellList: TList);
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
  Comment := '# Data Set 16: EXTWC';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, umAssign, False, Dummy, 'EXTWC');
end;

procedure TModflowUzfWriter.WriteDataSet1a;
var
  NwtFormat: TNwtFormat;
  unitrech: Integer;
  unitdis: Integer;
  BaseName: string;
begin
  IUZFOPT := Model.ModflowPackages.UzfPackage.VerticalKSource;
  if not (Model.ModflowPackages.LpfPackage.IsSelected
    or Model.ModflowPackages.UpwPackage.IsSelected) then
  begin
    IUZFOPT := 1;
  end;
  if Model.ModelSelection in [msModflowNWT, msModflowLGR2, msModflow,
    msModflowFmp {, msModflowCfp}] then
  begin
    UzfPackage := Model.ModflowPackages.UzfPackage;
    NwtFormat := Model.NWT_Format;

    case NwtFormat of
      nf1_0:
        begin
          if UzfPackage.SpecifyResidualWaterContent
            or UzfPackage.SpecifyInitialWaterContent
            or not UzfPackage.CalulateSurfaceLeakage
            then
          begin
            if UzfPackage.SpecifyResidualWaterContent then
            begin
              WriteString('SPECIFYTHTR ');
            end;
            if UzfPackage.SpecifyInitialWaterContent then
            begin
              WriteString('SPECIFYTHTI ');
            end;
            if not UzfPackage.CalulateSurfaceLeakage then
            begin
              WriteString('NOSURFLEAK ');
            end;
            WriteString('# Data set 1a');
            NewLine;
          end;

          if UzfPackage.SurfaceKUsedToCalculateRejection then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrUnsupportedOptionI,
              Format(StrTheOptionIsOnly, ['REJECTSURFK']));
          end;
          if UzfPackage.SurfaceKUsedToCalculateSeepage then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrUnsupportedOptionI,
              Format(StrTheOptionIsOnly, ['SEEPSURFK']));
          end;
          if UzfPackage.ETSmoothed then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrUnsupportedOptionI,
              Format(StrTheOptionIsOnly, ['ETSQUARE']));
          end;
          if UzfPackage.WriteRechargeAndDischarge then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrUnsupportedOptionI,
              Format(StrTheOptionIsOnly, ['NETFLUX']));
          end;
        end;
      nf1_1:
        begin
          WriteString('OPTIONS');
          NewLine;

          if UzfPackage.SpecifyResidualWaterContent then
          begin
            WriteString('  SPECIFYTHTR');
            NewLine;
          end;

          if UzfPackage.SpecifyInitialWaterContent then
          begin
            WriteString('  SPECIFYTHTI');
            NewLine;
          end;

          if not UzfPackage.CalulateSurfaceLeakage then
          begin
            WriteString('  NOSURFLEAK');
            NewLine;
          end;

          if {(IUZFOPT = 1) and} UzfPackage.SpecifySurfaceK then
          begin
            WriteString('  SPECIFYSURFK');
            NewLine;

            if UzfPackage.SurfaceKUsedToCalculateRejection then
            begin
              WriteString('  REJECTSURFK');
              NewLine;
            end;

            if UzfPackage.SurfaceKUsedToCalculateSeepage then
            begin
              WriteString('  SEEPSURFK');
              NewLine;
            end;
          end;

          if UzfPackage.ETSmoothed then
          begin
            WriteString('  ETSQUARE');
            WriteFloat(UzfPackage.SmoothFactor);
            NewLine;
          end;

          if UzfPackage.WriteRechargeAndDischarge then
          begin
            unitrech := Model.UnitNumbers.UnitNumber(StrUzfRecharge);
            unitdis := Model.UnitNumbers.UnitNumber(StrUzfDischarge);

            WriteString('  NETFLUX');
            WriteInteger(unitrech);
            WriteInteger(unitdis);
            NewLine;

            if not WritingTemplate then
            begin
              BaseName := ChangeFileExt(FNameOfFile, '');
              WriteToNameFile(StrDATABINARY, unitrech,
                ChangeFileExt(BaseName, StrUzfRch), foOutput, Model);
              WriteToNameFile(StrDATABINARY, unitdis,
                ChangeFileExt(BaseName, StrUzfDisch), foOutput, Model);
            end;
          end;

          WriteString('END');
          NewLine;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TModflowUzfWriter.WriteDataSet1b;
var
  NUZTOP: integer;
  IETFLG: integer;
  IUZFCB1: integer;
  IUZFCB2: integer;
  NTRAIL2: integer;
  NSETS2: integer;
  SURFDEP: double;
  LAYTYP: TOneDIntegerArray;
  LayerIndex: Integer;
  ShowWarning: Boolean;
begin
  NUZTOP := 0;
  case Model.ModflowPackages.UzfPackage.LayerOption of
    loTop:
      begin
        NUZTOP := 1;
      end;
    loSpecified:
      begin
        NUZTOP := 2;
      end;
    loTopActive:
      begin
        NUZTOP := 3;
      end;
    else
      Assert(False);
  end;
  if Model.ModflowPackages.UzfPackage.RouteDischargeToStreams
    and (Model.ModflowPackages.SfrPackage.IsSelected
    or Model.ModflowPackages.LakPackage.IsSelected
    or Model.ModflowPackages.SwrPackage.IsSelected
    ) then
  begin
    IRUNFLG := 1;
  end
  else
  begin
    IRUNFLG := 0;
  end;
  if Model.ModflowPackages.UzfPackage.SimulateET then
  begin
    IETFLG := 1;
  end
  else
  begin
    IETFLG := 0;
  end;
  IUZFCB1 := 0;
  IUZFCB2 := 0;
  if Model.ModflowOutputControl.Compact then
  begin
    GetFlowUnitNumber(IUZFCB2);
  end
  else
  begin
    GetFlowUnitNumber(IUZFCB1);
  end;
  NTRAIL2 := Model.ModflowPackages.UzfPackage.NumberOfTrailingWaves;
  NSETS2 := Model.ModflowPackages.UzfPackage.NumberOfWaveSets;
  SURFDEP := Model.ModflowPackages.UzfPackage.DepthOfUndulations;


  WriteInteger(NUZTOP);
  WriteInteger(IUZFOPT);
  WriteInteger(IRUNFLG);
  WriteInteger(IETFLG);
  WriteInteger(IUZFCB1);
  WriteInteger(IUZFCB2);
  if IUZFOPT > 0 then
  begin
    WriteInteger(NTRAIL2);
    WriteInteger(NSETS2);
  end;
  WriteInteger(NUZGAG);
  WriteFloat(SURFDEP);
  if Model.ModelSelection in [msModflowNWT, msModflowLGR2, msModflow,
    msModflowFmp {, msModflowCfp}] then
  begin
    WriteString(' # Data Set 1b: NUZTOP IUZFOPT IRUNFLG IETFLG IUZFCB1 IUZFCB2');
  end
  else
  begin
    WriteString(' # Data Set 1: NUZTOP IUZFOPT IRUNFLG IETFLG IUZFCB1 IUZFCB2');
  end;
  if IUZFOPT > 0 then
  begin
    WriteString(' NTRAIL2 NSETS2');
  end;
  WriteString(' NUZGAG SURFDEP');
  NewLine;

  if Abs(IUZFOPT) > 2 then
  begin
    ShowWarning := True;
    LAYTYP := Model.Laytyp;
    for LayerIndex := 0 to Length(LAYTYP) - 1 do
    begin
      if LAYTYP[LayerIndex] > 0 then
       begin
         ShowWarning := False;
         break;
       end;
    end;
    if ShowWarning then
    begin
      frmErrorsAndWarnings.AddError(Model, StrErrorInUZFOptions,
        StrInTheUZFPackageT);
    end;
  end;

end;

procedure TModflowUzfWriter.WriteDataSet2;
var
  IUZFBND: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  OK: Boolean;
  Active: TDataArray;
  IsActive: Boolean;
  SpecifiedHead: TDataArray;
  LayerIndex: Integer;
  LakeId: TDataArray;

begin
  IUZFBND := Model.DataArrayManager.GetDataSetByName(StrUzfLayer);
  Active := Model.DataArrayManager.GetDataSetByName(rsActive);
  SpecifiedHead := Model.DataArrayManager.GetDataSetByName(rsModflowSpecifiedHead);
  if Model.ModflowPackages.LakPackage.IsSelected then
  begin
    LakeId := Model.DataArrayManager.GetDataSetByName(rsLakeID);
  end
  else
  begin
    LakeId := nil;
  end;

//  OK := True;
  for RowIndex := 0 to IUZFBND.RowCount - 1 do
  begin
    for ColIndex := 0 to IUZFBND.ColumnCount - 1 do
    begin
      IsActive := False;
      for LayerIndex := 0 to Active.LayerCount - 1 do
      begin
        IsActive := Active.BooleanData[LayerIndex,RowIndex,ColIndex];
        if IsActive then
        begin
          IsActive := not SpecifiedHead.BooleanData[LayerIndex,RowIndex,ColIndex];
          if not IsActive then
          begin
            break
          end;
        end;
        if (not IsActive) and (LakeId <> nil) then
        begin
          if LakeId.IntegerData[LayerIndex,RowIndex,ColIndex] > 0 then
          begin
            break;
          end;
        end;
      end;
      if IsActive then
      begin
        OK := IUZFBND.IntegerData[0,RowIndex,ColIndex] > 0;
        if not OK then
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            Format(StrWhenTheUZFPackage, [IUZFBND.DisplayName]),
            Format(StrRow0dColumn1, [RowIndex+1,ColIndex+1]));
        end;
      end;
    end;
//    if not OK then
//    begin
//      Break;
//    end;
  end;
//  if not OK then
//  begin
//    frmErrorsAndWarnings.AddError(Model, Format(StrInvalidS, [IUZFBND.DisplayName]),
//      Format(StrWhenTheUZFPackage, [IUZFBND.DisplayName]));
//  end;
  WriteArray(IUZFBND, 0, 'Data Set 2: IUZFBND', StrNoValueAssigned, 'IUZFBND');
end;

procedure TModflowUzfWriter.WriteDataSet3;
var
  IRUNBND: TDataArray;
begin
  if IRUNFLG > 0 then
  begin
    // IRUNBND is automatically renumbered in TCustomModel.UpdateDischargeRouting.
    IRUNBND := Model.DataArrayManager.GetDataSetByName(StrUzfDischargeRouting);
    WriteArray(IRUNBND, 0, 'Data Set 3: IRUNBND', StrNoValueAssigned, 'IRUNBND');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet4;
var
  VKS: TDataArray;
begin
  if IUZFOPT = 1 then
  begin
    VKS := Model.DataArrayManager.GetDataSetByName(StrUzfVerticalK);
    WriteArray(VKS, 0, 'Data Set 4: VKS', StrNoValueAssigned, 'VKS');
    WritePestZones(VKS, FInputFileName, StrVKS, 'VKS');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet4B;
var
  SURFK: TDataArray;
begin
  if {(IUZFOPT = 1) and} (Model.NWT_Format = nf1_1)
    and UzfPackage.SpecifySurfaceK then
  begin
    SURFK := Model.DataArrayManager.GetDataSetByName(StrUzfSurfaceK);
    WriteArray(SURFK, 0, 'Data Set 4b: SURFK', StrNoValueAssigned, 'SURFK');
    WritePestZones(SURFK, FInputFileName, StrSURF, 'SRK');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet5;
var
  EPS: TDataArray;
begin
  EPS := Model.DataArrayManager.GetDataSetByName(StrUzfBrooksCoreyEpsilon);
  WriteArray(EPS, 0, 'Data Set 5: EPS', StrNoValueAssigned, 'EPS');
  WritePestZones(EPS, FInputFileName, StrEPS, 'EPS');
end;

procedure TModflowUzfWriter.WriteDataSet6a;
var
  THTS: TDataArray;
begin
  THTS := Model.DataArrayManager.GetDataSetByName(StrUzfSaturatedWaterContent);
  if Model.ModelSelection in [msModflowNWT, msModflowLGR2, msModflow,
    msModflowFmp {, msModflowCfp}] then
  begin
    WriteArray(THTS, 0, 'Data Set 6a: THTS', StrNoValueAssigned, 'THTS');
  end
  else
  begin
    WriteArray(THTS, 0, 'Data Set 6: THTS', StrNoValueAssigned, 'THTS');
  end;
  WritePestZones(THTS, FInputFileName, StrTHTS, 'THS');
end;

procedure TModflowUzfWriter.WriteDataSet6b;
var
  THTR: TDataArray;
begin
  if (Model.ModelSelection in [msModflowNWT, msModflowLGR2, msModflow,
    msModflowFmp {, msModflowCfp} ] )
    and Model.ModflowPackages.UzfPackage.SpecifyResidualWaterContent then
  begin
    THTR := Model.DataArrayManager.GetDataSetByName(StrUzfReisidualWaterContent);
    WriteArray(THTR, 0, 'Data Set 6b: THTR', StrNoValueAssigned, 'THTR');
    WritePestZones(THTR, FInputFileName, StrTHTR, 'THR');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet7;
var
  THTI: TDataArray;
begin
  if Model.ModflowStressPeriods.CompletelyTransient
    or ((Model.ModelSelection in [msModflowNWT, msModflowLGR2, msModflow,
      msModflowFmp {, msModflowCfp}])
      and Model.ModflowPackages.UzfPackage.SpecifyInitialWaterContent) then
  begin
    THTI := Model.DataArrayManager.GetDataSetByName(StrUzfInitialUnsaturatedWaterContent);
    WriteArray(THTI, 0, 'Data Set 7: THTI', StrNoValueAssigned, 'THTI');
    WritePestZones(THTI, FInputFileName, StrTHTI, 'THI');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet8(GageStart: integer);
var
  GageArray: TDataArray;
  ColIndex: integer;
  RowIndex: integer;
begin
  if NUZGAG > 0 then
  begin
    if Model.ModflowPackages.UzfPackage.PrintSummary <> 0 then
    begin
      WriteInteger(-GageStart);
      Inc(GageStart);
      WriteString(' # Data Set 8: IFTUNIT');
      NewLine;
    end;
  end;

  GageArray := Model.DataArrayManager.GetDataSetByName(StrUzfGage_1_and_2);
  GageArray.Initialize;
  for RowIndex := 0 to Model.ModflowGrid.RowCount -1 do
  begin
    for ColIndex := 0 to Model.ModflowGrid.ColumnCount -1 do
    begin
      if GageArray.IntegerData[0,RowIndex,ColIndex] <> 0 then
      begin
        WriteInteger(RowIndex+1);
        WriteInteger(ColIndex+1);
        WriteInteger(GageStart);
        Inc(GageStart);
        WriteInteger(GageArray.IntegerData[0,RowIndex,ColIndex]);
        WriteString(' # Data Set 8: IUZROW IUZCOL IFTUNIT IUZOPT');
        NewLine;
      end;
    end;
  end;

  GageArray := Model.DataArrayManager.GetDataSetByName(StrUzfGage3);
  GageArray.Initialize;
  for RowIndex := 0 to Model.ModflowGrid.RowCount -1 do
  begin
    for ColIndex := 0 to Model.ModflowGrid.ColumnCount -1 do
    begin
      if GageArray.IntegerData[0,RowIndex,ColIndex] <> 0 then
      begin
        WriteInteger(RowIndex+1);
        WriteInteger(ColIndex+1);
        WriteInteger(GageStart);
        Inc(GageStart);
        WriteInteger(GageArray.IntegerData[0,RowIndex,ColIndex]);
        WriteString(' # Data Set 8: IUZROW IUZCOL IFTUNIT IUZOPT');
        NewLine;
      end;
    end;
  end;
end;

procedure TModflowUzfWriter.WriteFile(const AFileName: string);
var
  GageStart: integer;
  IUZFBND: TDataArray;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrErrorInUZFOptions);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrUZF) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingUZFPackage);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUnspecifiedUZFData);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheInfiltrationRat);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETDemandRateI);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETExtinctionDe);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETExtinctionWa);
  IUZFBND := Model.DataArrayManager.GetDataSetByName(StrUzfLayer);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, Format(StrWhenTheUZFPackage, [IUZFBND.DisplayName]));

  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(StrUZF, Model.UnitNumbers.UnitNumber(StrUZF), FNameOfFile, foInput, Model);
  WriteGagesToNameFile(AFileName, GageStart);
  WriteFileInternal(GageStart);

  if  Model.PestUsed and FPestParamUsed then
  begin
    frmErrorsAndWarnings.BeginUpdate;
    try
      FNameOfFile := FNameOfFile + '.tpl';
      WritePestTemplateLine(FNameOfFile);
      WritingTemplate := True;
      WriteFileInternal(GageStart);

    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  end;

end;

procedure TModflowUzfWriter.WriteInfiltrationRates(CellList: TList);
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
  Comment := '# Data Set 10: FINF';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, Model.ModflowPackages.UzfPackage.AssignmentMethod,
    True, Dummy, 'FINF');
end;


procedure TModflowUzfWriter.WriteStressPeriods;
var
  TimeIndex: Integer;
  CellList: TList;
begin
  if Values.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrUnspecifiedUZFData,
      StrNoTransientDataI);
  end;
  for TimeIndex := 0 to Values.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage(Format(StrWritingStressP, [TimeIndex+1]));

    // data set 9
    WriteInteger(0);
    WriteString(' # Data Set 9, Stress Period ');
    WriteInteger(TimeIndex + 1);
    WriteString(': NUZF1');
    NewLine;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Data Set 10
    if TimeIndex < Values.Count then
    begin
      CellList := Values[TimeIndex];
      WriteInfiltrationRates(CellList);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model,
        StrTheInfiltrationRat, IntToStr(TimeIndex+1));
    end;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if (Model as TCustomModel).ModflowPackages.UzfPackage.SimulateET then
    begin
      // data set 11
      WriteInteger(0);
      WriteString(' # Data Set 11, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF2');
      NewLine;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data Set 12
      if TimeIndex < FEtDemand.Count then
      begin
        CellList := FEtDemand[TimeIndex];
        WritePotentialEtRates(CellList);
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETDemandRateI, IntToStr(TimeIndex+1));
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // data set 13
      WriteInteger(0);
      WriteString(' # Data Set 13, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF3');
      NewLine;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data Set 14
      if TimeIndex < FEExtinctionDepths.Count then
      begin
        CellList := FEExtinctionDepths[TimeIndex];
        WriteExtinctionDepth(CellList);
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETExtinctionDe, IntToStr(TimeIndex+1));
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // data set 15
      WriteInteger(0);
      WriteString(' # Data Set 5, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF4');
      NewLine;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data Set 16
      if TimeIndex < FExtinctionWaterContent.Count then
      begin
        CellList := FExtinctionWaterContent[TimeIndex];
        WriteExtinctionWaterContent(CellList);
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETExtinctionWa, IntToStr(TimeIndex+1));
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TModflowUzfWriter.CountGages;
var
  RowIndex: Integer;
  GageArray: TDataArray;
  ColIndex: Integer;
begin
  NUZGAG := 0;

  GageArray := Model.DataArrayManager.GetDataSetByName(StrUzfGage_1_and_2);
  GageArray.Initialize;
  for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
  begin
    for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
    begin
      if GageArray.IntegerData[0, RowIndex, ColIndex] <> 0 then
      begin
        Inc(NUZGAG);
      end;
    end;
  end;

  GageArray := Model.DataArrayManager.GetDataSetByName(StrUzfGage3);
  GageArray.Initialize;
  for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
  begin
    for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
    begin
      if GageArray.IntegerData[0, RowIndex, ColIndex] <> 0 then
      begin
        Inc(NUZGAG);
      end;
    end;
  end;

  if Model.ModflowPackages.UzfPackage.PrintSummary <> 0 then
  begin
    Inc(NUZGAG);
  end;
end;

end.
