unit PestFeatureDisplayerUnit;

interface

uses Winapi.Windows, System.UITypes, Modflow6Importer, GoPhastTypes,
  frmImportShapefileUnit, System.Classes, System.SysUtils;

type
  TUndoImportPestModelFeatureDisplay = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TPestFeatureDisplayer = class(TObject)
  private
    FModel: TBaseModel;
    FFeatures: TModflowFeatureList;
    FFeatureType: TModflow6FeatureType;
  public
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure ImportFeatures(const FileName: string; GridType: TModflow6GridType;
      StressPeriod: integer);
    property FeatureType: TModflow6FeatureType read FFeatureType;
  end;

implementation

uses
  ModflowBoundaryDisplayUnit, PhastModelUnit, ScreenObjectUnit, frmGoPhastUnit,
  FastGEO, ValueArrayStorageUnit, DataSetUnit, RbwParser, UndoItems,
  GIS_Functions, System.Contnrs, AbstractGridUnit, Vcl.Dialogs, ModflowMawUnit;

resourcestring
  StrAnErrorOccurredWh = 'An error occurred while importing the file. Check ' +
  'that the file is a valid MODFLOW 6 file.';
  StrAFileContainedAFMesh = 'A file contained a features that was outside th' +
  'e mesh. The cell locations was (Layer: %0:d, Number: %1:d).';
  StrAFileContainedAFGrid = 'A file contained a features that was outside th' +
  'e grid. The cell locations was (Layer: %0:d, Row: %1:d, Column: %2:d).';


{ TPestFeatureDisplayer }

constructor TPestFeatureDisplayer.Create(Model: TBaseModel);
begin
  FModel := Model;
end;

destructor TPestFeatureDisplayer.Destroy;
begin
  FFeatures.Free;
  inherited;
end;

procedure TPestFeatureDisplayer.ImportFeatures(const FileName: string;
  GridType: TModflow6GridType; StressPeriod: integer);
var
  HeadDataArray: TModflowBoundaryDisplayDataArray;
  NewDataSets: TList;
  FeatureReader: TModflow6FileReader;
  InvalidNames: TStringList;
  Undo: TUndoImportPestModelFeatureDisplay;
  LocalModel: TCustomModel;
  WellDataArray: TModflowBoundaryDisplayDataArray;
  FeatureIndex: Integer;
  AFeature: TModflow6Feature;
  Root: string;
  AScreenObject: TScreenObject;
  ExistingObjectCount: Integer;
  ScreenObjectList: TList;
  Position: Integer;
  WelValueArrayItem: TValueArrayItem;
  HeadValueArrayItem: TValueArrayItem;
  ALocation: TPoint3D;
  UndoCreateScreenObject: TCustomUndo;
  ImportedSectionElevations: TValueArrayStorage;
  APoint: TPoint2D;
  DrnElevDataArray: TModflowBoundaryDisplayDataArray;
  DrnCondDataArray: TModflowBoundaryDisplayDataArray;
  DrnElevValueArrayItem: TValueArrayItem;
  DrnCondValueArrayItem: TValueArrayItem;
  RivStageDataArray: TModflowBoundaryDisplayDataArray;
  RivCondDataArray: TModflowBoundaryDisplayDataArray;
  RivRBotDataArray: TModflowBoundaryDisplayDataArray;
  RivStageValueArrayItem: TValueArrayItem;
  RivCondValueArrayItem: TValueArrayItem;
  RivRBotValueArrayItem: TValueArrayItem;
  GhbHeadDataArray: TModflowBoundaryDisplayDataArray;
  GhbCondDataArray: TModflowBoundaryDisplayDataArray;
  GhbHeadValueArrayItem: TValueArrayItem;
  GhbCondValueArrayItem: TValueArrayItem;
  RechargeDataArray: TModflowBoundaryDisplayDataArray;
  RchRechargeValueArrayItem: TValueArrayItem;
  EvtSurfaceDataArray: TModflowBoundaryDisplayDataArray;
  EvtRateDataArray: TModflowBoundaryDisplayDataArray;
  EvtDepthDataArray: TModflowBoundaryDisplayDataArray;
  EvtSurfaceValueArrayItem: TValueArrayItem;
  EvtRateValueArrayItem: TValueArrayItem;
  EvtDepthValueArrayItem: TValueArrayItem;
  CSubStressOffsetDataArray: TModflowBoundaryDisplayDataArray;
  CSubStressOffsetValueArrayItem: TValueArrayItem;
  MawFeature: TMawFeature;
  MawRateDataArray: TModflowBoundaryDisplayDataArray;
  MawFwelevDataArray: TModflowBoundaryDisplayDataArray;
  MawFwcondDataArray: TModflowBoundaryDisplayDataArray;
  MawFwrlenDataArray: TModflowBoundaryDisplayDataArray;
  MawWell_HeadDataArray: TModflowBoundaryDisplayDataArray;
  MawHead_limitDataArray: TModflowBoundaryDisplayDataArray;
  MawMinRateDataArray: TModflowBoundaryDisplayDataArray;
  MawMaxRateDataArray: TModflowBoundaryDisplayDataArray;
  MawPumpElevationDataArray: TModflowBoundaryDisplayDataArray;
  MawScalingLengthDataArray: TModflowBoundaryDisplayDataArray;
  MawRateScreenObject: TScreenObject;
  MawFlowingWellScreenObject: TScreenObject;
  MawWellHeadScreenObject: TScreenObject;
  MawHeadLimitScreenObject: TScreenObject;
  MawShutOffScreenObject: TScreenObject;
  MawRateScalingScreenObject: TScreenObject;
  MawRateValueArrayItem: TValueArrayItem;
  MawFwelevValueArrayItem: TValueArrayItem;
  MawFwcondValueArrayItem: TValueArrayItem;
  MawFwrlenValueArrayItem: TValueArrayItem;
  MawWell_HeadValueArrayItem: TValueArrayItem;
  MawHead_limitValueArrayItem: TValueArrayItem;
  MawMinRateValueArrayItem: TValueArrayItem;
  MawMaxRateValueArrayItem: TValueArrayItem;
  MawPumpElevationValueArrayItem: TValueArrayItem;
  MawScalingLengthValueArrayItem: TValueArrayItem;
  MawRateCount: Integer;
  MawFlowingWellCount: Integer;
  MawConstantHeadCount: Integer;
  MawHeadLimitCount: Integer;
  MawShutoffCount: Integer;
  MawRateScalingCount: Integer;
  ImportedMawRateSectionElevations: TValueArrayStorage;
  ImportedMawFlowingWellSectionElevations: TValueArrayStorage;
  ImportedMawConstantHeadSectionElevations: TValueArrayStorage;
  ImportedMawHeadLimitSectionElevations: TValueArrayStorage;
  ImportedMawShutoffSectionElevations: TValueArrayStorage;
  ImportedMawRateScalingSectionElevations: TValueArrayStorage;
  function CreateDataSet(const Root: string;
    Method: TValueAddMethod): TModflowBoundaryDisplayDataArray;
  begin
    result := TModflowBoundaryDisplayDataArray.Create(LocalModel);
    result.Orientation := dso3D;
    result.EvaluatedAt := eaBlocks;
    result.AddMethod := vamAveragedDelayed;
    result.Name := GenerateNewName(Format('%0:s_SP_%1:d',
      [Root, StressPeriod]), InvalidNames, '_');
    result.Formula := '0';
    (result.Limits.RealValuesToSkip.Add as TSkipReal).RealValue := 0;
    NewDataSets.Add(result);
    LocalModel.UpdateDataArrayDimensions(result);
  end;
  function CreateValueArrayItem(ScreenObject: TScreenObject;
    DataArray: TModflowBoundaryDisplayDataArray): TValueArrayItem;
  begin
    Position := ScreenObject.AddDataSet(DataArray);
    result := ScreenObject.ImportedValues.Add;
    result.Name := 'Imported_' + DataArray.Name;
    result.Values.DataType := rdtDouble;
    result.Values.Count := FFeatures.Count;
    ScreenObject.DataSetFormulas[Position]
      := rsObjectImportedValuesR + '("' + result.Name + '")';
  end;
  function MakeNewScreenObject: TScreenObject;
  begin
    result :=
      TScreenObject.CreateWithViewDirection(
      frmGoPhast.PhastModel, vdTop,
      UndoCreateScreenObject, False);
    result.Comment := 'Imported from ' + FileName +' on ' + DateTimeToStr(Now);
    result.SetValuesOfEnclosedCells := False;
    result.SetValuesOfIntersectedCells := True;
    result.SetValuesByInterpolation := False;
    result.ElevationCount := ecOne;
    result.Capacity := FFeatures.Count;
    result.EvaluatedAt := eaBlocks;
    result.ElevationFormula := rsObjectImportedValuesR
      + '("' + StrImportedElevations + '")';
  end;
begin
  FeatureReader := TModflow6FileReader.Create(GridType);
  try
    FeatureReader.OpenFile(FileName);
    FFeatures := FeatureReader.ReadStressPeriod(StressPeriod);
    FFeatureType := FeatureReader.FeatureType;
  finally
    FeatureReader.Free;
  end;

  if FFeatures.Count = 0 then
  begin
    Exit;
  end;

  HeadDataArray := nil;
  WellDataArray := nil;
  DrnElevDataArray := nil;
  DrnCondDataArray := nil;
  RivStageDataArray := nil;
  RivCondDataArray := nil;
  RivRBotDataArray := nil;
  GhbHeadDataArray := nil;
  GhbCondDataArray := nil;
  RechargeDataArray := nil;
  EvtSurfaceDataArray := nil;
  EvtRateDataArray := nil;
  EvtDepthDataArray := nil;
  CSubStressOffsetDataArray := nil;
  MawRateDataArray := nil;
  MawFwelevDataArray := nil;
  MawFwcondDataArray := nil;
  MawFwrlenDataArray := nil;
  MawWell_HeadDataArray := nil;
  MawHead_limitDataArray := nil;
  MawMinRateDataArray := nil;
  MawMaxRateDataArray := nil;
  MawPumpElevationDataArray := nil;
  MawScalingLengthDataArray := nil;

  MawRateCount := 0;
  MawFlowingWellCount := 0;
  MawConstantHeadCount := 0;
  MawHeadLimitCount := 0;
  MawShutoffCount := 0;
  MawRateScalingCount := 0;

  LocalModel := FModel as TCustomModel;
  InvalidNames:= TStringList.Create;
  NewDataSets := TList.Create;
  ScreenObjectList := TObjectList.Create;
  Undo := TUndoImportPestModelFeatureDisplay.Create;
  try
    case FFeatureType of
      m6ftChd:
        begin
          HeadDataArray := CreateDataSet('CHD_Head', vamAveragedDelayed);
          Root := 'CHD';
        end;
      m6ftWell:
        begin
          WellDataArray := CreateDataSet('WELL_Pumping_Rate', vamAddDelayed);
          Root := 'WEL';
        end;
      m6ftDrn:
        begin
          DrnElevDataArray := CreateDataSet('DRN_Elevation', vamAveragedDelayed);
          DrnCondDataArray := CreateDataSet('DRN_Conductance', vamAveragedDelayed);
          Root := 'DRN';
        end;
      m6ftRiv:
        begin
          RivStageDataArray := CreateDataSet('RIV_Stage', vamAveragedDelayed);
          RivCondDataArray := CreateDataSet('RIV_Conductance', vamAveragedDelayed);
          RivRBotDataArray := CreateDataSet('RIV_Bottom', vamAveragedDelayed);
          Root := 'RIV';
        end;
      m6ftGhb:
        begin
          GhbHeadDataArray := CreateDataSet('GHB_Head', vamAveragedDelayed);
          GhbCondDataArray := CreateDataSet('GHB_Conductance', vamAveragedDelayed);
          Root := 'GHB';
        end;
      m6ftRch:
        begin
          RechargeDataArray := CreateDataSet('RCH_Recharge', vamAddDelayed);
          Root := 'RCH';
        end;
      m6ftEvt:
        begin
          EvtSurfaceDataArray := CreateDataSet('EVT_Surface', vamAveragedDelayed);
          EvtRateDataArray := CreateDataSet('EVT_Rate', vamAddDelayed);
          EvtDepthDataArray := CreateDataSet('EVT_Depth', vamAveragedDelayed);
          Root := 'EVT';
        end;
      m6ftCSub:
        begin
          CSubStressOffsetDataArray := CreateDataSet('CSUB_Stress_Offset', vamAveragedDelayed);
          Root := 'CSUB';
        end;
      m6ftMaw:
        begin
          for FeatureIndex := 0 to FFeatures.Count - 1 do
          begin
            MawFeature := FFeatures[FeatureIndex] as TMawFeature;
            if MawFeature.MawProperties.Status = mwInactive then
            begin
              Continue;
            end;

            Inc(MawRateCount);
            if MawRateDataArray = nil then
            begin
              MawRateDataArray := CreateDataSet('MAW_Rate', vamAddDelayed);
            end;

            if MawFeature.MawProperties.FlowingWell then
            begin
              Inc(MawFlowingWellCount);
              if MawFwelevDataArray = nil then
              begin
                MawFwelevDataArray := CreateDataSet('MAW_FlowingWellElevation', vamAveragedDelayed);
                MawFwcondDataArray := CreateDataSet('MAW_FlowingWellConductance', vamAveragedDelayed);
                MawFwrlenDataArray := CreateDataSet('MAW_FlowingWellReductionLength', vamAveragedDelayed);
              end;
            end;

            if MawFeature.MawProperties.Status = mwConstantHead then
            begin
              Inc(MawConstantHeadCount);
              if MawWell_HeadDataArray = nil then
              begin
                MawWell_HeadDataArray := CreateDataSet('MAW_WellHead', vamAveragedDelayed);
              end;
            end;

            if MawFeature.MawProperties.HeadLimitUsed then
            begin
              Inc(MawHeadLimitCount);
              if MawHead_limitDataArray = nil then
              begin
                MawHead_limitDataArray := CreateDataSet('MAW_Head_limit', vamAveragedDelayed);
              end;
            end;

            if MawFeature.MawProperties.ShutOff then
            begin
              Inc(MawShutoffCount);
              if MawMinRateDataArray = nil then
              begin
                MawMinRateDataArray := CreateDataSet('MAW_MinRate', vamAveragedDelayed);
                MawMaxRateDataArray := CreateDataSet('MAW_MaxRate', vamAveragedDelayed);
              end;
            end;

            if MawFeature.MawProperties.RateScaling then
            begin
              Inc(MawRateScalingCount);
              if MawPumpElevationDataArray = nil then
              begin
                MawPumpElevationDataArray := CreateDataSet('MAW_PumpElevation', vamAveragedDelayed);
                MawScalingLengthDataArray := CreateDataSet('MAW_ScalingLength', vamAveragedDelayed);
              end;
            end;
          end;
          Root := 'MAW';
        end;
      else
        Assert(False);
    end;

    ExistingObjectCount :=
      frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

    HeadValueArrayItem := nil;
    WelValueArrayItem := nil;
    DrnElevValueArrayItem := nil;
    DrnCondValueArrayItem := nil;
    RivStageValueArrayItem := nil;
    RivCondValueArrayItem := nil;
    RivRBotValueArrayItem := nil;
    GhbHeadValueArrayItem := nil;
    GhbCondValueArrayItem := nil;
    RchRechargeValueArrayItem := nil;
    EvtSurfaceValueArrayItem := nil;
    EvtRateValueArrayItem := nil;
    EvtDepthValueArrayItem := nil;
    CSubStressOffsetValueArrayItem := nil;
    MawRateValueArrayItem := nil;
    MawFwelevValueArrayItem := nil;
    MawFwcondValueArrayItem := nil;
    MawFwrlenValueArrayItem := nil;
    MawWell_HeadValueArrayItem := nil;
    MawHead_limitValueArrayItem := nil;
    MawMinRateValueArrayItem := nil;
    MawMaxRateValueArrayItem := nil;
    MawPumpElevationValueArrayItem := nil;
    MawScalingLengthValueArrayItem := nil;

    AFeature := nil;
    try
      AScreenObject := nil;
      MawRateScreenObject := nil;
      MawFlowingWellScreenObject := nil;
      MawWellHeadScreenObject := nil;
      MawHeadLimitScreenObject := nil;
      MawShutOffScreenObject := nil;
      MawRateScalingScreenObject := nil;

      if FFeatureType = m6ftMaw then
      begin
        if MawRateDataArray <> nil then
        begin
          MawRateScreenObject := MakeNewScreenObject;
          MawRateScreenObject.Name := Format('%0:s_%1:d', [Root, ExistingObjectCount+1]);
          ScreenObjectList.Add(MawRateScreenObject);
          Inc(ExistingObjectCount);
        end;
        if MawFwelevDataArray <> nil then
        begin
          MawFlowingWellScreenObject := MakeNewScreenObject;
          MawFlowingWellScreenObject.Name := Format('%0:s_%1:d', [Root, ExistingObjectCount+1]);
          ScreenObjectList.Add(MawFlowingWellScreenObject);
          Inc(ExistingObjectCount);
        end;
        if MawWell_HeadDataArray <> nil then
        begin
          MawWellHeadScreenObject := MakeNewScreenObject;
          MawWellHeadScreenObject.Name := Format('%0:s_%1:d', [Root, ExistingObjectCount+1]);
          ScreenObjectList.Add(MawWellHeadScreenObject);
          Inc(ExistingObjectCount);
        end;
        if MawHead_limitDataArray <> nil then
        begin
          MawHeadLimitScreenObject := MakeNewScreenObject;
          MawHeadLimitScreenObject.Name := Format('%0:s_%1:d', [Root, ExistingObjectCount+1]);
          ScreenObjectList.Add(MawHeadLimitScreenObject);
          Inc(ExistingObjectCount);
        end;
        if MawMinRateDataArray <> nil then
        begin
          MawShutOffScreenObject := MakeNewScreenObject;
          MawShutOffScreenObject.Name := Format('%0:s_%1:d', [Root, ExistingObjectCount+1]);
          ScreenObjectList.Add(MawShutOffScreenObject);
          Inc(ExistingObjectCount);
        end;
        if MawPumpElevationDataArray <> nil then
        begin
          MawRateScalingScreenObject := MakeNewScreenObject;
          MawRateScalingScreenObject.Name := Format('%0:s_%1:d', [Root, ExistingObjectCount+1]);
          ScreenObjectList.Add(MawRateScalingScreenObject);
//          Inc(ExistingObjectCount);
        end;
      end
      else
      begin
        AScreenObject := MakeNewScreenObject;
        AScreenObject.Name := Format('%0:s_%1:d', [Root, ExistingObjectCount+1]);
        ScreenObjectList.Add(AScreenObject);
      end;
      case FFeatureType of
        m6ftChd:
          begin
            HeadValueArrayItem := CreateValueArrayItem(AScreenObject,
              HeadDataArray);
          end;
        m6ftWell:
          begin
            WelValueArrayItem := CreateValueArrayItem(AScreenObject,
              WellDataArray);
          end;
        m6ftDrn:
          begin
            DrnElevValueArrayItem := CreateValueArrayItem(AScreenObject,
              DrnElevDataArray);
            DrnCondValueArrayItem := CreateValueArrayItem(AScreenObject,
              DrnCondDataArray);
          end;
        m6ftRiv:
          begin
            RivStageValueArrayItem := CreateValueArrayItem(AScreenObject,
              RivStageDataArray);
            RivCondValueArrayItem := CreateValueArrayItem(AScreenObject,
              RivCondDataArray);
            RivRBotValueArrayItem := CreateValueArrayItem(AScreenObject,
              RivRBotDataArray);
          end;
        m6ftGhb:
          begin
            GhbHeadValueArrayItem := CreateValueArrayItem(AScreenObject,
              GhbHeadDataArray);
            GhbCondValueArrayItem := CreateValueArrayItem(AScreenObject,
              GhbCondDataArray);
          end;
        m6ftRch:
          begin
            RchRechargeValueArrayItem := CreateValueArrayItem(AScreenObject,
              RechargeDataArray);
          end;
        m6ftEvt:
          begin
            EvtSurfaceValueArrayItem := CreateValueArrayItem(AScreenObject,
              EvtSurfaceDataArray);
            EvtRateValueArrayItem := CreateValueArrayItem(AScreenObject,
              EvtRateDataArray);
            EvtDepthValueArrayItem := CreateValueArrayItem(AScreenObject,
              EvtDepthDataArray);
          end;
        m6ftCSub:
          begin
            CSubStressOffsetValueArrayItem := CreateValueArrayItem(AScreenObject,
              CSubStressOffsetDataArray);
          end;
        m6ftMaw:
          begin
            if MawRateScreenObject <> nil then
            begin
              MawRateValueArrayItem := CreateValueArrayItem(MawRateScreenObject,
                MawRateDataArray);
            end;
            if MawFlowingWellScreenObject <> nil then
            begin
              MawFwelevValueArrayItem := CreateValueArrayItem(MawFlowingWellScreenObject,
                MawFwelevDataArray);
              MawFwcondValueArrayItem := CreateValueArrayItem(MawFlowingWellScreenObject,
                MawFwcondDataArray);
              MawFwrlenValueArrayItem := CreateValueArrayItem(MawFlowingWellScreenObject,
                MawFwrlenDataArray);
            end;
            if MawWellHeadScreenObject <> nil then
            begin
              MawWell_HeadValueArrayItem := CreateValueArrayItem(MawWellHeadScreenObject,
                MawWell_HeadDataArray);
            end;
            if MawHeadLimitScreenObject <> nil then
            begin
              MawHead_limitValueArrayItem := CreateValueArrayItem(MawHeadLimitScreenObject,
                MawHead_limitDataArray);
            end;
            if MawShutOffScreenObject <> nil then
            begin
              MawMinRateValueArrayItem := CreateValueArrayItem(MawShutOffScreenObject,
                MawMinRateDataArray);
              MawMaxRateValueArrayItem := CreateValueArrayItem(MawShutOffScreenObject,
                MawMaxRateDataArray);
            end;
            if MawRateScalingScreenObject <> nil then
            begin
              MawPumpElevationValueArrayItem := CreateValueArrayItem(MawRateScalingScreenObject,
                MawPumpElevationDataArray);
              MawScalingLengthValueArrayItem := CreateValueArrayItem(MawRateScalingScreenObject,
                MawScalingLengthDataArray);
            end;
          end
        else
          Assert(False);
      end;

      ImportedSectionElevations := nil;
      ImportedMawRateSectionElevations := nil;
      ImportedMawFlowingWellSectionElevations := nil;
      ImportedMawConstantHeadSectionElevations := nil;
      ImportedMawHeadLimitSectionElevations := nil;
      ImportedMawShutoffSectionElevations := nil;
      ImportedMawRateScalingSectionElevations := nil;
      if FFeatureType = m6ftMaw then
      begin
        if MawRateScreenObject <> nil then
        begin
          ImportedMawRateSectionElevations := MawRateScreenObject.ImportedSectionElevations;
          ImportedMawRateSectionElevations.Count := MawRateCount;
        end;
        if MawFlowingWellScreenObject <> nil then
        begin
          ImportedMawFlowingWellSectionElevations := MawFlowingWellScreenObject.ImportedSectionElevations;
          ImportedMawFlowingWellSectionElevations.Count := MawFlowingWellCount;
        end;
        if MawWellHeadScreenObject <> nil then
        begin
          ImportedMawConstantHeadSectionElevations := MawWellHeadScreenObject.ImportedSectionElevations;
          ImportedMawConstantHeadSectionElevations.Count := MawConstantHeadCount;
        end;
        if MawHeadLimitScreenObject <> nil then
        begin
          ImportedMawHeadLimitSectionElevations := MawHeadLimitScreenObject.ImportedSectionElevations;
          ImportedMawHeadLimitSectionElevations.Count := MawHeadLimitCount;
        end;
        if MawShutOffScreenObject <> nil then
        begin
          ImportedMawShutoffSectionElevations := MawShutOffScreenObject.ImportedSectionElevations;
          ImportedMawShutoffSectionElevations.Count := MawShutoffCount;
        end;
        if MawRateScalingScreenObject <> nil then
        begin
          ImportedMawRateScalingSectionElevations := MawRateScalingScreenObject.ImportedSectionElevations;
          ImportedMawRateScalingSectionElevations.Count := MawRateScalingCount;
        end;
      end
      else
      begin
        ImportedSectionElevations := AScreenObject.ImportedSectionElevations;
        ImportedSectionElevations.Count := FFeatures.Count;
      end;

      Assert(LocalModel.ModelSelection in ModflowSelection);

      MawRateCount := 0;
      MawFlowingWellCount := 0;
      MawConstantHeadCount := 0;
      MawHeadLimitCount := 0;
      MawShutoffCount := 0;
      MawRateScalingCount := 0;
      for FeatureIndex := 0 to FFeatures.Count - 1 do
      begin
        AFeature := FFeatures[FeatureIndex];
        ALocation := LocalModel.CellToPoint(AFeature.Cell, eaBlocks);
        APoint.x := ALocation.x;
        APoint.y := ALocation.y;
        if FFeatureType = m6ftMaw then
        begin
        end
        else
        begin
          AScreenObject.AddPoint(APoint, True);
          ImportedSectionElevations.RealValues[FeatureIndex] := ALocation.Z;
        end;
        case FFeatureType of
          m6ftChd:
            begin
              HeadValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TChdFeature).Head;
            end;
          m6ftWell:
            begin
              WelValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TWellFeature).Q;
            end;
          m6ftDrn:
            begin
              DrnElevValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TDrnFeature).Elev;
              DrnCondValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TDrnFeature).Cond;
            end;
          m6ftRiv:
            begin
              RivStageValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TRivFeature).Stage;
              RivCondValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TRivFeature).Cond;
              RivRBotValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TRivFeature).RBot;
            end;
          m6ftGhb:
            begin
              GhbHeadValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TGhbFeature).Head;
              GhbCondValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TGhbFeature).Cond;
            end;
          m6ftRch:
            begin
              RchRechargeValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TRchFeature).Recharge;
            end;
          m6ftEvt:
            begin
              EvtSurfaceValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TEvtFeature).Surface;
              EvtRateValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TEvtFeature).Rate;
              EvtDepthValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TEvtFeature).Depth;
            end;
          m6ftCSub:
            begin
              CSubStressOffsetValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TCSubFeature).Sig0;
            end;
          m6ftMAW:
            begin
              MawFeature := AFeature as TMawFeature;
              if MawFeature.MawProperties.Status = mwInactive then
              begin
                Continue;
              end;

              MawRateScreenObject.AddPoint(APoint, True);
              ImportedMawRateSectionElevations.RealValues
                [MawRateCount] := ALocation.Z;
              MawRateValueArrayItem.Values.RealValues[MawRateCount]
                := MawFeature.MawProperties.Rate;
              Inc(MawRateCount);

              if MawFeature.MawProperties.FlowingWell then
              begin
                MawFlowingWellScreenObject.AddPoint(APoint, True);
                ImportedMawFlowingWellSectionElevations.RealValues
                  [MawFlowingWellCount] := ALocation.Z;
                MawFwelevValueArrayItem.Values.RealValues[MawFlowingWellCount]
                  := MawFeature.MawProperties.Fwelev;
                MawFwcondValueArrayItem.Values.RealValues[MawFlowingWellCount]
                  := MawFeature.MawProperties.Fwcond;
                MawFwrlenValueArrayItem.Values.RealValues[MawFlowingWellCount]
                  := MawFeature.MawProperties.Fwrlen;
                Inc(MawFlowingWellCount);
              end;

              if MawFeature.MawProperties.Status = mwConstantHead then
              begin
                MawWellHeadScreenObject.AddPoint(APoint, True);
                ImportedMawConstantHeadSectionElevations.RealValues
                  [MawConstantHeadCount] := ALocation.Z;
                MawWell_HeadValueArrayItem.Values.RealValues[MawConstantHeadCount]
                  := MawFeature.MawProperties.Well_Head;
                Inc(MawConstantHeadCount);
              end;

              if MawFeature.MawProperties.HeadLimitUsed then
              begin
                MawHeadLimitScreenObject.AddPoint(APoint, True);
                ImportedMawHeadLimitSectionElevations.RealValues
                  [MawHeadLimitCount] := ALocation.Z;
                MawHead_limitValueArrayItem.Values.RealValues[MawHeadLimitCount]
                  := MawFeature.MawProperties.Head_limit;
                Inc(MawHeadLimitCount);
              end;

              if MawFeature.MawProperties.ShutOff then
              begin
                MawShutOffScreenObject.AddPoint(APoint, True);
                ImportedMawShutoffSectionElevations.RealValues
                  [MawShutoffCount] := ALocation.Z;
                MawMinRateValueArrayItem.Values.RealValues[MawShutoffCount]
                  := MawFeature.MawProperties.MinRate;
                MawMaxRateValueArrayItem.Values.RealValues[MawShutoffCount]
                  := MawFeature.MawProperties.MaxRate;
                Inc(MawShutoffCount);
              end;

              if MawFeature.MawProperties.RateScaling then
              begin
                MawShutOffScreenObject.AddPoint(APoint, True);
                ImportedMawRateScalingSectionElevations.RealValues
                  [MawRateScalingCount] := ALocation.Z;
                MawPumpElevationValueArrayItem.Values.RealValues[MawRateScalingCount]
                  := MawFeature.MawProperties.PumpElevation;
                MawScalingLengthValueArrayItem.Values.RealValues[MawRateScalingCount]
                  := MawFeature.MawProperties.ScalingLength;
                Inc(MawRateScalingCount);
              end;
            end;
          else
            Assert(False);
        end;
      end;

      Undo.StoreNewScreenObjects(ScreenObjectList);
      Undo.StoreNewDataSets(NewDataSets);
      frmGoPhast.UndoStack.Submit(Undo);
      Undo := nil;
      (ScreenObjectList as TObjectList).OwnsObjects := False;
      frmGoPhast.PhastModel.AddFileToArchive(FileName);
    except on E: EInvalidGrid do
      begin
        Beep;
        if AFeature <> nil then
        begin
          if LocalModel.DisvUsed then
          begin
            MessageDlg(Format(StrAFileContainedAFMesh,
              [AFeature.Cell.Layer, AFeature.Cell.Column]), mtError, [mbOK], 0);
          end
          else
          begin
            MessageDlg(Format(StrAFileContainedAFGrid,
              [AFeature.Cell.Layer, AFeature.Cell.Row, AFeature.Cell.Column]),
              mtError, [mbOK], 0);
          end;
        end
        else
        begin
          MessageDlg(StrAnErrorOccurredWh, mtError, [mbOK], 0);
        end;
      end;

    end;

  finally
    ScreenObjectList.Free;
    NewDataSets.Free;
    Undo.Free;
    InvalidNames.Free;
  end;

end;

{ TUndoImportPestModelFeatureDisplay }

function TUndoImportPestModelFeatureDisplay.Description: string;
begin
  result := 'import model feature data sets.';
end;

end.
