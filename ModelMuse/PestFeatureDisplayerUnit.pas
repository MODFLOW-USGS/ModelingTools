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
  GIS_Functions, System.Contnrs, AbstractGridUnit, Vcl.Dialogs;

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
  CSubStressOffsestValueArrayItem: TValueArrayItem;
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
  function CreateValueArrayItem(
    DataArray: TModflowBoundaryDisplayDataArray): TValueArrayItem;
  begin
    Position := AScreenObject.AddDataSet(DataArray);
    result := AScreenObject.ImportedValues.Add;
    result.Name := 'Imported_' + DataArray.Name;
    result.Values.DataType := rdtDouble;
    result.Values.Count := FFeatures.Count;
    AScreenObject.DataSetFormulas[Position]
      := rsObjectImportedValuesR + '("' + result.Name + '")';
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
    CSubStressOffsestValueArrayItem := nil;

    AFeature := nil;
    try
      AScreenObject :=
        TScreenObject.CreateWithViewDirection(
        frmGoPhast.PhastModel, vdTop,
        UndoCreateScreenObject, False);
      AScreenObject.Comment := 'Imported from ' + FileName +' on ' + DateTimeToStr(Now);
      AScreenObject.Name := Format('%0:s_%1:d', [Root, ExistingObjectCount+1]);
      AScreenObject.SetValuesOfEnclosedCells := False;
      AScreenObject.SetValuesOfIntersectedCells := True;
      AScreenObject.SetValuesByInterpolation := False;
      AScreenObject.ElevationCount := ecOne;
      AScreenObject.Capacity := FFeatures.Count;
      AScreenObject.EvaluatedAt := eaBlocks;
      AScreenObject.ElevationFormula := rsObjectImportedValuesR
        + '("' + StrImportedElevations + '")';
      ScreenObjectList.Add(AScreenObject);
      case FFeatureType of
        m6ftChd:
          begin
            HeadValueArrayItem := CreateValueArrayItem(HeadDataArray);
          end;
        m6ftWell:
          begin
            WelValueArrayItem := CreateValueArrayItem(WellDataArray);
          end;
        m6ftDrn:
          begin
            DrnElevValueArrayItem := CreateValueArrayItem(DrnElevDataArray);
            DrnCondValueArrayItem := CreateValueArrayItem(DrnCondDataArray);
          end;
        m6ftRiv:
          begin
            RivStageValueArrayItem := CreateValueArrayItem(RivStageDataArray);
            RivCondValueArrayItem := CreateValueArrayItem(RivCondDataArray);
            RivRBotValueArrayItem := CreateValueArrayItem(RivRBotDataArray);
          end;
        m6ftGhb:
          begin
            GhbHeadValueArrayItem := CreateValueArrayItem(GhbHeadDataArray);
            GhbCondValueArrayItem := CreateValueArrayItem(GhbCondDataArray);
          end;
        m6ftRch:
          begin
            RchRechargeValueArrayItem := CreateValueArrayItem(RechargeDataArray);
          end;
        m6ftEvt:
          begin
            EvtSurfaceValueArrayItem := CreateValueArrayItem(EvtSurfaceDataArray);
            EvtRateValueArrayItem := CreateValueArrayItem(EvtRateDataArray);
            EvtDepthValueArrayItem := CreateValueArrayItem(EvtDepthDataArray);
          end;
        m6ftCSub:
          begin
            CSubStressOffsestValueArrayItem := CreateValueArrayItem(CSubStressOffsetDataArray);
          end;
        else
          Assert(False);
      end;

      ImportedSectionElevations := AScreenObject.ImportedSectionElevations;
      ImportedSectionElevations.Count := FFeatures.Count;

      for FeatureIndex := 0 to FFeatures.Count - 1 do
      begin
        AFeature := FFeatures[FeatureIndex];
        Assert(LocalModel.ModelSelection in ModflowSelection);
        ALocation := LocalModel.CellToPoint(AFeature.Cell, eaBlocks);
        APoint.x := ALocation.x;
        APoint.y := ALocation.y;
        AScreenObject.AddPoint(APoint, True);
        ImportedSectionElevations.RealValues[FeatureIndex] := ALocation.Z;
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
              CSubStressOffsestValueArrayItem.Values.RealValues[FeatureIndex]
                := (AFeature as TCSubFeature).Sig0;
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
