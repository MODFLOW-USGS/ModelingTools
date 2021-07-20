unit PestFeatureDisplayerUnit;

interface

uses Modflow6Importer, GoPhastTypes, frmImportShapefileUnit, System.Classes,
  System.SysUtils;

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
  GIS_Functions, System.Contnrs;


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

  LocalModel := FModel as TCustomModel;
  InvalidNames:= TStringList.Create;
  NewDataSets := TList.Create;
  ScreenObjectList := TObjectList.Create;
  Undo := TUndoImportPestModelFeatureDisplay.Create;
  try
    case FFeatureType of
      m6ftChd:
        begin
          HeadDataArray := TModflowBoundaryDisplayDataArray.Create(LocalModel);
          HeadDataArray.Orientation := dso3D;
          HeadDataArray.EvaluatedAt := eaBlocks;
          HeadDataArray.AddMethod := vamAveragedDelayed;
          HeadDataArray.Name := GenerateNewName(Format('CHD_Head_SP_%d',
            [StressPeriod]), InvalidNames, '_');
          NewDataSets.Add(HeadDataArray);
          LocalModel.UpdateDataArrayDimensions(HeadDataArray);
          Root := 'CHD';
        end;
      m6ftWell:
        begin
          WellDataArray := TModflowBoundaryDisplayDataArray.Create(LocalModel);
          WellDataArray.Orientation := dso3D;
          WellDataArray.EvaluatedAt := eaBlocks;
          WellDataArray.AddMethod := vamAddDelayed;
          WellDataArray.Name := GenerateNewName(Format('WELL_Pumping_Rate_SP_%d',
            [StressPeriod]), InvalidNames, '_');
          NewDataSets.Add(WellDataArray);
          LocalModel.UpdateDataArrayDimensions(WellDataArray);
          Root := 'WEL';
        end;
      else
        Assert(False);
    end;

    ExistingObjectCount :=
      frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

    HeadValueArrayItem := nil;
    WelValueArrayItem := nil;

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
          Position := AScreenObject.AddDataSet(HeadDataArray);
          HeadValueArrayItem := AScreenObject.ImportedValues.Add;
          HeadValueArrayItem.Name := 'Imported_' + HeadDataArray.Name;
          HeadValueArrayItem.Values.DataType := rdtDouble;
          HeadValueArrayItem.Values.Count := FFeatures.Count;
          AScreenObject.DataSetFormulas[Position]
            := rsObjectImportedValuesR + '("' + HeadValueArrayItem.Name + '")';
        end;
      m6ftWell:
        begin
          Position := AScreenObject.AddDataSet(WellDataArray);
          WelValueArrayItem := AScreenObject.ImportedValues.Add;
          WelValueArrayItem.Name := 'Imported_' + WellDataArray.Name;
          WelValueArrayItem.Values.DataType := rdtDouble;
          WelValueArrayItem.Values.Count := FFeatures.Count;
          AScreenObject.DataSetFormulas[Position]
            := rsObjectImportedValuesR + '("' + WelValueArrayItem.Name + '")';
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
