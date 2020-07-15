unit PestObsExtractorInputWriterUnit;

interface

uses
  System.Classes, PhastModelUnit, System.SysUtils, PestObsUnit;

type
  TPestObsExtractorInputWriter = class(TObject)
  private
    FModel: TCustomModel;
    FDerivedObservationLines: TStringList;
    procedure GetDerivedObs;
  public
    Constructor Create(Model: TCustomModel);
    procedure WriteFile(FileName: string);
  end;

implementation

uses
  GoPhastTypes, ScreenObjectUnit, Modflow6ObsUnit, ObservationComparisonsUnit,
  PestGlobalComparisonScriptWriterUnit, frmErrorsAndWarningsUnit;

{ TPestObsExtractorInputWriter }

constructor TPestObsExtractorInputWriter.Create(Model: TCustomModel);
begin
  FModel := Model;
end;

procedure TPestObsExtractorInputWriter.GetDerivedObs;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ObsItemDictionary: TObsItemDictionary;
  CalibrationObservations: TMf6CalibrationObservations;
  ObsIndex: Integer;
  Item: TMf6CalibrationObs;
  CompItem: TObsCompareItem;
  Obs1: TMf6CalibrationObs;
  Obs2: TMf6CalibrationObs;
  CompIndex: Integer;
  ObservationComparisons: TGlobalObservationComparisons;
  CompareItem: TGlobalObsComparisonItem;
  FirstItem: TCustomObservationItem;
  SecondItem: TCustomObservationItem;
  ErrorMessage: string;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(FModel, StrUnableToExportObs);
  FDerivedObservationLines := FModel.DerivedObservationLines;
  ObsItemDictionary := TObsItemDictionary.Create;
  try
    for ScreenObjectIndex := 0 to FModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := FModel.ScreenObjects[ScreenObjectIndex];
      if (AScreenObject.Modflow6Obs <> nil)
        and AScreenObject.Modflow6Obs.Used then
      begin
        CalibrationObservations :=
          AScreenObject.Modflow6Obs.CalibrationObservations;
        for ObsIndex := 0 to CalibrationObservations.Count - 1 do
        begin
          Item := CalibrationObservations[ObsIndex];
          ObsItemDictionary.Add(Item.GUID, Item);
        end;

        for CompIndex := 0 to CalibrationObservations.Comparisons.Count - 1 do
        begin
          CompItem := CalibrationObservations.Comparisons[CompIndex];

          Obs1 := CalibrationObservations[CompItem.Index1];
          Obs2 := CalibrationObservations[CompItem.Index2];

          FDerivedObservationLines.Add(Format('  OBSNAME %s', [CompItem.Name]));
          FDerivedObservationLines.Add(Format('  FORMULA %0:s - %1:s',
             [Obs1.Name, Obs2.Name]));
          FDerivedObservationLines.Add('');

          ObsItemDictionary.Add(CompItem.GUID, CompItem);
        end;
      end;

      ObservationComparisons := FModel.Modflow6GlobalObservationComparisons;
      for CompIndex := 0 to ObservationComparisons.Count - 1 do
      begin
        CompareItem := ObservationComparisons[CompIndex];
        if ObsItemDictionary.TryGetValue(CompareItem.GUID1, FirstItem)
          and ObsItemDictionary.TryGetValue(CompareItem.GUID2, SecondItem) then
        begin
          FDerivedObservationLines.Add(Format('  OBSNAME %s', [CompareItem.Name]));
          FDerivedObservationLines.Add(Format('  FORMULA %0:s - %1:s',
             [FirstItem.Name, SecondItem.Name]));
          FDerivedObservationLines.Add('');
        end
        else
        begin
          ErrorMessage := Format(StrTheObservationComp, [CompareItem.Name]);
          frmErrorsAndWarnings.AddWarning(FModel, StrUnableToExportObs, ErrorMessage)
        end;
      end;
    end;
  finally
    ObsItemDictionary.Free;
  end;
end;

procedure TPestObsExtractorInputWriter.WriteFile(FileName: string);
var
  Lines: TStringList;
  LinePostion: Integer;
begin
  if FModel.PestUsed and (FModel.ModelSelection = msModflow2015)
    and (FModel.DirectObservationLines.Count > 0) then
  begin
    GetDerivedObs;

    Lines := TStringList.Create;
    try
      Lines.Add('BEGIN OPTIONS');
      Lines.Add('  LISTING ' + ExtractFileName(ChangeFileExt(FileName, '.Mf6ObsExtInsLst')));
//      Lines.Add('  VALUES ' + ChangeFileExt(FileName, '.Mf6Values'));
      LinePostion := Lines.Add('  INSTRUCTION ' + ExtractFileName(ChangeFileExt(FileName, '.PestIns')));
      Lines.Add('END OPTIONS');
      Lines.Add('');

      Lines.Add('BEGIN OBSERVATION_FILES');
      Lines.AddStrings(FModel.FileNameLines);
      Lines.Add('END OBSERVATION_FILES');
      Lines.Add('');

      Lines.Add('BEGIN IDENTIFIERS');
      Lines.AddStrings(FModel.DirectObservationLines);
      Lines.Add('END IDENTIFIERS');
      Lines.Add('');

      if FModel.DerivedObservationLines.Count > 0 then
      begin
        Lines.Add('BEGIN DERIVED_OBSERVATIONS');
        Lines.AddStrings(FModel.DerivedObservationLines);
        Lines.Add('END DERIVED_OBSERVATIONS');
      end;

      FileName := ChangeFileExt(FileName, '.Mf6WriteIns');
      Lines.SaveToFile(FileName);

      Lines[LinePostion] := '  VALUES ' + ExtractFileName(ChangeFileExt(FileName, '.Mf6Values'));
      FileName := ExtractFileName(ChangeFileExt(FileName, '.Mf6ExtractValues'));
      Lines.SaveToFile(FileName);

    finally
      Lines.Free;
    end;
  end;
end;

end.
