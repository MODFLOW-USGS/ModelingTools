unit PestGlobalComparisonScriptWriterUnit;

interface

uses
  CustomModflowWriterUnit, System.SysUtils;

type
  TGlobalComparisonScriptWriter = class(TCustomFileWriter)
  public
    procedure WriteFile(const FileName: string);
  end;

implementation

uses
  PestObsUnit, ObservationComparisonsUnit, ScreenObjectUnit, GoPhastTypes;

{ TGlobalComparisonScriptWriter }

procedure TGlobalComparisonScriptWriter.WriteFile(const FileName: string);
var
  ScriptFileName: string;
  ComparisonIndex: Integer;
  GloCompItem: TGlobalObsComparisonItem;
  FObsItemDictionary: TObsItemDictionary;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ObsIndex: Integer;
  ObsItem: TCustomTimeObservationItem;
  PriorItem1: TCustomObservationItem;
  PriorItem2: TCustomObservationItem;
  ErrorMessage: string;
begin
{$IFNDEF PEST}
  Exit;
{$ENDIF}

  if Model.GlobalObservationComparisons.Count = 0 then
  begin
    Exit;
  end;
  
  ScriptFileName := ChangeFileExt(FileName, '.der_script');

  FObsItemDictionary := TObsItemDictionary.Create;
  try
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if Model.ModflowPackages.Mnw2Package.IsSelected
        and (Model.ModelSelection <> msMODFLOW2015) then
      begin
        if (AScreenObject.ModflowMnw2Boundary <> nil)
          and AScreenObject.ModflowMnw2Boundary.Used
          and (AScreenObject.ModflowMnw2Boundary.Observations.Count > 0) then
        begin
          for ObsIndex := 0 to AScreenObject.ModflowMnw2Boundary.Observations.Count - 1 do
          begin
            ObsItem := AScreenObject.ModflowMnw2Boundary.Observations[ObsIndex];
            FObsItemDictionary.Add(ObsItem.GUID, ObsItem);
          end;
        end;
      end;
      if Model.ModflowPackages.LakPackage.IsSelected
        and (Model.ModelSelection <> msMODFLOW2015) then
      begin
        if (AScreenObject.ModflowLakBoundary <> nil)
          and AScreenObject.ModflowLakBoundary.Used
          and (AScreenObject.ModflowLakBoundary.Observations.Count > 0) then
        begin
          for ObsIndex := 0 to AScreenObject.ModflowLakBoundary.Observations.Count - 1 do
          begin
            ObsItem := AScreenObject.ModflowLakBoundary.Observations[ObsIndex];
            FObsItemDictionary.Add(ObsItem.GUID, ObsItem);
          end;
        end;
      end;
    end;
      
    if FObsItemDictionary.Count = 0 then
    begin
      Exit;
    end;

    OpenFile(ScriptFileName);
    try

      WriteString('BEGIN DERIVED_OBSERVATIONS');
      NewLine;

      WriteString('  # ');
      WriteString('Global observation comparisons');
      NewLine;

      for ComparisonIndex := 0 to Model.GlobalObservationComparisons.Count - 1 do
      begin
        GloCompItem := Model.GlobalObservationComparisons[ComparisonIndex];
        if FObsItemDictionary.TryGetValue(GloCompItem.GUID1, PriorItem1)
          and FObsItemDictionary.TryGetValue(GloCompItem.GUID2, PriorItem2) then
        begin
          WriteString('  DIFFERENCE ');
          WriteString(GloCompItem.Name);
          WriteString(' ');
          WriteString(PriorItem1.ExportedName);
          WriteString(' ');
          WriteString(PriorItem2.ExportedName);
          WriteFloat(GloCompItem.ObservedValue);
          WriteFloat(GloCompItem.Weight);
          WriteString(' PRINT');
          NewLine;
        end
        else
        begin
          ErrorMessage := Format('The observation comparison item "%s" could not be exported. Check that it is defined correctly for this model.', [GloCompItem.Name]);
        end;


        WriteString('END DERIVED_OBSERVATIONS');
      end;

    finally
      CloseFile;
    end;
  finally
    FObsItemDictionary.Free;
  end;


end;

end.
