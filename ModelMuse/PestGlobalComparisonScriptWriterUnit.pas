unit PestGlobalComparisonScriptWriterUnit;

interface

uses
  CustomModflowWriterUnit, System.SysUtils;

type
  TGlobalComparisonScriptWriter = class(TCustomFileWriter)
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

resourcestring
  StrTheObservationComp = 'The observation comparison item "%s" could not be' +
  ' exported. Check that it is defined correctly for this model.';
  StrUnableToExportObs = 'Unable to export observations';

implementation

uses
  PestObsUnit, ObservationComparisonsUnit, ScreenObjectUnit, GoPhastTypes,
  frmErrorsAndWarningsUnit, ModelMuseUtilities, ObsInterfaceUnit;

{ TGlobalComparisonScriptWriter }

class function TGlobalComparisonScriptWriter.Extension: string;
begin
  result := '.der_script';
end;

procedure TGlobalComparisonScriptWriter.WriteFile(const AFileName: string);
var
  ScriptFileName: string;
  ComparisonIndex: Integer;
  GloCompItem: TGlobalObsComparisonItem;
  FObsItemDictionary: TObsItemDictionary;
  ObsItem: IObservationItem;
  PriorItem1: IObservationItem;
  PriorItem2: IObservationItem;
  ErrorMessage: string;
  ObservationList: TObservationInterfaceList;
  ItemIndex: Integer;
  function GetObName(ObjectIndex: Integer; Obs: IObservationItem): string;
  var
    AnObs: TCustomObservationItem;
  begin
    if Obs is TCustomObservationItem then
    begin
      AnObs := TCustomObservationItem(Obs);
      Result := PrefixedObsName('Der', ObjectIndex, AnObs);
    end
    else
    begin
      Result := PrefixedIntName('Der', ObjectIndex, Obs);
    end;
  end;
begin
{$IFNDEF PEST}
  Exit;
{$ENDIF}
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrUnableToExportObs);

  if Model.ModflowGlobalObservationComparisons.Count = 0 then
  begin
    Exit;
  end;

  ScriptFileName := FileName(AFileName);

  ObservationList := TObservationInterfaceList.Create;
  FObsItemDictionary := TObsItemDictionary.Create;
  try
    Model.FillObsInterfaceItemList(ObservationList);

    if ObservationList.Count = 0 then
    begin
      Exit;
    end;

    for ItemIndex := 0 to ObservationList.Count - 1 do
    begin
      ObsItem := ObservationList[ItemIndex];
      FObsItemDictionary.Add(ObsItem.GUID, ObsItem);
    end;

//    FInputFileName := ScriptFileName;
    OpenFile(ScriptFileName);
    try
      if Model.ModelSelection in Modflow2005Selection then
      begin
        Model.FileNameLines.Add('  DERIVED ' +  ExtractFileName(ScriptFileName));
      end;

      WriteString('BEGIN DERIVED_OBSERVATIONS');
      NewLine;

      WriteString('  # ');
      WriteString('Global observation comparisons');
      NewLine;

      for ComparisonIndex := 0 to Model.ModflowGlobalObservationComparisons.Count - 1 do
      begin
        GloCompItem := Model.ModflowGlobalObservationComparisons[ComparisonIndex];
        if FObsItemDictionary.TryGetValue(GloCompItem.GUID1, PriorItem1)
          and FObsItemDictionary.TryGetValue(GloCompItem.GUID2, PriorItem2) then
        begin
          WriteString('  DIFFERENCE ');
          WriteString(GetObName(ComparisonIndex, GloCompItem));
          WriteString(' ');
          if PriorItem1 is TCustomObservationItem then
          begin
            WriteString(TCustomObservationItem(PriorItem1).ExportedName);
          end
          else
          begin
            WriteString(PriorItem1.Name);
          end;
          WriteString(' ');
          if PriorItem2 is TCustomObservationItem then
          begin
            WriteString(TCustomObservationItem(PriorItem2).ExportedName);
          end
          else
          begin
            WriteString(PriorItem2.Name);
          end;
//          WriteString(PriorItem2.ExportedName);
          WriteFloat(GloCompItem.ObservedValue);
          WriteFloat(GloCompItem.Weight);
          if GloCompItem.Print then
          begin
            WriteString(' PRINT');
          end;
          NewLine;
        end
        else
        begin
          ErrorMessage := Format(StrTheObservationComp, [GloCompItem.Name]);
          frmErrorsAndWarnings.AddWarning(Model, StrUnableToExportObs, ErrorMessage)
        end;
      end;
      WriteString('END DERIVED_OBSERVATIONS');

    finally
      CloseFile;
    end;
  finally
    FObsItemDictionary.Free;
    ObservationList.Free;
  end;
end;

end.
