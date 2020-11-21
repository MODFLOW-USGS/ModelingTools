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

const
  StrPestIns: string = '.PestIns';
  StrMf6Values: string = '.Mf6Values';
  StrMf6WriteIns: string = '.Mf6WriteIns';
  StrMf2005Values: string = '.Mf2005Values';
  StrMf2005WriteIns: string = '.Mf2005WriteIns';
  StrSutraValues: string = '.SutraValues';


implementation

uses
  GoPhastTypes, ScreenObjectUnit, Modflow6ObsUnit, ObservationComparisonsUnit,
  PestGlobalComparisonScriptWriterUnit, frmErrorsAndWarningsUnit,
  ObsInterfaceUnit;

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
  FirstItem: IObservationItem;
  SecondItem: IObservationItem;
  ErrorMessage: string;
  ObsItemList: TObservationInterfaceList;
  ItemIndex: Integer;
  ObsItem: IObservationItem;
begin

  frmErrorsAndWarnings.RemoveWarningGroup(FModel, StrUnableToExportObs);
  FDerivedObservationLines := FModel.DerivedObservationLines;
  ObsItemDictionary := TObsItemDictionary.Create;
  ObsItemList:=  TObservationInterfaceList.Create;
  try
    FModel.FillObsInterfaceItemList(ObsItemList);
    for ItemIndex := 0 to ObsItemList.Count - 1 do
    begin
      ObsItem := ObsItemList[ItemIndex];
      ObsItemDictionary.Add(ObsItem.GUID, ObsItem);
    end;
    for ScreenObjectIndex := 0 to FModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := FModel.ScreenObjects[ScreenObjectIndex];
      if (AScreenObject.Modflow6Obs <> nil)
        and AScreenObject.Modflow6Obs.Used then
      begin
        CalibrationObservations :=
          AScreenObject.Modflow6Obs.CalibrationObservations;
//        for ObsIndex := 0 to CalibrationObservations.Count - 1 do
//        begin
//          Item := CalibrationObservations[ObsIndex];
//          ObsItemDictionary.Add(Item.GUID, Item);
//        end;

        for CompIndex := 0 to CalibrationObservations.Comparisons.Count - 1 do
        begin
          CompItem := CalibrationObservations.Comparisons[CompIndex];

          Obs1 := CalibrationObservations[CompItem.Index1];
          Obs2 := CalibrationObservations[CompItem.Index2];

          if FModel.ModelSelection = msModflow2015 then          
          begin          
            FDerivedObservationLines.Add(Format('  OBSNAME %0:s PRINT', [CompItem.Name]));
          end            
          else  if FModel.ModelSelection in Modflow2005Selection then          
          begin          
            FDerivedObservationLines.Add(Format('  OBSNAME %s', [CompItem.Name]));
          end
          else
          begin                    
            Assert(False);
          end;
          FDerivedObservationLines.Add(Format('  FORMULA %0:s - %1:s',
             [Obs1.Name, Obs2.Name]));
          FDerivedObservationLines.Add('');

          ObsItemDictionary.Add(CompItem.GUID, CompItem);
        end;
      end;
    end;

    ObservationComparisons := nil;
    if FModel.ModelSelection = msModflow2015 then
    begin
      ObservationComparisons := FModel.Modflow6GlobalObservationComparisons;
    end
    else if FModel.ModelSelection in Modflow2005Selection then
    begin
      ObservationComparisons := FModel.ModflowGlobalObservationComparisons;
    end
    else if FModel.ModelSelection in SutraSelection then
    begin
      ObservationComparisons := FModel.SutraGlobalObservationComparisons;
    end;
      
    if ObservationComparisons <> nil then      
    begin      
      for CompIndex := 0 to ObservationComparisons.Count - 1 do
      begin
        CompareItem := ObservationComparisons[CompIndex];
        if ObsItemDictionary.TryGetValue(CompareItem.GUID1, FirstItem)
          and ObsItemDictionary.TryGetValue(CompareItem.GUID2, SecondItem) then
        begin
          if FModel.ModelSelection = msModflow2015 then        
          begin          
            FDerivedObservationLines.Add(Format('  OBSNAME %s PRINT', [CompareItem.Name]));
          end            
          else if FModel.ModelSelection in Modflow2005Selection then          
          begin          
            FDerivedObservationLines.Add(Format('  OBSNAME %s', [CompareItem.Name]));
          end;                    
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
    ObsItemList.Free;
  end;
end;

procedure TPestObsExtractorInputWriter.WriteFile(FileName: string);
var
  Lines: TStringList;
  LinePostion: Integer;
  PestUsed: Boolean;
begin
  PestUsed := FModel.PestUsed; 
  if FModel.ModelSelection = msModflow2015 then
  begin                              
    PestUsed := PestUsed and (FModel.DirectObservationLines.Count > 0)
  end
  else if FModel.ModelSelection in Modflow2005Selection then
  begin
    PestUsed := PestUsed and (FModel.FileNameLines.Count > 0)
  end
  else
  begin
    PestUsed := False;
  end;
  
  if PestUsed then
  begin
    GetDerivedObs;

    Lines := TStringList.Create;
    try
      if FModel.ModelSelection = msModflow2015 then      
      begin      
        Lines.Add('BEGIN OPTIONS');
        Lines.Add('  LISTING ' + ExtractFileName(ChangeFileExt(FileName, '.Mf6ObsExtInsLst')));
        LinePostion := Lines.Add('  INSTRUCTION ' + ExtractFileName(ChangeFileExt(FileName, StrPestIns)));
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

        FileName := ChangeFileExt(FileName, StrMf6WriteIns);
        Lines.SaveToFile(FileName);

        Lines[LinePostion] := '  VALUES ' + ExtractFileName(ChangeFileExt(FileName, StrMf6Values));
        FileName := ExtractFileName(ChangeFileExt(FileName, '.Mf6ExtractValues'));
        Lines.SaveToFile(FileName);
      end        
      else if FModel.ModelSelection in Modflow2005Selection then
      begin
        Lines.Add('BEGIN OUTPUT_FILES');
        Lines.Add('  LIST ' + ExtractFileName(ChangeFileExt(FileName, '.Mf2005ObsExtInsLst')));
        LinePostion := Lines.Add('  INSTRUCTION_FILE ' + ExtractFileName(ChangeFileExt(FileName, StrPestIns)));
        Lines.Add('END OUTPUT_FILES');
        Lines.Add('');

        Lines.Add('BEGIN INPUT_FILES');
        Lines.AddStrings(FModel.FileNameLines);
        Lines.Add('END INPUT_FILES');
        Lines.Add('');

//        Lines.Add('BEGIN IDENTIFIERS');
//        Lines.AddStrings(FModel.DirectObservationLines);
//        Lines.Add('END IDENTIFIERS');
//        Lines.Add('');
//
//        if FModel.DerivedObservationLines.Count > 0 then
//        begin
//          Lines.Add('BEGIN DERIVED_OBSERVATIONS');
//          Lines.AddStrings(FModel.DerivedObservationLines);
//          Lines.Add('END DERIVED_OBSERVATIONS');
//        end;

        FileName := ChangeFileExt(FileName, StrMf2005WriteIns);
        Lines.SaveToFile(FileName);

        Lines[LinePostion] := '  OBSERVATIONS_FILE ' + ExtractFileName(ChangeFileExt(FileName, StrMf2005Values));
        FileName := ExtractFileName(ChangeFileExt(FileName, '.Mf2005ExtractValues'));
        Lines.SaveToFile(FileName);
      end
      else
      begin
        Assert(False);
      end;

    finally
      Lines.Free;
    end;
  end;
end;

end.
