unit ModflowGAG_WriterUnit;

interface

uses SysUtils, Classes, CustomModflowWriterUnit, ModflowSfrWriterUnit,
  ScreenObjectUnit, PhastModelUnit;

type
  TModflowGAG_Writer = class(TCustomModflowWriter)
  private
    FSfrWriter: TModflowSFR_Writer;
    FNameOfFile: string;
    FGagObservationsUsed: Boolean;
    FGageScreenObjectList: TScreenObjectList;
    procedure Evaluate(Gages: TStrings);
    procedure WriteObsScript(const AFileName: string);
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string; Gages: TStrings;
      SfrWriter: TModflowSFR_Writer);
  end;

implementation

uses Contnrs , RbwParser, GoPhastTypes, ModflowCellUnit, ModflowUnitNumbers,
  frmProgressUnit, DataSetUnit, frmGoPhastUnit,
  SubscriptionUnit, ModflowSfrUnit, PestObsUnit, ModflowGageUnit;

resourcestring
  StrWritingGAGEPackage = 'Writing GAGE Package input.';

{ TModflowGAG_Writer }

constructor TModflowGAG_Writer.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FGageScreenObjectList := TScreenObjectList.Create;
end;

destructor TModflowGAG_Writer.Destroy;
begin
  FGageScreenObjectList.Free;
  inherited;
end;

procedure TModflowGAG_Writer.Evaluate(Gages: TStrings);
var
  Index: Integer;
  DataSets: TList;
  DataArray: TDataArray;
  ScreenObject: TScreenObject;
  Segment: TSegment;
  ReachIndex: Integer;
  Reach: TValueCell;
  DataArray0: TDataArray;
  DataArray1: TDataArray;
  DataArray2: TDataArray;
  DataArray3: TDataArray;
  DataArray5: TDataArray;
  DataArray6: TDataArray;
  DataArray7: TDataArray;
  GAGESEG: Integer;
  GAGERCH: Integer;
  SubSegIndex: Integer;
  SubSeg: TSubSegment;
  ObservationsArray: array of array of array of TSfrObservations;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
  procedure WriteGage(OUTTYPE: integer);
  var
    UNIT_Number: Integer;
    Line: string;
    OutputName: string;
    Observations: TSfrObservations;
  begin
    UNIT_Number := Model.ParentModel.UnitNumbers.SequentialUnitNumber;
    Line := IntToStr(GAGESEG) + ' '
      + IntToStr(GAGERCH) + ' '
      + IntToStr(UNIT_Number) + ' '
      + IntToStr(OUTTYPE);
    Gages.Add(Line);
//    Inc(StartUnitNumber);
    OutputName := ChangeFileExt(FNameOfFile, '.sfrg');
    OutputName := OutputName + IntToStr(Gages.Count);
    WriteToNameFile(StrDATA, UNIT_Number, OutputName, foOutput, Model);
    if (OUTTYPE = 4) then
    begin
      Observations := ObservationsArray[Reach.Layer, Reach.Row, Reach.Column];
      if Observations <> nil then
      begin
        Observations.GageOutputName := OutputName;
        if Observations.Count > 0 then
        begin
          FGagObservationsUsed := True;
        end;
      end;
    end;
  end;
  procedure WriteReach;
  begin
    if DataArray0.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
      and DataArray1.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
      and DataArray2.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
      and DataArray3.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
      then
    begin
      WriteGage(4);
    end
    else
    begin
      if DataArray0.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
        then
      begin
        WriteGage(0);
      end;
      if DataArray1.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
        then
      begin
        WriteGage(1);
      end;
      if DataArray2.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
        then
      begin
        WriteGage(2);
      end;
      if DataArray3.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
        then
      begin
        WriteGage(3);
      end;
    end;
    if DataArray5.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
      then
    begin
      WriteGage(5);
    end;
    if DataArray6.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
      then
    begin
      WriteGage(6);
    end;
    if DataArray7.BooleanData[Reach.Layer, Reach.Row, Reach.Column]
      then
    begin
      WriteGage(7);
    end;
  end;
begin
  if (Model.ModflowPackages = nil)
    or (Model.ModflowPackages.SfrPackage = nil)
    or not Model.ModflowPackages.SfrPackage.IsSelected then
  begin
    Exit;
  end;

  FGagObservationsUsed := False;

  if Model.ModflowPackages.SfrPackage.GageOverallBudget then
  begin
    GAGESEG := 1;
    GAGERCH := 1;
    WriteGage(8);
  end;

  SetLength(ObservationsArray, Model.LayerCount, Model.RowCount,
    Model.ColumnCount);

  DataSets := TObjectList.Create;
  try
    for Index := 1 to 7 do
    begin
      DataArray := TDataArray.Create(Model);
      DataSets.Add(DataArray);
      DataArray.Orientation := dso3D;
      DataArray.EvaluatedAt := eaBlocks;
      DataArray.DataType := rdtBoolean;
      DataArray.UpdateDimensions(Model.LayerCount,
        Model.RowCount, Model.ColumnCount, True);
    end;

    for Index := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[Index];
      if (ScreenObject.ModflowStreamGage <> nil)
        and ScreenObject.ModflowStreamGage.Used then
      begin
        FGageScreenObjectList.Add(ScreenObject);
        ScreenObject.ModflowStreamGage.Evaluate(DataSets, Model);
        CellList := TCellAssignmentList.Create;
        try
          ScreenObject.GetModpathCellList(CellList, Model);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            ACell := CellList[CellIndex];
            ObservationsArray[ACell.Layer, ACell.Row, ACell.Column]
              := ScreenObject.ModflowStreamGage.Observations;
          end;
        finally
          CellList.Free;
        end;
      end;
    end;
    for Index := 0 to DataSets.Count - 1 do
    begin
      DataArray := DataSets[Index];
      DataArray.UpToDate := True;
    end;

    DataArray0 := DataSets[0];
    DataArray1 := DataSets[1];
    DataArray2 := DataSets[2];
    DataArray3 := DataSets[3];
    DataArray5 := DataSets[4];
    DataArray6 := DataSets[5];
    DataArray7 := DataSets[6];
    for Index := 0 to FSfrWriter.SegmentCount - 1 do
    begin
      Segment := FSfrWriter.Segments[Index];
      GAGESEG := Segment.NewSegmentNumber;
      if Segment.SubSegmentList.Count = 0 then
      begin
        for ReachIndex := 0 to Segment.ReachCount - 1 do
        begin
          GAGERCH := ReachIndex + 1;
          Reach := Segment.Reaches[ReachIndex];
          WriteReach;
        end;
      end
      else
      begin
        for SubSegIndex := 0 to Segment.SubSegmentList.Count - 1 do
        begin
          SubSeg := Segment.SubSegmentList[SubSegIndex];
          GAGESEG := SubSeg.SegmentNumber;
          for ReachIndex := 0 to SubSeg.ReachCount - 1 do
          begin
            GAGERCH := ReachIndex + 1;
            Reach := SubSeg.Reaches[ReachIndex];
            WriteReach;
          end;
        end;
      end;
    end;
  finally
    DataSets.Free;
  end;
end;

class function TModflowGAG_Writer.Extension: string;
begin
  result := '.gag';
end;

procedure TModflowGAG_Writer.WriteFile(const AFileName: string;
  Gages: TStrings; SfrWriter: TModflowSFR_Writer);
var
  NUMGAGES: integer;
begin
  if Model.PackageGeneratedExternally(StrGAG) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;

  FSfrWriter := SfrWriter;
  FNameOfFile := FileName(AFileName);
  Evaluate(Gages);
  if Gages.Count > 0 then
  begin
    frmProgressMM.AddMessage(StrWritingGAGEPackage);
    NUMGAGES := Gages.Count;
    Gages.Insert(0, IntToStr(NUMGAGES));
    WriteToNameFile(StrGAG, Model.UnitNumbers.UnitNumber(StrGAG),
      FNameOfFile, foInput, Model);
    Gages.SaveToFile(FNameOfFile);
  end;
  WriteObsScript(AFileName);
end;

procedure TModflowGAG_Writer.WriteObsScript(const AFileName: string);
var
  StartTime: Double;
  ScriptFileName: string;
  ComparisonsUsed: Boolean;
  SegmentIndex: Integer;
  ScreenObject: TScreenObject;
  ObsIndex: Integer;
  CompIndex: Integer;
  CompItem: TObsCompareItem;
  Observations: TSfrObservations;
  Obs: TSfrObs;
  Boundary: TStreamGage;
  function GetObName(ObjectIndex: Integer; Obs: TCustomObservationItem): string;
  begin
    Result := PrefixedObsName('Gag', ObjectIndex, Obs);
  end;
begin
{$IFNDEF PEST}
  Exit;
{$ENDIF}
  if not FGagObservationsUsed then
  begin
    Exit;
  end;

  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
  ScriptFileName := ChangeFileExt(AFileName, '.Gag_script');

  OpenFile(ScriptFileName);
  try
    ComparisonsUsed := False;
    // OBSERVATIONS block
    WriteString('BEGIN OBSERVATIONS');
    NewLine;
    for SegmentIndex := 0 to FGageScreenObjectList.Count - 1 do
    begin
      ScreenObject := FGageScreenObjectList[SegmentIndex];
      Boundary := ScreenObject.ModflowStreamGage;
      Observations := Boundary.Observations;
      if Observations.Count > 0 then
      begin
        WriteString('  # ');
        WriteString('Observations defined in ');
        WriteString(ScreenObject.Name);
        NewLine;
        WriteString('  FILENAME ');
        WriteString(Observations.GageOutputName);
        NewLine;

        for ObsIndex := 0 to Observations.Count - 1 do
        begin
          Obs := Observations[ObsIndex];
          WriteString('  OBSERVATION ');
          WriteString(GetObName(SegmentIndex, Obs));
          WriteString(' ');
          WriteString(Obs.ObservationType);
          WriteFloat(Obs.Time - StartTime);
          WriteFloat(Obs.ObservedValue);
          WriteFloat(Obs.Weight);
          WriteString(' PRINT');
          NewLine;
        end;

        if Observations.Comparisons.Count > 0 then
        begin
          ComparisonsUsed := True;
        end;
      end;
    end;
    WriteString('END OBSERVATIONS');

    // DERIVED_OBSERVATIONS block
    if ComparisonsUsed then
    begin
      NewLine;
      NewLine;
      WriteString('BEGIN DERIVED_OBSERVATIONS');
      NewLine;

      for SegmentIndex := 0 to FGageScreenObjectList.Count - 1 do
      begin
        ScreenObject := FGageScreenObjectList[SegmentIndex];
        Boundary := ScreenObject.ModflowStreamGage;
//        ScreenObject := Segment.FScreenObject;
        Observations := Boundary.Observations;
        if Observations.Comparisons.Count > 0 then
        begin
          WriteString('  # ');
          WriteString('Observation comparisons defined in ');
          WriteString(ScreenObject.Name);
          NewLine;
        end;

        for CompIndex := 0 to Observations.Comparisons.Count - 1 do
        begin
          WriteString('  DIFFERENCE ');
          CompItem := Observations.Comparisons[CompIndex];
          WriteString(GetObName(SegmentIndex, CompItem));
          WriteString(' ');
          Obs := Observations[CompItem.Index1];
          WriteString(Obs.ExportedName);
          WriteString(' ');
          Obs := Observations[CompItem.Index2];
          WriteString(Obs.ExportedName);
          WriteFloat(CompItem.ObservedValue);
          WriteFloat(CompItem.Weight);
          WriteString(' PRINT');
          NewLine;
        end;
      end;
      WriteString('END DERIVED_OBSERVATIONS');
    end;
  finally
    CloseFile;
  end;
end;

end.
