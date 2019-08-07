unit ModflowGAG_WriterUnit;

interface

uses SysUtils, Classes, CustomModflowWriterUnit, ModflowSfrWriterUnit;

type
  TModflowGAG_Writer = class(TCustomModflowWriter)
  private
    FSfrWriter: TModflowSFR_Writer;
    FNameOfFile: string;
    procedure Evaluate(Gages: TStrings);
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string; Gages: TStrings;
      SfrWriter: TModflowSFR_Writer);
  end;

implementation

uses Contnrs , RbwParser, GoPhastTypes, ModflowCellUnit, ModflowUnitNumbers,
  frmProgressUnit, DataSetUnit, frmGoPhastUnit, ScreenObjectUnit, 
  SubscriptionUnit;

resourcestring
  StrWritingGAGEPackage = 'Writing GAGE Package input.';

{ TModflowGAG_Writer }

procedure TModflowGAG_Writer.Evaluate(Gages: TStrings);
var
  Index: Integer;
  List: TList;
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
  procedure WriteGage(OUTTYPE: integer);
  var
    UNIT_Number: Integer;
    Line: string;
    OutputName: string;
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

  if Model.ModflowPackages.SfrPackage.GageOverallBudget then
  begin
    GAGESEG := 1;
    GAGERCH := 1;
    WriteGage(8);
  end;

  List := TObjectList.Create;
  try
    for Index := 1 to 7 do
    begin
      DataArray := TDataArray.Create(Model);
      List.Add(DataArray);
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
        ScreenObject.ModflowStreamGage.Evaluate(List, Model);
      end;
    end;
    for Index := 0 to List.Count - 1 do
    begin
      DataArray := List[Index];
      DataArray.UpToDate := True;
    end;

    DataArray0 := List[0];
    DataArray1 := List[1];
    DataArray2 := List[2];
    DataArray3 := List[3];
    DataArray5 := List[4];
    DataArray6 := List[5];
    DataArray7 := List[6];
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
    List.Free;
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
end;

end.
