unit CoarsenDemUnit;

interface

uses
  System.Types, System.Classes;

type
  TCoarsenInput = record
    // @name is the name of the input DEM in Surfer Grid File 7 format.
    InputFileName: string;
    // @name is the name of the output DEM in Surfer Grid File 7 format.
    OutputFileName: string;
    // Any points that are outside the polygon in @name will be treated as
    // no data values in the generation of the output DEM.
    IncludeShapeFileName: String;
    // Any points that are inside the polygons in @name will be treated as
    // no data values in the generation of the output DEM.
    ExcludeShapeFileNames: TStrings;
    // @name is the number of rows and columns whose Z coordinates will be
    // averaged in the output DEM. For example if @name is 3, 3 rows and
    // 3 columns for a total of 9 point from the input DEM will be combined
    // into a single Z value in the output DEM.
    CoarsenAmount: Integer;
  end;

//  TCoarsenOutput = record
//    ColumnStarts: TIntegerDynArray;
//    RowStarts: TIntegerDynArray;
//  end;

function CoarsenDem(const CoarsenInput: TCoarsenInput): Boolean;

implementation

uses
  ClippedSurferRasterUnit, SurferGridFileWriterUnit, SurferGridFileReaderUnit;

function CoarsenDem(const CoarsenInput: TCoarsenInput): Boolean;
var
  InputDem: TClippedSurferRaster;
  ExistingHeader: TGrid7Header;
  NewHeader: TGrid7Header;
  NewData: T2DDoubleArray;
  Counts: array of TIntegerDynArray;
  ColIndex: Integer;
  RowIndex: Integer;
  RIndex: Integer;
  CIndex: Integer;
begin
  result := False;
  Assert(CoarsenInput.InputFileName <> CoarsenInput.OutputFileName);
  Assert(CoarsenInput.CoarsenAmount > 1);

  InputDem := TClippedSurferRaster.Create(CoarsenInput.InputFileName);
  try
    InputDem.IncludeShapeFileName := CoarsenInput.IncludeShapeFileName;
    if CoarsenInput.ExcludeShapeFileNames <> nil then
    begin
      InputDem.ExcludeShapeFileNames := CoarsenInput.ExcludeShapeFileNames;
    end;

    ExistingHeader := InputDem.Header;
    NewHeader := ExistingHeader;
    NewHeader.xSize := NewHeader.xSize*CoarsenInput.CoarsenAmount;
    NewHeader.ySize := NewHeader.ySize*CoarsenInput.CoarsenAmount;

    NewHeader.xLL := NewHeader.xLL - ExistingHeader.xSize/2 + NewHeader.xSize/2;
    NewHeader.yLL := NewHeader.yLL - ExistingHeader.ySize/2 + NewHeader.ySize/2;

    NewHeader.nCol := NewHeader.nCol div CoarsenInput.CoarsenAmount;
    if (ExistingHeader.nCol mod CoarsenInput.CoarsenAmount) > 0 then
    begin
      Inc(NewHeader.nCol);
    end;
    NewHeader.nRow := NewHeader.nRow div CoarsenInput.CoarsenAmount;
    if (ExistingHeader.nRow mod CoarsenInput.CoarsenAmount) > 0 then
    begin
      Inc(NewHeader.nRow);
    end;

    SetLength(NewData, NewHeader.nCol, NewHeader.nRow);
    SetLength(Counts, NewHeader.nCol, NewHeader.nRow);


    for RowIndex := 0 to NewHeader.nRow - 1 do
    begin
      for ColIndex := 0 to NewHeader.nCol - 1 do
      begin
        NewData[ColIndex, RowIndex] := 0;
        Counts[ColIndex, RowIndex] := 0;
      end;
    end;

    for RowIndex := 0 to ExistingHeader.nRow - 1 do
    begin
      RIndex := RowIndex div CoarsenInput.CoarsenAmount;
      for ColIndex := 0 to ExistingHeader.nCol - 1 do
      begin
        CIndex := ColIndex div CoarsenInput.CoarsenAmount;
        if not InputDem.Ignore[ColIndex, RowIndex] then
        begin
          NewData[CIndex,RIndex] := NewData[CIndex,RIndex] + InputDem.Z[ColIndex,RowIndex];
          Inc(Counts[CIndex,RIndex]);
        end;
      end;
    end;

    for RowIndex := 0 to NewHeader.nRow - 1 do
    begin
      for ColIndex := 0 to NewHeader.nCol - 1 do
      begin
        if Counts[ColIndex,RowIndex] > 0 then
        begin
          NewData[ColIndex,RowIndex] :=
            NewData[ColIndex,RowIndex]/Counts[ColIndex,RowIndex];
        end
        else
        begin
          NewData[ColIndex,RowIndex] := NewHeader.BlankValue;
        end;
      end;
    end;
    WriteSurferGridFile(CoarsenInput.OutputFileName, NewHeader, NewData);
    Result := True;
  finally
    InputDem.Free;
  end;
end;

end.
