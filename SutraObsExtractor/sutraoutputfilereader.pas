unit SutraOutputFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustomOutputFileReader, CustomInputReader;

type

  { TSutraObsOutputFile }

  TSutraObsOutputFile = class(TCustomOutputFile)
  private
    FSplitter: TStringList;
    FNumberOfValues: Integer;
    FLocationDictionary: TLocationDictionary;
  protected
    procedure ReadHeader; override;
  public
    procedure ReadTimeAndValues; override;
    constructor Create(AFileName: string;
      IdLocations: TObservationDictionary; LocationDictionary: TLocationDictionary);
    destructor Destroy; override;
  end;

  { TCustomNodeOutputFile }

  TCustomNodeOutputFile = class(TCustomOutputFile)
  protected
    FSplitter: TStringList;
    FNumberOfValues: Integer;
    procedure ReadHeader; override;
    function NumberOfValuesPerLine: Integer; virtual; abstract;
    function GetNumberOfValues(CurrentLines: TStringList): Integer;
    function LineIsStartOfData(ALine: string): Boolean; virtual;
    function GetObsID(NodeNumber: string; ObsTypeIndex: integer): string; virtual; abstract;
    function GetObsValue(ObsTypeIndex: integer): double; virtual; abstract;
    procedure ExtractValues(ALine: string); virtual;
  public
    constructor Create(AFileName: string;
      IdLocations: TObservationDictionary);
    destructor Destroy; override;
    procedure ReadTimeAndValues; override;
  end;

  { TSutraLakeStageOutputFile }

  TSutraLakeStageOutputFile = class(TCustomNodeOutputFile)
  protected
    function NumberOfValuesPerLine: Integer; override;
    function LineIsStartOfData(ALine: string): Boolean; override;
    function GetObsID(NodeNumber: string; ObsTypeIndex: integer): string; override;
    function GetObsValue(ObsTypeIndex: integer): double; override;
    procedure ExtractValues(ALine: string); override;
  end;

  { TSutraSpecifiedPressureOutputFile }

  TSutraSpecifiedPressureOutputFile = class(TCustomNodeOutputFile)
  protected
    function NumberOfValuesPerLine: Integer; override;
    function GetObsID(NodeNumber: string; ObsTypeIndex: integer): string; override;
    function GetObsValue(ObsTypeIndex: integer): double; override;
  end;

  { TSutraFluidSourceSinkOutputFile }

  TSutraFluidSourceSinkOutputFile = class(TCustomNodeOutputFile)
  protected
    function NumberOfValuesPerLine: Integer; override;
    function GetObsID(NodeNumber: string; ObsTypeIndex: integer): string; override;
    function GetObsValue(ObsTypeIndex: integer): double; override;
  end;

  { TSutraSpecifiedConcentrationOutputFile }

  TSutraSpecifiedConcentrationOutputFile = class(TCustomNodeOutputFile)
  protected
    function NumberOfValuesPerLine: Integer; override;
    function GetObsID(NodeNumber: string; ObsTypeIndex: integer): string; override;
    function GetObsValue(ObsTypeIndex: integer): double; override;
  end;

  { TSutraGeneralizedFlowOutputFile }

  TSutraGeneralizedFlowOutputFile = class(TCustomNodeOutputFile)
  protected
    function NumberOfValuesPerLine: Integer; override;
    function GetObsID(NodeNumber: string; ObsTypeIndex: integer): string; override;
    function GetObsValue(ObsTypeIndex: integer): double; override;
  end;

  { TSutraGeneralizedTransportOutputFile }

  TSutraGeneralizedTransportOutputFile = class(TCustomNodeOutputFile)
  protected
    function NumberOfValuesPerLine: Integer; override;
    function GetObsID(NodeNumber: string; ObsTypeIndex: integer): string; override;
    function GetObsValue(ObsTypeIndex: integer): double; override;
  end;

implementation

resourcestring
  rsTheIdentifie = 'The identifier %0:s in %1:s duplicates another identifier '
    +'in %2:s';

{ TSutraGeneralizedTransportOutputFile }

function TSutraGeneralizedTransportOutputFile.NumberOfValuesPerLine: Integer;
begin
  result := 2;
end;

function TSutraGeneralizedTransportOutputFile.GetObsID(NodeNumber: string;
  ObsTypeIndex: integer): string;
begin
  case ObsTypeIndex of
    0:
      begin
        result := NodeNumber + '_UGR'
      end;
    1:
      begin
        result := NodeNumber + '_UGU'
      end;
  else
    Assert(False);
  end;

end;

function TSutraGeneralizedTransportOutputFile.GetObsValue(ObsTypeIndex: integer
  ): double;
begin
  Assert(ObsTypeIndex in [0..1]);
  result := StrToFloat(FSplitter[ObsTypeIndex]);
  //if FSplitter[1] = 'INP' then
  //begin
  //  case ObsTypeIndex of
  //    0:
  //      begin
  //        result := StrToFloat(FSplitter[3]);
  //      end;
  //    1:
  //      begin
  //        result := StrToFloat(FSplitter[4]);
  //      end;
  //  else
  //    Assert(False);
  //  end;
  //end
  //else
  //begin
  //  Assert(FSplitter[1] = 'BCS');
  //  case ObsTypeIndex of
  //    0:
  //      begin
  //        result := StrToFloat(FSplitter[5]);
  //      end;
  //    1:
  //      begin
  //        result := StrToFloat(FSplitter[6]);
  //      end;
  //  else
  //    Assert(False);
  //  end;
  //end;
end;

{ TSutraGeneralizedFlowOutputFile }

function TSutraGeneralizedFlowOutputFile.NumberOfValuesPerLine: Integer;
begin
  result := 3;
end;

function TSutraGeneralizedFlowOutputFile.GetObsID(NodeNumber: string;
  ObsTypeIndex: integer): string;
begin
  case ObsTypeIndex of
    0:
      begin
        result := NodeNumber + '_PGF'
      end;
    1:
      begin
        result := NodeNumber + '_PGU'
      end;
    2:
      begin
        result := NodeNumber + '_PGR'
      end;
  else
    Assert(False);
  end;
end;

function TSutraGeneralizedFlowOutputFile.GetObsValue(ObsTypeIndex: integer
  ): double;
begin
  Assert(ObsTypeIndex in [0..2]);
  result := StrToFloat(FSplitter[ObsTypeIndex]);
  //if FSplitter[1] = 'INP' then
  //begin
  //  case ObsTypeIndex of
  //    0:
  //      begin
  //        result := StrToFloat(FSplitter[3]);
  //      end;
  //    1:
  //      begin
  //        result := StrToFloat(FSplitter[4]);
  //      end;
  //    2:
  //      begin
  //        result := StrToFloat(FSplitter[5]);
  //      end;
  //  else
  //    Assert(False);
  //  end;
  //end
  //else
  //begin
  //  Assert(FSplitter[1] = 'BCS');
  //  case ObsTypeIndex of
  //    0:
  //      begin
  //        result := StrToFloat(FSplitter[5]);
  //      end;
  //    1:
  //      begin
  //        result := StrToFloat(FSplitter[6]);
  //      end;
  //    2:
  //      begin
  //        result := StrToFloat(FSplitter[7]);
  //      end;
  //  else
  //    Assert(False);
  //  end;
  //end;
end;

{ TSutraSpecifiedConcentrationOutputFile }

function TSutraSpecifiedConcentrationOutputFile.NumberOfValuesPerLine: Integer;
begin
  result := 1;
end;

function TSutraSpecifiedConcentrationOutputFile.GetObsID(NodeNumber: string;
  ObsTypeIndex: integer): string;
begin
  Assert(ObsTypeIndex = 0);
  result := NodeNumber + '_UU';
end;

function TSutraSpecifiedConcentrationOutputFile.GetObsValue(
  ObsTypeIndex: integer): double;
begin
  Assert(ObsTypeIndex = 0);
  result := StrToFloat(FSplitter[0]);
end;

{ TSutraFluidSourceSinkOutputFile }

function TSutraFluidSourceSinkOutputFile.NumberOfValuesPerLine: Integer;
begin
  result := 2;
end;

function TSutraFluidSourceSinkOutputFile.GetObsID(NodeNumber: string;
  ObsTypeIndex: integer): string;
begin
  case ObsTypeIndex of
    0:
      begin
        result := NodeNumber + '_FU'
      end;
    1:
      begin
        result := NodeNumber + '_FR'
      end;
  else
    Assert(False);
  end;
end;

function TSutraFluidSourceSinkOutputFile.GetObsValue(ObsTypeIndex: integer
  ): double;
begin
  Assert(ObsTypeIndex in [0..1]);
  result := StrToFloat(FSplitter[ObsTypeIndex+1]);
  //if FSplitter[1] <> 'BCS' then
  //begin
  //  case ObsTypeIndex of
  //    0:
  //      begin
  //        result := StrToFloat(FSplitter[4]);
  //      end;
  //    1:
  //      begin
  //        result := StrToFloat(FSplitter[5]);
  //      end;
  //  else
  //    Assert(False);
  //  end;
  //end
  //else
  //begin
  //  case ObsTypeIndex of
  //    0:
  //      begin
  //        result := StrToFloat(FSplitter[5]);
  //      end;
  //    1:
  //      begin
  //        result := StrToFloat(FSplitter[6]);
  //      end;
  //  else
  //    Assert(False);
  //  end;
  //end;
end;

{ TCustomNodeOutputFile }

procedure TCustomNodeOutputFile.ReadHeader;
begin

end;

function TCustomNodeOutputFile.GetNumberOfValues(CurrentLines: TStringList
  ): Integer;
begin
  result := CurrentLines.Count * NumberOfValuesPerLine;
end;

function TCustomNodeOutputFile.LineIsStartOfData(ALine: string): Boolean;
begin
  Result := Pos('##   Node    Defined in', ALine) = 1;
end;

procedure TCustomNodeOutputFile.ExtractValues(ALine: string);
begin
  ALine := Copy(ALine, 75, MAXINT);
  FSplitter.DelimitedText := ALine;
end;

constructor TCustomNodeOutputFile.Create(AFileName: string;
  IdLocations: TObservationDictionary);
begin
  FSplitter := TStringList.Create;
  FNumberOfValues := -1;
  inherited Create(AFileName, ftText, IdLocations);
end;

destructor TCustomNodeOutputFile.Destroy;
begin
  FSplitter.Free;
  inherited Destroy;
end;

procedure TCustomNodeOutputFile.ReadTimeAndValues;
var
  ALine: string;
  CurrentLines: TStringList;
  Values: TDoubleArray;
  NewIds: Boolean;
  ObsIndex: Integer;
  LineIndex: Integer;
  FileID: TFileId;
  ATime: double;
  NodeNumber: string;
  ID: string;
  ObsTypeIndex: Integer;
  procedure AddKey;
  begin
    if NewIds then
    begin
      if FIdLocations.TryGetValue(ID, FileID) then
       begin
         raise EReadOutputError.Create(Format(rsTheIdentifie, [ID, FileName,
           FileID.OutputFile.FileName]));
       end
       else
       begin
         FileID.OutputFile := self;
         FileID.Key := ID;
         FileID.Position := ObsIndex;
         FIdLocations.Add(ID, FileID);
       end;
    end
    else
    begin
      if FIdLocations.TryGetValue(ID, FileID) then
      begin
        Assert(FileID.OutputFile = self);
        Assert(FileID.Position = ObsIndex);
      end
      else
      begin
        Assert(False, Format('ID = "%s" not found.', [ID]));
      end;
    end;
  end;
begin
  Values := nil;
  ATime := -1;
  Assert(FileType = ftText);
  repeat
    Readln(FTextFile, ALine);
    if Pos('## TIME STEP', ALine) = 1 then
    begin
      FSplitter.DelimitedText := ALine;
      ATime := StrToFloat(FSplitter[FSplitter.Count-2]);
    end
    else if LineIsStartOfData(ALine) then
    begin
      CurrentLines := TStringList.Create;
      try
        repeat
          Readln(FTextFile, ALine);
          if (Pos('##', ALine) = 1) or (ALine = '') then
          begin
            break;
          end;
          CurrentLines.Add(ALine);
        until EOF(FTextFile);

        NewIds := FNumberOfValues = -1;
        if NewIds then
        begin
          FNumberOfValues := GetNumberOfValues(CurrentLines);
        end
        else
        begin
          Assert(FNumberOfValues = GetNumberOfValues(CurrentLines));
        end;
        SetLength(Values, FNumberOfValues);

        ObsIndex := 0;
        for LineIndex := 0 to Pred(CurrentLines.Count) do
        begin
          ALine := CurrentLines[LineIndex];
          FSplitter.DelimitedText := ALine;
          NodeNumber := UpperCase(FSplitter[0]);
          ExtractValues(ALine);

          for ObsTypeIndex := 0 to Pred(NumberOfValuesPerLine) do
          begin
            ID := GetObsID(NodeNumber, ObsTypeIndex);
            AddKey;
            Values[ObsIndex] := GetObsValue(ObsTypeIndex);
            Inc(ObsIndex);
          end;
        end;
      finally
        CurrentLines.Free;
      end;
      break;
    end;

  until EOF(FTextFile);
  UpdateStoredValues(ATime, Values);
end;

{ TSutraSpecifiedPressureOutputFile }

function TSutraSpecifiedPressureOutputFile.NumberOfValuesPerLine: Integer;
begin
  result := 3;
end;

function TSutraSpecifiedPressureOutputFile.GetObsID(NodeNumber: string;
  ObsTypeIndex: integer): string;
begin
  case ObsTypeIndex of
    0:
      begin
        result := NodeNumber + '_PF'
      end;
    1:
      begin
        result := NodeNumber + '_PU'
      end;
    2:
      begin
        result := NodeNumber + '_PR'
      end;
  else
    Assert(False);
  end;
end;

function TSutraSpecifiedPressureOutputFile.GetObsValue(ObsTypeIndex: integer
  ): double;
begin
  Assert(ObsTypeIndex in [0..2]);
  result := StrToFloat(FSplitter[ObsTypeIndex]);
  //if FSplitter[1] <> 'BCS' then
  //begin
  //  case ObsTypeIndex of
  //    0:
  //      begin
  //        result := StrToFloat(FSplitter[3]);
  //      end;
  //    1:
  //      begin
  //        result := StrToFloat(FSplitter[4]);
  //      end;
  //    2:
  //      begin
  //        result := StrToFloat(FSplitter[5]);
  //      end;
  //  else
  //    Assert(False);
  //  end;
  //end
  //else
  //begin
  //  case ObsTypeIndex of
  //    0:
  //      begin
  //        result := StrToFloat(FSplitter[4]);
  //      end;
  //    1:
  //      begin
  //        result := StrToFloat(FSplitter[5]);
  //      end;
  //    2:
  //      begin
  //        result := StrToFloat(FSplitter[6]);
  //      end;
  //  else
  //    Assert(False);
  //  end;
  //end;
end;

{ TSutraLakeStageOutputFile }

function TSutraLakeStageOutputFile.NumberOfValuesPerLine: Integer;
begin
  result := 1;
end;

function TSutraLakeStageOutputFile.LineIsStartOfData(ALine: string): Boolean;
begin
  result := ALine = '##   Node          Stage          Depth'
end;

function TSutraLakeStageOutputFile.GetObsID(NodeNumber: string;
  ObsTypeIndex: integer): string;
begin
  Assert(ObsTypeIndex = 0);
  result := NodeNumber + '_LKST';
end;

function TSutraLakeStageOutputFile.GetObsValue(ObsTypeIndex: integer): double;
begin
  Assert(ObsTypeIndex = 0);
  result := StrToFloat(FSplitter[1]);
end;

procedure TSutraLakeStageOutputFile.ExtractValues(ALine: string);
begin
  FSplitter.DelimitedText := ALine;
end;

{ TSutraObsOutputFile }

procedure TSutraObsOutputFile.ReadHeader;
begin
  // do nothing
end;

procedure TSutraObsOutputFile.ReadTimeAndValues;
var
  ALine: string;
  CurrentLines: TStringList;
  NamePos: Integer;
  XPos: Integer;
  YPos: Integer;
  PressurePos: Integer;
  ConcTempPos: Integer;
  SatPos: Integer;
  Values: TDoubleArray;
  NewIds: Boolean;
  ObsIndex: Integer;
  LineIndex: Integer;
  ObsNameRoot: String;
  ObsName: string;
  FileID: TFileId;
  ATime: double;
  LocationID: TLocationID;
  procedure AddKey;
  begin
    if NewIds then
    begin
      if FIdLocations.TryGetValue(ObsName, FileID) then
       begin
         raise EReadOutputError.Create(Format(rsTheIdentifie, [ObsName, FileName,
           FileID.OutputFile.FileName]));
       end
       else
       begin
         FileID.OutputFile := self;
         FileID.Key := ObsName;
         FileID.Position := ObsIndex;
         FIdLocations.Add(ObsName, FileID);

         LocationID.ID := ObsName;
         FLocationDictionary.Add(ObsName, LocationID);
       end;
    end
    else
    begin
      if FIdLocations.TryGetValue(ObsName, FileID) then
      begin
        Assert(FileID.OutputFile = self);
        Assert(FileID.Position = ObsIndex);
      end
      else
      begin
        Assert(False);
      end;
    end;
  end;
begin
  Values := nil;
  ATime := -1;
  Assert(FileType = ftText);
  repeat
    Readln(FTextFile, ALine);
    if Pos('## TIME STEP', ALine) = 1 then
    begin
      FSplitter.DelimitedText := ALine;
      ATime := StrToFloat(FSplitter[FSplitter.Count-2]);
    end
    else if Pos('##                                  Name', ALine) = 1 then
    begin
      FSplitter.DelimitedText := ALine;
      FSplitter.Delete(0);
      NamePos := 0;
      XPos := 1;
      YPos := 2;
      PressurePos := FSplitter.IndexOf('Pressure');
      ConcTempPos := PressurePos + 1;
      SatPos := PressurePos + 2;
      CurrentLines := TStringList.Create;
      try
        repeat
          Readln(FTextFile, ALine);
          if (Pos('##', ALine) = 1) or (ALine = '') then
          begin
            break;
          end;
          CurrentLines.Add(ALine);
        until EOF(FTextFile);

        NewIds := FNumberOfValues = -1;
        if NewIds then
        begin
          FNumberOfValues := CurrentLines.Count * 3;
        end
        else
        begin
          Assert(FNumberOfValues = CurrentLines.Count * 3);
        end;
        SetLength(Values, FNumberOfValues);

        ObsIndex := 0;
        for LineIndex := 0 to Pred(CurrentLines.Count) do
        begin
          FSplitter.DelimitedText := CurrentLines[LineIndex];
          ObsNameRoot := UpperCase(FSplitter[NamePos]);

          LocationID.APoint.X := StrToFloat(FSplitter[XPos]);
          LocationID.APoint.Y := StrToFloat(FSplitter[YPos]);

          ObsName := ObsNameRoot + '_P';
          AddKey;
          Values[ObsIndex] := StrToFloat(FSplitter[PressurePos]);
          Inc(ObsIndex);

          ObsName := ObsNameRoot + '_U';
          AddKey;
          Values[ObsIndex] := StrToFloat(FSplitter[ConcTempPos]);
          Inc(ObsIndex);

          ObsName := ObsNameRoot + '_S';
          AddKey;
          Values[ObsIndex] := StrToFloat(FSplitter[SatPos]);
          Inc(ObsIndex);
        end;
      finally
        CurrentLines.Free;
      end;
      break;
    end;

  until EOF(FTextFile);
  UpdateStoredValues(ATime, Values);
end;

constructor TSutraObsOutputFile.Create(AFileName: string;
  IdLocations: TObservationDictionary; LocationDictionary: TLocationDictionary);
begin
  FLocationDictionary := LocationDictionary;
  FSplitter := TStringList.Create;
  FNumberOfValues := -1;
  inherited Create(AFileName, ftText, IdLocations);
end;

destructor TSutraObsOutputFile.Destroy;
begin
  FSplitter.Free;
  inherited Destroy;
end;

end.

