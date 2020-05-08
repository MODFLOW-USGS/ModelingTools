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

  { TSutraLakeStageOutputFile }

  TSutraLakeStageOutputFile = class(TCustomOutputFile)
  protected
    FSplitter: TStringList;
    FNumberOfValues: Integer;
    procedure ReadHeader; override;
  public
    procedure ReadTimeAndValues; override;
    constructor Create(AFileName: string;
      IdLocations: TObservationDictionary);
    destructor Destroy; override;
  end;

implementation

resourcestring
  rsTheIdentifie = 'The identifier %0:s in %1:s duplicates another identifier '
    +'in %2:s';

{ TSutraLakeStageOutputFile }

procedure TSutraLakeStageOutputFile.ReadHeader;
begin

end;

procedure TSutraLakeStageOutputFile.ReadTimeAndValues;
var
  ALine: string;
  CurrentLines: TStringList;
  Values: TDoubleArray;
  NewIds: Boolean;
  ObsIndex: Integer;
  LineIndex: Integer;
  //ObsName: string;
  FileID: TFileId;
  ATime: double;
  NodeNumber: string;
  procedure AddKey;
  begin
    if NewIds then
    begin
      if FIdLocations.TryGetValue(NodeNumber, FileID) then
       begin
         raise EReadOutputError.Create(Format(rsTheIdentifie, [NodeNumber, FileName,
           FileID.OutputFile.FileName]));
       end
       else
       begin
         FileID.OutputFile := self;
         FileID.Key := NodeNumber;
         FileID.Position := ObsIndex;
         FIdLocations.Add(NodeNumber, FileID);
       end;
    end
    else
    begin
      if FIdLocations.TryGetValue(NodeNumber, FileID) then
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
    else if ALine = '##   Node          Stage          Depth' then
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

        NewIds := FNumberOfValues = 0;
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
          NodeNumber := UpperCase(FSplitter[0]);

          //ObsName := 'Stage_' + NodeNumber;
          AddKey;
          Values[ObsIndex] := StrToFloat(FSplitter[1]);
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

constructor TSutraLakeStageOutputFile.Create(AFileName: string;
  IdLocations: TObservationDictionary);
begin
  FSplitter := TStringList.Create;
  FNumberOfValues := 0;
  inherited Create(AFileName, ftText, IdLocations);
end;

destructor TSutraLakeStageOutputFile.Destroy;
begin
  FSplitter.Free;
  inherited Destroy;
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

        NewIds := FNumberOfValues = 0;
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
  FNumberOfValues := 0;
  inherited Create(AFileName, ftText, IdLocations);
end;

destructor TSutraObsOutputFile.Destroy;
begin
  FSplitter.Free;
  inherited Destroy;
end;

end.

