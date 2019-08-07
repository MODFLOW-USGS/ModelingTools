unit IndexerUnit;

interface

uses
  System.Classes, System.SyncObjs, SearchTrie, frmListAnalyzerUnit,
  frameFileListHandlerUnit, FileIndexUnit, System.SysUtils;

var
  // protect access to LinesToIndex, AllLines, and FListFile.
  LinesCritSect: TCriticalSection;
  LinesToIndex: TStringList;
  // The AllLines event is set when all lines have been added to LinesToIndex.
  AllLines: boolean = False;
  FListFile: TFileIndex;

type
  TIndexingThread = class(TThread)
  private
    FSearcher: TModflowSearchTrie;
    FMinLength: integer;
    Indent: Integer;
    SomeLines: TStringList;
    NewTimeStepPostions: TListInt64;
    NewTimeStepLines: TListInt64;
    LineIndex: integer;
    ALine: string;
    FAmountToIndent: Integer;
    FCurrentFrame: TframeFileListHandler;
    LineNumber: Integer;
    FAddLineResult: Integer;
    MultipleLines: string;
    procedure InitializeSearchTrie;
    procedure IndexSomeLines;
    function SearchTrie(ALine: AnsiString; var IndexTypes: TIndexTypeArray;
      var KeyLength: integer): boolean;
    procedure AddLine;
    procedure UpdateTimeSteps;
    procedure BeginUpdate;
    procedure EndUpdate;
  protected
    procedure Execute; override;
  public
    frameWarning: TframeFileListHandler;
    frameErrors: TframeFileListHandler;
    frameListing: TframeFileListHandler;
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
  end;

implementation

uses
  ErrorMessages, ModflowIdentifiersUnit, System.StrUtils, BMSearch;

{ TIndexingThread }

procedure TIndexingThread.AddLine;
begin
  FAddLineResult := FCurrentFrame.AddLine(LineNumber, ALine, FAmountToIndent);
end;

procedure TIndexingThread.BeginUpdate;
begin
  frameWarning.BeginUpdate;
  frameErrors.BeginUpdate;
  frameListing.BeginUpdate;
end;

constructor TIndexingThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FSearcher := TModflowSearchTrie.Create;
  InitializeSearchTrie;
  SomeLines := TStringList.Create;
  NewTimeStepPostions := TListInt64.Create;
  NewTimeStepLines := TListInt64.Create;
end;

destructor TIndexingThread.Destroy;
begin
  NewTimeStepLines.Free;
  NewTimeStepPostions.Free;
  SomeLines.Free;
  FSearcher.Free;
  inherited;
end;

procedure TIndexingThread.EndUpdate;
begin
  frameWarning.EndUpdate;
  frameErrors.EndUpdate;
  frameListing.EndUpdate;
end;

procedure TIndexingThread.Execute;
var
//  AString: string;
  index: Integer;
begin
  Synchronize(BeginUpdate);
  try
    index := 0;
    while True do
    begin
      LinesCritSect.Enter;
      if index < LinesToIndex.Count then
      begin
//        While index < LinesToIndex.Count do
//        begin
          MultipleLines := LinesToIndex[index];
          LinesToIndex[index] := '';
        LinesCritSect.Leave;
  //        LinesToIndex.Delete(0);
          Inc(index);
    //      SomeLines.Text := AString;
  //        MultipleLines := AString;
          IndexSomeLines;
//        end;
//        LinesCritSect.Leave;
      end
      else if AllLines then
      begin
        LinesCritSect.Leave;
        break;
      end
      else
      begin
        LinesCritSect.Leave;
//        Sleep(20);
      end;
    end;
    UpdateTimeSteps;
  finally
    Synchronize(EndUpdate);
  end;
end;

procedure TIndexingThread.IndexSomeLines;
var
//  it: TIndexType;
  KeyLength: Integer;
  TrimmedLine: string;
//  ALine: string;
  NewTimeStepPostion: Integer;
  RechBudPos: Integer;
  InnerLineIndex: Integer;
  IndexTypes: TIndexTypeArray;
  InnerLine: AnsiString;
//  NumOK: Boolean;
//  IdIndex: Integer;
//  SpacePos: Integer;
  AnsiMultipleLines: AnsiString;
  CheckNumLines: Boolean;
  IdIndex: Integer;
  NumOK: Boolean;
begin
  AnsiMultipleLines := AnsiString(MultipleLines);
  CheckNumLines := False;
  for IdIndex := 0 to NumberErrorValues.Count - 1 do
  begin
    if BMPos(NumberErrorValues[IdIndex], AnsiMultipleLines) >= 1 then
    begin
      CheckNumLines := True;
      Break;
    end;
  end;
  if not CheckNumLines then
  begin
    for IdIndex := 0 to NumberBoundaryIdentifiers.Count - 1 do
    begin
      if BMPos(NumberBoundaryIdentifiers[IdIndex], AnsiMultipleLines) >= 1 then
      begin
        CheckNumLines := True;
        Break;
      end;
    end;
  end;

  SomeLines.Text := MultipleLines;
  for InnerLineIndex := 0 to SomeLines.Count - 1 do
  begin
    ALine := SomeLines[InnerLineIndex];
    // Putting the detection of lines to index into a subroutine
    // increases the time required by ~75%.

//  IndexALine(Indent, NewTimeStepLines, NewTimeStepPostions, ALine, LineIndex);

    TrimmedLine := Trim(ALine);
    if TrimmedLine = '' then
    begin
      Continue;
    end;
    // There are only a few cases where a line containing a key
    // starts with a number so skip most lines that start with a number.
    InnerLine := AnsiString(ALine);
    if CharInSet(TrimmedLine[1], ['0'..'9', '-']) then
    begin
      if CheckNumLines then
      begin
        NumOK := False;
        for IdIndex := 0 to NumberErrorValues.Count - 1 do
        begin
          if BMPos(NumberErrorValues[IdIndex], InnerLine) >= 1 then
          begin
            NumOK := True;
            Break;
          end;
        end;
        if not NumOK then
        begin
          for IdIndex := 0 to NumberBoundaryIdentifiers.Count - 1 do
          begin
            if BMPos(NumberBoundaryIdentifiers[IdIndex], InnerLine) >= 1 then
            begin
              NumOK := True;
              Break;
            end;
          end;
        end;
        if not NumOK then
        begin
          Continue;
        end;
      end
      else
      begin
        Continue
      end;
    end;
//    InnerLine := AnsiString(ALine);
    if SearchTrie(InnerLine, IndexTypes, KeyLength) then
    begin
      if (itNonID = IndexTypes[0]) or (itNonID = IndexTypes[1]) then
      begin
        Continue;
      end;
      // Skip "RECHARGE" when it is in the volumetric budget.
      // "RECHARGE" is repeated twice in the volumetric budget
      // which allows it to be identified.
      RechBudPos := Pos(StrRECHARGE, ALine);
      if RechBudPos >= 1 then
      begin
        RechBudPos := PosEx(StrRECHARGE, ALine, RechBudPos + 1);
        if RechBudPos >= 1 then
        begin
          Continue;
        end;
      end;

      LineNumber := LineIndex + InnerLineIndex;
      case IndexTypes[0] of
        itNone:
          Assert(False);
        itPackage:
          begin
            FAmountToIndent := 0;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
//            frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 0);
            Indent := 1;
          end;
        itObservation:
          begin
            FAmountToIndent := 0;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
//            frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 0);
          end;
        itBoundary, itNumberBoundary:
          begin
            FAmountToIndent := Indent;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
//            frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
          end;
        itWarning:
          begin
            FAmountToIndent := 0;
            FCurrentFrame := frameWarning;
            Synchronize(AddLine);
//            frameWarning.AddLine(LineIndex + InnerLineIndex, ALine, 0);
          end;
        itError:
          begin
            FAmountToIndent := 0;
            FCurrentFrame := frameErrors;
            Synchronize(AddLine);
//            frameErrors.AddLine(LineIndex + InnerLineIndex, ALine, 0);
          end;
        itIndent1:
          begin
            FAmountToIndent := 1;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
//            frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 1);
            Indent := 1;
          end;
        itIndent3:
          begin
            FAmountToIndent := 2;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
//            frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 2);
            Indent := 3;
          end;
        itNoIndentChange:
          begin
            FAmountToIndent := 1;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
//            frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 1);
          end;
        itStressPeriod:
          begin
            if (itStressPeriod2 = IndexTypes[1]) then
            begin
              FAmountToIndent := 0;
              FCurrentFrame := frameListing;
              Synchronize(AddLine);
//              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 0);
              Indent := 1;
            end;
          end;
        itStressPeriod2:
          begin
            // do nothing
          end;
        itStartTimeStep:
          begin
            FAmountToIndent := 1;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
            NewTimeStepPostion := FAddLineResult;
//          NewTimeStepPostion := frameListing.AddLine(
//              LineIndex + InnerLineIndex, ALine, 1);
            Indent := 2;
            NewTimeStepLines.Add(LineIndex + InnerLineIndex);
            NewTimeStepPostions.Add(NewTimeStepPostion);
          end;
        itIteration:
          begin
            if (itIteration2 = IndexTypes[1]) then
            begin
              FAmountToIndent := Indent;
              FCurrentFrame := frameListing;
              Synchronize(AddLine);
//              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
            end;
          end;
        itIteration2:
          begin
            // do nothing
          end;
        itArray1:
          begin
            if (itArray2 = IndexTypes[1]) then
            begin
              FAmountToIndent := Indent;
              FCurrentFrame := frameListing;
              Synchronize(AddLine);
//              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
            end;
          end;
        itArray2:
          begin
            // do nothing
          end;
        itArray3:
          begin
            FAmountToIndent := Indent;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
//            frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
          end;
        itEndModel:
          begin
            FAmountToIndent := 0;
            FCurrentFrame := frameListing;
            Synchronize(AddLine);
//            frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 0);
          end;
        itNonID:
          begin
            Assert(False);
          end;
      else
        Assert(False);
      end;
    end;
  end;
  Inc(LineIndex, SomeLines.Count);
end;

procedure TIndexingThread.UpdateTimeSteps;
var
  ALine: string;
  SPFound: Boolean;
  TempLines: TStringList;
  StartLine: Int64;
  SP_Start: Integer;
  TS_Start: Integer;
  StepIndex: Integer;
  Local_LineIndex: Integer;
begin
  TempLines := TStringList.Create;
  try
    for StepIndex := 0 to NewTimeStepPostions.Count - 1 do
    begin
      StartLine := NewTimeStepLines[StepIndex];
      SPFound := False;
      repeat
        LinesCritSect.Enter;
        try
        FListFile.ReadLines(TempLines, StartLine, LinesToRead);
        finally
          LinesCritSect.Leave;
        end;
        for Local_LineIndex := 0 to TempLines.Count - 1 do
        begin
          ALine := TempLines[Local_LineIndex];
          SP_Start := Pos('STRESS PERIOD', ALine);
          if SP_Start > 0 then
          begin
            TS_Start := Pos('TIME STEP', ALine);
            if TS_Start > 0 then
            begin
              if TS_Start > SP_Start then
              begin
                ALine := Copy(ALine, SP_Start, MAXINT);
              end
              else
              begin
                ALine := Copy(ALine, TS_Start, MAXINT);
              end;
              frameListing.AddToLine(NewTimeStepPostions[StepIndex], ALine);
              SPFound := True;
              break;
            end;
          end;
        end;
        Inc(StartLine, LinesToRead);
      until SPFound or (TempLines.Count = 0);
    end;
  finally
    TempLines.Free;
  end;
end;

procedure TIndexingThread.InitializeSearchTrie;
  procedure AddKey(const AString: AnsiString; IndexType: TIndexType);
  begin
    FSearcher.AddKey(PAnsiChar(AString), IndexType);
    if Length(AString) < FMinLength then
    begin
      FMinLength := Length(AString);
    end;
  end;
var
  index: Integer;
begin
  FMinLength := Length(ErrorValues[0]);
  for index := 0 to ErrorValues.Count - 1 do
  begin
    AddKey((ErrorValues[index]), itError);
  end;
  for index := 0 to NumberErrorValues.Count - 1 do
  begin
    AddKey((NumberErrorValues[index]), itError);
  end;
  for index := 0 to WarningValues.Count - 1 do
  begin
    AddKey((WarningValues[index]), itWarning);
  end;
  for index := 0 to PackageIdentifiers.Count - 1 do
  begin
    AddKey((PackageIdentifiers[index]), itPackage);
  end;
  for index := 0 to ObsIdentifiers.Count - 1 do
  begin
    AddKey((ObsIdentifiers[index]), itObservation);
  end;
  for index := 0 to BoundaryIdentifiers.Count - 1 do
  begin
    AddKey((BoundaryIdentifiers[index]), itBoundary);
  end;
  for index := 0 to NumberBoundaryIdentifiers.Count - 1 do
  begin
    AddKey((NumberBoundaryIdentifiers[index]), itNumberBoundary);
  end;
  for index := 0 to NonIdentifiers.Count - 1 do
  begin
    AddKey((NonIdentifiers[index]), itNonID);
  end;

  AddKey(StressPeriodID1, itStressPeriod);
  AddKey(StressPeriodID2, itStressPeriod2);
  AddKey(StartNewTimeStep, itStartTimeStep);
  AddKey(StrParameterName, itNoIndentChange);
  AddKey(StrINSTANCE, itIndent3);
  AddKey(StrParameter, itNoIndentChange);
  AddKey(TransportStep, itIndent3);
  AddKey(StrINSTANCE2, itIndent3);
  AddKey(StrCLASSIFICATIONCOU, itIndent1);
  AddKey(IterationID1, itIteration);
  AddKey(IterationID2, itIteration2);
  AddKey(ArrayID1, itArray1);
  AddKey(ArrayID2, itArray2);
  AddKey(ArrayID3, itArray3);
  AddKey(ArrayID4, itArray3);
  AddKey(BudgetID, itArray3);
  AddKey(TimeSummary, itArray3);
  AddKey(EndModel, itEndModel);

//  for index := 0 to KeyWords.Count - 1 do
//  begin
//    FQuickSearcher.AddKey(PAnsiChar(KeyWords[index]), True);
//  end;

end;

function TIndexingThread.SearchTrie(ALine: AnsiString;
  var IndexTypes: TIndexTypeArray; var KeyLength: integer): boolean;
const
  DontBreak = [itStressPeriod, itIteration, itArray1, itNonID];
var
  Key: PAnsiChar;
  IndexType: TIndexType;
  TestLength: Integer;
  Index: Integer;
begin
  Index := 0;
  result := False;
  Key := PAnsiChar(ALine);
  TestLength := Length(ALine);
  IndexTypes[1] := itNone;
  while (TestLength >= FMinLength) and (Key^ <> AnsiChar(0)) do
  begin
    if FSearcher.Find(Key, IndexType, KeyLength) then
    begin
      IndexTypes[Index] := IndexType;
      Inc(Index);
      result := True;
      if not (IndexType in DontBreak) then
      begin
        break;
      end;
      if Index = 2 then
      begin
        break;
      end;
    end;

    Inc(Key);
    Dec(TestLength);
  end;
end;

initialization
  LinesCritSect := TCriticalSection.Create;
//  AllLines := TEvent.Create;
  LinesToIndex := TStringList.Create;


finalization
  LinesToIndex.Free;
//  AllLines.Free////////;
  LinesCritSect.Free;

end.
