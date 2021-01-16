unit FileIndexUnit;

interface

uses
  Winapi.Windows, Classes, IOUtils, Generics.Collections, SysUtils;

type
  TIntList = class (TList<Int64>);
  TListInt64 = TList<Int64>;


  TOnProgress = procedure (Sender: TObject; PerMil: integer) of object;

  TOnIndexSomeLines = procedure (const MultipleLines: string; var Indent: Integer;
    NewTimeStepPostions, NewTimeStepLines: TListInt64; var LineIndex: Integer)
    of Object;

  TOnUpdateTimeSteps = procedure (NewTimeStepLines,
    NewTimeStepPostions: TListInt64) of object;


  TFileIndex = class(TObject)
  private
    FFile: TStreamReader;
    FPositions: TIntList;
    FFileName: string;
    FOnProgress: TOnProgress;
    FCurrentStartLine: integer;
    FAbort: Boolean;
    FOnStartLineChange: TNotifyEvent;
    FOnUpdateTimeSteps: TOnUpdateTimeSteps;
    FOnIndexSomeLines: TOnIndexSomeLines;
//    FIndexer: TThread;
    LineBreakLength: Integer;
    FFileSize: Int64;
    FFileDate: Extended;
    function GetLine(Index: integer): string;
    function GetLineCount: integer;
    procedure SetFileName(const Value: string);
    procedure SetOnStartLineChange(const Value: TNotifyEvent);
    procedure GetLineStartingPositions(Offset: Int64;
      LineStarts: TIntList;
      const Text: string);
  public
    Constructor Create;
    property FileName: string read FFileName write SetFileName;
    Destructor Destroy; override;
    property Line[Index: integer]: string read GetLine; default;
    function  ReadLines(Lines: TStrings; StartLine, Count: integer;
      AdjustStart: Boolean = False): boolean; overload;
//    function ReadLines(Lines: TStringBuilder; StartLine, Count: integer;
//      AdjustStart: Boolean = False): boolean;overload;
    procedure ReadLines(var Buffer: TCharArray; StartLine, Count: integer;
      AdjustStart: Boolean = False); overload;
    procedure ReadLines(var AString: string; StartLine, Count: integer;
      AdjustStart: Boolean = False); overload;
    property LineCount: integer read GetLineCount;
    function ReadLine: string;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    procedure GoToStart;
    property CurrentStartLine: integer read FCurrentStartLine;
    procedure Abort;
    property OnStartLineChange: TNotifyEvent read FOnStartLineChange
      write SetOnStartLineChange;
    function GetNewLineFromOffset(Line, Offset: integer): integer;
    property OnIndexSomeLines: TOnIndexSomeLines read FOnIndexSomeLines
      write FOnIndexSomeLines;
    property OnUpdateTimeSteps: TOnUpdateTimeSteps read FOnUpdateTimeSteps
      write FOnUpdateTimeSteps;
    function TryReopenFile: boolean;
    procedure CloseFile;
    function DateChanged: Boolean;
//    property Indexer: TThread read FIndexer write FIndexer;
  end;

implementation

uses
  Forms, System.StrUtils, Vcl.Dialogs, System.Math;

{ TFileIndex }

procedure TFileIndex.Abort;
begin
  FAbort := True;
end;

procedure TFileIndex.ReadLines(var Buffer: TCharArray;
  StartLine, Count: integer; AdjustStart: Boolean = False);
var
  EndLine: Integer;
  BufLength: Integer;
begin
//  OutputDebugString('SAMPLING ON');
  EndLine := StartLine + Count;
  if StartLine >= FPositions.Count then
  begin
    StartLine := FPositions.Count - 1;
  end;
  if EndLine >= FPositions.Count then
  begin
    EndLine := FPositions.Count - 1;
  end;
  if EndLine = FPositions.Count - 1 then
  begin
    SetLength(Buffer, FFile.BaseStream.Size - FPositions[StartLine]);
  end
  else
  begin
    SetLength(Buffer, FPositions[EndLine] - FPositions[StartLine]);
  end;
  FFile.BaseStream.Position := FPositions[StartLine];
  FFile.DiscardBufferedData;
  BufLength := FFile.ReadBlock(Buffer, 0, Length(Buffer));
  if (BufLength <> Length(Buffer)) and (BufLength >= 0) then
  begin
    SetLength(Buffer, BufLength)
  end;

  if AdjustStart and (FCurrentStartLine <> StartLine) then
  begin
    FCurrentStartLine := StartLine;
    FOnStartLineChange(self);
  end;
//  OutputDebugString('SAMPLING OFF')
end;

procedure TFileIndex.CloseFile;
begin
  FreeAndNil(FFile);
end;

constructor TFileIndex.Create;
begin
  FPositions := TIntList.Create;
  FCurrentStartLine := -1;
end;

function TFileIndex.DateChanged: Boolean;
begin
  result := (FileName <> '')
    and (FFileDate <> TFile.GetLastWriteTimeUtc(FileName));
end;

destructor TFileIndex.Destroy;
begin
  FPositions.Free;
  FFile.Free;
  inherited;
end;

function TFileIndex.GetLine(Index: integer): string;
begin
  FFile.BaseStream.Position := FPositions[Index];
  FFile.DiscardBufferedData;
  result := FFile.ReadLine;
end;

function TFileIndex.GetLineCount: integer;
begin
  result := FPositions.Count;
end;

procedure TFileIndex.GoToStart;
begin
  FFile.BaseStream.Position := 0;
  FFile.DiscardBufferedData;
end;

function TFileIndex.ReadLine: string;
begin
  result := FFile.ReadLine;
end;

function TFileIndex.ReadLines(Lines: TStrings; StartLine, Count: integer;
  AdjustStart: Boolean = False): boolean;
var
  Buffer: string;
begin
  Lines.Clear;
  if StartLine >= FPositions.Count then
  begin
    Exit(False);
  end;
  Lines.Capacity := Count;
  ReadLines(Buffer, StartLine, Count, AdjustStart);

  Lines.Text := Buffer;

  result := True;
end;

{function TFileIndex.ReadLines(Lines: TStringBuilder; StartLine, Count: integer;
  AdjustStart: Boolean = False): boolean;
var
  Buffer: TCharArray;
begin
  Lines.Clear;
  if StartLine >= FPositions.Count then
  begin
    Exit(false);
  end;

  ReadLines(Buffer, StartLine, Count, AdjustStart);
  Lines.Append(Buffer);

  result := True;
end;  }

function FindLineBreak(const AString: AnsiString; StartPos: Integer): integer;
var
  P: PAnsiChar;
  Index: integer;
begin
  if (AString = '') then
  begin
    result := 0;
    Exit;
  end;
  P := Pointer(AString);
  Inc(P, StartPos-1);
  result := 0;
  for Index := StartPos to Length(AString) do
  begin
    if (P^ in [#10, #13]) then
    begin
      result := Index;
      Exit;
    end
    else
    begin
      Inc(P);
    end;
  end
end;


procedure TFileIndex.GetLineStartingPositions(Offset: Int64;
  LineStarts: TIntList; const Text: string);
var
  TextLength: Integer;
//  NextPos: PWideChar;
//  LineBreak: string;
//  StartChar: PWideChar;
//  InitialTextLength: Integer;
//  SearchStart: PWideChar;
//  SearchLength: Integer;
  AnsiText: AnsiString;
  NextLineBreak: integer;
  TestBreak: Integer;
begin
  AnsiText := AnsiString(Text);
  TextLength := Length(AnsiText);

//  TextLength := Length(Text);
//  InitialTextLength := TextLength;
//  StartChar := Addr(Text[1]);
//  NextPos := StartChar;

  if LineBreakLength = 0 then
  begin
    NextLineBreak := 1;
    NextLineBreak := FindLineBreak(AnsiText, NextLineBreak);
    Inc(NextLineBreak);
    TestBreak := NextLineBreak;
    if TestBreak = FindLineBreak(AnsiText, NextLineBreak) then
    begin
      LineBreakLength := 2;
    end
    else
    begin
      LineBreakLength := 1;
    end;


//    SearchLength := Min(250,TextLength);
//    repeat
//      SearchLength := SearchLength*2;
//      SearchLength := Min(SearchLength,TextLength);
//      LineBreak := sLineBreak;
//      SearchStart := NextPos;
//      NextPos := SearchBuf(SearchStart, SearchLength, 0, 0, LineBreak,
//        [soDown, soMatchCase]);
//      if NextPos = nil then
//      begin
//        LineBreak := #13;
//        NextPos := SearchBuf(SearchStart, SearchLength, 0, 0, LineBreak,
//          [soDown, soMatchCase]);
//      end;
//      if NextPos = nil then
//      begin
//        LineBreak := #10;
//        NextPos := SearchBuf(SearchStart, SearchLength, 0, 0, LineBreak,
//          [soDown, soMatchCase]);
//      end;
//      if NextPos <> nil then
//      begin
//        break;
//      end;
//    until SearchLength >= TextLength;
//
//    LineBreakLength := Length(LineBreak);
  end;

  NextLineBreak := 1;
  repeat
    NextLineBreak := FindLineBreak(AnsiText, NextLineBreak);
    if NextLineBreak > 0 then
    begin
      if NextLineBreak < TextLength then
      begin
        Inc(NextLineBreak, LineBreakLength);
        LineStarts.Add(Offset + NextLineBreak -1);
      end
      else
      begin
        Inc(NextLineBreak, LineBreakLength);
      end;
    end
  until NextLineBreak = 0;


//  repeat
//    SearchStart := NextPos;
//    NextPos := SearchBuf(SearchStart, TextLength, 0, 0, LineBreak,
//      [soDown, soMatchCase]);
//    if NextPos = nil then
//    begin
//      Break;
//    end
//    else
//    begin
//      Inc(NextPos, Length(LineBreak));
//      LineStarts.Add((NextPos - StartChar) + Offset);
//      TextLength := InitialTextLength + Offset
//        - LineStarts[LineStarts.Count - 1];
//    end;
//  until False;
end;

function TFileIndex.GetNewLineFromOffset(Line, Offset: integer): integer;
var
  APosition: Int64;
begin
  APosition := FPositions[Line] + Offset;
  if not FPositions.BinarySearch(APosition, Result) then
  begin
    Dec(result);
  end;
end;


function FindLastLineBreak(const AString: string): Integer;
var
  P: PChar;
  Index: Integer;
begin
  if (AString = '') then
  begin
    result := 0;
    Exit;
  end;
  P := Pointer(AString);
  Inc(P, Length(AString)-1);
  result := 0;
  for Index := Length(AString) downto 1 do
  begin
    if CharInSet(P^, [#10, #13]) then
    begin
      result := Index+1;
      if (P^ = #10) then
      begin
        Dec(P);
        Dec(result);
      end;
      if (Result > 1) and (P^ = #13) then
      begin
        Dec(result);
      end;
      Exit;
    end
    else
    begin
      Dec(P);
    end;
  end;
end;

procedure TFileIndex.SetFileName(const Value: string);
const
  CharReadCount = 32*1024;
  BufLength = CharReadCount*SizeOf(Char);
var
  PerMil: Integer;
  NewPerMil: Int64;
//  Divider: Int64;
  OffSet: Int64;
  Buffer: TCharArray;
  NumberRead: Integer;
  StartPosition: Integer;
  Converter: TStringBuilder;
  AString: string;
  SomeLines: TStringList;
  NewTimeStepPostions: TListInt64;
  NewTimeStepLines: TListInt64;
  Indent: Integer;
  CurrentLine: string;
  NextLine: string;
  LineIndex: integer;
  LineBreakPos: integer;
begin
//  if Assigned(Indexer) then
//  begin
//    LinesCritSect.Enter;
//    AllLines := False;
//    FListFile := Self;
//    LinesCritSect.Leave;
//  end;
  FAbort := False;
  FFileName := Value;
  FreeAndNil(FFile);
  FPositions.Clear;
  LineBreakLength := 0;


//  FFile := TFile.OpenText(FileName);

  // The default buffer size is 1024.
  // Both increasing (16x) and decreasing (4x) the default buffer size result
  // in poorer performance.
  // Increasing it by a factor of 4 improves performance.
  try
    FFile := TStreamReader.Create(FileName, TEncoding.ANSI, True, 1024*4);
  except
    on E: EFileStreamError do
      raise EInOutError.Create(E.Message);
  end;

  StartPosition := 0;
  FFileSize := FFile.BaseStream.Size;
  FFileDate := TFile.GetLastWriteTimeUtc(FileName);

//  Divider := FFile.BaseStream.Size div 1000;
  FPositions.Capacity := FFile.BaseStream.Size div 50;
  PerMil := 0;
  FPositions.Add(0);
  OffSet := 0;
  Indent := 0;
  LineIndex := 0;
  CurrentLine := '';
  NextLine := '';
  SomeLines := TStringList.Create;
  NewTimeStepPostions := TListInt64.Create;
  NewTimeStepLines := TListInt64.Create;
  Converter := TStringBuilder.Create;
  try
    while True do
    begin
      if FAbort then
      begin
        break;
      end;
      if Length(Buffer) <> CharReadCount then
      begin
        SetLength(Buffer, CharReadCount);
      end;
      if StartPosition = 0 then
      begin
        NumberRead := FFile.Read(Buffer, StartPosition, CharReadCount);
        if NumberRead < 0 then
        begin
          Break;
        end;
      end
      else
      begin
        Assert(StartPosition = 1);
        NumberRead := FFile.Read(Buffer, StartPosition, CharReadCount-1)+1;
        if NumberRead < 1 then
        begin
          Break;
        end;
      end;
      if NumberRead <> CharReadCount then
      begin
        SetLength(Buffer, NumberRead);
      end;
      Converter.Clear;
      Converter.Append(Buffer);
      AString := Converter.ToString;
      GetLineStartingPositions(OffSet, FPositions, AString);
      if Buffer[Length(Buffer)-1] = #13 then
      begin
        Buffer[0] := #13;
        if Assigned(OnIndexSomeLines) {or Assigned(Indexer)} then
        begin
          if NextLine = '' then
          begin
            if (AString[1] = #13) and (StartPosition <> 1) then
            begin
              CurrentLine := Copy(AString,3,MAXINT);
            end
            else
            begin
              CurrentLine := Copy(AString,1,Length(AString)-1);
            end;
          end
          else
          begin
            if (Length(AString) >= 3)
              and (AString[Length(AString)-1] = #10)
              and (AString[Length(AString)-2] = #13) then
            begin
              Converter.Clear;
              Converter.Append(NextLine);
              Converter.Append(AString);
              Converter.Append(#10);
              CurrentLine := Converter.ToString;
            end
            else
            begin
              CurrentLine := NextLine + Copy(AString,1,Length(AString)-1);
            end;
          end;
          NextLine := '';
        end;
        StartPosition := 1;
      end
      else
      begin
        if Assigned(OnIndexSomeLines) {or Assigned(Indexer)} then
        begin
          LineBreakPos := FindLastLineBreak(AString);
          if NextLine = '' then
          begin
            if AString = '' then
            begin
              CurrentLine := ''
            end
            else if (AString[1] = #13) and (StartPosition <> 0) then
            begin
              CurrentLine := Copy(AString,3,LineBreakPos-1);
            end
            else
            begin
              CurrentLine := Copy(AString,1,LineBreakPos+1);
            end;
          end
          else
          begin
            CurrentLine := NextLine + Copy(AString,1,LineBreakPos+1);
          end;
          NextLine := Copy(AString,LineBreakPos+2,MaxInt);
        end;
        StartPosition := 0;
      end;

//      if (Length(CurrentLine) >=2)
//        and (CurrentLine[Length(CurrentLine)-1] = #13)
//        and (CurrentLine[Length(CurrentLine)] = #10)
//        then
//      begin
//        Beep;
////        CurrentLine := Copy(CurrentLine, 1, Length(CurrentLine)-2);
////        NextLine := #13#10 + NextLine;
//      end;


      if Assigned(OnIndexSomeLines) then
      begin
//        SomeLines.Text := CurrentLine;
        OnIndexSomeLines(CurrentLine, Indent, NewTimeStepPostions,
          NewTimeStepLines, LineIndex);
//        Inc(LineIndex, SomeLines.Count);
//      end
//      else if Assigned(Indexer) then
//      begin
//        LinesCritSect.Enter;
//        LinesToIndex.Add(CurrentLine);
//        LinesCritSect.Leave;
      end;


      OffSet := OffSet + NumberRead-StartPosition;
      if Assigned(OnProgress) and (FFileSize > 0) then
      begin
        NewPerMil := Round((OffSet / FFileSize) * 1000);
        if NewPerMil > PerMil then
        begin
          PerMil := NewPerMil;
          OnProgress(self, PerMil);
//          Application.ProcessMessages;
        end;
      end;
    end;

    if (NextLine <> '') then
    begin
      if Assigned(OnIndexSomeLines) then
      begin
  //      SomeLines.Text := NextLine;
        OnIndexSomeLines(NextLine, Indent, NewTimeStepPostions,
          NewTimeStepLines, LineIndex);
  //      Inc(LineIndex, SomeLines.Count);
//      end
//      else if Assigned(Indexer) then
//      begin
//        LinesCritSect.Enter;
//        LinesToIndex.Add(NextLine);
//        AllLines := True;
//        LinesCritSect.Leave;
      end;
    end;
//    if Assigned(Indexer) then
//    begin
//      LinesCritSect.Enter;
//      AllLines := True;
//      LinesCritSect.Leave;
//    end;

    if Assigned(OnUpdateTimeSteps) then
    begin
      OnUpdateTimeSteps(NewTimeStepLines, NewTimeStepPostions);
    end;
  finally
    Converter.Free;
    SomeLines.Free;
    NewTimeStepPostions.Free;
    NewTimeStepLines.Free;
  end;

  FFile.BaseStream.Position := 0;
  FFile.DiscardBufferedData;

  FreeAndNil(FFile);
end;

procedure TFileIndex.SetOnStartLineChange(const Value: TNotifyEvent);
begin
  FOnStartLineChange := Value;
end;

function TFileIndex.TryReopenFile: boolean;
begin
  result := true;
  if FFile = nil then
  begin
    try
      FFile := TStreamReader.Create(FFileName, TEncoding.ANSI, True, 1024*4);
    except
      on E: EFileStreamError do
        raise EInOutError.Create(E.Message);
    end;
  end;
  if (FFileSize <> FFile.BaseStream.Size)
    or (FFileDate <> TFile.GetLastWriteTimeUtc(FileName)) then
  begin
    result := False;
  end;
end;

procedure TFileIndex.ReadLines(var AString: string; StartLine, Count: integer;
  AdjustStart: Boolean);
var
  Buffer: TCharArray;
  Lines: TStringBuilder;
begin
  ReadLines(Buffer, StartLine, Count, AdjustStart);
  Lines := TStringBuilder.Create;
  try
    Lines.Append(Buffer);
    AString := Lines.ToString;
  finally
    Lines.Free;
  end;
end;

end.
