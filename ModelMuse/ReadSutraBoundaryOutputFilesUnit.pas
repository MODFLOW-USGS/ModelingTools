unit ReadSutraBoundaryOutputFilesUnit;

interface

uses
  Generics.Collections, ReadSutraNodEleUnit;

type
  TCustomSutraBoundaryItem = class(TObject)
  private
    FNode: Integer;
    function GetValue(Index: Integer): double; virtual; abstract;
  public
    property Node: integer read FNode;
    property Values[Index: Integer]: Double read GetValue;
  end;

  TBoundaryList<T: TCustomSutraBoundaryItem> = class(TObjectList<T>)
    TimeStep: Integer;
    Time: Double;
  end;

  TCustomItemList = TBoundaryList<TCustomSutraBoundaryItem>;

  TCustomBoundaryLists<T: TCustomSutraBoundaryItem> = class(TObjectList<TBoundaryList<T>>);

  TBcofItem = class(TCustomSutraBoundaryItem)
  private
    FSpecifiedFluidSourceRate: double;
    // Specified concentration or temperature
    FSpecifiedU: double;
    // Resultant rate of mass or energy
    FResultantU: double;
  protected
    function GetValue(Index: Integer): double; override;
  public
    property SpecifiedFluidSourceRate: double read FSpecifiedFluidSourceRate;
    property SpecifiedU: double read FSpecifiedU;
    property ResultantU: double read FResultantU;
  end;

  TBcofList = TBoundaryList<TBcofItem>;

  TBcofLists = TCustomBoundaryLists<TBcofItem>;

  TBcosItem = class(TCustomSutraBoundaryItem)
  private
    // Specified concentration or temperature
    FSpecifiedU: double;
  protected
    function GetValue(Index: Integer): double; override;
  public
    property SpecifiedU: double read FSpecifiedU;
  end;

  TBcosList = TBoundaryList<TBcosItem>;

  TBcosLists = TCustomBoundaryLists<TBcosItem>;

  TBcopItem = class(TCustomSutraBoundaryItem)
  private
    FResultantFluidSource: double;
    FU: double;
    FResultantURate: double;
    FComputedPressure: double;
    FSpecifiedPressure: double;
  protected
    function GetValue(Index: Integer): double; override;
  public
    property ResultantFluidSource: double read FResultantFluidSource;
    property U: double read FU;
    property ResultantURate: double read FResultantURate;
    property ComputedPressure: double read FComputedPressure;
    property SpecifiedPressure: double read FSpecifiedPressure;
  end;

  TBcopList = TBoundaryList<TBcopItem>;

  TBcopLists = TCustomBoundaryLists<TBcopItem>;

  TBcouItem = class(TCustomSutraBoundaryItem)
  private
    FResultantURate: Double;
    FComputedU: double;
    FSpecifiedU: Double;
  protected
    function GetValue(Index: Integer): double; override;
  public
    property ResultantURate: Double read FResultantURate;
    property ComputedU: Double read FComputedU;
    property SpecifiedU: Double read FSpecifiedU;
  end;

  TBcouList = TBoundaryList<TBcouItem>;

  TBcouLists = TCustomBoundaryLists<TBcouItem>;

  TLkstItem = class(TCustomSutraBoundaryItem)
  private
    FStage: Double;
    FDepth: Double;
  protected
    function GetValue(Index: Integer): double; override;
  public
    property Stage: Double read FStage;
    property Depth: Double read FDepth;
  end;

  TLkstList = TBoundaryList<TLkstItem>;

  TLkstLists = TCustomBoundaryLists<TLkstItem>;

function ReadFileHeader(const FileName: string; List: TStoredResultsList): Boolean;

procedure ReadBcofFile(const FileName: string; DesiredItems: TStoredResultsList;
  List: TBcofLists);

procedure ReadBcosFile(const FileName: string; DesiredItems: TStoredResultsList;
  List: TBcosLists);

procedure ReadBcopFile(const FileName: string; DesiredItems: TStoredResultsList;
  List: TBcopLists);

procedure ReadBcouFile(const FileName: string; DesiredItems: TStoredResultsList;
  List: TBcouLists);

procedure ReadLkstFile(const FileName: string; DesiredItems: TStoredResultsList;
  List: TLkstLists);

implementation

uses
  IOUtils, Classes, ModelMuseUtilities, SysUtils, Vcl.Dialogs;

function ReadFileHeader(const FileName: string; List: TStoredResultsList): Boolean;
const
  SearchText = '##    in this file      Time (sec)';
  SearchTextLength = Length(SearchText);
var
  FileReader: TStreamReader;
  ALine: string;
  Splitter: TStringList;
  HeaderItem: TStoredResults;
begin
  result := False;
  List.Clear;
  FileReader := nil;
  try
    FileReader := TFile.OpenText(FileName);
  except on E: EInOutError do
    begin
      Beep;
      MessageDlg(E.message, mtError, [mbOK], 0);
      Exit;
    end;
  end;
  try
    while not FileReader.EndOfStream do
    begin
      ALine := FileReader.ReadLine;
      if Copy(ALine, 1, SearchTextLength) = SearchText then
      begin
        // Skip 1 lines;
        FileReader.ReadLine;

        Splitter := TStringList.Create;
        try
          Splitter.Delimiter := ' ';
          while not FileReader.EndOfStream do
          begin
            Splitter.DelimitedText := FileReader.ReadLine;
            if Splitter.Count = 1 then
            begin
              Break;
            end;
            HeaderItem := TStoredResults.Create;
            HeaderItem.TimeStep := StrToInt(Splitter[1]);
            HeaderItem.Time := FortranStrToFloat(Splitter[2]);
            List.Add(HeaderItem);
          end;
        finally
          Splitter.Free;
        end;
        break;
      end;
    end;
  finally
    FileReader.Free;
  end;
  result := True;
end;

procedure ReadBcofFile(const FileName: string; DesiredItems: TStoredResultsList; List: TBcofLists);
const
  SearchText = '## TIME STEP';
  SearchTextLength = Length(SearchText);
var
  FileReader: TStreamReader;
  ALine: string;
  DesiredIndex: Integer;
  Splitter: TStringList;
  TimeStep: Integer;
  DesiredItem: TStoredResults;
  CurrentList: TBcofList;
  CurrentItem: TBcofItem;
  LineIndex: integer;
begin
  List.Clear;
  if DesiredItems.Count = 0 then
  begin
    Exit;
  end;
  FileReader := TFile.OpenText(FileName);
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    DesiredIndex := 0;
    DesiredItem := DesiredItems[DesiredIndex];
    while not FileReader.EndOfStream do
    begin
      ALine := FileReader.ReadLine;
      if Copy(ALine, 1, SearchTextLength) = SearchText then
      begin
        Splitter.DelimitedText := ALine;
        TimeStep := StrToInt(Splitter[3]);
        if TimeStep = DesiredItem.TimeStep then
        begin
          // Skip 4 lines;
          for LineIndex := 1 to 4 do
          begin
            FileReader.ReadLine;
          end;

          CurrentList := TBcofList.Create;
          List.Add(CurrentList);
          CurrentList.TimeStep := TimeStep;
          CurrentList.Time := DesiredItem.Time;

          while not FileReader.EndOfStream do
          begin
            ALine := FileReader.ReadLine;
            if Copy(ALine, 1,2) = '##' then
            begin
              break;
            end;
            Splitter.DelimitedText := ALine;
            CurrentItem := TBcofItem.Create;
            CurrentList.Add(CurrentItem);
            CurrentItem.FNode := StrToInt(Splitter[0]);
            if Pos('INACTIVE', ALine) > 0 then
            begin
              CurrentItem.FSpecifiedFluidSourceRate := -1e98;
              CurrentItem.FSpecifiedU := -1e98;
              CurrentItem.FResultantU := -1e98;
            end
            else
            begin
              Splitter.DelimitedText := Copy(ALine, 68, MaxInt);
              CurrentItem.FSpecifiedFluidSourceRate := FortranStrToFloat(Splitter[0]);
              CurrentItem.FSpecifiedU := FortranStrToFloat(Splitter[1]);
              CurrentItem.FResultantU := FortranStrToFloat(Splitter[2]);
            end;
          end;

          Inc(DesiredIndex);
          if DesiredIndex >= DesiredItems.Count then
          begin
            break;
          end;
          DesiredItem := DesiredItems[DesiredIndex];
        end;
      end;
    end;

  finally
    Splitter.Free;
    FileReader.Free;
  end;
end;

procedure ReadBcosFile(const FileName: string; DesiredItems: TStoredResultsList; List: TBcosLists);
const
  SearchText = '## TIME STEP';
  SearchTextLength = Length(SearchText);
var
  FileReader: TStreamReader;
  ALine: string;
  DesiredIndex: Integer;
  Splitter: TStringList;
  TimeStep: Integer;
  DesiredItem: TStoredResults;
  CurrentList: TBcosList;
  CurrentItem: TBcosItem;
  LineIndex: integer;
begin
  List.Clear;
  if DesiredItems.Count = 0 then
  begin
    Exit;
  end;
  FileReader := TFile.OpenText(FileName);
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    DesiredIndex := 0;
    DesiredItem := DesiredItems[DesiredIndex];
    while not FileReader.EndOfStream do
    begin
      ALine := FileReader.ReadLine;
      if Copy(ALine, 1, SearchTextLength) = SearchText then
      begin
        Splitter.DelimitedText := ALine;
        TimeStep := StrToInt(Splitter[3]);
        Assert(TimeStep <= DesiredItem.TimeStep);
        if TimeStep = DesiredItem.TimeStep then
        begin
          // Skip 4 lines;
          for LineIndex := 1 to 4 do
          begin
            FileReader.ReadLine;
          end;

          CurrentList := TBcosList.Create;
          List.Add(CurrentList);
          CurrentList.TimeStep := TimeStep;
          CurrentList.Time := DesiredItem.Time;

          while not FileReader.EndOfStream do
          begin
            ALine := FileReader.ReadLine;
            if Copy(ALine, 1,2) = '##' then
            begin
              break;
            end;
            if Pos('INACTIVE', ALine) > 0 then
            begin
              Continue;
            end;
            Splitter.DelimitedText := ALine;
            CurrentItem := TBcosItem.Create;
            CurrentList.Add(CurrentItem);
            CurrentItem.FNode := StrToInt(Splitter[0]);
            if Pos('INACTIVE', ALine) > 0 then
            begin
              CurrentItem.FSpecifiedU := -1E98;
            end
            else
            begin
              Splitter.DelimitedText := Copy(ALine, 68, MaxInt);
              CurrentItem.FSpecifiedU := FortranStrToFloat(Splitter[0]);
            end;
          end;

          Inc(DesiredIndex);
          if DesiredIndex >= DesiredItems.Count then
          begin
            break;
          end;
          DesiredItem := DesiredItems[DesiredIndex];
        end;
      end;
    end;

  finally
    Splitter.Free;
    FileReader.Free;
  end;
end;

procedure ReadBcopFile(const FileName: string; DesiredItems: TStoredResultsList; List: TBcopLists);
const
  SearchText = '## TIME STEP';
  SearchTextLength = Length(SearchText);
var
  FileReader: TStreamReader;
  ALine: string;
  DesiredIndex: Integer;
  Splitter: TStringList;
  TimeStep: Integer;
  DesiredItem: TStoredResults;
  CurrentList: TBcopList;
  CurrentItem: TBcopItem;
  LineIndex: integer;
begin
  List.Clear;
  if DesiredItems.Count = 0 then
  begin
    Exit;
  end;
  FileReader := TFile.OpenText(FileName);
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    DesiredIndex := 0;
    DesiredItem := DesiredItems[DesiredIndex];
    while not FileReader.EndOfStream do
    begin
      ALine := FileReader.ReadLine;
      if Copy(ALine, 1, SearchTextLength) = SearchText then
      begin
        Splitter.DelimitedText := ALine;
        TimeStep := StrToInt(Splitter[3]);
        Assert(TimeStep <= DesiredItem.TimeStep);
        if TimeStep = DesiredItem.TimeStep then
        begin
          // Skip 4 lines;
          for LineIndex := 1 to 4 do
          begin
            FileReader.ReadLine;
          end;

          CurrentList := TBcopList.Create;
          List.Add(CurrentList);
          CurrentList.TimeStep := TimeStep;
          CurrentList.Time := DesiredItem.Time;

          while not FileReader.EndOfStream do
          begin
            ALine := FileReader.ReadLine;
            if Copy(ALine, 1,2) = '##' then
            begin
              break;
            end;
            Splitter.DelimitedText := ALine;
            CurrentItem := TBcopItem.Create;
            CurrentList.Add(CurrentItem);
            CurrentItem.FNode := StrToInt(Splitter[0]);
            Splitter.DelimitedText := Copy(ALine, 68, MaxInt);
            if Pos('INACTIVE', ALine) > 0 then
            begin
              CurrentItem.FResultantFluidSource := -1e98;
              CurrentItem.FU := -1e98;
              CurrentItem.FResultantURate := -1e98;
              CurrentItem.FComputedPressure := -1e98;
              CurrentItem.FSpecifiedPressure := -1e98;
            end
            else
            begin
              CurrentItem.FResultantFluidSource := FortranStrToFloat(Splitter[0]);
              CurrentItem.FU := FortranStrToFloat(Splitter[1]);
              CurrentItem.FResultantURate := FortranStrToFloat(Splitter[2]);
              CurrentItem.FComputedPressure := FortranStrToFloat(Splitter[3]);
              CurrentItem.FSpecifiedPressure := FortranStrToFloat(Splitter[4]);
            end;
          end;

          Inc(DesiredIndex);
          if DesiredIndex >= DesiredItems.Count then
          begin
            break;
          end;
          DesiredItem := DesiredItems[DesiredIndex];
        end;
      end;
    end;

  finally
    Splitter.Free;
    FileReader.Free;
  end;
end;

procedure ReadBcouFile(const FileName: string; DesiredItems: TStoredResultsList; List: TBcouLists);
const
  SearchText = '## TIME STEP';
  SearchTextLength = Length(SearchText);
var
  FileReader: TStreamReader;
  ALine: string;
  DesiredIndex: Integer;
  Splitter: TStringList;
  TimeStep: Integer;
  DesiredItem: TStoredResults;
  CurrentList: TBcouList;
  CurrentItem: TBcouItem;
  LineIndex: integer;
begin
  List.Clear;
  if DesiredItems.Count = 0 then
  begin
    Exit;
  end;
  FileReader := TFile.OpenText(FileName);
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    DesiredIndex := 0;
    DesiredItem := DesiredItems[DesiredIndex];
    while not FileReader.EndOfStream do
    begin
      ALine := FileReader.ReadLine;
      if Copy(ALine, 1, SearchTextLength) = SearchText then
      begin
        Splitter.DelimitedText := ALine;
        TimeStep := StrToInt(Splitter[3]);
        Assert(TimeStep <= DesiredItem.TimeStep);
        if TimeStep = DesiredItem.TimeStep then
        begin
          // Skip 4 lines;
          for LineIndex := 1 to 4 do
          begin
            FileReader.ReadLine;
          end;

          CurrentList := TBcouList.Create;
          List.Add(CurrentList);
          CurrentList.TimeStep := TimeStep;
          CurrentList.Time := DesiredItem.Time;

          while not FileReader.EndOfStream do
          begin
            ALine := FileReader.ReadLine;
            if Copy(ALine, 1,2) = '##' then
            begin
              break;
            end;
            if Pos('INACTIVE', ALine) > 0 then
            begin
              Continue;
            end;
            Splitter.DelimitedText := ALine;
            CurrentItem := TBcouItem.Create;
            CurrentList.Add(CurrentItem);
            CurrentItem.FNode := StrToInt(Splitter[0]);
            if Pos('INACTIVE', ALine) > 0 then
            begin
              CurrentItem.FResultantURate := -1E98;
              CurrentItem.FComputedU := -1E98;
              CurrentItem.FSpecifiedU := -1E98;
            end
            else
            begin
              Splitter.DelimitedText := Copy(ALine, 68, MaxInt);
              CurrentItem.FResultantURate := FortranStrToFloat(Splitter[0]);
              CurrentItem.FComputedU := FortranStrToFloat(Splitter[1]);
              CurrentItem.FSpecifiedU := FortranStrToFloat(Splitter[2]);
            end;
          end;

          Inc(DesiredIndex);
          if DesiredIndex >= DesiredItems.Count then
          begin
            break;
          end;
          DesiredItem := DesiredItems[DesiredIndex];
        end;
      end;
    end;

  finally
    Splitter.Free;
    FileReader.Free;
  end;
end;

procedure ReadLkstFile(const FileName: string; DesiredItems: TStoredResultsList;
  List: TLkstLists);
const
  SearchText = '## TIME STEP';
  SearchTextLength = Length(SearchText);
var
  FileReader: TStreamReader;
  ALine: string;
  DesiredIndex: Integer;
  Splitter: TStringList;
  TimeStep: Integer;
  DesiredItem: TStoredResults;
  CurrentList: TLkstList;
  CurrentItem: TLkstItem;
  LineIndex: integer;
begin
  List.Clear;
  if DesiredItems.Count = 0 then
  begin
    Exit;
  end;
  FileReader := TFile.OpenText(FileName);
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    DesiredIndex := 0;
    DesiredItem := DesiredItems[DesiredIndex];
    while not FileReader.EndOfStream do
    begin
      ALine := FileReader.ReadLine;
      if Copy(ALine, 1, SearchTextLength) = SearchText then
      begin
        Splitter.DelimitedText := ALine;
        TimeStep := StrToInt(Splitter[3]);
        Assert(TimeStep <= DesiredItem.TimeStep);
        if TimeStep = DesiredItem.TimeStep then
        begin
          // Skip 2 lines;
          for LineIndex := 1 to 2 do
          begin
            FileReader.ReadLine;
          end;

          CurrentList := TLkstList.Create;
          List.Add(CurrentList);
          CurrentList.TimeStep := TimeStep;
          CurrentList.Time := DesiredItem.Time;

          while not FileReader.EndOfStream do
          begin
            ALine := FileReader.ReadLine;
            if (ALine= '') or (Copy(ALine, 1,2) = '##') then
            begin
              break;
            end;
            Splitter.DelimitedText := ALine;
            CurrentItem := TLkstItem.Create;
            CurrentList.Add(CurrentItem);
            CurrentItem.FNode := StrToInt(Splitter[0]);
            CurrentItem.FStage := FortranStrToFloat(Splitter[1]);
            CurrentItem.FDepth := FortranStrToFloat(Splitter[2]);
          end;

          Inc(DesiredIndex);
          if DesiredIndex >= DesiredItems.Count then
          begin
            break;
          end;
          DesiredItem := DesiredItems[DesiredIndex];
        end;
      end;
    end;

  finally
    Splitter.Free;
    FileReader.Free;
  end;
end;


{ TBcofItem }

function TBcofItem.GetValue(Index: Integer): double;
begin
  result := 0;
  case Index of
    0: result := SpecifiedFluidSourceRate;
    1: Result := SpecifiedU;
    2: Result := ResultantU;
    else Assert(False);
  end;
end;

{ TBcosItem }

function TBcosItem.GetValue(Index: Integer): double;
begin
  if Index = 0 then
  begin
    result := SpecifiedU;
  end
  else
  begin
    result := 0;
    Assert(False);
  end;
end;

{ TBcopItem }

function TBcopItem.GetValue(Index: Integer): double;
begin
  result := 0;
  case Index of
    0: result := ResultantFluidSource;
    1: result := U;
    2: result := ResultantURate;
    3: result := ComputedPressure;
    4: result := SpecifiedPressure;
    else Assert(False);
  end;
end;

{ TBcouItem }

function TBcouItem.GetValue(Index: Integer): double;
begin
  result := 0;
  case Index of
    0: result := ResultantURate;
    1: result := ComputedU;
    2: result := SpecifiedU;
    else
     Assert(False);
  end;
end;


{ TLkstItem }

function TLkstItem.GetValue(Index: Integer): double;
begin
  result := 0;
  case Index of
    0: result := Stage;
    1: result := Depth;
    else
     Assert(False);
  end;
end;

end.
