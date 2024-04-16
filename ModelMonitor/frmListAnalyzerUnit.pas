unit frmListAnalyzerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileIndexUnit, Mask, JvExMask, JvSpin, ComCtrls,
  frameFileListHandlerUnit, ExtCtrls, Buttons, ImgList, Grids, RbwDataGrid4,
  ArgusDataEntry, JvExExtCtrls, JvNetscapeSplitter, JvExStdCtrls, VirtualTrees,
  JvComponentBase, JvDragDrop, JvRichEdit, frmIndexFileUnit, SearchTrie,
  Generics.Collections, Vcl.AppEvnts, System.ImageList, System.UITypes;

type
  TIndexType = (itNone, itPackage, itObservation, itBoundary, itNumberBoundary,
    itWarning, itError, itIndent1, {itIndent2,} itIndent3, itNoIndentChange,
    itStressPeriod, itStressPeriod2, itStartTimeStep, itIteration, itIteration2,
    itArray1, itArray2, itArray3, itEndModel, itParameter, itNonID);
  TIndexTypes = set of TIndexType;
  TIndexTypeArray = array[0..1] of TIndexType;
  TModflowSearchTrie = TSearchTrie<TIndexType>;
//  TQuickSearchTrie = TSearchTrie<Boolean>;

  TIndexFileChoice = ({ifcUnknown,} ifcYes, ifcNo);

  TfrmMain = class(TForm)
    OpenDialog1: TOpenDialog;
    pgcIndex: TPageControl;
    tabIndex: TTabSheet;
    tabErrors: TTabSheet;
    tabWarnings: TTabSheet;
    frameWarning: TframeFileListHandler;
    frameErrors: TframeFileListHandler;
    frameListing: TframeFileListHandler;
    ProgressBar1: TProgressBar;
    Panel1: TPanel;
    btnOpenFile: TBitBtn;
    btnReadMoreLines: TBitBtn;
    btnReadEarlierLines: TBitBtn;
    spinLineCount: TJvSpinEdit;
    lblLineCount: TLabel;
    ilTabFaces: TImageList;
    pgcDisplay: TPageControl;
    tabLines: TTabSheet;
    tabTable: TTabSheet;
    rdgTable: TRbwDataGrid4;
    btnIndex: TButton;
    Panel2: TPanel;
    lblFileCount: TLabel;
    rdeLineTarget: TRbwDataEntry;
    btnGoTo: TButton;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    sbLines: TScrollBar;
    btnAbort: TBitBtn;
    FindDialog1: TFindDialog;
    btnFind: TBitBtn;
    pnlTableBottom: TPanel;
    btnCopy: TButton;
    drgdrpFiles: TJvDragDrop;
    jvrchdtLines: TJvRichEdit;
    dlgFont: TFontDialog;
    btnFont: TButton;
    tmrOpenFile: TTimer;
    jreLineNumbers: TJvRichEdit;
    tabSorted: TTabSheet;
    frameSorted: TframeFileListHandler;
    AppEvnt1: TApplicationEvents;
    btnSettings: TButton;
    lblDescription: TLabel;
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnReadMoreLinesClick(Sender: TObject);
    procedure btnReadEarlierLinesClick(Sender: TObject);
    procedure btnGoToClick(Sender: TObject);
    procedure btnIndexClick(Sender: TObject);
    procedure sbLinesScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure memoLinesClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure AlternateFindDialog1Find(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure frameListingvstIndexLinesNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure redtLines1SelectionChange(Sender: TObject);
    procedure drgdrpFilesDrop(Sender: TObject; Pos: TPoint; Value: TStrings);
    procedure btnFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrOpenFileTimer(Sender: TObject);
    procedure OpenDialog1Show(Sender: TObject);
    procedure OpenDialog1Close(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure jvrchdtLinesVerticalScroll(Sender: TObject);
    procedure frameSortedvstIndexLinesCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure AppEvnt1Idle(Sender: TObject; var Done: Boolean);
    procedure btnSettingsClick(Sender: TObject);
  private
    FListFile: TFileIndex;
    FChangingPosition: Boolean;
    FAborting : Boolean;
    FShouldOpenFile: Boolean;
    FIndexFile: TfrmIndexFile;
    FShowIndexFile: TIndexFileChoice;
    FSearcher: TModflowSearchTrie;
    FMinLength: Integer;
    FParameterDefined: Boolean;
    FFileDate: Extended;
    FIdling: Boolean;
    PriorStepString: String;
    NextIndent: Integer;
    PriorSP: string;
    PriorIndent: Integer;
    PriorStressPeriod: string;
    FModflow6: Boolean;
    FReopeningAllowed: Boolean;
    FPromptReopen: Boolean;
    procedure ListFileProgress(Sender: TObject; PerMil: integer);
    function DisplayObservations: boolean;
    function DisplayArray: Boolean;
    procedure FilePositionChanged(Sender: TObject);
    procedure InitializeSearchTrie;
    procedure AlternateIndexFile;
    function SearchTrie(ALine: AnsiString; var IndexTypes: TIndexTypeArray;
      var KeyLength: integer; out FoundKey: string): boolean;
    procedure IndexSomeLines(const MultipleLines: string; var Indent: Integer;
      NewTimeStepPostions, NewTimeStepLines: TListInt64; var LineIndex: Integer);
    procedure UpdateTimeSteps(NewTimeStepLines, NewTimeStepPostions: TListInt64);
    procedure UpdateLineNumberWidth;
    procedure OpenFile(FileName: string);
    function TryReopenFile: Boolean;
//    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
//    procedure WndProc(var Message: TMessage); override;
    { Private declarations }
  public
    procedure OpenAFile(const FileName: string);
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

const
  LinesToRead = 1000;

implementation

uses
  ModflowIdentifiersUnit, Math, ErrorMessages, ExtractObservationsUnit,
  BMSearch, System.StrUtils, System.IOUtils, frmSettingsUnit;

resourcestring
  StrListingAnalyst = 'ListingAnalyst';
//  StrTheFileHasChanged = 'The file has changed. Do you want to open the new ' +
//  'version?';
  StrTheFileHasChanged = 'The file has changed. Do you want to reload the fi' +
  'le?';

{$R *.dfm}

type
  TObsColumns = (ocName, ocObserved, ocSimulated, ocDifference);

const
  LineConcatCount = 300;
  MinLines = 25;
  MaxLines = 5000;

procedure TfrmMain.btnAbortClick(Sender: TObject);
begin
  FListFile.Abort;
  FAborting := True;
end;

procedure TfrmMain.btnCopyClick(Sender: TObject);
begin
  rdgTable.CopyAllCellsToClipboard;
end;

procedure TfrmMain.btnFindClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TfrmMain.UpdateLineNumberWidth;
var
  NumberStr: string;
var
  DC: HDC;
  Metrics : TTextMetric;
begin
  if (FListFile <> nil) and (FListFile.LineCount <> 0) then
  begin
    DC := GetDC(jreLineNumbers.Handle);
//    SaveFont := SelectObject(DC, Memo1.Font.Handle);
    GetTextMetrics(DC, Metrics);
    NumberStr := IntToStr(FListFile.LineCount+1);
    jreLineNumbers.Width := (Length(NumberStr)+3)* (Metrics.tmAveCharWidth+1);
    ReleaseDC(jreLineNumbers.Handle, DC);
//    tmAveCharWidth
  end;
end;

procedure TfrmMain.btnFontClick(Sender: TObject);
begin
  dlgFont.Font := jvrchdtLines.Font;
  if dlgFont.Execute then
  begin
    jvrchdtLines.Font := dlgFont.Font;
    rdgTable.Font := dlgFont.Font;
    jreLineNumbers.Font := dlgFont.Font;
    UpdateLineNumberWidth;
  end;
end;

procedure TfrmMain.btnGoToClick(Sender: TObject);
var
  LineNumber: Integer;
begin
  LineNumber := StrToInt(rdeLineTarget.Text)-1;
  case pgcIndex.ActivePageIndex of
    0:
      begin
        frameListing.GoToLine(LineNumber);
      end;
    1:
      begin
        frameSorted.GoToLine(LineNumber);
      end;
    2:
      begin
        frameErrors.GoToLine(LineNumber);
      end;
    3:
      begin
        frameWarning.GoToLine(LineNumber);
      end;
    else
      Assert(False);
  end;
  frameListing.ChangeLineNumber(LineNumber);
  frameSorted.ChangeLineNumber(LineNumber);
  frameErrors.ChangeLineNumber(LineNumber);
  frameWarning.ChangeLineNumber(LineNumber);
end;

//{$DEFINE ShowTimes}

procedure TfrmMain.btnIndexClick(Sender: TObject);
  {$IFDEF ShowTimes}
var
  StartTime: Extended;
  EndTime: Extended;
  {$ENDIF}
begin
  {$IFDEF ShowTimes}
  StartTime := Now;
  {$ENDIF}
  AlternateIndexFile;
//  IndexFile;
  btnIndex.Enabled := False;
  ProgressBar1.Position := 0;
  {$IFDEF ShowTimes}
  EndTime := Now;
  ShowMessage(FloatToStr((EndTime-StartTime)*24*3600));
  {$ENDIF}
end;

procedure TfrmMain.btnOpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    OpenAFile(OpenDialog1.FileName);
  end;
end;

procedure TfrmMain.btnReadEarlierLinesClick(Sender: TObject);
begin
  case pgcIndex.ActivePageIndex of
    0:
      begin
        frameListing.InsertLines;
      end;
    1:
      begin
        frameSorted.InsertLines;
      end;
    2:
      begin
        frameErrors.InsertLines;
      end;
    3:
      begin
        frameWarning.InsertLines;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure TfrmMain.btnReadMoreLinesClick(Sender: TObject);
begin
  case pgcIndex.ActivePageIndex of
    0:
      begin
        frameListing.AddLines;
      end;
    1:
      begin
        frameSorted.AddLines;
      end;
    2:
      begin
        frameErrors.AddLines;
      end;
    3:
      begin
        frameWarning.AddLines;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure TfrmMain.FilePositionChanged(Sender: TObject);
begin
  if FChangingPosition then
  begin
    Exit;
  end;
  FChangingPosition := True;
  try
    sbLines.Position := Round(FListFile.CurrentStartLine
      /FListFile.LineCount*sbLines.Max);
  finally
    FChangingPosition := False;
  end;
end;

procedure TfrmMain.OpenAFile(const FileName: string);
  {$IFDEF ShowTimes}
var
  StartTime: Extended;
  EndTime: Extended;
  {$ENDIF}
begin
  {$IFDEF ShowTimes}
  StartTime := Now;
  {$ENDIF}
  FPromptReopen := True;
  FReopeningAllowed := True;
  tabTable.TabVisible := False;
  tabLines.TabVisible := False;
  pgcDisplay.ActivePage := tabLines;
  tabErrors.TabVisible := False;
  tabWarnings.TabVisible := False;

  sbLines.Enabled := False;
  btnGoTo.Enabled := False;
  btnFind.Enabled := False;
  jvrchdtLines.Enabled := False;
  frameListing.BeginUpdate;
  frameWarning.BeginUpdate;
  frameErrors.BeginUpdate;
  frameSorted.BeginUpdate;

  jvrchdtLines.Tag := 1;
  try
    FParameterDefined := False;
    OpenFile(FileName);
  finally
    UpdateLineNumberWidth;
    jvrchdtLines.Tag := 0;

    pgcIndex.ActivePage := tabIndex;
    tabWarnings.TabVisible := frameWarning.HasContent;
    if tabWarnings.TabVisible then
    begin
      pgcIndex.ActivePage := tabWarnings;
    end;
    tabErrors.TabVisible := frameErrors.HasContent;
    if tabErrors.TabVisible then
    begin
      pgcIndex.ActivePage := tabErrors;
    end;

    frameListing.EndUpdate;
    frameWarning.EndUpdate;
    frameErrors.EndUpdate;
    frameSorted.EndUpdate;
    sbLines.Enabled := True;
    btnGoTo.Enabled := True;
    btnFind.Enabled := True;
    jvrchdtLines.Enabled := True;
  end;

  {$IFDEF ShowTimes}
  EndTime := Now;
  ShowMessage(FloatToStr((EndTime-StartTime)*24*3600));
  {$ENDIF}
  if not FAborting then
  begin
    case FShowIndexFile of
//      ifcUnknown:
//        begin
//          if (MessageDlg(
//            'Do you want to create an index of this file?', mtInformation,
//            [mbYes, mbNo], 0) = mrYes) then
//          begin
//            FShowIndexFile := ifcYes;
//            btnIndexClick(nil);
//          end
//          else
//          begin
//            FShowIndexFile := ifcNo;
//          end;
//        end;
      ifcYes:
        begin
          btnIndex.Enabled := False;
        end;
      ifcNo: ;
      else Assert(False);
    end;
  end;
end;

function TfrmMain.TryReopenFile: Boolean;
begin
  result := True;
  if not FListFile.TryReopenFile then
  begin
    if FReopeningAllowed and (MessageDlg(StrTheFileHasChanged,
    mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
    begin
      OpenAFile(FListFile.FileName);
    end
    else
    begin
      FReopeningAllowed := False;
    end;
    result := False;
  end;
end;

//procedure TfrmMain.ShowThreadTime(Sender: TObject);
//var
//  EndTime: Extended;
//begin
//  {$IFDEF ShowTimes}
//  EndTime := Now;
//  ShowMessage(FloatToStr((EndTime-StartTime)*24*3600));
//  {$ENDIF}
//end;

procedure TfrmMain.InitializeSearchTrie;
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
  for index := 0 to NumberPackageIdentifiers.Count - 1 do
  begin
    AddKey((NumberPackageIdentifiers[index]), itPackage);
  end;
  for index := 0 to NonIdentifiers.Count - 1 do
  begin
    AddKey((NonIdentifiers[index]), itNonID);
  end;

  AddKey(StressPeriodID1, itStressPeriod);
  AddKey(StressPeriodID2, itStressPeriod2);

  AddKey(StartNewTimeStep, itStartTimeStep);
  AddKey(StrINNERITERATIONSUMM, itStartTimeStep);
//  AddKey(StrTimeseriesControll, itStartTimeStep);

  AddKey(StrParameterName, itNoIndentChange);
  AddKey(StrINSTANCE, itIndent3);
//  AddKey(StrParameter, itNoIndentChange);
  AddKey(StrParameter, itParameter);
//  AddKey(StrParameterName, itParameter);
  AddKey(TransportStep, itIndent3);
  AddKey(StrINSTANCE2, itIndent3);
  AddKey(StrCLASSIFICATIONCOU, itIndent1);
  AddKey(StrGROUNDSURFACE, itIndent1);
  AddKey(IterationID1, itIteration);
  AddKey(IterationID2, itIteration2);
  AddKey(ArrayID1, itArray1);
  AddKey(ArrayID2, itArray2);
  AddKey(ArrayID3, itArray3);
  AddKey(ArrayID4, itArray3);
  AddKey(SwiBudgetID, itArray3);
  AddKey(SwrBudgetID, itArray3);
  AddKey(SwrBudgetDiscrepancy, itArray3);
  AddKey(BudgetID, itArray3);
  AddKey(TimeSummary, itArray3);
  AddKey(EndModel, itEndModel);
  AddKey(StrCfpm1Results, itArray3);
  AddKey(StrPipeBudget, itArray3);
  AddKey(BudgetIDMf6, itArray3);

  // MODFLOW 6
  AddKey(StressPeriodMf6ID1, itStressPeriod);
  AddKey(StrOUTPUTCONTROLFORS, itStressPeriod);
  AddKey(Mf6StartTimeStep, itStressPeriod);

  AddKey(StressPeriodMf6ID2A, itStressPeriod2);
  AddKey(StressPeriodMf6ID2B, itStressPeriod2);
  AddKey(Mf6Kstp, itStressPeriod2);




//  for index := 0 to KeyWords.Count - 1 do
//  begin
//    FQuickSearcher.AddKey(PAnsiChar(KeyWords[index]), True);
//  end;

end;

var PriorValue: Integer = -1;

procedure TfrmMain.jvrchdtLinesVerticalScroll(Sender: TObject);
var
  FirstLine, LineNumbersFirstLine: integer;
  LineIndex: integer;
begin
  if jvrchdtLines.Tag <> 0 then
  begin
    Exit;
  end;

  FirstLine := jvrchdtLines.Perform(EM_GETFIRSTVISIBLELINE, 0, 0 );
  LineNumbersFirstLine := jreLineNumbers.Perform(EM_GETFIRSTVISIBLELINE, 0, 0 );
  if FirstLine <> LineNumbersFirstLine then
  begin
    jreLineNumbers.Lines.BeginUpdate;
    try
      jvrchdtLines.Perform(EM_SCROLL, SB_LINEDOWN, 0 );
      jvrchdtLines.Perform(EM_SCROLL, SB_LINEUP, 0 );

      FirstLine := jvrchdtLines.Perform(EM_GETFIRSTVISIBLELINE, 0, 0 );
      if FirstLine = LineNumbersFirstLine then
      begin
        Exit;
      end;

//      APoint1 := jreLineNumbers.GetCharPos(0);
//      CharIndex := Length(jreLineNumbers.Lines[0])+1;
//      APoint2 := jreLineNumbers.GetCharPos(CharIndex);
//      while APoint1.Y = APoint2.Y do
//      begin
//        Inc(CharIndex);
//        APoint2 := jreLineNumbers.GetCharPos(CharIndex);
//      end;

//      GetScrollInfo(jreLineNumbers.Handle, SB_VERT, ScrollInfo)
//      jreLineNumbers.CaretPos := Point(0,FirstLine);
//      jreLineNumbers.ScrollBy(0,-(FirstLine-LineNumbersFirstLine)*(APoint2.Y-APoint1.Y));
//      LineNumbersFirstLine := jreLineNumbers.Perform(EM_GETFIRSTVISIBLELINE, 0, 0 );

      for LineIndex := LineNumbersFirstLine to FirstLine -1  do
      begin
        jreLineNumbers.Perform(EM_SCROLL, SB_LINEDOWN, 0 );
//        LineNumbersFirstLine := jreLineNumbers.Perform(EM_GETFIRSTVISIBLELINE, 0, 0 );
      end;
      for LineIndex := FirstLine to LineNumbersFirstLine -1 do
      begin
        jreLineNumbers.Perform(EM_SCROLL, SB_LINEUP, 0 );
//        LineNumbersFirstLine := jreLineNumbers.Perform(EM_GETFIRSTVISIBLELINE, 0, 0 );
      end;
    finally
      jreLineNumbers.Lines.EndUpdate;
    end;
  end;
end;

procedure TfrmMain.OpenDialog1Close(Sender: TObject);
begin
  if FIndexFile.cbIndex.Checked then
  begin
    FShowIndexFile := ifcYes;
  end
  else
  begin
    FShowIndexFile := ifcNo;
  end;
  FreeAndNil(FIndexFile);
end;

procedure TfrmMain.OpenDialog1Show(Sender: TObject);
var
  ADialog: TOpenDialog;
begin
  ADialog := Sender as TOpenDialog;
  FIndexFile := TfrmIndexFile.createfordialog(ADialog);
  FIndexFile.cbIndex.Checked := FShowIndexFile = ifcYes;// in [ifcUnknown, ifcYes];
end;

function WholeWord(WordPosition: integer;
  const ALine, SearchTerm: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
begin
  result := True;
  if (WordPosition > 1)
    and CharInSet(ALine[WordPosition-1], AlphaNumeric) then
  begin
    result := False;
  end;
  WordPosition := WordPosition + Length(SearchTerm)+1;
  if (WordPosition <= Length(ALine))
    and CharInSet(ALine[WordPosition-1], AlphaNumeric) then
  begin
    result := False;
  end;
end;

procedure TfrmMain.FindDialog1Find(Sender: TObject);
var
  FoundAt: LongInt;
  StartPos, ToEnd: Integer;
  mySearchTypes : TRichSearchTypes;
  NextLineIndex: integer;
  ALine: string;
  SearchTerm: string;
  StartLine: integer;
  WordPosition: integer;
  FoundCount: integer;
  NotFoundCount: integer;
  LinesToRead: integer;
  CurrentLines: TStringList;
  LineIndex: integer;
  Backwards: boolean;
  PriorWordPostion: integer;
  PriorPostions: TIntList;
  NewCaretPos: TPoint;
  WordIndex: integer;
  FoundWord: boolean;
  AnotherLine: string;
  LineNumber: Integer;
//  result: boolean;
begin
  jvrchdtLines.Tag := 1;
  PriorPostions := TIntList.Create;
  try
    if not TryReopenFile then
    begin
      Exit
    end;
    FAborting := False;
    mySearchTypes := [];

    if frMatchCase in FindDialog1.Options then
       mySearchTypes := mySearchTypes + [stMatchCase];
    if frWholeWord in FindDialog1.Options then
       mySearchTypes := mySearchTypes + [stWholeWord];
    Backwards := not (frDown in FindDialog1.Options);
    if Backwards then
       mySearchTypes := mySearchTypes + [stBackward];

    { Begin the search after the current selection, if there is one. }
    { Otherwise, begin at the cursor position. }
    if (jvrchdtLines.SelLength <> 0) and not Backwards then
      StartPos := jvrchdtLines.SelStart + jvrchdtLines.SelLength
    else
      StartPos := jvrchdtLines.SelStart;
    { ToEnd is the length from StartPos through the end of the
      text in the rich edit control. }
    if not Backwards then
    begin
      ToEnd := Length(jvrchdtLines.Text) - StartPos;
    end
    else
    begin
      ToEnd := StartPos;
    end;
    FoundAt :=
      jvrchdtLines.FindText(FindDialog1.FindText, StartPos, ToEnd, mySearchTypes);
    if FoundAt <> -1 then
    begin
      if jvrchdtLines.CanFocus then
      begin
        jvrchdtLines.SetFocus;
      end;
      jvrchdtLines.SelStart := FoundAt;
      jvrchdtLines.SelLength := Length(FindDialog1.FindText);
    end
    else
    begin
      SearchTerm := FindDialog1.FindText;
      if not (frMatchCase in FindDialog1.Options) then
      begin
        SearchTerm := UpperCase(SearchTerm);
      end;

      try
        if Backwards then
        begin
          NextLineIndex := FListFile.CurrentStartLine - LineConcatCount;
          LinesToRead := LineConcatCount;
          if NextLineIndex < 0 then
          begin
            LinesToRead := LinesToRead + NextLineIndex;
            NextLineIndex := 0
          end;
          StartLine := NextLineIndex+LinesToRead;
        end
        else
        begin
          NextLineIndex := FListFile.CurrentStartLine
            + jvrchdtLines.Lines.Count;
          StartLine := 0;

          LinesToRead := LineConcatCount;
        end;
        ProgressBar1.Max := (FListFile.LineCount div 1000) + 1;
        FoundCount := 0;
        NotFoundCount := 0;
        if Backwards then
        begin
          repeat
            Application.ProcessMessages;
            if FAborting then
            begin
              Exit;
            end;
            ProgressBar1.Position := NextLineIndex div 1000;
            FListFile.ReadLines(ALine,NextLineIndex, StartLine-NextLineIndex);
            StartLine := NextLineIndex;

            Dec(NextLineIndex,LinesToRead);
            if NextLineIndex < 0 then
            begin
              LinesToRead := -NextLineIndex;
              NextLineIndex := 0;
            end;
            if not (frMatchCase in FindDialog1.Options) then
            begin
              ALine := UpperCase(ALine);
            end;
            WordPosition := BmPosSimple(AnsiString(SearchTerm),
              AnsiString(ALine));
            if WordPosition > 0 then
            begin
              Inc(FoundCount);
              NotFoundCount := 0;
              PriorPostions.Clear;
              PriorPostions.Add(WordPosition);
              PriorWordPostion := WordPosition;
              WordPosition := BmPos(AnsiString(SearchTerm), AnsiString(ALine),
                True, WordPosition+1);
              while WordPosition > 0 do
              begin
                PriorWordPostion := WordPosition;
                WordPosition := BmPos(AnsiString(SearchTerm),
                  AnsiString(ALine), True, WordPosition+1);
                PriorPostions.Add(WordPosition);
              end;
              WordPosition := PriorWordPostion;
              PriorPostions.Delete(PriorPostions.Count-1);
            end
            else
            begin
              FoundCount := 0;
              Inc(NotFoundCount);
            end;
            if FoundCount >= 3 then
            begin
              LinesToRead := Max(LinesToRead div 2, MinLines);
              FoundCount := 0;
            end;
            if NotFoundCount >= 3 then
            begin
              LinesToRead := Min(LinesToRead *2, MaxLines);
              NotFoundCount := 0;
            end;
            if WordPosition > 0 then
            begin
              FoundWord := True;
              if frWholeWord in FindDialog1.Options then
              begin
                FoundWord := False;
                for WordIndex := PriorPostions.Count - 1 downto 0 do
                begin
                  WordPosition := PriorPostions[WordIndex];
                  if not WholeWord(WordPosition, ALine, SearchTerm) then
                  begin
                    Continue;
                  end;
                  FoundWord := True;
                  Break;
                end;
              end;
              if FoundWord then
              begin
                CurrentLines := TStringList.Create;
                try
                  CurrentLines.Text := ALine;
                  for LineIndex := CurrentLines.Count - 1 downto 0 do
                  begin
                    AnotherLine := CurrentLines[LineIndex];
                    PriorPostions.Clear;
                    WordPosition := Pos(SearchTerm, AnotherLine);
                    while WordPosition > 0 do
                    begin
                      PriorPostions.Add(WordPosition);
                      WordPosition := PosEx(SearchTerm, AnotherLine,
                        WordPosition+1);
                    end;
                    for WordIndex := PriorPostions.Count - 1 downto 0 do
                    begin
                      WordPosition := PriorPostions[WordIndex];
                      if frWholeWord in FindDialog1.Options then
                      begin

                        if WholeWord(WordPosition, AnotherLine,
                          SearchTerm) then
                        begin
                          frameListing.GoToLine(StartLine+LineIndex);
                          NewCaretPos.Y := 0;
                          NewCaretPos.X :=
                            Length(jvrchdtLines.Lines[0]);
                          jvrchdtLines.CaretPos := NewCaretPos;
                          FindDialog1Find(Sender);
                          Exit;
                        end;
                      end
                      else
                      begin
                          frameListing.GoToLine(StartLine+LineIndex);
                          NewCaretPos.Y := 0;
                          NewCaretPos.X :=
                            Length(jvrchdtLines.Lines[0]);
                          jvrchdtLines.CaretPos := NewCaretPos;
                          FindDialog1Find(Sender);
                          Exit;
                      end;
                    end;
                  end;
                finally
                  CurrentLines.Free;
                end;
                Exit;
              end;
            end
          until (StartLine-NextLineIndex <= 0);
        end
        else
        begin
          while NextLineIndex < FListFile.LineCount do
          begin
            Application.ProcessMessages;
            if FAborting then
            begin
              Exit;
            end;
            ProgressBar1.Position := NextLineIndex div 1000;
            StartLine := NextLineIndex;
            FListFile.ReadLines(ALine,NextLineIndex, LinesToRead);

            Inc(NextLineIndex,LinesToRead);
            if NextLineIndex > FListFile.LineCount then
            begin
              NextLineIndex := FListFile.LineCount
            end;
            if not (frMatchCase in FindDialog1.Options) then
            begin
              ALine := UpperCase(ALine);
            end;
            WordPosition := BmPosSimple(AnsiString(SearchTerm),
              AnsiString(ALine));
            if WordPosition > 0 then
            begin
              Inc(FoundCount);
              NotFoundCount := 0;
            end
            else
            begin
              FoundCount := 0;
              Inc(NotFoundCount);
            end;
            if FoundCount >= 3 then
            begin
              LinesToRead := Max(LinesToRead div 2, MinLines);
              FoundCount := 0;
            end;
            if NotFoundCount >= 3 then
            begin
              LinesToRead := Min(LinesToRead *2, MaxLines);
              NotFoundCount := 0;
            end;
            if WordPosition > 0 then
            begin
              if frWholeWord in FindDialog1.Options then
              begin
                if not WholeWord(WordPosition, ALine, SearchTerm) then
                begin
                  Continue;
                end;
              end;
              CurrentLines := TStringList.Create;
              try
                CurrentLines.Text := ALine;
                for LineIndex := 0 to CurrentLines.Count - 1 do
                begin
                  if Pos(SearchTerm, CurrentLines[LineIndex]) > 0 then
                  begin
                    frameListing.GoToLine(StartLine+LineIndex);
                    FindDialog1Find(Sender);
                    break;
                  end;
                end;
              finally
                CurrentLines.Free;
              end;
              Exit;
            end;
          end;
        end;
        Beep;
      finally
        ProgressBar1.Position := 0;
      end;
    end;
  finally
    jvrchdtLines.Tag := 0;
    PriorPostions.Free;
    frameListing.ChangeLineNumber(FListFile.CurrentStartLine);
    frameErrors.ChangeLineNumber(FListFile.CurrentStartLine);
    frameWarning.ChangeLineNumber(FListFile.CurrentStartLine);
//    lblFileCount.Caption := IntToStr(1 + FListFile.CurrentStartLine);
    LineNumber := jvrchdtLines.CaretPos.Y + FListFile.CurrentStartLine + 1;
    lblFileCount.Caption := IntToStr(LineNumber);
    memoLinesClick(nil);
    FListFile.CloseFile;
  end;
end;

procedure TfrmMain.AlternateFindDialog1Find(Sender: TObject);
var
  FoundAt: LongInt;
  StartPos, ToEnd: Integer;
  mySearchTypes : TRichSearchTypes;
  NextLineIndex: integer;
  SearchTerm: string;
  StartLine: integer;
  FoundCount: integer;
  NotFoundCount: integer;
  LinesToRead: integer;
  Backwards: boolean;
  NewCaretPos: TPoint;
  ABuffer: TCharArray;
  Options: TStringSearchOptions;
  StartChar: PChar;
  FoundChar: PChar;
  NewLine: integer;
begin
  // This procedure is somewhat slower than FindDialog1Find.
  jvrchdtLines.Tag := 1;
  try
    FAborting := False;
    mySearchTypes := [];
    Options := [];

    if frMatchCase in FindDialog1.Options then
    begin
      mySearchTypes := mySearchTypes + [stMatchCase];
      Options := Options + [soMatchCase];
    end;
    if frWholeWord in FindDialog1.Options then
    begin
      mySearchTypes := mySearchTypes + [stWholeWord];
      Options := Options + [soWholeWord];
    end;
    Backwards := not (frDown in FindDialog1.Options);
    if Backwards then
    begin
      mySearchTypes := mySearchTypes + [stBackward];
    end
    else
    begin
      Options := Options + [soDown];
    end;

    { Begin the search after the current selection, if there is one. }
    { Otherwise, begin at the cursor position. }
    if (jvrchdtLines.SelLength <> 0) and not Backwards then
      StartPos := jvrchdtLines.SelStart + jvrchdtLines.SelLength
    else
      StartPos := jvrchdtLines.SelStart;
    { ToEnd is the length from StartPos through the end of the
      text in the rich edit control. }
    if not Backwards then
    begin
      ToEnd := Length(Text) - StartPos;
    end
    else
    begin
      ToEnd := StartPos;
    end;
    FoundAt :=
      jvrchdtLines.FindText(FindDialog1.FindText, StartPos, ToEnd, mySearchTypes);
    if FoundAt <> -1 then
    begin
      if jvrchdtLines.CanFocus then
      begin
        jvrchdtLines.SetFocus;
      end;
      jvrchdtLines.SelStart := FoundAt;
      jvrchdtLines.SelLength := Length(FindDialog1.FindText);
      memoLinesClick(nil);
    end
    else
    begin
      SearchTerm := FindDialog1.FindText;
      try
        if Backwards then
        begin
          NextLineIndex := FListFile.CurrentStartLine - LineConcatCount;
          LinesToRead := LineConcatCount;
          if NextLineIndex < 0 then
          begin
            LinesToRead := LinesToRead + NextLineIndex;
            NextLineIndex := 0
          end;
          StartLine := NextLineIndex+LinesToRead;
        end
        else
        begin
          NextLineIndex := FListFile.CurrentStartLine
            + jvrchdtLines.Lines.Count;
          StartLine := 0;

          LinesToRead := LineConcatCount;
        end;
        ProgressBar1.Max := (FListFile.LineCount div 1000) + 1;
        FoundCount := 0;
        NotFoundCount := 0;
        if Backwards then
        begin
          repeat
            Application.ProcessMessages;
            if FAborting then
            begin
              Exit;
            end;
            ProgressBar1.Position := NextLineIndex div 1000;
            FListFile.ReadLines(ABuffer,NextLineIndex, StartLine-NextLineIndex);
            StartLine := NextLineIndex;

            Dec(NextLineIndex,LinesToRead);
            if NextLineIndex < 0 then
            begin
              LinesToRead := -NextLineIndex;
              NextLineIndex := 0;
            end;

            StartChar := Addr(ABuffer[0]);

            FoundChar := SearchBuf(StartChar, Length(ABuffer),
              Length(ABuffer)-1, 0, SearchTerm, Options);

            if FoundChar <> nil then
            begin
              Inc(FoundCount);
              NotFoundCount := 0;
            end
            else
            begin
              FoundCount := 0;
              Inc(NotFoundCount);
            end;
            if FoundCount >= 3 then
            begin
              LinesToRead := Max(LinesToRead div 2, MinLines);
              FoundCount := 0;
            end;
            if NotFoundCount >= 3 then
            begin
              LinesToRead := Min(LinesToRead *2, MaxLines);
              NotFoundCount := 0;
            end;
            if FoundChar <> nil then
            begin
              NewLine := FListFile.GetNewLineFromOffset(StartLine, FoundChar-StartChar);
              frameListing.GoToLine(NewLine);
              NewCaretPos.Y := 0;
              NewCaretPos.X :=
                Length(jvrchdtLines.Lines[0]);
              jvrchdtLines.CaretPos := NewCaretPos;
              AlternateFindDialog1Find(Sender);
              Exit;
            end
          until (StartLine-NextLineIndex <= 0);
        end
        else
        begin
          while NextLineIndex < FListFile.LineCount do
          begin
            Application.ProcessMessages;
            if FAborting then
            begin
              Exit;
            end;
            ProgressBar1.Position := NextLineIndex div 1000;
            StartLine := NextLineIndex;
            FListFile.ReadLines(ABuffer,NextLineIndex, LinesToRead);

            Inc(NextLineIndex,LinesToRead);
            if NextLineIndex > FListFile.LineCount then
            begin
              NextLineIndex := FListFile.LineCount
            end;

            StartChar := Addr(ABuffer[0]);

            FoundChar := SearchBuf(StartChar, Length(ABuffer),
              0, 0, SearchTerm, Options);

            if FoundChar <> nil then
            begin
              Inc(FoundCount);
              NotFoundCount := 0;
            end
            else
            begin
              FoundCount := 0;
              Inc(NotFoundCount);
            end;
            if FoundCount >= 3 then
            begin
              LinesToRead := Max(LinesToRead div 2, MinLines);
              FoundCount := 0;
            end;
            if NotFoundCount >= 3 then
            begin
              LinesToRead := Min(LinesToRead *2, MaxLines);
              NotFoundCount := 0;
            end;
            if FoundChar <> nil then
            begin
              NewLine := FListFile.GetNewLineFromOffset(StartLine,
                FoundChar-StartChar);
              frameListing.GoToLine(NewLine);
              AlternateFindDialog1Find(Sender);
              Exit;
            end;
          end;
        end;
        Beep;
      finally
        ProgressBar1.Position := 0;
      end;
    end;
  finally
    jvrchdtLines.Tag := 0;
    frameListing.ChangeLineNumber(FListFile.CurrentStartLine);
    frameErrors.ChangeLineNumber(FListFile.CurrentStartLine);
    frameWarning.ChangeLineNumber(FListFile.CurrentStartLine);
    frameSorted.ChangeLineNumber(FListFile.CurrentStartLine);
    lblFileCount.Caption := IntToStr(1 + FListFile.CurrentStartLine);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSearcher := TModflowSearchTrie.Create;
//  FQuickSearcher := TQuickSearchTrie.Create;
  InitializeSearchTrie;

  FShowIndexFile := ifcYes;//ifcUnknown;
  Caption := strListingAnalyst;

  rdgTable.Font := jvrchdtLines.Font;
  jreLineNumbers.Font := jvrchdtLines.Font;

  frameWarning.Memo := jvrchdtLines;
  frameWarning.memoLineNumbers := jreLineNumbers;
  frameWarning.LineCountToRead := spinLineCount;

  frameErrors.Memo := jvrchdtLines;
  frameErrors.memoLineNumbers := jreLineNumbers;
  frameErrors.LineCountToRead := spinLineCount;

  frameListing.Memo := jvrchdtLines;
  frameListing.memoLineNumbers := jreLineNumbers;
  frameListing.LineCountToRead := spinLineCount;

  frameSorted.Memo := jvrchdtLines;
  frameSorted.memoLineNumbers := jreLineNumbers;
  frameSorted.LineCountToRead := spinLineCount;

  if ParamCount > 0 then
  begin
    FShouldOpenFile := True;
  end;

  pgcDisplay.ActivePageIndex := 0;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FListFile.Free;
//  FQuickSearcher.Free;
  FSearcher.Free;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  L = 76;
  P = 80;
  T = 84;
var
  NodeData: PIndexDataNode;
  frame: TframeFileListHandler;
  SearchTerm: string;
  SeachPos: Integer;
  ANode: PVirtualNode;
  FollowingText: string;
  SearchTerm2: string;
  FollowingText2: string;
  SearchTerm3: string;
  FollowingText3: string;
  SearchTerms: TStringList;
  NextNode: PVirtualNode;
  function ExtractFollowingText(const SearchTerm, Text: string): string;
  var
    SeachPos: Integer;
  begin
    SeachPos := Pos(SearchTerm, UpperCase(Text));
    if SeachPos > 0 then
    begin
      result := Trim(Copy(Text,
        SeachPos + Length(SearchTerm), MaxInt));
      SeachPos := Pos(' ', result);
      if SeachPos > 0 then
      begin
        result := Copy(result, 1, SeachPos-1);
      end;
    end
    else
    begin
      result := '';
    end;
  end;
  function ExtractNonNumericText(Text: string): string;
  var
    index: Integer;
    AValue: double;
  begin
    SearchTerms.DelimitedText := StringReplace(Trim(Text), ',', ' ',
        [rfReplaceAll, rfIgnoreCase]);
    for index := SearchTerms.Count - 1 downto 0 do
    begin
      if TryStrToFloat(SearchTerms[index], AValue) then
      begin
        SearchTerms.Delete(index);
      end;
    end;
    result := SearchTerms.DelimitedText;
  end;
begin
  if (ssCtrl in Shift)
    and (Key in [L, P, T, VK_UP, VK_DOWN, VK_LEFT, VK_RETURN]) then
  begin
    frame := nil;
    case pgcIndex.ActivePageIndex of
      0:
        begin
          frame := frameListing;
        end;
      1:
        begin
          frame := frameSorted;
        end;
      2:
        begin
          frame := frameErrors;
        end;
      3:
        begin
          frame := frameWarning;
        end;
      else
        Assert(False);
    end;
    ANode := frame.SelectedNode;
    if ANode = nil then
    begin
      ANode := frame.vstIndexLines.GetFirst;
    end;
    if  (ANode <> nil) then
    begin
      if Key = VK_RETURN then
      begin
        if frame.vstIndexLines.HasChildren[ANode] then
        begin
          frame.vstIndexLines.Expanded[ANode] :=
            not frame.vstIndexLines.Expanded[ANode];
        end;
      end
      else if Key = VK_LEFT then
      begin
        ANode := frame.vstIndexLines.NodeParent[ANode];
      end
      else if Key in [L, P, T] then
      begin
        NodeData := frame.vstIndexLines.GetNodeData(ANode);
        SearchTerm2 := '';
        SearchTerm3 := '';
        case Key of
          L:
            begin
              SearchTerm := 'IN LAYER';
              SearchTerm2 := 'IN STRESS PERIOD';
              SearchTerm3 := 'TIME STEP';
            end;
          P:
            begin
              SearchTerm := 'PERIOD NO.';
            end;
          T:
            begin
//              SearchTerm := 'SOLVING FOR HEAD TIME STEP';
//              SearchTerm2 := 'IN STRESS PERIOD';
              SearchTerm := 'SOLVING FOR HEAD STRESS PERIOD';
              SearchTerm2 := 'IN TIME STEP';
            end;
          else Assert(False);
        end;
        SeachPos := Pos(SearchTerm, UpperCase(NodeData.Text));
        if SeachPos > 0 then
        begin
          FollowingText := ExtractFollowingText(SearchTerm, NodeData.Text);
          if SearchTerm2 <> '' then
          begin
            FollowingText2 := ExtractFollowingText(SearchTerm2, NodeData.Text);
          end
          else
          begin
            FollowingText2 := '';
          end;
          if SearchTerm3 <> '' then
          begin
            FollowingText3 := ExtractFollowingText(SearchTerm3, NodeData.Text);
          end
          else
          begin
            FollowingText3 := '';
          end;
        end
        else
        begin
          FollowingText := '';
          FollowingText2 := '';
          FollowingText3 := '';
        end;
  //      ANode := frame.SelectedNode;
        while ANode <> nil do
        begin
          NodeData := frame.vstIndexLines.GetNodeData(ANode);
          SeachPos := Pos(SearchTerm, UpperCase(NodeData.Text));
          if SeachPos > 0 then
          begin
            if FollowingText <>
              ExtractFollowingText(SearchTerm, NodeData.Text) then
            begin
              break;
            end
            else if SearchTerm2 <> '' then
            begin
              if FollowingText2 <>
                ExtractFollowingText(SearchTerm2, NodeData.Text) then
              begin
                break;
              end
              else if SearchTerm3 <> '' then
              begin
                if FollowingText3 <>
                  ExtractFollowingText(SearchTerm3, NodeData.Text) then
                begin
                  break;
                end
              end;
            end;
          end;
          if ssShift in Shift then
          begin
            ANode := frame.vstIndexLines.GetPrevious(ANode);
          end
          else
          begin
            ANode := frame.vstIndexLines.GetNext(ANode);
          end;
        end;
      end
      else
      begin
        Assert(Key in [VK_UP, VK_DOWN]);
        if ssShift in Shift then
        begin
          SearchTerms := TStringList.Create;
          try
            SearchTerms.Delimiter := ' ';
            NodeData := frame.vstIndexLines.GetNodeData(ANode);
            SearchTerm := ExtractNonNumericText(NodeData.Text);
            case Key of
              VK_UP: ANode := frame.vstIndexLines.GetPrevious(ANode);
              VK_DOWN: ANode := frame.vstIndexLines.GetNext(ANode);
              else
                Assert(False);
            end;
            while ANode <> nil do
            begin
              NodeData := frame.vstIndexLines.GetNodeData(ANode);
              if SearchTerm = ExtractNonNumericText(NodeData.Text) then
              begin
                break;
              end;
              case Key of
                VK_UP: ANode := frame.vstIndexLines.GetPrevious(ANode);
                VK_DOWN: ANode := frame.vstIndexLines.GetNext(ANode);
                else
                  Assert(False);
              end;
            end;

          finally
            SearchTerms.Free;
          end;
        end
        else
        begin
          case Key of
            VK_UP:
              begin
                NextNode := frame.vstIndexLines.GetPreviousSibling(ANode);
                if NextNode <> nil then
                begin
                  while frame.vstIndexLines.HasChildren[NextNode]
                    and frame.vstIndexLines.Expanded[NextNode] do
                  begin
                    NextNode := frame.vstIndexLines.GetLastChild(NextNode);
                  end;
                end
                else
                begin
                  NextNode := frame.vstIndexLines.NodeParent[ANode];
                end;
                ANode := NextNode;
              end;
            VK_DOWN:
              begin
                if frame.vstIndexLines.HasChildren[ANode]
                  and frame.vstIndexLines.Expanded[ANode] then
                begin
                  NextNode := frame.vstIndexLines.GetFirstChild(ANode);
                end
                else
                begin
                  NextNode := frame.vstIndexLines.GetNextSibling(ANode);
                end;
                if NextNode = nil then
                begin
                  NextNode := frame.vstIndexLines.NodeParent[ANode];
                  if NextNode <> nil then
                  begin
                    NextNode := frame.vstIndexLines.GetNextSibling(NextNode);
                  end;
                end;
                if NextNode = nil then
                begin
                  NextNode := frame.vstIndexLines.GetNext(ANode);
                end;
                ANode := NextNode;
              end;
            else
              Assert(False);
          end;
        end;
      end;
      if ANode <> nil then
      begin
        frame.SelectedNode := ANode;
      end
      else
      begin
        Beep;
      end;
    end;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if FShouldOpenFile then
  begin
    FShouldOpenFile := False;
    tmrOpenFile.Enabled := True;
    Assert(ParamCount > 0);
  end;
end;

procedure TfrmMain.frameListingvstIndexLinesNodeDblClick(
  Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  tabTable.TabVisible := False;
  tabLines.TabVisible := False;
  pgcDisplay.ActivePage := tabLines;
  frameListing.vstIndexLinesNodeDblClick(Sender, HitInfo);
  Screen.cursor := crHourGlass;
  try
    if not DisplayObservations then
    begin
      if not DisplayArray then
      begin
        tabTable.TabVisible := False;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

end;

procedure TfrmMain.frameSortedvstIndexLinesCompareNodes(
  Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var
  NodeData1: PIndexDataNode;
  NodeData2: PIndexDataNode;
begin
  NodeData1 := frameSorted.vstIndexLines.GetNodeData(Node1);
  NodeData2 := frameSorted.vstIndexLines.GetNodeData(Node2);
  if (NodeData1.LineNumber >= 0) and (NodeData2.LineNumber >= 0) then
  begin
    result := 0;
  end
  else
  begin
    result := AnsiCompareText(NodeData1.Text, NodeData2.Text)
  end;
end;

procedure TfrmMain.ListFileProgress(Sender: TObject; PerMil: integer);
begin
  ProgressBar1.Position := PerMil;
  Application.ProcessMessages;
end;

procedure TfrmMain.memoLinesClick(Sender: TObject);
var
  LineNumber: integer;
begin
  if jvrchdtLines.Tag <> 0 then
  begin
    Exit;
  end;
  if jvrchdtLines.Lines.Count = 0 then
  begin
    Exit;
  end;
  LineNumber := jvrchdtLines.CaretPos.Y + FListFile.CurrentStartLine;
  frameListing.ChangeLineNumber(LineNumber);
  frameErrors.ChangeLineNumber(LineNumber);
  frameWarning.ChangeLineNumber(LineNumber);
  frameSorted.ChangeLineNumber(LineNumber);
  lblFileCount.Caption := IntToStr(1+LineNumber);
  Screen.cursor := crHourGlass;
  try
    if not DisplayObservations then
    begin
      if not DisplayArray then
      begin
        tabTable.TabVisible := False;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.OpenFile(FileName: string);
var
  Divisor: Integer;
//  AnIndexer: TIndexingThread;
begin
  Caption := Format('%0:s: %1:s', [FileName, StrListingAnalyst]);
  FAborting := False;
  FListFile.Free;
  FListFile := TFileIndex.Create;
  frameWarning.ListFile := FListFile;
  frameErrors.ListFile := FListFile;
  frameListing.ListFile := FListFile;
  frameSorted.ListFile := FListFile;
  ProgressBar1.Max := 1000;
  FListFile.OnProgress := ListFileProgress;
  FListFile.OnStartLineChange := FilePositionChanged;
  frameWarning.Initialize;
  frameErrors.Initialize;
  frameListing.Initialize;
  frameSorted.Initialize;
  PriorStepString := '';
  PriorSP := '';
  NextIndent := 0;
  PriorIndent := 2;
  PriorStressPeriod := '';
  FModflow6 := False;
  if FShowIndexFile = ifcYes then
  begin
    FListFile.OnIndexSomeLines := IndexSomeLines;
    FListFile.OnUpdateTimeSteps := UpdateTimeSteps;
  end;

  // Specifying FListFile.FileName cause the file to be read.
  FListFile.FileName := FileName;
  frameSorted.vstIndexLines.SortTree(0, sdAscending);
  progressbar1.Position := 0;
  Divisor := (FListFile.LineCount div 1000) + 1;
  ProgressBar1.Max := Divisor;
  lblFileCount.Caption := IntToStr(FListFile.LineCount-1);
  rdeLineTarget.Max := FListFile.LineCount;
  btnIndex.Enabled := True;
  btnGoTo.Enabled := True;
  btnFind.Enabled := True;
  frameListing.GoToLine(0);
  FFileDate := TFile.GetLastWriteTimeUtc(FListFile.FileName);
end;

procedure TfrmMain.redtLines1SelectionChange(Sender: TObject);
begin
  memoLinesClick(Sender);
  if jvrchdtLines.Tag = 0 then
  begin
    jvrchdtLinesVerticalScroll(Sender);
  end;
end;

procedure TfrmMain.sbLinesScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if FChangingPosition or (FListFile = nil) then
  begin
    Exit;
  end;
  FChangingPosition := True;
  try
    jvrchdtLines.Tag := 1;
    try
      frameListing.GoToLine(FListFile.LineCount div sbLines.Max * ScrollPos );
      frameListing.ChangeLineNumber(FListFile.CurrentStartLine);
      frameSorted.ChangeLineNumber(FListFile.CurrentStartLine);
      frameErrors.ChangeLineNumber(FListFile.CurrentStartLine);
      frameWarning.ChangeLineNumber(FListFile.CurrentStartLine);
      lblFileCount.Caption := IntToStr(1 + FListFile.CurrentStartLine);
    finally
      jvrchdtLines.Tag := 0;
    end;
    jvrchdtLinesVerticalScroll(Sender);
  finally
    FChangingPosition := False;
  end;
end;

procedure TfrmMain.tmrOpenFileTimer(Sender: TObject);
var
  AFileName: string;
  Option: string;
  OutputFileName: string;
  ContentsLines: TStringList;
  ANode: PVirtualNode;
  NodeData: PIndexDataNode;
  CellText: string;
begin
  tmrOpenFile.Enabled := False;
  Assert(ParamCount > 0);
  AFileName := ParamStr(1);
  if FileExists(AFileName) then
  begin
    OpenAFile(AFileName);
    if (ParamCount > 1) then
    begin
      Option := ParamStr(2);
      if Option = '-e' then
      begin
        if (ParamCount > 2) then
        begin
          OutputFileName := ParamStr(3);
        end
        else
        begin
          OutputFileName := ChangeFileExt(AFileName, '.txt')
        end;
        ContentsLines := TStringList.Create;
        try
          ANode := frameListing.vstIndexLines.GetFirst();
          while ANode <> nil do
          begin
            NodeData := frameListing.vstIndexLines.GetNodeData(ANode);
            CellText := NodeData.Text;
            ContentsLines.Add(NodeData.LineNumber.ToString + #9 + CellText);
            ANode := frameListing.vstIndexLines.GetNext(ANode, False)
          end;
          ContentsLines.SaveToFile(OutputFileName);
        finally
          ContentsLines.Free;
        end;
        Close;
      end;
    end;
  end;
end;

function TfrmMain.DisplayArray: Boolean;
const
  LinesToRead = 1000;
var
  LineIndex: integer;
  BlankCount : integer;
  ALine: string;
  FoundArray: Boolean;
  ArrayStart: integer;
  Splitter: TStringList;
  LabelStart: Integer;
  Labels: TStringList;
  DataRow: TStringList;
  TableRows: TList<TStringList>;
  ColIndex: Integer;
  RowIndex: Integer;
  NumIndex: Integer;
  AFloat: double;
  ErrorCode: integer;
  EndFound: Boolean;
  StartLine: Integer;
  Lines: TStringList;
  ActiveLineNo: integer;
  ScrollBarActive: Boolean;
  DisV: Boolean;
begin
  try
    result := False;
    if not TryReopenFile then
    begin
      Exit;
    end;
    BlankCount := 0;
    FoundArray := False;
    ArrayStart := -1;
    ActiveLineNo := SendMessage(jvrchdtLines.Handle,
      EM_LINEFROMCHAR, WPARAM(-1), 0);
    for LineIndex := ActiveLineNo to jvrchdtLines.Lines.Count -1 do
    begin
      ALine := Trim(jvrchdtLines.Lines[LineIndex]);
      if ALine = '' then
      begin
        Inc(BlankCount);
      end;
      if Pos('PRINT ', ALine) > 0 then
      begin
        Break;
      end;
      if Pos('SAVE ', ALine) > 0 then
      begin
        Break;
      end;
      if Pos('SAVING ', ALine) > 0 then
      begin
        Break;
      end;
      if BlankCount >= 3 then
      begin
        break;
      end;
      if Pos('........', ALine) = 1 then
      begin
        FoundArray := True;
        ArrayStart := LineIndex + 1;
        break;
      end;
    end;
    if FoundArray then
    begin
      LabelStart := -1;
      for LineIndex := ArrayStart-2 downto 0 do
      begin
        ALine := jvrchdtLines.Lines[LineIndex];
        if Pos(' 1 ', ALine) > 0 then
        begin
          LabelStart := LineIndex;
          tabTable.Caption := 'Table (Line: ' + IntToStr(LabelStart
             + frameListing.ListFile.CurrentStartLine +1) + ')';
          break;
        end;
        if ALine = '' then
        begin
          LabelStart := LineIndex+1;
          tabTable.Caption := 'Table (Line: ' + IntToStr(LabelStart
             + frameListing.ListFile.CurrentStartLine +1) + ')';
          break;
        end;
      end;
      if LabelStart >= 0 then
      begin
        rdgTable.FixedCols := 1;
        Splitter := TStringList.Create;
        Labels := TStringList.Create;
        TableRows := TList<TStringList>.Create;
        try
          Splitter.Delimiter := ' ';
          for LineIndex := LabelStart to ArrayStart-2 do
          begin
            ALine := jvrchdtLines.Lines[LineIndex];
            Splitter.DelimitedText := ALine;
            Labels.AddStrings(Splitter);
          end;
          DataRow := nil;

          DisV := Labels.Count = 0;

          EndFound := False;
          StartLine := ArrayStart + frameListing.ListFile.CurrentStartLine;
          Lines := TStringList.Create;
          try
            while not EndFound do
            begin
              frameListing.ListFile.ReadLines(Lines, StartLine, LinesToRead);
              Inc(StartLine, LinesToRead);
              if Lines.Count = 0 then
              begin
                EndFound := True;
              end;
              if StartLine >= frameListing.ListFile.LineCount then
              begin
                EndFound := True;
              end;
              for LineIndex := 0 to Lines.Count - 1 do
              begin
                ALine := Lines[LineIndex];
                if (Trim(ALine) = '') then
                begin
                  if (LineIndex > 0) then
                  begin
                    EndFound := True;
                    break;
                  end
                  else
                  begin
                    Continue;
                  end;
                end;
                if ((Trim(ALine) = '1')) then
                begin
                  EndFound := True;
                  break;
                end;
                Splitter.DelimitedText := ALine;
                ErrorCode := 0;
                for NumIndex := 0 to Splitter.Count - 1 do
                begin
                  Val(Splitter[NumIndex], AFloat, ErrorCode);
                  if ErrorCode <> 0 then
                  begin
                    EndFound := True;
                    Break;
                  end;
                end;
                if ErrorCode <> 0 then
                begin
                  Break;
                end;
                if DataRow = nil then
                begin
                  DataRow := TStringList.Create;
                  TableRows.Add(DataRow);
                end;
                DataRow.AddStrings(Splitter);
                if (not DisV) and (DataRow.Count >= Labels.Count + 1) then
                begin
                  DataRow := nil;
                end;
              end;
            end;
          finally
            Lines.Free;
          end;

          if TableRows.Count > 0 then
          begin
            rdgTable.BeginUpdate;
            try
              rdgTable.ColCount := Labels.Count + 1;
              rdgTable.RowCount := TableRows.Count + 1;
              for RowIndex := 0 to rdgTable.RowCount - 1 do
              begin
                rdgTable.RowHeights[RowIndex] := rdgTable.DefaultRowHeight;
              end;
              for ColIndex := 0 to rdgTable.ColCount - 1 do
              begin
                rdgTable.Columns[ColIndex].AutoAdjustColWidths := True;
                rdgTable.ColWidths[ColIndex] := rdgTable.DefaultColWidth;
              end;
              for RowIndex := 0 to rdgTable.RowCount - 1 do
              begin
                for ColIndex := 0 to rdgTable.ColCount - 1 do
                begin
                  rdgTable.Cells[ColIndex,RowIndex] := '';
                end;
              end;
              for ColIndex := 0 to Labels.Count - 1 do
              begin
                rdgTable.Cells[ColIndex+1, 0] := Labels[ColIndex];
              end;
              for RowIndex := 0 to TableRows.Count - 1 do
              begin
                DataRow := TableRows[RowIndex];
                if rdgTable.ColCount < DataRow.Count then
                begin
                  rdgTable.ColCount := DataRow.Count;
                end;
                for ColIndex := 0 to DataRow.Count - 1 do
                begin
                  rdgTable.Cells[ColIndex, RowIndex+1] := DataRow[ColIndex];
                end;
              end;
              if DisV then
              begin
                for ColIndex := 1 to rdgTable.ColCount - 1 do
                begin
                  rdgTable.Columns[ColIndex].AutoAdjustColWidths := True;
                  if rdgTable.Cells[ColIndex,0] = '' then
                  begin
                    rdgTable.Cells[ColIndex,0] := IntToStr(ColIndex);
                  end;
                end;
              end;
            finally
              rdgTable.EndUpdate;
            end;
            ScrollBarActive := sbLines.Focused;
            tabTable.TabVisible := True;
            tabLines.TabVisible := True;
            if ScrollBarActive then
            begin
              pgcDisplay.ActivePage := tabLines;
              sbLines.SetFocus;
            end;
            Result := True;
          end;
        finally
          TableRows.Free;
          Labels.Free;
          Splitter.Free;
        end;
      end;
    end;
  finally
    FListFile.CloseFile;
  end;  
end;

function TfrmMain.DisplayObservations: boolean;
const
  LinesToRead = 1000;
var
  FoundObservations: Boolean;
  ObservationLines: TStringList;
  ALine: string;
  LineIndex: Integer;
  Observations: TObsRecordArray;
  RowIndex: Integer;
  ColIndex: Integer;
  StartIndex: Integer;
  Lines: TStringList;
  StartLine: Integer;
  ScrollBarActive: Boolean;
  ActiveLineNo: integer;
begin
  try
    Result := False;
    if not TryReopenFile then
    begin
      Exit;
    end;
    FoundObservations := False;
    begin
      ActiveLineNo := SendMessage(jvrchdtLines.Handle,
        EM_LINEFROMCHAR, WPARAM(-1), 0);
      ALine := Trim(jvrchdtLines.Lines[ActiveLineNo]);
  //    ALine := frameListing.ActiveLine;
      if ObsIdentifiers.IndexOf(AnsiString(ALine)) >= 0 then
      begin
        ObservationLines := TStringList.Create;
        try
          StartIndex := -1;
          for LineIndex := 0 to jvrchdtLines.Lines.Count - 1 do
          begin
            if Pos(ALine, jvrchdtLines.Lines[LineIndex]) >= 1 then
            begin
              StartIndex := LineIndex;
              Break;
            end;
          end;
          if StartIndex >= 0 then
          begin
            for LineIndex := StartIndex + 1 to jvrchdtLines.Lines.Count - 1 do
            begin
              if Pos('----', jvrchdtLines.Lines[LineIndex]) > 0 then
              begin
                StartIndex := LineIndex + 1;
                FoundObservations := StartIndex < jvrchdtLines.Lines.Count;
                break;
              end;
            end;
          end;
          if FoundObservations then
          begin
            FoundObservations := False;

            StartLine := StartIndex + frameListing.ListFile.CurrentStartLine;
            Lines := TStringList.Create;
            try
              while not FoundObservations do
              begin
                frameListing.ListFile.ReadLines(Lines, StartLine, LinesToRead);
                Inc(StartLine, LinesToRead);
                if Lines.Count = 0 then
                begin
                  FoundObservations := true;
                end;
                for LineIndex := 0 to Lines.Count - 1 do
                begin
                  ALine := Trim(Lines[LineIndex]);
                  if ALine = '' then
                  begin
                    FoundObservations := true;
                    Break;
                  end
                  else
                  begin
                    ObservationLines.Add(ALine);
                  end;
                end;
              end;
            finally
              Lines.Free;
            end;

          end;
          if FoundObservations then
          begin
            rdgTable.FixedCols := 0;
            ExtractObservations(ObservationLines, Observations);
            Screen.Cursor := crHourGlass;
            rdgTable.BeginUpdate;
            try
              rdgTable.ColCount := 4;
              rdgTable.RowCount := Length(Observations) + 1;
              for RowIndex := 0 to rdgTable.RowCount - 1 do
              begin
                rdgTable.RowHeights[RowIndex] := rdgTable.DefaultRowHeight;
              end;
              for ColIndex := 0 to rdgTable.ColCount - 1 do
              begin
                rdgTable.Columns[ColIndex].AutoAdjustColWidths := True;
                rdgTable.ColWidths[ColIndex] := rdgTable.DefaultColWidth;
              end;
              rdgTable.Cells[Ord(ocName), 0] := 'Observations Name';
              rdgTable.Cells[Ord(ocObserved), 0] := 'Observed Value';
              rdgTable.Cells[Ord(ocSimulated), 0] := 'Simulated Value';
              rdgTable.Cells[Ord(ocDifference), 0] := 'Difference';
              for RowIndex := 1 to rdgTable.RowCount - 1 do
              begin
                rdgTable.Cells[Ord(ocName), RowIndex] :=
                  Observations[RowIndex - 1].Name;
                if Observations[RowIndex - 1].InvalidValues then
                begin
                  Continue;
                end;
                rdgTable.Cells[Ord(ocObserved), RowIndex] :=
                  FloatToStr(Observations[RowIndex - 1].ObservedValue);
                rdgTable.Cells[Ord(ocSimulated), RowIndex] :=
                  FloatToStr(Observations[RowIndex - 1].SimulatedValue);
                if not Observations[RowIndex - 1].DryCells then
                begin
                  rdgTable.Cells[Ord(ocDifference), RowIndex] :=
                    FloatToStr(Observations[RowIndex - 1].Difference);
                end;
              end;
              result := True;
            finally
              rdgTable.EndUpdate;
              Screen.Cursor := crDefault;
            end;
            ScrollBarActive := sbLines.Focused;
            tabTable.Caption := 'Table (Line: ' + IntToStr(ActiveLineNo
               + frameListing.ListFile.CurrentStartLine +1) + ')';
            tabTable.TabVisible := True;
            tabLines.TabVisible := True;
            if ScrollBarActive then
            begin
              pgcDisplay.ActivePage := tabLines;
              sbLines.SetFocus;
            end;
          end;
        finally
          ObservationLines.Free;
        end;
      end;
    end;
  finally
    FListFile.CloseFile;
  end;
end;

procedure TfrmMain.drgdrpFilesDrop(Sender: TObject; Pos: TPoint;
  Value: TStrings);
var
  AFileName: string;
  FileIndexUnit: integer;
begin
  for FileIndexUnit := 0 to Value.Count - 1 do
  begin
    AFileName := Value[FileIndexUnit];
    if FileExists(AFileName) then
    begin
      FShowIndexFile := ifcYes;
      OpenAFile(AFileName);
      break;
    end;
  end;
end;

function TfrmMain.SearchTrie(ALine: AnsiString; var IndexTypes: TIndexTypeArray;
    var KeyLength: integer; out FoundKey: string): boolean;
const
  DontBreak = [itStressPeriod, itIteration, itArray1, itNonID];
var
  Key: PAnsiChar;
  IndexType: TIndexType;
  TestLength: Integer;
  Index: Integer;
  StartIndex: integer;
begin
  Index := 0;
  result := False;
  Key := PAnsiChar(ALine);
  TestLength := Length(ALine);
  IndexTypes[1] := itNone;
  StartIndex:= 0;
  while (TestLength >= FMinLength) and (Key^ <> AnsiChar(0)) do
  begin
    if FSearcher.Find(Key, IndexType, KeyLength) then
    begin
      IndexTypes[Index] := IndexType;
      Inc(Index);
      if not result then
      begin
        Inc(StartIndex);
      end;
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
    if not result then
    begin
      Inc(StartIndex);
    end;
    Dec(TestLength);
  end;
  if result then
  begin
    FoundKey := Trim(Copy(string(ALine), StartIndex, KeyLength-1));
  end
  else
  begin
    FoundKey := '';
  end;
end;

procedure TfrmMain.IndexSomeLines(const MultipleLines: string; var Indent: Integer;
  NewTimeStepPostions, NewTimeStepLines: TListInt64; var LineIndex: Integer);
var
//  it: TIndexType;
  KeyLength: Integer;
  TrimmedLine: string;
  ALine: string;
  NewTimeStepPostion: Integer;
  RechBudPos: Integer;
  InnerLineIndex: Integer;
  IndexTypes: TIndexTypeArray;
  InnerLine: AnsiString;
  NumOK: Boolean;
  IdIndex: Integer;
  SomeLines: TStringList;
  CheckNumLines: boolean;
  AnsiMultipleLines: AnsiString;
//  Key: string;
  UseKey: Boolean;
  FoundKey: string;
  SP_Start: Integer;
  AnsiLine: AnsiString;
  TS_Start: Integer;
  StepLine: string;
  StressPeriod: string;
  TimeStep: string;
  PeriodSearchTerm: Ansistring;
  StepSearchTerm: Ansistring;
  AddToDictionaryCount: Integer;
  AddLineCount: Integer;
  function GetSP_Pos(const AnsiLine: AnsiString; out PeriodSearchTerm: AnsiString): Integer;
  begin
    PeriodSearchTerm := '';
    result := BMPos(Mf6StartTimeStep, AnsiLine);
    if result > 0 then
    begin
      PeriodSearchTerm := Mf6StartTimeStep;
      Exit;
    end;
    result := BMPos('STRESS PERIOD NO.', AnsiLine);
    if result > 0 then
    begin
      PeriodSearchTerm := 'STRESS PERIOD NO.';
      Exit;
    end;
    result := BMPos('STRESS PERIOD', AnsiLine);
    if result > 0 then
    begin
      PeriodSearchTerm := 'STRESS PERIOD';
      Exit;
    end;
    result := BMPos('PERIOD', AnsiLine);
    if result > 0 then
    begin
      PeriodSearchTerm := 'PERIOD';
      Exit;
    end;
    result := BMPos('stress period:', AnsiLine);
    if result > 0 then
    begin
      PeriodSearchTerm := 'stress period:';
      Exit;
    end;
  end;
  function ExtractStringBetween(const AString: string; BeforeString: AnsiString; AfterStrings: array of AnsiString): string;
  var
    BeforePosition: Integer;
    AfterPosition: Integer;
    AfterIndex: Integer;
    AfterString: AnsiString;
  begin
    BeforePosition := BMPos(AnsiString(BeforeString), AnsiString(AString));
    Assert(BeforePosition >= 1);

    Result := Trim(Copy(AString, BeforePosition + Length(BeforeString)));
    if Length(AfterStrings) <> 0 then
    begin
      for AfterIndex := 0 to Length(AfterStrings)- 1 do
      begin
        AfterString := AfterStrings[AfterIndex];
        if AfterString <> '' then
        begin
          AfterPosition := BMPos(AfterString, AnsiString(Result));
          if (AfterPosition >= 1) then
          begin
            Result := copy(Result, 1, AfterPosition-1);
            break;
          end;
        end;
      end;
    end;
    result := Trim(Result);
  end;
  procedure AddStressPeriod;
  var
    StepLine: String;
  begin
    StepLine := Format('Stress Period %0:s', [StressPeriod]);
    frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, StepLine);
    Inc(AddToDictionaryCount);
    frameListing.AddLine(LineIndex + InnerLineIndex, StepLine, 0);
    Inc(AddLineCount);
    frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
    Inc(AddToDictionaryCount);
    frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 1);
    Inc(AddLineCount);
    Indent := 1;
  end;
//  SpacePos: Integer;
begin
  AddToDictionaryCount := 0;
  AddLineCount := 0;

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
  if not CheckNumLines then
  begin
    for IdIndex := 0 to NumberPackageIdentifiers.Count - 1 do
    begin
      if BMPos(NumberPackageIdentifiers[IdIndex], AnsiMultipleLines) >= 1 then
      begin
        CheckNumLines := True;
        Break;
      end;
    end;
  end;

//  PriorStepString := '';
  SomeLines := TStringList.Create;
  try
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
      if TrimmedLine[1] = '#' then
      begin
        Continue;
      end;
      if (LineIndex + InnerLineIndex = 0)  and (TrimmedLine = 'MODFLOW 6') then
      begin
        FModflow6 := True;
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
            for IdIndex := 0 to NumberPackageIdentifiers.Count - 1 do
            begin
              if BMPos(NumberPackageIdentifiers[IdIndex], InnerLine) >= 1 then
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
      if SearchTrie(InnerLine, IndexTypes, KeyLength, FoundKey) then
      begin
        if (itNonID = IndexTypes[0]) or (itNonID = IndexTypes[1]) then
        begin
          Continue;
        end;
        // Skip "RECHARGE" when it is in the volumetric budget.
        // "RECHARGE" is repeated twice in the volumetric budget
        // which allows it to be identified.
        RechBudPos := BMPos(StrRECHARGE, AnsiString(ALine));
        if RechBudPos >= 1 then
        begin
          RechBudPos := PosEx(StrRECHARGE, ALine, RechBudPos + 1);
          if RechBudPos >= 1 then
          begin
            Continue;
          end;
        end;

//        Key := Copy(string(InnerLine),1,KeyLength);

        UseKey := False;
        if IndexTypes[0] in [itPackage, itObservation, itBoundary,
          itNumberBoundary, itIndent1, itIndent3, itNoIndentChange,
          itStartTimeStep, itArray2, itArray3, itEndModel, itParameter] then
        begin
          UseKey := True;
        end
        else if (IndexTypes[0] = itStressPeriod)
          and (IndexTypes[1] = itStressPeriod2) then
        begin
          UseKey := True;
        end
        else if (IndexTypes[0] = itIteration)
          and (IndexTypes[1] = itIteration2) then
        begin
          UseKey := True;
        end
        else if (IndexTypes[0] = itArray1)
          and (IndexTypes[1] = itArray2) then
        begin
          UseKey := True;
        end;

        if UseKey then
        begin
          if NextIndent <> 0 then
          begin
            Indent := NextIndent;
//            NextIndent := 0;
          end;
          if FModflow6 {and (IndexTypes[0] <> itStartTimeStep)} then
          begin
            AnsiLine := AnsiString(ALine);
            SP_Start := GetSP_Pos(AnsiLine, PeriodSearchTerm);
            if SP_Start > 0 then
            begin
              TS_Start := BMPos(Mf6Kstp, AnsiLine);
              if TS_Start > 0 then
              begin
                StepSearchTerm := Mf6Kstp
              end;

              if TS_Start <= 0 then
              begin
                TS_Start := BMPos('TIME STEP', AnsiLine);
                if TS_Start > 0 then
                begin
                  StepSearchTerm := 'TIME STEP'
                end;
              end;

              if TS_Start <= 0 then
              begin
                TS_Start := BMPos('time step', AnsiLine);
                if TS_Start > 0 then
                begin
                  StepSearchTerm := 'time step'
                end;
              end;

              if TS_Start <= 0 then
              begin
                TS_Start := BMPos('STEP', AnsiLine);
                if TS_Start > 0 then
                begin
                  StepSearchTerm := 'STEP'
                end;
              end;

              if TS_Start > 0 then
              begin
                if TS_Start > SP_Start then
                begin
//                  StepLine := Copy(ALine, SP_Start, MAXINT);
                  StressPeriod := ExtractStringBetween(ALine,PeriodSearchTerm, ['', 'STEP', ',', '"']);
                  TimeStep := ExtractStringBetween(ALine,StepSearchTerm, [',', 'IN', ':', '"']);
                  StepLine := Format('Stress Period %0:s, Time Step %1:s', [StressPeriod, TimeStep]);
                end
                else
                begin
//                  StepLine := Copy(ALine, TS_Start, MAXINT);
                  StressPeriod := ExtractStringBetween(ALine,PeriodSearchTerm, ['', 'STEP']);
                  TimeStep := ExtractStringBetween(ALine,'TIME STEP', [',', 'IN']);
                  StepLine := Format('Stress Period %0:s, Time Step %1:s', [StressPeriod, TimeStep]);
                end;
                if PriorStepString <> StepLine then
                begin
                  frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, StepLine);
                  Inc(AddToDictionaryCount);
                  if (TimeStep = '1') and (StressPeriod <> PriorStressPeriod) then
                  begin
                    AddStressPeriod;
                  end;
                  PriorStressPeriod := StressPeriod;
                  frameListing.AddLine(LineIndex + InnerLineIndex, StepLine, 1);
                  Inc(AddLineCount);
                  Indent := 2;
                  NewTimeStepPostion := frameListing.NodeCount;
                  if NextIndent = 0 then
                  begin
                    NextIndent := 2;
//                    Indent := 1;
//                    Indent := 1;
//                    PriorIndent := NextIndent;
                  end
                  else
                  begin
                    NextIndent := 2;
                  end;
                  PriorSP := StressPeriod;
                  NewTimeStepLines.Add(LineIndex + InnerLineIndex);
                  NewTimeStepPostions.Add(NewTimeStepPostion);
                  FParameterDefined := False;
                  PriorStepString := StepLine;
                end
                else
                begin
                  NextIndent := 2;
                end;
              end
              else
              begin
                NextIndent := 2;
              end;
            end
            else
            begin
              NextIndent := 0;
            end;
//          end
//          else
//          begin
//            NextIndent := 0;
          end;
        end;

        case IndexTypes[0] of
          itNone:
            Assert(False);
          itPackage:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 0);
              Inc(AddLineCount);
              Indent := 1;
              FParameterDefined := False;
            end;
          itObservation:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 0);
              Inc(AddLineCount);
              FParameterDefined := False;
            end;
          itBoundary, itNumberBoundary:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
              Inc(AddLineCount);
              if FParameterDefined then
              begin
                Indent := 1;
                FParameterDefined := False;
              end;
            end;
          itWarning:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameWarning.AddLine(LineIndex + InnerLineIndex, ALine, 0);
              Inc(AddLineCount);
              FParameterDefined := False;
            end;
          itError:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameErrors.AddLine(LineIndex + InnerLineIndex, ALine, 0);
              Inc(AddLineCount);
              FParameterDefined := False;
            end;
          itIndent1:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 1);
              Inc(AddLineCount);
              Indent := 1;
              FParameterDefined := False;
            end;
          itIndent3:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              // This may be an Instance so don't change FParameterDefined
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 2);
              Inc(AddLineCount);
              Indent := 3;
            end;
          itNoIndentChange:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 1);
              Inc(AddLineCount);
              FParameterDefined := False;
            end;
          itStressPeriod:
            begin
              if (IndexTypes[1] in [itStressPeriod, itStressPeriod2]) then
              begin
                AnsiLine := AnsiString(ALine);
                SP_Start := GetSP_Pos(AnsiLine, PeriodSearchTerm);
//                SP_Start := BMPos('STRESS PERIOD', AnsiLine);
//                if SP_Start <= 0 then
//                begin
//                  SP_Start := BMPos('PERIOD', AnsiLine);
//                  PeriodSearchTerm := 'PERIOD';
//                  if SP_Start <= 0 then
//                  begin
//                    SP_Start := BMPos('stress period:', AnsiLine);
//                    PeriodSearchTerm := 'stress period:'
//                  end;
//                end
//                else
//                begin
//                  PeriodSearchTerm := 'STRESS PERIOD'
//                end;
                Assert(SP_Start > 0);
                StressPeriod := ExtractStringBetween(ALine,PeriodSearchTerm, ['', 'TIME', 'STEP', ',', 'IS', '"', ' ']);
                if PriorStressPeriod <> StressPeriod then
                begin
                  AddStressPeriod;
                end
                else
                begin
                  frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
                  Inc(AddToDictionaryCount);
                  frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 2);
                  Inc(AddLineCount);
                  Indent := 2;
                end;
                PriorStressPeriod := StressPeriod;
              end;
              FParameterDefined := False;
            end;
          itStressPeriod2:
            begin
              // do nothing
            end;
          itStartTimeStep:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              NewTimeStepPostion := frameListing.AddLine(
                LineIndex + InnerLineIndex, ALine, 1);
              Inc(AddLineCount);
              Indent := 2;
              NewTimeStepLines.Add(LineIndex + InnerLineIndex);
              NewTimeStepPostions.Add(NewTimeStepPostion);
              FParameterDefined := False;
            end;
          itIteration:
            begin
              if (itIteration2 = IndexTypes[1]) then
              begin
                frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
                Inc(AddToDictionaryCount);
                frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
                Inc(AddLineCount);
              end;
              FParameterDefined := False;
            end;
          itIteration2:
            begin
              // do nothing
            end;
          itArray1:
            begin
              if (itArray2 = IndexTypes[1]) then
              begin
                frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
                Inc(AddToDictionaryCount);
                frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
                Inc(AddLineCount);
              end;
              FParameterDefined := False;
            end;
          itArray2:
            begin
                frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
                Inc(AddToDictionaryCount);
                frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
                Inc(AddLineCount);
            end;
          itArray3:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, Indent);
              Inc(AddLineCount);
              FParameterDefined := False;
            end;
          itEndModel:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 0);
              Inc(AddLineCount);
              FParameterDefined := False;
            end;
          itParameter:
            begin
              frameSorted.AddToDictionary(FoundKey, LineIndex + InnerLineIndex, ALine);
              Inc(AddToDictionaryCount);
              frameListing.AddLine(LineIndex + InnerLineIndex, ALine, 1);
              Inc(AddLineCount);
              FParameterDefined := True;
            end;
          itNonID:
            begin
              Assert(False);
            end;
        else
          Assert(False);
        end;
      end;
      Assert(AddToDictionaryCount = AddLineCount);
    end;
    Inc(LineIndex, SomeLines.Count);
//    if (MultipleLines[Length(MultipleLines)] = #10)
//      and (MultipleLines[Length(MultipleLines)-1] = #13) then
//    begin
//      Inc(LineIndex);
//    end;
  finally
    SomeLines.Free;
  end;
end;

procedure TfrmMain.UpdateTimeSteps(NewTimeStepLines, NewTimeStepPostions:
  TListInt64);
var
  ALine: string;
  SPFound: Boolean;
  TempLines: TStringList;
  StartLine: Int64;
  SP_Start: Integer;
  TS_Start: Integer;
  StepIndex: Integer;
  Local_LineIndex: Integer;
  AnsiLine: AnsiString;
begin
  TempLines := TStringList.Create;
  try
    for StepIndex := 0 to NewTimeStepPostions.Count - 1 do
    begin
      StartLine := NewTimeStepLines[StepIndex];
      SPFound := False;
      repeat
        FListFile.ReadLines(TempLines, StartLine, LinesToRead);
        for Local_LineIndex := 0 to TempLines.Count - 1 do
        begin
          ALine := TempLines[Local_LineIndex];
          AnsiLine := AnsiString(ALine);
          SP_Start := BMPos('STRESS PERIOD', AnsiLine);
          if SP_Start <= 0 then
          begin
            SP_Start := BMPos('PERIOD', AnsiLine);
          end;
          if SP_Start <= 0 then
          begin
            SP_Start := BMPos('stress period:', AnsiLine);
          end;
          if SP_Start > 0 then
          begin
            TS_Start := BMPos('TIME STEP', AnsiLine);
            if TS_Start <= 0 then
            begin
              TS_Start := BMPos('time step', AnsiLine);
            end;
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
              frameSorted.AddToLine(NewTimeStepPostions[StepIndex], ALine);
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

//procedure TfrmMain.WMVScroll(var Message: TWMVScroll);
//var
//  Control: TWinControl;
//  OldHandle: HWND;
//begin
//  inherited;
//  if jreLineNumbers <> nil then
//  begin
//    Control := FindControl(Message.ScrollBar);
//    if Control = jvrchdtLines then
//    begin
//      OldHandle := Message.ScrollBar;
//      try
//        Message.ScrollBar := jreLineNumbers.Handle;
//        with TMessage(Message) do
//        begin
//          Result := Control.Perform(Msg + CN_BASE, WParam, LParam);
//        end;
//      finally
//        Message.ScrollBar := OldHandle;
//      end;
//    end;
//  end;
//end;

//procedure TfrmMain.WndProc(var Message: TMessage);
//begin
//  inherited;
////  if Message.Msg = WM_VSCROLL then
////  begin
////    WMVScroll(TWMVScroll(Message));
////  end;
//end;

procedure TfrmMain.AlternateIndexFile;
var
  LineIndex: Integer;
  Indent: Integer;
  NewTimeStepLines: TListInt64;
  NewTimeStepPostions: TListInt64;
//  SomeLines: TStringList;
  MultipleLines: string;
  LineIndex2: Integer;
begin
  try
    if not TryReopenFile then
    begin
      Exit;
    end;
    FAborting := False;
    sbLines.Enabled := False;
    btnGoTo.Enabled := False;
    btnFind.Enabled := False;
    jvrchdtLines.Enabled := False;
    FListFile.GoToStart;
    frameListing.BeginUpdate;
    frameWarning.BeginUpdate;
    frameErrors.BeginUpdate;
    frameSorted.BeginUpdate;
    NewTimeStepLines := TListInt64.Create;
    NewTimeStepPostions := TListInt64.Create;
  //  SomeLines := TStringList.Create;
    try
      ProgressBar1.Position := 0;
      Indent := 0;
      try
        for LineIndex := 0 to FListFile.LineCount - 1 do
        begin
          if (LineIndex mod 1000) = 0 then
          begin
            ProgressBar1.StepIt;
            Application.ProcessMessages;
          end;
          if FAborting then
          begin
            Exit;
          end;

          if LineIndex mod LinesToRead = 0 then
          begin
            FListFile.ReadLines(MultipleLines, LineIndex, LinesToRead);
            LineIndex2 := LineIndex;
            IndexSomeLines(MultipleLines, Indent, NewTimeStepPostions,
              NewTimeStepLines, LineIndex2);
          end;

        end;
      finally
        pgcIndex.ActivePage := tabIndex;
        tabWarnings.TabVisible := frameWarning.HasContent;
        if tabWarnings.TabVisible then
        begin
          pgcIndex.ActivePage := tabWarnings;
        end;
        tabErrors.TabVisible := frameErrors.HasContent;
        if tabErrors.TabVisible then
        begin
          pgcIndex.ActivePage := tabErrors;
        end;
      end;

      UpdateTimeSteps(NewTimeStepLines, NewTimeStepPostions)

    finally
      frameListing.EndUpdate;
      frameWarning.EndUpdate;
      frameErrors.EndUpdate;
      frameSorted.EndUpdate;
  //    SomeLines.Free;
      NewTimeStepPostions.Free;
      NewTimeStepLines.Free;
      sbLines.Enabled := True;
      btnGoTo.Enabled := True;
      btnFind.Enabled := True;
      jvrchdtLines.Enabled := True;
  //    ShowMessage('Found ' + IntToStr(FoundCount) + sLineBreak
  //      + 'Not Found ' + IntToStr(NotFoundCount));
    end;
  finally
    FListFile.CloseFile;
  end;
end;

procedure TfrmMain.AppEvnt1Idle(Sender: TObject; var Done: Boolean);
var
  AFileDate: TDateTime;
begin
  if FIdling or not FPromptReopen then
  begin
    Exit;
  end;
  FIdling := True;
  try
    if (FListFile <> nil) and FListFile.DateChanged then
    begin
      if FListFile.FileName <> '' then
      begin
        AFileDate := TFile.GetLastWriteTimeUtc(FListFile.FileName);
        if FFileDate <> AFileDate then
        begin
          if (MessageDlg(StrTheFileHasChanged,
            mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          begin
            OpenAFile(FListFile.FileName);
          end
          else
          begin
            FPromptReopen := False;
          end;
        end;
        FFileDate := AFileDate;
      end;
    end;
  finally
    FIdling := False;
  end;
end;

procedure TfrmMain.btnSettingsClick(Sender: TObject);
begin
  frmSettings.GetData;
  frmSettings.ShowModal;
end;

end.
