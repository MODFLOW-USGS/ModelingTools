unit frmMonitorUnit;

{#BACKUP VIEWER1.ICO}
{#BACKUP FacesRed.bmp}
{#BACKUP FacesYellow.bmp}
{#BACKUP FacesGreen.bmp}

interface

{
  1.0.2.0 Fixed bug that caused ModelMonitor to work improperly when the
    decimal separator was not a period in the local language settings.
  1.1.0.0 Added support for MODFLOW-LGR.
  1.2.0.0 Added support for MODFLOW-NWT. Converted to compiling with Delphi XE.
  1.3.0.0 NaN is now flagged as an error.
  1.4.0.0 If the name file or the program does not exist, attempting to
    start the program will be flagged as an error.
  1.5.0.0 Minor changes.
  1.6.0.0 Minor changes.
  1.7.0.0 Added support for MODFLOW-FMP. Converted to compiling with Delphi XE2.
  1.8.0.0 Some more error and warning messages detected.
  1.9.0.0 When run manually, the location of MODFLOW specified by the user will
    be saved and used again the next time ModelMonitor is run manually.
  1.10.0.0 Added disclaimer. Some more error and warning messages detected.
    Converted to compiling with Delphi 10.2.
    Fixed Restart model button.
  1.11.0.0 Added support for MODFLOW 6.
}

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, JvExExtCtrls, JvImage, ImgList,
  JvImageList, TeEngine, Series, TeeProcs, Chart, ComCtrls, Buttons,
  JvToolEdit, AppEvnts, RealListUnit, JvHtControls, JvPageList,
  JvExControls, JvExStdCtrls, JvExMask, Mask, JvExComCtrls,
  JvComCtrls, JvComponentBase, JvCreateProcess, ErrorMessages, SearchTrie,
  JvRichEdit, System.ImageList, System.UITypes;

type
  TStatusChange = (scOK, scWarning, scError, scNone);

  TStringFileStream = class(TFileStream)
  private
    FBuffer: array[0..1000] of AnsiChar;
    TempLine: AnsiString;
    FEOF: boolean;
    function GetEOF: boolean;
  public
    function ReadLn: AnsiString;
    property EOF: boolean read GetEOF;
  end;

  TOnStatusChanged = procedure(Sender: TObject; NewStatus: TStatusChange) of object;

  TProblemType = (ptWarning, ptError);

  TListFileHandler = class(TObject)
  private
    FFileStream: TStringFileStream;
    FErrorMessages: TJvRichEdit;
    FBudgetChart: TChart;
    FserCumulative: TLineSeries;
    FserTimeStep: TLineSeries;
    FPageControl: TJvPageList;
    FListingTabSheet: TjvStandardPage;
    FResultsTabSheet: TjvStandardPage;
    FLabel: TLabel;
    FOnStatusChanged: TOnStatusChanged;
    FPercentRate: TRealList;
    FPercentCumulative: TRealList;
    FStartPlotTime: TDateTime;
    FLineCount: Integer;
    FErrorPositions: TIntegerDynArray;
    FWarningPositions: TIntegerDynArray;
    FListingFile: string;
    FStartTime: TDateTime;
    FShouldAbort: Boolean;
    FTree: TTreeView;
    FParentNode: TTreeNode;
    FListingNode: TTreeNode;
    FResultsNode: TTreeNode;
    FVolBudget: Boolean;
    FSearcher: TSearchTrie<TProblemType>;
    procedure CreateNewTabSheet(out ATabSheet: TjvStandardPage;
      NewCaption: string; out NewNode: TTreeNode);
    procedure CreateLineSeries(AColor: TColor; ATitle: string;
      var ASeries: TLineSeries);
    procedure FindStart(PositionInLine: integer; out SelStart: integer);
    procedure GetColor(StatusChangeIndicator: TStatusChange; Value: Double;
      var AColor: TColor);
    procedure StorePercentDiscrepancy(ALine: string);
    function PlotPercentDiscrepancy : boolean;
//    procedure IndentifyProblem(ALine: string; var IsProblem: Boolean;
//      StatusChangeIndicator: TStatusChange; var Positions: TIntegerDynArray;
//      KeyTerms: TAnsiStringList); overload;
    procedure IndentifyProblem(ALine: string;
      var IsError, IsWarning: Boolean; var PositionInLine, KeyLength: Integer); //overload;
    procedure HandleListFileLine(ALine: string);
//    procedure HandleProblem(IsError: Boolean; AColor: TColor;
//      Positions: TIntegerDynArray; EV: TAnsiStringList); overload;
    procedure HandleProblem(IsProblem: Boolean; AColor: TColor;
      PositionInLine: integer; KeyLength: integer); //overload;
    function GetPageStatus(APage: TjvStandardPage): TStatusChange;
    procedure SetPageStatus(APage: TjvStandardPage;
      const Value: TStatusChange);
    property PageStatus[APage: TjvStandardPage]: TStatusChange
      read GetPageStatus write SetPageStatus;
    function GetConnectedNode(APage: TJvStandardPage): TTreeNode;
  public
    property OnStatusChanged: TOnStatusChanged read FOnStatusChanged
      write FOnStatusChanged;
    constructor Create(AFileName: string; APageControl: TJvPageList;
      ATree: TTreeView; ModelCaption: string; CreateSubTree: boolean);
    Destructor Destroy; override;
    procedure HandleListingFile;
    procedure ReadListingFileLines;
    procedure Abort;
    function Done: boolean;
  end;

  TfrmMonitor = class(TForm)
    jilBigFaces: TJvImageList;
    timerReadOutput: TTimer;
    pnlLeft: TPanel;
    jimageStatus: TJvImage;
    imTabFaces: TImageList;
    AppEvents: TApplicationEvents;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    btnRun: TBitBtn;
    timerStartFromCommandParameters: TTimer;
    lblModelDone: TLabel;
    jvplMain: TJvPageList;
    tabConfiguration: TJvStandardPage;
    lblModelName: TLabel;
    jvfeModelName: TJvFilenameEdit;
    lblNameFile: TLabel;
    jvfeNameFile: TJvFilenameEdit;
    tabMonitor: TJvStandardPage;
    lblMonitor: TLabel;
    tabAbout: TJvStandardPage;
    ImageLogo: TImage;
    reReference: TRichEdit;
    JvHTLabel2: TJvHTLabel;
    lblGoPhast: TLabel;
    lblDeveloperName: TLabel;
    JvHTLabel1: TJvHTLabel;
    lblVersion: TLabel;
    treeNavigation: TJvTreeView;
    jvcpRunModel: TJvCreateProcess;
    btnStopModel: TBitBtn;
    reMonitor: TJvRichEdit;
    memoDisclaimer: TMemo;
    cbModflow6: TCheckBox;
    procedure btnRunClick(Sender: TObject);
    procedure timerReadOutputTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure jvfeModelNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure timerStartFromCommandParametersTimer(Sender: TObject);
    procedure SetPageStatus(APage: TJvStandardPage; Status: TStatusChange);
    procedure treeNavigationClick(Sender: TObject);
    procedure jvcpRunModelRead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
    procedure jvcpRunModelTerminate(Sender: TObject; ExitCode: Cardinal);
    procedure btnStopModelClick(Sender: TObject);
    procedure cbModflow6Click(Sender: TObject);
  private
    FListFilesNames: TStringList;
    FListFileHandlers: TList;
    FErrorPositions: TIntegerDynArray;
    FWarningPositions: TIntegerDynArray;
    FLineCount: Integer;
    FShouldAbort: Boolean;
    FModelFinished: Boolean;
    FStartTime: Extended;
    FReading1: Boolean;
    FReading2: Boolean;
    FAlreadyStarted: Boolean;
    FPercentRate: TRealList;
    FPercentCumulative: TRealList;
    FActivated: Boolean;
    FStartPlotTime: TDateTime;
    FDone: Boolean;
    FPriorSolving: Boolean;
    FPriorLine: string;
    FProblem: Boolean;
    FModflow2015: boolean;
    FSaveIniFile: Boolean;
    procedure GetListFile(AFileName: string; ListFiles: TStringList;
      var ListSuppressed: boolean);
    procedure FindStart(RichEdit: TJvRichEdit; PositionInLine: integer;
      out SelStart: integer);
    procedure CreateFileReaders;
    procedure HandleMonitorFileLine(ALine: string);
    procedure ReadCommandLine;
    function ListFileReadersFinished: boolean;
    function RemoveQuotes(AFileName: string): string;
    procedure ReadModflowLocationFromIniFile;
    procedure GetMf6ListFileNames(MfSimFileName: string; ListFiles: TStringList);
    procedure GetAMf6ListFileName(NameFileName: string; ListFiles: TStringList);
    { Private declarations }
  public
    procedure StatusChanged(Sender: TObject; NewStatus: TStatusChange);
    { Public declarations }
  end;

var
  frmMonitor: TfrmMonitor;

implementation

uses {ShellApi,} JvVersionInfo, contnrs, System.Math,
  IniFileUtilities, System.IniFiles, System.IOUtils, DisclaimerTextUnit;

const
  StrPERCENTDISCREPANCY = 'PERCENT DISCREPANCY =';

resourcestring
  StrMFOuttxt = 'MF_Out.txt';
  StrNormalTermination = 'normal termination of simulation';
  StrFailureToConverge = 'Failure to converge';
  StrStopMonitoringMode = 'Stop monitoring model';
  StrStartModel = 'Restart model';
  StrFIRSTENTRYINNAME = 'FIRST ENTRY IN NAME FILE MUST BE "LIST".';
  StrFAILEDTOMEETSOLVE = 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA';
  StrNoListFileInName = 'No list file in name file.';
  StrScreenOutput = 'Screen output';
  StrVersionS = 'Version: %s';
  StrProgramFailedToTe = 'Screen output' + sLineBreak
    + 'Program failed to terminate normally';
  StrTheModelIsStillR = 'The model is still running. Closing ModelMonitor wi' +
  'll also halt the model. Do you want to close ModelMonitor?';
  StrMfsimnamFile = 'mfsim.nam file';
  StrNameFileOrLGRFil = 'Name file or LGR file';
  StrNoModelHasBeenSp = 'No model has been specified';
  StrSDoesNotExist = '%s does not exist.';
  StrNoNameFileHasBee = 'No name file has been specified';
  StrUnableToOpenS = 'Unable to open %s';
  StrModelD = 'Model %d ';
  StrChildD = 'Child %d ';
  StrAbout = 'About';
  StrListing = 'Listing';
  StrErrorsAndWarnings = 'Errors and warnings in Listing file';
  StrResults = 'Results';
  StrPercentDiscrepancy2 = 'Percent Discrepancy';
  StrCumulative = 'Cumulative';
  StrTimeStep = 'Time Step';
  StrMODFLOW6MayNotBe = 'MODFLOW 6 may not be able to run because of a space' +
  ' character in the name of the folder containing it. Moving it to a direct' +
  'ory with no space characters in its name may fix the problem.';

const
  StrProgramLocation = 'ProgramLocation';
  StrModflowLocation = 'ModflowLocation';

const
  WarningColor = clYellow;

{$R *.dfm}


procedure TfrmMonitor.AppEventsIdle(Sender: TObject;
  var Done: Boolean);
var
  FileReaderIndex: Integer;
  ListHandler: TListFileHandler;
begin
  if FProblem then
  begin
    Exit;
  end;
  CreateFileReaders;
  FReading1 := True;
  try
    for FileReaderIndex := 0 to FListFileHandlers.Count - 1 do
    begin
      ListHandler := FListFileHandlers[FileReaderIndex];
      ListHandler.HandleListingFile;
    end;

    Application.ProcessMessages;
  finally
    FReading1 := False;
  end;
end;

function TfrmMonitor.ListFileReadersFinished: boolean;
var
  Index: Integer;
  FileHandler: TListFileHandler;
begin
  result := True;
  for Index := 0 to FListFileHandlers.Count - 1 do
  begin
    FileHandler := FListFileHandlers[Index];
    result := FileHandler.Done;
    if not result then
    begin
      Exit;
    end;
  end;
end;

procedure TfrmMonitor.btnStopModelClick(Sender: TObject);
begin
  if jvcpRunModel.State <> psReady then
  begin
    jvcpRunModel.Terminate;
    timerReadOutput.Enabled := False;
  end;
end;

procedure TfrmMonitor.cbModflow6Click(Sender: TObject);
begin
  if cbModflow6.Checked then
  begin
    lblNameFile.Caption := StrMfsimnamFile;
  end
  else
  begin
    lblNameFile.Caption := StrNameFileOrLGRFil;
  end;
end;

procedure TfrmMonitor.btnRunClick(Sender: TObject);
var
  CommandLine: string;
  FileDir: string;
  FileName: string;
  Index: Integer;
  {$IFDEF MakeBatchFile}
  ListHandler: TListFileHandler;
  ListSuppressed: Boolean;
  {$ENDIF}
begin
  if timerReadOutput.Enabled then
  begin
    FProblem := False;
    btnRun.Caption := StrStartModel;
    FShouldAbort := True;
    timerReadOutput.Enabled := False;
    btnRun.Glyph := jilBigFaces.Items[3].Bitmap;
    for Index := 0 to FListFileHandlers.Count - 1 do
    begin
      ListHandler := FListFileHandlers[Index];
      ListHandler.Abort;
    end;
  end
  else
  begin
    if not FileExists(jvfeModelName.FileName)
      or not FileExists(jvfeNameFile.FileName) then
    begin
      Beep;
      if not FileExists(jvfeModelName.FileName) then
      begin
        if Trim(jvfeModelName.FileName) = '' then
        begin
          reMonitor.Lines.Add(StrNoModelHasBeenSp);
        end
        else
        begin
          reMonitor.Lines.Add(Format(StrSDoesNotExist, [jvfeModelName.FileName]));
        end;
      end;
      if not FileExists(jvfeNameFile.FileName) then
      begin
        if Trim(jvfeNameFile.FileName) = '' then
        begin
          reMonitor.Lines.Add(StrNoNameFileHasBee);
        end
        else
        begin
          reMonitor.Lines.Add(Format(StrSDoesNotExist, [jvfeNameFile.FileName]));
        end;
      end;
      SetPageStatus(tabMonitor, scError);
      StatusChanged(nil, scError);
      Exit;
    end;

    FActivated := False;
    FModelFinished := False;
    FDone := False;
    btnRun.Glyph := jilBigFaces.Items[4].Bitmap;

    CommandLine := jvfeModelName.FileName;
    if Pos(' ', CommandLine) > 0 then
    begin
      CommandLine := '"' + CommandLine + '"';
    end;
    FileDir := ExtractFileDir(jvfeNameFile.FileName);
    FileName := ExtractFileName(jvfeNameFile.FileName);
    SetCurrentDir(FileDir);
    FileDir := IncludeTrailingPathDelimiter(FileDir);
    if not cbModflow6.Checked then
    begin
      CommandLine := CommandLine + ' ' + FileName;
    end;
    FListFilesNames.Clear;
    FListFileHandlers.Clear;
    ListSuppressed := False;
    FModflow2015 := False;
    if not cbModflow6.Checked then
    begin
      GetListFile(jvfeNameFile.FileName, FListFilesNames, ListSuppressed);
    end
    else
    begin
      GetMf6ListFileNames(jvfeNameFile.FileName, FListFilesNames);
      FModflow2015 := True;
    end;
    if (FListFilesNames.Count = 0) and not ListSuppressed then
    begin
      Beep;
      MessageDlg(StrNoListFileInName, mtError, [mbOK], 0);
      Exit;
    end;

    for Index := 0 to FListFilesNames.Count - 1 do
    begin
      if FileExists(FListFilesNames[Index]) then
      begin
        DeleteFile(FListFilesNames[Index]);
      end;
    end;

    btnRun.Caption := StrStopMonitoringMode;
    jimageStatus.Tag := 0;
    jimageStatus.Picture.Assign(jilBigFaces.Items[0].Bitmap);

    SetPageStatus(tabMonitor, scOK);
    FLineCount := 0;
    FShouldAbort := False;

    reMonitor.Lines.Clear;

    lblMonitor.Caption := StrScreenOutput;
    lblMonitor.Font.Color := clBlack;
    lblMonitor.Font.Style := [];

    treeNavigation.AutoExpand := True;
    timerReadOutput.Enabled := True;
    jvcpRunModel.CommandLine := CommandLine;
    FStartTime := Now;
    btnStopModel.Enabled := True;
    jvcpRunModel.Run;

  end;
end;

procedure TfrmMonitor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  timerReadOutput.Enabled := False;
  AppEvents.OnIdle := nil;
  if jvcpRunModel.State in [psRunning, psWaiting] then
  begin
    Beep;
    if not (MessageDlg(StrTheModelIsStillR, mtWarning, [mbYes, mbNo], 0) = mrYes) then
    begin
      CanClose := False;
    end;
  end;
end;

type
  TCommandLineState = (clsUnknown, clsModelName, clsNameFile, clsModflowVersion);

procedure TfrmMonitor.FormCreate(Sender: TObject);
var
  VerInfo: TJvVersionInfo;
begin

  FReading1 := False;
  FReading2 := False;
  // The ItemHeight property seems to get lost all the time.
  treeNavigation.ItemHeight := 28;

  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';

  FListFilesNames := TStringList.Create;
  FListFileHandlers := TObjectList.Create;
  VerInfo := TJvVersionInfo.Create(Application.ExeName);
  try
    lblVersion.Caption := Format(StrVersionS, [VerInfo.FileVersion]);
  finally
    VerInfo.Free;
  end;

  treeNavigation.Items[0].Data := tabConfiguration;
  treeNavigation.Items[1].Data := tabMonitor;
  treeNavigation.Items[2].Data := tabAbout;

  SetLength(FErrorPositions, ErrorValues.Count);
  SetLength(FWarningPositions, WarningValues.Count);

  jvfeModelNameChange(jvfeModelName);
  jvfeModelNameChange(jvfeNameFile);
  jvplMain.ActivePageIndex := 0;
  FPercentRate := TRealList.Create;
  FPercentCumulative := TRealList.Create;
  FStartPlotTime  := 0;

  reReference.WordWrap := True;

  memoDisclaimer.Lines.Clear;
  memoDisclaimer.Lines.Add(DisclaimerString);
end;

procedure TfrmMonitor.FormDestroy(Sender: TObject);
var
  IniFName: string;
  IniFile: TMemIniFile;
begin
  if FSaveIniFile and TFile.Exists(jvfeModelName.FileName) then
  begin
    IniFName := IniFileName(Handle, Application.ExeName);
    IniFile:= TMemInifile.Create(IniFName);
    try
      IniFile.WriteString(StrProgramLocation, StrModflowLocation,
        jvfeModelName.FileName);
      IniFile.UpdateFile;
    finally
      IniFile.Free;
    end;
  end;
  FPercentCumulative.Free;
  FPercentRate.Free;
  FListFilesNames.Free;
  FListFileHandlers.Free;
end;

procedure TfrmMonitor.FormShow(Sender: TObject);
begin
  timerStartFromCommandParameters.Enabled := True;
end;

procedure TfrmMonitor.jvcpRunModelRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  if FPriorSolving and (S = '') then
  begin
    Exit
  end;
  HandleMonitorFileLine(S);
  FPriorSolving := Pos('Solving', S) > 0;
end;

procedure TfrmMonitor.jvcpRunModelTerminate(Sender: TObject;
  ExitCode: Cardinal);
var
  Dummy: Boolean;
  NormalTermination: Boolean;
  Index: Integer;
begin
  btnStopModel.Enabled := False;
  lblModelDone.Visible := True;

  AppEventsIdle(Sender, Dummy);

  while not FShouldAbort and not ListFileReadersFinished do
  begin
    Application.ProcessMessages;
    Sleep(20);
  end;

  FDone := True;

  NormalTermination := False;
  for Index := 0 to reMonitor.Lines.Count - 1 do
  begin
    if Pos(StrNormalTermination,
      LowerCase(reMonitor.Lines[Index])) > 0 then
    begin
      NormalTermination := True;
      break;
    end;
  end;

  if not FShouldAbort and not NormalTermination then
  begin
    lblMonitor.Caption := StrProgramFailedToTe;
    lblMonitor.Font.Color := clRed;
    lblMonitor.Font.Style := [fsBold];
    SetPageStatus(tabMonitor, scError);
    StatusChanged(nil, scError);
  end;

  lblModelDone.Visible := False;

  btnRun.Caption := StrStartModel;
  btnRun.Glyph := jilBigFaces.Items[3].Bitmap;
  FStartTime := Now;
  FModelFinished := True;

  timerReadOutput.Enabled := False;
end;

procedure TfrmMonitor.jvfeModelNameChange(Sender: TObject);
var
  FileEdit: TJvFilenameEdit;
begin
  FileEdit := Sender as TJvFilenameEdit;
  if FileExists(FileEdit.FileName) then
  begin
    FileEdit.Color := clWindow;
  end
  else
  begin
    FileEdit.Color := clRed;
  end;
end;

procedure TfrmMonitor.FindStart(RichEdit: TJvRichEdit; PositionInLine: integer;
  out SelStart: integer);
var
  LineIndex: Integer;
begin
  SelStart := -1;
  for LineIndex := 0 to RichEdit.Lines.Count - 2 do
  begin
    SelStart := SelStart + Length(RichEdit.Lines[LineIndex]) + 1;
  end;
  SelStart := SelStart + PositionInLine;
end;

function TfrmMonitor.RemoveQuotes(AFileName: string): string;
begin
  result := Trim(AFileName);
  if Length(result) >= 2 then
  begin
    if (result[1] = '''') and  (result[Length(result)] = '''')then
    begin
      result := Copy(result, 2, Length(result)-2);
    end
    else if (result[1] = '"') and  (result[Length(result)] = ':')then
    begin
      result := Copy(result, 2, Length(result)-2);
    end;
  end;
end;

procedure TfrmMonitor.GetListFile(AFileName: string; ListFiles: TStringList;
  var ListSuppressed: boolean);
var
  ALine: string;
  LineList: TStringList;
  NameFile: TStringList;
  Index: Integer;
  NameFileLine: string;
  GridCountLine: string;
  GridCount: Integer;
  GridIndex: Integer;
  GridLineIndex: Integer;
  ListFile: string;
  OutputSuppression: integer;
  UnitNumber: Integer;
  innerIndex: Integer;
begin
  ListFile := '';
  NameFile := TStringList.Create;
  try
    AFileName := RemoveQuotes(AFileName);
    NameFile.LoadFromFile(AFileName);
    for Index := 0 to NameFile.Count - 1 do
    begin
      ALine := UpperCase(Trim(NameFile[Index]));
      if (Length(ALine) = 0) or (ALine[1] = '#') then
      begin
        Continue
      end;
      if (Pos('LGR', ALine) > 0) and (Index < NameFile.Count - 2) then
      begin
        LineList := TStringList.Create;
        try
          LineList.Delimiter := ' ';
          LineList.DelimitedText := Trim(ALine);
          if LineList[0] = 'LGR' then
          begin
            GridCountLine := NameFile[Index+1];
            Assert(Length(GridCountLine) > 0);
            LineList.DelimitedText := Trim(GridCountLine);
            GridCount := StrToInt(LineList[0]);

            NameFileLine := NameFile[Index+2];
            Assert(Length(NameFileLine) > 0);
            LineList.DelimitedText := Trim(NameFileLine);
            GetListFile(LineList[0], ListFiles, ListSuppressed);

            for GridIndex := 1 to GridCount - 1 do
            begin
              GridLineIndex := (GridIndex-1)*10 + 3 + Index + 2;
              Assert(GridLineIndex < NameFile.Count);
              NameFileLine := NameFile[GridLineIndex];
              Assert(Length(NameFileLine) > 0);
              LineList.DelimitedText := Trim(NameFileLine);
              GetListFile(LineList[0], ListFiles, ListSuppressed);
            end;
            Exit;
          end;
        finally
          LineList.Free;
        end;
      end;
      if (Pos('BEGIN MODELS', ALine) > 0) then
      begin
        FModflow2015 := True;
        LineList := TStringList.Create;
        try
          LineList.Delimiter := ' ';
          for innerIndex := Index+1 to NameFile.Count - 1 do
          begin
            ALine := UpperCase(Trim(NameFile[innerIndex]));
            if (Pos('END MODELS', ALine) > 0) then
            begin
              Break;
            end;
            if ALine = '' then
            begin
              continue;
            end;
            if ALine[1] = '#' then
            begin
              continue;
            end;
            LineList.DelimitedText := ALine;
            if (LineList[0] = 'GWF') and (LineList.Count >= 2) then
            begin
              GetListFile(LineList[1], ListFiles, ListSuppressed);
            end;
          end;
        finally
          LineList.Free;
        end;

      end;
      if Pos('LIST', ALine) > 0 then
      begin
        LineList := TStringList.Create;
        try
          LineList.Delimiter := ' ';
          LineList.DelimitedText := Trim(ALine);
          if FModflow2015 then
          begin
            if LineList.Count < 2 then
            begin
              Continue;
            end;
          end
          else
          begin
            if LineList.Count < 3 then
            begin
              Continue;
            end;
          end;
          if LineList[0] <> 'LIST' then
          begin
            Continue;
          end;
          if FModflow2015 then
          begin
            ListFile := LineList[1];
          end
          else
          begin
            ListFile := LineList[2];
            UnitNumber := StrToInt(LineList[1]);
            if UnitNumber < 0 then
            begin
              OutputSuppression := StrToInt(LineList[3]);
              if OutputSuppression = 1 then
              begin
                ListSuppressed := True;
                Continue;
              end;
            end;
          end;
          ListFiles.Add(ListFile);
          break;
        finally
          LineList.Free;
        end;
      end;
    end;
  finally
    NameFile.Free;
  end;
end;

procedure TfrmMonitor.GetAMf6ListFileName(NameFileName: string;
  ListFiles: TStringList);
var
  NameFile: TStringList;
  LineSplitter: TStringList;
  OptionsFound: Boolean;
  Option: string;
  ListFileName: string;
  ListFileFound: Boolean;
  index: Integer;
  ALine: string;
begin
  if NameFileName[1] = '''' then
  begin
    NameFileName := Copy(NameFileName, 2, Length(NameFileName)-2);
  end;
  ListFileFound := False;
  NameFile := TStringList.Create;
  LineSplitter := TStringList.Create;
  try
    OptionsFound := False;
    NameFile.LoadFromFile(NameFileName);
    for index := 0 to NameFile.Count - 1 do
    begin
      ALine := NameFile[index];
      if (ALine = '') or (ALine[1] = '#') then
      begin
        Continue;
      end;
      ALine := StringReplace(Aline, Char(39), Char(34), [rfReplaceAll, rfIgnoreCase]);
      LineSplitter.DelimitedText := ALine;
      if LineSplitter.Count >= 2 then
      begin
        if OptionsFound then
        begin
          if (UpperCase(LineSplitter[0]) = 'END')
            and (UpperCase(LineSplitter[1]) = 'OPTIONS') then
          begin
            Exit;
          end;
          Option := UpperCase(LineSplitter[0]);
          if Option = 'LIST' then
          begin
            ListFileName := ExpandFileName(LineSplitter[1]);
            ListFiles.Add(ListFileName);
            ListFileFound := True;
          end;
        end
        else if (UpperCase(LineSplitter[0]) = 'BEGIN')
          and (UpperCase(LineSplitter[1]) = 'OPTIONS') then
        begin
          OptionsFound := True;
        end;
      end;
    end;
  finally
    NameFile.Free;
    LineSplitter.Free;
    if not ListFileFound then
    begin
      ListFileName := ChangeFileExt(ExpandFileName(NameFileName), '.lst');
      ListFiles.Add(ListFileName);
    end;
  end;
end;

procedure TfrmMonitor.GetMf6ListFileNames(MfSimFileName: string;
  ListFiles: TStringList);
var
  MfSimFile: TStringList;
  index: Integer;
  LineSplitter: TStringList;
  ModelsFound: Boolean;
  mtype: string;
  mfname: string;
  mname: string;
  ALine: string;
begin
  ListFiles.Add(ChangeFileExt(MfSimFileName, '.lst'));
  MfSimFile := TStringList.Create;
  LineSplitter := TStringList.Create;
  try
    ModelsFound := False;
    MfSimFile.LoadFromFile(MfSimFileName);
    for index := 0 to MfSimFile.Count - 1 do
    begin
      ALine := MfSimFile[index];
      if (ALine = '') or (ALine[1] = '#') then
      begin
        Continue;
      end;
      ALine := StringReplace(Aline, Char(39), Char(34), [rfReplaceAll, rfIgnoreCase]);
      LineSplitter.DelimitedText := ALine;
      if (LineSplitter.Count >= 2) then
      begin
        if ModelsFound then
        begin
          if (UpperCase(LineSplitter[0]) = 'END')
            and (UpperCase(LineSplitter[1]) = 'MODELS') then
          begin
            Exit;
          end;
          Assert(LineSplitter.Count >= 3);
          mtype := UpperCase(LineSplitter[0]);
          mfname := LineSplitter[1];
          mname := LineSplitter[2];
          if mtype = 'GWF6' then
          begin
            GetAMf6ListFileName(mfname, ListFiles);
          end;
        end
        else if (UpperCase(LineSplitter[0]) = 'BEGIN')
          and (UpperCase(LineSplitter[1]) = 'MODELS') then
        begin
          ModelsFound := True;
        end;
      end;
    end;
  finally
    MfSimFile.Free;
    LineSplitter.Free;
  end;
end;

procedure TfrmMonitor.CreateFileReaders;
var
  FileHandlerIndex: Integer;
  AListingFile: string;
  FileHandler: TListFileHandler;
  ModelCaption: string;
  ANode: TTreeNode;
begin
  if FProblem then
  begin
    Exit;
  end;

  if FListFileHandlers.Count = FListFilesNames.Count then
  begin
    treeNavigation.AutoExpand := False;
    Exit;
  end;

  if treeNavigation.Items.Count = 0 then
  begin
    Exit;
  end;
  ANode := treeNavigation.Items[treeNavigation.Items.Count-1];
  treeNavigation.Items.Delete(ANode);

  for FileHandlerIndex := FListFileHandlers.Count
    to FListFilesNames.Count - 1 do
  begin
    AListingFile := FListFilesNames[FileHandlerIndex];
    if FileExists(AListingFile) then
    begin
      if FListFilesNames.Count = 1 then
      begin
        ModelCaption := '';
      end
      else
      begin
        if cbModflow6.Checked then
        begin
          if FileHandlerIndex = 0 then
          begin
            ModelCaption := 'mfsim.lst';
          end
          else
          begin
            ModelCaption := Format(StrModelD, [FileHandlerIndex]);
          end;
        end
        else
        begin
          if FileHandlerIndex = 0 then
          begin
            ModelCaption := 'Parent ';
          end
          else
          begin
            ModelCaption := Format(StrChildD, [FileHandlerIndex]);
          end;
        end;
      end;
//      FileHandler := nil;
      try
        FileHandler := TListFileHandler.Create(AListingFile, jvplMain,
          treeNavigation, ModelCaption, FListFilesNames.Count > 1);
      except on EAccessViolation do
        begin
          FProblem := True;
          Beep;
          MessageDlg(Format(StrUnableToOpenS, [AListingFile]), mtError, [mbOK], 0);
          Exit;
        end;
      end;
      FileHandler.OnStatusChanged := StatusChanged;
      FListFileHandlers.Add(FileHandler);
    end
    else
    begin
      break;
    end;
  end;
  ANode := treeNavigation.Items.Add(nil, StrAbout);
  ANode.Data := tabAbout;
  treeNavigation.SetFocus;
end;

procedure TfrmMonitor.HandleMonitorFileLine(ALine: string);
var
  Position: integer;
  SelStart: Integer;
  ErrorIndex: Integer;
  AnErrorMessage: string;
  procedure SetTextColor(const AText: string;
    AStatus: TStatusChange);
  var
    AColor: TColor;
    SelStart: integer;
  begin
    AColor := clBlack;
    case AStatus of
      scWarning:
        begin
          AColor := WarningColor;
        end;
      scError:
        begin
          AColor := clRed;
        end;
      else Assert(False);
    end;

    // For TJvRichEdit;
    FindStart(reMonitor, Position, SelStart);
    reMonitor.SetSelection(SelStart,
      SelStart + Length(AText), True);
    reMonitor.SelAttributes.BackColor := AColor;
    reMonitor.SelAttributes.Color := clWhite;
    reMonitor.SetSelection(SelStart, SelStart, True);
    StatusChanged(nil, scError);
    SetPageStatus(tabMonitor, scError);
  end;
begin
  if (FPriorLine <> '') and (Pos(FPriorLine, ALine) > 0) then
  begin
    reMonitor.Lines[reMonitor.Lines.Count-1] := ALine;
  end
  else
  begin
    reMonitor.Lines.Add(ALine);
  end;
  FPriorLine := ALine;
  if Trim(ALine) = '' then
  begin
    Exit;
  end;
  Position := Pos(StrNormalTermination, LowerCase(ALine));
  if Position > 0 then
  begin
    FindStart(reMonitor, Position, SelStart);
    reMonitor.SelStart := SelStart;
    reMonitor.SelLength := Length(StrNormalTermination);
    reMonitor.SelAttributes.Color := clGreen;
    reMonitor.SelLength := 0;
  end
  else
  begin
    Position := Pos(StrFIRSTENTRYINNAME, ALine);
    if Position > 0 then
    begin
      SetTextColor(StrFIRSTENTRYINNAME, scError);
    end;

    Position := Pos(StrFailureToConverge, ALine);
    if Position > 0 then
    begin
      SetTextColor(StrFailureToConverge, scError);
    end;

    Position := Pos(StrFAILEDTOMEETSOLVE, ALine);
    if Position > 0 then
    begin
      SetTextColor(StrFAILEDTOMEETSOLVE, scError);
    end;

    for ErrorIndex := 0 to ErrorValues.Count - 1 do
    begin
      AnErrorMessage := string(ErrorValues[ErrorIndex]);
      Position := Pos(AnErrorMessage, ALine);
      if Position > 0 then
      begin
        SetTextColor(AnErrorMessage, scError);
      end;
    end;
  end;
end;

procedure TfrmMonitor.ReadCommandLine;
var
  State: TCommandLineState;
  Index: Integer;
  Param: string;
  LowerCaseParam: string;
const
  ModelNameOption = '-m';
  NameFileOption = '-n';
  Mf6Option = '-mv';
begin
  if FAlreadyStarted then
  begin
    Exit;
  end;
  FSaveIniFile := True;
  FAlreadyStarted := True;
  if ParamCount > 0 then
  begin
    State := clsUnknown;
    for Index := 0 to ParamCount - 1 do
    begin
      Param := ParamStr(Index + 1);
      LowerCaseParam := LowerCase(Param);
      if LowerCaseParam = ModelNameOption then
      begin
        State := clsModelName;
        Continue;
      end
      else if LowerCaseParam = NameFileOption then
      begin
        State := clsNameFile;
        Continue;
      end
      else if LowerCaseParam = Mf6Option then
      begin
        State := clsModflowVersion;
        Continue;
      end;
      case State of
        clsUnknown:
          begin
            if FileExists(Param) then
            begin
              jvfeModelName.FileName := Param;
              State := clsNameFile;
              FSaveIniFile := False;
            end;
          end;
        clsModelName:
          begin
            if FileExists(Param) then
            begin
              jvfeModelName.FileName := Param;
              State := clsNameFile;
              FSaveIniFile := False;
            end;
          end;
        clsNameFile:
          begin
            if FileExists(Param) then
            begin
              jvfeNameFile.FileName := Param;
              State := clsModelName;
            end;
          end;
        clsModflowVersion:
          begin
            if Param = '6' then
            begin
              cbModflow6.Checked := True;
            end
            else
            begin
              cbModflow6.Checked := False;
            end;
            State := clsNameFile;
          end;
      else
        Assert(False);
      end;
    end;
    if FileExists(jvfeModelName.FileName) and FileExists(jvfeNameFile.FileName) then
    begin
      btnRunClick(nil);
    end;
  end;
  ReadModflowLocationFromIniFile;
end;

procedure TfrmMonitor.SetPageStatus(APage: TJvStandardPage;
  Status: TStatusChange);
var
  ANode: TTreeNode;
begin
  if treeNavigation.Items.Count > APage.PageIndex then
  begin
    ANode := treeNavigation.Items[APage.PageIndex];
    if Status = scNone then
    begin
      ANode.StateIndex := -1;
    end
    else
    begin
      ANode.StateIndex := Ord(Status)+1;
    end;
  end;
end;

procedure TfrmMonitor.StatusChanged(Sender: TObject; NewStatus: TStatusChange);
begin
  if Ord(NewStatus) > jimageStatus.Tag then
  begin
    jimageStatus.Tag := Ord(NewStatus);
    jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
  end;
end;

procedure TfrmMonitor.ReadModflowLocationFromIniFile;
var
  IniFName: string;
  ModflowLocation: string;
  IniFile: TMemIniFile;
begin
  ModflowLocation := jvfeModelName.FileName;
  if TFile.Exists(ModflowLocation) then
  begin
    Exit;
  end;
  if FSaveIniFile then
  begin
    IniFName := IniFileName(Handle, Application.ExeName);
    IniFile := TMemInifile.Create(IniFName);
    try
      ModflowLocation := IniFile.ReadString(StrProgramLocation, StrModflowLocation, jvfeModelName.FileName);
      if TFile.Exists(ModflowLocation) then
      begin
        jvfeModelName.FileName := ModflowLocation;
      end;
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TfrmMonitor.timerReadOutputTimer(Sender: TObject);
const
  TimeOutTime = 1/24/3600;
var
//  ALine: AnsiString;
//  ATime: TDateTime;
  FileReaderIndex: Integer;
  ListHandler : TListFileHandler;
begin
  if FReading2 or FProblem then
  begin
    Exit;
  end;
  FReading2 := True;
  try
    CreateFileReaders;
    for FileReaderIndex := 0 to FListFileHandlers.Count - 1 do
    begin
      ListHandler := FListFileHandlers[FileReaderIndex];
      ListHandler.ReadListingFileLines;
    end;

    Application.ProcessMessages;
  finally
    FReading2 := False;
  end;
end;

procedure TfrmMonitor.timerStartFromCommandParametersTimer(Sender: TObject);
begin
  timerStartFromCommandParameters.Enabled := False;
  ReadCommandLine;
end;

procedure TfrmMonitor.treeNavigationClick(Sender: TObject);
var
  APage: TJvStandardPage;
begin
  APage := treeNavigation.Selected.Data;
  if APage <> nil then
  begin
    jvplMain.ActivePage := APage;
  end;
end;

{ TStringFileStream }

function TStringFileStream.GetEOF: boolean;
begin
  if TempLine <> '' then
  begin
    result := False;
  end
  else
  begin
    result := FEOF and (Position >= Size-1);
  end;
end;

function TStringFileStream.ReadLn: AnsiString;
var
  EndPos: integer;
begin
  if Position < Size -1 then
  begin
    FEOF := False;
  end;
  EndPos := Pos(sLineBreak, TempLine);
  if EndPos > 0 then
  begin
    FEOF := False;
    result := Copy(TempLine, 1,EndPos-1);
    TempLine := Copy(TempLine,EndPos+2, MAXINT);
  end
  else if not FEOF then
  begin
    if Position < Size -1 then
    begin
      FillChar(FBuffer, SizeOf(FBuffer), 0) ;
      Read(FBuffer, SizeOf(FBuffer)-SizeOf(AnsiChar));
      TempLine := TempLine + FBuffer;
      result := ReadLn;
    end
    else if TempLine <> '' then
    begin
      result := TempLine;
      TempLine := '';
    end
    else
    begin
      FEOF := True;
      result := '';
    end;
  end
  else
  begin
    result := '';
  end;
end;

{ TListFileHandler }

procedure TListFileHandler.Abort;
begin
  FShouldAbort := True;
end;

function TListFileHandler.GetConnectedNode(APage: TJvStandardPage): TTreeNode; 
var
  ANode: TTreeNode;
  Index: Integer;
begin
  result := nil;
  for Index := 0 to FTree.Items.Count - 1 do
  begin
    ANode := FTree.Items[Index];
    if ANode.Data = APage then
    begin
      result := ANode;
      break;
    end;
  end;
end;

constructor TListFileHandler.Create(AFileName: string;
  APageControl: TJvPageList; ATree: TTreeView; ModelCaption: string;
  CreateSubTree: boolean);
var
  index: Integer;
begin
  FSearcher := TSearchTrie<TProblemType>.Create;

  for index := 0 to ErrorValues.Count - 1 do
  begin
    FSearcher.AddKey(PAnsiChar(ErrorValues[index]), ptError);
  end;
  for index := 0 to NumberErrorValues.Count - 1 do
  begin
    FSearcher.AddKey(PAnsiChar(NumberErrorValues[index]), ptError);
  end;
  for index := 0 to WarningValues.Count - 1 do
  begin
    FSearcher.AddKey(PAnsiChar(WarningValues[index]), ptWarning);
  end;


  SetLength(FErrorPositions, ErrorValues.Count);
  SetLength(FWarningPositions, WarningValues.Count);

  FListingFile := AFileName;
  FPageControl := APageControl;
  FTree := ATree;
  Assert(FileExists(FListingFile));

  FPercentRate := TRealList.Create;
  FPercentCumulative := TRealList.Create;

  FFileStream := TStringFileStream.Create(FListingFile,
    fmOpenRead or fmShareDenyNone);

  if CreateSubTree then
  begin
    FParentNode := FTree.Items.Add(nil, ModelCaption);
    FParentNode.StateIndex := 1;
  end
  else
  begin
    FParentNode := nil;
  end;

  CreateNewTabSheet(FListingTabSheet, StrListing, FListingNode);

  FLabel := TLabel.Create(APageControl.Owner);
  FLabel.Parent := FListingTabSheet;
  FLabel.Align := alTop;
  FLabel.Alignment := Classes.taCenter;
  FLabel.Caption := StrErrorsAndWarnings;
  FLabel.Height := 24;

  FErrorMessages := TJvRichEdit.Create(FPageControl.Owner);
  FErrorMessages.Parent := FListingTabSheet;
  FErrorMessages.Align := alClient;
  FErrorMessages.WordWrap := False;
  FErrorMessages.ScrollBars := ssBoth;

  CreateNewTabSheet(FResultsTabSheet, StrResults, FResultsNode);

  FBudgetChart := TChart.Create(FPageControl.Owner);
  FBudgetChart.Parent := FResultsTabSheet;
  FBudgetChart.Align := alClient;
  FBudgetChart.View3D := False;
  FBudgetChart.LeftAxis.Automatic := False;
  FBudgetChart.LeftAxis.Minimum := -1;
  FBudgetChart.LeftAxis.Maximum := 1;
  FBudgetChart.LeftAxis.Title.Caption := StrPercentDiscrepancy2;
  FBudgetChart.Color := clWhite;

  CreateLineSeries($00FF8000, StrCumulative, FserCumulative);
  CreateLineSeries($0080FFFF, StrTimeStep, FserTimeStep);

  FTree.Selected := FResultsNode;
  FPageControl.ActivePage := FResultsTabSheet;
  
end;

destructor TListFileHandler.Destroy;
begin
  FFileStream.Free;
  FPercentRate.Free;
  FPercentCumulative.Free;


  if FListingNode <> nil then
  begin
    FTree.Items.Delete(FListingNode);
  end;
  if FResultsNode <> nil then
  begin
    FTree.Items.Delete(FResultsNode);
  end;
  if FParentNode <> nil then
  begin
    FTree.Items.Delete(FParentNode);
  end;

  FserCumulative.Free;
  FserTimeStep.Free;
  FBudgetChart.Free;
  FLabel.Free;
  FErrorMessages.Free;
  FResultsTabSheet.Free;
  FListingTabSheet.Free;
  FSearcher.Free;
  inherited;
end;

function TListFileHandler.Done: boolean;
begin
  result := FFileStream.EOF;
end;

procedure TListFileHandler.FindStart(PositionInLine: integer;
  out SelStart: integer);
var
  LineIndex: Integer;
begin
  SelStart := -1;
  for LineIndex := 0 to FErrorMessages.Lines.Count - 2 do
  begin
    SelStart := SelStart + Length(FErrorMessages.Lines[LineIndex]) + 1;
  end;
  SelStart := SelStart + PositionInLine;
end;

procedure TListFileHandler.GetColor(StatusChangeIndicator: TStatusChange;
  Value: Double; var AColor: TColor);
begin
  if Abs(Value) >= 1 then
  begin
    AColor := clRed;
    if Assigned(OnStatusChanged) then
    begin
      OnStatusChanged(self, StatusChangeIndicator);
    end;

    if (PageStatus[FResultsTabSheet] = scNone)
       or (PageStatus[FResultsTabSheet] < StatusChangeIndicator) then
    begin
      PageStatus[FResultsTabSheet] := StatusChangeIndicator;
    end;
    FBudgetChart.LeftAxis.Automatic := True;
  end;
end;

function TListFileHandler.GetPageStatus(
  APage: TjvStandardPage): TStatusChange;
var
  ConnectedNode: TTreeNode;
begin
  ConnectedNode := GetConnectedNode(APage);
  if ConnectedNode.StateIndex < 0 then
  begin
    result := scNone
  end
  else
  begin
    result := TStatusChange(ConnectedNode.StateIndex-1);
  end;
end;

procedure TListFileHandler.HandleListFileLine(ALine: string);
var
  IsError: boolean;
  IsWarning: Boolean;
  KeyLength: Integer;
  PositionInLine: Integer;
  Prefix: string;
begin
  Inc(FLineCount);
  StorePercentDiscrepancy(ALine);

  IndentifyProblem(ALine, IsError, IsWarning, PositionInLine, KeyLength);
  if IsError or IsWarning then
  begin
    Prefix := IntToStr(FLineCount) + ': ';
    ALine := Prefix + ALine;
    PositionInLine := PositionInLine + Length(Prefix);
    FErrorMessages.Lines.Add(ALine);
    HandleProblem(IsWarning, WarningColor, PositionInLine, KeyLength);
    HandleProblem(IsError, clRed, PositionInLine, KeyLength);
  end;
end;

procedure TListFileHandler.HandleListingFile;
var
  ALine: AnsiString;
begin
  while (FListingFile <> '')
    and not FFileStream.EOF do
  begin
    if FShouldAbort or (FListingFile = '') then
    begin
      Exit;
    end;
    ALine := FFileStream.ReadLn;
    if (ALine <> '') then
    begin
      HandleListFileLine(string(ALine));
    end;
    Application.ProcessMessages;
    FStartTime := Now;
  end;
  PlotPercentDiscrepancy;
end;

procedure TListFileHandler.HandleProblem(IsProblem: Boolean; AColor: TColor;
  PositionInLine: integer; KeyLength: integer);
var
  SelStart: Integer;
begin
  if IsProblem then
  begin
    FindStart(PositionInLine, SelStart);
    FErrorMessages.SelStart := SelStart;
    FErrorMessages.SelLength := KeyLength;
    FErrorMessages.SelAttributes.BackColor := AColor;
    if AColor = clRed then
    begin
      FErrorMessages.SelAttributes.Color := clWhite;
    end;
    FErrorMessages.SelLength := 0;
  end;
end;

{procedure TListFileHandler.HandleProblem(IsError: Boolean; AColor: TColor;
  Positions: TIntegerDynArray; EV: TAnsiStringList);
var
  SelStart: Integer;
  Index: Integer;
begin
  if IsError then
  begin
    for Index := 0 to EV.Count - 1 do
    begin
      if Positions[Index] > 0 then
      begin
        FindStart(Positions[Index], SelStart);
        FErrorMessages.SelStart := SelStart;
        FErrorMessages.SelLength := Length(EV[Index]);

        FErrorMessages.SelAttributes.BackColor := AColor;
        if AColor = clRed then
        begin
          FErrorMessages.SelAttributes.Color := clWhite;
        end;
        FErrorMessages.SelLength := 0;
      end;
    end;
  end;
end;}

procedure TListFileHandler.IndentifyProblem(ALine: string;
  var IsError, IsWarning: Boolean; var PositionInLine, KeyLength: Integer);
var
  AnisLine: AnsiString;
  Key: PAnsiChar;
  ProblemType: TProblemType;
  StatusChangeIndicator: TStatusChange;
begin
  IsError := False;
  IsWarning := False;
  if Pos('BUDGET ERROR:', ALine) > 0 then
  begin
    Exit;
  end;
  if Pos('NO WARNINGS', ALine) > 0 then
  begin
    Exit;
  end;
  AnisLine := AnsiString(ALine);
  Key := PAnsiChar(AnisLine);
  while Key^ <> AnsiChar(0) do
  begin
    if FSearcher.Find(Key, ProblemType, KeyLength) then
    begin
      PositionInLine := Key - PAnsiChar(AnisLine);
      case ProblemType of
        ptWarning:
          begin
            IsWarning := True;
            StatusChangeIndicator := scWarning;
          end;
        ptError:
          begin
            IsError := True;
            StatusChangeIndicator := scError;
          end;
        else
          begin
            Assert(False);
            StatusChangeIndicator := scError;
          end;
      end;
      if Assigned(OnStatusChanged) then
      begin
        OnStatusChanged(self, StatusChangeIndicator);
      end;
      if (PageStatus[FListingTabSheet] = scNone)
        or (PageStatus[FListingTabSheet] < StatusChangeIndicator) then
      begin
        PageStatus[FListingTabSheet] := StatusChangeIndicator;
      end;
      break;
    end;
    Inc(Key);
  end;
end;

{procedure TListFileHandler.IndentifyProblem(ALine: string;
  var IsProblem: Boolean; StatusChangeIndicator: TStatusChange;
  var Positions: TIntegerDynArray; KeyTerms: TAnsiStringList);
var
  Position: Integer;
  Index: Integer;
begin
  IsProblem := False;
  for Index := 0 to KeyTerms.Count - 1 do
  begin
    Position := Pos(string(KeyTerms[Index]), ALine);
    Positions[Index] := Position;
    if Position > 0 then
    begin
      IsProblem := True;
      if Assigned(OnStatusChanged) then
      begin
        OnStatusChanged(self, StatusChangeIndicator);
      end;
      if (PageStatus[FListingTabSheet] = scNone)
        or (PageStatus[FListingTabSheet] < StatusChangeIndicator) then
      begin
        PageStatus[FListingTabSheet] := StatusChangeIndicator;
      end;
    end;
  end;
end;}

function TListFileHandler.PlotPercentDiscrepancy: boolean;
const
  TimeOutTime = 1/24/3600;
var
  Index: Integer;
  Rate: Double;
  AColor: TColor;
begin
  result := False;
  if {not FDone and} (Now - FStartPlotTime < TimeOutTime) then
  begin
    Exit;
  end;
  result := True;
  FStartPlotTime := Now;
  if (FPercentRate.Count > 0) or (FPercentCumulative.Count > 0) then
  begin
    // Hide the series while it is being updated so
    // that the program doesn't spend too much time
    // trying to redraw the screen.
    FserCumulative.Active := False;
    try
      for Index := 0 to FPercentCumulative.Count - 1 do
      begin
        Rate := FPercentCumulative[Index];
        AColor := clBlue;
        GetColor(scError, Rate, AColor);
        FserCumulative.AddXY(FserCumulative.Count + 1, Rate, '', AColor);
      end;
    finally
      FPercentCumulative.Clear;
      FserCumulative.Active := True;
    end;

    FserTimeStep.Active := False;
    try
      for Index := 0 to FPercentRate.Count - 1 do
      begin
        Rate := FPercentRate[Index];
        AColor := clYellow;
        GetColor(scError, Rate, AColor);
        FserTimeStep.AddXY(FserTimeStep.Count + 1, Rate, '', AColor);
      end;
    finally
      FPercentRate.Clear;
      FserTimeStep.Active := True;
    end;
  end;
end;

procedure TListFileHandler.ReadListingFileLines;
const
  OneSecond = 1/24/3600;
var
  ALine: AnsiString;
  StartTime : TDateTime;
begin
  StartTime := Now;
  repeat
  begin
    if FShouldAbort then
    begin
      Exit;
    end;
    if (FFileStream.Position < FFileStream.Size - 1)
      or not FFileStream.EOF then
    begin
      if (FListingFile = '') then
      begin
        break;
      end;
      ALine := FFileStream.ReadLn;
      if (ALine <> '') or not FFileStream.EOF then
      begin
        HandleListFileLine(string(ALine));
      end;
      if PlotPercentDiscrepancy then
      begin
        // only update the plot once per second so the program doesn't spend
        // too much time trying to redraw the screen.
        Application.ProcessMessages;
        break;
      end;
      Application.ProcessMessages;
    end
    else
    begin
      break;
    end;
  end
  until Now - StartTime > OneSecond;
end;

procedure TListFileHandler.SetPageStatus(APage: TjvStandardPage;
  const Value: TStatusChange);
var
  ConnectedNode: TTreeNode;
begin
  ConnectedNode := GetConnectedNode(APage);
  if Value = scNone then
  begin
    ConnectedNode.StateIndex := -1;
  end
  else
  begin
    ConnectedNode.StateIndex := Ord(Value)+1;
  end;
  if (ConnectedNode.Parent <> nil) then
  begin
    if ConnectedNode.StateIndex = -1 then
    begin
      ConnectedNode.Parent.StateIndex := -1;
    end
    else
    begin
      if ConnectedNode.Parent.StateIndex < ConnectedNode.StateIndex then
      begin
        ConnectedNode.Parent.StateIndex := ConnectedNode.StateIndex;
      end;
    end;
  end;
end;

procedure TListFileHandler.StorePercentDiscrepancy(ALine: string);
var
  Position: Integer;
  TestLine: string;
  Num1: string;
  Num2: string;
  Cum: Double;
  Rate: Double;
begin
  Position := Max(Pos('UNSATURATED ZONE PACKAGE VOLUMETRIC BUDGET', ALine),
    Pos('VOLUMETRIC SWI', ALine));
  if Position > 0 then
  begin
    FVolBudget := False;
  end
  else
  begin
    // "VOLUMETRIC BUDGET" is used in MODFLOW-2005 and earlier.
    // "VOLUME BUDGET" is used in MODFLOW-2015.
    Position := Max(Pos('VOLUMETRIC BUDGET', ALine),
      Pos('VOLUME BUDGET', ALine));
    if Position > 0 then
    begin
      FVolBudget := True;
    end;
  end;

  if FVolBudget then
  begin
    Position := Pos(StrPERCENTDISCREPANCY, ALine);
    if Position > 0 then
    begin
      TestLine := Trim(Copy(ALine, Position + Length(StrPERCENTDISCREPANCY), MAXINT));
      Position := Pos(StrPERCENTDISCREPANCY, TestLine);
      Assert(Position > 0);
      Num1 := Trim(Copy(TestLine, 1, Position - 1));
      Num2 := Trim(Copy(TestLine, Position + Length(StrPERCENTDISCREPANCY), MAXINT));
      Cum := StrToFloatDef(Num1, 200);
      Rate := StrToFloatDef(Num2, 200);

      FPercentRate.Add(Rate);
      FPercentCumulative.Add(Cum);
      FVolBudget := False;
    end;
  end;
end;

procedure TListFileHandler.CreateLineSeries(AColor: TColor; ATitle: string;
  var ASeries: TLineSeries);
begin
  ASeries := TLineSeries.Create(FPageControl.Owner);
  ASeries.ParentChart := FBudgetChart;
  ASeries.SeriesColor := AColor;
  ASeries.Title := ATitle;
  ASeries.Pointer.Style := psRectangle;
  ASeries.Pointer.Visible := True;
end;

procedure TListFileHandler.CreateNewTabSheet(out ATabSheet: TjvStandardPage;
  NewCaption: string; out NewNode: TTreeNode);
var
  NewPageIndex: Integer;
begin
  NewPageIndex := FPageControl.PageCount - 1;
  ATabSheet := TjvStandardPage.Create(FPageControl.Owner);
  ATabSheet.PageList := FPageControl;
  ATabSheet.PageIndex := NewPageIndex;
  ATabSheet.Caption := NewCaption;

  NewNode := FTree.Items.AddChild(FParentNode, NewCaption);
  NewNode.Data := ATabSheet;
  NewNode.StateIndex := 1;
end;

end.
