unit frmAutomateUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, ArgusDataEntry, Grids,
  RbwDataGrid4, JvSpin, Menus, Buttons, ExtCtrls, ComCtrls, ToolWin,
  ImgList, JvComponentBase, JvCreateProcess, IOUtils, RealListUnit, Types,
  AbBase, AbBrowse, AbZBrows, AbZipper, System.ImageList;

type
  TResultsColumns = (rcDirectory, rcShape, rcBasinArea, rcKv, rcKx, rcSy,
    rcBasinDepth, rcPercentDiscCumulative, rcPercentDiscTimeStep, rcMaxDrawDown,
    rcMaxRow, rcMaxDistance, rcFixedDistance);
  TMethod = (mModflow, mAnalytic);

  TRealValueItem = class(TCollectionItem)
  private
    FValue: double;
    procedure SetValue(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Value: double read FValue write SetValue;
  end;

  TRealValueCollection = class(TCollection)
  private
    function GetValue(Index: integer): double;
    procedure SetValue(Index: integer; const Value: double);
    function GetItems(Index: integer): TRealValueItem;
    procedure SetItems(Index: integer; const Value: TRealValueItem);
  public
    function Add: TRealValueItem;
    Constructor Create;
    property Items[Index: integer]: TRealValueItem read GetItems write SetItems;
    property Values[Index: integer]: double read GetValue write SetValue; default;
  end;

  TFileIndexes = record
    KhPosition: integer;
    PointsEnd: Integer;
    PointsStart: Integer;
    KvPosition: Integer;
    SyPosition: Integer;
    DepthPosition: Integer;
    ParamEndTimePosition1: Integer;
    ParamStartTimePosition2: Integer;
    ParamEndTimePosition2: Integer;
    RCH_PARPosition: Integer;
    EndtimePosition1: Integer;
    PeriodLengthPosition1: Integer;
    FirstTimeStepPosition1: Integer;
    TSMultPosition1: Integer;
    EndtimePosition2: Integer;
    PeriodLengthPosition2: Integer;
    FirstTimeStepPosition2: Integer;
    TSMultPosition2: Integer;
    StartTimePos2: Integer;
    EndtimePosition3: Integer;
    PeriodLengthPosition3: Integer;
    FirstTimeStepPosition3: Integer;
    TSMultPosition3: Integer;
    StartTimePos3: Integer;
  end;

  TConstantFileValues = record
    FirstStepLength: double;
    NSTP: Integer;
    ModflowLocation: string;
    ModelMuseLocation: string;
  end;

  TVariableFileValues = record
    BasinArea: Extended;
    ShapeFactor: Extended;
    Kv: Extended;
    Ratio: Integer;
    SpecificYield: Extended;
    BasinDepth: Extended;
    ShapeName: string;
  end;
  TFileValuesArray = array of TVariableFileValues;

  TResults = record
    Kx: double;
    MaxCumulativePercentDiscrepancy: double;
    MaxTimeStepPercentDiscrepancy: double;
    FixedDistanceDrawDowns: TDoubleDynArray;
    MaxDrawdown: double;
    StressPeriodLength: Extended;
//  Highest row number with drawdown < -0.05
    RowForDrawdown005: integer;
//  Maximum distance with drawdown < -0.05
    DistanceForDrawdown005: double;
    RowForDrawdown025: integer;
    DistanceForDrawdown025: double;
    RowForDrawdown100: integer;
    DistanceForDrawdown100: double;
  end;

  THydCell = Class(TObject)
    Name: string;
    Col: integer;
    Row: integer;
    Layer: integer;
    X: double;
    Y: double;
  end;

  TRunModelThread = class(TThread)
  private
    FIndicies: TFileIndexes;
    FConstants: TConstantFileValues;
    FFileValues: TFileValuesArray;
    FModelMuseFile: TStringList;
    FDirectory: string;
    FProgressBar: TProgressBar;
    FModelMuseFileName: string;
    FCreateProcess: TJvCreateProcess;
    FResults: TResults;
    FFixedDistances: TRealList;
    FixedDistanceCells: array of THydCell;
    FCenterX: Extended;
    FCenterY: Extended;
    FFileIndex: Integer;
    FOutputFile: TStreamWriter;
    FLastIndex: Integer;
    FMethod: TMethod;
    procedure ProcessDone(Sender: TObject; ExitCode: DWORD);
    procedure PrepareModelMuseFile;
    procedure RunModelMuse;
    procedure RunModflow;
    procedure ExtractResults;
    procedure SaveResults;
    procedure SaveFiles;
    procedure UpdateProgressBar;
    procedure ExtractFromListingFile;
    procedure GetColRowPositions(var ColPositions: TRealList; var RowPositions: TRealList);
    procedure ReadHydmodInput(RowPositions: TRealList; ColPositions: TRealList; var HydInput: TStringList);
    procedure ExtractedHydmodResults(HydInput: TStringList);
    procedure RunHantush;
  protected
    procedure Execute; override;
  public
    constructor Create(Indicies: TFileIndexes;
      Constants: TConstantFileValues; FileValues: TFileValuesArray;
      FixedDistances: TRealList; OutputFile: TStreamWriter;
      Directory: string; ModelMuseFile: TStringList; ProgressBar: TProgressBar;
      LastIndex: integer; Method: TMethod);
    destructor Destroy; override;
    procedure Terminate;
  end;

  TAutomateFile = class(TComponent)
  private
    FBasinAreas: TRealValueCollection;
    FBasinDepths: TRealValueCollection;
    FKv: TRealValueCollection;
    FSy: TRealValueCollection;
    FFixedDistances: TRealValueCollection;
    function GetBasinAreas: TRealValueCollection;
    function GetBasinDepths: TRealValueCollection;
    function GetKv: TRealValueCollection;
    function GetModelLocation: string;
    function GetModelMuseLocation: string;
    function GetModflowLocation: string;
    function GetRatio: double;
    function GetRunModflow: Boolean;
    function GetSy: TRealValueCollection;
    function GetTimeOut: integer;
    procedure SetBasinAreas(const Value: TRealValueCollection);
    procedure SetBasinDepths(const Value: TRealValueCollection);
    procedure SetKv(const Value: TRealValueCollection);
    procedure SetModelLocation(const Value: string);
    procedure SetModelMuseLocation(const Value: string);
    procedure SetModflowLocation(const Value: string);
    procedure SetRatio(const Value: double);
    procedure SetRunModflow(const Value: Boolean);
    procedure SetSy(const Value: TRealValueCollection);
    procedure SetTimeOut(const Value: integer);
    procedure GetRealValueCollection(var Collection: TRealValueCollection; Grid: TRbwDataGrid4);
    procedure SetRealValueCollection(var Collection: TRealValueCollection;
      const Value: TRealValueCollection; Grid: TRbwDataGrid4);
    procedure UpdateGrid(var Collection: TRealValueCollection; Grid: TRbwDataGrid4);
    function GetFixedDistances: TRealValueCollection;
    procedure SetFixedDistances(const Value: TRealValueCollection);
    function GetNSTP: Integer;
    procedure SetNSTP(const Value: Integer);
    function GetMethod: Integer;
    procedure SetMethod(const Value: Integer);
  public
    procedure UpdateGui;
  published
    property ModflowLocation: string read GetModflowLocation write SetModflowLocation;
    property ModelMuseLocation: string read GetModelMuseLocation write SetModelMuseLocation;
    property ModelLocation: string read GetModelLocation write SetModelLocation;
    property Ratio: double read GetRatio write SetRatio;
    property TimeOut: integer read GetTimeOut write SetTimeOut;
    property RunModflow: Boolean read GetRunModflow write SetRunModflow;
    property BasinAreas: TRealValueCollection read GetBasinAreas write SetBasinAreas;
    property Kv: TRealValueCollection read GetKv write SetKv;
    property Sy: TRealValueCollection read GetSy write SetSy;
    property BasinDepths: TRealValueCollection read GetBasinDepths write SetBasinDepths;
    property FixedDistances: TRealValueCollection read GetFixedDistances write SetFixedDistances;
    property NSTP: Integer read GetNSTP write SetNSTP;
    property Method: Integer read GetMethod write SetMethod;
  end;

  TfrmAutomate = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    pcMain: TPageControl;
    tabRunModels: TTabSheet;
    Label1: TLabel;
    feMODFLOW: TJvFilenameEdit;
    Label2: TLabel;
    feModelMuseApplication: TJvFilenameEdit;
    Label3: TLabel;
    feModelMuseFile: TJvFilenameEdit;
    Label4: TLabel;
    rdeRatio: TRbwDataEntry;
    rdgBasinAreas: TRbwDataGrid4;
    Label9: TLabel;
    seTimeOut: TJvSpinEdit;
    rdgKv: TRbwDataGrid4;
    cbRunModflow: TCheckBox;
    rdgSy: TRbwDataGrid4;
    rdgBasinDepth: TRbwDataGrid4;
    btnRun: TButton;
    btnAbort: TBitBtn;
    memoErrors: TMemo;
    Label10: TLabel;
    tabAnalyzeResults: TTabSheet;
    Panel1: TPanel;
    btnAnalyze: TButton;
    Button2: TButton;
    FillTable: TButton;
    Panel2: TPanel;
    rdgResults: TRbwDataGrid4;
    rdgFixedDistances: TRbwDataGrid4;
    Splitter1: TSplitter;
    ImageList1: TImageList;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    rdeNSTP: TRbwDataEntry;
    Label5: TLabel;
    jvcrtprcs1: TJvCreateProcess;
    pb1: TProgressBar;
    SaveDialogResults: TSaveDialog;
    seProcessors: TJvSpinEdit;
    Label6: TLabel;
    cbRestart: TCheckBox;
    seStart: TJvSpinEdit;
    lblStart: TLabel;
    lblEnd: TLabel;
    seEnd: TJvSpinEdit;
    rgMethod: TRadioGroup;
    procedure btnRunClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rdgResultsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnAnalyzeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnTestThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThreadCount: integer;
    FOutputFile: TStreamWriter;
    FShouldAbort: Boolean;
    FThreadList: TList;
    procedure ThreadTerminate(Sender: TObject);
    { Private declarations }
  public
    procedure PlotResults(Row: integer; FileValues: TVariableFileValues;
      Results: TResults);
    { Public declarations }
  end;

var
  frmAutomate: TfrmAutomate;

implementation

uses
  ReadModflowArrayUnit, Clipbrd, Math, SyncObjs, BMSearch, IntListUnit,
  JclSysInfo, HantushUnit;

resourcestring
  StrPERCENTDISCREPANCY = 'PERCENT DISCREPANCY =';
  StrNoFixedDistancesF = 'No fixed distances for measuring drawdown have bee' +
  'n specified. Do you want to continue?';
  StrThisWillOverwrite = 'This will overwrite your previous results and mode' +
  'l files. Are you sure you want to do this?';
  StrYouMustSpecifiyTh = 'You must specifiy the distances at which the drawd' +
  'own will be calculated when using the analytical model.';

{$R *.dfm}

var
  IndexLock: TCriticalSection;
  FileVarIndex: integer = 0;
  DirectoryLock: TCriticalSection;

function GetFileVarIndex: integer;
begin
  IndexLock.Acquire;
  try
    Result := FileVarIndex;
    Inc(FileVarIndex);
  finally
    IndexLock.Release;
  end;
end;

procedure SetStartingIndex(Value: integer);
begin
  IndexLock.Acquire;
  try
    FileVarIndex := Value;
  finally
    IndexLock.Release;
  end;
end;

function GetTSMULT(const PERLEN: double; var FirstStep: double;
  const NSTP: integer): double;
  function GetPerLen(TSMULT: double): double;
  begin
    if TSMULT = 1 then
    begin
      result := FirstStep*NSTP;
    end
    else
    begin
      result := FirstStep * (Power(TSMULT, NSTP)-1)/(TSMULT-1);
    end;
  end;
var
  LowerTestValue: double;
  HigherTestValue: double;
  TestLength: double;
begin
  if PERLEN/NSTP < FirstStep then
  begin
    FirstStep := PERLEN/NSTP;
    result := 1;
    Exit;
  end;
  LowerTestValue := 1;
  HigherTestValue := 1;
  Assert(GetPerLen(LowerTestValue) < PERLEN);
  repeat
    HigherTestValue := HigherTestValue * 2;
  until GetPerLen(HigherTestValue) >= PERLEN;
  repeat
    result := (LowerTestValue+HigherTestValue)/2;
    TestLength := GetPerLen(result);
    if TestLength > PERLEN then
    begin
      HigherTestValue := result;
    end
    else
    begin
      LowerTestValue := result;
    end;
  until Abs(TestLength-PERLEN)/PERLEN < 1e-6;
end;

procedure TfrmAutomate.btnAbortClick(Sender: TObject);
var
  ThreadIndex: Integer;
  MyThread: TRunModelThread;
begin
  FShouldAbort := True;
  for ThreadIndex := 0 to FThreadList.Count - 1 do
  begin
    MyThread := FThreadList[ThreadIndex];
    MyThread.Terminate;
  end;
end;

procedure TfrmAutomate.btnAnalyzeClick(Sender: TObject);
var
  ModelIndex: Integer;
  Directory: string;
  FileName: string;
  ListFileName: string;
  DrawDownFileName: string;
  ListFile: TStringList;
  LineIndex: Integer;
  FirstPercent: Boolean;
  ALine: string;
  MaxTimeStepDiscrep: Extended;
  MaxCumulativeDiscrep: Extended;
  FFileStream: TFileStream;
  Precision: TModflowPrecision;
  KSTP: Integer;
  KPER: Integer;
  PERTIM: TModflowDouble;
  TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL: Integer;
  NROW: Integer;
  ILAY: Integer;
  AnArray: TModflowDoubleArray;
  ReadArray: Boolean;
  ACol: Integer;
  TotalTime: TModflowDouble;
//  StartPosition, LastFilePosition: Int64;
  ARow: Integer;
  MaxDrawDown: TModflowDouble;
  LayerIndex: Integer;
  AValue: TModflowDouble;
  MaxRow: Integer;
  ModelMuseFile: TStringList;
  RowPositions: TRealList;
  RowPositionStart, RowPositionEnd: integer;
  CenterRow: Integer;
  ColIndex: Integer;
  CenterY: Double;
  Y: Double;
  FixedDistances: TRealList;
  SpecifiedRows: array of integer;
  RowCenters: TRealList;
  FixedDistanceIndex: Integer;
  RowIndex: Integer;
  procedure HandlePercentDiscpencyLine;
  var
    Percent1: Extended;
    Percent2: Extended;
    Position: integer;
  begin
    Position := Pos(StrPERCENTDISCREPANCY, ALine) + Length(StrPERCENTDISCREPANCY);
    Assert(Position > 0);
    ALine := Copy(ALine, Position, MAXINT);
    Position := Pos(StrPERCENTDISCREPANCY, ALine);
    Assert(Position > 0);
    Percent1 := StrToFloat(Trim(Copy(ALine, 1, Position-1)));
    Position := Position + Length(StrPERCENTDISCREPANCY);
    Percent2 := StrToFloat(Trim(Copy(ALine, Position, MAXINT)));
    if FirstPercent then
    begin
      MaxCumulativeDiscrep := Percent1;
      MaxTimeStepDiscrep := Percent2;
      FirstPercent := False;
    end
    else
    begin
      if Abs(Percent1) > Abs(MaxCumulativeDiscrep) then
      begin
        MaxCumulativeDiscrep := Percent1;
      end;
      if Abs(Percent2) > Abs(MaxTimeStepDiscrep) then
      begin
        MaxTimeStepDiscrep := Percent2;
      end;
    end;
  end;
begin
  RowPositions := TRealList.Create;
  FixedDistances := TRealList.Create;

  try
    ModelMuseFile := TStringList.Create;
    try
      ModelMuseFile.LoadFromFile(feModelMuseFile.FileName);
      RowPositionStart := ModelMuseFile.IndexOf('  ModflowGrid.RowPositions = (')+1;
      Assert(RowPositionStart > 1);
      RowPositionEnd := ModelMuseFile.IndexOf('  ModflowPackages.ChdBoundary.IsSelected = False')-1;
      Assert(RowPositionEnd > RowPositionStart);
      RowPositions.Capacity := RowPositionEnd-RowPositionStart+1;
      for LineIndex := RowPositionStart to RowPositionEnd do
      begin
        ALine := Trim(ModelMuseFile[LineIndex]);
        if LineIndex = RowPositionEnd then
        begin
          Assert(ALine[Length(ALine)] = ')');
          ALine := Copy(ALine, 1, Length(ALine)-1);
        end;
        RowPositions.Add(StrToFloat(ALine));
      end;
      CenterRow := (RowPositions.Count div 2) -1;
      CenterY := (RowPositions[CenterRow] + RowPositions[CenterRow+1])/2;
    finally
      ModelMuseFile.Free;
    end;

    FixedDistances.Capacity := rdgFixedDistances.RowCount -1;
    for ModelIndex := 1 to rdgFixedDistances.RowCount - 1 do
    begin
      if TryStrToFloat(rdgFixedDistances.Cells[0,ModelIndex], AValue) then
      begin
        FixedDistances.Add(AValue)
      end;
    end;

    rdgResults.ColCount := Ord(rcFixedDistance) + FixedDistances.Count;
    for ColIndex := 0 to FixedDistances.Count - 1 do
    begin
      rdgResults.Columns[ColIndex+Ord(rcFixedDistance)].AutoAdjustColWidths := True;
      rdgResults.Cells[ColIndex+Ord(rcFixedDistance), 0] := 'Drawdown at = '
        + FloatToStr(FixedDistances[ColIndex]);
    end;

    RowCenters := TRealList.Create;
    try
      RowCenters.Capacity := RowPositions.Count -1;
      for LineIndex := 0 to RowPositions.Count - 2 do
      begin
        RowCenters.Add((RowPositions[LineIndex]+RowPositions[LineIndex+1])/2)
      end;
      SetLength(SpecifiedRows, FixedDistances.Count);
      for LineIndex := 0 to FixedDistances.Count - 1 do
      begin
        AValue := FixedDistances[LineIndex]+ CenterY;
        SpecifiedRows[LineIndex] := RowCenters.IndexOfClosest(AValue)
      end;
    finally
      RowCenters.Free;
    end;

    rdgResults.BeginUpdate;
    try
      for ModelIndex := 1 to rdgResults.RowCount - 1 do
      begin
        Directory := rdgResults.Cells[Ord(rcDirectory), ModelIndex];
        FileName := Directory + ExtractFileName(feModelMuseFile.FileName);

        ListFileName := ChangeFileExt(FileName, '.lst');
        FirstPercent := True;
        if FileExists(ListFileName) then
        begin
          ListFile := TStringList.Create;
          try
            ListFile.LoadFromFile(ListFileName);
            for LineIndex := 0 to ListFile.Count - 1 do
            begin
              ALine := ListFile[LineIndex];
              if Pos(StrPERCENTDISCREPANCY, ALine) > 0 then
              begin
                HandlePercentDiscpencyLine;
              end;
            end;
          finally
            ListFile.Free;
          end;
        end;
        if FirstPercent then
        begin
          rdgResults.Cells[Ord(rcPercentDiscCumulative), ModelIndex] := '';
          rdgResults.Cells[Ord(rcPercentDiscTimeStep), ModelIndex] := '';
        end
        else
        begin
          rdgResults.Cells[Ord(rcPercentDiscCumulative), ModelIndex]
            := FloatToStr(MaxCumulativeDiscrep);
          rdgResults.Cells[Ord(rcPercentDiscTimeStep), ModelIndex]
            := FloatToStr(MaxTimeStepDiscrep);
        end;

        DrawDownFileName := ChangeFileExt(FileName, '.bdn');
        if FileExists(DrawDownFileName) then
        begin
          FFileStream := TFileStream.Create(DrawDownFileName, fmOpenRead or fmShareDenyWrite);
          Precision := CheckArrayPrecision(FFileStream);
          TotalTime := -1;
//          LastFilePosition := 0;
          case Precision of
            mpSingle:
              begin
//                While FFileStream.Position < FFileStream.Size do
//                begin;
//                  ReadArray := False;
//                  StartPosition := FFileStream.Position;
//                  ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER, PERTIM,
//                    TOTIM, DESC, NCOL, NROW, ILAY, AnArray, ReadArray);
//                  if (FFileStream.Position < FFileStream.Size) and (TOTIM > TotalTime) then
//                  begin
//                    LastFilePosition := StartPosition;
//                    TotalTime := TOTIM;
//                  end
//                end;
//                FFileStream.Position := LastFilePosition;
                ReadArray := True;
                ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER, PERTIM,
                    TOTIM, DESC, NCOL, NROW, ILAY, AnArray, ReadArray);
              end;
            mpDouble:
              begin
//                While FFileStream.Position < FFileStream.Size do
//                begin;
//                  ReadArray := False;
//                  StartPosition := FFileStream.Position;
//                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER, PERTIM,
//                    TOTIM, DESC, NCOL, NROW, ILAY, AnArray, ReadArray);
//                  if (FFileStream.Position < FFileStream.Size) and (TOTIM > TotalTime) then
//                  begin
//                    LastFilePosition := StartPosition;
//                    TotalTime := TOTIM;
//                  end
//                end;
//                FFileStream.Position := LastFilePosition;
                ReadArray := True;
                ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER, PERTIM,
                    TOTIM, DESC, NCOL, NROW, ILAY, AnArray, ReadArray);
              end;
            else Assert(False);
          end;
          ACol := NCOL div 2;
          ARow := NROW div 2;
          MaxDrawDown := AnArray[ARow,ACol];

          rdgResults.Cells[Ord(rcMaxDrawDown), ModelIndex]
            := FloatToStr(MaxDrawDown);

          MaxRow := ARow;
          for RowIndex := ARow to Length(AnArray) -1 do
          begin
            AValue := AnArray[RowIndex,ACol];
            if AValue < -0.05 then
            begin
              MaxRow := RowIndex;
            end
            else
            begin
              Break;
            end;
          end;
          rdgResults.Cells[Ord(rcMaxRow), ModelIndex]
            := IntToStr(MaxRow+1);

          if MaxRow+1 < RowPositions.Count then
          begin
            Y := (RowPositions[MaxRow] + RowPositions[MaxRow+1])/2;
          end
          else
          begin
            Y := RowPositions[MaxRow]
          end;
          Y := CenterY - Y;
          rdgResults.Cells[Ord(rcMaxDistance), ModelIndex]
            := FloatToStr(Y);

          for FixedDistanceIndex := 0 to Length(SpecifiedRows) - 1 do
          begin
            RowIndex := SpecifiedRows[FixedDistanceIndex];
            AValue := AnArray[RowIndex,ACol];
            rdgResults.Cells[Ord(rcFixedDistance)+FixedDistanceIndex, ModelIndex]
              := FloatToStr(AValue);
          end;

          Application.ProcessMessages;
        end;

      end;
    finally
      rdgResults.EndUpdate;
    end;
  finally
    RowPositions.Free;
    FixedDistances.Free;
  end;
  Beep;
  ShowMessage('Done with getting results');
end;

procedure FillPointLines(PointLines: TStringList; XOffset,YOffset: Extended);
const
  XCenter = 1999.5;//(1800.204311978139000000 + 2200.351675648263000000)/2;
  YCenter = 1999.5;//(1799.727963983634000000 + 2199.739169803733000000)/2;
begin
  PointLines.Clear;
  PointLines.Add('      Points = <');
  PointLines.Add('        item');
  PointLines.Add('          X = ' + FloatToStr(XCenter-XOffset));
  PointLines.Add('          Y = ' + FloatToStr(YCenter-YOffset));
  PointLines.Add('        end');

  PointLines.Add('        item');
  PointLines.Add('          X = ' + FloatToStr(XCenter+XOffset));
  PointLines.Add('          Y = ' + FloatToStr(YCenter-YOffset));
  PointLines.Add('        end');

  PointLines.Add('        item');
  PointLines.Add('          X = ' + FloatToStr(XCenter+XOffset));
  PointLines.Add('          Y = ' + FloatToStr(YCenter+YOffset));
  PointLines.Add('        end');

  PointLines.Add('        item');
  PointLines.Add('          X = ' + FloatToStr(XCenter-XOffset));
  PointLines.Add('          Y = ' + FloatToStr(YCenter+YOffset));
  PointLines.Add('        end');

  PointLines.Add('        item');
  PointLines.Add('          X = ' + FloatToStr(XCenter-XOffset));
  PointLines.Add('          Y = ' + FloatToStr(YCenter-YOffset));
  PointLines.Add('        end>');
end;

procedure TfrmAutomate.btnRunClick(Sender: TObject);
//const
//  XCenter = (1800.204311978139000000 + 2200.351675648263000000)/2;
//  YCenter = (1799.727963983634000000 + 2199.739169803733000000)/2;
var
  ModelMuseFile: TStringList;
  RootDir: string;
  SimpleFileName: string;
  ShapeIndex: Integer;
//  ShapeName: string;
  AreaIndex: Integer;
  AreaName: string;
  KvIndex: Integer;
  KvName: string;
  RatioIndex: Integer;
  RatioName: string;
  Kx: Extended;
  SpecificYieldIndex: Integer;
  SpecificYieldName: string;
  BasinDepthIndex: Integer;
  BasinDepthName: string;
  FileName: string;
  SaveFile: TStringList;
  SearchTerm: string;
  PositionIndex: Integer;
  ValueLine: string;
  RectangleRatio: Extended;
  YOffset: Extended;
  XOffset: Extended;
  CalculatedArea: Extended;
  PointLines: TStringList;
  PointLineIndex: Integer;
  FileDir: string;
  Success: Boolean;
  NameFile: string;
  Temp: TStringList;
  StartTime: Extended;
  StressPeriodLength: Extended;
  TimeOutTime: Extended;
  RunAllBatFile: TStringList;
  BatFileName: string;
  ResultRowIndex: integer;
  GenerateResults: Boolean;
  EqPos: Integer;
  TSMULT: Double;
  FileIndexes: TFileIndexes;
  FileConstants: TConstantFileValues;
  FileVariables: TVariableFileValues;
begin
  TimeOutTime := 1/24/3600 * seTimeOut.AsInteger;
  if not FileExists(feMODFLOW.FileName) then
  begin
    Beep;
    MessageDlg(feMODFLOW.FileName + ' does not exist.', mtError, [mbOK], 0);
    Exit;
  end;
  if not FileExists(feModelMuseApplication.FileName) then
  begin
    Beep;
    MessageDlg(feModelMuseApplication.FileName + ' does not exist.', mtError, [mbOK], 0);
    Exit;
  end;
  if not FileExists(feModelMuseFile.FileName) then
  begin
    Beep;
    MessageDlg(feModelMuseFile.FileName + ' does not exist.', mtError, [mbOK], 0);
    Exit;
  end;
  FileConstants.NSTP := StrToInt(rdeNSTP.Text);
  FileConstants.ModflowLocation := feMODFLOW.FileName;
  FileConstants.ModelMuseLocation := feModelMuseApplication.FileName;
  GenerateResults := Sender = btnRun;
  FShouldAbort := False;
  btnAbort.Enabled := True;
  ModelMuseFile := TStringList.Create;
  SaveFile := TStringList.Create;
  PointLines := TStringList.Create;
  RunAllBatFile := TStringList.Create;
  try
    Application.ProcessMessages;
    ModelMuseFile.LoadFromFile(feModelMuseFile.FileName);
    SearchTerm := '      ScreenObject.Name = ''Recharge64''';
    PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
    Assert(PositionIndex >= 0);
    FileIndexes.PointsEnd := PositionIndex-1;
    FileIndexes.PointsStart := PositionIndex-21;
    Assert(ModelMuseFile[FileIndexes.PointsEnd] = '        end>');
    Assert(ModelMuseFile[FileIndexes.PointsStart] = '      Points = <');

    FileIndexes.ParamEndTimePosition1 := PositionIndex + 24;
    ValueLine := ModelMuseFile[FileIndexes.ParamEndTimePosition1];
    Assert(Pos('              EndTime = ', ValueLine) > 0);
    FileIndexes.ParamStartTimePosition2 := FileIndexes.ParamEndTimePosition1+4;
    ValueLine := ModelMuseFile[FileIndexes.ParamStartTimePosition2];
    Assert(Pos('              StartTime =', ValueLine) > 0);
    FileIndexes.ParamEndTimePosition2 := FileIndexes.ParamStartTimePosition2+1;
    ValueLine := ModelMuseFile[FileIndexes.ParamEndTimePosition2];
    Assert(Pos('              EndTime =', ValueLine) > 0);

    SearchTerm := '  ModflowSteadyParameters = <';
    PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
    Assert(PositionIndex >= 0);

    FileIndexes.KhPosition := PositionIndex + 4;
    Assert(ModelMuseFile[FileIndexes.KhPosition-2] = '      ParameterName = ''HK_Par1''');
    ValueLine := ModelMuseFile[FileIndexes.KhPosition];
    Assert(Pos('Value = ', ValueLine) > 0);

    FileIndexes.KvPosition := FileIndexes.KhPosition + 8;
    Assert(ModelMuseFile[FileIndexes.KvPosition-2] = '      ParameterName = ''VK_Par1''');
    ValueLine := ModelMuseFile[FileIndexes.KvPosition];
    Assert(Pos('Value = ', ValueLine) > 0);

    FileIndexes.SyPosition := FileIndexes.KvPosition + 8;
    Assert(ModelMuseFile[FileIndexes.SyPosition-2] = '      ParameterName = ''SY_Par1''');
    ValueLine := ModelMuseFile[FileIndexes.SyPosition];
    Assert(Pos('Value = ', ValueLine) > 0);

    SearchTerm := '      ParameterName = ''RCH_Rate''';
    FileIndexes.RCH_PARPosition := ModelMuseFile.IndexOf(SearchTerm)+2;
    Assert(FileIndexes.RCH_PARPosition >= 2);
    ValueLine := ModelMuseFile[FileIndexes.RCH_PARPosition];
    Assert(Pos('Value = ', ValueLine) > 0);

    SearchTerm := '  ModflowStressPeriods = <';
    PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
    Assert(PositionIndex >= 0);
    FileIndexes.EndtimePosition1 := PositionIndex + 3;
    ValueLine := ModelMuseFile[FileIndexes.EndtimePosition1];
    Assert(Pos('EndTime = ', ValueLine) > 0);

    FileIndexes.FirstTimeStepPosition1 := FileIndexes.EndtimePosition1+1;
    ValueLine := ModelMuseFile[FileIndexes.FirstTimeStepPosition1];
    Assert(Pos('MaxLengthOfFirstTimeStep = ', ValueLine) > 0);
    EqPos := Pos('=', ValueLine);
    FileConstants.FirstStepLength := StrToFloat(Trim(Copy(ValueLine,EqPos+1,MAXINT)));

    FileIndexes.PeriodLengthPosition1 := FileIndexes.EndtimePosition1 +2;
    ValueLine := ModelMuseFile[FileIndexes.PeriodLengthPosition1];
    Assert(Pos('PeriodLength = ', ValueLine) > 0);

    FileIndexes.TSMultPosition1 := FileIndexes.PeriodLengthPosition1 +1;
    ValueLine := ModelMuseFile[FileIndexes.TSMultPosition1];
    Assert(Pos('TimeStepMultiplier = ', ValueLine) > 0);


    FileIndexes.EndtimePosition2 := FileIndexes.EndtimePosition1 + 8;
    ValueLine := ModelMuseFile[FileIndexes.EndtimePosition2];
    Assert(Pos('EndTime = ', ValueLine) > 0);

    FileIndexes.FirstTimeStepPosition2 := FileIndexes.EndtimePosition2+1;
    ValueLine := ModelMuseFile[FileIndexes.FirstTimeStepPosition2];
    Assert(Pos('MaxLengthOfFirstTimeStep = ', ValueLine) > 0);

    FileIndexes.PeriodLengthPosition2 := FileIndexes.EndtimePosition2 +2;
    ValueLine := ModelMuseFile[FileIndexes.PeriodLengthPosition2];
    Assert(Pos('PeriodLength = ', ValueLine) > 0);

    FileIndexes.TSMultPosition2 := FileIndexes.PeriodLengthPosition2 +1;
    ValueLine := ModelMuseFile[FileIndexes.TSMultPosition2];
    Assert(Pos('TimeStepMultiplier = ', ValueLine) > 0);


    FileIndexes.EndtimePosition3 := FileIndexes.EndtimePosition2 + 8;
    ValueLine := ModelMuseFile[FileIndexes.EndtimePosition3];
    Assert(Pos('EndTime = ', ValueLine) > 0);

    FileIndexes.FirstTimeStepPosition3 := FileIndexes.EndtimePosition3+1;
    ValueLine := ModelMuseFile[FileIndexes.FirstTimeStepPosition3];
    Assert(Pos('MaxLengthOfFirstTimeStep = ', ValueLine) > 0);

    FileIndexes.PeriodLengthPosition3 := FileIndexes.EndtimePosition3 +2;
    ValueLine := ModelMuseFile[FileIndexes.PeriodLengthPosition3];
    Assert(Pos('PeriodLength = ', ValueLine) > 0);

    FileIndexes.TSMultPosition3 := FileIndexes.PeriodLengthPosition3 +1;
    ValueLine := ModelMuseFile[FileIndexes.TSMultPosition3];
    Assert(Pos('TimeStepMultiplier = ', ValueLine) > 0);



    SearchTerm := '      Variable.Name = ''BasinDepth''';
    PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
    Assert(PositionIndex >= 0);
    FileIndexes.DepthPosition := PositionIndex +2;
    ValueLine := ModelMuseFile[FileIndexes.DepthPosition];
    Assert(Pos('Variable.RealValue =', ValueLine) > 0);

    RectangleRatio := StrToFloat(rdeRatio.Text);

    RootDir := IncludeTrailingPathDelimiter(
      ExtractFileDir(feModelMuseFile.FileName));

    BatFileName := RootDir + 'RunAll.bat';

    rdgResults.BeginUpdate;
    SimpleFileName := ExtractFileName(feModelMuseFile.FileName);
    try
      ResultRowIndex := 0;
      for ShapeIndex := 0 to 1 do
      begin
        if ShapeIndex = 0 then
        begin
          FileVariables.ShapeName := 'Square\';
          FileVariables.ShapeFactor := 1.;
        end
        else
        begin
          FileVariables.ShapeName := 'Rectangle\';
          FileVariables.ShapeFactor := RectangleRatio;
        end;
        for AreaIndex := 1 to rdgBasinAreas.RowCount - 1 do
        begin
          if TryStrToFloat(rdgBasinAreas.Cells[0,AreaIndex], FileVariables.BasinArea) then
          begin
            YOffset := Sqrt(FileVariables.BasinArea/FileVariables.ShapeFactor)/2;
            XOffset := YOffset*FileVariables.ShapeFactor;
            CalculatedArea := ((YOffset*2)*(XOffset*2));
            Assert((CalculatedArea-FileVariables.BasinArea)/FileVariables.BasinArea < 1e-6);

            FillPointLines(PointLines, XOffset,YOffset);

            AreaName := 'Area' + IntToStr(AreaIndex) + '\';
            for KvIndex := 1 to rdgKv.RowCount - 1 do
            begin
              if TryStrToFloat(rdgKv.Cells[0,KvIndex], FileVariables.Kv) then
              begin
                KvName := 'Kv' + IntToStr(KvIndex) + '\';
                for RatioIndex := 0 to 1 do
                begin
                  if RatioIndex = 0 then
                  begin
                    RatioName := 'Ratio1\';
                    FileVariables.Ratio := 10;
                  end
                  else
                  begin
                    RatioName := 'Ratio2\';
                    FileVariables.Ratio := 1;
                  end;
                  Kx := FileVariables.Ratio*FileVariables.Kv;
                  for SpecificYieldIndex := 1 to rdgSy.RowCount - 1 do
                  begin
                    if TryStrToFloat(rdgSy.Cells[0,SpecificYieldIndex], FileVariables.SpecificYield) then
                    begin
                      SpecificYieldName := 'Sy' + IntToStr(SpecificYieldIndex) + '\';
                      for BasinDepthIndex := 0 to rdgBasinDepth.RowCount - 1 do
                      begin
                        if TryStrToFloat(rdgBasinDepth.Cells[0,BasinDepthIndex], FileVariables.BasinDepth) then
                        begin
                          Application.ProcessMessages;
                          if FShouldAbort then
                          begin
                            Exit;
                          end;

                          BasinDepthName := 'BasinDepth' + IntToStr(BasinDepthIndex) + '\';
                          SaveFile.Assign(ModelMuseFile);

                          Assert(FileIndexes.PointsStart + PointLines.Count-1 = FileIndexes.PointsEnd);
                          for PointLineIndex := 0 to PointLines.Count - 1 do
                          begin
                            SaveFile[PointLineIndex+FileIndexes.PointsStart] :=
                              PointLines[PointLineIndex];
                          end;

                          StressPeriodLength := FileVariables.BasinDepth/FileVariables.Kv;
                          TSMULT := GetTSMULT(StressPeriodLength, FileConstants.FirstStepLength, FileConstants.NSTP);

                          SaveFile[FileIndexes.KhPosition] := '      Value = ' + FloatToStr(Kx);
                          SaveFile[FileIndexes.KvPosition] := '      Value = ' + FloatToStr(FileVariables.Kv);
                          SaveFile[FileIndexes.RCH_PARPosition] := '      Value = ' + FloatToStr(FileVariables.Kv);
                          SaveFile[FileIndexes.SyPosition] := '      Value = ' + FloatToStr(FileVariables.SpecificYield);

                          SaveFile[FileIndexes.EndtimePosition1] := '      EndTime = ' + FloatToStr(StressPeriodLength);
                          SaveFile[FileIndexes.PeriodLengthPosition1] := '      PeriodLength = ' + FloatToStr(StressPeriodLength);
                          SaveFile[FileIndexes.ParamEndTimePosition1] := '              EndTime = ' + FloatToStr(StressPeriodLength);
                          SaveFile[FileIndexes.TSMultPosition1] := '      TimeStepMultiplier = ' + FloatToStr(TSMULT);


                          SaveFile[FileIndexes.DepthPosition] := '      Variable.RealValue = ' + FloatToStr(-FileVariables.BasinDepth);

                          FileDir := RootDir + FileVariables.ShapeName + AreaName + KvName
                            + RatioName + SpecificYieldName + BasinDepthName;
                          if not DirectoryExists(FileDir) then
                          begin
                            ForceDirectories(FileDir)
                          end;
                          FileName := FileDir + SimpleFileName;
                          SaveFile.SaveToFile(FileName);

                          Inc(ResultRowIndex);
                          if ResultRowIndex >= rdgResults.RowCount -1 then
                          begin
                            rdgResults.RowCount := ResultRowIndex + 1;
                          end;
                          rdgResults.Cells[Ord(rcDirectory), ResultRowIndex] := FileDir;
                          rdgResults.Cells[Ord(rcShape), ResultRowIndex] := FileVariables.ShapeName;
                          rdgResults.Cells[Ord(rcBasinArea), ResultRowIndex] := FloatToStr(FileVariables.BasinArea);
                          rdgResults.Cells[Ord(rcKv), ResultRowIndex] := FloatToStr(FileVariables.Kv);
                          rdgResults.Cells[Ord(rcKx), ResultRowIndex] := FloatToStr(Kx);
                          rdgResults.Cells[Ord(rcSy), ResultRowIndex] := FloatToStr(FileVariables.SpecificYield);
                          rdgResults.Cells[Ord(rcBasinDepth), ResultRowIndex] := FloatToStr(FileVariables.BasinDepth);

                          if GenerateResults then
                          begin
                            NameFile := ChangeFileExt(FileName, '.nam');
                            DeleteFile(NameFile);

                            jvcrtprcs1.CommandLine := '"' + feModelMuseApplication.FileName + '" "' + FileName + '" -E -C';
                            jvcrtprcs1.Run;
//                            ActivApp1.ExePath := '"' + feModelMuseApplication.FileName + '" "' + FileName + '" -E -C';
//                            ActivApp1.ExecuteApp(Success);
//                            if not Success then
//                            begin
//                              MessageDlg('Failed to execute "' + FileName + '".', mtError, [mbOK], 0);
//                              Exit;
//                            end;

                            StartTime := Now;
                            while not FileExists(NameFile) do
                            begin
                              Sleep(1000);
                              Application.ProcessMessages;
                              if FShouldAbort then
                              begin
                                Exit;
                              end;
                              if Now - StartTime > TimeOutTime then
                              begin
                                memoErrors.Lines.Add('Timed out waiting for ' + FileName);
                              end;
                            end;
                            RunAllBatFile.Add('cd "' + FileDir + '"');
                            RunAllBatFile.Add('"' + feMODFLOW.FileName + '" ' + ExtractFileName(NameFile));
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      rdgResults.EndUpdate;
      if RunAllBatFile.Count > 0 then
      begin
        RunAllBatFile.Add('pause');
        RunAllBatFile.SaveToFile(BatFileName);
        if cbRunModflow.Checked then
        begin
          jvcrtprcs1.CommandLine := BatFileName;
          jvcrtprcs1.Run;
//          ActivApp1.ExePath := BatFileName;
//          ActivApp1.ExecuteApp(Success);
//          if not Success then
//          begin
//            MessageDlg('Failed to execute "' + BatFileName + '".', mtError, [mbOK], 0);
//          end;
        end;
      end;
    end;
  finally
    ModelMuseFile.Free;
    SaveFile.Free;
    PointLines.Free;
    RunAllBatFile.Free;
    btnAbort.Enabled := False;
  end;
end;

// modigfied from
// http://coding.derkeiler.com/Archive/Delphi/alt.comp.lang.borland-delphi/2009-01/msg00045.html
function AvailableProcessorCount: DWORD;
//returns total number of processors available to system including
//logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask {, SystemAffinityMask}: DWORD;
  Mask: DWORD;
  SysInfo: TSystemInfo;
  PProcessAffinityMask, PSystemAffinityMask: DWORD_PTR;
begin
  if GetProcessAffinityMask(GetCurrentProcess, PProcessAffinityMask,
    PSystemAffinityMask) then
  begin
    ProcessAffinityMask := PProcessAffinityMask;
    Result := 0;
    for i := 0 to 31 do
    begin
      Mask := 1 shl i;
      if (ProcessAffinityMask and Mask)<>0 then
      begin
        inc(Result);
      end;
    end;
  end
  else
  begin
    //can't get the affinity mask so we just report the total number of
    //processors
    GetSystemInfo(SysInfo);
    Result := SysInfo.dwNumberOfProcessors;
  end;
end; (* AvailableProcessorCount *)

procedure TfrmAutomate.btnTestThreadClick(Sender: TObject);
var
  FileIndexes: TFileIndexes;
  FileConstants: TConstantFileValues;
  FileVariables: TVariableFileValues;
  FileValuesArray: TFileValuesArray;
  ModelMuseFile: TStringList;
  SearchTerm: string;
  PositionIndex: Integer;
  ValueLine: string;
  RectangleRatio: Extended;
//  RootDir: string;
  EqPos: Integer;
  ResultRowIndex: Integer;
//  SimpleFileName: string;
  ShapeIndex: integer;
  AreaIndex: integer;
  AreaName: string;
  KvIndex: integer;
  KvName: string;
  RatioIndex: integer;
  RatioName: string;
  SpecificYieldIndex: integer;
  SpecificYieldName: string;
  BasinDepthIndex: integer;
  MyThread: TRunModelThread;
  FixedDistances: TRealList;
  ModelIndex: Integer;
  AValue: double;
  AreaList: TRealList;
  KvList: TRealList;
  SyList: TRealList;
  BasinDepthList: TRealList;
  ModelCount: Integer;
  ThreadIndex: Integer;
  Dir: string;
  Index: TObject;
  StartIndex: integer;
  ModelFileName: string;
  RowIndex: integer;
  FirstIndex: integer;
  LastIndex: integer;
  AskedUser: boolean;
  TestIndex: integer;
begin

  if rgMethod.ItemIndex = 0 then
  begin
    if not FileExists(feMODFLOW.FileName) then
    begin
      Beep;
      MessageDlg(feMODFLOW.FileName + ' does not exist.', mtError, [mbOK], 0);
      Exit;
    end;
    if not FileExists(feModelMuseApplication.FileName) then
    begin
      Beep;
      MessageDlg(feModelMuseApplication.FileName + ' does not exist.', mtError, [mbOK], 0);
      Exit;
    end;
  end;
  if not FileExists(feModelMuseFile.FileName) then
  begin
    Beep;
    MessageDlg(feModelMuseFile.FileName + ' does not exist.', mtError, [mbOK], 0);
    Exit;
  end;
  AreaList := TRealList.Create;
  KvList := TRealList.Create;
  SyList := TRealList.Create;
  BasinDepthList := TRealList.Create;
  try
    AreaList.Capacity := rdgBasinAreas.RowCount - 1;
    for AreaIndex := 1 to rdgBasinAreas.RowCount - 1 do
    begin
      if TryStrToFloat(rdgBasinAreas.Cells[0,AreaIndex], AValue) then
      begin
        AreaList.Add(AValue);
      end;
    end;

    KvList.Capacity := rdgKv.RowCount - 1;
    for KvIndex := 1 to rdgKv.RowCount - 1 do
    begin
      if TryStrToFloat(rdgKv.Cells[0,KvIndex], AValue) then
      begin
        KvList.Add(AValue);
      end;
    end;

    SyList.Capacity := rdgSy.RowCount - 1;
    for SpecificYieldIndex := 1 to rdgSy.RowCount - 1 do
    begin
      if TryStrToFloat(rdgSy.Cells[0,SpecificYieldIndex], AValue) then
      begin
        SyList.Add(AValue);
      end;
    end;

    BasinDepthList.Capacity := rdgBasinDepth.RowCount - 1;
    for BasinDepthIndex := 1 to rdgBasinDepth.RowCount - 1 do
    begin
      if TryStrToFloat(rdgBasinDepth.Cells[0,BasinDepthIndex], AValue) then
      begin
        BasinDepthList.Add(AValue);
      end;
    end;

    ModelCount := 4*BasinDepthList.Count*SyList.Count*KvList.Count*AreaList.Count;
    if ModelCount = 0 then
    begin
      Beep;
      MessageDlg('You must speciy at least one value for Area, Kv, specific yield, and basin depth', mtError, [mbOK], 0);
      Exit;
    end;

    if SaveDialogResults.Execute then
    begin
      if FileExists(SaveDialogResults.FileName) and (not cbRestart.Checked) then
      begin
        Beep;
        if (MessageDlg(StrThisWillOverwrite, mtWarning, [mbYes, mbNo, mbCancel], 0) in [mrNo, mrCancel, mrNone]) then
        begin
          Exit;
        end;
      end;
      btnAbort.Enabled := True;
      FreeAndNil(FOutputFile);
      if not cbRestart.Checked then
      begin
        if TFile.Exists(SaveDialogResults.FileName) then
        begin
          TFile.Delete(SaveDialogResults.FileName);
        end;
      end;
      FOutputFile := TFile.AppendText(SaveDialogResults.FileName);

      try
        SetLength(FileValuesArray, ModelCount);

        FileConstants.NSTP := StrToInt(rdeNSTP.Text);
        FileConstants.ModflowLocation := feMODFLOW.FileName;
        FileConstants.ModelMuseLocation := feModelMuseApplication.FileName;
        ModelMuseFile := TStringList.Create;
        try
          FixedDistances := TRealList.Create;

          try
            FixedDistances.Capacity := rdgFixedDistances.RowCount -1;
            for ModelIndex := 1 to rdgFixedDistances.RowCount - 1 do
            begin
              if TryStrToFloat(rdgFixedDistances.Cells[0,ModelIndex], AValue) then
              begin
                FixedDistances.Add(AValue)
              end;
            end;

            if FixedDistances.Count = 0 then
            begin
              Beep;
              if rgMethod.ItemIndex = 0 then
              begin
                if (MessageDlg(StrNoFixedDistancesF, mtWarning,
                  [mbYes, mbNo], 0, mbNo) <> mrYes) then
                begin
                  btnAbort.Enabled := False;
                  Exit;
                end;
              end
              else
              begin
                MessageDlg(StrYouMustSpecifiyTh, mtError, [mbOK], 0);
                Exit;
              end;
            end;

            if not cbRestart.Checked then
            begin
              FOutputFile.Write('"Model Number"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Shape"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Basin Area"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Kv"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Kx"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Sy"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Basin Depth"');
              FOutputFile.Write(#9);

              FOutputFile.Write('"Length of first stress period"');
              FOutputFile.Write(#9);

              FOutputFile.Write('"Max cummulative percent discrepancy"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Max percent discrepancy for a time step"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Highest row number with drawdown < -0.05"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Maximum distance with drawdown < -0.05"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Highest row number with drawdown < -0.25"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Maximum distance with drawdown < -0.25"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Highest row number with drawdown < -1.00"');
              FOutputFile.Write(#9);
              FOutputFile.Write('"Maximum distance with drawdown < -1.00"');
              FOutputFile.Write(#9);

              for ModelIndex := 0 to FixedDistances.Count - 1 do
              begin
                FOutputFile.Write('"Drawdown at '
                  + FloatToStr(FixedDistances[ModelIndex]) + '"');
                FOutputFile.Write(#9);
              end;
              FOutputFile.WriteLine;
            end;

            RectangleRatio := StrToFloat(rdeRatio.Text);


//            if rgMethod.ItemIndex = 0 then
            begin

              ModelMuseFile.LoadFromFile(feModelMuseFile.FileName);
              SearchTerm := '      ScreenObject.Name = ''Recharge64''';
              PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
              Assert(PositionIndex >= 0);
              FileIndexes.PointsEnd := PositionIndex-1;
              FileIndexes.PointsStart := PositionIndex-21;
              Assert(ModelMuseFile[FileIndexes.PointsEnd] = '        end>');
              Assert(ModelMuseFile[FileIndexes.PointsStart] = '      Points = <');

              FileIndexes.ParamEndTimePosition1 := -1;
              for TestIndex := PositionIndex+1 to ModelMuseFile.Count - 1 do
              begin
                ValueLine := ModelMuseFile[TestIndex];
                if Pos('              EndTime = ', ValueLine) > 0 then
                begin
                  FileIndexes.ParamEndTimePosition1 := TestIndex;
                  break;
                end;
              end;
              Assert(FileIndexes.ParamEndTimePosition1 >= 0);
              Assert(FileIndexes.ParamEndTimePosition1 - PositionIndex <= 30);

              ValueLine := ModelMuseFile[FileIndexes.ParamEndTimePosition1];
              Assert(Pos('              EndTime = ', ValueLine) > 0);
              FileIndexes.ParamStartTimePosition2 := FileIndexes.ParamEndTimePosition1+4;
              ValueLine := ModelMuseFile[FileIndexes.ParamStartTimePosition2];
              Assert(Pos('              StartTime =', ValueLine) > 0);
              FileIndexes.ParamEndTimePosition2 := FileIndexes.ParamStartTimePosition2+1;
              ValueLine := ModelMuseFile[FileIndexes.ParamEndTimePosition2];
              Assert(Pos('              EndTime =', ValueLine) > 0);

              SearchTerm := '  ModflowSteadyParameters = <';
              PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
              Assert(PositionIndex >= 0);

              FileIndexes.KhPosition := PositionIndex + 4;
              Assert(ModelMuseFile[FileIndexes.KhPosition-2] = '      ParameterName = ''HK_Par1''');
              ValueLine := ModelMuseFile[FileIndexes.KhPosition];
              Assert(Pos('Value = ', ValueLine) > 0);

              FileIndexes.KvPosition := FileIndexes.KhPosition + 8;
              Assert(ModelMuseFile[FileIndexes.KvPosition-2] = '      ParameterName = ''VK_Par1''');
              ValueLine := ModelMuseFile[FileIndexes.KvPosition];
              Assert(Pos('Value = ', ValueLine) > 0);

              FileIndexes.SyPosition := FileIndexes.KvPosition + 8;
              Assert(ModelMuseFile[FileIndexes.SyPosition-2] = '      ParameterName = ''SY_Par1''');
              ValueLine := ModelMuseFile[FileIndexes.SyPosition];
              Assert(Pos('Value = ', ValueLine) > 0);

              SearchTerm := '      ParameterName = ''RCH_Rate''';
              FileIndexes.RCH_PARPosition := ModelMuseFile.IndexOf(SearchTerm)+2;
              Assert(FileIndexes.RCH_PARPosition >= 0);
              ValueLine := ModelMuseFile[FileIndexes.RCH_PARPosition];
              Assert(Pos('Value = ', ValueLine) > 0);

              SearchTerm := '  ModflowStressPeriods = <';
              PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
              Assert(PositionIndex >= 0);
              FileIndexes.EndtimePosition1 := PositionIndex + 3;
              ValueLine := ModelMuseFile[FileIndexes.EndtimePosition1];
              Assert(Pos('EndTime = ', ValueLine) > 0);

              FileIndexes.FirstTimeStepPosition1 := FileIndexes.EndtimePosition1+1;
              ValueLine := ModelMuseFile[FileIndexes.FirstTimeStepPosition1];
              Assert(Pos('MaxLengthOfFirstTimeStep = ', ValueLine) > 0);
              EqPos := Pos('=', ValueLine);
              FileConstants.FirstStepLength := StrToFloat(Trim(Copy(ValueLine,EqPos+1,MAXINT)));

              FileIndexes.PeriodLengthPosition1 := FileIndexes.EndtimePosition1 +2;
              ValueLine := ModelMuseFile[FileIndexes.PeriodLengthPosition1];
              Assert(Pos('PeriodLength = ', ValueLine) > 0);

              FileIndexes.TSMultPosition1 := FileIndexes.PeriodLengthPosition1 +2;
              ValueLine := ModelMuseFile[FileIndexes.TSMultPosition1];
              Assert(Pos('TimeStepMultiplier = ', ValueLine) > 0);


              FileIndexes.EndtimePosition2 := FileIndexes.EndtimePosition1 + 8;
              ValueLine := ModelMuseFile[FileIndexes.EndtimePosition2];
              Assert(Pos('EndTime = ', ValueLine) > 0);

              FileIndexes.FirstTimeStepPosition2 := FileIndexes.EndtimePosition2+1;
              ValueLine := ModelMuseFile[FileIndexes.FirstTimeStepPosition2];
              Assert(Pos('MaxLengthOfFirstTimeStep = ', ValueLine) > 0);

              FileIndexes.PeriodLengthPosition2 := FileIndexes.EndtimePosition2 +2;
              ValueLine := ModelMuseFile[FileIndexes.PeriodLengthPosition2];
              Assert(Pos('PeriodLength = ', ValueLine) > 0);

              FileIndexes.StartTimePos2 := FileIndexes.EndtimePosition2 +3;
              ValueLine := ModelMuseFile[FileIndexes.StartTimePos2];
              Assert(Pos('StartTime = ', ValueLine) > 0);

              FileIndexes.TSMultPosition2 := FileIndexes.PeriodLengthPosition2 +3;
              ValueLine := ModelMuseFile[FileIndexes.TSMultPosition2];
              Assert(Pos('TimeStepMultiplier = ', ValueLine) > 0);


              FileIndexes.EndtimePosition3 := FileIndexes.EndtimePosition2 + 9;
              ValueLine := ModelMuseFile[FileIndexes.EndtimePosition3];
              Assert(Pos('EndTime = ', ValueLine) > 0);

              FileIndexes.FirstTimeStepPosition3 := FileIndexes.EndtimePosition3+1;
              ValueLine := ModelMuseFile[FileIndexes.FirstTimeStepPosition3];
              Assert(Pos('MaxLengthOfFirstTimeStep = ', ValueLine) > 0);

              FileIndexes.PeriodLengthPosition3 := FileIndexes.EndtimePosition3 +2;
              ValueLine := ModelMuseFile[FileIndexes.PeriodLengthPosition3];
              Assert(Pos('PeriodLength = ', ValueLine) > 0);

              FileIndexes.StartTimePos3 := FileIndexes.EndtimePosition3 +3;
              ValueLine := ModelMuseFile[FileIndexes.StartTimePos3];
              Assert(Pos('StartTime = ', ValueLine) > 0);

              FileIndexes.TSMultPosition3 := FileIndexes.PeriodLengthPosition3 +3;
              ValueLine := ModelMuseFile[FileIndexes.TSMultPosition3];
              Assert(Pos('TimeStepMultiplier = ', ValueLine) > 0);





              SearchTerm := '      Variable.Name = ''BasinDepth''';
              PositionIndex := ModelMuseFile.IndexOf(SearchTerm);
              Assert(PositionIndex >= 0);
              FileIndexes.DepthPosition := PositionIndex +2;
              ValueLine := ModelMuseFile[FileIndexes.DepthPosition];
              Assert(Pos('Variable.RealValue =', ValueLine) > 0);

//              RootDir := IncludeTrailingPathDelimiter(
//                ExtractFileDir(feModelMuseFile.FileName));
            end;



//              SimpleFileName := ExtractFileName(feModelMuseFile.FileName);
//              try
                ResultRowIndex := 0;
                for ShapeIndex := 0 to 1 do
                begin
                  if ShapeIndex = 0 then
                  begin
                    FileVariables.ShapeName := 'Square';
                    FileVariables.ShapeFactor := 1.;
                  end
                  else
                  begin
                    FileVariables.ShapeName := 'Rectangle';
                    FileVariables.ShapeFactor := RectangleRatio;
                  end;
                  for AreaIndex := 0 to AreaList.Count - 1 do
                  begin
                    FileVariables.BasinArea := AreaList[AreaIndex];
  //                  if TryStrToFloat(rdgBasinAreas.Cells[0,AreaIndex], ) then
                    begin

                      AreaName := 'Area' + IntToStr(AreaIndex) + '\';
                      for KvIndex := 0 to KvList.Count - 1 do
                      begin
                        FileVariables.Kv := KvList[KvIndex];
  //                      if TryStrToFloat(rdgKv.Cells[0,KvIndex], FileVariables.Kv) then
                        begin
                          KvName := 'Kv' + IntToStr(KvIndex) + '\';
                          for RatioIndex := 0 to 1 do
                          begin
                            if RatioIndex = 0 then
                            begin
                              RatioName := 'Ratio1\';
                              FileVariables.Ratio := 10;
                            end
                            else
                            begin
                              RatioName := 'Ratio2\';
                              FileVariables.Ratio := 1;
                            end;
          //                  Kx := FileVariables.Ratio*FileVariables.Kv;
                            for SpecificYieldIndex := 0 to SyList.Count - 1 do
                            begin
                              FileVariables.SpecificYield := SyList[SpecificYieldIndex];
  //                            if TryStrToFloat(rdgSy.Cells[0,SpecificYieldIndex], FileVariables.SpecificYield) then
                              begin
                                SpecificYieldName := 'Sy' + IntToStr(SpecificYieldIndex) + '\';
                                for BasinDepthIndex := 0 to BasinDepthList.Count - 1 do
                                begin
                                  FileVariables.BasinDepth := BasinDepthList[BasinDepthIndex];
  //                                if TryStrToFloat(rdgBasinDepth.Cells[0,BasinDepthIndex], FileVariables.BasinDepth) then
                                  begin
                                    Application.ProcessMessages;
                                    if FShouldAbort then
                                    begin
                                      btnAbort.Enabled := False;
                                      Exit;
                                    end;
                                    Assert( ResultRowIndex < Length(FileValuesArray));
  //                                  begin
                                      FileValuesArray[ResultRowIndex] := FileVariables;
  //                                  end;
                                    Inc(ResultRowIndex);
                                  end;
                                end;
                              end;
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
//              finally
//
//
//              end;


              Assert(ResultRowIndex = Length(FileValuesArray));

//            if rgMethod.ItemIndex = 0 then
            begin
              Dir := IncludeTrailingPathDelimiter(GetCurrentDir);
              StartIndex := ResultRowIndex+1;
              rdgResults.RowCount := ResultRowIndex+1;
              FirstIndex := seStart.AsInteger;
              LastIndex :=  Min(seEnd.AsInteger, ResultRowIndex)-1;
              AskedUser := False;
              for RowIndex := FirstIndex to LastIndex do
              begin
                ModelFileName := Dir + 'Model' + IntToStr(RowIndex) + '.zip';
                if cbRestart.Checked then
                begin
                  if not FileExists(ModelFileName) then
                  begin
                    StartIndex := RowIndex-1;
                    break;
                  end;
                end
                else
                begin
                  if FileExists(ModelFileName) and (rgMethod.ItemIndex = 0) then
                  begin
                    DeleteFile(ModelFileName)
                  end;
                end;
              end;
              if cbRestart.Checked then
              begin
                SetStartingIndex(StartIndex);
                pb1.Max := LastIndex-StartIndex;
              end
              else
              begin
                SetStartingIndex(FirstIndex-1);
                pb1.Max := ResultRowIndex;
              end;
              pb1.Position := 0;

              FThreadCount := seProcessors.AsInteger ;

              FThreadList.Clear;
              for ThreadIndex := 0 to FThreadCount - 1 do
              begin
                MyThread := TRunModelThread.Create(FileIndexes, FileConstants,
                  FileValuesArray, FixedDistances, FOutputFile,
                  IncludeTrailingPathDelimiter(GetCurrentDir) + 'Thread' + IntToStr(ThreadIndex+1),
                  ModelMuseFile, pb1, LastIndex, TMethod(rgMethod.ItemIndex));
                FThreadList.Add(MyThread);
                MyThread.OnTerminate := ThreadTerminate;
              end;

              rdgResults.BeginUpdate;
              for ThreadIndex := 0 to FThreadList.Count - 1 do
              begin
                MyThread := FThreadList[ThreadIndex];
                MyThread.Start;
              end;
//            end
//            else
//            begin
//
            end;

          finally
            FixedDistances.Free;
          end;
        finally
          ModelMuseFile.Free;
        end;
      finally
  //      FOutputFile.Free;
      end;
    end;
  finally
    BasinDepthList.Free;
    SyList.Free;
    KvList.Free;
    AreaList.Free;
  end;

end;

procedure TfrmAutomate.Button2Click(Sender: TObject);
var
  Lines: TStringList;
  RowIndex: Integer;
  ColIndex: Integer;
  ALine: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Capacity := rdgResults.RowCount;
    for RowIndex := 0 to rdgResults.RowCount - 1 do
    begin
      ALine := '';
      for ColIndex := 0 to rdgResults.ColCount - 1 do
      begin
        ALine := ALine + rdgResults.Cells[ColIndex, RowIndex];
        if ColIndex < rdgResults.ColCount - 1 then
        begin
          ALine := ALine + #9;
        end;
      end;
      Lines.Add(ALine);
    end;
    ClipBoard.AsText := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TfrmAutomate.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmAutomate.FormCreate(Sender: TObject);
begin
  FThreadList := TList.Create;
  rdgResults.BeginUpdate;
  try
    rdgResults.Cells[Ord(rcDirectory), 0] := 'Directory';
    rdgResults.Cells[Ord(rcShape), 0] := 'Shape';
    rdgResults.Cells[Ord(rcBasinArea), 0] := 'Basin area';
    rdgResults.Cells[Ord(rcKv), 0] := 'Kv';
    rdgResults.Cells[Ord(rcKx), 0] := 'Kx';
    rdgResults.Cells[Ord(rcSy), 0] := 'Sy';
    rdgResults.Cells[Ord(rcBasinDepth), 0] := 'Basin Depth';
    rdgResults.Cells[Ord(rcPercentDiscCumulative), 0] := 'Maximum Cumulative Percent Discrepancy';
    rdgResults.Cells[Ord(rcPercentDiscTimeStep), 0] := 'Maximum Percent Discrepancy for a Time Step';
    rdgResults.Cells[Ord(rcMaxDrawDown), 0] := 'Maximum drawdown';
    rdgResults.Cells[Ord(rcMaxRow), 0] := 'Highest row number with drawdown < -0.05';
    rdgResults.Cells[Ord(rcMaxDistance), 0] := 'Maximum distance with drawdown < -0.05';
    rdgResults.Cells[Ord(rcFixedDistance), 0] := 'Drawdown at fixed distances';
  finally
    rdgResults.EndUpdate;
  end;

  rdgBasinAreas.Cells[0,0] := 'Basin Areas';
  rdgKv.Cells[0,0] := 'Kv values';
  rdgSy.Cells[0,0] := 'Sy values';
  rdgBasinDepth.Cells[0,0] := 'Basin depths';
  rdgFixedDistances.Cells[0,0] := 'Fixed distances';

  pcMain.ActivePageIndex := 0;

  seProcessors.AsInteger := Max(AvailableProcessorCount-1,1);
end;

procedure TfrmAutomate.FormDestroy(Sender: TObject);
begin
  FOutputFile.Free;
  FThreadList.Free;
end;

procedure TfrmAutomate.Open1Click(Sender: TObject);
var
  AutomateFile: TAutomateFile;
  FileStream: TFileStream;
  TempStream : TMemoryStream;
begin
  if OpenDialog1.Execute then
  begin
    SaveDialog1.FileName := OpenDialog1.FileName;
    AutomateFile := TAutomateFile.Create(nil);
    try
      FileStream := TFileStream.Create(OpenDialog1.FileName,
        fmOpenRead or fmShareDenyWrite, 0);
      try
        FileStream.Position := 0;
        TempStream := TMemoryStream.Create;
        try
          ObjectTextToBinary(FileStream, TempStream);
          TempStream.Position := 0;
          TempStream.ReadComponent(AutomateFile);
        finally
          TempStream.Free;
        end;
      finally
        FreeAndNil(FileStream);
      end;
      AutomateFile.UpdateGui;
    finally
      AutomateFile.Free;
    end;
  end;
end;

procedure TfrmAutomate.PlotResults(Row: integer;
  FileValues: TVariableFileValues; Results: TResults);
begin
//  TResultsColumns = (rcDirectory, rcShape, rcBasinArea, rcKv, rcKx, rcSy,
//    rcBasinDepth, rcPercentDiscCumulative, rcPercentDiscTimeStep, rcMaxDrawDown,
//    rcMaxRow, rcMaxDistance, rcFixedDistance);
  rdgResults.Cells[Ord(rcDirectory), Row] := IntToStr(Row);
  rdgResults.Cells[Ord(rcShape), Row] := FileValues.ShapeName;
  rdgResults.Cells[Ord(rcBasinArea), Row] := FloatToStr(FileValues.BasinArea);
  rdgResults.Cells[Ord(rcKv), Row] := FloatToStr(FileValues.Kv);
  rdgResults.Cells[Ord(rcKx), Row] := FloatToStr(Results.Kx);
  rdgResults.Cells[Ord(rcSy), Row] := FloatToStr(FileValues.SpecificYield);
  rdgResults.Cells[Ord(rcBasinDepth), Row] := FloatToStr(FileValues.BasinDepth);
  rdgResults.Cells[Ord(rcPercentDiscCumulative), Row] := FloatToStr(Results.MaxCumulativePercentDiscrepancy);
  rdgResults.Cells[Ord(rcPercentDiscTimeStep), Row] := FloatToStr(Results.MaxTimeStepPercentDiscrepancy);
  rdgResults.Cells[Ord(rcMaxDrawDown), Row] := FloatToStr(Results.MaxDrawdown);
//  rdgResults.Cells[Ord(rcMaxRow), Row] := FloatToStr(Results.MaxDrawdown);
end;

procedure TfrmAutomate.rdgResultsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := ACol >= Ord(rcPercentDiscCumulative);
end;

procedure TfrmAutomate.Save1Click(Sender: TObject);
var
  Automater: TAutomateFile;
  MemStream: TMemoryStream;
  FileStream: TFileStream;
begin
  if SaveDialog1.Execute then
  begin

    Automater := TAutomateFile.Create(nil);
    try
      MemStream := TMemoryStream.Create;
      try
        FileStream := TFileStream.CREATE(SaveDialog1.FileName, FMCREATE OR FMSHAREDENYWRITE, 0);
        try
          MemStream.WriteComponent(Automater);
          MemStream.Position := 0;
          ObjectBinaryToText(MemStream, FileStream);
        finally
          FileStream.Free;
        end;
      finally
        MemStream.Free;
      end;
    finally
      Automater.Free;
    end;
  end;
end;

procedure TfrmAutomate.ThreadTerminate(Sender: TObject);
begin
  Dec(FThreadCount);
  FThreadList.Remove(Sender);
  if FThreadCount = 0 then
  begin
    btnAbort.Enabled := False;
    FreeAndNil(FOutputFile);
    rdgResults.EndUpdate;
  end;
end;

{ TRealValueItem }

procedure TRealValueItem.Assign(Source: TPersistent);
begin
  if Source is TRealValueItem then
  begin
    Value := TRealValueItem(Source).Value;
  end
  else
  begin
    inherited;
  end;
end;

procedure TRealValueItem.SetValue(const Value: double);
begin
  FValue := Value;
end;

{ TRealValueCollection }

function TRealValueCollection.Add: TRealValueItem;
begin
  result := inherited Add as TRealValueItem;
end;

constructor TRealValueCollection.Create;
begin
  inherited Create(TRealValueItem);
end;

function TRealValueCollection.GetItems(Index: integer): TRealValueItem;
begin
  result := inherited Items[Index] as TRealValueItem;
end;

function TRealValueCollection.GetValue(Index: integer): double;
begin
  result := Items[Index].Value;
end;

procedure TRealValueCollection.SetItems(Index: integer; const Value: TRealValueItem);
begin
  inherited Items[Index] := Value;
end;

procedure TRealValueCollection.SetValue(Index: integer; const Value: double);
begin
  Items[Index].Value := Value;
end;

{ TAutomateFile }

function TAutomateFile.GetBasinAreas: TRealValueCollection;
begin
  GetRealValueCollection(FBasinAreas, frmAutomate.rdgBasinAreas);
  result := FBasinAreas;
end;

function TAutomateFile.GetBasinDepths: TRealValueCollection;
begin
  GetRealValueCollection(FBasinDepths, frmAutomate.rdgBasinDepth);
  result := FBasinDepths;
end;

function TAutomateFile.GetFixedDistances: TRealValueCollection;
begin
  GetRealValueCollection(FFixedDistances, frmAutomate.rdgFixedDistances);
  result := FFixedDistances;
end;

function TAutomateFile.GetKv: TRealValueCollection;
begin
  GetRealValueCollection(FKv, frmAutomate.rdgKv);
  result := FKv;
end;

function TAutomateFile.GetMethod: Integer;
begin
  result := frmAutomate.rgMethod.ItemIndex;
end;

function TAutomateFile.GetModelLocation: string;
begin
  result := frmAutomate.feModelMuseFile.FileName;
end;

function TAutomateFile.GetModelMuseLocation: string;
begin
  result := frmAutomate.feModelMuseApplication.FileName;
end;

function TAutomateFile.GetModflowLocation: string;
begin
  result := frmAutomate.feMODFLOW.FileName;
end;

function TAutomateFile.GetNSTP: Integer;
begin
  result := StrToInt(frmAutomate.rdeNSTP.Text)
end;

function TAutomateFile.GetRatio: double;
begin
  Result := StrToFloat(frmAutomate.rdeRatio.Output);
end;

function TAutomateFile.GetRunModflow: Boolean;
begin
  Result := frmAutomate.cbRunModflow.Checked;
end;

function TAutomateFile.GetSy: TRealValueCollection;
begin
  GetRealValueCollection(FSy, frmAutomate.rdgSy);
  result := FSy;
end;

function TAutomateFile.GetTimeOut: integer;
begin
  Result := frmAutomate.seTimeOut.AsInteger;
end;

procedure TAutomateFile.SetBasinAreas(const Value: TRealValueCollection);
begin
  SetRealValueCollection(FBasinAreas, Value, frmAutomate.rdgBasinAreas)
end;

procedure TAutomateFile.SetBasinDepths(const Value: TRealValueCollection);
begin
  SetRealValueCollection(FBasinDepths, Value, frmAutomate.rdgBasinDepth)
end;

procedure TAutomateFile.SetFixedDistances(const Value: TRealValueCollection);
begin
  SetRealValueCollection(FFixedDistances, Value, frmAutomate.rdgFixedDistances)
end;

procedure TAutomateFile.SetKv(const Value: TRealValueCollection);
begin
  SetRealValueCollection(FKv, Value, frmAutomate.rdgKv)
end;

procedure TAutomateFile.SetMethod(const Value: Integer);
begin
  frmAutomate.rgMethod.ItemIndex := Value;
end;

procedure TAutomateFile.SetModelLocation(const Value: string);
begin
  frmAutomate.feModelMuseFile.FileName := Value;
end;

procedure TAutomateFile.SetModelMuseLocation(const Value: string);
begin
  frmAutomate.feModelMuseApplication.FileName := Value;
end;

procedure TAutomateFile.SetModflowLocation(const Value: string);
begin
  frmAutomate.feMODFLOW.FileName := Value;
end;

procedure TAutomateFile.SetNSTP(const Value: Integer);
begin
  frmAutomate.rdeNSTP.Text := IntToStr(Value);
end;

procedure TAutomateFile.SetRatio(const Value: double);
begin
  frmAutomate.rdeRatio.Text := FloatToStr(Value);
end;

procedure TAutomateFile.SetRunModflow(const Value: Boolean);
begin
  frmAutomate.cbRunModflow.Checked := Value;
end;

procedure TAutomateFile.SetSy(const Value: TRealValueCollection);
begin
  SetRealValueCollection(FSy, Value, frmAutomate.rdgSy)
end;

procedure TAutomateFile.SetTimeOut(const Value: integer);
begin
  frmAutomate.seTimeOut.AsInteger := Value;
end;

procedure TAutomateFile.UpdateGui;
begin
  UpdateGrid(FBasinAreas, frmAutomate.rdgBasinAreas);
  UpdateGrid(FKv, frmAutomate.rdgKv);
  UpdateGrid(FSy, frmAutomate.rdgSy);
  UpdateGrid(FBasinDepths, frmAutomate.rdgBasinDepth);
  UpdateGrid(FFixedDistances, frmAutomate.rdgFixedDistances);
end;

procedure TAutomateFile.UpdateGrid(var Collection: TRealValueCollection; Grid: TRbwDataGrid4);
var
  RowIndex: Integer;
begin
  if Collection = nil then
  begin
    Collection := TRealValueCollection.Create;
  end;
  Grid.BeginUpdate;
  try
    if Collection.Count+1 > Grid.RowCount then
    begin
      Grid.RowCount := Collection.Count+1;
    end;
    for RowIndex := 1 to Grid.RowCount - 1 do
    begin
      Grid.Cells[0, RowIndex] := '';
    end;
    for RowIndex := 0 to Collection.Count - 1 do
    begin
      Grid.Cells[0, RowIndex+1] := FloatToStr(Collection[RowIndex]);
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TAutomateFile.GetRealValueCollection(
  var Collection: TRealValueCollection; Grid: TRbwDataGrid4);
var
  AValue: Extended;
  Item: TRealValueItem;
  ValidCount: Integer;
  RowIndex: Integer;
begin
  if Collection = nil then
  begin
    Collection := TRealValueCollection.Create;
  end;
  ValidCount := 0;
  for RowIndex := 1 to Grid.RowCount - 1 do
  begin
    if TryStrToFloat(Grid.Cells[0, RowIndex], AValue) then
    begin
      Inc(ValidCount);
      if ValidCount > Collection.Count then
      begin
        Item := Collection.Add;
      end
      else
      begin
        Item := Collection.Items[ValidCount - 1];
      end;
      Item.Value := AValue;
    end;
  end;
  while (Collection.Count > ValidCount) do
  begin
    Collection.Delete(Collection.Count - 1);
  end;
end;

procedure TAutomateFile.SetRealValueCollection(
  var Collection: TRealValueCollection; const Value: TRealValueCollection;
  Grid: TRbwDataGrid4);
begin
  if Collection = nil then
  begin
    Collection := TRealValueCollection.Create;
  end;
  Collection.Assign(Value);
  UpdateGrid(Collection, Grid);
end;

{ TRunModelThread }

constructor TRunModelThread.Create(Indicies: TFileIndexes;
  Constants: TConstantFileValues; FileValues: TFileValuesArray;
  FixedDistances: TRealList; OutputFile: TStreamWriter;
  Directory: string; ModelMuseFile: TStringList; ProgressBar: TProgressBar;
  LastIndex: integer; Method: TMethod);
begin
  inherited Create(True);
  FLastIndex := LastIndex;
  FOutputFile:= OutputFile ;
  FreeOnTerminate := True;
  FIndicies := Indicies;
  FConstants := Constants;
  FFixedDistances := TRealList.Create;
  FFixedDistances.Assign(FixedDistances);
  FFileValues := FileValues;
  SetLength(FFileValues, Length(FFileValues));
  FModelMuseFile := TStringList.Create;
  FModelMuseFile.Assign(ModelMuseFile);
  FDirectory := Directory;
  FMethod := Method;
  if not DirectoryExists(FDirectory) then
  begin
    ForceDirectories(FDirectory)
  end;
  FProgressBar := ProgressBar;
  FCreateProcess := TJvCreateProcess.Create(nil);
  FCreateProcess.OnTerminate := ProcessDone;
  SetLength(FResults.FixedDistanceDrawDowns, FFixedDistances.Count);
  SetLength(FixedDistanceCells, FFixedDistances.Count);
end;

destructor TRunModelThread.Destroy;
begin
  FCreateProcess.Free;
  FModelMuseFile.Free;
  inherited;
end;

procedure TRunModelThread.ExtractedHydmodResults(HydInput: TStringList);
var
  HydModFile: string;
  AValue: TModflowDouble;
  LabelPos: Integer;
  LabelIndex: Integer;
  Distance: Double;
  TimeIndex: Integer;
  HydCell: THydCell;
  CellIndex: Integer;
  TestDistIndex: Integer;
  HydModResults: THydModData;
  ACell: THydCell;
  LabelPositions: TIntegerList;
begin
  HydModResults := THydModData.Create;
  try
    HydModFile := ChangeFileExt(FModelMuseFileName, '.hyd_out');
    HydModResults.ReadFile(HydModFile);
    for TestDistIndex := 0 to FFixedDistances.Count - 1 do
    begin
      ACell := FixedDistanceCells[TestDistIndex];
      CellIndex := HydModResults.IndexOfLabel(ACell.Name);
      Assert(CellIndex >= 0);
      FResults.FixedDistanceDrawDowns[TestDistIndex] := HydModResults.Values[CellIndex, 0];
      for TimeIndex := 0 to HydModResults.TimeCount - 1 do
      begin
        if HydModResults.Values[CellIndex, TimeIndex] < FResults.FixedDistanceDrawDowns[TestDistIndex] then
        begin
          FResults.FixedDistanceDrawDowns[TestDistIndex] := HydModResults.Values[CellIndex, TimeIndex];
        end;
      end;
    end;
    LabelPositions := TIntegerList.Create;
    try
      LabelPositions.Capacity := HydModResults.LabelCount;
      for LabelIndex := 0 to HydModResults.LabelCount - 1 do
      begin
        LabelPositions.Add(HydInput.IndexOf(HydModResults.Labels[LabelIndex]));
      end;
      if (HydModResults.LabelCount > 0) and (HydModResults.TimeCount > 0) then
      begin
        FResults.MaxDrawdown := 0;
        FResults.RowForDrawdown005 := 0;
        FResults.DistanceForDrawdown005 := 0;
        FResults.RowForDrawdown025 := 0;
        FResults.DistanceForDrawdown025 := 0;
        FResults.RowForDrawdown100 := 0;
        FResults.DistanceForDrawdown100 := 0;
        for LabelIndex := 0 to LabelPositions.Count - 1 do
        begin
          LabelPos := LabelPositions[LabelIndex];
          if LabelPos >= 0 then
          begin
            HydCell := HydInput.Objects[LabelPos] as THydCell;
            Assert(HydCell.Name = HydModResults.Labels[LabelIndex]);
            for TimeIndex := 0 to HydModResults.TimeCount - 1 do
            begin
              AValue := HydModResults.Values[LabelIndex, TimeIndex];
              if FResults.MaxDrawdown > AValue then
              begin
                FResults.MaxDrawdown := AValue;
              end;
              if AValue <= -0.05 then
              begin
                Distance := Abs(FCenterY - HydCell.Y);
                if FResults.DistanceForDrawdown005 < Distance then
                begin
                  FResults.DistanceForDrawdown005 := Distance;
                  FResults.RowForDrawdown005 := HydCell.Row;
                end;
              end;
              if AValue <= -0.25 then
              begin
                Distance := Abs(FCenterY - HydCell.Y);
                if FResults.DistanceForDrawdown025 < Distance then
                begin
                  FResults.DistanceForDrawdown025 := Distance;
                  FResults.RowForDrawdown025 := HydCell.Row;
                end;
              end;
              if AValue <= -1.00 then
              begin
                Distance := Abs(FCenterY - HydCell.Y);
                if FResults.DistanceForDrawdown100 < Distance then
                begin
                  FResults.DistanceForDrawdown100 := Distance;
                  FResults.RowForDrawdown100 := HydCell.Row;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      LabelPositions.Free;
    end;
  finally
    HydModResults.Free;
  end;
end;

procedure TRunModelThread.ReadHydmodInput(RowPositions: TRealList; ColPositions: TRealList; var HydInput: TStringList);
var
  Y: Double;
  Splitter: TStringList;
  HydModInputFile: string;
  ClosestBoundary: Integer;
  ALine: string;
  X: Double;
  HydModFile: TStreamReader;
  HydCell: THydCell;
  TestDistances: array of double;
  DistIndex: Integer;
  FixedDist: Double;
  CellIndex: Integer;
  TestDist: double;
begin
  HydModInputFile := ChangeFileExt(FModelMuseFileName, '.hyd');
  HydModFile := TFile.OpenText(HydModInputFile);
  try
    ALine := HydModFile.ReadLine;
    while (ALine = '') or (ALine[1] = '#') do
    begin
      ALine := HydModFile.ReadLine;
    end;
    // Skip a line to get to Data Set 2;
//    HydModFile.ReadLine;
    Splitter := TStringList.Create;
    try
      Splitter.Delimiter := ' ';
      repeat
        Splitter.DelimitedText := Trim(HydModFile.ReadLine);
        if (UpperCase(Splitter[0]) = 'BAS') and (UpperCase(Splitter[1]) = 'DD') then
        begin
          HydCell := THydCell.Create;
          HydCell.Name := UpperCase(Splitter[1] + Splitter[2]);
          HydCell.Layer := StrToInt(Splitter[3]);
          While (Length(Splitter[3]) < 3) do
          begin
            Splitter[3] := '0' + Splitter[3];
          end;
          HydCell.Name := HydCell.Name + Splitter[3] + Splitter[6];
          X := StrToFloat(Splitter[4]);
          Y := StrToFloat(Splitter[5]);
          ClosestBoundary := ColPositions.IndexOfClosest(X);
          if ColPositions[ClosestBoundary] < ClosestBoundary then
          begin
            HydCell.Col := ClosestBoundary + 1;
          end
          else
          begin
            HydCell.Col := ClosestBoundary;
          end;
          ClosestBoundary := RowPositions.IndexOfClosest(y);
          if RowPositions[ClosestBoundary] < ClosestBoundary then
          begin
            HydCell.Row := ClosestBoundary + 1;
          end
          else
          begin
            HydCell.Row := ClosestBoundary;
          end;
          if UpperCase(Splitter[2]) = 'C' then
          begin
            HydCell.X := (ColPositions[HydCell.Col - 1] + ColPositions[HydCell.Col]) / 2;
            HydCell.Y := (RowPositions[HydCell.Row - 1] + RowPositions[HydCell.Row]) / 2;
          end
          else
          begin
            HydCell.X := X;
            HydCell.Y := Y;
          end;
          HydInput.AddObject(HydCell.Name, HydCell);
        end;
      until (HydModFile.EndOfStream);
    finally
      Splitter.Free;
    end;
  finally
    HydModFile.Free;
  end;
  HydInput.Sorted := True;
  SetLength(TestDistances, FFixedDistances.Count);
  if HydInput.Count > 0 then
  begin
    HydCell := HydInput.Objects[0] as THydCell;
    for DistIndex := 0 to FFixedDistances.Count - 1 do
    begin
      FixedDist := FFixedDistances[DistIndex];
      TestDistances[DistIndex] := Abs(Abs(FCenterY-HydCell.Y)-FixedDist);
      FixedDistanceCells[DistIndex] := HydCell;
    end;
    for CellIndex := 1 to HydInput.Count - 1 do
    begin
      HydCell := HydInput.Objects[CellIndex] as THydCell;
      for DistIndex := 0 to FFixedDistances.Count - 1 do
      begin
        FixedDist := FFixedDistances[DistIndex];
        TestDist := Abs(Abs(FCenterY-HydCell.Y)-FixedDist);
        if TestDist < TestDistances[DistIndex] then
        begin
          TestDistances[DistIndex] := TestDist;
          FixedDistanceCells[DistIndex] := HydCell;
        end
        else if (TestDist = TestDistances[DistIndex])
          and (HydCell.Layer < FixedDistanceCells[DistIndex].Layer) then
        begin
          FixedDistanceCells[DistIndex] := HydCell;
        end;
      end;
    end;
  end;
end;

procedure TRunModelThread.GetColRowPositions(var ColPositions: TRealList; var RowPositions: TRealList);
var
  Index: Integer;
  DisFileName: string;
  NCOL: Integer;
  RowPosition: Double;
  ALine: string;
  Splitter: TStringList;
  NROW: Integer;
  DisFile: TStreamReader;
  ColPosition: Double;
begin
  DisFileName := ChangeFileExt(FModelMuseFileName, '.dis');
  DisFile := TFile.OpenText(DisFileName);
  try
    ALine := DisFile.ReadLine;
    while (ALine = '') or (ALine[1] = '#') do
    begin
      ALine := DisFile.ReadLine;
    end;
    Splitter := TStringList.Create;
    try
      // Data set 1.
      Splitter.Delimiter := ' ';
      Splitter.DelimitedText := Trim(ALine);
      NROW := StrToInt(Splitter[1]);
      NCOL := StrToInt(Splitter[2]);
      ColPositions.Capacity := NCOL+1;
      RowPositions.Capacity := NROW+1;
      ColPositions.Add(0);
      RowPositions.Add(0);
      // Skip two lines to get to DELR.
      DisFile.ReadLine;
      DisFile.ReadLine;
      ColPosition := 0;
      while ColPositions.Count < ColPositions.Capacity do
      begin
        Splitter.DelimitedText := Trim(DisFile.ReadLine);
        for Index := 0 to Splitter.Count - 1 do
        begin
          ColPosition := ColPosition + StrToFloat(Splitter[Index]);
          ColPositions.Add(ColPosition);
        end;
      end;
      Assert(ColPositions.Count = NCOL+1);
      // Skip one line to get to DELC
      DisFile.ReadLine;
      RowPosition := 0;
      while RowPositions.Count < RowPositions.Capacity do
      begin
        Splitter.DelimitedText := Trim(DisFile.ReadLine);
        for Index := 0 to Splitter.Count - 1 do
        begin
          RowPosition := RowPosition + StrToFloat(Splitter[Index]);
          RowPositions.Add(RowPosition);
        end;
      end;
      Assert(RowPositions.Count = NROW+1);
      FCenterX := (ColPositions[0] + ColPositions[ColPositions.Count-1])/2;
      FCenterY := (RowPositions[0] + RowPositions[RowPositions.Count-1])/2;
    finally
      Splitter.Free;
    end;
  finally
    DisFile.Free;
  end;
  RowPositions.Sorted := true;
  ColPositions.Sorted := true;
end;

procedure TRunModelThread.ExtractFromListingFile;
const
  BudgetSearch = 'BUDGET FOR ENTIRE MODEL AT END OF TIME STEP';
  DiscSearch = 'PERCENT DISCREPANCY =';
var
  BudStart: Integer;
  DiscrepStart1: Integer;
  DiscrepEnd: Integer;
  AnAnsiString: AnsiString;
  ListingFileName: string;
  TimeStepString: string;
  StringBuilder: TStringBuilder;
  CumPercent: Double;
  LineIndex: Integer;
  SeachIndex: Integer;
  CumPercentString: string;
  AString: string;
  TimeStepPercent: Double;
  OffSet: Integer;
  DiscrepStart2: Integer;
  ListFile: TStreamReader;
begin
  ListingFileName := ChangeFileExt(FModelMuseFileName, '.lst');
  ListFile := TFile.OpenText(ListingFileName);
  try
    StringBuilder := TStringBuilder.Create;
    try
      SeachIndex := 0;
      repeat
        StringBuilder.Clear;
        for LineIndex := 1 to 1000 do
        begin
          StringBuilder.Append(ListFile.ReadLine);
          StringBuilder.AppendLine;
          if ListFile.EndOfStream then
          begin
            break;
          end;
        end;
        AString := StringBuilder.ToString;
        AnAnsiString := AnsiString(AString);
        OffSet := 1;
        repeat
          if SeachIndex = 0 then
          begin
            BudStart := BMPosSimpleEx(BudgetSearch, AnAnsiString, OffSet);
            if BudStart > 0 then
            begin
              OffSet := BudStart + Length(BudgetSearch);
              SeachIndex := 1;
            end;
          end
          else
          begin
            BudStart := 1;
            OffSet := 1;
          end;
          if SeachIndex = 1 then
          begin
            DiscrepStart1 := BMPosSimpleEx(DiscSearch, AnAnsiString, OffSet);
            if DiscrepStart1 > 0 then
            begin
              DiscrepStart1 := DiscrepStart1 + Length(DiscSearch);
              DiscrepStart2 := BMPosSimpleEx(DiscSearch, AnAnsiString, DiscrepStart1);
              Assert(DiscrepStart2 > DiscrepStart1);
              CumPercentString := Trim(Copy(AString, DiscrepStart1, DiscrepStart2 - DiscrepStart1));
              CumPercent := Abs(StrToFloat(CumPercentString));
              DiscrepStart2 := DiscrepStart2 + Length(DiscSearch);
              DiscrepEnd := BMPosSimpleEx(sLineBreak, AnAnsiString, DiscrepStart2);
              Assert(DiscrepEnd > DiscrepStart2);
              TimeStepString := Trim(Copy(AString, DiscrepStart2, DiscrepEnd - DiscrepStart2));
              TimeStepPercent := Abs(StrToFloat(TimeStepString));
              if CumPercent > FResults.MaxCumulativePercentDiscrepancy then
              begin
                FResults.MaxCumulativePercentDiscrepancy := CumPercent;
              end;
              if TimeStepPercent > FResults.MaxTimeStepPercentDiscrepancy then
              begin
                FResults.MaxTimeStepPercentDiscrepancy := TimeStepPercent;
              end;
              SeachIndex := 0;
              OffSet := DiscrepEnd;
            end
            else
            begin
              BudStart := 0;
            end;
          end;
        until (BudStart = 0);
      until (ListFile.EndOfStream);
//      StringBuilder.Clear;
    finally
      StringBuilder.Free;
    end;
  finally
    ListFile.Free;
  end;
end;

procedure TRunModelThread.Execute;
var
  FileIndex: Integer;
begin
  inherited;
  FileIndex := GetFileVarIndex;
  while (FileIndex < Length(FFileValues)) and (FileIndex <= FLastIndex) do
  begin
    FFileIndex := FileIndex;
    if Terminated then Exit;
    if FMethod = mModflow then
    begin
      PrepareModelMuseFile;
      if Terminated then Exit;
      RunModelMuse;
      if Terminated then Exit;
      RunModflow;
      if Terminated then Exit;
      ExtractResults;
    end
    else
    begin
      RunHantush;
//      if Terminated then Exit;
    end;
    if Terminated then Exit;
    Synchronize(SaveResults);
    if Terminated then Exit;
    Synchronize(UpdateProgressBar);
    if Terminated then Exit;
    if FMethod = mModflow then
    begin
      SaveFiles;
      if Terminated then Exit;
    end;
    FileIndex := GetFileVarIndex;
  end;
end;

procedure TRunModelThread.ExtractResults;

var
  OldDecSeparator : Char;
  ColPositions: TRealList;
  RowPositions: TRealList;
  HydInput: TStringList;
  Index: Integer;
begin
{
   Variables to get:

   Included in TVariableFileValues:
     Shape
     Basin area
     Kv
     Sy
     Basin depth

   Calculated:
     Kx

   Extracted from Listing file:
     Max cummulative percent discrepancy
     Max percent discrepancy for a time step

   Extracted from hydmod output
     Max drawdown

   Determined using a combination of the .dis, hydmod input, and hydmod output
     Highest row number with drawdown < -0.05
     Maximum distance with drawdown < -0.05
     Drawdown at fixed distances
}

  FResults.MaxCumulativePercentDiscrepancy := 0;
  FResults.MaxTimeStepPercentDiscrepancy := 0;
  OldDecSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    ExtractFromListingFile;

    HydInput := TStringList.Create;
    try
      ColPositions := TRealList.Create;
      RowPositions := TRealList.Create;
      try
        GetColRowPositions(ColPositions, RowPositions);
        ReadHydmodInput(RowPositions, ColPositions, HydInput);
      finally
        RowPositions.Free;
        ColPositions.Free;
      end;
      ExtractedHydmodResults(HydInput);

    finally
      for Index := 0 to HydInput.Count - 1 do
      begin
        HydInput.Objects[Index].Free;
      end;
      HydInput.Free;
    end;


  finally
    FormatSettings.DecimalSeparator := OldDecSeparator;
  end;




end;

procedure TRunModelThread.PrepareModelMuseFile;
var
  YOffset: Extended;
  XOffset: Extended;
  CalculatedArea: Extended;
  CurrentFileValues: TVariableFileValues;
  TSMULT: Double;
  PointLines: TStringList;
  PointLineIndex: Integer;
begin
  CurrentFileValues := FFileValues[FFileIndex];
  YOffset := Sqrt(CurrentFileValues.BasinArea/CurrentFileValues.ShapeFactor)/2;
  XOffset := YOffset*CurrentFileValues.ShapeFactor;
  CalculatedArea := ((YOffset*2)*(XOffset*2));
  Assert((CalculatedArea-CurrentFileValues.BasinArea)/CurrentFileValues.BasinArea < 1e-6);

  PointLines := TStringlist.Create;
  try
    FillPointLines(PointLines, XOffset,YOffset);


    Assert(FIndicies.PointsStart + PointLines.Count-1 = FIndicies.PointsEnd);
    for PointLineIndex := 0 to PointLines.Count - 1 do
    begin
      FModelMuseFile[PointLineIndex+FIndicies.PointsStart] :=
        PointLines[PointLineIndex];
    end;

    FResults.StressPeriodLength := CurrentFileValues.BasinDepth/CurrentFileValues.Kv;
    TSMULT := GetTSMULT(FResults.StressPeriodLength, FConstants.FirstStepLength, FConstants.NSTP);

    FResults.Kx := CurrentFileValues.Ratio*CurrentFileValues.Kv;
    FModelMuseFile[FIndicies.KhPosition] := '      Value = ' + FloatToStr(FResults.Kx);
    FModelMuseFile[FIndicies.KvPosition] := '      Value = ' + FloatToStr(CurrentFileValues.Kv);
    FModelMuseFile[FIndicies.RCH_PARPosition] := '      Value = ' + FloatToStr(CurrentFileValues.Kv);
    FModelMuseFile[FIndicies.SyPosition] := '      Value = ' + FloatToStr(CurrentFileValues.SpecificYield);

    FModelMuseFile[FIndicies.EndtimePosition1] := '      EndTime = ' + FloatToStr(FResults.StressPeriodLength);
    FModelMuseFile[FIndicies.PeriodLengthPosition1] := '      PeriodLength = ' + FloatToStr(FResults.StressPeriodLength);
    FModelMuseFile[FIndicies.ParamEndTimePosition1] := '              EndTime = ' + FloatToStr(FResults.StressPeriodLength);
    FModelMuseFile[FIndicies.ParamStartTimePosition2] := '              StartTime = ' + FloatToStr(FResults.StressPeriodLength);
    FModelMuseFile[FIndicies.ParamEndTimePosition2] := '              EndTime = 10';// + FloatToStr(2*FResults.StressPeriodLength);


    FModelMuseFile[FIndicies.TSMultPosition1] := '      TimeStepMultiplier = ' + FloatToStr(TSMULT);
    FModelMuseFile[FIndicies.FirstTimeStepPosition1] := '      MaxLengthOfFirstTimeStep = ' + FloatToStr(FConstants.FirstStepLength);

    FModelMuseFile[FIndicies.EndtimePosition2] := '      EndTime = ' + FloatToStr(FResults.StressPeriodLength*2);
    FModelMuseFile[FIndicies.PeriodLengthPosition2] := '      PeriodLength = ' + FloatToStr(FResults.StressPeriodLength);
    FModelMuseFile[FIndicies.TSMultPosition2] := '      TimeStepMultiplier = ' + FloatToStr(TSMULT);
    FModelMuseFile[FIndicies.FirstTimeStepPosition2] := '      MaxLengthOfFirstTimeStep = ' + FloatToStr(FConstants.FirstStepLength);
    FModelMuseFile[FIndicies.StartTimePos2] := '      StartTime = ' + FloatToStr(FResults.StressPeriodLength);


    FModelMuseFile[FIndicies.EndtimePosition3] := '      EndTime = ' + FloatToStr(10-FResults.StressPeriodLength*2);
    FModelMuseFile[FIndicies.PeriodLengthPosition3] := '      PeriodLength = ' + FloatToStr(10-FResults.StressPeriodLength*2);
    FModelMuseFile[FIndicies.TSMultPosition3] := '      TimeStepMultiplier = 1';// + FloatToStr(TSMULT);
    FModelMuseFile[FIndicies.FirstTimeStepPosition3] := '      MaxLengthOfFirstTimeStep = 1';// + FloatToStr(FConstants.FirstStepLength);
    FModelMuseFile[FIndicies.StartTimePos3] := '      StartTime = ' + FloatToStr(FResults.StressPeriodLength*2);



    FModelMuseFile[FIndicies.DepthPosition] := '      Variable.RealValue = ' +
      FloatToStr(-CurrentFileValues.BasinDepth);

    FModelMuseFileName := IncludeTrailingPathDelimiter(FDirectory)
      + 'Model' + IntToStr(FFileIndex+1) + '.gpt';
    FModelMuseFile.SaveToFile(FModelMuseFileName);
  finally
    PointLines.Free;
  end;
end;

procedure TRunModelThread.ProcessDone(Sender: TObject; ExitCode: DWORD);
begin
  Suspended := False;
end;

procedure TRunModelThread.RunHantush;
const
  AquiferThickness = 10;
var
  CurrentFileValues: TVariableFileValues;
  YOffset: Extended;
  XOffset: Extended;
  CalculatedArea: Extended;
  InitialHead: double;
  RechargeRate: Double;
  RechargeTime: double;
  SimulationTime: Double;
  XIndex: Integer;
  X: Double;
  MoundHeight: Double;
  TSMULT: Double;
  SimulationTimes: array of  double;
  TimeIndex: Integer;
  StepLength: Double;
  Drawdown: Double;
//  AquiferThickness: double;
begin
  CurrentFileValues := FFileValues[FFileIndex];
//  AquiferThickness := CurrentFileValues.BasinDepth;
  YOffset := Sqrt(CurrentFileValues.BasinArea/CurrentFileValues.ShapeFactor)/2;
  XOffset := YOffset*CurrentFileValues.ShapeFactor;
  CalculatedArea := ((YOffset*2)*(XOffset*2));
  Assert((CalculatedArea-CurrentFileValues.BasinArea)/CurrentFileValues.BasinArea < 1e-6);

  InitialHead := AquiferThickness;
  RechargeRate := CurrentFileValues.Kv;
  FResults.Kx := CurrentFileValues.Ratio*CurrentFileValues.Kv;
  FResults.StressPeriodLength := CurrentFileValues.BasinDepth/CurrentFileValues.Kv;
  RechargeTime := FResults.StressPeriodLength;
//  SimulationTime := RechargeTime;
  TSMULT := GetTSMULT(FResults.StressPeriodLength, FConstants.FirstStepLength, FConstants.NSTP);

  SetLength(SimulationTimes, FConstants.NSTP*2);
  SimulationTime := 0;
  for TimeIndex := 0 to FConstants.NSTP - 1 do
  begin
    if TimeIndex = 0 then
    begin
      StepLength := FConstants.FirstStepLength;
    end
    else
    begin
      StepLength := StepLength * TSMULT;
    end;
    SimulationTime := SimulationTime + StepLength;
    SimulationTimes[TimeIndex] := SimulationTime;
    SimulationTimes[TimeIndex+FConstants.NSTP] := SimulationTime+RechargeTime;
  end;
{
  TVariableFileValues = record
    BasinArea: Extended;
    ShapeFactor: Extended;
    Kv: Extended;
    Ratio: Integer;
    SpecificYield: Extended;
    BasinDepth: Extended;
    ShapeName: string;
  end;

  TResults = record
    Kx: double;
    MaxCumulativePercentDiscrepancy: double;
    MaxTimeStepPercentDiscrepancy: double;
    FixedDistanceDrawDowns: TDoubleDynArray;
    MaxDrawdown: double;
    StressPeriodLength: Extended;
//  Highest row number with drawdown < -0.05
    RowForDrawdown005: integer;
//  Maximum distance with drawdown < -0.05
    DistanceForDrawdown005: double;
    RowForDrawdown025: integer;
    DistanceForDrawdown025: double;
    RowForDrawdown100: integer;
    DistanceForDrawdown100: double;
  end;
}
  FResults.MaxDrawdown := 0;
  FResults.MaxCumulativePercentDiscrepancy := 0;
  FResults.MaxTimeStepPercentDiscrepancy := 0;
  FResults.RowForDrawdown005 := -1;
  FResults.RowForDrawdown025 := -1;
  FResults.RowForDrawdown100 := -1;
  FResults.DistanceForDrawdown005 := 0;
  FResults.DistanceForDrawdown025 := 0;
  FResults.DistanceForDrawdown100 := 0;


  SetLength(FResults.FixedDistanceDrawDowns, FFixedDistances.Count);
  for XIndex := 0 to FFixedDistances.Count - 1 do
  begin
    FResults.FixedDistanceDrawDowns[XIndex] := 0;
  end;

  for TimeIndex := 0 to Length(SimulationTimes) -1 do
  begin
    SimulationTime := SimulationTimes[TimeIndex];
    for XIndex := 0 to FFixedDistances.Count - 1 do
    begin
      X := FFixedDistances[XIndex];

      MoundHeight := IterateHantushRectangular(InitialHead, RechargeRate, FResults.Kx, AquiferThickness,
        CurrentFileValues.SpecificYield, RechargeTime, SimulationTime, XOffset,
        YOffset, X, 0);

      MoundHeight := MoundHeight - InitialHead;
      Drawdown := -MoundHeight;

      if Drawdown < FResults.MaxDrawdown then
      begin
        FResults.MaxDrawdown := Drawdown;
      end;

      if Drawdown < FResults.FixedDistanceDrawDowns[XIndex]  then
      begin
        FResults.FixedDistanceDrawDowns[XIndex] := Drawdown;
      end;

      if Drawdown < -0.05 then
      begin
        if X > FResults.DistanceForDrawdown005 then
        begin
          FResults.DistanceForDrawdown005 := X
        end;
      end;

      if Drawdown < -0.25 then
      begin
        if X > FResults.DistanceForDrawdown025 then
        begin
          FResults.DistanceForDrawdown025 := X
        end;
      end;

      if Drawdown < -1 then
      begin
        if X > FResults.DistanceForDrawdown100 then
        begin
          FResults.DistanceForDrawdown100 := X
        end;
      end;

    end;
  end;


end;

procedure TRunModelThread.RunModelMuse;
begin
  DirectoryLock.Acquire;
  try
    SetCurrentDir(FDirectory);
    FCreateProcess.CommandLine := '"' + FConstants.ModelMuseLocation + '" '
      + ExtractFileName(FModelMuseFileName) + ' -e -c';
    FCreateProcess.Run;
  finally
    DirectoryLock.Release;
  end;
  Suspended := True;
end;

procedure TRunModelThread.RunModflow;
begin
  DirectoryLock.Acquire;
  try
    SetCurrentDir(FDirectory);
    FCreateProcess.CommandLine := '"' + FConstants.ModflowLocation + '" '
      + ExtractFileName(ChangeFileExt(FModelMuseFileName, '.nam'));
    FCreateProcess.Run;
  finally
    DirectoryLock.Release;
  end;
  Suspended := True;
end;

procedure TRunModelThread.SaveFiles;
var
  Directory: string;
  Files: TStringDynArray;
  Zipper: TAbZipper;
  ZipName: string;
  FileIndex: Integer;
begin
  Directory := ExtractFileDir(FModelMuseFileName);
  Files := TDirectory.GetFiles(Directory, '*', TSearchOption.soAllDirectories);

  Zipper := TAbZipper.Create(nil);
  try

    ZipName := IncludeTrailingPathDelimiter(TDirectory.GetParent(Directory));
    Zipper.BaseDirectory := ZipName;
    ZipName := ZipName + ExtractFileName(ChangeFileExt(FModelMuseFileName, '.zip'));
    Zipper.FileName := ZipName;
    for FileIndex := 0 to Length(Files) - 1 do
    begin
      Zipper.AddFiles(Files[FileIndex], faAnyFile);
    end;
    Zipper.Save;
  finally
    Zipper.Free
  end;
  for FileIndex := 0 to Length(Files) - 1 do
  begin
    TFile.Delete(Files[FileIndex]);
  end;
end;

procedure TRunModelThread.SaveResults;
var
  CurrentFileValues: TVariableFileValues;
  DistIndex: Integer;
begin
  CurrentFileValues := FFileValues[FFileIndex];
  FOutputFile.Write(FFileIndex+1);
  FOutputFile.Write(#9);
  FOutputFile.Write(CurrentFileValues.ShapeName);
  FOutputFile.Write(#9);
  FOutputFile.Write(CurrentFileValues.BasinArea);
  FOutputFile.Write(#9);
  FOutputFile.Write(CurrentFileValues.Kv);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.Kx);
  FOutputFile.Write(#9);
  FOutputFile.Write(CurrentFileValues.SpecificYield);
  FOutputFile.Write(#9);
  FOutputFile.Write(CurrentFileValues.BasinDepth);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.StressPeriodLength);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.MaxCumulativePercentDiscrepancy);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.MaxTimeStepPercentDiscrepancy);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.RowForDrawdown005);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.DistanceForDrawdown005);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.RowForDrawdown025);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.DistanceForDrawdown025);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.RowForDrawdown100);
  FOutputFile.Write(#9);
  FOutputFile.Write(FResults.DistanceForDrawdown100);
  FOutputFile.Write(#9);
  for DistIndex := 0 to Length(FResults.FixedDistanceDrawDowns) - 1 do
  begin
    FOutputFile.Write(FResults.FixedDistanceDrawDowns[DistIndex]);
    FOutputFile.Write(#9);
  end;
  FOutputFile.WriteLine;
  FOutputFile.Flush;
  frmAutomate.PlotResults(FFileIndex+1, CurrentFileValues, FResults);
end;

procedure TRunModelThread.Terminate;
begin
  inherited;
  if FCreateProcess.State <> psReady then
  begin
    FCreateProcess.Terminate
  end;
end;

procedure TRunModelThread.UpdateProgressBar;
begin
  FProgressBar.StepIt;
end;

initialization
  IndexLock := TCriticalSection.Create;
  DirectoryLock := TCriticalSection.Create;

finalization
  DirectoryLock.Free;
  IndexLock.Free;

end.
