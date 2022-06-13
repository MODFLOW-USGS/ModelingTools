unit ReaderUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, TeeProcs, TeEngine, Chart, Series, Buttons, Menus,
  MyFormUnit, TeeEdit, TeeScroB, CheckLst, IntListUnit, TeeTools,
  VclTee.TeeGDIPlus;

type
  TBudget = class;
  TBudgetItem = class;

  TStringArray = array of string;

  TLineStoredProperties = class(TObject)
  private
    FSeriesPointerStyle: TSeriesPointerStyle;
    FName: string;
    FColor: TColor;
    procedure SetSeriesPointerStyle(const Value: TSeriesPointerStyle);
    procedure SetColor(const Value: TColor);
    procedure SetName(const Value: string);
  public
    property Name: string read FName write SetName;
    property Color: TColor read FColor write SetColor;
    property SeriesPointerStyle: TSeriesPointerStyle read FSeriesPointerStyle
      write SetSeriesPointerStyle;
  end;

  TUsedTime = record
    IsUsed: boolean;
    Time: double;
  end;

  TLineReader = class(TObject)
  private
    FFilePositions: TInt64List;
    FLineCounts: TInt64List;
    FCurrentLines: TStringList;
    FFileName: string;
    FCurrentStartLine: integer;
    FCurrentSection: integer;
    function GetCount: integer;
    procedure SetFileName(const Value: string);
    function GetLine(Index: integer): string;
  public
    Constructor Create;
    Destructor Destroy; override;
    property FileName: string read FFileName write SetFileName;
    property Count: integer read GetCount;
    property Strings[Index: integer]: string read GetLine; default;
    procedure Clear;
  end;

  TfrmZoneBdgtReader = class(TMyForm)
    OpenDialog1: TOpenDialog;
    chartZONEBDGT: TChart;
    pnlBottom: TPanel;
    SaveDialog1: TSaveDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    About2: TMenuItem;
    Read1: TMenuItem;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    Splitter1: TSplitter;
    Panel1: TPanel;
    cbDecay: TCheckBox;
    cbStoredMass: TCheckBox;
    cbInOut: TCheckBox;
    cbDiscrepancy: TCheckBox;
    rgDataSource: TRadioGroup;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    BitBtnClose: TBitBtn;
    Panel5: TPanel;
    Panel2: TPanel;
    lblTime: TLabel;
    lblZone: TLabel;
    comboTimeStep: TComboBox;
    comboZone: TComboBox;
    rgPlotType: TRadioGroup;
    pnlBudgetItems: TPanel;
    pnlInBudget: TPanel;
    Panel7: TPanel;
    cbIn: TCheckBox;
    pnlOutBudget: TPanel;
    Panel3: TPanel;
    cbOut: TCheckBox;
    splBudget: TSplitter;
    sbImage: TSpeedButton;
    sbFormat: TSpeedButton;
    Saveasimage1: TMenuItem;
    FormatChart1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    ChartEditor1: TChartEditor;
    sbZoomIn: TSpeedButton;
    sbZoomOut: TSpeedButton;
    sbPan: TSpeedButton;
    ChartPreviewer1: TChartPreviewer;
    Configure1: TMenuItem;
    Zoomin1: TMenuItem;
    Zoomextents1: TMenuItem;
    miPan: TMenuItem;
    Formatchart2: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintDialog1: TPrintDialog;
    miPrintSetup: TMenuItem;
    miPrintPreview: TMenuItem;
    miPrint: TMenuItem;
    pnlNetBudget: TPanel;
    pnl: TPanel;
    cbNet: TCheckBox;
    splNet: TSplitter;
    clbNet: TCheckListBox;
    clbIn: TCheckListBox;
    clbOut: TCheckListBox;
    ChartTool1: TMarksTipTool;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure rgPlotTypeClick(Sender: TObject);
    procedure comboZoneChange(Sender: TObject);
    procedure comboTimeStepChange(Sender: TObject);
    procedure cbInOutClick(Sender: TObject);
    procedure cbDiscrepancyClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure cbOutClick(Sender: TObject);
    procedure cbInClick(Sender: TObject);
    procedure rgDataSourceClick(Sender: TObject);
    procedure cbDecayClick(Sender: TObject);
    procedure cbStoredMassClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbImageClick(Sender: TObject);
    procedure sbFormatClick(Sender: TObject);
    procedure ToolBar1MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure chartZONEBDGTGetLegendPos(Sender: TCustomChart;
      Index: Integer; var X, Y, XColor: Integer);
    procedure chartZONEBDGTGetLegendRect(Sender: TCustomChart;
      var Rect: TRect);
    procedure chartZONEBDGTAfterDraw(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure sbZoomInClick(Sender: TObject);
    procedure sbZoomOutClick(Sender: TObject);
    procedure sbPanClick(Sender: TObject);
    procedure chartZONEBDGTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure chartZONEBDGTMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure chartZONEBDGTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miPrintSetupClick(Sender: TObject);
    procedure miPrintPreviewClick(Sender: TObject);
    procedure miPrintClick(Sender: TObject);
    procedure cbNetClick(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure clbInClickCheck(Sender: TObject);
    procedure clbOutClickCheck(Sender: TObject);
    procedure clbNetClickCheck(Sender: TObject);
    procedure Series1GetMarkText(Sender: TChartSeries; ValueIndex: Integer;
      var MarkText: String);
  private
    MarkedPoints: TIntegerList;
    LegendRect: TRect;
    ExplanationTop, ExplanationLeft: integer;
    ExplanationVisible: boolean;
    Begun: Boolean;
    XStart, YStart, XEnd, YEnd: double;
    NPER: longint;
    PERLEN: array of single;
    NSTEP: array of longint;
    TSMULT: array of single;
    Used: array of array of TUsedTime;
    StoredLineValues: TList;
    function GetNextLine(SearchTerm: string; Start: integer): integer;
    function GetStringBetween(AString, Before, After: string): string;
    function GetColor(AColor: TColor): TColor;
    procedure ReadZonebdgtFile;
    procedure ReadZonebdgt6File;
    procedure ReadModflowFile;
    procedure ReadMOC3DFile;
    procedure ReadSutraFile;
    procedure ReadHST3DFile;
    procedure ReadMT3DFile;
    procedure ReadSeawatFile;
    function GetNCharacters(var AString: string; N: integer): string;
    procedure ReadFile;
    procedure MakeIncrementalBudget(ZoneName: string; ComponentCount: integer);
    function DeleteLeadingDots(AString: string): string;
    procedure FillTitles(const InList, OutList: TStringList;
      const ZoneIndex: Integer);
    procedure ReadSutra2D3D_File;
    procedure ReadSutra4_File;
    Function TimeStepLength(const StressPeriod, TimeStep: integer): double;
    function GetLineStorage(const Name: string): TLineStoredProperties;
    procedure StoreLineSeriesValues;
    procedure ReadModflowOrSeawatFile(const BudgetStartLine: string;
      const AltBudgetStartLines: TStringArray = nil);
    procedure Series1ClickPointer(Sender: TCustomSeries; ValueIndex, X,
      Y: Integer);
    procedure IncrementStyle(var MyStyle: TSeriesPointerStyle);
    procedure ReadGSFLOW;
    procedure ReadUzfBudget(TimeUnitsString: string;
      LineIndex: Integer; SearchTerm: string;
      const FirstSearchTerm, CumBudgetName, RateBudgetName: string;
      HasSeparateStorageTerm: Boolean);
    procedure ReadABudget(CumBudget, RateBudget: TBudget;
      var LineIndex: Integer; const TimeUnitsString: string;
      HasSeparateStorageTerm: Boolean);
    procedure ReadModflow6File;
    function GetMF6TimeUnit: string;
    { Private declarations }
  public
    FFileName: string;
//    ZBLStringList: TStringList;
    ZBLStringList: TLineReader;

    FBudgetList: TList;
    procedure BudgetListClear;
    procedure PlotResults;
    { Public declarations }
  end;

  TBudget = class(TObject)
    Zone: string;
    StressPeriod: string;
    TimeStep: string;
    TransportStep: string;
    Time: string;
    InList: TList;
    OutList: TList;
    NetList: TList;
    InMinusOut: string;
    Discrepancy: string;
    StoredMass: string;
    Decay: string;
    Activity: String;
    constructor Create;
    destructor Destroy; override;
    function GetStressPeriod: integer;
    function GetTimeStep: integer;
    function GetInMinusOut: double;
    function GetDiscrepancy: double;
    function GetDecay: double;
    function GetStoredMass: double;
    procedure Clear;
    function GetFromListByName(const AList: TList; const AName: string):
      TBudgetItem;
    function GetFromInListByName(const AName: string): TBudgetItem;
    function GetFromOutListByName(const AName: string): TBudgetItem;
  private
    function GetFromNetListByName(const AName: string): TBudgetItem;
  end;

  TBudgetItem = class(TObject)
    Name: string;
    value: string;
    function RealValue: double;
  end;

  TSourceTypes = (stZONEBDGT, stMODFLOW, stModflow6, stGWT, stSUTRA97, stSUTRA,
    stSUTRA4, stMT3D, stHST3D, stSEAWAT2000, stGSFLOW);

var
  frmZoneBdgtReader: TfrmZoneBdgtReader;

implementation

uses Math, Contnrs, frmFormatUnit, frmAboutUnit, frmModChartUnit,
  frmSelectDiscretizationUnit, frmModflowModelUnitsUnit;

{$R *.DFM}

var
  DecimalSign: Char;

const
  SeawatComponent = '>>FOR COMPONENT NO.';
  SeawatBudget = 'CUMMULATIVE MASS BUDGETS AT END OF TRANSPORT STEP';


procedure GetStressPeriod(var I: longint; var PLen: single;
  var N: longint; var Mult: single);
  stdcall; external 'ReadFlow.dll';

procedure ReadStressPeriods(var Success: longbool; var NPER: longint; FNAME: PChar;
  TextLength: LongInt);
  stdcall; external 'ReadFlow.dll';

procedure DeAllocateStressPeriods;
  stdcall; external 'ReadFlow.dll';

function InternationalStrToFloat(Value: string): extended;
var
  DecimalLocation: integer;
  D_Location: integer;
  //CommaSeparator : array[0..255] of Char;
begin

  if DecimalSign = '.' then
  begin
    DecimalLocation := Pos(',', Value);
    if DecimalLocation > 0 then
    begin
      Value[DecimalLocation] := '.';
    end;
  end;
  if DecimalSign = ',' then
  begin
    DecimalLocation := Pos('.', Value);
    if DecimalLocation > 0 then
    begin
      Value[DecimalLocation] := ',';
    end;
  end;

  D_Location := Pos('D', Value);
  if D_Location > 0 then
  begin
    Value[D_Location] := 'E';
  end;
  Result := StrToFloat(Value);
end;

procedure TfrmZoneBdgtReader.FormCreate(Sender: TObject);
begin
  MarkedPoints := TIntegerList.Create;
  StoredLineValues:= TObjectList.Create;
  ExplanationVisible := False;
  ZBLStringList := TLineReader.Create;
  FBudgetList := TList.Create;
  Constraints.MinHeight := Height - Round(chartZONEBDGT.Height / 2);
  Width := rgPlotType.Left + rgPlotType.Width + 8
    + BitBtnClose.Left + BitBtnClose.Width + 12;
  Constraints.MinWidth := Width;
  pnlOutBudget.Width := (pnlOutBudget.Width + pnlInBudget.Width) div 2;
  Panel1.Constraints.MinHeight := Panel1.Height;
  frmSelectDiscretization := TfrmSelectDiscretization.Create(self);
  rgDataSourceClick(nil);
end;

procedure TfrmZoneBdgtReader.FormDestroy(Sender: TObject);
var
  Index: integer;
begin
  ZBLStringList.Free;
  for Index := FBudgetList.Count - 1 downto 0 do
  begin
    TBudget(FBudgetList[Index]).Free;
  end;
  FBudgetList.Free;
  StoredLineValues.Free;
  MarkedPoints.Free;
  if FFileName <> '' then
  begin
    DeleteFile(FFileName);
  end;
end;

function TfrmZoneBdgtReader.GetNextLine(SearchTerm: string; Start: integer):
  integer;
var
  Index: integer;
begin
  result := -1;
  for Index := Start to ZBLStringList.Count - 1 do
  begin
    if Pos(SearchTerm, ZBLStringList.Strings[Index]) > 0 then
    begin
      result := Index;
      break;
    end;
  end;
end;

function TfrmZoneBdgtReader.GetNCharacters(var AString: string; N: integer):
  string;
begin
  result := Copy(AString, 1, N);
  AString := Copy(AString, N + 1, Length(AString));
end;

function TfrmZoneBdgtReader.DeleteLeadingDots(AString: string): string;
begin
  while (Length(AString) > 0) and (AString[1] = '.') do
  begin
    Delete(AString, 1, 1);
  end;
  result := AString;
end;

function TfrmZoneBdgtReader.GetStringBetween(AString, Before, After: string):
  string;
begin
  result := AString;
  if Before <> '' then
  begin
    result := Copy(AString,
      Pos(Before, AString) + Length(Before),
      MAXINT);
  end;

  if After <> '' then
  begin
    result := Copy(result, 1, Pos(After, result) - 1);
  end;

  result := Trim(result);
end;

procedure TfrmZoneBdgtReader.ReadHST3DFile;
var
  SearchTerm: string;
  LineIndex: integer;
  CurrentLine, PreviousLine: string;
  TimeStepString, TimeString: string;
  StartLine, StopLine, InnerLineIndex, EndOfSection: integer;
  BudgetItem, Rate: string;
  RateBudget, AmountBudget: TBudget;
  StepTotalBudget: TBudget;
  CumulativeBudget: TBudget;
  BoundaryConditionBudget: TBudget;
  ABudgetItem: TBudgetItem;
begin
  LineIndex := 0;
  SearchTerm := 'Output at End of Time Step No.';
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  while LineIndex > -1 do
  begin
    RateBudget := TBudget.Create;
    AmountBudget := TBudget.Create;
    FBudgetList.Add(RateBudget);
    FBudgetList.Add(AmountBudget);
    CurrentLine := ZBLStringList.Strings[LineIndex];

    TimeStepString := GetStringBetween(CurrentLine, SearchTerm, '***');

    LineIndex := LineIndex + 2;
    CurrentLine := ZBLStringList.Strings[LineIndex];
    TimeString := GetStringBetween(CurrentLine,
      'Time .......................................................', '(');

    RateBudget.Zone := 'HST3D Flow Budget - Rates';
    RateBudget.TimeStep := TimeStepString;
    RateBudget.Time := TimeString;

    AmountBudget.Zone := 'HST3D Flow Budget - Amounts';
    AmountBudget.TimeStep := TimeStepString;
    AmountBudget.Time := TimeString;

    StartLine := LineIndex + 5;
    SearchTerm := 'imbalance';
    StopLine := GetNextLine(SearchTerm, StartLine + 1);

    if StopLine > -1 then
    begin
      for InnerLineIndex := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[InnerLineIndex];
        if Pos('..', CurrentLine) > 0 then
        begin

          BudgetItem := GetStringBetween(CurrentLine, '', '..');
          CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
          CurrentLine := DeleteLeadingDots(CurrentLine);
          Rate := (GetStringBetween(CurrentLine, '', '('));

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem + ' rate';
          ABudgetItem.value := Rate;
          RateBudget.InList.Add(ABudgetItem);

          Rate := (GetStringBetween(CurrentLine, ')', '('));

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem + ' amount';
          ABudgetItem.value := Rate;
          AmountBudget.InList.Add(ABudgetItem);
        end;
      end;
    end;
    LineIndex := StopLine + 1;
    CurrentLine := ZBLStringList.Strings[LineIndex];

    BudgetItem := GetStringBetween(CurrentLine, '', '..');
    CurrentLine := GetStringBetween(CurrentLine, '..', '');
    Rate := Trim(DeleteLeadingDots(CurrentLine));
    AmountBudget.Discrepancy := Rate;

    StartLine := LineIndex + 1;
    SearchTerm := 'Amounts';
    EndOfSection := GetNextLine(SearchTerm, StartLine);

    SearchTerm := 'imbalance';
    StopLine := GetNextLine(SearchTerm, StartLine + 1);
    if (StopLine > -1) and (EndOfSection > -1) and (StopLine < EndOfSection)
      then
    begin
      RateBudget := TBudget.Create;
      AmountBudget := TBudget.Create;
      FBudgetList.Add(RateBudget);
      FBudgetList.Add(AmountBudget);

      LineIndex := LineIndex + 2;
      CurrentLine := ZBLStringList.Strings[LineIndex];
      if Pos('Heat', CurrentLine) > 0 then
      begin
        RateBudget.Zone := 'HST3D Heat Budget - Rates';
        AmountBudget.Zone := 'HST3D Heat Budget - Amounts';
      end
      else
      begin
        RateBudget.Zone := 'HST3D Solute Budget - Rates';
        AmountBudget.Zone := 'HST3D Solute Budget - Amounts';
      end;

      RateBudget.TimeStep := TimeStepString;
      RateBudget.Time := TimeString;
      AmountBudget.TimeStep := TimeStepString;
      AmountBudget.Time := TimeString;

      StartLine := LineIndex;
      SearchTerm := 'imbalance';
      StopLine := GetNextLine(SearchTerm, StartLine + 1);

      if StopLine > -1 then
      begin
        for InnerLineIndex := StartLine to StopLine do
        begin
          CurrentLine := ZBLStringList.Strings[InnerLineIndex];
          if Pos('..', CurrentLine) > 0 then
          begin

            BudgetItem := GetStringBetween(CurrentLine, '', '..');
            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            CurrentLine := DeleteLeadingDots(CurrentLine);
            Rate := (GetStringBetween(CurrentLine, '', '('));

            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem + ' rate';
            ABudgetItem.value := Rate;
            RateBudget.InList.Add(ABudgetItem);

            Rate := (GetStringBetween(CurrentLine, ')', '('));

            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem + ' amount';
            ABudgetItem.value := Rate;
            AmountBudget.InList.Add(ABudgetItem);
          end;
        end;
      end;
      LineIndex := StopLine + 1;
      CurrentLine := ZBLStringList.Strings[LineIndex];

      BudgetItem := GetStringBetween(CurrentLine, '', '..');
      CurrentLine := GetStringBetween(CurrentLine, '..', '');
      Rate := Trim(DeleteLeadingDots(CurrentLine));
      AmountBudget.Discrepancy := Rate;

      StartLine := LineIndex + 1;
      SearchTerm := 'Amounts';
      EndOfSection := GetNextLine(SearchTerm, StartLine);

      SearchTerm := 'imbalance';
      StopLine := GetNextLine(SearchTerm, StartLine + 1);
      if (StopLine > -1) and (EndOfSection > -1) and (StopLine < EndOfSection)
        then
      begin
        RateBudget := TBudget.Create;
        AmountBudget := TBudget.Create;
        FBudgetList.Add(RateBudget);
        FBudgetList.Add(AmountBudget);

        LineIndex := LineIndex + 2;
        CurrentLine := ZBLStringList.Strings[LineIndex];
        RateBudget.Zone := 'HST3D Solute Budget - Rates';
        AmountBudget.Zone := 'HST3D Solute Budget - Amounts';

        RateBudget.TimeStep := TimeStepString;
        RateBudget.Time := TimeString;
        AmountBudget.TimeStep := TimeStepString;
        AmountBudget.Time := TimeString;

        StartLine := LineIndex;
        SearchTerm := 'imbalance';
        StopLine := GetNextLine(SearchTerm, StartLine + 1);

        if StopLine > -1 then
        begin
          for InnerLineIndex := StartLine to StopLine do
          begin
            CurrentLine := ZBLStringList.Strings[InnerLineIndex];
            if Pos('..', CurrentLine) > 0 then
            begin

              BudgetItem := GetStringBetween(CurrentLine, '', '..');
              CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
              CurrentLine := DeleteLeadingDots(CurrentLine);
              Rate := (GetStringBetween(CurrentLine, '', '('));

              ABudgetItem := TBudgetItem.Create;
              ABudgetItem.Name := BudgetItem + ' rate';
              ABudgetItem.value := Rate;
              RateBudget.InList.Add(ABudgetItem);

              Rate := (GetStringBetween(CurrentLine, ')', '('));

              ABudgetItem := TBudgetItem.Create;
              ABudgetItem.Name := BudgetItem + ' amount';
              ABudgetItem.value := Rate;
              AmountBudget.InList.Add(ABudgetItem);
            end;
          end;
        end;
        LineIndex := StopLine + 1;
        CurrentLine := ZBLStringList.Strings[LineIndex];

        BudgetItem := GetStringBetween(CurrentLine, '', '..');
        CurrentLine := GetStringBetween(CurrentLine, '..', '');
        Rate := Trim(DeleteLeadingDots(CurrentLine));
        AmountBudget.Discrepancy := Rate;
      end;
    end;

    StepTotalBudget := TBudget.Create;
    FBudgetList.Add(StepTotalBudget);

    StepTotalBudget.Zone := 'HST3D Flow Budget - Step Totals';
    StepTotalBudget.TimeStep := TimeStepString;
    StepTotalBudget.Time := TimeString;

    LineIndex := EndOfSection + 1;
    CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    while (CurrentLine = '') do
    begin
      Inc(LineIndex);
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    end;

    while (CurrentLine <> '') do
    begin
      BudgetItem := GetStringBetween(CurrentLine, '', '..');
      CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
      CurrentLine := DeleteLeadingDots(CurrentLine);
      Rate := (GetStringBetween(CurrentLine, '', '('));
      SearchTerm := 'Step total ';
      if Pos(SearchTerm, BudgetItem) > 0 then
      begin
        Delete(BudgetItem, 1, Length(SearchTerm));
      end;

      ABudgetItem := TBudgetItem.Create;
      ABudgetItem.Name := BudgetItem;
      ABudgetItem.value := Rate;
      StepTotalBudget.InList.Add(ABudgetItem);

      Inc(LineIndex);
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    end;

    LineIndex := LineIndex + 1;
    CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    while (CurrentLine = '') do
    begin
      Inc(LineIndex);
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    end;

    if CurrentLine <> 'Cumulative Summary' then
    begin
      StepTotalBudget := TBudget.Create;
      FBudgetList.Add(StepTotalBudget);

      if (Pos('specified T', CurrentLine) > 0) then
      begin
        StepTotalBudget.Zone := 'HST3D Heat Budget - Step Totals';
      end
      else
      begin
        StepTotalBudget.Zone := 'HST3D Solute Budget - Step Totals';
      end;

      StepTotalBudget.TimeStep := TimeStepString;
      StepTotalBudget.Time := TimeString;

      while (CurrentLine <> '') do
      begin
        if Pos('..', CurrentLine) = 0 then
        begin
          PreviousLine := CurrentLine
        end
        else
        begin
          BudgetItem := GetStringBetween(CurrentLine, '', '..');
          CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
          CurrentLine := DeleteLeadingDots(CurrentLine);
          Rate := (GetStringBetween(CurrentLine, '', '('));
          BudgetItem := Trim(PreviousLine + ' ' + BudgetItem);
          PreviousLine := '';
          SearchTerm := 'Step total ';
          if Pos(SearchTerm, BudgetItem) = 1 then
          begin
            Delete(BudgetItem, 1, Length(SearchTerm));
          end;

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          StepTotalBudget.InList.Add(ABudgetItem);
        end;

        Inc(LineIndex);
        CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      end;

      LineIndex := LineIndex + 1;
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      while (CurrentLine = '') do
      begin
        Inc(LineIndex);
        CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      end;

      if CurrentLine <> 'Cumulative Summary' then
      begin
        StepTotalBudget := TBudget.Create;
        FBudgetList.Add(StepTotalBudget);

        StepTotalBudget.Zone := 'HST3D Solute Budget - Step Totals';
        StepTotalBudget.TimeStep := TimeStepString;
        StepTotalBudget.Time := TimeString;

        while (CurrentLine <> '') do
        begin
          if Pos('..', CurrentLine) = 0 then
          begin
            PreviousLine := CurrentLine;
          end
          else
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '..');
            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            CurrentLine := DeleteLeadingDots(CurrentLine);
            Rate := (GetStringBetween(CurrentLine, '', '('));
            BudgetItem := Trim(PreviousLine + ' ' + BudgetItem);
            PreviousLine := '';
            SearchTerm := 'Step total ';
            if Pos(SearchTerm, BudgetItem) = 1 then
            begin
              Delete(BudgetItem, 1, Length(SearchTerm));
            end;

            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            StepTotalBudget.InList.Add(ABudgetItem);

          end;
          Inc(LineIndex);
          CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
        end;
      end;
    end;

    SearchTerm := 'Amounts';
    LineIndex := GetNextLine(SearchTerm, LineIndex + 1);

    LineIndex := LineIndex + 1;
    CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    while (CurrentLine = '') do
    begin
      Inc(LineIndex);
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    end;

    CumulativeBudget := TBudget.Create;
    FBudgetList.Add(CumulativeBudget);

    CumulativeBudget.Zone := 'HST3D Flow Budget - Cumulative';
    CumulativeBudget.TimeStep := TimeStepString;
    CumulativeBudget.Time := TimeString;

    StartLine := LineIndex;
    SearchTerm := 'Fractional';
    StopLine := GetNextLine(SearchTerm, LineIndex + 1);

    for InnerLineIndex := StartLine to StopLine - 1 do
    begin
      CurrentLine := Trim(ZBLStringList.Strings[InnerLineIndex]);
      if (CurrentLine <> '') then
      begin
        BudgetItem := GetStringBetween(CurrentLine, '', '..');
        CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
        CurrentLine := DeleteLeadingDots(CurrentLine);
        Rate := (GetStringBetween(CurrentLine, '', '('));

        ABudgetItem := TBudgetItem.Create;
        ABudgetItem.Name := BudgetItem;
        ABudgetItem.value := Rate;
        CumulativeBudget.InList.Add(ABudgetItem);
      end;
    end;

    LineIndex := StopLine;
    CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    BudgetItem := GetStringBetween(CurrentLine, '', '..');
    CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
    Rate := Trim(DeleteLeadingDots(CurrentLine));

    CumulativeBudget.Discrepancy := Rate;
    Inc(LineIndex);

    CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    while (CurrentLine = '') do
    begin
      Inc(LineIndex);
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    end;

    if Pos('Cumulative', CurrentLine) = 0 then
    begin
      CumulativeBudget := TBudget.Create;
      FBudgetList.Add(CumulativeBudget);

      if (Pos('Heat', CurrentLine) > 0) then
      begin
        CumulativeBudget.Zone := 'HST3D Heat Budget - Cumulative';
      end
      else
      begin
        CumulativeBudget.Zone := 'HST3D Solute Budget - Cumulative';
      end;

      CumulativeBudget.TimeStep := TimeStepString;
      CumulativeBudget.Time := TimeString;

      StartLine := LineIndex;
      SearchTerm := 'Fractional';
      StopLine := GetNextLine(SearchTerm, LineIndex + 1);

      for InnerLineIndex := StartLine to StopLine - 1 do
      begin
        CurrentLine := Trim(ZBLStringList.Strings[InnerLineIndex]);
        if (CurrentLine <> '') then
        begin
          if Pos('..', CurrentLine) = 0 then
          begin
            PreviousLine := CurrentLine
          end
          else
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '..');
            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            CurrentLine := DeleteLeadingDots(CurrentLine);
            Rate := (GetStringBetween(CurrentLine, '', '('));
            BudgetItem := Trim(PreviousLine + ' ' + BudgetItem);
            PreviousLine := '';

            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            CumulativeBudget.InList.Add(ABudgetItem);

          end;
        end;
      end;

      LineIndex := StopLine;
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      BudgetItem := GetStringBetween(CurrentLine, '', '..');
      CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
      Rate := Trim(DeleteLeadingDots(CurrentLine));

      CumulativeBudget.Discrepancy := Rate;
      Inc(LineIndex);

      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      while (CurrentLine = '') do
      begin
        Inc(LineIndex);
        CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      end;

      if Pos('Cumulative', CurrentLine) = 0 then
      begin
        CumulativeBudget := TBudget.Create;
        FBudgetList.Add(CumulativeBudget);

        CumulativeBudget.Zone := 'HST3D Solute Budget - Cumulative';
        CumulativeBudget.TimeStep := TimeStepString;
        CumulativeBudget.Time := TimeString;

        StartLine := LineIndex;
        SearchTerm := 'Fractional';
        StopLine := GetNextLine(SearchTerm, LineIndex + 1);

        for InnerLineIndex := StartLine to StopLine - 1 do
        begin
          CurrentLine := Trim(ZBLStringList.Strings[InnerLineIndex]);
          if (CurrentLine <> '') then
          begin
            if Pos('..', CurrentLine) = 0 then
            begin
              PreviousLine := CurrentLine;
            end
            else
            begin
              BudgetItem := GetStringBetween(CurrentLine, '', '..');
              CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
              CurrentLine := DeleteLeadingDots(CurrentLine);
              Rate := (GetStringBetween(CurrentLine, '', '('));
              BudgetItem := Trim(PreviousLine + ' ' + BudgetItem);
              PreviousLine := '';

              ABudgetItem := TBudgetItem.Create;
              ABudgetItem.Name := BudgetItem;
              ABudgetItem.value := Rate;
              CumulativeBudget.InList.Add(ABudgetItem);

            end;
          end;
        end;
        LineIndex := StopLine;
        CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
        BudgetItem := GetStringBetween(CurrentLine, '', '..');
        CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
        Rate := Trim(DeleteLeadingDots(CurrentLine));

        CumulativeBudget.Discrepancy := Rate;
        Inc(LineIndex);
      end;

    end;

    LineIndex := StopLine + 1;
    CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    while (CurrentLine = '') do
    begin
      Inc(LineIndex);
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    end;

    BoundaryConditionBudget := TBudget.Create;
    FBudgetList.Add(BoundaryConditionBudget);

    BoundaryConditionBudget.Zone := 'HST3D Flow Boundaries - Cumulative';
    BoundaryConditionBudget.TimeStep := TimeStepString;
    BoundaryConditionBudget.Time := TimeString;

    PreviousLine := '';
    while (CurrentLine <> '') do
    begin
      if Pos('..', CurrentLine) = 0 then
      begin
        PreviousLine := CurrentLine
      end
      else
      begin
        BudgetItem := GetStringBetween(CurrentLine, '', '..');
        CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
        CurrentLine := DeleteLeadingDots(CurrentLine);
        Rate := (GetStringBetween(CurrentLine, '', '('));
        BudgetItem := Trim(PreviousLine + ' ' + BudgetItem);
        SearchTerm := 'Cumulative ';
        if Pos(SearchTerm, BudgetItem) = 1 then
        begin
          Delete(BudgetItem, 1, Length(SearchTerm));
        end;
        PreviousLine := '';

        ABudgetItem := TBudgetItem.Create;
        ABudgetItem.Name := BudgetItem;
        ABudgetItem.value := Rate;
        BoundaryConditionBudget.InList.Add(ABudgetItem);

      end;

      Inc(LineIndex);
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    end;

    LineIndex := LineIndex + 1;
    CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    while (CurrentLine = '') do
    begin
      Inc(LineIndex);
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
    end;

    if Pos('Job Completed', CurrentLine) > 0 then
      Exit;
    if Pos('Output at End of Time Step No.', CurrentLine) = 0 then
    begin
      BoundaryConditionBudget := TBudget.Create;
      FBudgetList.Add(BoundaryConditionBudget);

      if Pos('specified T', CurrentLine) > 0 then
      begin
        BoundaryConditionBudget.Zone := 'HST3D Heat Boundaries - Cumulative';
      end
      else
      begin
        BoundaryConditionBudget.Zone := 'HST3D Solute Boundaries - Cumulative';
      end;

      BoundaryConditionBudget.TimeStep := TimeStepString;
      BoundaryConditionBudget.Time := TimeString;

      PreviousLine := '';
      while (CurrentLine <> '') do
      begin
        if Pos('..', CurrentLine) = 0 then
        begin
          PreviousLine := CurrentLine
        end
        else
        begin
          BudgetItem := GetStringBetween(CurrentLine, '', '..');
          CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
          CurrentLine := DeleteLeadingDots(CurrentLine);
          Rate := (GetStringBetween(CurrentLine, '', '('));
          BudgetItem := Trim(PreviousLine + ' ' + BudgetItem);
          SearchTerm := 'Cumulative ';
          if Pos(SearchTerm, BudgetItem) = 1 then
          begin
            Delete(BudgetItem, 1, Length(SearchTerm));
          end;
          PreviousLine := '';

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          BoundaryConditionBudget.InList.Add(ABudgetItem);

        end;

        Inc(LineIndex);
        CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      end;

      LineIndex := LineIndex + 1;
      CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      while (CurrentLine = '') do
      begin
        Inc(LineIndex);
        CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
      end;

      if Pos('Job Completed', CurrentLine) > 0 then
        Exit;
      if Pos('Output at End of Time Step No.', CurrentLine) = 0 then
      begin
        BoundaryConditionBudget := TBudget.Create;
        FBudgetList.Add(BoundaryConditionBudget);

        BoundaryConditionBudget.Zone := 'HST3D Solute Boundaries - Cumulative';

        BoundaryConditionBudget.TimeStep := TimeStepString;
        BoundaryConditionBudget.Time := TimeString;

        PreviousLine := '';
        while (CurrentLine <> '') do
        begin
          if Pos('..', CurrentLine) = 0 then
          begin
            PreviousLine := CurrentLine
          end
          else
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '..');
            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            CurrentLine := DeleteLeadingDots(CurrentLine);
            Rate := (GetStringBetween(CurrentLine, '', '('));
            BudgetItem := Trim(PreviousLine + ' ' + BudgetItem);
            SearchTerm := 'Cumulative ';
            if Pos(SearchTerm, BudgetItem) = 1 then
            begin
              Delete(BudgetItem, 1, Length(SearchTerm));
            end;
            PreviousLine := '';

            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            BoundaryConditionBudget.InList.Add(ABudgetItem);
          end;

          Inc(LineIndex);
          CurrentLine := Trim(ZBLStringList.Strings[LineIndex]);
        end;
      end;
    end;

    SearchTerm := 'Output at End of Time Step No.';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
  end;

end;

procedure TfrmZoneBdgtReader.ReadMT3DFile;
const
  ComponentNumSearchTerm = '>>>FOR COMPONENT NO.';
  ElapsedTimeSearchTerm = 'TOTAL ELAPSED TIME SINCE';
  Mt3dMSBudgetTerm = 'CUMMULATIVE MASS BUDGETS';
  Mt3dUsgsBudgetTerm = 'CUMULATIVE MASS BUDGETS';
var
  SearchTerm: string;
  LineIndex: integer;
  CurrentLine: string;
  TimeStepString, StressPeriodString, TransportStepString, TimeString: string;
  StartLine, StopLine, InnerLineIndex: integer;
  BudgetItem, Rate: string;
  InMinusOut, Discrepancy: string;
  ABudget: TBudget;
  ABudgetItem: TBudgetItem;
  digits: set of Char;
  ComponentNumber: string;
  ComponentCount: Integer;
  ComponentItem: Integer;
  BudgetTerm: string;
  PriorLineIndex: Integer;
begin
  digits := ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'];
  LineIndex := 0;
//  SearchTerm := ElapsedTimeSearchTerm;
  SearchTerm := ComponentNumSearchTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ComponentCount := 0;
  BudgetTerm := Mt3dMSBudgetTerm;
  while LineIndex > -1 do
  begin
    ABudget := TBudget.Create;
    FBudgetList.Add(ABudget);

    CurrentLine := ZBLStringList.Strings[LineIndex];

    ComponentNumber := GetStringBetween(CurrentLine, ComponentNumSearchTerm, '<');
    ComponentItem := StrToInt(ComponentNumber);
    if ComponentItem > ComponentCount then
    begin
      ComponentCount := ComponentItem;
    end;


    SearchTerm := ElapsedTimeSearchTerm;
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    CurrentLine := ZBLStringList.Strings[LineIndex];

    TimeString := GetStringBetween(CurrentLine, '=', '');
    while (Length(TimeString) > 0) and
      not (TimeString[Length(TimeString)] in digits) do
    begin
      TimeString := Trim(Copy(TimeString, 1, Length(TimeString) - 1));
    end;

    SearchTerm := BudgetTerm;
    PriorLineIndex := LineIndex;
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    if (LineIndex < 0) and (BudgetTerm = Mt3dMSBudgetTerm) then
    begin
      BudgetTerm := Mt3dUsgsBudgetTerm;
      SearchTerm := BudgetTerm;
      LineIndex := GetNextLine(SearchTerm, PriorLineIndex);
    end;

    CurrentLine := ZBLStringList.Strings[LineIndex];
    TransportStepString := GetStringBetween(CurrentLine, 'TRANSPORT STEP', ',');

    TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP', ',');

    StressPeriodString := GetStringBetween(CurrentLine, 'STRESS PERIOD', '');

    ABudget.Zone := 'MT3D Cumulative; Component ' + ComponentNumber;
    ABudget.StressPeriod := StressPeriodString;
    ABudget.TimeStep := TimeStepString;
    ABudget.TransportStep := TransportStepString;
    ABudget.Time := TimeString;

    SearchTerm := 'IN';
    LineIndex := GetNextLine(SearchTerm, LineIndex) + 2;

    StartLine := LineIndex;
    SearchTerm := 'TOTAL';
    StopLine := GetNextLine(SearchTerm, StartLine + 1);

    if StopLine > -1 then
    begin
      for InnerLineIndex := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[InnerLineIndex];
        if Pos(':', CurrentLine) > 0 then
        begin
          BudgetItem := GetStringBetween(CurrentLine, '', ':');
          CurrentLine := Copy(CurrentLine, Pos(':', CurrentLine) + 1,
            Length(CurrentLine));
          Rate := Trim(GetNCharacters(CurrentLine, 16));
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.InList.Add(ABudgetItem);

          if Pos('TOTAL', BudgetItem) > 0 then
          begin
            CurrentLine := Copy(CurrentLine, 4, Length(CurrentLine) - 7);
          end;
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Trim(CurrentLine);
          while (Length(ABudgetItem.value) > 0)
            and not (ABudgetItem.value[1] in digits) do
          begin
            ABudgetItem.value := Trim(Copy(ABudgetItem.value, 2,
              Length(ABudgetItem.value)));
          end;
          ABudget.OutList.Add(ABudgetItem);
        end;
      end;
    end;

    if StopLine > -1 then
    begin
      SearchTerm := 'NET (IN - OUT)';
      LineIndex := GetNextLine(SearchTerm, StopLine + 1);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        InMinusOut := GetStringBetween(CurrentLine, ':', '');
        ABudget.InMinusOut := InMinusOut;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'DISCREPANCY';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        Discrepancy := GetStringBetween(CurrentLine, ':', '');
        ABudget.Discrepancy := Discrepancy;
      end;
    end;

//    SearchTerm := ElapsedTimeSearchTerm;
    SearchTerm := ComponentNumSearchTerm;
    LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
  end;
  MakeIncrementalBudget('MT3D Incremental', ComponentCount);
end;

procedure TfrmZoneBdgtReader.MakeIncrementalBudget(ZoneName: string;
  ComponentCount: integer);
var
  ABudget, Budget1, Budget2: TBudget;
  ABudgetItem, AnotherBudgetItem, PreviousBudgetItem: TBudgetItem;
  Limit: integer;
  Index, BudgetItemIndex: integer;
  ComponentLists: TList;
  AComponentList: TList;
  CompIndex: integer;
begin

  ComponentLists := TList.Create;
  try
    for Index := 0 to ComponentCount-1 do
    begin
      AComponentList := TList.Create;
      ComponentLists.Add(AComponentList);
    end;


    for Index := 0 to FBudgetList.Count-1 do
    begin
      AComponentList := ComponentLists[Index mod ComponentCount];
      AComponentList.Add(FBudgetList[Index]);
    end;

    for CompIndex := 0 to ComponentLists.Count -1 do
    begin
      AComponentList := ComponentLists[CompIndex];


      Limit := AComponentList.Count - 1;
      Budget1 := nil;
      for Index := 0 to Limit do
      begin
        Budget2 := AComponentList[Index];
        ABudget := TBudget.Create;
        FBudgetList.Add(ABudget);

        if ComponentCount > 1 then
        begin
          ABudget.Zone := ZoneName + '; Component ' + IntToStr(CompIndex+1);
        end
        else
        begin
          ABudget.Zone := ZoneName;
        end;

        ABudget.StressPeriod := Budget2.StressPeriod;
        ABudget.TimeStep := Budget2.TimeStep;
        ABudget.TransportStep := Budget2.TransportStep;
        ABudget.Time := Budget2.Time;

        for BudgetItemIndex := 0 to Budget2.InList.Count - 1 do
        begin
          AnotherBudgetItem := Budget2.InList[BudgetItemIndex];
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := AnotherBudgetItem.Name;
          if Budget1 = nil then
          begin
            ABudgetItem.value := AnotherBudgetItem.value;
          end
          else
          begin
            PreviousBudgetItem := Budget1.InList[BudgetItemIndex];
            ABudgetItem.value :=
              FloatToStr(InternationalStrToFloat(AnotherBudgetItem.value)
              - InternationalStrToFloat(PreviousBudgetItem.value));
          end;
          ABudget.InList.Add(ABudgetItem);
        end;

        for BudgetItemIndex := 0 to Budget2.OutList.Count - 1 do
        begin
          AnotherBudgetItem := Budget2.OutList[BudgetItemIndex];
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := AnotherBudgetItem.Name;
          if Budget1 = nil then
          begin
            ABudgetItem.value := AnotherBudgetItem.value;
          end
          else
          begin
            PreviousBudgetItem := Budget1.OutList[BudgetItemIndex];
            ABudgetItem.value :=
              FloatToStr(InternationalStrToFloat(AnotherBudgetItem.value)
              - InternationalStrToFloat(PreviousBudgetItem.value));
          end;
          ABudget.OutList.Add(ABudgetItem);
        end;

        if Budget1 = nil then
        begin
          ABudget.InMinusOut := Budget2.InMinusOut;
          ABudget.Discrepancy := Budget2.Discrepancy;
        end
        else
        begin
          ABudget.InMinusOut :=
            FloatToStr(InternationalStrToFloat(Budget2.InMinusOut)
            - InternationalStrToFloat(Budget1.InMinusOut));
          ABudget.Discrepancy :=
            FloatToStr(InternationalStrToFloat(Budget2.Discrepancy)
            - InternationalStrToFloat(Budget1.Discrepancy));
        end;
        Budget1 := Budget2;
      end;
    end;
  finally
    for Index := ComponentLists.Count -1 downto 0 do
    begin
      AComponentList := ComponentLists[Index];
      AComponentList.Free;
    end;
    ComponentLists.Free;
  end;
end;

procedure TfrmZoneBdgtReader.ReadMOC3DFile;
var
  SearchTerm: string;
  LineIndex: integer;
  CurrentLine: string;
  TimeStepString, StressPeriodString, TransportStepString, TimeString: string;
  StoredMass: string;
  StartLine, StopLine, InnerLineIndex: integer;
  BudgetItem, Rate: string;
  InMinusOut, Discrepancy, Decay: string;
  ABudget: TBudget;
  ABudgetItem: TBudgetItem;
begin
  LineIndex := 0;
  SearchTerm := 'SOLUTE BUDGET';
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  while LineIndex > -1 do
  begin
    ABudget := TBudget.Create;
    FBudgetList.Add(ABudget);

    SearchTerm := 'STRESS PERIOD';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    CurrentLine := ZBLStringList.Strings[LineIndex];
    StressPeriodString := GetStringBetween(CurrentLine, SearchTerm, 'OUT OF');

    SearchTerm := 'FLOW TIME STEP';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    CurrentLine := ZBLStringList.Strings[LineIndex];
    TimeStepString := GetStringBetween(CurrentLine, SearchTerm, 'OUT OF');

    SearchTerm := 'TRANSPORT TIME INCREMENT';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    CurrentLine := ZBLStringList.Strings[LineIndex];
    TransportStepString := GetStringBetween(CurrentLine, SearchTerm, 'OUT OF');

    SearchTerm := 'ELAPSED TIME';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    CurrentLine := ZBLStringList.Strings[LineIndex];
    TimeString := GetStringBetween(CurrentLine, '=', '');

    SearchTerm := 'CHANGE IN MASS STORED';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    CurrentLine := ZBLStringList.Strings[LineIndex];
    StoredMass := GetStringBetween(CurrentLine, '=', '');

    ABudget.Zone := 'MOC3D Cumulative';
    ABudget.StressPeriod := StressPeriodString;
    ABudget.TimeStep := TimeStepString;
    ABudget.TransportStep := TransportStepString;
    ABudget.Time := TimeString;
    ABudget.StoredMass := StoredMass;

    SearchTerm := 'CUMULATIVE SOLUTE MASS';
    LineIndex := GetNextLine(SearchTerm, LineIndex) + 1;
    SearchTerm := 'IN';
    LineIndex := GetNextLine(SearchTerm, LineIndex) + 1;

    StartLine := LineIndex + 1;
    SearchTerm := 'OUT:';
    StopLine := GetNextLine(SearchTerm, StartLine + 1);

    if StopLine > -1 then
    begin
      for InnerLineIndex := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[InnerLineIndex];
        if Pos('=', CurrentLine) > 0 then
        begin

          BudgetItem := GetStringBetween(CurrentLine, '', '=');
          Rate := GetStringBetween(CurrentLine, '=', '');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.InList.Add(ABudgetItem);
        end;
      end;
    end;
    if StopLine > -1 then
    begin
      StartLine := StopLine + 1;
      SearchTerm := 'SOURCE-TERM DECAY';
      StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;
      if StopLine > -1 then
      begin
        for InnerLineIndex := StartLine to StopLine do
        begin
          CurrentLine := ZBLStringList.Strings[InnerLineIndex];
          if Pos('=', CurrentLine) > 0 then
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '=');
            Rate := GetStringBetween(CurrentLine, '=', '');
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            ABudget.OutList.Add(ABudgetItem);
          end;
        end;
      end;
    end;

    if StopLine > -1 then
    begin
      SearchTerm := 'SOURCE-TERM DECAY';
      LineIndex := GetNextLine(SearchTerm, StopLine + 1);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        Decay := GetStringBetween(CurrentLine, '=', '');
        ABudget.Decay := Decay;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'RESIDUAL';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        InMinusOut := GetStringBetween(CurrentLine, '=', '');
        ABudget.InMinusOut := InMinusOut;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'PERCENT DISCREPANCY';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        SearchTerm := 'RELATIVE';
        if Pos(SearchTerm, CurrentLine) > 0 then
        begin
          Discrepancy := GetStringBetween(CurrentLine, '=', SearchTerm);
        end
        else
        begin
          Discrepancy := GetStringBetween(CurrentLine, '=', '');
        end;
        ABudget.Discrepancy := Discrepancy;
      end;
    end;
    SearchTerm := 'SOLUTE BUDGET';
    LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
  end;

  MakeIncrementalBudget('MOC3D Incremental', 1);
end;

procedure TfrmZoneBdgtReader.ReadZonebdgtFile;
var
  SearchTerm: string;
  LineIndex: integer;
  CurrentLine: string;
  CurrentZone: string;
  TimeStepString, StressPeriodString: string;
  StartLine, StopLine, InnerLineIndex: integer;
  BudgetItem, Rate: string;
  InMinusOut, Discrepancy: string;
  ABudget , PriorBudget, CumBudget: TBudget;
  ABudgetItem, CumBudgetItem, PriorBudgetItem, TempItem: TBudgetItem;
  BudgetGroups: TStringList;
  AList: TList;
  BudgetPos: integer;
  MultiplesFound: boolean;
  GroupIndex: integer;
  BudgetIndex: integer;
  BudgetItemIndex, InnerIndex: integer;
  Index: integer;
  TotalIn, TotalOut, Total: double;
  FileName: string;
  Success: longbool;
  Dir: string;
  OldFilter: string;
  PLen: single;
  N: longint;
  Mult: single;
  I: longint;
  SP, TimeStep: integer;
  FirstFound: boolean;
  CumulativeTime: double;
  IsModflow6: Boolean;
begin
  FirstFound := False;
  LineIndex := 0;
  IsModflow6 := False;
  if (ZBLStringList.Count > 0) and (Trim(ZBLStringList[0]) = 'ZONEBUDGET Version 6') then
  begin
    IsModflow6 := True
  end;
  if IsModflow6 then
  begin
    ReadZonebdgt6File;
    Exit;
  end;


  SearchTerm := 'Flow Budget for ';
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  while LineIndex > -1 do
  begin
    ABudget := TBudget.Create;
    FBudgetList.Add(ABudget);
    CurrentLine := ZBLStringList.Strings[LineIndex];

    CurrentZone := GetStringBetween(CurrentLine, SearchTerm, 'at');
    CurrentLine := GetStringBetween(CurrentLine, CurrentZone, '');

    TimeStepString := GetStringBetween(CurrentLine, 'Time Step', 'of');
    CurrentLine := GetStringBetween(CurrentLine, TimeStepString, '');

    StressPeriodString := GetStringBetween(CurrentLine, 'Stress Period', '');

    ABudget.Zone := CurrentZone;
    ABudget.StressPeriod := StressPeriodString;
    ABudget.TimeStep := TimeStepString;

    StartLine := LineIndex + 1;
    SearchTerm := 'OUT:';
    StopLine := GetNextLine(SearchTerm, StartLine + 1);

    if StopLine > -1 then
    begin
      for InnerLineIndex := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[InnerLineIndex];
        if Pos('=', CurrentLine) > 0 then
        begin

          BudgetItem := GetStringBetween(CurrentLine, '', '=');
          Rate := GetStringBetween(CurrentLine, '=', '');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.InList.Add(ABudgetItem);
        end;
      end;
    end;
    if StopLine > -1 then
    begin
      StartLine := StopLine + 1;
      SearchTerm := 'IN - OUT';
      StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;
      if StopLine > -1 then
      begin
        for InnerLineIndex := StartLine to StopLine do
        begin
          CurrentLine := ZBLStringList.Strings[InnerLineIndex];
          if Pos('=', CurrentLine) > 0 then
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '=');
            Rate := GetStringBetween(CurrentLine, '=', '');
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            ABudget.OutList.Add(ABudgetItem);
          end;
        end;
      end;
    end;

    if StopLine > -1 then
    begin
      SearchTerm := 'IN - OUT';
      LineIndex := GetNextLine(SearchTerm, StopLine + 1);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        InMinusOut := GetStringBetween(CurrentLine, '=', '');
        ABudget.InMinusOut := InMinusOut;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'Percent Discrepancy';
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        Discrepancy := GetStringBetween(CurrentLine, '=', '');
        ABudget.Discrepancy := Discrepancy;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'Flow Budget for ';
      LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
    end;
  end;

  BudgetGroups := TStringList.Create;
  try
    MultiplesFound := False;
    for Index := 0 to FBudgetList.Count -1 do
    begin
      ABudget := FBudgetList[Index];
      BudgetPos := BudgetGroups.IndexOf(ABudget.Zone);
      if BudgetPos < 0 then
      begin
        AList := TList.Create;
        BudgetGroups.AddObject(ABudget.Zone, AList);
      end
      else
      begin
        AList := BudgetGroups.Objects[BudgetPos] as TList;
        MultiplesFound := True;
      end;
      AList.Add(ABudget);
    end;
    if MultiplesFound then
    begin
      Dir := GetCurrentDir;
      try
        if frmSelectDiscretization.ShowModal = mrOK then
        begin
          FileName := frmSelectDiscretization.framDisFile.edFilePath.Text;
          SetCurrentDir(ExtractFileDir(FileName));
          FileName := ExtractFileName(FileName);
          Success := False;
          try
            ReadStressPeriods(Success, NPER, PChar(FileName), Length(FileName));
            if Success then
            begin
              CumulativeTime := 0;
              SetLength(PERLEN,NPER);
              SetLength(NSTEP,NPER);
              SetLength(TSMULT,NPER);
              SetLength(Used,NPER);
              for Index := 0 to NPER-1 do
              begin
                I := Index + 1;
                GetStressPeriod(I, PLen, N, Mult);
                PERLEN[Index] := PLen;
                NSTEP[Index] := N;
                TSMULT[Index] := Mult;
                SetLength(Used[Index],N);
                for I := 0 to N-1 do
                begin
                  CumulativeTime := CumulativeTime + TimeStepLength(Index+1, I+1);
                  Used[Index,I].IsUsed := False;
                  Used[Index,I].Time := CumulativeTime;
                end;
              end;
            end;
          finally
            DeAllocateStressPeriods;
          end;

          for GroupIndex := 0 to BudgetGroups.Count -1 do
          begin
            AList := BudgetGroups.Objects[GroupIndex] as TList;
            for BudgetIndex := 0 to AList.Count -1 do
            begin
              ABudget := AList[BudgetIndex];
              SP := ABudget.GetStressPeriod -1;
              TimeStep := ABudget.GetTimeStep -1;
              if (SP >= 0) and (TimeStep >= 0)
                and (SP < Length(Used)) and (TimeStep <= Length(Used[SP])) then
              begin
                Used[SP,TimeStep].IsUsed := True;
                ABudget.Time := FloatToStr(Used[SP,TimeStep].Time);
              end
              else
              begin
                Beep;
                MessageDlg('Invalid stress period or time step; cumulative '
                  + 'volumes can not be calculated.', mtError, [mbOK], 0);
                Exit;
              end;
            end;
          end;

          for SP := 0 to NPER -1 do
          begin
            for TimeStep := 0 to NSTEP[SP] -1 do
            begin
              if Used[SP,TimeStep].IsUsed then
              begin
                if FirstFound then
                begin
                  Beep;
                  MessageDlg('Incomplete budget data; cumulative '
                    + 'volumes can not be calculated.', mtError, [mbOK], 0);
                  Exit;
                end;
              end
              else
              begin
                FirstFound := true;
              end;
            end;
          end;


          for GroupIndex := 0 to BudgetGroups.Count -1 do
          begin
            AList := BudgetGroups.Objects[GroupIndex] as TList;
            CumBudget := nil;
            for BudgetIndex := 0 to AList.Count -1 do
            begin
              PriorBudget := CumBudget;
              ABudget := AList[BudgetIndex];
              CumBudget:= TBudget.Create;
              FBudgetList.Add(CumBudget);
              CumBudget.Zone := ABudget.Zone + ': Cumulative';
              CumBudget.StressPeriod := ABudget.StressPeriod;
              CumBudget.TimeStep := ABudget.TimeStep;
              CumBudget.Time := ABudget.Time;
              TotalIn := 0;
              for BudgetItemIndex := 0 to ABudget.InList.Count -1 do
              begin
                ABudgetItem := ABudget.InList[BudgetItemIndex];
                PriorBudgetItem := nil;
                if PriorBudget <> nil then
                begin
                  for InnerIndex := 0 to PriorBudget.InList.Count -1 do
                  begin
                    TempItem := PriorBudget.InList[InnerIndex];
                    if ABudgetItem.Name = TempItem.Name then
                    begin
                      PriorBudgetItem := TempItem;
                      break;
                    end;
                  end;
                end;
                CumBudgetItem := TBudgetItem.Create;
                CumBudget.InList.Add(CumBudgetItem);
                CumBudgetItem.Name := ABudgetItem.Name;
                if PriorBudgetItem = nil then
                begin
                  CumBudgetItem.value := FloatToStr(ABudgetItem.RealValue
                    * TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep));
                end
                else
                begin
                  CumBudgetItem.value := FloatToStr(ABudgetItem.RealValue
                    * TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep) + PriorBudgetItem.RealValue);
                end;
                if Pos('Total', CumBudgetItem.Name) > 0  then
                begin
                  TotalIn := CumBudgetItem.RealValue;
                    //* TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep);
                end;

              end;
              TotalOut := 0;
              for BudgetItemIndex := 0 to ABudget.OutList.Count -1 do
              begin
                ABudgetItem := ABudget.OutList[BudgetItemIndex];
                PriorBudgetItem := nil;
                if PriorBudget <> nil then
                begin
                  for InnerIndex := 0 to PriorBudget.OutList.Count -1 do
                  begin
                    TempItem := PriorBudget.OutList[InnerIndex];
                    if ABudgetItem.Name = TempItem.Name then
                    begin
                      PriorBudgetItem := TempItem;
                      break;
                    end;
                  end;
                end;
                CumBudgetItem := TBudgetItem.Create;
                CumBudget.OutList.Add(CumBudgetItem);
                CumBudgetItem.Name := ABudgetItem.Name;
                if PriorBudgetItem = nil then
                begin
                  CumBudgetItem.value := FloatToStr(ABudgetItem.RealValue
                    * TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep));
                end
                else
                begin
                  CumBudgetItem.value := FloatToStr(ABudgetItem.RealValue
                    * TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep) + PriorBudgetItem.RealValue);
                end;
                if Pos('Total', CumBudgetItem.Name) > 0  then
                begin
                  TotalOut := CumBudgetItem.RealValue;
                    //* TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep);
                end;
              end;

              if PriorBudget = nil then
              begin
                CumBudget.InMinusOut := FloatToStr(ABudget.GetInMinusOut
                    * TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep));
                CumBudget.Discrepancy := FloatToStr(ABudget.GetDiscrepancy
                    * TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep));
              end
              else
              begin
                CumBudget.InMinusOut := FloatToStr(PriorBudget.GetInMinusOut
                   + ABudget.GetInMinusOut* TimeStepLength(CumBudget.GetStressPeriod, CumBudget.GetTimeStep));
                Total := (Abs(TotalIn) + Abs(TotalOut))/2;
                if Total <> 0 then
                begin
                  CumBudget.Discrepancy := FloatToStr(CumBudget.GetInMinusOut/Total*100);
                end;

              end;
            end;
          end;
        end;
      finally
        SetCurrentDir(Dir);
      end;
    end;
  finally
    for Index := 0 to BudgetGroups.Count -1 do
    begin
      AList := BudgetGroups.Objects[Index] as TList;
      AList.Free;
    end;
    BudgetGroups.Free;
  end;
end;

procedure TfrmZoneBdgtReader.ReadSutraFile;
var
  SearchTerm: string;
  LineIndex: integer;
  CurrentLine: string;
  TimeStepString: string;
  StartLine, StopLine, InnerLineIndex: integer;
  BudgetItem, Rate: string;
  ABudget: TBudget;
  ABudgetItem: TBudgetItem;
  ReplacementsNames: TStringList;
  ReplacementIndex: Integer;
begin
  // Sutra 97
  ReplacementsNames := TStringList.Create;
  try
    begin
      ReplacementsNames.Add('NET RATE OF INCREASE(+)/DECREASE(-) OF SOLUTE DUE TO CONCENTRATION CHANGE');
      ReplacementsNames.Add('Solute Change due to Concentration Change');

      ReplacementsNames.Add('NET RATE OF INCREASE(+)/DECREASE(-) OF ADSORBATE');
      ReplacementsNames.Add('Adsorbate Change');

      ReplacementsNames.Add('NET RATE OF INCREASE(+)/DECREASE(-) OF SOLUTE DUE TO CHANGE IN MASS OF FLUID');
      ReplacementsNames.Add('Solute Change due to Change in Fluid Mass');

      ReplacementsNames.Add('NET FIRST-ORDER PRODUCTION(+)/DECAY(-) OF SOLUTE');
      ReplacementsNames.Add('1''st-Order Production of Solute');

      ReplacementsNames.Add('NET FIRST-ORDER PRODUCTION(+)/DECAY(-) OF ADSORBATE');
      ReplacementsNames.Add('1''st-Order Production of Adsorbate');

      ReplacementsNames.Add('NET ZERO-ORDER PRODUCTION(+)/DECAY(-) OF SOLUTE');
      ReplacementsNames.Add('Zero-Order Production of Solute');

      ReplacementsNames.Add('NET ZERO-ORDER PRODUCTION(+)/DECAY(-) OF ADSORBATE');
      ReplacementsNames.Add('Zero-Order Production of Adsorbate');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-) OF SOLUTE THROUGH FLUID SOURCES AND SINKS');
      ReplacementsNames.Add('Gain of Solute through Fluid Sources and Sinks');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-) OF SOLUTE THROUGH INFLOWS OR OUTFLOWS AT POINTS OF SPECIFIED PRESSURE');
      ReplacementsNames.Add('Gain of Solute through Flows at Specified Pressure Points');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-) OF SOLUTE THROUGH SOLUTE SOURCES AND SINKS');
      ReplacementsNames.Add('Gain of Solute through Solute Sources');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-)'' OF SOLUTE AT POINTS OF SPECIFIED CONCENTRATION');
      ReplacementsNames.Add('Gain of Solute at Specified Concentration Points');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-) OF SOLUTE AT POINTS OF SPECIFIED CONCENTRATION');
      ReplacementsNames.Add('Gain of Solute at Specified Concentration Points');

      ReplacementsNames.Add('RATE OF CHANGE IN TOTAL STORED FLUID DUE TO PRESSURE CHANGE, INCREASE(+)/DECREASE(-)');
      ReplacementsNames.Add('Change in Fluid due to Pressure Change');

      ReplacementsNames.Add('RATE OF CHANGE IN TOTAL STORED FLUID DUE TO CONCENTRATION CHANGE, INCREASE(+)/DECREASE(-)');
      ReplacementsNames.Add('Change in Fluid due to Concentration Change');

      ReplacementsNames.Add('TOTAL OF FLUID SOURCES AND SINKS, NET INFLOW(+)/NET OUTFLOW(-)');
      ReplacementsNames.Add('Total of Fluid Sources and Sinks');

      ReplacementsNames.Add('TOTAL OF FLUID FLOWS AT POINTS OF SPECIFIED PRESSURE, NET INFLOW(+)/NET OUTFLOW(-)');
      ReplacementsNames.Add('Total of Fluid Flows at Points of Specified Pressure');

      ReplacementsNames.Add('RATE OF CHANGE IN TOTAL STORED FLUID DUE TO  TEMPERATURE  CHANGE, INCREASE(+)/DECREASE(-)');
      ReplacementsNames.Add('Change in Fluid due to Temperature Change');

      ReplacementsNames.Add('NET RATE OF INCREASE(+)/DECREASE(-) OF ENERGY IN FLUID DUE TO TEMPERATURE CHANGE');
      ReplacementsNames.Add('Energy Change in Fluid due to Temperature Change');

      ReplacementsNames.Add('NET RATE OF INCREASE(+)/DECREASE(-) OF ENERGY IN SOLID GRAINS');
      ReplacementsNames.Add('Energy Change in Solid Grains');

      ReplacementsNames.Add('NET RATE OF INCREASE(+)/DECREASE(-) OF ENERGY IN FLUID DUE TO CHANGE IN MASS OF FLUID');
      ReplacementsNames.Add('Energy Change in Fluid due to Fluid Mass Change');

      ReplacementsNames.Add('NET ZERO-ORDER PRODUCTION(+)/LOSS(-) OF ENERGY IN FLUID');
      ReplacementsNames.Add('Zero-Order Production/Loss of Energy in Fluid');

      ReplacementsNames.Add('NET ZERO-ORDER PRODUCTION(+)/LOSS(-) OF ENERGY IN SOLID GRAINS');
      ReplacementsNames.Add('Zero-Order Production/Loss of Energy in Solid Grains');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-) OF ENERGY THROUGH FLUID SOURCES AND SINKS');
      ReplacementsNames.Add('Energy Gain/Loss through Fluid Sources and Sinks');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-) OF ENERGY THROUGH INFLOWS OR OUTFLOWS AT POINTS OF SPECIFIED PRESSURE');
      ReplacementsNames.Add('Energy Gain/Loss through flows at Specified Pressure Points');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-) OF ENERGY THROUGH ENERGY SOURCES AND SINKS');
      ReplacementsNames.Add('Energy Gain/Loss through Energy Sources and Sinks');

      ReplacementsNames.Add('NET GAIN(+)''/LOSS(-) OF ENERGY THROUGH POINTS OF SPECIFIED TEMPERATURE');
      ReplacementsNames.Add('Energy Gain/Loss through Points of Specified Temperature');

      ReplacementsNames.Add('NET GAIN(+)/LOSS(-) OF ENERGY THROUGH POINTS OF SPECIFIED TEMPERATURE');
      ReplacementsNames.Add('Energy Gain/Loss through Points of Specified Temperature');

      LineIndex := 0;
      SearchTerm := 'S O L U T E   B U D G E T';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      while LineIndex > -1 do
      begin
        ABudget := TBudget.Create;
        FBudgetList.Add(ABudget);
        CurrentLine := ZBLStringList.Strings[LineIndex];

        TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP', ',');

        ABudget.Zone := 'SUTRA Solute Budget';
        ABudget.TimeStep := TimeStepString;

        StartLine := LineIndex + 1;
        SearchTerm := 'SOLUTE SOURCES OR SINKS';
        StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;

        if StopLine > -1 then
        begin
          for InnerLineIndex := StartLine to StopLine do
          begin
            CurrentLine := ZBLStringList.Strings[InnerLineIndex];
            if Pos('NET', CurrentLine) > 0 then
            begin

              Rate := GetStringBetween(CurrentLine, '', 'NET');
              BudgetItem := GetStringBetween(CurrentLine, Rate, '');
              ABudgetItem := TBudgetItem.Create;
              ReplacementIndex := ReplacementsNames.IndexOf(BudgetItem);
              if ReplacementIndex > -1 then
              begin
                BudgetItem := ReplacementsNames[ReplacementIndex + 1];
              end;
              ABudgetItem.Name := BudgetItem;
              ABudgetItem.value := Rate;
              ABudget.InList.Add(ABudgetItem);
            end;
          end;
        end;
        SearchTerm := 'S O L U T E   B U D G E T';
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
      end;

      LineIndex := 0;
      SearchTerm := 'F L U I D   M A S S   B U D G E T';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      while LineIndex > -1 do
      begin
        ABudget := TBudget.Create;
        FBudgetList.Add(ABudget);
        CurrentLine := ZBLStringList.Strings[LineIndex];

        TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP', ',');

        ABudget.Zone := 'SUTRA Mass Budget';
        ABudget.TimeStep := TimeStepString;

        StartLine := LineIndex + 1;
        SearchTerm := 'FLUID SOURCES OR SINKS';
        StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;

        if StopLine > -1 then
        begin
          for InnerLineIndex := StartLine to StopLine do
          begin
            CurrentLine := ZBLStringList.Strings[InnerLineIndex];
            if Pos('RATE OF', CurrentLine) > 0 then
            begin
              Rate := GetStringBetween(CurrentLine, '', 'RATE OF');
              BudgetItem := GetStringBetween(CurrentLine, Rate, '');
              ABudgetItem := TBudgetItem.Create;
              ReplacementIndex := ReplacementsNames.IndexOf(BudgetItem);
              if ReplacementIndex > -1 then
              begin
                BudgetItem := ReplacementsNames[ReplacementIndex + 1];
              end;
              ABudgetItem.Name := BudgetItem;
              ABudgetItem.value := Rate;
              ABudget.InList.Add(ABudgetItem);
            end;
            if Pos('TOTAL OF', CurrentLine) > 0 then
            begin

              Rate := GetStringBetween(CurrentLine, '', 'TOTAL OF');
              BudgetItem := GetStringBetween(CurrentLine, Rate, '');
              ABudgetItem := TBudgetItem.Create;
              ReplacementIndex := ReplacementsNames.IndexOf(BudgetItem);
              if ReplacementIndex > -1 then
              begin
                BudgetItem := ReplacementsNames[ReplacementIndex + 1];
              end;
              ABudgetItem.Name := BudgetItem;
              ABudgetItem.value := Rate;
              ABudget.InList.Add(ABudgetItem);
            end;
          end;
        end;
        SearchTerm := 'F L U I D   M A S S   B U D G E T';
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
      end;

      LineIndex := 0;
      SearchTerm := 'E N E R G Y   B U D G E T';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      while LineIndex > -1 do
      begin
        ABudget := TBudget.Create;
        FBudgetList.Add(ABudget);
        CurrentLine := ZBLStringList.Strings[LineIndex];

        TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP', ',');

        ABudget.Zone := 'SUTRA Energy Budget';
        ABudget.TimeStep := TimeStepString;

        StartLine := LineIndex + 1;
        SearchTerm := 'ENERGY SOURCES OR SINKS';
        StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;

        if StopLine > -1 then
        begin
          for InnerLineIndex := StartLine to StopLine do
          begin
            CurrentLine := ZBLStringList.Strings[InnerLineIndex];
            if Pos('NET', CurrentLine) > 0 then
            begin

              Rate := GetStringBetween(CurrentLine, '', 'NET');
              BudgetItem := GetStringBetween(CurrentLine, Rate, '');
              ABudgetItem := TBudgetItem.Create;
              ReplacementIndex := ReplacementsNames.IndexOf(BudgetItem);
              if ReplacementIndex > -1 then
              begin
                BudgetItem := ReplacementsNames[ReplacementIndex + 1];
              end;
              ABudgetItem.Name := BudgetItem;
              ABudgetItem.value := Rate;
              ABudget.InList.Add(ABudgetItem);
            end;
          end;
        end;
        SearchTerm := 'E N E R G Y   B U D G E T';
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
      end;
    end;
  finally
    begin
      ReplacementsNames.Free;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.ReadSutra2D3D_File;
const
  S1 = 'RATE OF CHANGE IN SOLUTE DUE TO CONCENTRATION CHANGE';
  S2 = 'RATE OF CHANGE OF ADSORBATE';
  S3 = 'RATE OF CHANGE IN SOLUTE DUE TO CHANGE IN MASS OF FLUID';
  S4 = 'TOTAL RATE OF CHANGE OF SOLUTE [ S+, S-, S ]';
  S5 = 'FIRST-ORDER PRODUCTION/DECAY OF SOLUTE';
  S6 = 'FIRST-ORDER PRODUCTION/DECAY OF ADSORBATE';
  S7 = 'ZERO-ORDER PRODUCTION/DECAY OF SOLUTE';
  S8 = 'ZERO-ORDER PRODUCTION/DECAY OF ADSORBATE';
  S9 = 'TOTAL RATE OF PRODUCTION/DECAY OF SOLUTE AND ADSORBATE [ P+, P-, P ]';
  S10 = 'GAIN/LOSS OF SOLUTE THROUGH FLUID SOURCES AND SINKS';
  S11 = 'GAIN/LOSS OF SOLUTE THROUGH INFLOWS/OUTFLOWS AT SPECIFIED P NODES';
  S12 = 'GAIN/LOSS OF SOLUTE THROUGH SOLUTE SOURCES AND SINKS';
  S13 = 'GAIN/LOSS OF SOLUTE AT SPECIFIED CONCENTRATION NODES';
  S14 = 'TOTAL RATE OF GAIN/LOSS OF SOLUTE';
  S15 =
    'SOLUTE MASS BAL. ACTIVITY [ A = ((S+) - (S-) + (P+) - (P-) + (F+) - (F-))/2 ]';
  S16 = 'ABSOLUTE SOLUTE MASS BALANCE ERROR [ S - P - F ]';
  S17 = 'RELATIVE SOLUTE MASS BALANCE ERROR [ 100*(S - P - F)/A ]';
  S18 = 'RATE OF CHANGE IN GW SOLUTE DUE TO CONCENTRATION CHANGE';
  S19 = 'RATE OF CHANGE IN GW SOLUTE DUE TO CHANGE IN MASS OF FLUID';
  S20 = 'TOTAL RATE OF CHANGE OF GW SOLUTE [ S+, S-, S ]';
  S21 = 'FIRST-ORDER PRODUCTION/DECAY OF GW SOLUTE';
  S22 = 'ZERO-ORDER PRODUCTION/DECAY OF GW SOLUTE';
  S23 = 'TOTAL RATE OF PRODUCTION/DECAY OF GW SOLUTE & ADSORBATE [ P+, P-, P ]';
  S24 = 'GAIN/LOSS OF GW SOLUTE THROUGH FLUID SOURCES AND SINKS';
  S25 = 'GAIN/LOSS OF GW SOLUTE THROUGH INFLOWS/OUTFLOWS AT SPECIFIED P NODES';
  S26 = 'GAIN/LOSS OF GW SOLUTE THROUGH INFLOWS/OUTFLOWS AT GEN.-FLOW NODES';
  S27 = 'GAIN/LOSS OF GW SOLUTE THROUGH SOLUTE SOURCES AND SINKS';
  S28 = 'GAIN/LOSS OF GW SOLUTE AT SPECIFIED CONCENTRATION NODES';
  S29 = 'GAIN/LOSS OF GW SOLUTE AT GEN.-TRANSPORT NODES';
  S30 = 'TOTAL RATE OF GAIN/LOSS OF GW SOLUTE';
  S31 = 'GW SOL. MASS BAL. ACTIVITY [ A = ((S+) - (S-) + (P+) - (P-) + (F+) - (F-))/2 ]';
  S32 = 'ABSOLUTE GW SOLUTE MASS BALANCE ERROR [ S - P - F ]';
  S33 = 'RELATIVE GW SOLUTE MASS BALANCE ERROR [ 100*(S - P - F)/A ]';

  F1 =  'RATE OF CHANGE IN TOTAL STORED FLUID DUE TO PRESSURE CHANGE';
  F1Alt = 'RATE OF CHANGE IN TOTAL STORED GROUNDWATER DUE TO PRESSURE CHANGE';
  F2a = 'RATE OF CHANGE IN TOTAL STORED FLUID DUE TO CONCENTRATION CHANGE';
  F2aAlt = 'RATE OF CHANGE IN TOTAL STORED GROUNDWATER DUE TO CONCENTRATION CHANGE';
  F2b = 'RATE OF CHANGE IN TOTAL STORED FLUID DUE TO  TEMPERATURE  CHANGE';
  F2bAlt = 'RATE OF CHANGE IN TOTAL STORED GROUNDWATER DUE TO TEMPERATURE CHANGE';
  F3 = 'TOTAL RATE OF CHANGE IN STORED FLUID [ S+, S-, S ]';
  F3Alt = 'TOTAL RATE OF CHANGE IN STORED GROUNDWATER [ S+, S-, S ]';
  F4 = 'GAIN/LOSS OF FLUID THROUGH FLUID SOURCES AND SINKS';
  F4Alt = 'GAIN/LOSS OF GROUNDWATER THROUGH FLUID SOURCES AND SINKS';
  F5 = 'GAIN/LOSS OF FLUID THROUGH INFLOWS/OUTFLOWS AT SPECIFIED P NODES';
  F5Alt = 'GAIN/LOSS OF GROUNDWATER THROUGH INFLOWS/OUTFLOWS AT SPECIFIED P NODES';
  F6 = 'TOTAL RATE OF GAIN/LOSS OF FLUID THROUGH FLOWS [ F+, F-, F ]';
  F6Alt = 'TOTAL RATE OF GAIN/LOSS OF GROUNDWATER THROUGH FLOWS [ F+, F-, F ]';
  F7 = 'FLUID MASS BALANCE ACTIVITY [ A = ((S+) - (S-) + (F+) - (F-))/2 ]';
  F7Alt = 'GROUNDWATER MASS BALANCE ACTIVITY [ A = ((S+) - (S-) + (F+) - (F-))/2 ]';
  F8 = 'ABSOLUTE FLUID MASS BALANCE ERROR [ S - F ]';
  F8Alt = 'ABSOLUTE GROUNDWATER MASS BALANCE ERROR [ S - F ]';
  F9 = 'RELATIVE FLUID MASS BALANCE ERROR [ 100*(S - F)/A ]';
  F9Alt = 'RELATIVE GROUNDWATER MASS BALANCE ERROR [ 100*(S - F)/A ]';

  F10 = 'GAIN/LOSS OF FLUID THROUGH INFLOWS/OUTFLOWS AT GEN.-FLOW NODES';
  F10Alt = 'GAIN/LOSS OF GROUNDWATER THROUGH INFLOWS/OUTFLOWS AT GEN.-FLOW NODES';
  F11 = 'GAIN/LOSS OF FLUID THROUGH INFLOWS/OUTFLOWS AT LAKE NODES';
  F11Alt = 'GAIN/LOSS OF GROUNDWATER THROUGH INFLOWS/OUTFLOWS AT LAKE NODES';

  E1 = 'RATE OF CHANGE OF ENERGY IN FLUID DUE TO TEMPERATURE CHANGE';
  E1Alt = 'RATE OF CHANGE OF ENERGY IN GW DUE TO TEMPERATURE CHANGE';
  E2 = 'RATE OF CHANGE OF ENERGY IN SOLID GRAINS';
  E3 = 'RATE OF CHANGE OF ENERGY DUE TO CHANGE IN MASS OF FLUID';
  E3Alt = 'RATE OF CHANGE OF ENERGY IN GW & SOLID GRAINS DUE TO CHANGE';
  E4 = 'TOTAL RATE OF CHANGE OF ENERGY [ S+, S-, S ]';
  E4Alt = 'TOTAL RATE OF CHANGE OF ENERGY IN GW & SOLID GRAINS [ S+, S-, S ]';
  E5 = 'ZERO-ORDER PRODUCTION/DECAY OF ENERGY IN FLUID';
  E5Alt = 'ZERO-ORDER PRODUCTION/DECAY OF ENERGY IN GW';
  E6 = 'ZERO-ORDER PRODUCTION/DECAY OF ENERGY IN SOLID GRAINS';
  E7 = 'TOTAL RATE OF PRODUCTION/DECAY OF ENERGY [ P+, P-, P ]';
  E7Alt = 'TOTAL RATE OF PROD./DECAY OF ENERGY IN GW & SOLID GRAINS [ P+, P-, P ]';
  E8 = 'GAIN/LOSS OF ENERGY THROUGH FLUID SOURCES AND SINKS';
  E8Alt = 'GAIN/LOSS OF ENERGY IN GW THROUGH FLUID SOURCES AND SINKS';
  E9 = 'GAIN/LOSS OF ENERGY THROUGH INFLOWS/OUTFLOWS AT SPECIFIED P NODES';
  E9Alt = 'GAIN/LOSS OF ENERGY IN GW THROUGH INFLOWS/OUTFLOWS AT SPEC. P NODES';
  E10 = 'GAIN/LOSS OF ENERGY THROUGH ENERGY SOURCES AND SINKS';
  E10Alt = 'GAIN/LOSS OF ENERGY IN GW THROUGH ENERGY SOURCES AND SINKS';
  E11 = 'GAIN/LOSS OF ENERGY AT SPECIFIED TEMPERATURE NODES';
  E11Alt = 'GAIN/LOSS OF ENERGY IN GW AT SPECIFIED TEMPERATURE NODES';
  E12 = 'TOTAL RATE OF GAIN/LOSS OF ENERGY';
  E12Alt = 'TOTAL RATE OF GAIN/LOSS OF ENERGY IN GW';
  E13 =
    'ENERGY BALANCE ACTIVITY [ A = ((S+) - (S-) + (P+) - (P-) + (F+) - (F-))/2 ]';
  E13Alt =
    'GW ENERGY BALANCE ACTIVITY [ A = ((S+) - (S-) + (P+) - (P-) + (F+) - (F-))/2 ]';
  E14 = 'ABSOLUTE ENERGY BALANCE ERROR [ S - P - F ]';
  E14Alt = 'ABSOLUTE GW ENERGY BALANCE ERROR [ S - P - F ]';
  E15 = 'RELATIVE ENERGY BALANCE ERROR [ 100*(S - P - F)/A ]';
  E15Alt = 'RELATIVE GW ENERGY BALANCE ERROR [ 100*(S - P - F)/A ]';

  E16 = 'GAIN/LOSS OF ENERGY THROUGH INFLOWS/OUTFLOWS AT GEN.-FLOW NODES';
  E16Alt= 'GAIN/LOSS OF ENERGY IN GW THROUGH INFLOWS/OUTFLOWS AT GEN.-FLOW NODES';
  E17 = 'GAIN/LOSS OF ENERGY THROUGH INFLOWS/OUTFLOWS AT LAKE NODES';
//  E17Alt = 'GAIN/LOSS OF ENERGY THROUGH INFLOWS/OUTFLOWS AT LAKE NODES';
  E18 = 'GAIN/LOSS OF ENERGY AT GEN.-TRANSPORT NODES';
  E18Alt = 'GAIN/LOSS OF ENERGY IN GW AT GEN.-TRANSPORT NODES';

  E19_1 = 'RATE OF CHANGE OF ENERGY IN LAKES';
  E19 = 'GAIN/LOSS OF ENERGY IN LAKES THROUGH FLUID SOURCES AND SINKS';
  E20 = 'GAIN/LOSS OF ENERGY IN LAKES THROUGH INFLOWS/OUTFLOWS AT SPEC. P NODES';
  E21 = 'GAIN/LOSS OF ENERGY IN LAKES THROUGH INFLOWS/OUTFL. AT GEN.-FLOW NODES';
  E22 = 'GAIN/LOSS OF ENERGY IN LAKES THROUGH ENERGY SOURCES AND SINKS';
  E23 = 'GAIN/LOSS OF ENERGY IN LAKES AT SPECIFIED TEMPERATURE NODES';
  E24 = 'GAIN/LOSS OF ENERGY IN LAKES AT GEN.-TRANSPORT NODES';
  E25 = 'TOTAL RATE OF GAIN/LOSS OF ENERGY IN LAKES';
var
  SearchTerm: string;
  LineIndex, PrevIndex: integer;
  CurrentLine: string;
  TimeStepString: string;
  BudgetItem, Rate: string;
  ABudget: TBudget;
  ABudgetItem: TBudgetItem;
  ReplacementsNames: TStringList;
  ReplacementIndex: Integer;
  FTerms, STerms, ETerms: TStringList;
  SearchTermIndex: integer;
  PriorLineIndex: Integer;
begin
  ReplacementsNames := TStringList.Create;
  FTerms := TStringList.Create;
  STerms := TStringList.Create;
  ETerms := TStringList.Create;
  try
    begin
      FTerms.Add(F1);
      FTerms.Add(F1Alt);
      FTerms.Add(F2a);
      FTerms.Add(F2aAlt);
      FTerms.Add(F2b);
      FTerms.Add(F2bAlt);
      FTerms.Add(F3);
      FTerms.Add(F3Alt);
      FTerms.Add(F4);
      FTerms.Add(F4Alt);
      FTerms.Add(F5);
      FTerms.Add(F5Alt);
      FTerms.Add(F10);
      FTerms.Add(F10Alt);
      FTerms.Add(F11);
      FTerms.Add(F11Alt);
      FTerms.Add(F6);
      FTerms.Add(F6Alt);

      STerms.Add(S1);
      STerms.Add(S18);
      STerms.Add(S2);
      STerms.Add(S3);
      STerms.Add(S19);
      STerms.Add(S4);
      STerms.Add(S20);
      STerms.Add(S5);
      STerms.Add(S21);
      STerms.Add(S6);
      STerms.Add(S7);
      STerms.Add(S22);
      STerms.Add(S8);
      STerms.Add(S9);
      STerms.Add(S23);
      STerms.Add(S10);
      STerms.Add(S24);
      STerms.Add(S11);
      STerms.Add(S25);
      STerms.Add(S12);
      STerms.Add(S26);
      STerms.Add(S27);
      STerms.Add(S13);
      STerms.Add(S28);
      STerms.Add(S14);
      STerms.Add(S29);
      STerms.Add(S30);

      ETerms.Add(E1);
      ETerms.Add(E1Alt);
      ETerms.Add(E2);
      ETerms.Add(E3);
      ETerms.Add(E3Alt);
      ETerms.Add(E4);
      ETerms.Add(E4Alt);
      ETerms.Add(E5);
      ETerms.Add(E5Alt);
      ETerms.Add(E6);
      ETerms.Add(E7);
      ETerms.Add(E7Alt);
      ETerms.Add(E8);
      ETerms.Add(E8Alt);
      ETerms.Add(E9);
      ETerms.Add(E9Alt);
      ETerms.Add(E16);
      ETerms.Add(E16Alt);
      ETerms.Add(E17);
//      ETerms.Add(E17Alt);
      ETerms.Add(E10);
      ETerms.Add(E10Alt);
      ETerms.Add(E11);
      ETerms.Add(E11Alt);
      ETerms.Add(E18);
      ETerms.Add(E18Alt);
      ETerms.Add(E12);
      ETerms.Add(E12Alt);

      ETerms.Add(E19_1);
      ETerms.Add(E19);
      ETerms.Add(E20);
      ETerms.Add(E21);
      ETerms.Add(E22);
      ETerms.Add(E23);
      ETerms.Add(E24);
      ETerms.Add(E25);

      // Solute budget
      ReplacementsNames.Add(S1);
      ReplacementsNames.Add('Rate of Change in Solute due to Concentration Change');

      ReplacementsNames.Add(S2);
      ReplacementsNames.Add('Rate of Change of Adsorbate');

      ReplacementsNames.Add(S3);
      ReplacementsNames.Add('Rate of Change in Solute due to Change in Mass Of Fluid');

      ReplacementsNames.Add(S4);
      ReplacementsNames.Add('Total Rate of Change of Solute');

      ReplacementsNames.Add(S5);
      ReplacementsNames.Add('First-Order Production/Decay of Solute');

      ReplacementsNames.Add(S6);
      ReplacementsNames.Add('First-Order Production/Decay of Adsorbate');

      ReplacementsNames.Add(S7);
      ReplacementsNames.Add('Zero-Order Production/Decay of Solute');

      ReplacementsNames.Add(S8);
      ReplacementsNames.Add('Zero-Order Production/Decay of Adsorbate');

      ReplacementsNames.Add(S9);
      ReplacementsNames.Add('Total Rate of Production/Decay of Solute and Adsorbate');

      ReplacementsNames.Add(S10);
      ReplacementsNames.Add('Gain/Loss of Solute Through Fluid Sources and Sinks');

      ReplacementsNames.Add(S11);
      ReplacementsNames.Add('Gain/Loss of Solute through Inflows/Outflows at Specified P Nodes');

      ReplacementsNames.Add(S12);
      ReplacementsNames.Add('Gain/Loss of Solute through Solute Sources and Sinks');

      ReplacementsNames.Add(S13);
      ReplacementsNames.Add('Gain/Loss of Solute at Specified Concentration Nodes');

      ReplacementsNames.Add(S14);
      ReplacementsNames.Add('Total Rate of Gain/Loss of Solute through Flows and Sources/Sinks');

      ReplacementsNames.Add(S15);
      ReplacementsNames.Add('Solute Mass Balance Activity');

      ReplacementsNames.Add(S16);
      ReplacementsNames.Add('Absolute Solute Mass Balance Error');

      ReplacementsNames.Add(S17);
      ReplacementsNames.Add('Relative Solute Mass Balance Error');

      ReplacementsNames.Add(S18);
      ReplacementsNames.Add('Rate of Change in Gw Solute Due to Concentration Change');

      ReplacementsNames.Add(S19);
      ReplacementsNames.Add('Rate of Change in Gw Solute Due to Change in Mass of Fluid');

      ReplacementsNames.Add(S20);
      ReplacementsNames.Add('Total Rate of Change of Gw Solute');

      ReplacementsNames.Add(S21);
      ReplacementsNames.Add('First-Order Production/Decay of Gw Solute');

      ReplacementsNames.Add(S22);
      ReplacementsNames.Add('Zero-Order Production/Decay of Gw Solute');

      ReplacementsNames.Add(S23);
      ReplacementsNames.Add('Total Rate of Production/Decay of Gw Solute & Adsorbate');

      ReplacementsNames.Add(S24);
      ReplacementsNames.Add('Gain/Loss of Gw Solute Through Fluid Sources And Sinks');

      ReplacementsNames.Add(S25);
      ReplacementsNames.Add('Gain/Loss of Gw Solute Through Inflows/Outflows At Specified P Nodes');

      ReplacementsNames.Add(S26);
      ReplacementsNames.Add('Gain/Loss of Gw Solute Through Inflows/Outflows At Gen.-Flow Nodes');

      ReplacementsNames.Add(S27);
      ReplacementsNames.Add('Gain/Loss of Gw Solute Through Solute Sources And Sinks');

      ReplacementsNames.Add(S28);
      ReplacementsNames.Add('Gain/Loss of Gw Solute At Specified Concentration Nodes');

      ReplacementsNames.Add(S29);
      ReplacementsNames.Add('Gain/Loss of Gw Solute At Gen.-Transport Nodes');

      ReplacementsNames.Add(S30);
      ReplacementsNames.Add('Total Rate of Gain/Loss of Gw Solute');

      // Fluid Budget

      ReplacementsNames.Add(F1);
      ReplacementsNames.Add('Rate of Change in Total Stored Fluid due to Pressure Change');

      ReplacementsNames.Add(F1Alt);
      ReplacementsNames.Add('Rate of Change in Total Stored Groundwater due to Pressure Change');

      ReplacementsNames.Add(F2a);
      ReplacementsNames.Add('Rate of Change in Total Stored Fluid due to Concentration Change');

      ReplacementsNames.Add(F2aAlt);
      ReplacementsNames.Add('Rate of Change in Total Stored Groundwater due to Concentration Change');

      ReplacementsNames.Add(F2b);
      ReplacementsNames.Add('Rate of Change in Total Stored Fluid due to Temperature Change');

      ReplacementsNames.Add(F2bAlt);
      ReplacementsNames.Add('Rate of Change in Total Stored Groundwater due to Temperature Change');

      ReplacementsNames.Add(F3);
      ReplacementsNames.Add('Total Rate of Change in Stored Fluid');

      ReplacementsNames.Add(F3Alt);
      ReplacementsNames.Add('Total Rate of Change in Stored Groundwater');

      ReplacementsNames.Add(F4);
      ReplacementsNames.Add('Gain/Loss of Fluid through Fluid Sources and Sinks');

      ReplacementsNames.Add(F4Alt);
      ReplacementsNames.Add('Gain/Loss of Groundwater through Fluid Sources and Sinks');

      ReplacementsNames.Add(F5);
      ReplacementsNames.Add('Gain/Loss of Fluid through Inflows/Outflows at Specified P Nodes');

      ReplacementsNames.Add(F5Alt);
      ReplacementsNames.Add('Gain/Loss of Groundwater through Inflows/Outflows at Specified P Nodes');

      ReplacementsNames.Add(F6);
      ReplacementsNames.Add('Total Rate of Gain/Loss of Fluid through Flows');

      ReplacementsNames.Add(F6Alt);
      ReplacementsNames.Add('Total Rate of Gain/Loss of Groundwater through Flows');

      ReplacementsNames.Add(F7);
      ReplacementsNames.Add('Fluid Mass Balance Activity');

      ReplacementsNames.Add(F7Alt);
      ReplacementsNames.Add('Groundwater Mass Balance Activity');

      ReplacementsNames.Add(F8);
      ReplacementsNames.Add('Absolute Fluid Mass Balance Error');

      ReplacementsNames.Add(F8Alt);
      ReplacementsNames.Add('Absolute Groundwater Mass Balance Error');

      ReplacementsNames.Add(F10);
      ReplacementsNames.Add('Gain/Loss of Fluid Through Inflows/Outflows at Gen.-Flow Nodes');
      ReplacementsNames.Add(F10Alt);
      ReplacementsNames.Add('Gain/Loss of Groundwater Through Inflows/Outflows at Gen.-Flow Nodes');
      ReplacementsNames.Add(F11);
      ReplacementsNames.Add('Gain/Loss of Fluid Through Inflows/Outflows at Lake Nodes');
      ReplacementsNames.Add(F11Alt);
      ReplacementsNames.Add('Gain/Loss of Groundwater Through Inflows/Outflows at Lake Nodes');

      // Energy Budget

      ReplacementsNames.Add(E1);
      ReplacementsNames.Add('Rate of Change of Energy in Fluid due to Temperature');

      ReplacementsNames.Add(E1Alt);
      ReplacementsNames.Add('Rate of Change of Energy in GW due to Temperature');

      ReplacementsNames.Add(E2);
      ReplacementsNames.Add('Rate of Change of Energy in Solid Grains');

      ReplacementsNames.Add(E3);
      ReplacementsNames.Add('Rate of Change of Energy due to Change in Mass of Fluid');

      ReplacementsNames.Add(E3Alt);
      ReplacementsNames.Add('Rate of Change of Energy in GW due to Change in Mass of Fluid');

      ReplacementsNames.Add(E4);
      ReplacementsNames.Add('Total Rate of Change of Energy');

      ReplacementsNames.Add(E4Alt);
      ReplacementsNames.Add('Total Rate of Change of Energy in GW & Solid Grains');

      ReplacementsNames.Add(E5);
      ReplacementsNames.Add('Zero-Order Production/Decay of Energy in Fluid');

      ReplacementsNames.Add(E5Alt);
      ReplacementsNames.Add('Zero-Order Production/Decay of Energy in GW');

      ReplacementsNames.Add(E6);
      ReplacementsNames.Add('Zero-Order Production/Decay of Energy in Solid Grains');

      ReplacementsNames.Add(E7);
      ReplacementsNames.Add('Total Rate of Production/Decay of Energy');

      ReplacementsNames.Add(E7Alt);
      ReplacementsNames.Add('Total Rate of Production/Decay of Energy in GW & Solid Grains');

      ReplacementsNames.Add(E8);
      ReplacementsNames.Add('Gain/Loss of Energy through Fluid Sources and Sinks');

      ReplacementsNames.Add(E8Alt);
      ReplacementsNames.Add('Gain/Loss of Energy in GW through Fluid Sources and Sinks');

      ReplacementsNames.Add(E9);
      ReplacementsNames.Add('Gain/Loss of Energy through Inflows/Outflows at Specified P Nodes');

      ReplacementsNames.Add(E9Alt);
      ReplacementsNames.Add('Gain/Loss of Energy in GW through Inflows/Outflows at Specified P Nodes');

      ReplacementsNames.Add(E10);
      ReplacementsNames.Add('Gain/Loss of Energy through Energy Sources and Sinks');

      ReplacementsNames.Add(E10Alt);
      ReplacementsNames.Add('Gain/Loss of Energy in GW through Energy Sources and Sinks');

      ReplacementsNames.Add(E11);
      ReplacementsNames.Add('Gain/Loss of Energy at Specified Temperature Nodes');

      ReplacementsNames.Add(E11Alt);
      ReplacementsNames.Add('Gain/Loss of Energy in GW at Specified Temperature Nodes');

      ReplacementsNames.Add(E12);
      ReplacementsNames.Add('Total Rate of Gain/Loss of Energy through Flows and Sources/Sinks');

      ReplacementsNames.Add(E12Alt);
      ReplacementsNames.Add('Total Rate of Gain/Loss of Energy in GW through Flows and Sources/Sinks');

      ReplacementsNames.Add(E13);
      ReplacementsNames.Add('Energy Balance Activity');

      ReplacementsNames.Add(E13Alt);
      ReplacementsNames.Add('GW Energy Balance Activity');

      ReplacementsNames.Add(E14);
      ReplacementsNames.Add('Absolute Energy Balance Error');

      ReplacementsNames.Add(E14Alt);
      ReplacementsNames.Add('Absolute GW Energy Balance Error');

      ReplacementsNames.Add(E16);
      ReplacementsNames.Add('Gain/Loss of Energy Through Inflows/Outflows at Gen.-Flow Nodes');

      ReplacementsNames.Add(E16alt);
      ReplacementsNames.Add('Gain/Loss of Energy In Gw Through Inflows/Outflows at Gen.-Flow Nodes');

      ReplacementsNames.Add(E17);
      ReplacementsNames.Add('Gain/Loss of Energy Through Inflows/Outflows at Lake Nodes');

      ReplacementsNames.Add(E18);
      ReplacementsNames.Add('Gain/Loss of Energy at Gen.-Transport Nodes');

      ReplacementsNames.Add(E18alt);
      ReplacementsNames.Add('Gain/Loss of Energy In Gw at Gen.-Transport Nodes');

      ReplacementsNames.Add(E19_1);
      ReplacementsNames.Add('Rate of Change of Energy in Lakes');

      ReplacementsNames.Add(E19);
      ReplacementsNames.Add('Gain/Loss of Energy In Lakes Through Fluid Sources and Sinks');

      ReplacementsNames.Add(E20);
      ReplacementsNames.Add('Gain/Loss of Energy In Lakes Through Inflows/Outflows at Spec. P Nodes');

      ReplacementsNames.Add(E21);
      ReplacementsNames.Add('Gain/Loss of Energy In Lakes Through Inflows/Outfl. at Gen.-Flow Nodes');

      ReplacementsNames.Add(E22);
      ReplacementsNames.Add('Gain/Loss of Energy In Lakes Through Energy Sources and Sinks');

      ReplacementsNames.Add(E23);
      ReplacementsNames.Add('Gain/Loss of Energy In Lakes at Specified Temperature Nodes');

      ReplacementsNames.Add(E24);
      ReplacementsNames.Add('Gain/Loss of Energy In Lakes at Gen.-Transport Nodes');

      ReplacementsNames.Add(E25);
      ReplacementsNames.Add('Total Rate of Gain/Loss of Energy In Lakes');

      LineIndex := 0;
      SearchTerm := 'S O L U T E   B U D G E T';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      while LineIndex > -1 do
      begin
        ABudget := TBudget.Create;
        FBudgetList.Add(ABudget);
        CurrentLine := ZBLStringList.Strings[LineIndex];

        TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP', ',');

        ABudget.Zone := 'SUTRA Solute Budget';
        ABudget.TimeStep := TimeStepString;

        for SearchTermIndex := 0 to STerms.Count - 1 do
        begin
          SearchTerm := STerms[SearchTermIndex];
          BudgetItem := SearchTerm;
          ReplacementIndex := ReplacementsNames.IndexOf(BudgetItem);
          if ReplacementIndex > -1 then
          begin
            BudgetItem := ReplacementsNames[ReplacementIndex + 1];
          end;
          PriorLineIndex := LineIndex;
          LineIndex := GetNextLine(SearchTerm, LineIndex);
          if LineIndex >= 0 then
          begin
            CurrentLine := ZBLStringList.Strings[LineIndex];
            CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
            Rate := GetStringBetween(CurrentLine, '', ' ');

            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            ABudget.InList.Add(ABudgetItem);

            CurrentLine := GetStringBetween(CurrentLine, Rate, '');
            Rate := GetStringBetween(CurrentLine, '', ' ');
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            ABudget.OutList.Add(ABudgetItem);

            Rate := GetStringBetween(CurrentLine, Rate, '');
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            ABudget.NetList.Add(ABudgetItem);
          end
          else
          begin
            LineIndex := PriorLineIndex;
          end;
        end;

        SearchTerm := S31;
        PriorLineIndex := LineIndex;
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
        if LineIndex >= 0 then
        begin
          CurrentLine := ZBLStringList.Strings[LineIndex];
          Rate := GetStringBetween(CurrentLine, SearchTerm, '');
          ABudget.Activity := Rate;
        end
        else
        begin
          LineIndex := PriorLineIndex;
          SearchTerm := S15;
          PriorLineIndex := LineIndex;
          LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
          if LineIndex >= 0 then
          begin
            CurrentLine := ZBLStringList.Strings[LineIndex];
            Rate := GetStringBetween(CurrentLine, SearchTerm, '');
            ABudget.Activity := Rate;
          end
          else
          begin
            LineIndex := PriorLineIndex;
          end;
        end;

        SearchTerm := S16;
        PriorLineIndex := LineIndex;
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
        if LineIndex >= 0 then
        begin
          CurrentLine := ZBLStringList.Strings[LineIndex];
          Rate := GetStringBetween(CurrentLine, SearchTerm, '');
          ABudget.InMinusOut := Rate;
        end
        else
        begin
          LineIndex := PriorLineIndex;
          SearchTerm := S31;
          LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
          if LineIndex >= 0 then
          begin
            CurrentLine := ZBLStringList.Strings[LineIndex];
            Rate := GetStringBetween(CurrentLine, SearchTerm, '');
            ABudget.InMinusOut := Rate;
          end
          else
          begin
            LineIndex := PriorLineIndex;
          end;
        end;

        SearchTerm := S17;
        PriorLineIndex := LineIndex;
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
        if LineIndex >= 0 then
        begin
          CurrentLine := ZBLStringList.Strings[LineIndex];
          Rate := GetStringBetween(CurrentLine, SearchTerm, '');
          if Pos('UNDEFINED', Rate) > 0 then
          begin
            Rate := '0';
          end
          else
          begin
            Rate := GetStringBetween(Rate, '', '(PERCENT)');
          end;
          ABudget.Discrepancy := Rate;
        end
        else
        begin
          LineIndex := PriorLineIndex;
          SearchTerm := S32;
          LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
          if LineIndex >= 0 then
          begin
            CurrentLine := ZBLStringList.Strings[LineIndex];
            Rate := GetStringBetween(CurrentLine, SearchTerm, '');
            if Pos('UNDEFINED', Rate) > 0 then
            begin
              Rate := '0';
            end
            else
            begin
              Rate := GetStringBetween(Rate, '', '(PERCENT)');
            end;
            ABudget.Discrepancy := Rate;
          end
          else
          begin
            LineIndex := PriorLineIndex;
          end;
        end;


        SearchTerm := 'S O L U T E   B U D G E T';
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
      end;

      LineIndex := 0;
      SearchTerm := 'F L U I D   M A S S   B U D G E T';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      while LineIndex > -1 do
      begin
        ABudget := TBudget.Create;
        FBudgetList.Add(ABudget);
        CurrentLine := ZBLStringList.Strings[LineIndex];

        TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP', ',');

        ABudget.Zone := 'SUTRA Mass Budget';
        ABudget.TimeStep := TimeStepString;

        for SearchTermIndex := 0 to FTerms.Count - 1 do
        begin
          SearchTerm := FTerms[SearchTermIndex];
          BudgetItem := SearchTerm;
          ReplacementIndex := ReplacementsNames.IndexOf(BudgetItem);
          if ReplacementIndex > -1 then
          begin
            BudgetItem := ReplacementsNames[ReplacementIndex + 1];
          end;
          PrevIndex := LineIndex;
          LineIndex := GetNextLine(SearchTerm, LineIndex);
          if LineIndex < 0 then
          begin
            LineIndex := PrevIndex;
            Continue;
          end;
          CurrentLine := ZBLStringList.Strings[LineIndex];
          CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
          Rate := GetStringBetween(CurrentLine, '', ' ');

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.InList.Add(ABudgetItem);

          CurrentLine := GetStringBetween(CurrentLine, Rate, '');
          Rate := GetStringBetween(CurrentLine, '', ' ');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.OutList.Add(ABudgetItem);

          Rate := GetStringBetween(CurrentLine, Rate, '');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.NetList.Add(ABudgetItem);
        end;

        SearchTerm := F8;
        PrevIndex := LineIndex;
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
        if LineIndex >= 0 then
        begin
          CurrentLine := ZBLStringList.Strings[LineIndex];
          Rate := GetStringBetween(CurrentLine, SearchTerm, '');
          ABudget.InMinusOut := Rate;
        end
        else
        begin
          LineIndex := PrevIndex;
          SearchTerm := F8Alt;
//          PrevIndex := LineIndex;
          LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
          if LineIndex >= 0 then
          begin
            CurrentLine := ZBLStringList.Strings[LineIndex];
            Rate := GetStringBetween(CurrentLine, SearchTerm, '');
            ABudget.InMinusOut := Rate;
          end
          else
          begin
            LineIndex := PrevIndex;
          end;
        end;

        SearchTerm := F9;
        PrevIndex := LineIndex;
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
        if LineIndex >= 0 then
        begin
          CurrentLine := ZBLStringList.Strings[LineIndex];
          Rate := GetStringBetween(CurrentLine, SearchTerm, '');
          if Pos('UNDEFINED', Rate) > 0 then
          begin
            Rate := '0';
          end
          else
          begin
            Rate := GetStringBetween(Rate, '', '(PERCENT)');
          end;
          ABudget.Discrepancy := Rate;
        end
        else
        begin
          LineIndex := PrevIndex;
          SearchTerm := F9Alt;
          LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
          if LineIndex >= 0 then
          begin
            CurrentLine := ZBLStringList.Strings[LineIndex];
            Rate := GetStringBetween(CurrentLine, SearchTerm, '');
            if Pos('UNDEFINED', Rate) > 0 then
            begin
              Rate := '0';
            end
            else
            begin
              Rate := GetStringBetween(Rate, '', '(PERCENT)');
            end;
            ABudget.Discrepancy := Rate;
          end
          else
          begin
            LineIndex := PrevIndex;
          end;
        end;

        SearchTerm := 'F L U I D   M A S S   B U D G E T';
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
      end;

      LineIndex := 0;
      SearchTerm := 'E N E R G Y   B U D G E T';
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      while LineIndex > -1 do
      begin
        ABudget := TBudget.Create;
        FBudgetList.Add(ABudget);
        CurrentLine := ZBLStringList.Strings[LineIndex];

        TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP', ',');

        ABudget.Zone := 'SUTRA Energy Budget';
        ABudget.TimeStep := TimeStepString;

        for SearchTermIndex := 0 to ETerms.Count - 1 do
        begin
          SearchTerm := ETerms[SearchTermIndex];
          BudgetItem := SearchTerm;
          ReplacementIndex := ReplacementsNames.IndexOf(BudgetItem);
          if ReplacementIndex > -1 then
          begin
            BudgetItem := ReplacementsNames[ReplacementIndex + 1];
          end;
          PrevIndex := LineIndex;
          LineIndex := GetNextLine(SearchTerm, LineIndex);
          if LineIndex < 0 then
          begin
            LineIndex := PrevIndex;
            Continue;
          end;
          CurrentLine := ZBLStringList.Strings[LineIndex];
          CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
          Rate := GetStringBetween(CurrentLine, '', ' ');

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.InList.Add(ABudgetItem);

          CurrentLine := GetStringBetween(CurrentLine, Rate, '');
          Rate := GetStringBetween(CurrentLine, '', ' ');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.OutList.Add(ABudgetItem);

          Rate := GetStringBetween(CurrentLine, Rate, '');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          ABudget.NetList.Add(ABudgetItem);
        end;

        SearchTerm := E14;
        PrevIndex := LineIndex;
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
        if LineIndex > 0 then
        begin
          CurrentLine := ZBLStringList.Strings[LineIndex];
          Rate := GetStringBetween(CurrentLine, SearchTerm, '');
          ABudget.InMinusOut := Rate;
        end
        else
        begin
          LineIndex := PrevIndex;
          SearchTerm := E14Alt;
          LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
          if LineIndex > 0 then
          begin
            CurrentLine := ZBLStringList.Strings[LineIndex];
            Rate := GetStringBetween(CurrentLine, SearchTerm, '');
            ABudget.InMinusOut := Rate;
          end
          else
          begin
            LineIndex := PrevIndex;
          end;
        end;

        SearchTerm := E15;
        PrevIndex := LineIndex;
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
        if LineIndex >=0 then
        begin
          CurrentLine := ZBLStringList.Strings[LineIndex];
          Rate := GetStringBetween(CurrentLine, SearchTerm, '');
          if Pos('UNDEFINED', Rate) > 0 then
          begin
            Rate := '0';
          end
          else
          begin
            Rate := GetStringBetween(Rate, '', '(PERCENT)');
          end;
          ABudget.Discrepancy := Rate;
        end
        else
        begin
          LineIndex := PrevIndex;
          SearchTerm := E15Alt;
          LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
          if LineIndex >=0 then
          begin
            CurrentLine := ZBLStringList.Strings[LineIndex];
            Rate := GetStringBetween(CurrentLine, SearchTerm, '');
            if Pos('UNDEFINED', Rate) > 0 then
            begin
              Rate := '0';
            end
            else
            begin
              Rate := GetStringBetween(Rate, '', '(PERCENT)');
            end;
            ABudget.Discrepancy := Rate;
          end
          else
          begin
            LineIndex := PrevIndex;
          end;
        end;

        SearchTerm := 'E N E R G Y   B U D G E T';
        LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
      end;
    end;
  finally
    begin
      ReplacementsNames.Free;
      FTerms.Free;
      STerms.Free;
      ETerms.Free;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.ReadSutra4_File;
const
  KBalanceActivity = 'BALANCE ACTIVITY';
  KRateOfChange = '  RATE OF CHANGE OF';
  TermStart = 87;
var
  LineIndex: Integer;
  SearchTerm: string;
  TimeStep: Integer;
  BudgetType: string;
  ABudget: TBudget;
  ALine: string;
  BActivityPos: Integer;
  RateOfChangePos: Integer;
  InnerIndex: Integer;
  ImpactsString: string;
//  BudgetTerm: System.array;
  DashPosition: Integer;
  LinePosition: Integer;
  ABudgetItem: TBudgetItem;
  Rate: string;
  BudgetTerm: string;
  Splitter: TStringList;
  function GetTimeStepString: string;
  const
    KTimeStep = 'AFTER TIME STEP';
  var
    ALine: string;
    StepPosition: Integer;
    TimeStepString: string;
  begin
    ALine := ZBLStringList.Strings[LineIndex];
    result := Trim(GetStringBetween(ALine, KTimeStep, ','));
  end;
  procedure ReadBudget;
  const
    KGainLoss = 'GAIN/LOSS';
  var
//    TotalItem: String;
    RateOfChangeDesc: String;
    GainLossPos: Integer;
    GainLossDesc: string;
  begin
    ABudget.Zone := Format('SUTRA %s Budget', [BudgetType]);
    ABudget.TimeStep := GetTimeStepString;

    repeat
      Inc(LineIndex);
      ALine := ZBLStringList[LineIndex];
      RateOfChangePos := Pos(KRateOfChange, ALine);
      if RateOfChangePos > 0 then
      begin
//        RateOfChangeDesc := Trim(Copy(ALine, 1, TermStart-1)) + ' ';
        RateOfChangeDesc := Format('%s Rate of change: ', [BudgetType]);
        Inc(LineIndex);
        ALine := ZBLStringList[LineIndex];
        ImpactsString := GetStringBetween(ALine, '', ':');
        if Pos('CHANGE IMPACTS ON', ImpactsString) > 0 then
        begin
          ImpactsString:= GetStringBetween(ImpactsString, '',
            'CHANGE IMPACTS ON');
//          ImpactsString := Splitter.Strings[Splitter.Count-1];
        end;
        ImpactsString := RateOfChangeDesc + ImpactsString + ': ';

        Inc(LineIndex);
        ALine := ZBLStringList[LineIndex];
        DashPosition := Pos('-----------', ALine);
        LinePosition := Pos('___________', ALine);
        while (DashPosition <= 0) and (LinePosition <= 0) do
        begin
          BudgetTerm := Trim(Copy(ALine, 1, TermStart-1));

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := ImpactsString + BudgetTerm;
          ALine := GetStringBetween(ALine, BudgetTerm, '');
          Rate := GetStringBetween(ALine, '', ' ');
          ABudgetItem.value := Rate;
          ABudget.InList.Add(ABudgetItem);

          ALine := GetStringBetween(ALine, Rate, '');
          Rate := GetStringBetween(ALine, '', ' ');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := ImpactsString + BudgetTerm;
          ABudgetItem.value := Rate;
          ABudget.OutList.Add(ABudgetItem);

          Rate := GetStringBetween(ALine, Rate, '');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := ImpactsString + BudgetTerm;;
          ABudgetItem.value := Rate;
          ABudget.NetList.Add(ABudgetItem);

          Inc(LineIndex);
          ALine := ZBLStringList[LineIndex];
          DashPosition := Pos('-----------', ALine);
          LinePosition := Pos('___________', ALine);

          if (DashPosition > 0) or (LinePosition > 0) then
          begin
            Inc(LineIndex);
            ALine := ZBLStringList[LineIndex];

            BudgetTerm := Trim(Copy(ALine, 1, TermStart-1));
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetTerm;

            ALine := GetStringBetween(ALine, BudgetTerm, '');
            Rate := GetStringBetween(ALine, '', ' ');
            ABudgetItem.value := Rate;
            ABudget.InList.Add(ABudgetItem);

            ALine := GetStringBetween(ALine, Rate, '');
            Rate := GetStringBetween(ALine, '', ' ');
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetTerm;
            ABudgetItem.value := Rate;
            ABudget.OutList.Add(ABudgetItem);

            Rate := GetStringBetween(ALine, Rate, '');
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetTerm;;
            ABudgetItem.value := Rate;
            ABudget.NetList.Add(ABudgetItem);

            break;
          end;
        end;
      end;

      GainLossPos := Pos(KGainLoss, ALine);
      if GainLossPos > 0 then
      begin
//        GainLossDesc := Trim(Copy(ALine, 1, TermStart-1)) + ' ';
        GainLossDesc := 'Gain/Loss: ';
        Inc(LineIndex);
        ALine := ZBLStringList[LineIndex];
        ImpactsString := GainLossDesc + GetStringBetween(ALine, '', ':') + ' ';

        Inc(LineIndex);
        ALine := ZBLStringList[LineIndex];
        DashPosition := Pos('-----------', ALine);
        LinePosition := Pos('___________', ALine);
        while (DashPosition <= 0) and (LinePosition <= 0) do
        begin
          BudgetTerm := Trim(Copy(ALine, 1, TermStart-1));
          if Pos(':', BudgetTerm) > 0 then
          begin
            ImpactsString := GainLossDesc + GetStringBetween(ALine, '', ':') + ' ';

            Inc(LineIndex);
            ALine := ZBLStringList[LineIndex];

            continue
          end;

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := GainLossDesc + BudgetTerm;
          ALine := GetStringBetween(ALine, BudgetTerm, '');
          Rate := GetStringBetween(ALine, '', ' ');
          ABudgetItem.value := Rate;
          ABudget.InList.Add(ABudgetItem);

          ALine := GetStringBetween(ALine, Rate, '');
          Rate := GetStringBetween(ALine, '', ' ');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := GainLossDesc + BudgetTerm;
          ABudgetItem.value := Rate;
          ABudget.OutList.Add(ABudgetItem);

          Rate := GetStringBetween(ALine, Rate, '');
          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := GainLossDesc + BudgetTerm;;
          ABudgetItem.value := Rate;
          ABudget.NetList.Add(ABudgetItem);

          Inc(LineIndex);
          ALine := ZBLStringList[LineIndex];
          DashPosition := Pos('-----------', ALine);
          LinePosition := Pos('___________', ALine);

          if (DashPosition > 0) or (LinePosition > 0) then
          begin
            Inc(LineIndex);
            ALine := ZBLStringList[LineIndex];

            BudgetTerm := Trim(Copy(ALine, 1, TermStart-1));
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetTerm;

            ALine := GetStringBetween(ALine, BudgetTerm, '');
            Rate := GetStringBetween(ALine, '', ' ');
            ABudgetItem.value := Rate;
            ABudget.InList.Add(ABudgetItem);

            ALine := GetStringBetween(ALine, Rate, '');
            Rate := GetStringBetween(ALine, '', ' ');
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetTerm;
            ABudgetItem.value := Rate;
            ABudget.OutList.Add(ABudgetItem);

            Rate := GetStringBetween(ALine, Rate, '');
            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetTerm;;
            ABudgetItem.value := Rate;
            ABudget.NetList.Add(ABudgetItem);

            break;
          end;

        end;
      end;

      BActivityPos := Pos(KBalanceActivity, ALine);
      if BActivityPos > 0  then
      begin
        ABudget.Activity := GetStringBetween(ALine, ']', '');

        Inc(LineIndex);
        ALine := ZBLStringList[LineIndex];
        ABudget.InMinusOut := GetStringBetween(ALine, ']', '');

        Inc(LineIndex);
        ALine := ZBLStringList[LineIndex];
        ABudget.Discrepancy := GetStringBetween(ALine, ']', '(PERCENT)');

        break;
      end;
    until False;
  end;
begin
  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    LineIndex := 0;
    SearchTerm := 'H 2 O   M A S S   B U D G E T';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    while LineIndex > -1 do
    begin
      ABudget := TBudget.Create;
      FBudgetList.Add(ABudget);
      BudgetType := 'Water';
      BActivityPos := 0;

      ReadBudget;

      LineIndex := GetNextLine(SearchTerm, LineIndex);
    end;

    LineIndex := 0;
    SearchTerm := 'H 2 O  M A S S   B U D G E T';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    while LineIndex > -1 do
    begin
      ABudget := TBudget.Create;
      FBudgetList.Add(ABudget);
      BudgetType := 'Water';

      BActivityPos := 0;

      ReadBudget;

      LineIndex := GetNextLine(SearchTerm, LineIndex);
    end;

    LineIndex := 0;
    SearchTerm := 'S O L U T E   B U D G E T';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    while LineIndex > -1 do
    begin
      ABudget := TBudget.Create;
      FBudgetList.Add(ABudget);
      BudgetType := 'Solute';

      BActivityPos := 0;

      ReadBudget;

      LineIndex := GetNextLine(SearchTerm, LineIndex);
    end;

    LineIndex := 0;
    SearchTerm := 'E N E R G Y   B U D G E T';
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    while LineIndex > -1 do
    begin
      ABudget := TBudget.Create;
      FBudgetList.Add(ABudget);
      BudgetType := 'Energy';

      BActivityPos := 0;

      ReadBudget;

      LineIndex := GetNextLine(SearchTerm, LineIndex);
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TfrmZoneBdgtReader.ReadABudget(CumBudget: TBudget;
  RateBudget: TBudget; var LineIndex: Integer;
  const TimeUnitsString: string; HasSeparateStorageTerm: Boolean);
var
  BudgetItem: string;
  StartLine: Integer;
  CumBudgetItem: TBudgetItem;
  InMinusOut: string;
  Discrepancy: string;
  RateBudgetItem: TBudgetItem;
  Local_InnerLineIndex1: Integer;
  Local_InnerLineIndex: Integer;
  StopLine: Integer;
  Rate: string;
  SearchTerm: string;
  TimeStrings : TStringList;
  CurrentLine: string;
begin
  StartLine := LineIndex + 4;
  SearchTerm := 'IN:';
  StartLine := GetNextLine(SearchTerm, StartLine + 1);
  SearchTerm := 'OUT:';
  StopLine := GetNextLine(SearchTerm, StartLine + 1);
  for Local_InnerLineIndex := StartLine to StopLine do
  begin
    CurrentLine := ZBLStringList.Strings[Local_InnerLineIndex];
    if Pos('=', CurrentLine) > 0 then
    begin
      BudgetItem := GetStringBetween(CurrentLine, '', '=');
      if BudgetItem[1] = '0' then
      begin
        BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
      end;
      CurrentLine := GetStringBetween(CurrentLine, '=', '');
      CumBudgetItem := TBudgetItem.Create;
      RateBudgetItem := TBudgetItem.Create;
      CumBudget.InList.Add(CumBudgetItem);
      RateBudget.InList.Add(RateBudgetItem);
      CumBudgetItem.Name := BudgetItem;
      RateBudgetItem.Name := BudgetItem;
      Rate := GetStringBetween(CurrentLine, '', BudgetItem);
      CumBudgetItem.value := Rate;
      CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
      Rate := GetStringBetween(CurrentLine, '=', '');
      if Pos(' ', Rate) > 0 then
      begin
        Rate := GetStringBetween(Rate, '', ' ');
      end;
      RateBudgetItem.value := Rate;
    end;
  end;
  if StopLine > -1 then
  begin
    StartLine := StopLine + 1;
    SearchTerm := 'IN - OUT';
    StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;
    if StopLine > -1 then
    begin
      for Local_InnerLineIndex1 := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[Local_InnerLineIndex1];
        if Pos('=', CurrentLine) > 0 then
        begin
          BudgetItem := GetStringBetween(CurrentLine, '', '=');
          if BudgetItem[1] = '0' then
          begin
            BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
          end;
          CurrentLine := GetStringBetween(CurrentLine, '=', '');
          CumBudgetItem := TBudgetItem.Create;
          RateBudgetItem := TBudgetItem.Create;
          CumBudget.OutList.Add(CumBudgetItem);
          RateBudget.OutList.Add(RateBudgetItem);
          CumBudgetItem.Name := BudgetItem;
          RateBudgetItem.Name := BudgetItem;
          Rate := GetStringBetween(CurrentLine, '', BudgetItem);
          CumBudgetItem.value := Rate;
          CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
          Rate := GetStringBetween(CurrentLine, '=', '');
          if Pos(' ', Rate) > 0 then
          begin
            Rate := GetStringBetween(Rate, '', ' ');
          end;
          RateBudgetItem.value := Rate;
        end;
      end;
    end;
  end;
  if StopLine > -1 then
  begin
    SearchTerm := 'IN - OUT';
    LineIndex := GetNextLine(SearchTerm, StopLine + 1);
    if LineIndex > -1 then
    begin
      CurrentLine := ZBLStringList.Strings[LineIndex];
      CurrentLine := GetStringBetween(CurrentLine, '=', '');
      InMinusOut := GetStringBetween(CurrentLine, '', SearchTerm);
      CumBudget.InMinusOut := InMinusOut;
      CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
      InMinusOut := GetStringBetween(CurrentLine, '=', '');
      RateBudget.InMinusOut := InMinusOut;
    end;
  end;
  if HasSeparateStorageTerm and (StopLine > -1) then
  begin
    SearchTerm := 'STORAGE CHANGE';
    LineIndex := GetNextLine(SearchTerm, StopLine + 1);
    if LineIndex > -1 then
    begin
      BudgetItem := SearchTerm;
      CurrentLine := ZBLStringList.Strings[LineIndex];
      CumBudgetItem := TBudgetItem.Create;
      RateBudgetItem := TBudgetItem.Create;
      CumBudget.OutList.Add(CumBudgetItem);
      RateBudget.OutList.Add(RateBudgetItem);
      CumBudgetItem.Name := BudgetItem;
      RateBudgetItem.Name := BudgetItem;
      CurrentLine := GetStringBetween(CurrentLine, '=', '');
      Rate := GetStringBetween(CurrentLine, '', BudgetItem);
      CumBudgetItem.value := Rate;
      CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
      Rate := GetStringBetween(CurrentLine, '=', '');
      RateBudgetItem.value := Rate;
    end;
  end;
  if StopLine > -1 then
  begin
    SearchTerm := 'PERCENT DISCREPANCY =';
    LineIndex := GetNextLine(SearchTerm, StopLine);
    if LineIndex > -1 then
    begin
      SearchTerm := 'PERCENT DISCREPANCY';
      CurrentLine := ZBLStringList.Strings[LineIndex];
      CurrentLine := GetStringBetween(CurrentLine, '=', '');
      Discrepancy := GetStringBetween(CurrentLine, '', SearchTerm);
      CumBudget.Discrepancy := Discrepancy;
      CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
      Discrepancy := GetStringBetween(CurrentLine, '=', '');
      RateBudget.Discrepancy := Discrepancy;
    end;
  end;
  if StopLine > -1 then
  begin
    SearchTerm := 'TOTAL TIME';
    LineIndex := GetNextLine(SearchTerm, StopLine);
    if LineIndex > -1 then
    begin
      CurrentLine := ZBLStringList.Strings[LineIndex];
      CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
      TimeStrings := TStringList.Create;
      try
        TimeStrings.Delimiter := ' ';
        TimeStrings.DelimitedText := CurrentLine;
        if TimeUnitsString = 'SECONDS' then
        begin
          CumBudget.Time := TimeStrings[0];
        end
        else if TimeUnitsString = 'MINUTES' then
        begin
          CumBudget.Time := TimeStrings[1];
        end
        else if TimeUnitsString = 'HOURS' then
        begin
          CumBudget.Time := TimeStrings[2];
        end
        else if TimeUnitsString = 'DAYS' then
        begin
          CumBudget.Time := TimeStrings[3];
        end
        else if TimeUnitsString = 'YEARS' then
        begin
          CumBudget.Time := TimeStrings[4];
        end;
        RateBudget.Time := CumBudget.Time;
      finally
        TimeStrings.Free;
      end;
    end;
  end;
//  if StopLine > -1 then
//  begin
//    LineIndex := StopLine + 1;
//  end;

end;

procedure TfrmZoneBdgtReader.ReadUzfBudget(
  TimeUnitsString: string; LineIndex: Integer; SearchTerm: string;
  const FirstSearchTerm, CumBudgetName, RateBudgetName: string;
  HasSeparateStorageTerm: Boolean);
var
  CurrentLine: string;
  TimeStepString: string;
  StressPeriodString: string;
  CumBudget: TBudget;
  RateBudget: TBudget;
begin
  while LineIndex > -1 do
  begin
    CurrentLine := ZBLStringList.Strings[LineIndex];
    TimeStepString := GetStringBetween(CurrentLine, SearchTerm, 'STRESS PERIOD');
    StressPeriodString := GetStringBetween(CurrentLine, 'STRESS PERIOD', '');
    CumBudget := TBudget.Create;
    FBudgetList.Add(CumBudget);
    CumBudget.Zone := CumBudgetName;
    CumBudget.InMinusOut := '0';
    CumBudget.Discrepancy := '0';
    CumBudget.StressPeriod := StressPeriodString;
    CumBudget.TimeStep := TimeStepString;
    RateBudget := TBudget.Create;
    FBudgetList.Add(RateBudget);
    RateBudget.Zone := RateBudgetName;
    RateBudget.InMinusOut := '0';
    RateBudget.Discrepancy := '0';
    RateBudget.StressPeriod := StressPeriodString;
    RateBudget.TimeStep := TimeStepString;

    ReadABudget(CumBudget, RateBudget, LineIndex, TimeUnitsString, HasSeparateStorageTerm);


    SearchTerm := FirstSearchTerm;
    LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
  end;
end;

procedure TfrmZoneBdgtReader.ReadModflowOrSeawatFile(
  const BudgetStartLine: string; const AltBudgetStartLines: TStringArray = nil);
var
  SearchTerm: string;
  BudgetSearchTerm: string;
  LineIndex: integer;
  CurrentLine: string;
  TimeStepString, StressPeriodString: string;
  StartLine, StopLine, InnerLineIndex: integer;
  BudgetItem, Rate: string;
  InMinusOut, Discrepancy: string;
  CumBudget, RateBudget: TBudget;
  CumBudgetItem, RateBudgetItem: TBudgetItem;
  StreamBudget: TBudget;
  Step: Integer;
  FoundEnd: boolean;
  Segment: string;
  Reach: string;
  StreamBudgetItem: TBudgetItem;
  StreamAquiferBudgetItem: TBudget;
  BudgetItemPrefix: string;
  TimeUnitsString: string;
  TimeStrings: TStringList;
  F: TextFile;
  S: string;
//  StringList1, StringList2: TStringList;
  ShouldStoreFile: Boolean;
  StartSearchTerm, StopSearchTerm1, StopSearchTerm2: string;
  TimeUnitLine: boolean;
  TempList: TStringList;
  ZoneIndex: integer;
  ZoneFound: boolean;
  TimeLine: string;
  ZoneLine: string;
  AZone: integer;
  PriorLineIndex: Integer;
  AltIndex: Integer;
const
  UnsatBudTerm = 'UNSATURATED ZONE PACKAGE VOLUMETRIC BUDGET FOR  TIME STEP';
  StreamUnsatBudTerm = 'VOLUMETRIC BUDGET FOR UNSATURATED ZONE BENEATH STREAMS AT END OF TIME STEP';
  SWI_BudgetTerm = 'VOLUMETRIC SWI ZONE BUDGET FOR ENTIRE MODEL';
  AgBudgetTerm = 'VOLUMETRIC BUDGET FOR AGRICULTURAL FIELDS AT TIME STEP';
  StreamStartSearchTerm =
        'LAYER      ROW     COLUMN     STREAM    REACH      FLOW INTO    FLOW INTO      FLOW OUT OF';
  SfrMf6BudTerm = 'SFR-1 BUDGET FOR ENTIRE MODEL AT END OF TIME STEP';
  LakMf6BudTerm = 'LAK-1 BUDGET FOR ENTIRE MODEL AT END OF TIME STEP';
  MvfMf6BudTerm = 'WATER MOVER BUDGET FOR ENTIRE MODEL AT END OF TIME STEP';
  MawMf6BudTerm = 'MAW-NT_WELL BUDGET FOR ENTIRE MODEL AT END OF TIME STEP';
  UzfMf6BudTerm = 'UZF-1 BUDGET FOR ENTIRE MODEL AT END OF TIME STEP';
  SwrBudTerm = 'VOLUMETRIC SURFACE WATER BUDGET FOR ENTIRE MODEL';
  ISTBudTerm = 'MASS BUDGET FOR IST AT END OF TIME STEP';   
begin
  TempList:= TStringList.Create;
  try
    AssignFile(F, OpenDialog1.FileName);
    Reset(F);
    try
      ShouldStoreFile := False;
  //    ShouldStoreStreamFile := False;
      StartSearchTerm := BudgetStartLine;
      StopSearchTerm1 := 'TOTAL SIMULATION TIME';
      StopSearchTerm2 := 'TOTAL TIME';
      while not EOF(F) do
      begin
        Readln(F, S);
        if (Pos(StartSearchTerm, S) > 0) or (Pos(UnsatBudTerm, S) > 0)
          or (Pos(StreamUnsatBudTerm, S) > 0)
          or (Pos(StreamStartSearchTerm, S) > 0)
          or (Pos(SWI_BudgetTerm, S) > 0)
          or (Pos(AgBudgetTerm, S) > 0)
          or (Pos(SeawatComponent, S) > 0)
          or (Pos(SfrMf6BudTerm, S) > 0)
          or (Pos(LakMf6BudTerm, S) > 0)
          or (Pos(MvfMf6BudTerm, S) > 0)
          or (Pos(MawMf6BudTerm, S) > 0)
          or (Pos(UzfMf6BudTerm, S) > 0)
          or (Pos(SwrBudTerm, S) > 0)
          or (Pos(ISTBudTerm, S) > 0)
//          or ((AltBudgetStartLine <> '') and (Pos(AltBudgetStartLine, S) > 0))
          then
        begin
          ShouldStoreFile := True;
        end;
        for AltIndex := 0 to Length(AltBudgetStartLines) -1 do
        begin
          if Pos(AltBudgetStartLines[AltIndex], S) > 0 then
          begin
            ShouldStoreFile := True;
          end;

        end;

  //      if Pos(StreamStartSearchTerm, S) > 0 then
  //      begin
  //        ShouldStoreStreamFile := True;
  //      end;
        TimeUnitLine := Pos('MODEL TIME UNIT IS', S) > 0;
        if ShouldStoreFile or TimeUnitLine then
        begin
          TempList.Add(S)
        end;
        if (Pos(StopSearchTerm1, S) > 0) or (Pos(StopSearchTerm2, S) > 0) then
        begin
          ShouldStoreFile := False;
  //        ShouldStoreStreamFile := False;
        end;

      end;
    finally
      CloseFile(F);
    end;
    if FFileName <> '' then
    begin
      DeleteFile(FFileName);
    end;

    FFileName := ChangeFileExt(OpenDialog1.FileName, '.temp');
    TempList.SaveToFile(FFileName);
    ZBLStringList.FileName := FFileName;
  finally
    TempList.Free;
  end;


  LineIndex := 0;
  SearchTerm := 'MODEL TIME UNIT IS';
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  if LineIndex < 0 then
  begin
    LineIndex := 0;
    TimeUnitsString := GetMF6TimeUnit;
    if TimeUnitsString = '' then
    begin
      TimeUnitsString := '';
      frmModflowModelUnits.ShowModal;
      if frmModflowModelUnits.rgTimeUnits.ItemIndex >= 1 then
      begin
        TimeUnitsString := frmModflowModelUnits.rgTimeUnits.Items[frmModflowModelUnits.rgTimeUnits.ItemIndex];
      end;
    end;      
  end
  else
  begin
    CurrentLine := ZBLStringList.Strings[LineIndex];
    TimeUnitsString := GetStringBetween(CurrentLine, SearchTerm, '');
  end;


  SearchTerm := BudgetStartLine;
  BudgetSearchTerm := BudgetStartLine;
  LineIndex := GetNextLine(BudgetSearchTerm, LineIndex);
  if (LineIndex < 0) and (AltBudgetStartLines <> nil) then
  begin
    for AltIndex := 0 to Length(AltBudgetStartLines) -1 do
    begin
      BudgetSearchTerm := AltBudgetStartLines[AltIndex];
      LineIndex := 0;
      LineIndex := GetNextLine(BudgetSearchTerm, LineIndex);
      if LineIndex >= 0 then
      begin
        Break;
      end;
    end;
  end;

//  if (LineIndex < 0) and (AltBudgetStartLine <> '') then
//  begin
//    BudgetSearchTerm := AltBudgetStartLine;
//    LineIndex := 0;
//    LineIndex := GetNextLine(BudgetSearchTerm, LineIndex);
//  end;

  while LineIndex > -1 do
  begin
    CurrentLine := ZBLStringList.Strings[LineIndex];
    if Pos('Please check the', CurrentLine) > 0 then
    begin
      Inc(LineIndex);
      SearchTerm := BudgetSearchTerm;
      LineIndex := GetNextLine(SearchTerm, LineIndex);
      Continue;
    end;


    CumBudget := TBudget.Create;
    RateBudget := TBudget.Create;
    FBudgetList.Add(CumBudget);
    FBudgetList.Add(RateBudget);
    CumBudget.Zone := 'CUMULATIVE';
    RateBudget.Zone := 'RATES';

    TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP',
      'IN STRESS PERIOD');
    if TimeStepString = '' then
    begin
      TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP',
        ', STRESS PERIOD');
    end;

    CurrentLine := GetStringBetween(CurrentLine, TimeStepString, '');

    StressPeriodString := GetStringBetween(CurrentLine, 'STRESS PERIOD', '');

    CumBudget.StressPeriod := StressPeriodString;
    CumBudget.TimeStep := TimeStepString;
    RateBudget.StressPeriod := StressPeriodString;
    RateBudget.TimeStep := TimeStepString;

    StartLine := LineIndex + 1;
    SearchTerm := 'IN:';
    StartLine := GetNextLine(SearchTerm, StartLine + 1);

    SearchTerm := 'OUT:';
    StopLine := GetNextLine(SearchTerm, StartLine + 1);

    if StopLine > -1 then
    begin
      for InnerLineIndex := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[InnerLineIndex];
        if Pos('=', CurrentLine) > 0 then
        begin
          BudgetItem := GetStringBetween(CurrentLine, '', '=');
          if BudgetItem[1] = '0' then
          begin
            BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
          end;
          CurrentLine := GetStringBetween(CurrentLine, '=', '');

          CumBudgetItem := TBudgetItem.Create;
          RateBudgetItem := TBudgetItem.Create;

          CumBudget.InList.Add(CumBudgetItem);
          RateBudget.InList.Add(RateBudgetItem);

          CumBudgetItem.Name := BudgetItem;
          RateBudgetItem.Name := BudgetItem;

          Rate := GetStringBetween(CurrentLine, '', BudgetItem);
          CumBudgetItem.value := Rate;

          CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
          Rate := GetStringBetween(CurrentLine, '=', '');
          if Pos(' ', Rate) > 0 then
          begin
            Rate := GetStringBetween(Rate, '', ' ');
            BudgetItem := GetStringBetween(CurrentLine, Rate, '');
            if BudgetItem <> 'STORAGE' then
            begin
              CumBudgetItem.Name := BudgetItem;
              RateBudgetItem.Name := BudgetItem;
            end;
          end;
          RateBudgetItem.value := Rate;
        end;
      end;
    end;
    if StopLine > -1 then
    begin
      StartLine := StopLine + 1;
      SearchTerm := 'IN - OUT';
      StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;
      if StopLine > -1 then
      begin
        for InnerLineIndex := StartLine to StopLine do
        begin
          CurrentLine := ZBLStringList.Strings[InnerLineIndex];
          if Pos('=', CurrentLine) > 0 then
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '=');
            if BudgetItem[1] = '0' then
            begin
              BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
            end;
            CurrentLine := GetStringBetween(CurrentLine, '=', '');

            CumBudgetItem := TBudgetItem.Create;
            RateBudgetItem := TBudgetItem.Create;

            CumBudget.OutList.Add(CumBudgetItem);
            RateBudget.OutList.Add(RateBudgetItem);

            CumBudgetItem.Name := BudgetItem;
            RateBudgetItem.Name := BudgetItem;

            Rate := GetStringBetween(CurrentLine, '', BudgetItem);
            CumBudgetItem.value := Rate;

            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            Rate := GetStringBetween(CurrentLine, '=', '');
            if Pos(' ', Rate) > 0 then
            begin
              Rate := GetStringBetween(Rate, '', ' ');
              BudgetItem := GetStringBetween(CurrentLine, Rate, '');
              if BudgetItem <> 'STORAGE' then
              begin
                CumBudgetItem.Name := BudgetItem;
                RateBudgetItem.Name := BudgetItem;
              end;
            end;
            RateBudgetItem.value := Rate;
          end;
        end;
      end;
    end;

    if StopLine > -1 then
    begin
      SearchTerm := 'IN - OUT';
      LineIndex := GetNextLine(SearchTerm, StopLine + 1);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, '=', '');
        InMinusOut := GetStringBetween(CurrentLine, '', SearchTerm);
        CumBudget.InMinusOut := InMinusOut;

        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        InMinusOut := GetStringBetween(CurrentLine, '=', '');
        RateBudget.InMinusOut := InMinusOut;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'PERCENT DISCREPANCY';
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, '=', '');
        Discrepancy := GetStringBetween(CurrentLine, '', SearchTerm);
        CumBudget.Discrepancy := Discrepancy;

        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        Discrepancy := GetStringBetween(CurrentLine, '=', '');
        RateBudget.Discrepancy := Discrepancy;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'TOTAL TIME';
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex < 0 then
      begin
        SearchTerm := 'TOTAL SIMULATION TIME';
        LineIndex := GetNextLine(SearchTerm, StopLine);
      end;

      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        TimeStrings := TStringList.Create;
        try
          TimeStrings.Delimiter := ' ';
          TimeStrings.DelimitedText := CurrentLine;
          if TimeUnitsString = 'SECONDS' then
          begin
            CumBudget.Time := TimeStrings[0];
          end
          else if TimeUnitsString = 'MINUTES' then
          begin
            CumBudget.Time := TimeStrings[1];
          end
          else if TimeUnitsString = 'HOURS' then
          begin
            CumBudget.Time := TimeStrings[2];
          end
          else if TimeUnitsString = 'DAYS' then
          begin
            CumBudget.Time := TimeStrings[3];
          end
          else if TimeUnitsString = 'YEARS' then
          begin
            CumBudget.Time := TimeStrings[4];
          end;
          RateBudget.Time := CumBudget.Time;
        finally
          TimeStrings.Free;
        end;

      end
    end;

    if LineIndex < 0 then
    begin
      LineIndex := StopLine;
    end;

    SearchTerm := BudgetSearchTerm;
    LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
  end;

  Step := 0;
  LineIndex := 0;
  SearchTerm :=
    'LAYER      ROW     COLUMN     STREAM    REACH      FLOW INTO    FLOW INTO      FLOW OUT OF';
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  while LineIndex > -1 do
  begin
    Inc(Step);

    StreamBudget := TBudget.Create;
    FBudgetList.Add(StreamBudget);
    StreamBudget.Zone := 'Stream Flow';
    StreamBudget.InMinusOut := '0';
    StreamBudget.Discrepancy := '0';
    StreamBudget.StressPeriod := '0';
    StreamBudget.TimeStep := IntToStr(Step);

    StreamAquiferBudgetItem := TBudget.Create;
    FBudgetList.Add(StreamAquiferBudgetItem);
    StreamAquiferBudgetItem.Zone := 'Stream-Aquifer';
    StreamAquiferBudgetItem.InMinusOut := '0';
    StreamAquiferBudgetItem.Discrepancy := '0';
    StreamAquiferBudgetItem.StressPeriod := '0';
    StreamAquiferBudgetItem.TimeStep := IntToStr(Step);

    StartLine := LineIndex + 4;
    StopLine := StartLine - 1;
    LineIndex := StartLine;
    FoundEnd := False;
    while not FoundEnd do
    begin
      if (LineIndex > ZBLStringList.Count - 1)
        or (ZBLStringList[LineIndex] = '1')
        or (ZBLStringList[LineIndex] = '') then
      begin
        StopLine := LineIndex - 1;
        FoundEnd := True;
      end;
      Inc(LineIndex);
    end;

    for InnerLineIndex := StartLine to StopLine do
    begin
      CurrentLine := ZBLStringList.Strings[InnerLineIndex];

      Segment := Trim(Copy(CurrentLine, 37, 10));
      Reach := Trim(Copy(CurrentLine, 47, 10));

      BudgetItemPrefix := 'Segment: ' + Segment + '; Reach: ' + Reach + '; ';

      StreamBudgetItem := TBudgetItem.Create;
      StreamBudget.InList.Add(StreamBudgetItem);
      StreamBudgetItem.Name := BudgetItemPrefix + 'FLOW INTO STREAM REACH';
      Rate := Trim(Copy(CurrentLine, 65, 9));
      StreamBudgetItem.value := Rate;

      StreamBudgetItem := TBudgetItem.Create;
      StreamAquiferBudgetItem.InList.Add(StreamBudgetItem);
      StreamBudgetItem.Name := BudgetItemPrefix + 'FLOW INTO AQUIFER';
      Rate := Trim(Copy(CurrentLine, 79, 9));
      StreamBudgetItem.value := Rate;

      StreamBudgetItem := TBudgetItem.Create;
      StreamBudget.OutList.Add(StreamBudgetItem);
      StreamBudgetItem.Name := BudgetItemPrefix + 'FLOW OUT OF STREAM REACH';
      Rate := Trim(Copy(CurrentLine, 96, 9));
      StreamBudgetItem.value := Rate;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'TOTAL TIME';
      PriorLineIndex := LineIndex;
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        TimeStrings := TStringList.Create;
        try
          TimeStrings.Delimiter := ' ';
          TimeStrings.DelimitedText := CurrentLine;
          if TimeUnitsString = 'SECONDS' then
          begin
            StreamBudget.Time := TimeStrings[0];
          end
          else if TimeUnitsString = 'MINUTES' then
          begin
            StreamBudget.Time := TimeStrings[1];
          end
          else if TimeUnitsString = 'HOURS' then
          begin
            StreamBudget.Time := TimeStrings[2];
          end
          else if TimeUnitsString = 'DAYS' then
          begin
            StreamBudget.Time := TimeStrings[3];
          end
          else if TimeUnitsString = 'YEARS' then
          begin
            StreamBudget.Time := TimeStrings[4];
          end;
        finally
          TimeStrings.Free;
        end;

      end
      else
      begin
        LineIndex := PriorLineIndex;
      end;
    end;

    SearchTerm :=
      'LAYER      ROW     COLUMN     STREAM    REACH      FLOW INTO    FLOW INTO      FLOW OUT OF';
    LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
  end;

  LineIndex := 0;
  SearchTerm := UnsatBudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    UnsatBudTerm, 'UZF Cumulative Budget', 'UZF Budget Rates', True);

  LineIndex := 0;
  SearchTerm := AgBudgetTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    AgBudgetTerm, 'AG Cumulative Budget', 'AG Budget Rates', False);

  LineIndex := 0;
  SearchTerm := SfrMf6BudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    SfrMf6BudTerm, 'SFR Cumulative Budget', 'SFR Budget Rates', False);

  LineIndex := 0;
  SearchTerm := LakMf6BudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    LakMf6BudTerm, 'Lake Cumulative Budget', 'Lake Budget Rates', False);

  LineIndex := 0;
  SearchTerm := MvfMf6BudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    MvfMf6BudTerm, 'MVR Cumulative Budget', 'MVR Budget Rates', False);

  LineIndex := 0;
  SearchTerm := MawMf6BudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    MawMf6BudTerm, 'MAW Cumulative Budget', 'MAW Budget Rates', False);

  LineIndex := 0;
  SearchTerm := UzfMf6BudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    UzfMf6BudTerm, 'UZF Cumulative Budget', 'UZF Budget Rates', True);

  LineIndex := 0;
  SearchTerm := SwrBudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    SwrBudTerm, 'SWR Cumulative Budget', 'SWR Budget Rates', True);

  LineIndex := 0;
  SearchTerm := ISTBudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  ReadUzfBudget(TimeUnitsString, LineIndex, SearchTerm,
    ISTBudTerm, 'IST Cumulative Budget', 'IST Budget Rates', True);

  ZoneIndex := 1;
  repeat
    ZoneFound := False;
    LineIndex := 0;
    SearchTerm := SWI_BudgetTerm;
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    while LineIndex >= 0 do
    begin
      Inc(LineIndex);
      TimeLine := ZBLStringList.Strings[LineIndex];
      Inc(LineIndex);
      ZoneLine := ZBLStringList.Strings[LineIndex];
      SearchTerm := 'ZONE';
      ZoneLine := GetStringBetween(ZoneLine, SearchTerm, '');
      if TryStrToInt(ZoneLine, AZone) then
      begin
        if AZone = ZoneIndex then
        begin
          ZoneFound := True;
          TimeStepString := GetStringBetween(TimeLine,'TIME STEP','IN STRESS PERIOD');
          StressPeriodString := GetStringBetween(TimeLine,'IN STRESS PERIOD', '');

          CumBudget := TBudget.Create;
          FBudgetList.Add(CumBudget);
          CumBudget.Zone := 'SWI Cumulative Budget Zone ' + ZoneLine;
          CumBudget.InMinusOut := '0';
          CumBudget.Discrepancy := '0';
          CumBudget.StressPeriod := StressPeriodString;
          CumBudget.TimeStep := TimeStepString;

          RateBudget := TBudget.Create;
          FBudgetList.Add(RateBudget);
          RateBudget.Zone := 'SWI Budget Rates Zone ' + ZoneLine;
          RateBudget.InMinusOut := '0';
          RateBudget.Discrepancy := '0';
          RateBudget.StressPeriod := StressPeriodString;
          RateBudget.TimeStep := TimeStepString;

          ReadABudget(CumBudget, RateBudget, LineIndex, TimeUnitsString, False);
        end;
        
        SearchTerm := SWI_BudgetTerm;
        LineIndex := GetNextLine(SearchTerm, LineIndex);

      end;

    end;
    Inc(ZoneIndex);
  until not ZoneFound;
//  ReadUzfBudget(TimeStrings, TimeUnitsString, LineIndex, SearchTerm,
//    UnsatBudTerm, 'UZF Cumulative Budget', 'UZF Budget Rates');


  LineIndex := 0;
  SearchTerm := StreamUnsatBudTerm;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  while LineIndex > -1 do
  begin
    CurrentLine := ZBLStringList.Strings[LineIndex];
    TimeStepString := GetStringBetween(CurrentLine,SearchTerm,'STRESS PERIOD');
    StressPeriodString := GetStringBetween(CurrentLine,'STRESS PERIOD', '');

    CumBudget := TBudget.Create;
    FBudgetList.Add(CumBudget);
    CumBudget.Zone := 'Stream Unsat Zone Cum Bud';
    CumBudget.InMinusOut := '0';
    CumBudget.Discrepancy := '0';
    CumBudget.StressPeriod := StressPeriodString;
    CumBudget.TimeStep := TimeStepString;

    RateBudget := TBudget.Create;
    FBudgetList.Add(RateBudget);
    RateBudget.Zone := 'Stream Unsat Zone Bud Rates';
    RateBudget.InMinusOut := '0';
    RateBudget.Discrepancy := '0';
    RateBudget.StressPeriod := StressPeriodString;
    RateBudget.TimeStep := TimeStepString;

    StartLine := LineIndex + 4;

//    SearchTerm := 'IN:';
//    StartLine := GetNextLine(SearchTerm, StartLine + 1);

    SearchTerm := 'TOTAL OUT';
    StopLine := GetNextLine(SearchTerm, StartLine + 1)-1;

    for InnerLineIndex := StartLine to StopLine do
    begin
      CurrentLine := ZBLStringList.Strings[InnerLineIndex];
      if Pos('=', CurrentLine) > 0 then
      begin
        BudgetItem := GetStringBetween(CurrentLine, '', '=');
        if BudgetItem[1] = '0' then
        begin
          BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
        end;
        CurrentLine := GetStringBetween(CurrentLine, '=', '');

        CumBudgetItem := TBudgetItem.Create;
        RateBudgetItem := TBudgetItem.Create;

        CumBudget.InList.Add(CumBudgetItem);
        RateBudget.InList.Add(RateBudgetItem);

        CumBudgetItem.Name := BudgetItem;
        RateBudgetItem.Name := BudgetItem;

        Rate := GetStringBetween(CurrentLine, '', BudgetItem);
        CumBudgetItem.value := Rate;

        CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
        Rate := GetStringBetween(CurrentLine, '=', '');
        RateBudgetItem.value := Rate;
      end;
    end;

    if StopLine > -1 then
    begin
      StartLine := StopLine + 1;
      SearchTerm := 'TOTAL OUT';
      StopLine := GetNextLine(SearchTerm, StartLine);
      if StopLine > -1 then
      begin
        for InnerLineIndex := StartLine to StopLine do
        begin
          CurrentLine := ZBLStringList.Strings[InnerLineIndex];
          if Pos('=', CurrentLine) > 0 then
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '=');
            if BudgetItem[1] = '0' then
            begin
              BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
            end;
            CurrentLine := GetStringBetween(CurrentLine, '=', '');

            CumBudgetItem := TBudgetItem.Create;
            RateBudgetItem := TBudgetItem.Create;

            CumBudget.OutList.Add(CumBudgetItem);
            RateBudget.OutList.Add(RateBudgetItem);

            CumBudgetItem.Name := BudgetItem;
            RateBudgetItem.Name := BudgetItem;

            Rate := GetStringBetween(CurrentLine, '', BudgetItem);
            CumBudgetItem.value := Rate;

            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            Rate := GetStringBetween(CurrentLine, '=', '');
            RateBudgetItem.value := Rate;
          end;
        end;
      end;
    end;

    if StopLine > -1 then
    begin
      SearchTerm := 'IN - OUT';
      LineIndex := GetNextLine(SearchTerm, StopLine + 1);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, '=', '');
        InMinusOut := GetStringBetween(CurrentLine, '', SearchTerm);
        CumBudget.InMinusOut := InMinusOut;

        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        InMinusOut := GetStringBetween(CurrentLine, '=', '');
        RateBudget.InMinusOut := InMinusOut;
      end;
    end;


    if LineIndex > -1 then
    begin
      SearchTerm := 'PERCENT DISCREPANCY';
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, '=', '');
        Discrepancy := GetStringBetween(CurrentLine, '', SearchTerm);
        CumBudget.Discrepancy := Discrepancy;

        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        Discrepancy := GetStringBetween(CurrentLine, '=', '');
        RateBudget.Discrepancy := Discrepancy;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'TOTAL TIME';
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        TimeStrings := TStringList.Create;
        try
          TimeStrings.Delimiter := ' ';
          TimeStrings.DelimitedText := CurrentLine;
          if TimeUnitsString = 'SECONDS' then
          begin
            CumBudget.Time := TimeStrings[0];
          end
          else if TimeUnitsString = 'MINUTES' then
          begin
            CumBudget.Time := TimeStrings[1];
          end
          else if TimeUnitsString = 'HOURS' then
          begin
            CumBudget.Time := TimeStrings[2];
          end
          else if TimeUnitsString = 'DAYS' then
          begin
            CumBudget.Time := TimeStrings[3];
          end
          else if TimeUnitsString = 'YEARS' then
          begin
            CumBudget.Time := TimeStrings[4];
          end;
          RateBudget.Time := CumBudget.Time;
        finally
          TimeStrings.Free;
        end;

      end
    end;
    SearchTerm := StreamUnsatBudTerm;
    LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
  end
end;

procedure TfrmZoneBdgtReader.ReadSeawatFile;
const
  ElapsedTimeSearch = 'TOTAL ELAPSED TIME SINCE BEGINNING OF SIMULATION =';
var
  LineIndex: integer;
  SearchTerm: string;
  ComponentNumber: string;
  TransportStep: string;
  TimeStep: string;
  StressPeriod: string;
  CurrentLine: string;
  SoluteBudget: TBudget;
  ElapsedTime: string;
  StartLine: integer;
  StopLine: integer;
  BudgetItem: string;
  InBudgetItem, OutBudgetItem: TBudgetItem;
  InRate, OutRate: string;
  BudgetItemIndex: integer;
  Splitter: TStringList;
  procedure ReadAnItem;
  begin
    if Pos(':', CurrentLine) > 0 then
    begin
      BudgetItem := GetStringBetween(CurrentLine, '', ':');
      if BudgetItem[1] = '0' then
      begin
        BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
      end;

      CurrentLine := GetStringBetween(CurrentLine, ':', '');
      Splitter.DelimitedText := CurrentLine;
      InBudgetItem := TBudgetItem.Create;
      OutBudgetItem := TBudgetItem.Create;
      SoluteBudget.InList.Add(InBudgetItem);
      SoluteBudget.OutList.Add(OutBudgetItem);
      InBudgetItem.Name := BudgetItem;
      OutBudgetItem.Name := BudgetItem;

      if Splitter.Count = 2 then
      begin
        InRate := Splitter[0];
        OutRate := Splitter[1];
      end
      else
      begin
        Assert(Splitter.Count = 4);
        InRate := Splitter[0];
        OutRate := Splitter[2];
      end;
      InBudgetItem.value := InRate;
      OutBudgetItem.value := OutRate;
    end;
  end;
begin
  ReadModflowOrSeawatFile('MASS BUDGET FOR ENTIRE MODEL');

  Splitter := TStringList.Create;
  try
    Splitter.Delimiter := ' ';
    LineIndex := 0;
    SearchTerm := SeawatComponent;
    LineIndex := GetNextLine(SearchTerm, LineIndex);
    while LineIndex > -1 do
    begin
      CurrentLine := ZBLStringList.Strings[LineIndex];
      ComponentNumber := GetStringBetween(CurrentLine, SeawatComponent, '<<<<<<');

      LineIndex := GetNextLine(ElapsedTimeSearch, LineIndex);
      CurrentLine := ZBLStringList.Strings[LineIndex];
      ElapsedTime := GetStringBetween(CurrentLine, ElapsedTimeSearch, '');
      Splitter.DelimitedText := ElapsedTime;
      ElapsedTime := Splitter[0];
      LineIndex := GetNextLine(SeawatBudget, LineIndex);
      CurrentLine := ZBLStringList.Strings[LineIndex];
      TransportStep := GetStringBetween(CurrentLine, SeawatBudget, ', TIME STEP');
      TimeStep := GetStringBetween(CurrentLine, ', TIME STEP', ', STRESS PERIOD');
      StressPeriod := GetStringBetween(CurrentLine, ', STRESS PERIOD', '');

      SoluteBudget := TBudget.Create;
      FBudgetList.Add(SoluteBudget);
      SoluteBudget.Zone := 'Cumulative Mass Budget Component ' + ComponentNumber;
      SoluteBudget.InMinusOut := '0';
      SoluteBudget.Discrepancy := '0';
      SoluteBudget.StressPeriod := StressPeriod;
      SoluteBudget.TimeStep := TimeStep;
      SoluteBudget.TransportStep := TransportStep;
      SoluteBudget.Time := ElapsedTime;

  //    ReadABudget(CumBudget, RateBudget, LineIndex, TimeUnitsString);

      SearchTerm := '----------------             ----------------';
      StartLine := GetNextLine(SearchTerm, LineIndex + 1) + 1;

      SearchTerm := '---------------------------------------------------------------------------';
      StopLine := GetNextLine(SearchTerm, StartLine + 1)-1;

      for BudgetItemIndex := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[BudgetItemIndex];
        ReadAnItem;
      end;

      LineIndex := StopLine + 2;
      CurrentLine := ZBLStringList.Strings[LineIndex];
      ReadAnItem;

      LineIndex := LineIndex + 2;
      CurrentLine := ZBLStringList.Strings[LineIndex];
      SoluteBudget.InMinusOut := GetStringBetween(CurrentLine, 'NET (IN - OUT):', '');

      LineIndex := LineIndex + 1;
      CurrentLine := ZBLStringList.Strings[LineIndex];
      SoluteBudget.Discrepancy := GetStringBetween(CurrentLine, 'DISCREPANCY (PERCENT):', '');

      SearchTerm := SeawatComponent;
      LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
    end;
  finally
    Splitter.Free;
  end;          
end;

procedure TfrmZoneBdgtReader.ReadModflowFile;
begin
  ReadModflowOrSeawatFile('VOLUMETRIC BUDGET FOR ENTIRE MODEL');
end;

procedure TfrmZoneBdgtReader.ReadModflow6File;
var
  AltLines: TStringArray;
begin
  // With MODFLOW 6 version 1.1, VOLUME BUDGET
  // was replaced by VOLUMETRIC BUDGET.
  SetLength(AltLines, 2);
  AltLines[0] := 'VOLUME BUDGET FOR ENTIRE MODEL';
  AltLines[1] := 'MASS BUDGET FOR ENTIRE MODEL';
  ReadModflowOrSeawatFile('VOLUMETRIC BUDGET FOR ENTIRE MODEL',
    AltLines);
end;

procedure TfrmZoneBdgtReader.ReadGSFLOW;
var
  SearchTerm: string;
  LineIndex: integer;
  CurrentLine: string;
  TimeStepString, StressPeriodString: string;
  StartLine, StopLine, InnerLineIndex: integer;
  BudgetItem, Rate: string;
  InMinusOut, Discrepancy: string;
  CumBudget, RateBudget: TBudget;
  CumBudgetItem, RateBudgetItem: TBudgetItem;
  TimeStrings: TStringList;
  Month, Day, Year: string;
  wMonth, wDay, wYear: Word;
  Date: TDateTime;
  TimeString: string;
const
  BudgetStartLine = 'SUMMARY VOLUMETRIC BUDGET FOR GSFLOW';
begin
  SearchTerm := BudgetStartLine;
  LineIndex := 0;
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  while LineIndex > -1 do
  begin
    Inc(LineIndex);
    CumBudget := TBudget.Create;
    RateBudget := TBudget.Create;
    FBudgetList.Add(CumBudget);
    FBudgetList.Add(RateBudget);
    CumBudget.Zone := 'CUMULATIVE';
    RateBudget.Zone := 'RATES';

    CurrentLine := ZBLStringList.Strings[LineIndex];

    TimeString := GetStringBetween(CurrentLine, 'DATE:',
      'CUMULATIVE TIME STEP:');

    Inc(LineIndex);
    CurrentLine := ZBLStringList.Strings[LineIndex];


    StressPeriodString := GetStringBetween(CurrentLine, 'MODFLOW STRESS PERIOD', 'CURRENT TIME STEP:');
    TimeStepString := GetStringBetween(CurrentLine, 'CURRENT TIME STEP:',
      'ITERATIONS:');

    CumBudget.StressPeriod := StressPeriodString;
    CumBudget.TimeStep := TimeStepString;
    RateBudget.StressPeriod := StressPeriodString;
    RateBudget.TimeStep := TimeStepString;

    StartLine := LineIndex + 1;
    SearchTerm := 'IN';
    StartLine := GetNextLine(SearchTerm, StartLine + 1);

    SearchTerm := 'OUT';
    StopLine := GetNextLine(SearchTerm, StartLine + 1);

    if StopLine > -1 then
    begin
      for InnerLineIndex := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[InnerLineIndex];
        if Pos('=', CurrentLine) > 0 then
        begin
          BudgetItem := GetStringBetween(CurrentLine, '', '=');
          if BudgetItem[1] = '0' then
          begin
            BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
          end;
          CurrentLine := GetStringBetween(CurrentLine, '=', '');

          CumBudgetItem := TBudgetItem.Create;
          RateBudgetItem := TBudgetItem.Create;

          CumBudget.InList.Add(CumBudgetItem);
          RateBudget.InList.Add(RateBudgetItem);

          CumBudgetItem.Name := BudgetItem;
          RateBudgetItem.Name := BudgetItem;

          Rate := GetStringBetween(CurrentLine, '', BudgetItem);
          if Rate = '' then
          begin
            Rate := '0'
          end;

          CumBudgetItem.value := Rate;

          CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
          Rate := GetStringBetween(CurrentLine, '=', '');
          if Rate = '' then
          begin
            Rate := '0'
          end;
          RateBudgetItem.value := Rate;
        end;
      end;
    end;
    if StopLine > -1 then
    begin
      StartLine := StopLine + 1;
      SearchTerm := 'OVERALL BUDGET ERROR';
      StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;
      if StopLine > -1 then
      begin
        for InnerLineIndex := StartLine to StopLine do
        begin
          CurrentLine := ZBLStringList.Strings[InnerLineIndex];
          if Pos('=', CurrentLine) > 0 then
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '=');
            if BudgetItem[1] = '0' then
            begin
              BudgetItem := Trim(Copy(BudgetItem, 2, Length(BudgetItem)));
            end;
            CurrentLine := GetStringBetween(CurrentLine, '=', '');

            CumBudgetItem := TBudgetItem.Create;
            RateBudgetItem := TBudgetItem.Create;

            CumBudget.OutList.Add(CumBudgetItem);
            RateBudget.OutList.Add(RateBudgetItem);

            CumBudgetItem.Name := BudgetItem;
            RateBudgetItem.Name := BudgetItem;

            Rate := GetStringBetween(CurrentLine, '', BudgetItem);
            if Rate = '' then
            begin
              Rate := '0'
            end;
            CumBudgetItem.value := Rate;

            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            Rate := GetStringBetween(CurrentLine, '=', '');
            if Rate = '' then
            begin
              Rate := '0'
            end;
            RateBudgetItem.value := Rate;
          end;
        end;
      end;
    end;

    if StopLine > -1 then
    begin
      SearchTerm := 'OVERALL BUDGET ERROR';
      LineIndex := GetNextLine(SearchTerm, StopLine + 1);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, '=', '');
        InMinusOut := GetStringBetween(CurrentLine, '', SearchTerm);
        CumBudget.InMinusOut := InMinusOut;

        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        InMinusOut := GetStringBetween(CurrentLine, '=', '');
        RateBudget.InMinusOut := InMinusOut;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'PERCENT DISCREPANCY';
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, '=', '');
        Discrepancy := GetStringBetween(CurrentLine, '', SearchTerm);
        CumBudget.Discrepancy := Discrepancy;

        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        Discrepancy := GetStringBetween(CurrentLine, '=', '');
        RateBudget.Discrepancy := Discrepancy;
      end;
    end;

    TimeStrings := TStringList.Create;
    try
      TimeStrings.Delimiter := ' ';
      TimeStrings.DelimitedText := TimeString;
      if TimeStrings.Count = 3 then
      begin
        Month := TimeStrings[0];
        Day := TimeStrings[1];
        Year := TimeStrings[2];
        wMonth := StrToInt(Month);
        wDay := StrToInt(Day);
        wYear := StrToInt(Year);
        Date := EncodeDate(wYear, wMonth, wDay);
        CumBudget.Time := FloatToStr(Date);
        RateBudget.Time := CumBudget.Time;
      end;

    finally
      TimeStrings.Free;
    end;

{    if LineIndex > -1 then
    begin
      SearchTerm := 'TOTAL TIME';
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        CurrentLine := GetStringBetween(CurrentLine, SearchTerm, '');
        TimeStrings := TStringList.Create;
        try
          TimeStrings.Delimiter := ' ';
          TimeStrings.DelimitedText := CurrentLine;
          if TimeUnitsString = 'SECONDS' then
          begin
            CumBudget.Time := TimeStrings[0];
          end
          else if TimeUnitsString = 'MINUTES' then
          begin
            CumBudget.Time := TimeStrings[1];
          end
          else if TimeUnitsString = 'HOURS' then
          begin
            CumBudget.Time := TimeStrings[2];
          end
          else if TimeUnitsString = 'DAYS' then
          begin
            CumBudget.Time := TimeStrings[3];
          end
          else if TimeUnitsString = 'YEARS' then
          begin
            CumBudget.Time := TimeStrings[4];
          end;
          RateBudget.Time := CumBudget.Time;
        finally
          TimeStrings.Free;
        end;

      end
    end;  }


    SearchTerm := BudgetStartLine;
    LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
  end;
end;

procedure TfrmZoneBdgtReader.ReadFile;
//var
//  F: TextFile;
//  S: string;
begin
  if not (TSourceTypes(rgDataSource.ItemIndex) in [stMODFLOW,stSEAWAT2000]) then
  begin
    ZBLStringList.FileName := OpenDialog1.FileName;
//    AssignFile(F, OpenDialog1.FileName);
//    Reset(F);
//    try
//      while not EOF(F) do
//      begin
//        Readln(F, S);
//        ZBLStringList.Add(S)
//      end;
//    finally
//      CloseFile(F);
//    end;
//    if ZBLStringList.Count < 2 then
//    begin
//      ZBLStringList.LoadFromFile(OpenDialog1.FileName);
//    end;
  end;
end;

function TfrmZoneBdgtReader.GetMF6TimeUnit: string;
var
  AFileName: string;
  Lines: TStringList;
  LineIndex: Integer;
  ALine: String;
  LinePosition: Integer;
const
  TimeString = 'SIMULATION TIME UNIT IS ';
begin
  result := '';
  AFileName := ExtractFileDir(OpenDialog1.FileName);
  AFileName := IncludeTrailingPathDelimiter(AFileName) + 'mfsim.lst';
  if FileExists(AFileName) then
  begin
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(AFileName);
      for LineIndex := 0 to Lines.Count -1 do
      begin
        ALine := Lines[LineIndex];
        LinePosition := Pos(TimeString, ALine);
        if LinePosition > 0 then
        begin
          Result := Copy(ALine, LinePosition + Length(TimeString), MaxInt);
          Exit;
        end;
      end;
    finally
      Lines.Free;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.btnReadClick(Sender: TObject);
begin

  if OpenDialog1.Execute then
  begin
    frmZoneBdgtReader.Cursor := crHourglass;
    try
      sbZoomIn.Enabled := True;
      sbZoomOut.Enabled := True;
      sbPan.Enabled := True;
      Configure1.Enabled := True;
      miPrint.Enabled := True;
      miPrintPreview.Enabled := True;
      miPrintSetup.Enabled := True;

      ExplanationVisible := True;
      Caption := 'GW_Chart: ' + OpenDialog1.FileName;
      Save1.Enabled := True;
      btnSave.Enabled := True;
      clbIn.Enabled := True;
      clbOut.Enabled := True;
      clbNet.Enabled := True;
      sbImage.Enabled := True;
      sbFormat.Enabled := True;
      Saveasimage1.Enabled := True;
      FormatChart1.Enabled := True;
      comboZone.Enabled := True;
      comboZone.Color := clWindow;
      BudgetListClear;
      ZBLStringList.Clear;
      chartZONEBDGT.BottomAxis.Automatic := True;
      chartZONEBDGT.LeftAxis.Automatic := True;
      MarkedPoints.Clear;
      ReadFile;
      //    ZBLStringList.LoadFromFile(OpenDialog1.FileName);
      case TSourceTypes(rgDataSource.ItemIndex) of
        stZONEBDGT:
          begin
            ReadZonebdgtFile;
          end;
        stMODFLOW:
          begin
            ReadModflowFile;
          end;
        stModflow6:
          begin
            ReadModflow6File;
          end;
        stGWT:
          begin
            ReadMOC3DFile;
          end;
        stSUTRA97:
          begin
            ReadSUTRAFile;
          end;
        stSUTRA:
          begin
            ReadSutra2D3D_File;
          end;
        stSUTRA4:
          begin
            ReadSutra4_File;
          end;
        stMT3D:
          begin
            ReadMT3DFile;
          end;
        stHST3D:
          begin
            ReadHST3DFile;
          end;
        stSEAWAT2000:
          begin
            ReadSeawatFile;
          end;
        stGSFLOW:
          begin
            ReadGSFLOW;
          end;
      end;

      rgPlotTypeClick(nil);
    finally
      frmZoneBdgtReader.Cursor := crDefault;
    end;
  end;
end;

{ TBudget }

procedure TBudget.Clear;
var
  Index: Integer;
begin
  for Index := InList.Count - 1 downto 0 do
  begin
    TBudgetItem(InList[Index]).Free;
  end;
  for Index := OutList.Count - 1 downto 0 do
  begin
    TBudgetItem(OutList[Index]).Free;
  end;
  for Index := NetList.Count - 1 downto 0 do
  begin
    TBudgetItem(NetList[Index]).Free;
  end;
  InList.Clear;
  OutList.Clear;
  NetList.Clear;
end;

constructor TBudget.Create;
begin
  InList := TList.Create;
  OutList := TList.Create;
  NetList := TList.Create;
end;

destructor TBudget.Destroy;
begin
  Clear;
  InList.Free;
  OutList.Free;
  NetList.Free;
  inherited;
end;

function TBudget.GetDecay: double;
begin
  try
    if Decay = '' then
    begin
      result := 0;
    end
    else
    begin
      result := InternationalStrToFloat(Decay);
    end;
  except on EConvertError do
    begin
      result := 0;
    end;
  end;
end;

function TBudget.GetDiscrepancy: double;
begin
  try
    if Discrepancy = '' then
    begin
      result := 0;
    end
    else
    begin
      result := InternationalStrToFloat(Discrepancy);
    end;
  except on EConvertError do
    begin
      result := 0;
    end;
  end;
end;

function TBudget.GetFromInListByName(const AName: string): TBudgetItem;
begin
  result := GetFromListByName(InList, AName);
end;

function TBudget.GetFromListByName(const AList: TList;
  const AName: string): TBudgetItem;
var
  Index: integer;
  ABudgetItem: TBudgetItem;
begin
  result := nil;
  for Index := 0 to AList.Count - 1 do
  begin
    ABudgetItem := AList[Index];
    if ABudgetItem.Name = AName then
    begin
      result := ABudgetItem;
      Exit;
    end;
  end;
end;

function TBudget.GetFromOutListByName(const AName: string): TBudgetItem;
begin
  result := GetFromListByName(OutList, AName);
end;

function TBudget.GetFromNetListByName(const AName: string): TBudgetItem;
begin
  result := GetFromListByName(NetList, AName);
end;

function TBudget.GetInMinusOut: double;
begin
  try
    if InMinusOut = '' then
    begin
      result := 0;
    end
    else
    begin
      result := InternationalStrToFloat(InMinusOut);
    end;
  except on EConvertError do
    begin
      result := 0;
    end;
  end;
end;

function TBudget.GetStoredMass: double;
begin
  try
    if StoredMass = '' then
    begin
      result := 0;
    end
    else
    begin
      result := InternationalStrToFloat(StoredMass);
    end;
  except on EConvertError do
    begin
      result := 0;
    end;
  end;
end;

function TBudget.GetStressPeriod: integer;
begin
  try
    result := StrToInt(StressPeriod);
  except on EConvertError do
    begin
      result := 0;
    end;
  end;
end;

function TBudget.GetTimeStep: integer;
begin
  try
    result := StrToInt(TimeStep);
  except on EConvertError do
    begin
      result := 0;
    end;
  end;
end;

{ TBudgetItem }

function TBudgetItem.RealValue: double;
var
  DPosition: integer;
begin
  try
    DPosition := Pos('D', Value);
    if DPosition > 0 then
    begin
      Value[DPosition] := 'E';
    end;
    result := InternationalStrToFloat(Value);
  except on EConvertError do
    begin
      result := 0;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.rgPlotTypeClick(Sender: TObject);
var
  Index: integer;
  ABudget: TBudget;
begin
  comboZone.Items.Clear;
  for Index := 0 to FBudgetList.Count - 1 do
  begin
    ABudget := FBudgetList[Index];
    if comboZone.Items.IndexOf(ABudget.Zone) = -1 then
    begin
      comboZone.Items.Add(ABudget.Zone);
    end;
  end;
  if comboZone.Items.Count > 0 then
  begin
    comboZone.ItemIndex := 0;
  end;
  comboTimeStep.Enabled := (rgPlotType.ItemIndex = 0);
  if comboTimeStep.Enabled then
  begin
    comboTimeStep.Color := clWindow;
  end
  else
  begin
    comboTimeStep.Color := clBtnFace;
  end;
  comboZoneChange(nil);
  PlotResults;
end;

function TimeText(ABudget: TBudget): string;
begin
  if Pos('SUTRA', ABudget.Zone) > 0 then
  begin
    result := 'Time Step: ' + ABudget.TimeStep;
  end
  else
  begin
    result := 'Stress Period: ' + ABudget.StressPeriod
      + '; Time Step: ' + ABudget.TimeStep;
    if ABudget.Time <> '' then
    begin
      result := result + '; Time: ' + ABudget.Time;
    end;
  end;
  if Pos('MOC3D', ABudget.Zone) > 0 then
  begin
    result := result + '; Transport Step: ' + ABudget.TransportStep
      + '; Time: ' + ABudget.Time;
  end;
end;

procedure TfrmZoneBdgtReader.comboZoneChange(Sender: TObject);
var
  Index: integer;
  ABudget: TBudget;
  InnerIndex: Integer;
  ABudgetItem: TBudgetItem;
begin
  Screen.Cursor := crHourGlass;
  try
    comboTimeStep.Items.Clear;
    clbIn.Items.Clear;
    clbOut.Items.Clear;
    clbNet.Items.Clear;
    if comboZone.ItemIndex > -1 then
    begin
      for Index := 0 to FBudgetList.Count - 1 do
      begin
        ABudget := FBudgetList[Index];
        if comboZone.Text = ABudget.Zone then
        begin
          comboTimeStep.Items.Add(TimeText(ABudget));

          for InnerIndex := 0 to ABudget.Inlist.Count - 1 do
          begin
            ABudgetItem := ABudget.Inlist[InnerIndex];
            //if rzckIn.Items.IndexOf(ABudgetItem.Name) < 0 then
            if clbIn.Items.IndexOf(ABudgetItem.Name) < 0 then
            begin
              //rzckIn.Items.Add(ABudgetItem.Name);
              clbIn.Items.Add(ABudgetItem.Name);
            end;
          end;

          for InnerIndex := 0 to ABudget.Outlist.Count - 1 do
          begin
            ABudgetItem := ABudget.Outlist[InnerIndex];
            if clbOut.Items.IndexOf(ABudgetItem.Name) < 0 then
            begin
              clbOut.Items.Add(ABudgetItem.Name);
            end;
          end;

          for InnerIndex := 0 to ABudget.Netlist.Count - 1 do
          begin
            ABudgetItem := ABudget.Netlist[InnerIndex];
            if clbNet.Items.IndexOf(ABudgetItem.Name) < 0 then
            begin
              clbNet.Items.Add(ABudgetItem.Name);
            end;
          end;
        end;
      end;
      if comboTimeStep.Items.Count > 0 then
      begin
        comboTimeStep.ItemIndex := 0;
      end;
    end;
    cbInClick(Sender);
    cbOutClick(Sender);
    cbNetClick(Sender);

    PlotResults;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmZoneBdgtReader.BudgetListClear;
var
  Index: integer;
begin
  for Index := FBudgetList.Count - 1 downto 0 do
  begin
    TBudget(FBudgetList[Index]).Free;
  end;
  FBudgetList.Clear;
end;

function TfrmZoneBdgtReader.GetLineStorage(const Name: string): TLineStoredProperties;
var
  Index: integer;
  temp: TLineStoredProperties;
begin
  result := nil;
  for Index := 0 to StoredLineValues.Count -1 do
  begin
    temp := StoredLineValues[Index];
    if temp.Name = Name then
    begin
      result := Temp;
      Exit;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.PlotResults;
var
  Index, BudgetIndex, BudgetItemIndex: integer;
  BarSeries: TBarSeries;
  ABudget: TBudget;
  ABudgetItem: TBudgetItem;
  ALineSeries: TLineSeries;
  AColor: TColor;
  UseTime, UseTimeStep: boolean;
  BudgetTime: double;
  MyStyle : TSeriesPointerStyle;
  function BudgetLabel: string;
  begin
    if (ABudget.StressPeriod <> '') and (ABudget.TimeStep <> '') then
    begin
      result := 'Stress Period: ' + ABudget.StressPeriod
        + '; Time Step: ' + ABudget.TimeStep;
    end
    else
    begin
      result := '';
    end;

  end;
  procedure UpdateLineSeriesProperties;
  var
    LineSeriesProperties: TLineStoredProperties;
  begin
    LineSeriesProperties := GetLineStorage(ALineSeries.Title);
    if LineSeriesProperties = nil then
    begin
      LineSeriesProperties := TLineStoredProperties.Create;
      StoredLineValues.Add(LineSeriesProperties);
      LineSeriesProperties.Name := ALineSeries.Title;
      LineSeriesProperties.Color := AColor;
      LineSeriesProperties.SeriesPointerStyle := ALineSeries.Pointer.Style;
    end
    else
    begin
      AColor := LineSeriesProperties.Color;
      ALineSeries.Pointer.Style := LineSeriesProperties.SeriesPointerStyle;
    end;
  end;
begin
  MyStyle := High(TSeriesPointerStyle);
  UseTime := False;
  UseTimeStep := False;
  chartZONEBDGT.Title.Text.Clear;
  for Index := chartZONEBDGT.SeriesList.Count - 1 downto 0 do
  begin
    chartZONEBDGT.Series[Index].Free;
  end;
  chartZONEBDGT.RemoveAllSeries;
  case rgPlotType.ItemIndex of
    0: // bar chart
      begin
        chartZONEBDGT.BottomAxis.Title.Caption := '';
        chartZONEBDGT.BottomAxis.Labels := False;
        AColor := clTeeColor;
        BarSeries := TBarSeries.Create(self);
        BarSeries.XValues.Order := loNone;
        BarSeries.ParentChart := chartZONEBDGT;
        BarSeries.Marks.Visible := False;

        ABudget := nil;
        for Index := 0 to FBudgetList.Count - 1 do
        begin
          ABudget := FBudgetList[Index];
          if (ABudget.Zone = comboZone.Text)
            and (comboTimeStep.text = TimeText(ABudget)) then
          begin
            if Pos('Transport', comboTimeStep.text) > 0 then
            begin
              chartZONEBDGT.Title.Text.Add(ABudget.Zone + '; '
                + Copy(comboTimeStep.text, 1, Pos('Transport',
                  comboTimeStep.text) - 1));
              chartZONEBDGT.Title.Text.Add(
                Copy(comboTimeStep.text, Pos('Transport', comboTimeStep.text),
                Length(comboTimeStep.text)));
            end
            else if Pos('SUTRA', comboZone.text) > 0 then
            begin
              chartZONEBDGT.Title.Text.Add('Net Rate of Change in');
              if Pos('Mass', comboZone.text) > 0 then
              begin
                chartZONEBDGT.Title.Text.Add('Fluid Mass (Mass/s)')
              end
              else if Pos('Energy', comboZone.text) > 0 then
              begin
                chartZONEBDGT.Title.Text.Add('Energy (Energy/s)')
              end
              else
              begin
                chartZONEBDGT.Title.Text.Add('Transported Species (Mass/s)')
              end;
              chartZONEBDGT.Title.Text.Add(comboTimeStep.text);
            end
            else
            begin
              chartZONEBDGT.Title.Text.Add(ABudget.Zone + '; ' +
                comboTimeStep.text);
            end;
            break;
          end;
        end;
        if ABudget <> nil then
        begin
          //for Index := 0 to rzckIn.Items.Count - 1 do
          for Index := 0 to clbIn.Items.Count - 1 do
          begin
            //if rzckIn.ItemChecked[Index] then
            if clbIn.Checked[Index] then
            begin
              //ABudgetItem := ABudget.GetFromInListByName(rzckIn.Items[Index]);
              ABudgetItem := ABudget.GetFromInListByName(clbIn.Items[Index]);
              if ABudgetItem <> nil then
                //              if ABudget.InList.Count > Index then
              begin
                //                ABudgetItem := ABudget.InList[Index];
                if TSourceTypes(rgDataSource.ItemIndex) = stSUTRA97 {Pos('SUTRA',ABudget.Zone) > 0}
                  then
                begin
                  BarSeries.Add(ABudgetItem.RealValue, ABudgetItem.Name,
                    AColor);
                end
                else if TSourceTypes(rgDataSource.ItemIndex) in [stSUTRA, stSUTRA4] then
                begin
                  BarSeries.Add(ABudgetItem.RealValue, '+ ' + ABudgetItem.Name,
                    AColor);
                end
                else
                begin
                  BarSeries.Add(ABudgetItem.RealValue, 'In: ' +
                    ABudgetItem.Name, AColor);
                end;
                AColor := GetColor(AColor);
              end;
            end;
          end;

          if pnlOutBudget.Visible then
          begin
            for Index := 0 to clbOut.Items.Count - 1 do
            begin
              if clbOut.Checked[Index] then
              begin
                ABudgetItem := ABudget.GetFromOutListByName(clbOut.Items[Index]);
                if ABudgetItem <> nil then
                begin
                  if TSourceTypes(rgDataSource.ItemIndex) in [stSUTRA, stSUTRA4] then
                  begin
                    BarSeries.Add(ABudgetItem.RealValue, '- ' + ABudgetItem.Name,
                      AColor);
                  end
                  else
                  begin
                    BarSeries.Add(ABudgetItem.RealValue, 'Out: ' +
                      ABudgetItem.Name, AColor);
                  end;
                  AColor := GetColor(AColor);
                end;
              end;
            end;
          end;

          if pnlNetBudget.Visible then
          begin
            for Index := 0 to clbNet.Items.Count - 1 do
            begin
              if clbNet.Checked[Index] then
              begin
                ABudgetItem := ABudget.GetFromNetListByName(clbNet.Items[Index]);
                if ABudgetItem <> nil then
                begin
                  BarSeries.Add(ABudgetItem.RealValue, 'Net ' + ABudgetItem.Name,
                    AColor);
                  AColor := GetColor(AColor);
                end;
              end;
            end;
          end;

          if cbDecay.Checked then
          begin
            BarSeries.Add(ABudget.GetDecay, cbDecay.Caption, AColor);
            AColor := GetColor(AColor);
          end;

          if cbStoredMass.Checked then
          begin
            BarSeries.Add(ABudget.GetStoredMass, cbStoredMass.Caption, AColor);
            AColor := GetColor(AColor);
          end;

          if cbInOut.Checked then
          begin
            BarSeries.Add(ABudget.GetInMinusOut, cbInOut.Caption, AColor);
            AColor := GetColor(AColor);
          end;

          if cbDiscrepancy.Checked then
          begin
            BarSeries.Add(ABudget.GetDiscrepancy, cbDiscrepancy.Caption,
              AColor);
            // The following line will be needed if more options are added.
            // AColor := GetColor(AColor);
          end;

        end;

      end;
    1: // time series
      begin
        //        AnotherColor := clAqua;
        AColor := clAqua;
        if Pos('SUTRA', comboZone.text) > 0 then
        begin
          chartZONEBDGT.Title.Text.Add('Time Series: Net Rate of Change in');
          if Pos('Mass', comboZone.text) > 0 then
          begin
            chartZONEBDGT.Title.Text.Add('Fluid Mass (Mass/s)')
          end
          else if Pos('Energy', comboZone.text) > 0 then
          begin
            chartZONEBDGT.Title.Text.Add('Energy (Energy/s)')
          end
          else
          begin
            chartZONEBDGT.Title.Text.Add('Transported Species (Mass/s)')
          end;
        end
        else
        begin
          chartZONEBDGT.Title.Text.Add('Time Series: ' + comboZone.Text);
        end;
        chartZONEBDGT.BottomAxis.Labels := True;
        chartZONEBDGT.BottomAxis.Title.Caption := UpperCase('Stored Time Step');
        for BudgetItemIndex := 0 to clbIn.Items.Count - 1 do
        begin
          //if rzckIn.ItemChecked[BudgetItemIndex] then
          if clbIn.Checked[BudgetItemIndex] then
          begin
            AColor := GetColor(AColor);
            ALineSeries := TLineSeries.Create(self);
            IncrementStyle(MyStyle);
            ALineSeries.Pointer.Style := MyStyle;

            ALineSeries.OnGetMarkText := Series1GetMarkText;
            ALineSeries.OnClickPointer := Series1ClickPointer;
            ALineSeries.Marks.Visible := True;

            if (TSourceTypes(rgDataSource.ItemIndex) in [stSUTRA97, stHST3D]) then
            begin // SUTRA 97 or HST3D
              ALineSeries.Title := clbIn.Items[BudgetItemIndex];
              ALineSeries.Marks.Visible := False;
            end
            else if (TSourceTypes(rgDataSource.ItemIndex) in [stSUTRA, stSUTRA4]) then
            begin // SUTRA
              ALineSeries.Title := '+ ' + clbIn.Items[BudgetItemIndex];
              ALineSeries.Marks.Visible := False;
            end
            else
            begin
              ALineSeries.Title := 'In: ' + clbIn.Items[BudgetItemIndex];
            end;

            UpdateLineSeriesProperties;

            ALineSeries.LinePen.Color := AColor;
            ALineSeries.AreaColor := AColor;
            ALineSeries.AreaLinesPen.Color := AColor;
            ALineSeries.SeriesColor := AColor;

            ALineSeries.XValues.Order := loNone;
            ALineSeries.ParentChart := chartZONEBDGT;
            ALineSeries.Pointer.Visible := True;
            for BudgetIndex := 0 to FBudgetList.Count - 1 do
            begin
              ABudget := FBudgetList[BudgetIndex];
              if (ABudget.Zone = comboZone.Text) then
              begin
                BudgetTime := 0;
                if ABudget.Time <> '' then
                begin
                  UseTime := True;
                  BudgetTime := FortranStrToFloat(ABudget.Time);
                end
                else
                If ABudget.TimeStep <> '' then
                begin
                  UseTimeStep := True;
                  BudgetTime := BudgetIndex + 1;
                end;


                ABudgetItem :=
                  ABudget.GetFromInListByName(clbIn.Items[BudgetItemIndex]);
                if ABudgetItem = nil then
                begin
                  ALineSeries.AddY(0, '', clTeeColor);
                end
                else
                begin

                  if (ABudget.Time = '') and (ABudget.TimeStep = '') then
                  begin
                    ALineSeries.AddXY(BudgetIndex + 1, ABudgetItem.RealValue,
                      BudgetLabel, clTeeColor);
                  end
                  else
                  begin
                    ALineSeries.AddXY(BudgetTime, ABudgetItem.RealValue,
                      BudgetLabel, clTeeColor);
                  end;
                end;
              end;
            end;
          end;
        end;
        if pnlOutBudget.Visible then
        begin
          for BudgetItemIndex := 0 to clbOut.Items.Count - 1 do
          begin
            if clbOut.Checked[BudgetItemIndex] then
            begin
              AColor := GetColor(AColor);
              ALineSeries := TLineSeries.Create(self);
              IncrementStyle(MyStyle);
              ALineSeries.Pointer.Style := MyStyle;
              ALineSeries.OnGetMarkText := Series1GetMarkText;
              ALineSeries.OnClickPointer := Series1ClickPointer;
              ALineSeries.Marks.Visible := True;
              if (TSourceTypes(rgDataSource.ItemIndex) in [stSUTRA, stSUTRA4]) then
              begin // SUTRA or HST3D
                ALineSeries.Title := '- ' + clbOut.Items[BudgetItemIndex];
                ALineSeries.Marks.Visible := False;
              end
              else
              begin
                ALineSeries.Title := 'Out: ' + clbOut.Items[BudgetItemIndex];
              end;

              UpdateLineSeriesProperties;

              ALineSeries.XValues.Order := loNone;
              ALineSeries.LinePen.Color := AColor;
              ALineSeries.AreaColor := AColor;
              ALineSeries.AreaLinesPen.Color := AColor;
              ALineSeries.SeriesColor := AColor;
              ALineSeries.ParentChart := chartZONEBDGT;
              ALineSeries.Pointer.Visible := True;
              for BudgetIndex := 0 to FBudgetList.Count - 1 do
              begin
                ABudget := FBudgetList[BudgetIndex];
                BudgetTime := 0;
                if ABudget.Time <> '' then
                begin
                  UseTime := True;
                  BudgetTime := FortranStrToFloat(ABudget.Time);
                end
                else
                If ABudget.TimeStep <> '' then
                begin
                  UseTimeStep := True;
                  BudgetTime := BudgetIndex + 1;
                end;

                if (ABudget.Zone = comboZone.Text) then
                begin
                  ABudgetItem :=
                    ABudget.GetFromOutListByName(clbOut.Items[BudgetItemIndex]);
                  if ABudgetItem = nil then
                  begin
                    ALineSeries.AddY(0, '', clTeeColor);
                  end
                  else
                  begin
                    if (ABudget.Time = '') and (ABudget.TimeStep = '')  then
                    begin
                      ALineSeries.AddXY(BudgetIndex + 1, ABudgetItem.RealValue,
                        BudgetLabel, clTeeColor);
                    end
                    else
                    begin
                      ALineSeries.AddXY(BudgetTime, ABudgetItem.RealValue,
                        BudgetLabel, clTeeColor);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        if pnlNetBudget.Visible then
        begin
          for BudgetItemIndex := 0 to clbNet.Items.Count - 1 do
          begin
            if clbNet.Checked[BudgetItemIndex] then
            begin
              AColor := GetColor(AColor);
              ALineSeries := TLineSeries.Create(self);
              IncrementStyle(MyStyle);
              ALineSeries.Pointer.Style := MyStyle;
              ALineSeries.OnGetMarkText := Series1GetMarkText;
              ALineSeries.OnClickPointer := Series1ClickPointer;
              ALineSeries.Marks.Visible := False;
              ALineSeries.Title := 'Net: ' + clbNet.Items[BudgetItemIndex];

              UpdateLineSeriesProperties;

              ALineSeries.XValues.Order := loNone;
              ALineSeries.LinePen.Color := AColor;
              ALineSeries.AreaColor := AColor;
              ALineSeries.AreaLinesPen.Color := AColor;
              ALineSeries.SeriesColor := AColor;
              ALineSeries.ParentChart := chartZONEBDGT;
              ALineSeries.Pointer.Visible := True;
              for BudgetIndex := 0 to FBudgetList.Count - 1 do
              begin
                ABudget := FBudgetList[BudgetIndex];
                BudgetTime := 0;
                if ABudget.Time <> '' then
                begin
                  UseTime := True;
                  BudgetTime := FortranStrToFloat(ABudget.Time);
                end
                else
                If ABudget.TimeStep <> '' then
                begin
                  UseTimeStep := True;
                  BudgetTime := BudgetIndex + 1;
                end;
                if (ABudget.Zone = comboZone.Text) then
                begin
                  ABudgetItem :=
                    ABudget.GetFromNetListByName(clbNet.Items[BudgetItemIndex]);
                  if ABudgetItem = nil then
                  begin
                    ALineSeries.AddY(0, '', clTeeColor);
                  end
                  else
                  begin
                    if (ABudget.Time = '') and (ABudget.TimeStep = '') then
                    begin
                      ALineSeries.AddXY(BudgetIndex + 1, ABudgetItem.RealValue,
                        BudgetLabel, clTeeColor);
                    end
                    else
                    begin
                      ALineSeries.AddXY(BudgetTime, ABudgetItem.RealValue,
                        BudgetLabel, clTeeColor);
                    end;

                  end;
                end;
              end;
            end;
          end;
        end;
        if cbDecay.Checked then
        begin
          AColor := GetColor(AColor);
          ALineSeries := TLineSeries.Create(self);
          IncrementStyle(MyStyle);
          ALineSeries.Pointer.Style := MyStyle;
          ALineSeries.OnGetMarkText := Series1GetMarkText;
          ALineSeries.OnClickPointer := Series1ClickPointer;
          ALineSeries.Marks.Visible := False;
          ALineSeries.Title := cbDecay.Caption;

          UpdateLineSeriesProperties;

          ALineSeries.XValues.Order := loNone;
          ALineSeries.LinePen.Color := AColor;
          ALineSeries.AreaColor := AColor;
          ALineSeries.AreaLinesPen.Color := AColor;
          ALineSeries.SeriesColor := AColor;
          ALineSeries.ParentChart := chartZONEBDGT;
          ALineSeries.Pointer.Visible := True;
          for BudgetIndex := 0 to FBudgetList.Count - 1 do
          begin
            ABudget := FBudgetList[BudgetIndex];
            if (ABudget.Zone = comboZone.Text) then
            begin
              ALineSeries.AddY(ABudget.GetDecay, '', clTeeColor);
            end;
          end;
        end;
        if cbStoredMass.Checked then
        begin
          AColor := GetColor(AColor);
          ALineSeries := TLineSeries.Create(self);
          IncrementStyle(MyStyle);
          ALineSeries.Pointer.Style := MyStyle;
          ALineSeries.OnGetMarkText := Series1GetMarkText;
          ALineSeries.OnClickPointer := Series1ClickPointer;
          ALineSeries.Marks.Visible := False;
          ALineSeries.Title := cbStoredMass.Caption;

          UpdateLineSeriesProperties;

          ALineSeries.XValues.Order := loNone;
          ALineSeries.LinePen.Color := AColor;
          ALineSeries.AreaColor := AColor;
          ALineSeries.AreaLinesPen.Color := AColor;
          ALineSeries.SeriesColor := AColor;
          ALineSeries.ParentChart := chartZONEBDGT;
          ALineSeries.Pointer.Visible := True;
          for BudgetIndex := 0 to FBudgetList.Count - 1 do
          begin
            ABudget := FBudgetList[BudgetIndex];
            BudgetTime := 0;
            if ABudget.Time <> '' then
            begin
              UseTime := True;
              BudgetTime := FortranStrToFloat(ABudget.Time);
            end
            else
            If ABudget.TimeStep <> '' then
            begin
              UseTimeStep := True;
              BudgetTime := BudgetIndex + 1;
            end;
            if (ABudget.Zone = comboZone.Text) then
            begin
              if (ABudget.Time = '') and (ABudget.TimeStep = '') then
              begin
                ALineSeries.AddXY(BudgetIndex + 1, ABudget.GetStoredMass,
                  BudgetLabel, clTeeColor);
              end
              else
              begin
                ALineSeries.AddXY(BudgetTime, ABudget.GetStoredMass,
                  BudgetLabel, clTeeColor);
              end;

            end;
          end;
        end;
        if cbInOut.Checked then
        begin


          AColor := GetColor(AColor);
          ALineSeries := TLineSeries.Create(self);
          IncrementStyle(MyStyle);
          ALineSeries.Pointer.Style := MyStyle;
          ALineSeries.OnGetMarkText := Series1GetMarkText;
          ALineSeries.OnClickPointer := Series1ClickPointer;
          ALineSeries.Marks.Visible := False;
          ALineSeries.Title := cbInOut.Caption;

          UpdateLineSeriesProperties;

          ALineSeries.XValues.Order := loNone;
          ALineSeries.LinePen.Color := AColor;
          ALineSeries.AreaColor := AColor;
          ALineSeries.AreaLinesPen.Color := AColor;
          ALineSeries.SeriesColor := AColor;
          ALineSeries.ParentChart := chartZONEBDGT;
          ALineSeries.Pointer.Visible := True;
          for BudgetIndex := 0 to FBudgetList.Count - 1 do
          begin
            ABudget := FBudgetList[BudgetIndex];
            BudgetTime := 0;
            if ABudget.Time <> '' then
            begin
              UseTime := True;
              BudgetTime := FortranStrToFloat(ABudget.Time);
            end
            else
            If ABudget.TimeStep <> '' then
            begin
              UseTimeStep := True;
              BudgetTime := BudgetIndex + 1;
            end;
            if (ABudget.Zone = comboZone.Text) then
            begin
              if (ABudget.Time = '') and (ABudget.TimeStep = '') then
              begin
                ALineSeries.AddXY(BudgetIndex + 1, ABudget.GetInMinusOut,
                  BudgetLabel, clTeeColor);
              end
              else
              begin
                ALineSeries.AddXY(BudgetTime, ABudget.GetInMinusOut,
                  BudgetLabel, clTeeColor);
              end;

            end;
          end;
        end;
        if cbDiscrepancy.Checked then
        begin

          AColor := GetColor(AColor);
          ALineSeries := TLineSeries.Create(self);
          IncrementStyle(MyStyle);
          ALineSeries.Pointer.Style := MyStyle;
          ALineSeries.OnGetMarkText := Series1GetMarkText;
          ALineSeries.OnClickPointer := Series1ClickPointer;
          ALineSeries.Marks.Visible := False;
          ALineSeries.Title := cbDiscrepancy.Caption;

          UpdateLineSeriesProperties;

          ALineSeries.XValues.Order := loNone;
          ALineSeries.LinePen.Color := AColor;
          ALineSeries.AreaColor := AColor;
          ALineSeries.AreaLinesPen.Color := AColor;
          ALineSeries.SeriesColor := AColor;
          ALineSeries.ParentChart := chartZONEBDGT;
          ALineSeries.Pointer.Visible := True;
          for BudgetIndex := 0 to FBudgetList.Count - 1 do
          begin
            ABudget := FBudgetList[BudgetIndex];
            BudgetTime := 0;
            if ABudget.Time <> '' then
            begin
              UseTime := True;
              BudgetTime := FortranStrToFloat(ABudget.Time);
            end
            else
            If ABudget.TimeStep <> '' then
            begin
              UseTimeStep := True;
              BudgetTime := BudgetIndex + 1;
            end;
            if (ABudget.Zone = comboZone.Text) then
            begin
              if (ABudget.Time = '') and (ABudget.TimeStep = '') then
              begin
                ALineSeries.AddXY(BudgetIndex + 1, ABudget.GetDiscrepancy,
                  BudgetLabel, clTeeColor);
              end
              else
              begin
                ALineSeries.AddXY(BudgetTime, ABudget.GetDiscrepancy,
                  BudgetLabel, clTeeColor);
              end;
            end;
          end;
        end;
        if UseTime then
        begin
          chartZONEBDGT.BottomAxis.Title.Caption := UpperCase('Stored Time');
        end
        else if UseTimeStep then
        begin
          chartZONEBDGT.BottomAxis.Title.Caption := UpperCase('Stored Time Step');
        end;
      end;
  else
    begin
      ShowMessage('No plot type selected');
    end;
  end;
end;

function TfrmZoneBdgtReader.GetColor(AColor: TColor): TColor;
begin
  case AColor of
    clAqua: Result := clBlack;
    clBlack: Result := clBlue;
    clBlue: Result := clDkGray;
    clDkGray: Result := clFuchsia;
    clFuchsia: Result := clGreen;
    clGreen: Result := clLime;
    clLime: Result := clMaroon;
    clMaroon: Result := clNavy;
    clNavy: Result := clOlive;
    clOlive: Result := clPurple;
    clPurple: Result := clRed;
    clRed: Result := clTeal;
    clTeal: Result := clYellow;
    clYellow: Result := clAqua;
  else
    result := clAqua;
  end;

end;

procedure TfrmZoneBdgtReader.comboTimeStepChange(Sender: TObject);
begin
  PlotResults;
end;

procedure TfrmZoneBdgtReader.cbInOutClick(Sender: TObject);
begin
  PlotResults;
end;

procedure TfrmZoneBdgtReader.cbDiscrepancyClick(Sender: TObject);
begin
  PlotResults;
end;

procedure TfrmZoneBdgtReader.FillTitles(const InList, OutList: TStringList;
  const ZoneIndex: Integer);
var
  CurrentZone: string;
  BudgetIndex, BudgetItemIndex: Integer;
  ABudget: TBudget;
  ABudgetItem: TBudgetItem;
begin
  CurrentZone := comboZone.Items[ZoneIndex];
  for BudgetIndex := 0 to FBudgetList.Count - 1 do
  begin
    ABudget := FBudgetList[BudgetIndex];
    if (ABudget.Zone = CurrentZone) then
    begin
      for BudgetItemIndex := 0 to ABudget.InList.Count - 1 do
      begin
        ABudgetItem := ABudget.InList[BudgetItemIndex];
        if InList.IndexOf(ABudgetItem.Name) < 0 then
        begin
          InList.Add(ABudgetItem.Name);
        end;
      end;
      for BudgetItemIndex := 0 to ABudget.OutList.Count - 1 do
      begin
        ABudgetItem := ABudget.OutList[BudgetItemIndex];
        if OutList.IndexOf(ABudgetItem.Name) < 0 then
        begin
          OutList.Add(ABudgetItem.Name);
        end;
      end;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.btnSaveClick(Sender: TObject);
var
  AStringList: TStringList;
  ZoneIndex: integer;
  AString: string;
  BudgetIndex, BudgetItemIndex: Integer;
  CurrentZone: string;
  ABudget: TBudget;
  ABudgetItem: TBudgetItem;
  TitleString: string;
  FileName: string;
  Directory: string;
  InList, OutList: TStringList;
  Values: TStringList;
  TitlePrinted: boolean;
begin
  FileName := OpenDialog1.FileName;
  Directory := ExtractFilePath(FileName);
  FileName := ExtractFileName(FileName);
  if Pos('.', FileName) > 0 then
  begin
    FileName := Copy(FileName, 1, Pos('.', FileName) - 1);
  end;
  SaveDialog1.FileName := Directory + FileName + '.' + SaveDialog1.DefaultExt;
  if SaveDialog1.Execute then
  begin
    AStringList := TStringList.Create;
    try
      for ZoneIndex := 0 to comboZone.Items.Count - 1 do
      begin
        InList := TStringList.Create;
        OutList := TStringList.Create;
        try
          FillTitles(InList, OutList, ZoneIndex);
          CurrentZone := comboZone.Items[ZoneIndex];
          TitlePrinted := False;
          AStringList.Add('');
          AStringList.Add(CurrentZone);
          if Pos('SUTRA', CurrentZone) > 0 then
          begin
            TitleString := 'Time Step';
          end
          else
          begin
            TitleString := 'Stress Period' + Chr(9) + 'Time Step';
            if FBudgetList.Count > 0 then
            begin
              ABudget := FBudgetList[0];
              if ABudget.Time <> '' then
              begin
                TitleString := TitleString + Chr(9) + 'Time';
              end;
            end;
          end;
          if Pos('MOC3D', CurrentZone) > 0 then
          begin
            TitleString := TitleString + Chr(9) + 'Transport Step'
              + Chr(9) + 'Time';
          end;
          for BudgetIndex := 0 to FBudgetList.Count - 1 do
          begin
            ABudget := FBudgetList[BudgetIndex];
            if (ABudget.Zone = CurrentZone) then
            begin
              if Pos('SUTRA', CurrentZone) > 0 then
              begin
                AString := ABudget.TimeStep;
              end
              else
              begin
                AString := ABudget.StressPeriod + chr(9) + ABudget.TimeStep;
                if ABudget.Time <> '' then
                begin
                  AString := AString + Chr(9) + ABudget.Time;
                end;
              end;
              if Pos('MOC3D', CurrentZone) > 0 then
              begin
                AString := AString + Chr(9) + ABudget.TransportStep
                  + Chr(9) + ABudget.Time;
              end;

              Values := TStringList.Create;
              try
                while Values.Count < InList.Count do
                begin
                  Values.Add('0');
                end;

                for BudgetItemIndex := 0 to ABudget.InList.Count - 1 do
                begin
                  ABudgetItem := ABudget.InList[BudgetItemIndex];
                  Values[InList.IndexOf(ABudgetItem.Name)] := ABudgetItem.value;
                end;
                for BudgetItemIndex := 0 to Values.Count - 1 do
                begin
                  AString := AString + Chr(9) + Values[BudgetItemIndex];
                end;
              finally
                Values.Free;
              end;

              if not TitlePrinted  then
              begin
                for BudgetItemIndex := 0 to InList.Count - 1 do
                begin
                  TitleString := TitleString + Chr(9) + 'In: ' +
                    InList[BudgetItemIndex];
                end;
              end;

              Values := TStringList.Create;
              try
                while Values.Count < OutList.Count do
                begin
                  Values.Add('0');
                end;

                for BudgetItemIndex := 0 to ABudget.OutList.Count - 1 do
                begin
                  ABudgetItem := ABudget.OutList[BudgetItemIndex];
                  Values[OutList.IndexOf(ABudgetItem.Name)] :=
                    ABudgetItem.value;
                end;

                for BudgetItemIndex := 0 to Values.Count - 1 do
                begin
                  AString := AString + Chr(9) + Values[BudgetItemIndex];
                end;

              finally
                Values.Free;
              end;

              if not TitlePrinted then
              begin
                for BudgetItemIndex := 0 to {ABudget.} OutList.Count - 1 do
                begin
                  TitleString := TitleString + Chr(9) + 'Out: ' +
                    OutList[BudgetItemIndex];
                end;
              end;

              if Pos('MOC3D', CurrentZone) > 0 then
              begin
                AString := AString + Chr(9) + ABudget.Decay
                  + Chr(9) + ABudget.StoredMass;
              end;

//              if Pos('SUTRA', CurrentZone) > 0 then
              begin
                AString := AString + Chr(9) + ABudget.InMinusOut
                  + Chr(9) + ABudget.Discrepancy;
              end;

              if not TitlePrinted then
              begin
                if Pos('MOC3D', CurrentZone) > 0 then
                begin
                  TitleString := TitleString + Chr(9) + cbDecay.Caption
                    + Chr(9) + cbStoredMass.Caption;
                end;
//                if Pos('SUTRA', CurrentZone) > 0 then
                begin
                  TitleString := TitleString + Chr(9) + cbInOut.Caption
                    + Chr(9) + cbDiscrepancy.Caption;
                end;
                AStringList.Add(TitleString);
                TitlePrinted := True;
              end;

              AStringList.Add(AString);
            end;
          end;
        finally
          InList.Free;
          OutList.Free;
        end;
      end;
      AStringList.SaveToFile(SaveDialog1.FileName);
    finally
      AStringList.Free;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.About1Click(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmZoneBdgtReader.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmZoneBdgtReader.cbOutClick(Sender: TObject);
var
  Index: integer;
begin
  for Index := 0 to clbOut.Items.Count - 1 do
  begin
    clbOut.Checked[Index] := cbOut.Checked;
  end;
  PlotResults;
end;

procedure TfrmZoneBdgtReader.cbInClick(Sender: TObject);
var
  Index: integer;
begin
  //for Index := 0 to rzckIn.Items.Count - 1 do
  for Index := 0 to clbIn.Items.Count - 1 do
  begin
    //rzckIn.ItemChecked[Index] := cbIn.Checked;
    clbIn.Checked[Index] := cbIn.Checked;
  end;
  PlotResults;
end;

procedure TfrmZoneBdgtReader.rgDataSourceClick(Sender: TObject);
begin
  cbOut.Caption := '"Out" Budget Items';
  cbIn.Caption := '"In" Budget Items';
  case TSourceTypes(rgDataSource.ItemIndex) of
    stZONEBDGT: // ZONEBDGT
      begin
        OpenDialog1.Filter :=
          'Zonebudget Listing files (*.zbl, *.zblst, *.zb.lst, .lst)|*.zbl;*.zblst;*.zb.lst;*.lst|All Files (*.*)|*.*';
//          'Zonebudget Listing files (*.zbl)|*.zbl|All Files (*.*)|*.*';
        cbDecay.Enabled := False;
        cbDecay.Checked := False;
        cbStoredMass.Enabled := False;
        cbStoredMass.Checked := False;
        cbInOut.Enabled := True;
        cbInOut.Checked := True;
        cbDiscrepancy.Enabled := True;
        cbDiscrepancy.Checked := True;
        cbInOut.Caption := 'IN - OUT';
        cbOut.Visible := True;
        pnlOutBudget.Visible := True;
        cbIn.Caption := '"In" Budget Items';
        chartZONEBDGT.LeftAxis.Title.Caption := '';
        btnOpen.Hint := 'Open ZONEBDGT file';
        btnSave.Hint := 'Save ZONEBDGT data in tab-delimited format';
      end;
    stMODFLOW, stModflow6: // MODFLOW
      begin
        OpenDialog1.Filter :=
          'MODFLOW Listing files |*.lst;*.list;*.out;output.dat|All Files (*.*)|*.*';
        cbDecay.Enabled := False;
        cbDecay.Checked := False;
        cbStoredMass.Enabled := False;
        cbStoredMass.Checked := False;
        cbInOut.Enabled := True;
        cbInOut.Checked := True;
        cbDiscrepancy.Enabled := True;
        cbDiscrepancy.Checked := True;
        cbInOut.Caption := 'IN - OUT';
        cbOut.Visible := True;
        pnlOutBudget.Visible := True;
        cbIn.Caption := '"In" Budget Items';
        chartZONEBDGT.LeftAxis.Title.Caption := '';
        btnOpen.Hint := 'Open MODFLOW file';
        btnSave.Hint := 'Save MODFLOW data in tab-delimited format';
      end;
    stGWT: // MOC3D
      begin
        OpenDialog1.Filter :=
          'GWT or MOC3D Listing files|*.out;Moc3d.lst|All Files (*.*)|*.*';
        cbDecay.Enabled := True;
        cbDecay.Checked := True;
        cbStoredMass.Enabled := True;
        cbStoredMass.Checked := True;
        cbInOut.Enabled := True;
        cbInOut.Checked := True;
        cbDiscrepancy.Enabled := True;
        cbDiscrepancy.Checked := True;
        cbInOut.Caption := 'Residual';
        cbOut.Visible := True;
        pnlOutBudget.Visible := True;
        cbIn.Caption := '"In" Budget Items';
        chartZONEBDGT.LeftAxis.Title.Caption := '';
        btnOpen.Hint := 'Open MOC3D file';
        btnSave.Hint := 'Save MOC3D data in tab-delimited format';
      end;
    stSUTRA97:
      begin
        OpenDialog1.Filter :=
          'SUTRA Listing files (*.lst)|*.lst|All Files (*.*)|*.*';
        cbDecay.Enabled := False;
        cbDecay.Checked := False;
        cbStoredMass.Enabled := False;
        cbStoredMass.Checked := False;
        cbInOut.Enabled := False;
        cbInOut.Checked := False;
        cbDiscrepancy.Enabled := False;
        cbDiscrepancy.Checked := False;
        cbOut.Visible := False;
        cbOut.Checked := False;
        pnlOutBudget.Visible := False;
        cbIn.Caption := 'Budget Items';
        chartZONEBDGT.LeftAxis.Title.Caption :=
          UpperCase('Decrease(-) / Increase(+)');
        btnOpen.Hint := 'Open SUTRA file';
        btnSave.Hint := 'Save SUTRA data in tab-delimited format';
      end;
    stSUTRA, stSUTRA4:
      begin
        cbIn.Caption := '"+" Budget Items';
        cbOut.Caption := '"-" Budget Items';
        OpenDialog1.Filter :=
          'SUTRA Listing files (*.lst)|*.lst|All Files (*.*)|*.*';
        cbDecay.Enabled := False;
        cbDecay.Checked := False;
        cbStoredMass.Enabled := False;
        cbStoredMass.Checked := False;
        cbInOut.Enabled := True;
        cbInOut.Checked := True;
        cbDiscrepancy.Enabled := True;
        cbDiscrepancy.Checked := True;
        cbOut.Visible := True;
        cbOut.Checked := True;
        pnlOutBudget.Visible := True;
        chartZONEBDGT.LeftAxis.Title.Caption :=
          UpperCase('Decrease(-) / Increase(+)');
        btnOpen.Hint := 'Open SUTRA file';
        btnSave.Hint := 'Save SUTRA data in tab-delimited format';
      end;

    stMT3D: // MT3D
      begin
        OpenDialog1.Filter :=
          'MT3D Listing files(*.mls;*.out;output.mt3;*.m3d)|*.mls;*.out;output.mt3;*.m3d|All Files (*.*)|*.*';
        cbDecay.Enabled := False;
        cbDecay.Checked := False;
        cbStoredMass.Enabled := False;
        cbStoredMass.Checked := False;
        cbInOut.Enabled := True;
        cbInOut.Checked := True;
        cbDiscrepancy.Enabled := True;
        cbDiscrepancy.Checked := True;
        cbInOut.Caption := 'NET (IN - OUT)';
        cbOut.Visible := True;
        pnlOutBudget.Visible := True;
        cbIn.Caption := '"In" Budget Items';
        chartZONEBDGT.LeftAxis.Title.Caption := '';
        btnOpen.Hint := 'Open MT3D file';
        btnSave.Hint := 'Save MT3D data in tab-delimited format';
      end;
    stHST3D: // HST3D
      begin
        OpenDialog1.Filter :=
          'Global flow balance (O.bal.*)|O.bal.*|All Files (*.*)|*.*';
        cbDecay.Enabled := False;
        cbDecay.Checked := False;
        cbStoredMass.Enabled := False;
        cbStoredMass.Checked := False;
        cbInOut.Enabled := False;
        cbInOut.Checked := False;
        cbDiscrepancy.Enabled := True;
        cbDiscrepancy.Checked := True;
        cbDiscrepancy.Caption := 'Fractional Imbalance';
        cbInOut.Caption := 'Residual Imbalance';
        cbOut.Visible := False;
        cbOut.Checked := False;
        pnlOutBudget.Visible := False;
        cbIn.Caption := 'Budget Items';
        chartZONEBDGT.LeftAxis.Title.Caption := '';
        btnOpen.Hint := 'Open HST3D file';
        btnSave.Hint := 'Save HST3D data in tab-delimited format';
      end;
    stSEAWAT2000: // SEAWAT-2000
      begin
        OpenDialog1.Filter :=
          'SEAWAT Listing files |*.lst;*.out;output.dat|All Files (*.*)|*.*';
        cbDecay.Enabled := False;
        cbDecay.Checked := False;
        cbStoredMass.Enabled := False;
        cbStoredMass.Checked := False;
        cbInOut.Enabled := True;
        cbInOut.Checked := True;
        cbDiscrepancy.Enabled := True;
        cbDiscrepancy.Checked := True;
        cbInOut.Caption := 'IN - OUT';
        cbOut.Visible := True;
        pnlOutBudget.Visible := True;
        cbIn.Caption := '"In" Budget Items';
        chartZONEBDGT.LeftAxis.Title.Caption := '';
        btnOpen.Hint := 'Open SEAWAT-2000 file';
        btnSave.Hint := 'Save SEAWAT-2000 data in tab-delimited format';
      end;
    stGSFLOW: // GSFLOW
      begin
        OpenDialog1.Filter :=
          'GSFLOW files |*.lst;*.out;output.dat|All Files (*.*)|*.*';
        cbDecay.Enabled := False;
        cbDecay.Checked := False;
        cbStoredMass.Enabled := False;
        cbStoredMass.Checked := False;
        cbInOut.Enabled := True;
        cbInOut.Checked := True;
        cbDiscrepancy.Enabled := True;
        cbDiscrepancy.Checked := True;
        cbInOut.Caption := 'BUDGET ERROR';
        cbOut.Visible := True;
        pnlOutBudget.Visible := True;
        cbIn.Caption := '"In" Budget Items';
        chartZONEBDGT.LeftAxis.Title.Caption := '';
        btnOpen.Hint := 'Open GSFLOW file';
        btnSave.Hint := 'Save GSFLOW data in tab-delimited format';
      end;
    else Assert(False);
  end;
  FormResize(Sender);
end;

procedure TfrmZoneBdgtReader.cbDecayClick(Sender: TObject);
begin
  PlotResults;
end;

procedure TfrmZoneBdgtReader.cbStoredMassClick(Sender: TObject);
begin
  PlotResults;
end;

procedure TfrmZoneBdgtReader.FormResize(Sender: TObject);
begin
  if TSourceTypes(rgDataSource.ItemIndex) in [stSUTRA, stSUTRA4] then
  begin
    pnlNetBudget.Visible := True;
    splNet.Visible := True;

    pnlNetBudget.Align := alNone;
    pnlInBudget.Align := alNone;
    pnlOutBudget.Align := alNone;

    pnlOutBudget.Top := pnlBudgetItems.Height div 3 * 2;
    pnlOutBudget.Height := pnlOutBudget.Top div 2;
    pnlOutBudget.Left := 0;

    pnlInBudget.Height := pnlOutBudget.Height;
    pnlInBudget.Top := pnlBudgetItems.Height div 3;
    pnlInBudget.Left := 0;

    pnlNetBudget.Height := pnlOutBudget.Height;
    pnlNetBudget.Top := 0;
    pnlNetBudget.Left := 0;

    pnlOutBudget.Align := alBottom;
    splBudget.Align := alBottom;
    splBudget.Top := 0;

    pnlNetBudget.Align := alTop;
    splNet.Align := alTop;
    splNet.Top := pnlNetBudget.Height + 10;

    pnlInBudget.Align := alClient;
    splBudget.Height := 3;
    splNet.Height := 3;
  end
  else
  begin
    pnlNetBudget.Visible := False;
    splNet.Visible := False;
    pnlInBudget.Align := alNone;
    pnlOutBudget.Align := alNone;
    pnlOutBudget.Left := pnlBudgetItems.Width div 2;
    pnlOutBudget.Width := pnlOutBudget.Left;
    pnlOutBudget.Top := 0;
    pnlInBudget.Width := pnlOutBudget.Width;
    pnlOutBudget.Align := alRight;
    splBudget.Align := alRight;
    pnlInBudget.Align := alClient;
    splBudget.Left := 0;
    splBudget.Width := 3;
  end;

end;

procedure SetDecimalSign;
var
  CommaSeparator: array[0..255] of Char;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SDECIMAL, @CommaSeparator, 255);
  DecimalSign := CommaSeparator[0];
end;

procedure TfrmZoneBdgtReader.sbImageClick(Sender: TObject);
var
  OldFilter: string;
  OldDefaultExtension: string;
  OldFilterIndex: integer;
begin
  OldFilter := SaveDialog1.Filter;
  OldDefaultExtension := SaveDialog1.DefaultExt;
  OldFilterIndex := SaveDialog1.FilterIndex;
  try
    SaveDialog1.Filter :=
      'Bitmap (*.bmp)|*.bmp|Enhanced Windows Metafile (*.emf)|*.emf|Windows Metafile (*.wmf)|*.wmf';
    SaveDialog1.DefaultExt := 'wmf';
    SaveDialog1.FilterIndex := 3;
    if SaveDialog1.Execute then
    begin
      if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.bmp' then
      begin
        chartZONEBDGT.SaveToBitmapFile(SaveDialog1.FileName);
      end
      else if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.emf' then
      begin
        chartZONEBDGT.SaveToMetafileEnh(SaveDialog1.FileName);
      end
      else if LowerCase(ExtractFileExt(SaveDialog1.FileName)) = '.wmf' then
      begin
        chartZONEBDGT.SaveToMetafile(SaveDialog1.FileName);
      end;
    end;
  finally
    SaveDialog1.Filter := OldFilter;
    SaveDialog1.DefaultExt := OldDefaultExtension;
    SaveDialog1.FilterIndex := OldFilterIndex;
  end;

end;

procedure TfrmZoneBdgtReader.StoreLineSeriesValues;
var
  Index: integer;
  ASeries: TChartSeries;
  StoredProperties: TLineStoredProperties;
begin
  for Index := 0 to chartZONEBDGT.SeriesList.Count -1 do
  begin
    ASeries := chartZONEBDGT.SeriesList.Items[Index];
    if ASeries is TLineSeries then
    begin
      StoredProperties := GetLineStorage(ASeries.Title);
      if StoredProperties <> nil then
      begin
        StoredProperties.Color := ASeries.Color;
        StoredProperties.SeriesPointerStyle := TLineSeries(ASeries).Pointer.Style;
      end;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.sbFormatClick(Sender: TObject);
begin
  mHHelp.ChmFile := ChartHelpFileName;
  try
    ChartEditor1.Execute;
  finally
    mHHelp.ChmFile := HelpFileName;
  end;
  StoreLineSeriesValues;
end;

procedure TfrmZoneBdgtReader.ToolBar1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TfrmZoneBdgtReader.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Visible then
  begin
    frmModChart.Close
  end;
end;

procedure TfrmZoneBdgtReader.FormShow(Sender: TObject);
begin
  MainMenu1.Merge(frmModChart.mainMenuFormChoice);
end;

procedure TfrmZoneBdgtReader.chartZONEBDGTGetLegendPos(
  Sender: TCustomChart; Index: Integer; var X, Y, XColor: Integer);
var
  ExplanationHeight: integer;
  TempFont: TFont;
begin
  TempFont := TFont.Create;
  try
    TempFont.Assign(chartZONEBDGT.Canvas.Font);
    try
      chartZONEBDGT.Canvas.Font.Assign(chartZONEBDGT.Legend.Font);
      ExplanationHeight := chartZONEBDGT.Canvas.TextHeight('EXPLANATION');
    finally
      chartZONEBDGT.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
  Y := Y + ExplanationHeight;
end;

procedure TfrmZoneBdgtReader.chartZONEBDGTGetLegendRect(
  Sender: TCustomChart; var Rect: TRect);
var
  ExplanationWidth: integer;
  ExplanationHeight: integer;
  TempFont: TFont;
begin
  LegendRect := Rect;
  ExplanationTop := Rect.Top + 5;
  ExplanationLeft := Rect.Left + 5;
  TempFont := TFont.Create;
  try
    TempFont.Assign(chartZONEBDGT.Canvas.Font);
    try
      chartZONEBDGT.Canvas.Font.Assign(chartZONEBDGT.Legend.Font);
      ExplanationWidth := chartZONEBDGT.Canvas.TextWidth('_EXPLANATION_');
      ExplanationHeight := chartZONEBDGT.Canvas.TextHeight('EXPLANATION');
    finally
      chartZONEBDGT.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
  if Rect.Right - Rect.Left < ExplanationWidth then
  begin
    Rect.Right := Rect.Left + ExplanationWidth;
  end;
  Rect.Bottom := Rect.Bottom + ExplanationHeight;
end;

procedure TfrmZoneBdgtReader.chartZONEBDGTAfterDraw(Sender: TObject);
var
  TempFont: TFont;
begin
  if ExplanationVisible and chartZONEBDGT.Legend.Visible then
  begin
    TempFont := TFont.Create;
    try
      TempFont.Assign(chartZONEBDGT.Canvas.Font);
      try
        chartZONEBDGT.Canvas.Font.Assign(chartZONEBDGT.Legend.Font);
        chartZONEBDGT.Canvas.TextOut(ExplanationLeft, ExplanationTop,
          'EXPLANATION');
      finally
        chartZONEBDGT.Canvas.Font.Assign(TempFont);
      end;
    finally
      TempFont.Free;
    end;
  end;
end;

procedure TfrmZoneBdgtReader.Help2Click(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfrmZoneBdgtReader.sbZoomInClick(Sender: TObject);
begin
  inherited;
  if Sender <> sbZoomIn then
  begin
    sbZoomIn.Down := True;
  end;
  chartZONEBDGT.AllowZoom := sbZoomIn.Down;
end;

procedure TfrmZoneBdgtReader.sbZoomOutClick(Sender: TObject);
begin
  inherited;
  chartZONEBDGT.BottomAxis.Automatic := True;
  chartZONEBDGT.LeftAxis.Automatic := True;
  chartZONEBDGT.UndoZoom;
  sbZoomIn.Down := False;
  sbPan.Down := False;
end;

procedure TfrmZoneBdgtReader.sbPanClick(Sender: TObject);
begin
  inherited;
  if Sender <> sbPan then
  begin
    sbPan.Down := not sbPan.Down;
  end;
  if sbPan.Down then
  begin
    miPan.Caption := 'Stop Panning';
  end
  else
  begin
    Begun := False;
    Screen.Cursor := crDefault;
    miPan.Caption := 'Pan';
  end;
  chartZONEBDGT.AllowZoom := sbZoomIn.Down;
end;

procedure TfrmZoneBdgtReader.chartZONEBDGTMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if sbPan.Down then
  begin
    Begun := True;
    XStart := chartZONEBDGT.BottomAxis.CalcPosPoint(X);
    YStart := chartZONEBDGT.LeftAxis.CalcPosPoint(Y);
  end;
end;

procedure TfrmZoneBdgtReader.chartZONEBDGTMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if sbPan.Down then
  begin
    Screen.Cursor := crHandPoint;
  end
  else if sbZoomIn.Down then
  begin
    Screen.Cursor := crCross;
  end
  else
  begin
    Screen.Cursor := crDefault;
  end;
  if sbPan.Down and Begun then
  begin
    XEnd := chartZONEBDGT.BottomAxis.CalcPosPoint(X);
    YEnd := chartZONEBDGT.LeftAxis.CalcPosPoint(Y);
    chartZONEBDGT.BottomAxis.Scroll(XStart - XEnd, False);
    chartZONEBDGT.LeftAxis.Scroll(YStart - YEnd, False);
  end;
end;

procedure TfrmZoneBdgtReader.chartZONEBDGTMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if sbPan.Down and Begun then
  begin
    XEnd := chartZONEBDGT.BottomAxis.CalcPosPoint(X);
    YEnd := chartZONEBDGT.LeftAxis.CalcPosPoint(Y);
    chartZONEBDGT.BottomAxis.Scroll(XStart - XEnd, False);
    chartZONEBDGT.LeftAxis.Scroll(YStart - YEnd, False);
    Begun := False;
  end;
end;

procedure TfrmZoneBdgtReader.miPrintSetupClick(Sender: TObject);
begin
  inherited;
  PrinterSetupDialog1.Execute;
end;

procedure TfrmZoneBdgtReader.miPrintPreviewClick(Sender: TObject);
begin
  inherited;
  ChartPreviewer1.Execute;
end;

procedure TfrmZoneBdgtReader.miPrintClick(Sender: TObject);
begin
  inherited;
  chartZONEBDGT.print;
end;

procedure TfrmZoneBdgtReader.cbNetClick(Sender: TObject);
var
  Index: integer;
begin
  for Index := 0 to clbNet.Items.Count - 1 do
  begin
    clbNet.Checked[Index] := cbNet.Checked;
  end;
  PlotResults;
end;

procedure TfrmZoneBdgtReader.Splitter1Moved(Sender: TObject);
begin
  inherited;
  FormResize(Sender);
end;

procedure TfrmZoneBdgtReader.clbInClickCheck(Sender: TObject);
begin
  inherited;
  PlotResults;
end;

procedure TfrmZoneBdgtReader.clbOutClickCheck(Sender: TObject);
begin
  inherited;
  PlotResults
end;

procedure TfrmZoneBdgtReader.clbNetClickCheck(Sender: TObject);
begin
  inherited;
  PlotResults
end;

function TfrmZoneBdgtReader.TimeStepLength(const StressPeriod,
  TimeStep: integer): double;
var
  FirstStep: double;
begin
  if TSMULT[StressPeriod-1] = 1 then
  begin
    result := PERLEN[StressPeriod-1] / NSTEP[StressPeriod-1];
  end
  else
  begin
    FirstStep := PERLEN[StressPeriod-1] * (TSMULT[StressPeriod-1] - 1)
      / (IntPower(TSMULT[StressPeriod-1], NSTEP[StressPeriod-1]) - 1);
    if TimeStep > 1 then
    begin
      result := FirstStep * IntPower(TSMULT[StressPeriod-1],(TimeStep-1))
    end
    else
    begin
      result := FirstStep
    end;
  end;
end;

{ TLineStoredProperties }

procedure TLineStoredProperties.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TLineStoredProperties.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TLineStoredProperties.SetSeriesPointerStyle(
  const Value: TSeriesPointerStyle);
begin
  FSeriesPointerStyle := Value;
end;

procedure TfrmZoneBdgtReader.Series1GetMarkText(Sender: TChartSeries;
  ValueIndex: Integer; var MarkText: String);
begin
  inherited;
  if (MarkedPoints.IndexOf(ValueIndex) < 0) then
  begin
    MarkText := '';
  end;
end;

procedure TfrmZoneBdgtReader.Series1ClickPointer(Sender: TCustomSeries;
  ValueIndex, X, Y: Integer);
var
  Location : integer;
begin
  Location := MarkedPoints.IndexOf(ValueIndex);
  if Location > -1 then
  begin
    MarkedPoints.Delete(Location);
  end
  else
  begin
    MarkedPoints.Add(ValueIndex);
  end;
  chartZONEBDGT.Invalidate;
end;

procedure TfrmZoneBdgtReader.IncrementStyle(var MyStyle : TSeriesPointerStyle);
begin
  if MyStyle = High(TSeriesPointerStyle) then
  begin
    MyStyle := Low(TSeriesPointerStyle)
  end
  else
  begin
    Inc(MyStyle);
  end;
  while MyStyle in [psSmallDot, psNothing] do
  begin
    Inc(MyStyle);
  end;
end;

{ TLineReader }

procedure TLineReader.Clear;
begin
  FFilePositions.Clear;
  FCurrentLines.Clear;
  FLineCounts.Clear;
  FFileName := '';
  FCurrentStartLine := -1;
  FCurrentSection := -1
end;

constructor TLineReader.Create;
begin
  inherited;
  FFilePositions := TInt64List.Create;
  FCurrentLines := TStringList.Create;
  FLineCounts := TInt64List.Create;
  FCurrentSection := -1;
end;

destructor TLineReader.Destroy;
begin
  FFilePositions.Free;
  FCurrentLines.Free;
  FLineCounts.Free;
  inherited;
end;

function TLineReader.GetCount: integer;
var
  Index: integer;
begin
   result := 0;
  for Index := 0 to FLineCounts.Count-1 do
  begin
    result := result + FLineCounts[Index];
  end;
end;

Const
  MaxBytesToRead = 1024*1024*16;

function TLineReader.GetLine(Index: integer): string;
var
  StartPosition: Int64;
  FileStream: TFileStream;
  MemoryStream: TMemoryStream;
  EndPosition: Int64;
  SectionIndex: integer;
  LineCount: integer;
  SizeToRead: Int64;
begin
  Assert(Index >= 0);
  Assert(Index < Count);
  if (FCurrentSection < 0) or (Index < FCurrentStartLine)
    or (Index >= FCurrentStartLine + FLineCounts[FCurrentSection]) then
  begin
    LineCount := 0;
    FCurrentSection := -1;
    Assert(FLineCounts.Count = FFilePositions.Count);
    for SectionIndex := 0 to FLineCounts.Count -1 do
    begin
      LineCount := LineCount + FLineCounts[SectionIndex];
      if LineCount > Index then
      begin
        FCurrentSection := SectionIndex;
        break;
      end;
    end;
    Assert(FCurrentSection >= 0);
    FCurrentStartLine := LineCount - FLineCounts[FCurrentSection];
    StartPosition := FFilePositions[FCurrentSection];
    FCurrentLines.Clear;
    FileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
      EndPosition := FileStream.Size;
      if FCurrentSection+1 < FFilePositions.Count then
      begin
        EndPosition := FFilePositions[FCurrentSection+1];
      end;
      Assert(EndPosition >= StartPosition);
      SizeToRead := EndPosition - StartPosition;
      Assert(SizeToRead < MaxBytesToRead*2);
      FileStream.Position := StartPosition;
      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.CopyFrom(FileStream, SizeToRead);
        MemoryStream.Position := 0;
        FCurrentLines.LoadFromStream(MemoryStream);
      finally
        MemoryStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  end;
  result := FCurrentLines[Index - FCurrentStartLine];
end;

procedure TLineReader.SetFileName(const Value: string);
var
//  FilePostion: Int64;
  FileStream: TFileStream;
  MemoryStream: TMemoryStream;
  BytesToRead: Int64;
  FileSize: Int64;
  PriorPosition: Int64;
  AChar: AnsiChar;
  NewPosition: Int64;
begin
  if FFileName <> Value then
  begin
    Clear;
    FFileName := Value;

    FileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
      MemoryStream := TMemoryStream.Create;
      try
        PriorPosition := 0;
        FileSize := FileStream.Size;

        repeat
        begin
          BytesToRead := Min(MaxBytesToRead, FileSize - FileStream.Position);
          FileStream.Position := PriorPosition + BytesToRead;

          repeat
          begin
            FileStream.Read(AChar, SizeOf(AChar));
            if AChar in [#10, #13] then
            begin
              break;
            end;
          end;
          until FileStream.Position = FileSize;
          NewPosition := FileStream.Position;

          MemoryStream.Clear;
          FileStream.Position := PriorPosition;

          MemoryStream.CopyFrom(FileStream, NewPosition-PriorPosition);
          MemoryStream.Position := 0;
          FCurrentLines.LoadFromStream(MemoryStream);

          FLineCounts.Add(FCurrentLines.Count);
          FFilePositions.Add(PriorPosition);
          Assert(FLineCounts.Count = FFilePositions.Count);
          PriorPosition := NewPosition;
        end;
        until FileStream.Position = FileSize;

      finally
        MemoryStream.Free;
      end;
    finally
      FileStream.Free
    end;
    FCurrentStartLine := -1;
    FCurrentLines.Clear;
  end;
end;

procedure TfrmZoneBdgtReader.ReadZonebdgt6File;
var
  SearchTerm: string;
  LineIndex: Integer;
  CumulativeBudget: TBudget;
  TimeStepBudget: TBudget;
  CurrentLine: string;
  CurrentZone: string;
  TimeStepString: string;
  StressPeriodString: string;
  StartLine: Integer;
  StopLine: Integer;
  InnerLineIndex: Integer;
  BudgetItem: string;
  Rate: string;
  ABudgetItem: TBudgetItem;
  InMinusOut: string;
  Discrepancy: string;
begin
  LineIndex := 0;
  SearchTerm := 'VOLUME BUDGET FOR';
  LineIndex := GetNextLine(SearchTerm, LineIndex);
  while LineIndex > -1 do
  begin
    CumulativeBudget := TBudget.Create;
    FBudgetList.Add(CumulativeBudget);

    TimeStepBudget := TBudget.Create;
    FBudgetList.Add(TimeStepBudget);

    CurrentLine := ZBLStringList.Strings[LineIndex];

    CurrentZone := GetStringBetween(CurrentLine, SearchTerm, 'AT');
    CurrentLine := GetStringBetween(CurrentLine, CurrentZone, '');

    TimeStepString := GetStringBetween(CurrentLine, 'TIME STEP', ',');
    CurrentLine := GetStringBetween(CurrentLine, TimeStepString, '');

    StressPeriodString := GetStringBetween(CurrentLine, 'STRESS PERIOD', '');

    CumulativeBudget.Zone := 'CUMULATIVE ' + CurrentZone;
    CumulativeBudget.StressPeriod := StressPeriodString;
    CumulativeBudget.TimeStep := TimeStepString;

    TimeStepBudget.Zone := 'RATES ' + CurrentZone;
    TimeStepBudget.StressPeriod := StressPeriodString;
    TimeStepBudget.TimeStep := TimeStepString;

    StartLine := LineIndex + 1;
    SearchTerm := 'OUT:';
    StopLine := GetNextLine(SearchTerm, StartLine + 1);

    if StopLine > -1 then
    begin
      for InnerLineIndex := StartLine to StopLine do
      begin
        CurrentLine := ZBLStringList.Strings[InnerLineIndex];
        if Pos('=', CurrentLine) > 0 then
        begin
          BudgetItem := GetStringBetween(CurrentLine, '', '=');
          Rate := GetStringBetween(CurrentLine, '=', BudgetItem);

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          CumulativeBudget.InList.Add(ABudgetItem);

          CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
          CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
          Rate := GetStringBetween(CurrentLine, '=', '');
          if Pos(' ', Rate) > 0 then
          begin
            Rate := GetStringBetween(Rate, '', ' ');
          end;

          ABudgetItem := TBudgetItem.Create;
          ABudgetItem.Name := BudgetItem;
          ABudgetItem.value := Rate;
          TimeStepBudget.InList.Add(ABudgetItem);
        end;
      end;
    end;

    if StopLine > -1 then
    begin
      StartLine := StopLine + 1;
      SearchTerm := 'IN - OUT';
      StopLine := GetNextLine(SearchTerm, StartLine + 1) - 1;
      if StopLine > -1 then
      begin
        for InnerLineIndex := StartLine to StopLine do
        begin
          CurrentLine := ZBLStringList.Strings[InnerLineIndex];
          if Pos('=', CurrentLine) > 0 then
          begin
            BudgetItem := GetStringBetween(CurrentLine, '', '=');
            Rate := GetStringBetween(CurrentLine, '=', BudgetItem);

            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            CumulativeBudget.OutList.Add(ABudgetItem);

            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            CurrentLine := GetStringBetween(CurrentLine, BudgetItem, '');
            Rate := GetStringBetween(CurrentLine, '=', '');
            if Pos(' ', Rate) > 0 then
            begin
              Rate := GetStringBetween(Rate, '', ' ');
            end;

            ABudgetItem := TBudgetItem.Create;
            ABudgetItem.Name := BudgetItem;
            ABudgetItem.value := Rate;
            TimeStepBudget.OutList.Add(ABudgetItem);
          end;
        end;
      end;
    end;

    if StopLine > -1 then
    begin
      SearchTerm := 'IN - OUT';
      LineIndex := GetNextLine(SearchTerm, StopLine + 1);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        InMinusOut := GetStringBetween(CurrentLine, '=', 'IN - OUT');
        CumulativeBudget.InMinusOut := InMinusOut;

        CurrentLine := GetStringBetween(CurrentLine, '=', '');
        InMinusOut := GetStringBetween(CurrentLine, '=', '');
        TimeStepBudget.InMinusOut := InMinusOut;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'PERCENT DISCREPANCY';
      LineIndex := GetNextLine(SearchTerm, StopLine);
      if LineIndex > -1 then
      begin
        CurrentLine := ZBLStringList.Strings[LineIndex];
        Discrepancy := GetStringBetween(CurrentLine, '=', 'PERCENT DISCREPANCY');
        CumulativeBudget.Discrepancy := Discrepancy;

        CurrentLine := GetStringBetween(CurrentLine, '=', '');
        Discrepancy := GetStringBetween(CurrentLine, '=', '');
        TimeStepBudget.Discrepancy := Discrepancy;
      end;
    end;

    if LineIndex > -1 then
    begin
      SearchTerm := 'VOLUME BUDGET FOR';
      LineIndex := GetNextLine(SearchTerm, LineIndex + 1);
    end;
  end;

end;

initialization
  SetDecimalSign;
end.

