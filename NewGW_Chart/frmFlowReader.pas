unit frmFlowReader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, Grids, TeeProcs,
  TeEngine, Chart, CheckLst, Spin, contnrs, Series, TeeEdit,
  Menus, MyFormUnit, RbwDataGrid4, VclTee.TeeGDIPlus;

type
  TfrmCellFlows = class(TMyForm)
    Panel1: TPanel;
    btnSelectFile: TButton;
    OpenDialog1: TOpenDialog;
    BitBtn1: TBitBtn;
    btnUpdatePlot: TButton;
    chartFlow: TChart;
    seCells: TSpinEdit;
    Panel2: TPanel;
    clbDataSets: TCheckListBox;
    dgCells: TRbwDataGrid4;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ChartEditor1: TChartEditor;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    SelectBudgetFile1: TMenuItem;
    UpdatePlot1: TMenuItem;
    Exit1: TMenuItem;
    FormatChart1: TMenuItem;
    lblCounts: TLabel;
    Label1: TLabel;
    comboModelChoice: TComboBox;
    sbFormat: TSpeedButton;
    BitBtn2: TBitBtn;
    Help1: TMenuItem;
    Help2: TMenuItem;
    About1: TMenuItem;
    ExportData1: TMenuItem;
    sdExportData: TSaveDialog;
    btnPlotAll: TButton;
    btnPlotNone: TButton;
    pbFileProgress: TProgressBar;
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnUpdatePlotClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure seCellsChange(Sender: TObject);
    procedure FormatChart1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure clbDataSetsClickCheck(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ExportData1Click(Sender: TObject);
    procedure dgCellsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure dgCellsEndUpdate(Sender: TObject);
    procedure btnPlotAllClick(Sender: TObject);
    procedure btnPlotNoneClick(Sender: TObject);
  private
    SeriesList: TObjectList;
    FNLay: Integer;
    FNRow: Integer;
    FNCol: Integer;
    procedure ClearList;
    function ReadDataSetNames(FileName: string; const Names: TStrings;
      out NCOL, NROW, NLAY: Integer): boolean;
    function RowCellText(const ARow: integer): string;
    procedure ActivateSeries;
    procedure AdjustColRowLayer(const DataTypeIndex: integer; var Col, Row,
      Layer: integer);
    function OppositeFace(const Name: string): string;
    procedure ChangePlots(Value: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCellFlows: TfrmCellFlows;

implementation

uses IntListUnit, frmModChartUnit, frmAboutUnit, ReadModflowArrayUnit,
  frmBudgetPrecisionQueryUnit;

{$R *.DFM}

const
  FlowRightFace = 'Flow Right Face';
  FlowLeftFace = 'Flow Left Face';
  FlowFrontFace = 'Flow Front Face';
  FlowBackFace = 'Flow Back Face';
  FlowLowerFace = 'Flow Lower Face';
  FlowUpperFace = 'Flow Upper Face';

//type
//  TText = array[0..15] of Char;

//procedure ReadArray(var TOTIM: single; var KSTP, KPER, IRESULT: longint;
//  var TEXT: TModflowDesc; TextLength: LongInt)
//  stdcall; external 'ReadFlow.dll';
//
//procedure OpenBudgetFile(var IRESULT, NC, NR, NL: longint; NAME: string;
//  NameLength: longint);
//  stdcall; external 'ReadFlow.dll';
//
//procedure CloseBudgetFile;
//  stdcall; external 'ReadFlow.dll';
//
//procedure GetValue(var Layer, Row, Column: longint; var Value: single);
//  stdcall; external 'ReadFlow.dll';


procedure ReadArray96(var TOTIM: single; var KSTP, KPER, IRESULT: longint;
  var TEXT: TModflowDesc; TextLength: LongInt)
  stdcall; external 'ReadFlow96.dll';

procedure OpenBudgetFile96(var IRESULT, NC, NR, NL: longint; NAME: string;
  NameLength: longint);
  stdcall; external 'ReadFlow96.dll';

procedure CloseBudgetFile96;
  stdcall; external 'ReadFlow96.dll';

procedure GetValue96(var Layer, Row, Column: longint; var Value: single);
  stdcall; external 'ReadFlow96.dll';

Function TfrmCellFlows.OppositeFace(const Name: string): string;
begin
  result := '';
  if Name = FlowRightFace then
  begin
    result := FlowLeftFace;
  end
  else if Name = FlowFrontFace then
  begin
    result := FlowBackFace;
  end
  else if Name = FlowLowerFace then
  begin
    result := FlowUpperFace
  end;
end;

function TfrmCellFlows.ReadDataSetNames(FileName: string;
  const Names: TStrings; out NCOL, NROW, NLAY: longint): boolean;
var
  Dir: string;
  IRESULT: longint;
  AName: string;
  TEXT: TModflowDesc;
  KSTP, KPER: longint;
  CharIndex: integer;
  DataIndicies: TIntegerList;
  NamePosition: integer;
  DataIndex: integer;
  TOTIM: single;
  PERTIM, TOTIMD: TModflowDouble;
  OppositeDataSetName: string;
  FileStream : TFileStream;
  Precision: TModflowPrecision;
  DESC: TModflowDesc;
  A3DArray: T3DTModflowArray;
  Mf_FileSize: Int64;
begin
  result := False;
  Dir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFileDir(FileName));
    FileStream := nil;
    FileName := ExtractFileName(FileName);
    try
      Precision := mpSingle;
      if comboModelChoice.ItemIndex = 1 then
      begin
        FileStream := TFileStream.Create(FileName,
          fmOpenRead or fmShareDenyWrite);
        Precision := QueryBudgetPrecision(FileStream);
        IRESULT := 0;
//        OpenBudgetFile(IRESULT, NCOL, NROW, NLAY, FileName,
//          Length(FileName));
      end
      else
      begin
        OpenBudgetFile96(IRESULT, NCOL, NROW, NLAY, FileName,
          Length(FileName));
      end;

      if IRESULT = 2 then
      begin
        Beep;
        ShowMessage('Error: Empty budget file');
        Exit;
      end
      else if IRESULT <> 0 then
      begin
        Beep;
        ShowMessage('Unknown Error');
        Exit;
      end;

      DataIndex := -1;
      if FileStream <> nil then
      begin
        Mf_FileSize := FileStream.Size;
      end;        
      while IResult = 0 do
      begin
        result := True;
        TEXT := '                ';
        if comboModelChoice.ItemIndex = 1 then
        begin
          case Precision of
            mpSingle:
              ReadModflowSinglePrecFluxArray(FileStream, KSTP, KPER,
                PERTIM, TOTIMD, DESC, NCOL, NROW, NLAY, A3DArray, IRESULT, False);
            mpDouble:
              ReadModflowDoublePrecFluxArray(FileStream, KSTP, KPER,
                PERTIM, TOTIMD, DESC, NCOL, NROW, NLAY, A3DArray,
                Abs(NLAY), NROW, NCOL,
                IRESULT, False);
          else Assert(False);
          end;
          pbFileProgress.Position := Round(FileStream.Position/(2*Mf_FileSize)
            * pbFileProgress.Max);
          TEXT := DESC;
          TOTIM := TOTIMD;
//          ReadArray(TOTIM, KSTP, KPER, IRESULT, TEXT, Length(Text));
        end
        else
        begin
          ReadArray96(TOTIM, KSTP, KPER, IRESULT, TEXT, Length(Text));
        end;

        if IRESULT = 1 then
        begin
          Exit;
        end
        else if IRESULT = 2 then
        begin
          ShowMessage('Error, Empty budget file');
          result := False;
          Exit;
        end
        else if IRESULT <> 0 then
        begin
          ShowMessage('Unknown Error');
          result := False;
          Exit;
        end
        else
        begin
//          for CharIndex := 1 to 15 do
//          begin
//            if TEXT[CharIndex - 1] <> ' ' then
//            begin
//              TEXT[CharIndex] := AnsiChar(LowerCase(TEXT[CharIndex])[1]);
//            end;
//          end;

          AName := Trim(TEXT);
          AName := LowerCase(AName);
          AName := String(AnsiString(AName));
          if (AName = 'flow-ja-face') or (AName = 'Flow-ja-face') then
          begin
            Continue;
          end;
          Inc(DataIndex);

          NamePosition := Names.IndexOf(AName);
          if NamePosition < 0 then
          begin
            DataIndicies := TIntegerList.Create;
            Names.AddObject(AName, DataIndicies);
          end
          else
          begin
            DataIndicies := Names.Objects[NamePosition] as TIntegerList;
          end;
          DataIndicies.Add(DataIndex);

          OppositeDataSetName := OppositeFace(AName);
          if OppositeDataSetName <> '' then
          begin
            NamePosition := Names.IndexOf(OppositeDataSetName);
            if NamePosition < 0 then
            begin
              DataIndicies := TIntegerList.Create;
              Names.AddObject(OppositeDataSetName, DataIndicies);
            end
            else
            begin
              DataIndicies := Names.Objects[NamePosition] as TIntegerList;
            end;
            DataIndicies.Add(-DataIndex-1);
          end;

        end;
      end;
    finally
      if comboModelChoice.ItemIndex = 1 then
      begin
        FileStream.Free;
//        CloseBudgetFile;
      end
      else
      begin
        CloseBudgetFile96;
      end;
      FNLay := NLAY;
      FNRow := NRow;
      FNCol := NCol
    end;
  finally
    SetCurrentDir(Dir);
  end;
end;

procedure TfrmCellFlows.btnSelectFileClick(Sender: TObject);
var
  NCOL, NROW, NLAY: longint;
begin
  if OpenDialog1.Execute then
  begin
    pbFileProgress.Position := 0;
    Screen.Cursor := crHourGlass;
    try
      lblCounts.Caption := 'Columns: 0, Rows: 0, Layers: 0';
      chartFlow.Title.Text.Clear;
      chartFlow.Title.Text.Add('Flow Rates: '
        + ExtractFileName(OpenDialog1.FileName));
      ClearList;
      dgCells.Enabled := ReadDataSetNames(OpenDialog1.FileName,
        clbDataSets.Items, NCOL, NROW, NLAY);

      if dgCells.Enabled then
      begin
        lblCounts.Caption := Format('Columns: %d, Rows: %d, Layers: %d',
          [NCOL, NROW, Abs(NLAY)]);
        dgCells.Columns[0].Max := NCOL;
        dgCells.Columns[1].Max := NROW;
        dgCells.Columns[2].Max := Abs(NLAY);
      end;
      seCells.Enabled := dgCells.Enabled;
      btnUpdatePlot.Enabled := dgCells.Enabled;
      sbFormat.Enabled := dgCells.Enabled;
      btnUpdatePlotClick(nil);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

function TfrmCellFlows.RowCellText(Const ARow: integer): string;
begin
  result := ' ('
    + dgCells.Cells[0, ARow] + ','
    + dgCells.Cells[1, ARow] + ','
    + dgCells.Cells[2, ARow] + ')';
end;

procedure TfrmCellFlows.ActivateSeries;
var
  DataTypeIndex, CellIndex: integer;
  SList: TObjectList;
  Series: TLineSeries;
begin
  if (SeriesList = nil) or (SeriesList.Count <> clbDataSets.Items.Count) then
  begin
    Exit;
  end;
  for DataTypeIndex := 0 to SeriesList.Count - 1 do
  begin
    SList := SeriesList[DataTypeIndex] as TObjectList;
    if SList.Count <> dgCells.RowCount - 1 then
    begin
      Exit;
    end;

    for CellIndex := 0 to SList.Count - 1 do
    begin
      Series := SList[CellIndex] as TLineSeries;
      if Series.Title = clbDataSets.Items[DataTypeIndex] + RowCellText(CellIndex+1) then
      begin
        Series.Active := clbDataSets.Checked[DataTypeIndex] and
          dgCells.Checked[3,CellIndex+1];
      end;

    end;
  end;
end;

procedure TfrmCellFlows.AdjustColRowLayer(const DataTypeIndex: integer;
  var Col, Row, Layer: integer);
var
  DataSetName: string;
begin
  DataSetName := clbDataSets.Items[DataTypeIndex];
  if DataSetName = FlowLeftFace then
  begin
    Dec(Col);
  end
  else if DataSetName = FlowBackFace then
  begin
    Dec(Row);
  end
  else if DataSetName = FlowUpperFace then
  begin
    Dec(Layer);
  end;
end;

procedure TfrmCellFlows.btnUpdatePlotClick(Sender: TObject);
var
  IRESULT: longint;
  KSTP, KPER: longint;
  TEXT: TModflowDesc;
  Value: single;
  Layer, Row, Column: longint;
  Series: TLineSeries;
  FileName: string;
  DataTypeIndex, CellIndex: integer;
  SList: TObjectList;
  IntList: TIntegerList;
  Cells: array of array of integer;
  NCOL, NROW, NLAY: longint;
  DataSetIndex: integer;
  TOTIM: single;
  ColorIndex: integer;
  AStyle: TSeriesPointerStyle;
  FileStream : TFileStream;
  Precision: TModflowPrecision;
  PERTIM, TOTIMD: TModflowDouble;
  A3DArray: T3DTModflowArray;
  DESC: TModflowDesc;
  PriorTotTime: double;
  Mf_FileSize: Int64;
begin
//  pbFileProgress.Position := 0;
  SetLength(Cells, 3, dgCells.RowCount - 1);
  for CellIndex := 1 to dgCells.RowCount - 1 do
  begin
    Cells[0, CellIndex - 1] := StrToInt(dgCells.Cells[0, CellIndex]);
    Cells[1, CellIndex - 1] := StrToInt(dgCells.Cells[1, CellIndex]);
    Cells[2, CellIndex - 1] := StrToInt(dgCells.Cells[2, CellIndex]);
  end;
  SeriesList.Clear;
  ColorIndex := -1;
  AStyle := High(TSeriesPointerStyle);
  for DataTypeIndex := 0 to clbDataSets.Items.Count - 1 do
  begin
    SList := TObjectList.Create;
    SeriesList.Add(SList);
    for CellIndex := 1 to dgCells.RowCount - 1 do
    begin
      Series := TLineSeries.Create(self);
      Series.XValues.Order := loNone;
      SList.Add(Series);
      Series.ParentChart := chartFlow;
      Series.Pointer.HorizSize := 4;
      Series.Pointer.VertSize := 4;
      Series.Pointer.Visible := True;
      Series.Title := clbDataSets.Items[DataTypeIndex] + RowCellText(CellIndex);
      Series.Active := clbDataSets.Checked[DataTypeIndex] and
        dgCells.Checked[3, CellIndex];
      Inc(ColorIndex);
      if ColorIndex >= Length(ColorPalette) then
      begin
        ColorIndex := 0;
      end;
      Series.SeriesColor := ColorPalette[ColorIndex];
      if AStyle = High(TSeriesPointerStyle) then
      begin
        AStyle := Low(TSeriesPointerStyle)
      end
      else
      begin
        Inc(AStyle);
      end;
      while AStyle in [psSmallDot, psNothing] do
      begin
        Inc(AStyle);
      end;
      Series.Pointer.Style := AStyle;
    end;
  end;

  SetCurrentDir(ExtractFileDir(OpenDialog1.FileName));
  FileStream := nil;
  FileName := ExtractFileName(OpenDialog1.FileName);

//  FileName := ExtractFileName(OpenDialog1.FileName);
  try
    Precision := mpSingle;
    if comboModelChoice.ItemIndex = 1 then
    begin
      FileStream := TFileStream.Create(FileName,
        fmOpenRead or fmShareDenyWrite);
      Precision := QueryBudgetPrecision(FileStream);
      IRESULT := 0;
//      OpenBudgetFile(IRESULT, NCOL, NROW, NLAY, FileName,
//        Length(FileName));
    end
    else
    begin
      OpenBudgetFile96(IRESULT, NCOL, NROW, NLAY, FileName,
        Length(FileName));
    end;

    if FileStream <> nil then
    begin
      Mf_FileSize := FileStream.Size;
    end;      
    if IResult <> 0 then
      Exit;

    PriorTotTime := -1;
    DataSetIndex := -1;
    while IResult = 0 do
    begin
      TEXT := '                ';
      if comboModelChoice.ItemIndex = 1 then
      begin
        case Precision of
          mpSingle:
            ReadModflowSinglePrecFluxArray(FileStream, KSTP, KPER,
              PERTIM, TOTIMD, DESC, NCOL, NROW, NLAY, A3DArray, IRESULT, True);
          mpDouble:
            ReadModflowDoublePrecFluxArray(FileStream, KSTP, KPER,
              PERTIM, TOTIMD, DESC, NCOL, NROW, NLAY, A3DArray,
              Abs(FNLay), FNRow, FNCol,
              IRESULT, True);
        else Assert(False);
        end;
        pbFileProgress.Position := (pbFileProgress.Max div 2)
          + Round(FileStream.Position/(2*Mf_FileSize)
          * pbFileProgress.Max);
        TEXT := DESC;
        if Trim(String(TEXT)) = 'FLOW-JA-FACE' then
        begin
          Continue;
        end;

        TOTIM := TOTIMD;
        NLAY := Abs(NLAY);
//        ReadArray(TOTIM, KSTP, KPER, IRESULT, TEXT, Length(Text));
      end
      else
      begin
        ReadArray96(TOTIM, KSTP, KPER, IRESULT, TEXT, Length(Text));
      end;
      if TOTIM < 0 then
      begin
        TOTIM := PriorTotTime;
      end
      else
      begin
        PriorTotTime := TOTIM;
      end;


      if IRESULT = 1 then
      begin
        Exit;
      end
      else if IRESULT = 2 then
      begin
        ShowMessage('Error, Empty budget file');
        Exit;
      end
      else if IRESULT <> 0 then
      begin
        ShowMessage('Unknown Error');
        Exit;
      end
      else
      begin
        Inc(DataSetIndex);

        for DataTypeIndex := 0 to clbDataSets.Items.Count - 1 do
        begin
          IntList := clbDataSets.Items.Objects[DataTypeIndex] as TIntegerList;
          if IntList.IndexOf(DataSetIndex) >= 0 then
          begin
            SList := SeriesList[DataTypeIndex] as TObjectList;
            for CellIndex := 1 to dgCells.RowCount - 1 do
            begin
              Series := SList[CellIndex - 1] as TLineSeries;
              Column := Cells[0, CellIndex - 1];
              Row := Cells[1, CellIndex - 1];
              Layer := Cells[2, CellIndex - 1];
              if comboModelChoice.ItemIndex = 1 then
              begin
                Value := A3DArray[Abs(Layer)-1,Row-1,Column-1];
//                GetValue(Layer, Row, Column, Value);
              end
              else
              begin
                GetValue96(Layer, Row, Column, Value);
              end;

              if TOTIM >= 0 then
              begin
                Series.AddXY(TOTIM, Value);
                chartFlow.BottomAxis.Title.Caption := 'Time';
              end
              else
              begin
                Series.AddXY(Series.Count + 1, Value);
                chartFlow.BottomAxis.Title.Caption := 'Stored Time Step';
              end
            end;
          end;

          if IntList.IndexOf(-DataSetIndex-1) >= 0 then
          begin
            SList := SeriesList[DataTypeIndex] as TObjectList;
            for CellIndex := 1 to dgCells.RowCount - 1 do
            begin
              Series := SList[CellIndex - 1] as TLineSeries;
              Column := Cells[0, CellIndex - 1];
              Row := Cells[1, CellIndex - 1];
              Layer := Cells[2, CellIndex - 1];
              AdjustColRowLayer(DataTypeIndex, Column, Row, Layer);
              if (Column >= 1) and (Row >= 1) and (Layer >= 1) then
              begin
                if comboModelChoice.ItemIndex = 1 then
                begin
                  Value := A3DArray[Layer-1,Row-1,Column-1];
//                  GetValue(Layer, Row, Column, Value);
                end
                else
                begin
                  GetValue96(Layer, Row, Column, Value);
                end;
                // Reverse sign of Value because flow is in opposite direction.
                Value := -Value;
              end
              else
              begin
                Value := 0;
              end;


              if TOTIM >= 0 then
              begin
                Series.AddXY(TOTIM, Value);
                chartFlow.BottomAxis.Title.Caption := 'Time';
              end
              else
              begin
                Series.AddXY(Series.Count + 1, Value);
                chartFlow.BottomAxis.Title.Caption := 'Stored Time Step';
              end
            end;
          end;
        end;
      end;
    end;
  finally
    if comboModelChoice.ItemIndex = 1 then
    begin
      FileStream.Free;
//      CloseBudgetFile;
    end
    else
    begin
      CloseBudgetFile96;
    end;
  end;
end;

procedure TfrmCellFlows.ClearList;
var
  NameIndex: integer;
begin
  for NameIndex := 0 to clbDataSets.Items.Count - 1 do
  begin
    clbDataSets.Items.Objects[NameIndex].Free;
  end;
  clbDataSets.Items.Clear;
  SeriesList.Clear;
end;

procedure TfrmCellFlows.FormDestroy(Sender: TObject);
begin
  ClearList;
  SeriesList.Free;
end;

procedure TfrmCellFlows.FormCreate(Sender: TObject);
begin
  dgCells.Cells[0, 0] := 'Col';
  dgCells.Cells[1, 0] := 'Row';
  dgCells.Cells[2, 0] := 'Lay';
  dgCells.Cells[3, 0] := 'Plot';
  dgCells.Cells[0, 1] := '1';
  dgCells.Cells[1, 1] := '1';
  dgCells.Cells[2, 1] := '1';
  dgCells.Checked[3, 1] := True;
  SeriesList := TObjectList.Create;
  comboModelChoice.ItemIndex := 1;
  Constraints.MinWidth := Width;
end;

procedure TfrmCellFlows.seCellsChange(Sender: TObject);
var
  FirstRow: integer;
  RowIndex: integer;
begin
  FirstRow := dgCells.RowCount;
  dgCells.RowCount := seCells.Value + 1;
  for RowIndex := FirstRow to dgCells.RowCount - 1 do
  begin
    dgCells.Cells[0, RowIndex] := dgCells.Cells[0, RowIndex - 1];
    dgCells.Cells[1, RowIndex] := dgCells.Cells[1, RowIndex - 1];
    dgCells.Cells[2, RowIndex] := dgCells.Cells[2, RowIndex - 1];
    dgCells.Checked[3, RowIndex] := dgCells.Checked[3, RowIndex - 1];
  end;
end;

procedure TfrmCellFlows.FormatChart1Click(Sender: TObject);
begin
  mHHelp.ChmFile := ChartHelpFileName;
  try
    ChartEditor1.Execute;
  finally
    mHHelp.ChmFile := HelpFileName;
  end;
end;

procedure TfrmCellFlows.Exit1Click(Sender: TObject);
begin
  inherited
  Close;
end;

procedure TfrmCellFlows.FormShow(Sender: TObject);
begin
  MainMenu1.Merge(frmModChart.mainMenuFormChoice);

end;

procedure TfrmCellFlows.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  if Visible then
  begin
    frmModChart.Close
  end;
end;

procedure TfrmCellFlows.clbDataSetsClickCheck(Sender: TObject);
begin
  inherited;
  ActivateSeries;
end;

procedure TfrmCellFlows.Help2Click(Sender: TObject);
begin
  inherited;
  Application.HelpContext(HelpContext);
end;

procedure TfrmCellFlows.About1Click(Sender: TObject);
begin
  inherited;
  // show the "About" form
  frmAbout.ShowModal;
end;

procedure TfrmCellFlows.ExportData1Click(Sender: TObject);
var
  GroupIndex: integer;
  Group: TList;
  SeriesIndex: integer;
  Series : TLineSeries;
  ExportFile: TStringList;
  TimeIndex: integer;
begin
  inherited;
  if sdExportData.Execute then
  begin
    ExportFile := TStringList.Create;
    try
      for GroupIndex := 0 to SeriesList.Count-1 do
      begin
        Group := SeriesList[GroupIndex] as TList;
        for SeriesIndex := 0 to Group.Count -1 do
        begin
          Series := Group[SeriesIndex];
          if Series.Visible then
          begin
            ExportFile.Add(chartFlow.BottomAxis.Title.Caption + #9
              + Series.Title + ' (Col, Row, Lay)');
            for TimeIndex := 0 to Series.XValues.Count -1 do
            begin
              ExportFile.Add(FloatToStr(Series.XValues[TimeIndex]) + #9
                + FloatToStr(Series.YValues[TimeIndex]));
            end;
            ExportFile.Add('');
          end;
        end;
      end;
      ExportFile.SaveToFile(sdExportData.FileName);
    finally
      ExportFile.Free;
    end;
  end;
end;

procedure TfrmCellFlows.dgCellsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  ActivateSeries;
end;

procedure TfrmCellFlows.dgCellsEndUpdate(Sender: TObject);
begin
  inherited;
  seCells.Value := dgCells.RowCount -1;
end;

procedure TfrmCellFlows.ChangePlots(Value: Boolean);
var
  RowIndex: integer;
begin
  for RowIndex := 0 to dgCells.RowCount -1 do
  begin
    dgCells.Checked[3,RowIndex] := Value;
  end;

end;

procedure TfrmCellFlows.btnPlotAllClick(Sender: TObject);
begin
  inherited;
  ChangePlots(True);
  ActivateSeries;
end;

procedure TfrmCellFlows.btnPlotNoneClick(Sender: TObject);
begin
  inherited;
  ChangePlots(False);
end;

end.

