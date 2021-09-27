unit frmFlowReader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, StringGrid3d, Grids, TeeProcs,
  TeEngine, Chart, CheckLst, RbwDataGrid, Spin, contnrs, Series, TeeEdit,
  Menus, MyFormUnit;

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
    dgCells: TRbwDataGrid;
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
    procedure dgCellsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Help2Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    SeriesList: TObjectList;
    procedure ClearList;
    function ReadDataSetNames(FileName: string; const Names: TStrings;
      out NCOL, NROW, NLAY: Integer): boolean;
    function RowCellText(const ARow: integer): string;
    procedure ActivateSeries;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCellFlows: TfrmCellFlows;

implementation

uses IntListUnit, frmModChartUnit, frmAboutUnit;

{$R *.DFM}

type
  TText = array[0..15] of Char;

procedure ReadArray(var TOTIM: single; var KSTP, KPER, IRESULT: longint;
  var TEXT: TText; TextLength: LongInt)
  stdcall; external 'ReadFlow.dll';

procedure OpenBudgetFile(var IRESULT, NC, NR, NL: longint; NAME: string;
  NameLength: longint);
  stdcall; external 'ReadFlow.dll';

procedure CloseBudgetFile;
  stdcall; external 'ReadFlow.dll';

procedure GetValue(var Layer, Row, Column: longint; var Value: single);
  stdcall; external 'ReadFlow.dll';


procedure ReadArray96(var TOTIM: single; var KSTP, KPER, IRESULT: longint;
  var TEXT: TText; TextLength: LongInt)
  stdcall; external 'ReadFlow96.dll';

procedure OpenBudgetFile96(var IRESULT, NC, NR, NL: longint; NAME: string;
  NameLength: longint);
  stdcall; external 'ReadFlow96.dll';

procedure CloseBudgetFile96;
  stdcall; external 'ReadFlow96.dll';

procedure GetValue96(var Layer, Row, Column: longint; var Value: single);
  stdcall; external 'ReadFlow96.dll';




function TfrmCellFlows.ReadDataSetNames(FileName: string;
  const Names: TStrings; out NCOL, NROW, NLAY: longint): boolean;
var
  Dir: string;
  IRESULT: longint;
  AName: string;
  TEXT: TText;
  KSTP, KPER: longint;
  CharIndex: integer;
  DataIndicies: TIntegerList;
  NamePosition: integer;
  DataIndex: integer;
  TOTIM: single;
begin
  result := False;
  Dir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFileDir(FileName));
    FileName := ExtractFileName(FileName);
    try
      if comboModelChoice.ItemIndex = 1 then
      begin
        OpenBudgetFile(IRESULT, NCOL, NROW, NLAY, FileName,
          Length(FileName));
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
        ShowMessage('Unknown Eror');
        Exit;
      end;

      DataIndex := -1;
      while IResult = 0 do
      begin
        result := True;
        TEXT := '                ';
        if comboModelChoice.ItemIndex = 1 then
        begin
          ReadArray(TOTIM, KSTP, KPER, IRESULT, TEXT, Length(Text));
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
          ShowMessage('Unknown Eror');
          result := False;
          Exit;
        end
        else
        begin
          Inc(DataIndex);
          for CharIndex := 1 to 15 do
          begin
            if Text[CharIndex - 1] <> ' ' then
            begin
              Text[CharIndex] := LowerCase(Text[CharIndex])[1];
            end;
          end;

          AName := Trim(TEXT);
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
        end;
      end;
    finally
      if comboModelChoice.ItemIndex = 1 then
      begin
        CloseBudgetFile;
      end
      else
      begin
        CloseBudgetFile96;
      end;
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
        [NCOL, NROW, NLAY]);
      dgCells.Columns[0].Max := NCOL;
      dgCells.Columns[1].Max := NROW;
      dgCells.Columns[2].Max := NLAY;
    end;
    seCells.Enabled := dgCells.Enabled;
    btnUpdatePlot.Enabled := dgCells.Enabled;
    sbFormat.Enabled := dgCells.Enabled;
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
  if SeriesList.Count <> clbDataSets.Items.Count then
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
          (dgCells.Cells[3,CellIndex+1] = dgCells.Columns[3].CheckedString);
      end;

    end;
  end;
end;

procedure TfrmCellFlows.btnUpdatePlotClick(Sender: TObject);
var
  IRESULT: longint;
  KSTP, KPER: longint;
  TEXT: TText;
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
begin
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
      SList.Add(Series);
      Series.ParentChart := chartFlow;
      Series.Pointer.HorizSize := 4;
      Series.Pointer.VertSize := 4;
      Series.Pointer.Visible := True;
      Series.Title := clbDataSets.Items[DataTypeIndex] + RowCellText(CellIndex);
      Series.Active := clbDataSets.Checked[DataTypeIndex] and
        (dgCells.Cells[3, CellIndex] = dgCells.Columns[3].CheckedString);
      Inc(ColorIndex);
      if ColorIndex > MaxColor then
      begin
        ColorIndex := 0;
      end;
      Series.SeriesColor := MyColors[ColorIndex];
      if AStyle = High(TSeriesPointerStyle) then
      begin
        AStyle := Low(TSeriesPointerStyle)
      end
      else
      begin
        Inc(AStyle);
      end;
      Series.Pointer.Style := AStyle;
    end;
  end;

  FileName := ExtractFileName(OpenDialog1.FileName);
  try
    if comboModelChoice.ItemIndex = 1 then
    begin
      OpenBudgetFile(IRESULT, NCOL, NROW, NLAY, FileName,
        Length(FileName));
    end
    else
    begin
      OpenBudgetFile96(IRESULT, NCOL, NROW, NLAY, FileName,
        Length(FileName));
    end;

    if IResult <> 0 then
      Exit;

    DataSetIndex := -1;
    while IResult = 0 do
    begin
      TEXT := '                ';
      if comboModelChoice.ItemIndex = 1 then
      begin
        ReadArray(TOTIM, KSTP, KPER, IRESULT, TEXT, Length(Text));
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
        Exit;
      end
      else if IRESULT <> 0 then
      begin
        ShowMessage('Unknown Eror');
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
                GetValue(Layer, Row, Column, Value);
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
            break;
          end;
        end;
      end;
    end;
  finally
    if comboModelChoice.ItemIndex = 1 then
    begin
      CloseBudgetFile;
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
  dgCells.Cells[3, 1] := dgCells.Columns[3].CheckedString;
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
    dgCells.Cells[3, RowIndex] := dgCells.Cells[3, RowIndex - 1];
  end;
end;

procedure TfrmCellFlows.FormatChart1Click(Sender: TObject);
begin
  ChartEditor1.Execute;
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

procedure TfrmCellFlows.dgCellsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: integer;
begin
  inherited;
  dgCells.MouseToCell(X, Y, ACol, ARow);
  if (ARow > 0) and (ACol = 3) then
  begin
    ActivateSeries;
  end;
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

end.

