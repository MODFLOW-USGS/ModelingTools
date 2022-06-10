unit ExtractUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ArgusDataEntry, Menus, Grids, Buttons, TeeProcs, TeEngine,
  Chart, ExtCtrls, Series, DataGrid, ComCtrls, MyFormUnit, TeeEdit,
  ReaderUnit, addbtn95, JvExStdCtrls, JvRichEdit, VclTee.TeeGDIPlus;

{ TODO : Tracy wrote  that he would like GWChart to calculate
"elevation head" instead of, or in addition to, pressure head, but I'm sure
he meant "total head."  Sounds like a good idea to me, since we typically
are concerned with total hydraulic head, not pressure head. Alden Provost,
April 11, 2006}

type
  TSutraCoordType = (sct2D, sct3D);

  TProgramChoice = (pcModflow, pcMt3dObs, pcMt3dConc, pcGwtObs, pcSutra1,
    pcsutra2, pcSutra2_1, pcHydMode, pcMf6Obs);
//MODFLOW head or drawdown file
//MT3D Observations
//GWT and MOC3D Observations
//SUTRA 09.97
//SUTRA 2D/3D
//SUTRA 2.1, SUTRA 2.2
//MODFLOW Hydmod package



  TSutraCoord = record
    X: double;
    Y: double;
    Z: Double;
    CoordType: TSutraCoordType;
  end;

  TSutraCoordArray = array of TSutraCoord;

  TfrmExtract = class(TMyForm)
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    About1: TMenuItem;
    Panel1: TPanel;
    Help1: TMenuItem;
    Panel2: TPanel;
    lblCount: TLabel;
    btnRead: TButton;
    btnSave: TButton;
    btnCancel: TButton;
    BitBtn1: TBitBtn;
    Splitter1: TSplitter;
    adeCellCount: TArgusDataEntry;
    File1: TMenuItem;
    ReadCellLocations1: TMenuItem;
    ReadHeadsorDrawdown1: TMenuItem;
    SaveCellLocations1: TMenuItem;
    SaveHeadsorDrawdown1: TMenuItem;
    SaveDialog2: TSaveDialog;
    dgDataPoints: TEcDataGrid;
    OpenDialog2: TOpenDialog;
    rgProgramChoice: TRadioGroup;
    rgMOC3D: TRadioGroup;
    rgSutra: TRadioGroup;
    dgDataNodes: TEcDataGrid;
    sbFormat: TSpeedButton;
    sbImage: TSpeedButton;
    ChartHydExtractor: TChart;
    SaveasImage1: TMenuItem;
    Exit1: TMenuItem;
    FormatChart1: TMenuItem;
    Help2: TMenuItem;
    Splitter2: TSplitter;
    ChartEditor1: TChartEditor;
    cbPlot: TCheckBox95;
    comboUnits: TComboBox;
    edExplanation: TEdit;
    lblLegend: TLabel;
    adeDensity: TArgusDataEntry;
    adeG: TArgusDataEntry;
    Label1: TLabel;
    Label2: TLabel;
    RichEdit1: TJvRichEdit;
    btnPlotAll: TButton;
    btnPlotNone: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgDataPointsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure sgDataPointsSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure adeCellCountChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure ReadCellLocations1Click(Sender: TObject);
    procedure SaveCellLocations1Click(Sender: TObject);
    procedure dgDataPointsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rgProgramChoiceClick(Sender: TObject);
    procedure rgMOC3DClick(Sender: TObject);
    procedure dgDataNodesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rgSutraClick(Sender: TObject);
    procedure sbFormatClick(Sender: TObject);
    procedure ToolBar1MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure sbImageClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ChartHydExtractorAfterDraw(Sender: TObject);
    procedure ChartHydExtractorGetLegendPos(Sender: TCustomChart; Index:
      Integer;
      var X, Y, XColor: Integer);
    procedure ChartHydExtractorGetLegendRect(Sender: TCustomChart; var Rect:
      TRect);
    procedure comboUnitsChange(Sender: TObject);
    procedure edExplanationChange(Sender: TObject);
    procedure dgDataNodesExit(Sender: TObject);
    procedure btnPlotAllClick(Sender: TObject);
    procedure btnPlotNoneClick(Sender: TObject);
  private
    ExplanationVisible: boolean;
    ExplanationLeft, ExplanationTop: integer;
    function GetStringBetween(var AString: string; Before, After: string):
      string;
    function GetNextLine(SearchTerm: string; Start: integer;
      AStringList: TStringList): integer;
    procedure ReadFile;
    procedure ClearNodeSelection;
    procedure ReadNewSutraFile2;
    procedure ReadHydmodFile;
    procedure SetAllPlots(Value: Boolean);
    procedure GetSutraCoord(ALine: string; var Coord: TSutraCoordArray);
    procedure ReadMt3dConcFile;
    procedure ReadMf6Obs;
    { Private declarations }
  public
    CancelProcess: boolean;
    CurrentText: string;
    MySeriesList: TList;
    FLinesToSave: TStringList;
    procedure ClearSeriesList;
    procedure ReadCellLocations(FileName: string);
    procedure ReadModflowFile;
    procedure ReadMt3dObservationsFile;
    procedure ReadMoc3dObservationsFile;
    procedure ReadSutraFile;
    procedure ReadNewSutraFile;
    function Density: double; //  Density of pure water
    function G: double; // acceleration due to gravity.
    { Public declarations }
  end;

var
  frmExtract: TfrmExtract;

implementation

uses {HelpUnit,} frmFormatUnit, frmAboutUnit, frmModChartUnit, 
  ReadModflowArrayUnit, Mf6ObsUtilOutputReaderUnit;

{$R *.DFM}

{var
  DecimalChar : Char;}

{function InternationalStrToFloat(Value : string) : extended;
var
  DecimalLocation : integer;
begin
  if DecimalChar = '.' then
  begin
    DecimalLocation := Pos(',',Value);
    if DecimalLocation > 0 then
    begin
      Value[DecimalLocation] := '.';
    end;
  end;
  if DecimalChar = ',' then
  begin
    DecimalLocation := Pos('.',Value);
    if DecimalLocation > 0 then
    begin
      Value[DecimalLocation] := ',';
    end;
  end;
  Result := StrToFloat(Value);
end; }

procedure TfrmExtract.ReadModflowFile;
const
  HeadLabel = 'HEAD';
  DrawdownLabel = 'DRAWDOWN';
  HGU_Label = 'HEAD IN HGU';
var
  ModflowOutput: TextFile; // the formatted head or drawdown file
  DescriptorLine: string; // line describing the model characteristics.
  ADatum: double; // a number read from the formatted head or drawdown file
  AString: string;
  TOTIM: double; // elapsed time
  NCOL, NROW, ILAY: integer;
    // the number of columns, number of rows and layer number
  ColIndex, RowIndex: integer; // indexes to the colums and rows
  PointsIndex: integer;
    // an index to the point for which a hydrograph is to be extracted.
  DataString: string;
    // This is used to store the formatted data that will be placed on a line of the TMemo
  ATime: double; // elapsed time for the current time step
  CaptionString: string;
  ASeries: TLineSeries; // a hydrograph plotted on the chart
//  RealArray: array of double; // an array of values read from
//  RealArrayLength: integer;
  ChartIndex: integer;
  Lines: TStringlist;
  ColorIndex: integer;
  PointCount: integer;
  PointIndex: integer;
  SeriesIndex: integer;
  Splitter: TStringList;
  FileStream: TFileStream;
  Precision: TModflowPrecision;
  KSTP: integer;
  KPER: integer;
  PERTIM: TModflowDouble;
  DESC: TModflowDesc;
  AnArray: TModflowDoubleArray;
  Columns, Rows, Layers: array of integer;
  DataArray: array of array of double;
  ItemIndex: Integer;
//  PosInFile: LongInt;
//  AnErrorLine: string;
begin
  //  Put the name of the file on the caption of the form.
  Caption := 'GW_Chart: ' + OpenDialog1.FileName;
  // disable thing that shouldn't be changed while reading the file.
  btnSave.Enabled := True;
  btnCancel.Enabled := True;
  btnRead.Enabled := False;
  adeCellCount.Enabled := False;
  dgDataPoints.Enabled := False;
  Lines := TStringlist.Create;
  try
    begin
      // ClearSeriesList destroys all the TLineSeries on the chart and
      // clears SeriesList
      ClearSeriesList;

      // initialize CancelProcess. CancelProcess will be tested while the
      // file is being read to see if the user has pressed the cancel button.
      CancelProcess := False;

      // initialize the memo
      RichEdit1.Clear;
      Lines.Add(Chr(9) + '(Column,Row,Layer)');

      // create a headers for the data in the memo and create a TLineSeries
      // for each MODFLOW cell.
      AString := 'Time';
      ColorIndex := -1;

      for RowIndex := 1 to dgDataPoints.RowCount - 1 do
      begin
        Inc(ColorIndex);
        if ColorIndex >= Length(ColorPalette) then
        begin
          ColorIndex := 0;
        end;
        CaptionString := '(' + dgDataPoints.Cells[0, RowIndex] + ',' +
          dgDataPoints.Cells[1, RowIndex] + ',' +
          dgDataPoints.Cells[2, RowIndex] + ')';
        AString := AString + Chr(9) + CaptionString;

        ASeries := TLineSeries.Create(ChartHydExtractor);
        ASeries.XValues.Order := loNone;
        ASeries.SeriesColor := ColorPalette[ColorIndex];
        ASeries.ParentChart := ChartHydExtractor;
        MySeriesList.Add(ASeries);
        ASeries.Title := CaptionString;
        ASeries.Pointer.Visible := True;
        ASeries.Active := cbPlot.Checked and (dgDataPoints.Cells[3, RowIndex]
          = dgDataPoints.Columns[3].PickList.Strings[1]);
        ASeries.BeginUpdate;
      end;
      //    Memo1.Lines.Add(AString);
      Lines.Add(AString);

      // this may take a while so change the cursor.

      Screen.Cursor := crHourGlass;
      try

        SetLength(Columns, dgDataPoints.RowCount - 1);
        SetLength(Rows, dgDataPoints.RowCount - 1);
        SetLength(Layers, dgDataPoints.RowCount - 1);
        for PointsIndex := 1 to dgDataPoints.RowCount - 1 do
        begin
          Columns[PointsIndex-1] := StrToInt(dgDataPoints.Cells[0, PointsIndex]);
          Rows[PointsIndex-1] := StrToInt(dgDataPoints.Cells[1, PointsIndex]);
          Layers[PointsIndex-1] := StrToInt(dgDataPoints.Cells[2, PointsIndex]);
        end;


        // create an array of reals used for temporary storage of data read from the
        // file.
//        RealArrayLength := StrToInt(frmExtract.adeCellCount.Text);
//        SetLength(RealArray, RealArrayLength);
        if OpenDialog1.FilterIndex in [1,4] then
        begin
          FileStream := TFileStream.Create(OpenDialog1.FileName,
            fmOpenRead	or fmShareDenyWrite);
          try
            if CheckArrayFileType(FileStream) = mftFormatted then
            begin
              OpenDialog1.FilterIndex := 1;
            end
            else
            begin
              OpenDialog1.FilterIndex := 4;
            end;
          finally
            FileStream.Free;
          end;

        end;

        if OpenDialog1.FilterIndex in [1,2,3,6] then
        begin
          // Formatted file.
          
          // This opens the formatted head or drawdown file.
          AssignFile(ModflowOutput, OpenDialog1.FileName);
          // start reading the MODFLOW file at the beginning
          Reset(ModflowOutput);

          try
            begin
              while not Eof(ModflowOutput) do
              begin
                // if the user has pressed the cancel button, stop reading the file.
                Application.ProcessMessages;
                if CancelProcess then
                begin
                  break;
                end;
                // read the descriptor line at the beginning of the array
                Readln(ModflowOutput, DescriptorLine);
                // if the line is empty you've reached the end of the file so stop.
                if DescriptorLine = '' then
                begin
                  break;
                end;
                // check if this is a head or drawdown file.
                // quit if it is neither.
                if (Pos(HGU_Label, DescriptorLine) <= 0)
                  and (Pos(HeadLabel, DescriptorLine) <= 0)
                  and (Pos(DrawdownLabel, DescriptorLine) <= 0) then
                begin
                  Beep;
                  ShowMessage('Unable to read file');
                  Exit;
                end;
                Splitter := TStringList.Create;
                try
                  Splitter.Delimiter := ' ';
                  Splitter.DelimitedText := DescriptorLine;

                  // The revised code expects each of the inputs to be
                  // separated from the next by at least one space.
                  // If the number of rows, columns, or layers is >= 100000,
                  // this may not be true.

                  // get the elapsed time- 4'th item.
                  AString := Splitter[3];
                  TOTIM := InternationalStrToFloat(AString);
                  // get the number of columns - 6'th item
                  AString := Splitter[5];
                  NCOL := StrToInt(AString);
                  // get the number of rows - 7'th item
                  AString := Splitter[6];
                  NROW := StrToInt(AString);
                  // get the layer number - 8'th item
                  AString := Splitter[7];
                  ILAY := StrToInt(AString);
//                  if ILAY <= 0 then
//                  begin
//                    NROW := Abs(ILAY);
//                  end;


                  SetLength(DataArray, NROW, NCOL);

                  // Set the time to the total elapased time.
                  ATime := TOTIM;
                  AString := '';
                  ItemIndex := 0;
                  Splitter.Clear;
                  for RowIndex := 1 to NROW do
                  begin
                    // if the user pressed the cancel button, quit.
                    Application.ProcessMessages;
                    if CancelProcess then
                    begin
                      break;
                    end;
                    for ColIndex := 1 to NCOL do
                    begin
                      // if the user pressed the cancel button, quit.
                      Application.ProcessMessages;
                      if CancelProcess then
                      begin
                        break;
                      end;
                      // don't try to read past the end of the file.
                      if not Eof(ModflowOutput) then
                      begin
                        // allow the application to respond to the user pressing the cancel button.
                        Application.ProcessMessages;
                        // if the user pressed the cancel button, quit.
                        if CancelProcess then
                        begin
                          break;
                        end;
                        // read a number from the MODFLOW file
//                        Read(ModflowOutput, ADatum);

                        if ItemIndex >= Splitter.Count then
                        begin
                          Readln(ModflowOutput, AString);
                          Splitter.DelimitedText := Trim(AString);
                          ItemIndex := 0;
                        end;
                        Assert(ItemIndex < Splitter.Count);
                        ADatum := FortranStrToFloat(Splitter[ItemIndex]);
                        Inc(ItemIndex);

                        DataArray[RowIndex-1, ColIndex-1] := ADatum;
                        // Check if this is one of the data point we need to check.
                        // (This could be made faster by moving the String to integer
                        // conversion outside the loops.)
                      end
                      else
                      begin
                        // quit if you've reached the end of the file
                        break;
                      end;
                    end;
                  end;
                finally
                  Splitter.Free;
                end;
                // read the rest of the last line after the last number in the array.
//                Readln(ModflowOutput, AString);

                for PointsIndex := 1 to dgDataPoints.RowCount - 1 do
                begin
                  if (ILAY = Layers[PointsIndex-1]) or (ILAY < 0) then
                  begin
                    // Add the data point to the appropriate TLineSeries
                    ColIndex := Columns[PointsIndex-1];
                    if ILAY < 0 then
                    begin
                      RowIndex := Layers[PointsIndex-1];
                    end
                    else
                    begin
                      RowIndex := Rows[PointsIndex-1];
                    end;
                    ADatum := DataArray[RowIndex-1, ColIndex-1];
                    ASeries := MySeriesList[PointsIndex - 1];
                    ASeries.AddXY(ATime, ADatum, '', clTeeColor);
                  end;
                end;


              end;
            end;

          finally
            begin

              // close the MODFLOW file
              CloseFile(ModflowOutput);


            end;
          end;
        end
        else
        begin
          // binary file.
          FileStream := TFileStream.Create(OpenDialog1.FileName,
            fmOpenRead	or fmShareDenyWrite);
          try
            Precision := CheckArrayPrecision(FileStream);
            While FileStream.Position < FileStream.Size do
            begin
              case Precision of
                mpSingle:
                  begin
                    ReadSinglePrecisionModflowBinaryRealArray(FileStream,
                      KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                  end;
                mpDouble:
                  begin
                    ReadDoublePrecisionModflowBinaryRealArray(FileStream,
                      KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                  end;
              else Assert(False);
              end;

              ATime := TOTIM;
              for PointsIndex := 1 to dgDataPoints.RowCount - 1 do
              begin
                if (ILAY = Layers[PointsIndex-1]) or (ILAY < 0) then
                begin
                  ColIndex := Columns[PointsIndex-1]-1;
                  if ILAY < 0 then
                  begin
                    RowIndex := Layers[PointsIndex-1]-1;
                  end
                  else
                  begin
                    RowIndex := Rows[PointsIndex-1]-1;
                  end;
                  ADatum := AnArray[RowIndex, ColIndex];

                  // store the data value temporarily in RealArray
//                  RealArray[PointsIndex - 1] := ADatum;
                  // Add the data point to the appropriate TLineSeries
                  ASeries := MySeriesList[PointsIndex - 1];
                  ASeries.AddXY(ATime, ADatum, '', clTeeColor);
                end;
              end;
            end;

          finally
            FileStream.Free;
          end;
        end;
      finally
        // store the data in the memo.

        if MySeriesList.Count > 0 then
        begin
          ASeries := MySeriesList[0];
          PointCount := ASeries.Count;
          for PointIndex := 0 to PointCount -1 do
          begin
            DataString := '';
            for SeriesIndex := 0 to MySeriesList.Count -1 do
            begin
              ASeries := MySeriesList[SeriesIndex];
              if SeriesIndex = 0 then
              begin
                DataString := FloatToStr(ASeries.XValues[PointIndex])+ Chr(9);
              end;
              if PointIndex < ASeries.Count then
              begin
                DataString := DataString + FloatToStr(ASeries.YValues[PointIndex]);
              end
              else
              begin
                DataString := DataString + ' ';
              end;
              if SeriesIndex < MySeriesList.Count -1 then
              begin
                DataString := DataString + Chr(9);
              end;

            end;
            Lines.Add(DataString);
          end;
        end;
        RichEdit1.Lines.AddStrings(Lines);
        FLinesToSave.Assign(Lines);

        for ChartIndex := 0 to ChartHydExtractor.SeriesList.Count - 1 do
        begin
          ASeries
            {AChartSeries}:= ChartHydExtractor.Series[ChartIndex] as
              TLineSeries;
          ASeries.Active := dgDataPoints.Cells[3, ChartIndex + 1]
            = dgDataPoints.Columns[3].PickList.Strings[1];
          ASeries.EndUpdate;
        end;

        // restore the cursor
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    // enable or disable controls as appropriate.
    btnCancel.Enabled := False;
    btnRead.Enabled := True;
    adeCellCount.Enabled := True;
    dgDataPoints.Enabled := True;
    Lines.Free;
    ChartHydExtractor.Invalidate;
    //    Chart1.Invalidate;
  end;

end;

procedure TfrmExtract.ReadMt3dObservationsFile;
const
  HeadLabel = 'HEAD';
  DrawdownLabel = 'DRAWDOWN';
var
  Mt3dObservations: TextFile; // the formatted head or drawdown file
  ADatum: double; // a number read from the formatted head or drawdown file
  AString: string;
  RowIndex: integer; // indexes to the colums and rows
  PointsIndex: integer;
    // an index to the point for which a hydrograph is to be extracted.
  DataString: string;
    // This is used to store the formatted data that will be placed on a line of the TMemo
  ATime: double; // elapsed time for the current time step
  CaptionString: string;
  ASeries: TLineSeries; // a hydrograph plotted on the chart
  RealArray: array of double; // an array of values read from
  RealArrayLength: integer;
  ALine: string;
  FoundEnd: boolean;
  LineIndex: integer;
  count: integer;
//  TimeStep: integer;
  TimeStep: string[6];
  Lines: TStringList;
  ColorIndex: integer;
  TimeString: string;
begin
  dgDataPoints.Visible := True;
  //  Put the name of the file on the caption of the form.
  Caption := 'GW_Chart: ' + OpenDialog1.FileName;
  // disable thing that shouldn't be changed while reading the file.
  btnSave.Enabled := True;
  btnCancel.Enabled := True;
  btnRead.Enabled := False;
  adeCellCount.Enabled := False;
  dgDataPoints.Enabled := False;
  Lines := TStringList.Create;
  try
    begin
      // ClearSeriesList destroys all the TLineSeries on the chart and
      // clears SeriesList
      ClearSeriesList;

      // initialize CancelProcess. CancelProcess will be tested while the
      // file is being read to see if the user has pressed the cancel button.
      CancelProcess := False;

      // initialize the memo
      RichEdit1.Clear;
      Lines.Add(Chr(9) + '(Column,Row,Layer)');

      // This opens the formatted head or drawdown file.
      AssignFile(Mt3dObservations, OpenDialog1.FileName);

      // start reading the MODFLOW file at the beginning
      Reset(Mt3dObservations);
      try
        // read title line
        readln(Mt3dObservations, ALine);
        if EOF(Mt3dObservations) then
        begin
          MessageDlg('The file is empty.', mtWarning, [mbOK], 0);
          Exit;
        end;
        Count := 0;

        LineIndex := 0;
        repeat
          begin
            readln(Mt3dObservations, ALine);
            Inc(Count);
            TimeString := Trim(Copy(ALine, 1, 20));
            FoundEnd := TimeString <> '';
            if not FoundEnd then
            begin
              ALine := Copy(ALine, 20, Length(ALine));
              while Length(ALine) > 0 do
              begin
                Inc(LineIndex);
                adeCellCount.Text := IntToStr(LineIndex);
                adeCellCountChange(adeCellCount);
                dgDataPoints.Cells[2, LineIndex] := Trim(Copy(ALine, 1, 4));
                dgDataPoints.Cells[1, LineIndex] := Trim(Copy(ALine, 5, 4));
                dgDataPoints.Cells[0, LineIndex] := Trim(Copy(ALine, 9, 4));
                ALine := Copy(ALine, 15, Length(ALine));
                dgDataPoints.Cells[3, LineIndex] :=
                  dgDataPoints.Columns[3].PickList[0];
              end;
            end;
          end
        until FoundEnd;

      finally
        CloseFile(Mt3dObservations);
      end;
      try
        // start reading the MODFLOW file at the beginning
        Reset(Mt3dObservations);

        for LineIndex := 1 to Count do
        begin
          readln(Mt3dObservations, ALine);
        end;

        // create a headers for the data in the memo and create a TLineSeries
        // for each MODFLOW cell.
        AString := 'Time';
        ColorIndex := -1;
        for RowIndex := 1 to dgDataPoints.RowCount - 1 do
        begin
          Inc(ColorIndex);
          if ColorIndex >= Length(ColorPalette) then
          begin
            ColorIndex := 0;
          end;

          CaptionString := '(' + dgDataPoints.Cells[0, RowIndex] + ',' +
            dgDataPoints.Cells[1, RowIndex] + ',' +
            dgDataPoints.Cells[2, RowIndex] + ')';
          AString := AString + Chr(9) + CaptionString;

          ASeries := TLineSeries.Create(ChartHydExtractor);
          ASeries.XValues.Order := loNone;
          ASeries.SeriesColor := ColorPalette[ColorIndex];
          ASeries.ParentChart := ChartHydExtractor;
          MySeriesList.Add(ASeries);
          ASeries.Title := CaptionString;
          ASeries.Pointer.Visible := True;
          ASeries.Active := cbPlot.Checked and (dgDataPoints.Cells[3, RowIndex]
            = dgDataPoints.Columns[3].PickList.Strings[1]);
        end;
        //      Memo1.Lines.Add(AString);
        Lines.Add(AString);

        // this may take a while so change the cursor.
        Screen.Cursor := crHourGlass;

        RealArrayLength := StrToInt(frmExtract.adeCellCount.Text);
        SetLength(RealArray, RealArrayLength);
        try
          begin

            // we don't need to do something on the first time step that
            // we need to do on the others to initialize a variable that
            // can be checked to see if it is the first time step.
            while not Eof(Mt3dObservations) do
            begin
              // if the user has pressed the cancel button, stop reading the file.
              Application.ProcessMessages;
              if CancelProcess then
              begin
                break;
              end;

              Read(Mt3dObservations, TimeStep);
              Read(Mt3dObservations, ATime);
              if Eof(Mt3dObservations) then
              begin
                break;
              end;


              for RowIndex := 1 to dgDataPoints.RowCount - 1 do
              begin
                Read(Mt3dObservations, ADatum);
                RealArray[RowIndex - 1] := ADatum;
                // Add the data point to the appropriate TLineSeries
                ASeries := MySeriesList[RowIndex - 1];
                ASeries.AddXY(ATime, ADatum, '', clTeeColor);
              end;
              // read the rest of the last line after the last number in the array.
              Readln(Mt3dObservations, AString);

              // add that data to RichEdit1.
              DataString := FloatToStr(ATime);
              for PointsIndex := 0 to RealArrayLength - 1 do
              begin
                DataString := DataString + Chr(9) +
                  FloatToStr(RealArray[PointsIndex]);
              end;
              //            Memo1.Lines.Add(DataString);
              Lines.Add(DataString);

            end;
          end;

        finally
          begin
            RichEdit1.Lines.AddStrings(Lines);
            FLinesToSave.Assign(Lines);
            // restore the cursor
            Screen.Cursor := crDefault;
          end;
        end;
      finally
        CloseFile(Mt3dObservations);
      end;
    end;
  finally
    // enable or disable controls as appropriate.
    btnCancel.Enabled := False;
    btnRead.Enabled := True;
    adeCellCount.Enabled := True;
    dgDataPoints.Enabled := True;
    Lines.Free;
  end;

end;

procedure TfrmExtract.ReadFile;
begin
  ChartHydExtractor.BottomAxis.Automatic := True;
  ChartHydExtractor.LeftAxis.Automatic := True;
  case TProgramChoice(rgProgramChoice.ItemIndex) of
    pcModflow:
      begin
        ReadModflowFile;
        // This is placed in a separate procedure to facilitate
        // updating the program to read data from programs other than the
        // currently supported programs.
      end;
    pcMt3dObs:
      begin
        ReadMt3dObservationsFile;
        // This is placed in a separate procedure to facilitate
        // updating the program to read data from programs other than the
        // currently supported programs.
      end;
    pcMt3dConc:
      begin
        ReadMt3dConcFile;
      end;
    pcGwtObs:
      begin
        ReadMoc3dObservationsFile;
        // This is placed in a separate procedure to facilitate
        // updating the program to read data from programs other than the
        // currently supported programs.
      end;
    pcSutra1:
      begin
        ReadSutraFile;
        // This is placed in a separate procedure to facilitate
        // updating the program to read data from programs other than the
        // currently supported programs.
      end;
    pcsutra2:
      begin
        ReadNewSutraFile;
        // This is placed in a separate procedure to facilitate
        // updating the program to read data from programs other than the
        // currently supported programs.
      end;
    pcSutra2_1:
      begin
        ReadNewSutraFile2;
        // This is placed in a separate procedure to facilitate
        // updating the program to read data from programs other than the
        // currently supported programs.
      end;
    pcHydMode:
      begin
        ReadHydmodFile;
      end;
    pcMf6Obs:
      begin
        ReadMf6Obs;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TfrmExtract.ReadMf6Obs;
var
  OutputFile: TMf6ObsOutputFile;
  FileType: TFileType;
  ColorIndex: integer;
  Time: Double;
  Values: TDoubleArray;
  ASeries: TLineSeries;
  Lines: TStringlist;
  ALine: string;
  ObsIndex: Integer;
  CaptionString: String;
  PlotText: String;
begin
  Caption := 'GW_Chart: ' + OpenDialog1.FileName;
  if cbPlot.Checked then
  begin
    PlotText := 'Yes';
  end
  else
  begin
    PlotText := 'No';
  end;

  // disable thing that shouldn't be changed while reading the file.
  btnSave.Enabled := True;
  btnCancel.Enabled := True;
  btnRead.Enabled := False;
  adeCellCount.Enabled := False;
  dgDataPoints.Enabled := False;
  Lines := TStringList.Create;
  try
    if LowerCase(ExtractFileExt(OpenDialog1.FileName))= '.csv' then
    begin
      FileType := ftText;
    end
    else
    begin
      FileType := ftBinary;
    end;

    OutputFile := TMf6ObsOutputFile.Create(OpenDialog1.FileName, FileType);
    try
      ClearSeriesList;
      // initialize the memo
      RichEdit1.Clear;
      ALine := 'Time';
      for ObsIndex := 0 to OutputFile.NumberOfObservations -1 do
      begin
        ALine := ALine + #9 + OutputFile.ObsName[ObsIndex];
      end;
      Lines.Add(ALine);

      ColorIndex := -1;
      dgDataNodes.RowCount := OutputFile.NumberOfObservations + 1;
      try
        for ObsIndex := 0 to OutputFile.NumberOfObservations -1 do
        begin
          Inc(ColorIndex);
          if ColorIndex >= Length(ColorPalette) then
          begin
            ColorIndex := 0;
          end;
          CaptionString := OutputFile.ObsName[ObsIndex];
          dgDataNodes.Cells[0, ObsIndex+1] := CaptionString;
          dgDataNodes.Cells[1, ObsIndex+1] := PlotText;
  //        AString := AString + Chr(9) + CaptionString;

          ASeries := TLineSeries.Create(ChartHydExtractor);
          ASeries.XValues.Order := loNone;
          ASeries.SeriesColor := ColorPalette[ColorIndex];
          ASeries.ParentChart := ChartHydExtractor;
          MySeriesList.Add(ASeries);
          ASeries.Title := CaptionString;
          ASeries.Pointer.Visible := True;
          ASeries.Active := cbPlot.Checked {and (dgDataPoints.Cells[3, RowIndex]
            = dgDataPoints.Columns[3].PickList.Strings[1])};
          ASeries.BeginUpdate;
        end;
      finally

      end;                      

      Screen.Cursor := crHourGlass;
      try
        while True do
        begin
          if OutputFile.ReadNextData(Time, Values) then
          begin
            ALine := FloatToStr(Time);
            Assert(Length(Values) = MySeriesList.Count);
            for ObsIndex := 0 to OutputFile.NumberOfObservations - 1 do
            begin
              ASeries := MySeriesList[ObsIndex];
              ASeries.AddXY(Time, Values[ObsIndex], '', clTeeColor);
              ALine := ALine + #9 + FloatToStr(Values[ObsIndex]);
            end;
            Lines.Add(ALine);
          end
          else
          begin
            RichEdit1.Lines.AddStrings(Lines);
            FLinesToSave.Assign(Lines);
            Break;
          end;

        end;
      finally
        for ObsIndex := 0 to OutputFile.NumberOfObservations -1 do
        begin
          ASeries := MySeriesList[ObsIndex];
          ASeries.EndUpdate;
        end;
        Screen.Cursor := crDefault;
      end;


    finally
      OutputFile.Free;
    end;

  finally
    btnCancel.Enabled := False;
    btnRead.Enabled := True;
    adeCellCount.Enabled := True;
    Lines.Free;
  end;


end;

procedure TfrmExtract.ClearNodeSelection;
var
  Index: integer;
begin
  for Index := 1 to dgDataNodes.RowCount - 1 do
  begin
    dgDataNodes.Cells[1, Index] := '';
  end;

end;

procedure TfrmExtract.btnReadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ExplanationVisible := True;
    sbFormat.Enabled := True;
    sbImage.Enabled := True;
    FormatChart1.Enabled := True;
    SaveasImage1.Enabled := True;
    ClearNodeSelection;
    ReadFile;
  end;
end;

procedure TfrmExtract.btnSaveClick(Sender: TObject);
//var
//  Lines: TStringList;
begin
  // save the memo to a file.
  if SaveDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      FLinesToSave.SaveToFile(SaveDialog1.FileName);

      // converting to ASCII from Rich Text format doesn't work.
//      Lines := TStringList.Create;
//      try
//        Lines.AddStrings(RichEdit1.Lines);
//        Lines.SaveToFile(SaveDialog1.FileName);
//      finally
//        Lines.Free;
//      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmExtract.About1Click(Sender: TObject);
begin
  // show the "About" form
  frmAbout.ShowModal;
end;

procedure TfrmExtract.btnCancelClick(Sender: TObject);
begin
  // CancelProcess is checked while reading a file. The program will
  // stop reading the file if CancelProcess is true.
  CancelProcess := True;
end;

procedure TfrmExtract.ReadCellLocations(FileName: string);
var
  RowIndex: integer;
  ColIndex: integer;
  DataFile: TextFile;
  RowCount: integer;
  AnIndex: integer;
  AString: string;
  SpacePos: integer;
begin

  if FileExists(FileName) then
  begin
    if TProgramChoice(rgProgramChoice.ItemIndex) = pcModflow then
    begin
      AssignFile(DataFile, FileName);
      Reset(DataFile);
      try
        // read the number of cells.
        if not Eof(DataFile) then
        begin
          read(DataFile, RowCount);
          adeCellCount.Text := IntToStr(RowCount);
        end;

        // read the row, column, and layer numbers.
        for RowIndex := 1 to dgDataPoints.RowCount - 1 do
        begin
          for ColIndex := 0 to dgDataPoints.ColCount - 2 do
          begin
            if not Eof(DataFile) then
            begin
              read(DataFile, AnIndex);
              if not Eof(DataFile) then
              begin
                dgDataPoints.Cells[ColIndex, RowIndex] := IntToStr(AnIndex);
              end;
            end;
          end;
          if not Eof(DataFile) then
          begin
            ReadLn(DataFile, AString);
            AString := Trim(AString);
            SpacePos := Pos(' ', AString);
            if SpacePos > 1 then
            begin
              AString := Copy(AString, 1, SpacePos-1);
            end;
            if SameText(AString, 'No') then
            begin
              AString := 'No';
            end;
            if SameText(AString, 'Yes') then
            begin
              AString := 'Yes';
            end;
            if AString <> '' then
              dgDataPoints.Cells[3, RowIndex] := AString;
          end;

        end;
      finally
        // close the file.
        CloseFile(DataFile)
      end;
    end;
  end;
end;

procedure TfrmExtract.FormCreate(Sender: TObject);
{var
  FileName : string; }
begin
  FLinesToSave:= TStringList.Create;
  comboUnits.ItemIndex := 0;
  ExplanationVisible := False;
  // prevent the top panel from getting to small.
  Panel1.Constraints.MinHeight := Panel1.Height;
  // create the list of TLineSeries
  MySeriesList := TList.Create;
  // initialize the text in the string grid.

  dgDataPoints.Cells[0, 1] := '1';
  dgDataPoints.Cells[1, 1] := '1';
  dgDataPoints.Cells[2, 1] := '1';
  dgDataPoints.Cells[3, 1] := dgDataPoints.Columns[3].PickList.Strings[1];
  // See if there are any command line parameters.
  rgProgramChoiceClick(rgProgramChoice);
end;

procedure TfrmExtract.sgDataPointsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  AValue: integer;
begin
  // test that the user has entered a valid number. If not, restore the cell to
  // what was in it before. (See sgDataPointsSelectCell.)
  try
    begin
      if (ARow > 0) and not (Value = '') then
      begin
        AValue := StrToInt(dgDataPoints.Cells[ACol, ARow]);
        if AValue < 1 then
        begin
          Beep;
          dgDataPoints.Cells[ACol, ARow] := CurrentText;
        end;
      end;
    end
  except on EConvertError do
    begin
      Beep;
      dgDataPoints.Cells[ACol, ARow] := CurrentText;
    end;
  end;
end;

procedure TfrmExtract.sgDataPointsSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  // temporily store the text in the current cell
  CurrentText := dgDataPoints.Cells[Col, Row];
end;

procedure TfrmExtract.adeCellCountChange(Sender: TObject);
var
  OldRowCount: integer;
  Index: integer;
begin
  // change the number of rows in response to a change in the number of cells
  // that a user has specified.
  if not (csLoading in frmExtract.ComponentState) then
  begin
    OldRowCount := dgDataPoints.RowCount;
    dgDataPoints.RowCount := StrToInt(adeCellCount.Text) + 1;
    dgDataNodes.RowCount := dgDataPoints.RowCount;
    if dgDataPoints.RowCount > OldRowCount then
    begin
      for Index := OldRowCount to dgDataPoints.RowCount - 1 do
      begin
        dgDataPoints.Cells[0, Index] := '1';
        dgDataPoints.Cells[1, Index] := '1';
        dgDataPoints.Cells[2, Index] := '1';
        dgDataPoints.Cells[3, Index]
          := dgDataPoints.Columns[3].PickList.Strings[1];

        dgDataNodes.Cells[0, Index] := '1';

        if (dgDataNodes.Cells[1, Index] = '') then
        begin
          if cbPlot.Checked then
          begin
            dgDataNodes.Cells[1, Index]
              := dgDataNodes.Columns[1].PickList.Strings[1];
          end
          else
          begin
            dgDataNodes.Cells[1, Index]
              := dgDataNodes.Columns[1].PickList.Strings[0];
          end;
        end;
      end;
    end;
  end;

end;

procedure TfrmExtract.ClearSeriesList;
var
  Index: integer;
  ASeries: TLineSeries;
begin
  // destroy the series in SeriesList and clear SeriesList.
  for Index := 0 to MySeriesList.Count - 1 do
  begin
    ASeries := MySeriesList[Index];
    ASeries.Free;
  end;
  MySeriesList.Clear;
end;

procedure TfrmExtract.FormDestroy(Sender: TObject);
begin
  // Destroy SeriesList.
  MySeriesList.Free;
  FLinesToSave.Free;
end;

procedure TfrmExtract.Help1Click(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfrmExtract.ReadCellLocations1Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    SaveDialog2.FileName := OpenDialog2.FileName;
    ReadCellLocations(OpenDialog2.FileName);
  end;
end;

procedure TfrmExtract.SaveCellLocations1Click(Sender: TObject);
var
  AStringList: TStringList;
  Index: integer;
begin
  if SaveDialog2.Execute then
  begin
    AStringList := TStringList.Create;
    try
      AStringList.Add(adeCellCount.Text);
      for Index := 1 to dgDataPoints.RowCount - 1 do
      begin
        AStringList.Add(dgDataPoints.Cells[0, Index] + ' ' +
          dgDataPoints.Cells[1, Index] + ' ' +
          dgDataPoints.Cells[2, Index] + ' ' +
          dgDataPoints.Cells[3, Index]);
      end;
      AStringList.SaveToFile(SaveDialog2.FileName);
    finally
      AStringList.Free;
    end;

  end;

end;

procedure TfrmExtract.dgDataPointsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  ASeries: TChartSeries;
  ChartIndex: integer;
begin
  if (ACol = 3) and (ARow > 0) then
  begin
    if TProgramChoice(rgProgramChoice.ItemIndex) = pcGwtObs then
    begin
      ChartIndex := (ARow - 1) * 2 + rgMOC3D.ItemIndex;
      if ChartHydExtractor.SeriesList.Count > ChartIndex then
      begin
        ASeries := ChartHydExtractor.Series[ChartIndex];
        ASeries.Active := dgDataPoints.Cells[3, ARow]
          = dgDataPoints.Columns[3].PickList.Strings[1];
      end;
    end
    else
    begin
      if ChartHydExtractor.SeriesList.Count > ARow - 1 then
      begin
        ASeries := ChartHydExtractor.Series[ARow - 1];
        ASeries.Active := dgDataPoints.Cells[3, ARow]
          = dgDataPoints.Columns[3].PickList.Strings[1];
      end;
    end;
  end;

end;

procedure TfrmExtract.rgProgramChoiceClick(Sender: TObject);
begin
  case TProgramChoice(rgProgramChoice.ItemIndex) of
    pcModflow:
      begin
        OpenDialog1.Filter :=
          'formatted head files (*.fhd, *.hds)|*.fhd;*.hds|'
          + 'formatted drawdown files (*.fdn)|*.fdn|'
          + 'formatted HUF head files (*.hhd)|*.hhd|'
          + 'binary head files (*.bhd, *.hds)|*.bhd;*.hds|'
          + 'binary drawdown files (*.bdn)|*.bdn|'
          + 'Any formatted file (*.*)|*.*|'
          + 'Any binary file (*.*)|*.*';

        ChartHydExtractor.LeftAxis.Title.Caption :=
          UpperCase('head or drawdown');
        ChartHydExtractor.BottomAxis.Title.Caption := 'time';
        dgDataPoints.Visible := True;
        dgDataNodes.Visible := False;
        dgDataNodes.Columns[0].Title.Caption := 'Node';
        dgDataNodes.ColCount := 3;
        dgDataNodes.Columns[2].Format := cfNumber;
        dgDataNodes.Columns[2].Title.Caption := 'Elevation';
        rgMOC3D.Enabled := False;
        rgSutra.Enabled := False;
        lblCount.Caption := 'Number of Cells';
        comboUnits.Enabled := False;
        comboUnits.Color := clBtnFace;
        adeDensity.Enabled := False;
        adeG.Enabled := False;
      end;
    pcMt3dObs:
      begin
        OpenDialog1.Filter :=
          'MT3D Observation files (*.obs,*.mto)|*.obs;*.mto|All Files|*.*';
        ChartHydExtractor.LeftAxis.Title.Caption := UpperCase('concentration');
        ChartHydExtractor.BottomAxis.Title.Caption := 'time';
        dgDataPoints.Visible := False;
        dgDataNodes.Visible := False;
        dgDataNodes.Columns[0].Title.Caption := 'Node';
        dgDataNodes.ColCount := 3;
        dgDataNodes.Columns[2].Format := cfNumber;
        dgDataNodes.Columns[2].Title.Caption := 'Elevation';
        rgMOC3D.Enabled := False;
        rgSutra.Enabled := False;
        lblCount.Caption := 'Number of Cells';
        comboUnits.Enabled := False;
        comboUnits.Color := clBtnFace;
        adeDensity.Enabled := False;
        adeG.Enabled := False;
      end;
    pcMt3dConc:
      begin
        OpenDialog1.Filter :=
          'formatted head files (*.ucn)|*.ucn|'
          + 'Any binary file (*.*)|*.*';

        ChartHydExtractor.LeftAxis.Title.Caption :=
          UpperCase('Concentration');
        ChartHydExtractor.BottomAxis.Title.Caption := 'time';
        dgDataPoints.Visible := True;
        dgDataNodes.Visible := False;
        dgDataNodes.Columns[0].Title.Caption := 'Node';
        dgDataNodes.ColCount := 3;
        dgDataNodes.Columns[2].Format := cfNumber;
        dgDataNodes.Columns[2].Title.Caption := 'Elevation';
        rgMOC3D.Enabled := False;
        rgSutra.Enabled := False;
        lblCount.Caption := 'Number of Cells';
        comboUnits.Enabled := False;
        comboUnits.Color := clBtnFace;
        adeDensity.Enabled := False;
        adeG.Enabled := False;
      end;
    pcGwtObs:
      begin
        OpenDialog1.Filter :=
          'GWT and MOC3D Observation files (*.oba)|*.oba|All Files|*.*';
        ChartHydExtractor.LeftAxis.Title.Caption :=
          UpperCase(rgMOC3D.Items[rgMOC3D.ItemIndex]);
        ChartHydExtractor.BottomAxis.Title.Caption := 'time';
        dgDataPoints.Visible := False;
        dgDataNodes.Visible := False;
        dgDataNodes.Columns[0].Title.Caption := 'Node';
        dgDataNodes.ColCount := 3;
        dgDataNodes.Columns[2].Format := cfNumber;
        dgDataNodes.Columns[2].Title.Caption := 'Elevation';
        rgMOC3D.Enabled := True;
        rgSutra.Enabled := False;
        lblCount.Caption := 'Number of Cells';
        comboUnits.Enabled := False;
        comboUnits.Color := clBtnFace;
        adeDensity.Enabled := False;
        adeG.Enabled := False;
      end;
    pcSutra1:
      begin
        OpenDialog1.Filter := 'SUTRA Listing files (*.d6)|*.d6|All Files|*.*';
        ChartHydExtractor.LeftAxis.Title.Caption :=
          UpperCase(rgSutra.Items[rgSutra.ItemIndex]);
        ChartHydExtractor.BottomAxis.Title.Caption := 'time ('
          + comboUnits.Text + ')';
        dgDataPoints.Visible := False;
        dgDataNodes.Visible := False;
        dgDataNodes.Columns[0].Title.Caption := 'Node';
        dgDataNodes.ColCount := 3;
        dgDataNodes.Columns[2].Format := cfNumber;
        dgDataNodes.Columns[2].Title.Caption := 'Elevation';
        rgMOC3D.Enabled := False;
        rgSutra.Enabled := True;
        lblCount.Caption := 'Number of Nodes';
        comboUnits.Enabled := True;
        comboUnits.Color := clWindow;
        adeDensity.Enabled := True;
        adeG.Enabled := True;
      end;
    pcsutra2: // SUTRA 2D/3D
      begin
        OpenDialog1.Filter :=
          'SUTRA Observation file (*.obs)|*.obs|All Files|*.*';
        ChartHydExtractor.LeftAxis.Title.Caption :=
          UpperCase(rgSutra.Items[rgSutra.ItemIndex]);
        ChartHydExtractor.BottomAxis.Title.Caption := 'time ('
          + comboUnits.Text + ')';
        dgDataPoints.Visible := False;
        dgDataNodes.Visible := False;
        dgDataNodes.Columns[0].Title.Caption := 'Node';
        dgDataNodes.ColCount := 3;
        dgDataNodes.Columns[2].Format := cfNumber;
        dgDataNodes.Columns[2].Title.Caption := 'Elevation';
        rgMOC3D.Enabled := False;
        rgSutra.Enabled := True;
        lblCount.Caption := 'Number of Nodes';
        comboUnits.Enabled := True;
        comboUnits.Color := clWindow;
        adeDensity.Enabled := True;
        adeG.Enabled := True;
      end;
    pcSutra2_1: // SUTRA 2.1
      begin
        OpenDialog1.Filter :=
          'SUTRA Observation file (*.obs)|*.obs|All Files|*.*';
        ChartHydExtractor.LeftAxis.Title.Caption :=
          UpperCase(rgSutra.Items[rgSutra.ItemIndex]);
        ChartHydExtractor.BottomAxis.Title.Caption := 'time ('
          + comboUnits.Text + ')';
        dgDataPoints.Visible := False;
        dgDataNodes.Visible := False;
        dgDataNodes.Columns[0].Title.Caption := 'Node';
        dgDataNodes.ColCount := 3;
        dgDataNodes.Columns[2].Format := cfNumber;
        dgDataNodes.Columns[2].Title.Caption := 'Elevation';
        rgMOC3D.Enabled := False;
        rgSutra.Enabled := True;
        lblCount.Caption := 'Number of Nodes';
        comboUnits.Enabled := True;
        comboUnits.Color := clWindow;
        adeDensity.Enabled := True;
        adeG.Enabled := True;
      end;
    pcHydMode:
      begin
        OpenDialog1.Filter :=
          'HYDMOD output files (*.hyd_out)|*.hyd_out|'
          + 'Any binary file (*.*)|*.*';

        ChartHydExtractor.LeftAxis.Title.Caption :=
          UpperCase('Value');
        ChartHydExtractor.BottomAxis.Title.Caption := 'time';
        dgDataPoints.Visible := False;
        dgDataNodes.Visible := True;
        dgDataNodes.Columns[0].Title.Caption := 'Obs Name';
        dgDataNodes.ColCount := 2;
        rgMOC3D.Enabled := False;
        rgSutra.Enabled := False;
        lblCount.Caption := 'Number of Observation Points';
        comboUnits.Enabled := False;
        comboUnits.Color := clBtnFace;
        adeDensity.Enabled := False;
        adeG.Enabled := False;
        Splitter2.Left := dgDataNodes.Left + dgDataNodes.Width;
      end;
    pcMf6Obs:
      begin
        OpenDialog1.Filter :=
          'CSV files (*.csv)|*.csv|'
          + 'Binary file (*.bin)|*.bin|'
          + 'Any file (*.*)|*.*';

        ChartHydExtractor.LeftAxis.Title.Caption :=
          UpperCase('Value');
        ChartHydExtractor.BottomAxis.Title.Caption := 'time';
        dgDataPoints.Visible := False;
        dgDataNodes.Visible := True;
        dgDataNodes.Columns[0].Title.Caption := 'Series Name';
        dgDataNodes.ColWidths[0] := 70;
        dgDataNodes.ColCount := 2;
        rgMOC3D.Enabled := False;
        rgSutra.Enabled := False;
        lblCount.Caption := 'Number of Observation Series';
        comboUnits.Enabled := False;
        comboUnits.Color := clBtnFace;
        adeDensity.Enabled := False;
        adeG.Enabled := False;
        Splitter2.Left := dgDataNodes.Left + dgDataNodes.Width;
      end;
  else
    begin
      Assert(False);
    end;
  end;

end;

procedure TfrmExtract.ReadMoc3dObservationsFile;
const
  HeadLabel = 'HEAD';
  DrawdownLabel = 'DRAWDOWN';
var
  Moc3dOutput: TextFile; // the formatted head or drawdown file
  ADatum: double; // a number read from the formatted head or drawdown file
  AString: string;
  TOTIM: double; // elapsed time
  RowIndex: integer; // indexes to the colums and rows
  PointsIndex: integer;
    // an index to the point for which a hydrograph is to be extracted.
  DataString: string;
    // This is used to store the formatted data that will be placed on a line of the TMemo
  ATime: double; // elapsed time for the current time step
  CaptionString: string;
  ASeries: TLineSeries; // a hydrograph plotted on the chart
  RealArray: array of double; // an array of values read from
  RealArrayLength: integer;
  ChartIndex: integer;
  AChartSeries: TChartSeries;
  Lines: TStringlist;
  ALine: string;
  LineIndex: integer;
  FoundEnd: boolean;
  Position: integer;
  SearchString: string;
  SearchStringLength: integer;
  CellIndex: integer;
  CorrectType: boolean;
  SeriesIndex: integer;
  ColorIndex: integer;
begin
  //  Put the name of the file on the caption of the form.
  Caption := 'GW_Chart: ' + OpenDialog1.FileName;
  // disable thing that shouldn't be changed while reading the file.
  btnSave.Enabled := True;
  btnCancel.Enabled := True;
  btnRead.Enabled := False;
  adeCellCount.Enabled := False;
  dgDataPoints.Enabled := False;
  Lines := TStringlist.Create;
  try
    begin
      // ClearSeriesList destroys all the TLineSeries on the chart and
      // clears SeriesList
      ClearSeriesList;

      // initialize CancelProcess. CancelProcess will be tested while the
      // file is being read to see if the user has pressed the cancel button.
      CancelProcess := False;

      // initialize RichEdit1
      RichEdit1.Clear;
      Lines.Add(Chr(9) + 'Head and Concentration at (Column,Row,Layer)');

      AssignFile(Moc3dOutput, OpenDialog1.FileName);
      // start reading the MOC3D file at the beginning
      Reset(Moc3dOutput);
      try
        // read title lines
        readln(Moc3dOutput, ALine);
        readln(Moc3dOutput, ALine);
        readln(Moc3dOutput, ALine);

        LineIndex := 0;
        repeat
          begin
            SearchString := 'H & C AT';
            SearchStringLength := Length(SearchString);
            Position := Pos(SearchString, ALine);
            FoundEnd := Position = 0;
            if not FoundEnd then
            begin
              ALine := Copy(ALine, Position + SearchStringLength,
                Length(ALine));
              if Length(ALine) > 0 then
              begin
                Inc(LineIndex);
                adeCellCount.Text := IntToStr(LineIndex);
                adeCellCountChange(adeCellCount);
                SearchString := ',';

                Position := Pos(SearchString, ALine);
                dgDataPoints.Cells[2, LineIndex] := Trim(Copy(ALine, 1, Position
                  - 1));
                ALine := Trim(Copy(ALine, Position + 1, Length(ALine)));

                Position := Pos(SearchString, ALine);
                dgDataPoints.Cells[1, LineIndex] := Trim(Copy(ALine, 1, Position
                  - 1));
                ALine := Trim(Copy(ALine, Position + 1, Length(ALine)));

                SearchString := ' ';
                Position := Pos(SearchString, ALine);
                dgDataPoints.Cells[0, LineIndex] := Trim(Copy(ALine, 1, Position
                  - 1));
                ALine := Trim(Copy(ALine, Position + 1, Length(ALine)));

                dgDataPoints.Cells[3, LineIndex] :=
                  dgDataPoints.Columns[3].PickList[0];
              end;
            end;
          end
        until FoundEnd;

      finally
        CloseFile(Moc3dOutput);
      end;

      // create a headers for the data in the memo and create a TLineSeries
      // for each MODFLOW cell.
      AString := 'Time';
      ColorIndex := -1;
      for RowIndex := 1 to dgDataPoints.RowCount - 1 do
      begin
        Inc(ColorIndex);
        if ColorIndex >= Length(ColorPalette) then
        begin
          ColorIndex := 0;
        end;

        CaptionString := '(' +
          dgDataPoints.Cells[0, RowIndex] + ',' +
          dgDataPoints.Cells[1, RowIndex] + ',' +
          dgDataPoints.Cells[2, RowIndex] + ')';
        AString := AString + Chr(9) + 'H & C AT' + Chr(9) + CaptionString;

        ASeries := TLineSeries.Create(ChartHydExtractor);
        ASeries.XValues.Order := loNone;
        ASeries.SeriesColor := ColorPalette[ColorIndex];
        ASeries.ParentChart := ChartHydExtractor;
        MySeriesList.Add(ASeries);
        ASeries.Title := CaptionString;
        ASeries.Pointer.Visible := True;
        ASeries.Active := cbPlot.Checked and (dgDataPoints.Cells[3, RowIndex]
          = dgDataPoints.Columns[3].PickList.Strings[1])
          and (rgMOC3D.ItemIndex = 0);

        ASeries := TLineSeries.Create(ChartHydExtractor);
        ASeries.XValues.Order := loNone;
        ASeries.SeriesColor := ColorPalette[ColorIndex];
        ASeries.ParentChart := ChartHydExtractor;
        MySeriesList.Add(ASeries);
        ASeries.Title := CaptionString;
        ASeries.Pointer.Visible := True;
        ASeries.Active := cbPlot.Checked and (dgDataPoints.Cells[3, RowIndex]
          = dgDataPoints.Columns[3].PickList.Strings[1])
          and (rgMOC3D.ItemIndex = 1);
      end;
      Lines.Add(AString);

      // this may take a while so change the cursor.
      Screen.Cursor := crHourGlass;

      // create an array of reals used for temporary storage of data read from the
      // file.

  //    RealArray
      RealArrayLength := StrToInt(frmExtract.adeCellCount.Text) * 2;
      SetLength(RealArray, RealArrayLength);

      // This opens the formatted head or drawdown file.
      AssignFile(Moc3dOutput, OpenDialog1.FileName);

      // start reading the MODFLOW file at the beginning
      Reset(Moc3dOutput);

      try
        begin
          // read title lines
          readln(Moc3dOutput, ALine);
          readln(Moc3dOutput, ALine);
          readln(Moc3dOutput, ALine);

          while not Eof(Moc3dOutput) do
          begin
            // if the user has pressed the cancel button, stop reading the file.
            Application.ProcessMessages;
            if CancelProcess then
            begin
              break;
            end;
            // get the elapsed time
            read(Moc3dOutput, TOTIM);
            ATime := TOTIM;

            // Set the time to the total elapased time.
            for RowIndex := 1 to dgDataPoints.RowCount - 1 do
            begin
              // if the user pressed the cancel button, quit.
              Application.ProcessMessages;
              if CancelProcess then
              begin
                break;
              end;
              // don't try to read past the end of the file.
              if not Eof(Moc3dOutput) then
              begin
                // allow the application to respond to the user pressing the cancel button.
                Application.ProcessMessages;
                // if the user pressed the cancel button, quit.
                if CancelProcess then
                begin
                  break;
                end;
                // read a number from the MODFLOW file
                Read(Moc3dOutput, ADatum);
                // Check if this is one of the data point we need to check.
                // (This could be made faster by moving the String to integer
                // conversion outside the loops.)

                SeriesIndex := (RowIndex - 1) * 2;

                RealArray[SeriesIndex] := ADatum;
                // Add the data point to the appropriate TLineSeries
                ASeries := MySeriesList[SeriesIndex];
                ASeries.AddXY(ATime, ADatum, '', clTeeColor);

                // read a number from the MODFLOW file
                Read(Moc3dOutput, ADatum);
                // Check if this is one of the data point we need to check.
                // (This could be made faster by moving the String to integer
                // conversion outside the loops.)

                Inc(SeriesIndex);

                RealArray[SeriesIndex] := ADatum;
                // Add the data point to the appropriate TLineSeries
                ASeries := MySeriesList[SeriesIndex];
                ASeries.AddXY(ATime, ADatum, '', clTeeColor);
              end
              else
              begin
                // quit if you've reached the end of the file
                break;
              end;
            end;
            // read the rest of the last line after the last number in the array.
            Readln(Moc3dOutput, AString);

            // If data for a time step has already been read,
            // add that data to RichEdit1.
            DataString := FloatToStr(ATime);
            for PointsIndex := 0 to RealArrayLength - 1 do
            begin
              DataString := DataString + Chr(9) +
                FloatToStr(RealArray[PointsIndex]);
            end;
            Lines.Add(DataString);

          end;
        end;

      finally
        begin
          // close the MODFLOW file
          CloseFile(Moc3dOutput);

          for ChartIndex := 0 to ChartHydExtractor.SeriesList.Count - 1 do
          begin
            AChartSeries := ChartHydExtractor.Series[ChartIndex];
            CellIndex := (ChartIndex div 2) + 1;
            CorrectType := (Odd(ChartIndex) = Odd(rgMOC3D.ItemIndex));
            AChartSeries.Active := CorrectType
              and (dgDataPoints.Cells[3, CellIndex]
              = dgDataPoints.Columns[3].PickList.Strings[1]);
          end;

          // restore the cursor
          Screen.Cursor := crDefault;
          RichEdit1.Lines.AddStrings(Lines);
          FLinesToSave.Assign(Lines);

        end;
      end;
    end;
  finally
    // enable or disable controls as appropriate.
    btnCancel.Enabled := False;
    btnRead.Enabled := True;
    adeCellCount.Enabled := True;
    dgDataPoints.Visible := True;
    dgDataPoints.Enabled := True;
    Lines.Free;
  end;
end;

procedure TfrmExtract.rgMOC3DClick(Sender: TObject);
var
  ASeries: TChartSeries;
  ChartIndex, CellIndex: integer;
  CorrectType: boolean;
begin
  if TProgramChoice(rgProgramChoice.ItemIndex) = pcGwtObs then
  begin
    ChartHydExtractor.LeftAxis.Title.Caption :=
      UpperCase(rgMOC3D.Items[rgMOC3D.ItemIndex]);
    for ChartIndex := 0 to ChartHydExtractor.SeriesList.Count - 1 do
    begin
      ASeries := ChartHydExtractor.Series[ChartIndex];

      CellIndex := (ChartIndex div 2) + 1;
      CorrectType := (Odd(ChartIndex) = Odd(rgMOC3D.ItemIndex));

      ASeries.Active := CorrectType
        and (dgDataPoints.Cells[3, CellIndex]
        = dgDataPoints.Columns[3].PickList.Strings[1]);
    end;
  end

end;

procedure TfrmExtract.ReadSutraFile;
var
  SutraFile: TStringList; // the Sutra output file
  ADatum: double; // a number read from the formatted head or drawdown file
  AString: string;
  RowIndex: integer; // indexes to the colums and rows
  PointsIndex: integer;
    // an index to the point for which a hydrograph is to be extracted.
  DataString: string;
    // This is used to store the formatted data that will be placed on a line of the TMemo
  ATime: double; // elapsed time for the current time step
  CaptionString: string;
  ASeries: TLineSeries; // a hydrograph plotted on the chart
  RealArray: array of double; // an array of values read from
  RealArrayLength: integer;
  ChartIndex: integer;
  AChartSeries: TChartSeries;
  Lines: TStringlist;
  ALine: string;
  LineIndex: integer;
  SearchString: string;
  SearchStringLength: integer;
  CellIndex: integer;
  CorrectType: boolean;
  SeriesIndex, InnerSeriesIndex: integer;
  StartLine, StopLine, CaptionLine: integer;
  Captions: TStringList;
  ItemIndex: integer;
  Node, TimeStep: string;
  NodeIndex, TypeIndex: integer;
  TitleLine: string;
  ObservationStart: integer;
  MoreNodes: boolean;
  ThisCount: integer;
  FirstTime: boolean;
  DataIndex: integer;
  ColorIndex: integer;
  TimeConstant: double;
  EquivHeadUsed: boolean;
  PressureIndex, EquivHeadIndex: integer;
  Elevation: double;
begin
  EquivHeadUsed := false;
  EquivHeadIndex := -1;
  case comboUnits.ItemIndex of
    0: TimeConstant := 1;
    1: TimeConstant := 60;
    2: TimeConstant := 3600;
    3: TimeConstant := 3600 * 24;
    4: TimeConstant := 3600 * 24 * 7;
    5: TimeConstant := 3600 * 24 * 365.25 / 12;
    6: TimeConstant := 3600 * 24 * 365.25;
  else
    Assert(False);
  end;
  //  Put the name of the file on the caption of the form.
  Caption := 'GW_Chart: ' + OpenDialog1.FileName;
  // disable thing that shouldn't be changed while reading the file.
  btnSave.Enabled := True;
  btnCancel.Enabled := True;
  btnRead.Enabled := False;
  adeCellCount.Enabled := False;
  dgDataPoints.Enabled := False;
  SutraFile := TStringlist.Create;
  Lines := TStringlist.Create;
  // this may take a while so change the cursor.
  Screen.Cursor := crHourGlass;

  try
    begin
      // ClearSeriesList destroys all the TLineSeries on the chart and
      // clears SeriesList
      ClearSeriesList;

      // initialize CancelProcess. CancelProcess will be tested while the
      // file is being read to see if the user has pressed the cancel button.
      CancelProcess := False;

      // initialize RichEdit1
      RichEdit1.Clear;

      RowIndex := 0;
      SutraFile.LoadFromFile(OpenDialog1.FileName);
      try
        ObservationStart := GetNextLine(
          'O  B  S  E  R  V  A  T  I  O  N     N  O  D  E     D  A  T  A',
          0, SutraFile);

        if ObservationStart = -1 then
        begin
          MessageDlg(OpenDialog1.FileName + ' contains no observation nodes.',
            mtWarning, [mbOK], 0);
        end
        else
        begin
          if not CancelProcess then
          begin
            Captions := TStringList.Create;
            try
              LineIndex := GetNextLine('NODE', ObservationStart, SutraFile);

              MoreNodes := LineIndex > -1;
              while MoreNodes do
              begin
                ALine := SutraFile[LineIndex];

                while (Length(ALine) > 0) do
                begin
                  Inc(RowIndex);
                  adeCellCount.Text := IntToStr(RowIndex);
                  adeCellCountChange(adeCellCount);
                  Node := GetStringBetween(ALine, 'NODE', 'NODE');
                  dgDataNodes.Cells[0, RowIndex] := Node;

                  if (dgDataNodes.Cells[1, RowIndex] = '') then
                  begin
                    if cbPlot.Checked then
                    begin
                      dgDataNodes.Cells[1, RowIndex]
                        := dgDataNodes.Columns[1].PickList[1];
                    end
                    else
                    begin
                      dgDataNodes.Cells[1, RowIndex]
                        := dgDataNodes.Columns[1].PickList[0];
                    end;
                  end;
                  {                if dgDataNodes.Cells[1,RowIndex] = '' then
                                  begin
                                    dgDataNodes.Cells[1,RowIndex] := dgDataNodes.Columns[1].PickList[0];
                                  end;   }
                    //              TitleLine := TitleLine + Chr(9) + 'Node ' + Node;
                end;

                CaptionLine := GetNextLine('TIME STEP', LineIndex, SutraFile);

                ALine := SutraFile[CaptionLine];
                GetStringBetween(ALine, 'TIME(SEC)', '');
                while Pos(' ', ALine) <> 0 do
                begin
                  AString := GetStringBetween(ALine, '', ' ');
                  if Captions.IndexOf(AString) = -1 then
                  begin
                    Captions.Add(AString);
                  end;
                end;
                if (Captions.IndexOf(ALine) = -1) and (ALine <> '') then
                begin
                  Captions.Add(ALine);
                end;
                //            ItemIndex := rgSutra.ItemIndex;
                //            rgSutra.Items := Captions;
                //            rgSutra.ItemIndex := ItemIndex;

                LineIndex := GetNextLine('NODE', LineIndex + 1, SutraFile);
                MoreNodes := LineIndex > -1;
              end;

              PressureIndex := Captions.IndexOf('Pressure');
              if PressureIndex > -1 then
              begin
                EquivHeadIndex := Captions.Add('Equivalent Head');
                EquivHeadUsed := True;
              end;

              ItemIndex := rgSutra.ItemIndex;
              rgSutra.Items := Captions;
              rgSutra.ItemIndex := ItemIndex;
            finally
              Captions.Free;
            end;

            {        StartLine := GetNextLine('TIME STEP',LineIndex, SutraFile)+1;
                    StopLine := StartLine;
                    for LineIndex := StartLine to SutraFile.Count -1 do
                    begin
                      StopLine := LineIndex;
                      if Trim(SutraFile[LineIndex]) = '' then
                      begin
                        StopLine := LineIndex -1;
                        break;
                      end;
                    end;
            }

            RealArrayLength := StrToInt(frmExtract.adeCellCount.Text) *
              rgSutra.Items.Count;
            SetLength(RealArray, RealArrayLength);

            TitleLine := ' TIME STEP' + Chr(9) + 'TIME(' + comboUnits.Text +
              ')';
            ColorIndex := -1;
            for RowIndex := 1 to dgDataNodes.RowCount - 1 do
            begin
              for TypeIndex := 0 to rgSutra.Items.Count - 1 do
              begin
                Inc(ColorIndex);
                if ColorIndex >= Length(ColorPalette) then
                begin
                  ColorIndex := 0;
                end;
                CaptionString := dgDataNodes.Cells[0, RowIndex];
                TitleLine := TitleLine + Chr(9) + rgSutra.Items[TypeIndex]
                  + ' Node ' + CaptionString;

                ASeries := TLineSeries.Create(ChartHydExtractor);
                ASeries.XValues.Order := loNone;
                ASeries.SeriesColor := ColorPalette[ColorIndex];
                ASeries.ParentChart := ChartHydExtractor;
                MySeriesList.Add(ASeries);
                ASeries.Title := CaptionString;
                ASeries.Pointer.Visible := True;
                ASeries.Active := cbPlot.Checked and (dgDataNodes.Cells[1,
                  RowIndex]
                  = dgDataNodes.Columns[1].PickList.Strings[1])
                  and (rgSutra.ItemIndex = TypeIndex);

              end;
            end;

            Lines.Add(TitleLine);

            //        RowIndex := 0;
            LineIndex := GetNextLine('NODE', ObservationStart, SutraFile);
            SeriesIndex := 0;

            FirstTime := True;
            MoreNodes := LineIndex > -1;
            while MoreNodes do
            begin
              ALine := SutraFile[LineIndex];
              ThisCount := 0;
              while (Length(ALine) > 0) do
              begin
                Inc(ThisCount);
                Node := GetStringBetween(ALine, 'NODE', 'NODE');
              end;

              StartLine := GetNextLine('TIME STEP', LineIndex, SutraFile) + 1;
              StopLine := StartLine;
              for LineIndex := StartLine to SutraFile.Count - 1 do
              begin
                StopLine := LineIndex;
                ALine := Trim(SutraFile[LineIndex]);
                if (ALine = '') or (ALine = '1') then
                begin
                  StopLine := LineIndex - 1;
                  break;
                end;
              end;

              DataIndex := 0;
              for LineIndex := StartLine to StopLine do
              begin
                Inc(DataIndex);
                Application.ProcessMessages;
                if CancelProcess then
                begin
                  break;
                end;
                ALine := Trim(SutraFile[LineIndex]);
                TimeStep := GetStringBetween(ALine, '', ' ');
                AString := GetStringBetween(ALine, '', ' ');
                if Pos('D', AString) > 0 then
                begin
                  AString[Pos('D', AString)] := 'E';
                end;
                if Pos('d', AString) > 0 then
                begin
                  AString[Pos('d', AString)] := 'e';
                end;
                InnerSeriesIndex := -1;
                ATime := InternationalStrToFloat(AString) / TimeConstant;

                RowIndex := 0;
                while Length(ALine) > 0 do
                begin
                  Inc(InnerSeriesIndex);
                  if EquivHeadUsed and (InnerSeriesIndex mod rgSutra.Items.Count
                    = EquivHeadIndex) then
                  begin
                    Continue;
                  end;

                  AString := GetStringBetween(ALine, '', ' ');
                  if Pos('D', AString) > 0 then
                  begin
                    AString[Pos('D', AString)] := 'E';
                  end;
                  if Pos('d', AString) > 0 then
                  begin
                    AString[Pos('d', AString)] := 'e';
                  end;
                  ADatum := InternationalStrToFloat(AString);
                  RealArray[SeriesIndex + InnerSeriesIndex] := ADatum;
                  // Add the data point to the appropriate TLineSeries
                  ASeries := MySeriesList[SeriesIndex + InnerSeriesIndex];
                  ASeries.AddXY(ATime, ADatum, '', clTeeColor);
                  if EquivHeadUsed and (InnerSeriesIndex mod rgSutra.Items.Count
                    = PressureIndex) then
                  begin
                    Inc(RowIndex);
                    Elevation := 0;
                    try
                      if dgDataNodes.Cells[2, RowIndex] <> '' then
                      begin
                        Elevation := StrToFloat(dgDataNodes.Cells[2, RowIndex])
                      end
                      else
                      begin
                        Elevation := 0;
                      end;
                    except on EConvertError do
                      begin
                        Elevation := 0;
                      end;
                    end;
                    ADatum := ADatum / Density / g + Elevation;
                    RealArray[SeriesIndex + InnerSeriesIndex + EquivHeadIndex -
                      PressureIndex] := ADatum;
                    ASeries := MySeriesList[SeriesIndex + InnerSeriesIndex +
                      EquivHeadIndex - PressureIndex];
                    ASeries.AddXY(ATime, ADatum, '', clTeeColor);
                  end;
                end;

                if FirstTime then
                begin
                  DataString := TimeStep + Chr(9) + FloatToStr(ATime);
                end
                else
                begin
                  DataString := Lines[DataIndex]
                end;
                for PointsIndex := SeriesIndex to SeriesIndex + ThisCount *
                  rgSutra.Items.Count - 1 do
                begin
                  DataString := DataString + Chr(9) +
                    FloatToStr(RealArray[PointsIndex]);
                end;
                if FirstTime then
                begin
                  Lines.Add(DataString);
                end
                else
                begin
                  Lines[DataIndex] := DataString;
                end;
              end;

              Inc(SeriesIndex, ThisCount * rgSutra.Items.Count);

              LineIndex := GetNextLine('NODE', LineIndex + 1, SutraFile);
              MoreNodes := LineIndex > -1;
              FirstTime := False;
            end;

          end;
        end;

      finally
        begin
          // close the MODFLOW file

          for ChartIndex := 0 to ChartHydExtractor.SeriesList.Count - 1 do
          begin
            AChartSeries := ChartHydExtractor.Series[ChartIndex];
            CellIndex := (ChartIndex div rgSutra.Items.Count) + 1;
            CorrectType := ((ChartIndex mod rgSutra.Items.Count) =
              rgSutra.ItemIndex);
            AChartSeries.Active := CorrectType
              and (dgDataNodes.Cells[1, CellIndex]
              = dgDataNodes.Columns[1].PickList.Strings[1]);
          end;

          RichEdit1.Lines.AddStrings(Lines);
          FLinesToSave.Assign(Lines);

        end;
      end;
    end;
  finally
    // enable or disable controls as appropriate.
    btnCancel.Enabled := False;
    btnRead.Enabled := True;
    adeCellCount.Enabled := True;
    dgDataNodes.Visible := True;
    Splitter2.Left := dgDataNodes.Left + dgDataNodes.Width;
    dgDataPoints.Enabled := True;
    Lines.Free;
    SutraFile.Free;
    // restore the cursor
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmExtract.ReadNewSutraFile;
var
  SutraFile: TStringList; // the Sutra output file
  ADatum: double; // a number read from the formatted head or drawdown file
  AString: string;
  RowIndex: integer; // indexes to the colums and rows
  PointsIndex: integer;
    // an index to the point for which a hydrograph is to be extracted.
  DataString: string;
    // This is used to store the formatted data that will be placed on a line of the TMemo
  ATime: double; // elapsed time for the current time step
  CaptionString: string;
  ASeries: TLineSeries; // a hydrograph plotted on the chart
  RealArray: array of double; // an array of values read from
  RealArrayLength: integer;
  ChartIndex: integer;
  AChartSeries: TChartSeries;
  Lines: TStringlist;
  ALine: string;
  LineIndex: integer;
  CellIndex: integer;
  CorrectType: boolean;
  InnerSeriesIndex: integer;
  CaptionLine: integer;
  Captions: TStringList;
  ItemIndex: integer;
  Node, TimeStep: string;
  TypeIndex: integer;
  TitleLine: string;
  ObservationStart: integer;
  //  MoreNodes : boolean;
  //  DataIndex : integer;
  ColorIndex: integer;
  TimeConstant: double;
  EquivHeadUsed: boolean;
  PressureIndex, EquivHeadIndex: integer;
  ElevationString: string;
  Elevation: double;
begin
  EquivHeadUsed := false;
  EquivHeadIndex := -1;
  TimeConstant := 1;
  case comboUnits.ItemIndex of
    0: TimeConstant := 1;
    1: TimeConstant := 60;
    2: TimeConstant := 3600;
    3: TimeConstant := 3600 * 24;
    4: TimeConstant := 3600 * 24 * 7;
    5: TimeConstant := 3600 * 24 * 365.25 / 12;
    6: TimeConstant := 3600 * 24 * 365.25;
  else
    Assert(False);
  end;

  //  Put the name of the file on the caption of the form.
  Caption := 'GW_Chart: ' + OpenDialog1.FileName;
  // disable thing that shouldn't be changed while reading the file.
  btnSave.Enabled := True;
  btnCancel.Enabled := True;
  btnRead.Enabled := False;
  adeCellCount.Enabled := False;
  dgDataPoints.Enabled := False;
  SutraFile := TStringlist.Create;
  Lines := TStringlist.Create;
  // this may take a while so change the cursor.
  Screen.Cursor := crHourGlass;

  try
    begin
      // ClearSeriesList destroys all the TLineSeries on the chart and
      // clears SeriesList
      ClearSeriesList;

      // initialize CancelProcess. CancelProcess will be tested while the
      // file is being read to see if the user has pressed the cancel button.
      CancelProcess := False;

      // initialize RichEdit1
      RichEdit1.Clear;

      RowIndex := 0;
      SutraFile.LoadFromFile(OpenDialog1.FileName);
      try
        ObservationStart := GetNextLine(
          '## Time Step',
          0, SutraFile);

        if ObservationStart = -1 then
        begin
          Beep;
          MessageDlg(OpenDialog1.FileName + ' contains no observation nodes.',
            mtWarning, [mbOK], 0);
        end
        else
        begin
          Inc(ObservationStart);
          if not CancelProcess then
          begin
            Captions := TStringList.Create;
            try
              LineIndex := ObservationStart - 5;

              ALine := SutraFile[LineIndex];

              while (Length(ALine) > 0) do
              begin
                Inc(RowIndex);
                adeCellCount.Text := IntToStr(RowIndex);
                adeCellCountChange(adeCellCount);
                Node := GetStringBetween(ALine, 'NODE', 'NODE');
                dgDataNodes.Cells[0, RowIndex] := Node;
                if (dgDataNodes.Cells[1, RowIndex] = '') then
                begin
                  if cbPlot.Checked then
                  begin
                    dgDataNodes.Cells[1, RowIndex]
                      := dgDataNodes.Columns[1].PickList[1];
                  end
                  else
                  begin
                    dgDataNodes.Cells[1, RowIndex]
                      := dgDataNodes.Columns[1].PickList[0];
                  end;
                end;
                {                if dgDataNodes.Cells[1,RowIndex] = '' then
                                begin
                                  dgDataNodes.Cells[1,RowIndex] := dgDataNodes.Columns[1].PickList[0];
                                end;  }
              end;
              LineIndex := LineIndex + 2;

              ALine := SutraFile[LineIndex];
              RowIndex := 0;
              while (Length(ALine) > 0) do
              begin
                Inc(RowIndex);
                //              adeCellCount.Text := IntToStr(RowIndex);
                //              adeCellCountChange(adeCellCount);
                ElevationString := GetStringBetween(ALine, '(', ')');
                ElevationString := GetStringBetween(ElevationString, ',', '');
                ElevationString := GetStringBetween(ElevationString, ',', '');
                ALine := Copy(Aline, 2, MAXINT);

                dgDataNodes.Cells[2, RowIndex] := ElevationString;
                {              if (dgDataNodes.Cells[1, RowIndex] = '') then
                              begin
                                if cbPlot.Checked then
                                begin
                                  dgDataNodes.Cells[1, RowIndex]
                                    := dgDataNodes.Columns[1].PickList[1];
                                end
                                else
                                begin
                                  dgDataNodes.Cells[1, RowIndex]
                                    := dgDataNodes.Columns[1].PickList[0];
                                end;
                              end; }
                {                if dgDataNodes.Cells[1,RowIndex] = '' then
                                begin
                                  dgDataNodes.Cells[1,RowIndex] := dgDataNodes.Columns[1].PickList[0];
                                end;  }
              end;

              CaptionLine := ObservationStart - 1;

              ALine := SutraFile[CaptionLine];
              GetStringBetween(ALine, 'Time (sec)', '');
              while Pos(' ', ALine) <> 0 do
              begin
                AString := GetStringBetween(ALine, '', ' ');
                if Captions.IndexOf(AString) = -1 then
                begin
                  Captions.Add(AString);
                end;
              end;
              if (Captions.IndexOf(ALine) = -1) and (ALine <> '') then
              begin
                Captions.Add(ALine);
              end;

              PressureIndex := Captions.IndexOf('Pressure');
              if PressureIndex > -1 then
              begin
                EquivHeadIndex := Captions.Add('Equivalent Head');
                EquivHeadUsed := True;
              end;

              ItemIndex := rgSutra.ItemIndex;
              rgSutra.Items := Captions;
              rgSutra.ItemIndex := ItemIndex;
            finally
              Captions.Free;
            end;

            RealArrayLength := StrToInt(adeCellCount.Text)
              * rgSutra.Items.Count;
            SetLength(RealArray, RealArrayLength);

            TitleLine := ' Time Step' + Chr(9) + 'Time (' + comboUnits.Text +
              ')';
            ColorIndex := -1;
            for RowIndex := 1 to dgDataNodes.RowCount - 1 do
            begin
              for TypeIndex := 0 to rgSutra.Items.Count - 1 do
              begin
                Inc(ColorIndex);
                if ColorIndex >= Length(ColorPalette) then
                begin
                  ColorIndex := 0;
                end;
                CaptionString := dgDataNodes.Cells[0, RowIndex];
                TitleLine := TitleLine + Chr(9) + rgSutra.Items[TypeIndex]
                  + ' Node ' + CaptionString;

                ASeries := TLineSeries.Create(ChartHydExtractor);
                ASeries.XValues.Order := loNone;

                ASeries.SeriesColor := ColorPalette[ColorIndex];
                ASeries.ParentChart := ChartHydExtractor;
                MySeriesList.Add(ASeries);
                ASeries.Title := CaptionString;
                ASeries.Pointer.Visible := True;
                ASeries.Active := cbPlot.Checked and (dgDataNodes.Cells[1,
                  RowIndex]
                  = dgDataNodes.Columns[1].PickList.Strings[1])
                  and (rgSutra.ItemIndex = TypeIndex);

              end;
            end;

            Lines.Add(TitleLine);

            LineIndex := ObservationStart;

            while LineIndex < SutraFile.Count do
            begin
              Application.ProcessMessages;
              if CancelProcess then
              begin
                break;
              end;
              ALine := Trim(SutraFile[LineIndex]);
              if Pos('OBSERVATION OUTPUT IS COMPLETE', ALine) > 0 then
              begin
                Break;
              end;

              TimeStep := GetStringBetween(ALine, '', ' ');
              AString := GetStringBetween(ALine, '', ' ');
              if AString = '' then
              begin
                Inc(LineIndex);
                Continue;
              end;

              if Pos('D', AString) > 0 then
              begin
                AString[Pos('D', AString)] := 'E';
              end;
              if Pos('d', AString) > 0 then
              begin
                AString[Pos('d', AString)] := 'e';
              end;
              InnerSeriesIndex := -1;
              ATime := InternationalStrToFloat(AString) / TimeConstant;
              RowIndex := 0;
              while Length(ALine) > 0 do
              begin
                Inc(InnerSeriesIndex);
                if EquivHeadUsed and (InnerSeriesIndex mod rgSutra.Items.Count =
                  EquivHeadIndex) then
                begin
                  Continue;
                end;

                AString := GetStringBetween(ALine, '', ' ');
                if Pos('D', AString) > 0 then
                begin
                  AString[Pos('D', AString)] := 'E';
                end;
                if Pos('d', AString) > 0 then
                begin
                  AString[Pos('d', AString)] := 'e';
                end;
                ADatum := InternationalStrToFloat(AString);
                RealArray[InnerSeriesIndex] := ADatum;
                // Add the data point to the appropriate TLineSeries
                ASeries := MySeriesList[InnerSeriesIndex];
                ASeries.AddXY(ATime, ADatum, '', clTeeColor);

                if EquivHeadUsed and (InnerSeriesIndex mod rgSutra.Items.Count =
                  PressureIndex) then
                begin
                  Inc(RowIndex);
                  Elevation := 0;
                  try
                    if dgDataNodes.Cells[2, RowIndex] <> '' then
                    begin
                      Elevation := StrToFloat(dgDataNodes.Cells[2, RowIndex])
                    end
                    else
                    begin
                      Elevation := 0;
                    end;
                  except on EConvertError do
                    begin
                      Elevation := 0;
                    end;
                  end;
                  ADatum := ADatum / Density / g + Elevation;
                  RealArray[InnerSeriesIndex + EquivHeadIndex - PressureIndex]
                    := ADatum;
                  ASeries := MySeriesList[InnerSeriesIndex + EquivHeadIndex -
                    PressureIndex];
                  ASeries.AddXY(ATime, ADatum, '', clTeeColor);
                end;

              end;

              DataString := TimeStep + Chr(9) + FloatToStr(ATime);

              for PointsIndex := 0 to Length(RealArray) - 1 do
              begin
                DataString := DataString + Chr(9) +
                  FloatToStr(RealArray[PointsIndex]);
              end;
              Lines.Add(DataString);
              //            Lines[DataIndex] := DataString;

              Inc(LineIndex);
              //            MoreNodes := LineIndex < SutraFile.Count ;
            end;

          end;
        end;

      finally
        begin
          // close the SUTRA file

          for ChartIndex := 0 to ChartHydExtractor.SeriesList.Count - 1 do
          begin
            AChartSeries := ChartHydExtractor.Series[ChartIndex];
            CellIndex := (ChartIndex div rgSutra.Items.Count) + 1;
            CorrectType := ((ChartIndex mod rgSutra.Items.Count) =
              rgSutra.ItemIndex);
            AChartSeries.Active := CorrectType
              and (dgDataNodes.Cells[1, CellIndex]
              = dgDataNodes.Columns[1].PickList.Strings[1]);
          end;

          RichEdit1.Lines.AddStrings(Lines);
          FLinesToSave.Assign(Lines);

        end;
      end;
    end;
  finally
    // enable or disable controls as appropriate.
    btnCancel.Enabled := False;
    btnRead.Enabled := True;
    adeCellCount.Enabled := True;
    dgDataNodes.Visible := True;
    Splitter2.Left := dgDataNodes.Left + dgDataNodes.Width;
    dgDataPoints.Enabled := True;
    Lines.Free;
    SutraFile.Free;
    // restore the cursor
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmExtract.GetSutraCoord(ALine: string; var Coord: TSutraCoordArray);
var
  Count: integer;
  CharIndex: Integer;
  StartIndex: integer;
  Splitter: TStringList;
begin
  Count := 0;
  for CharIndex := 1 to Length(ALine) do
  begin
    if ALine[CharIndex] = '(' then
    begin
      Inc(Count);
    end;
  end;
  SetLength(Coord, Count);

  Splitter := TStringList.Create;
  try
    Count := 0;
    for CharIndex := 1 to Length(ALine) do
    begin
      if ALine[CharIndex] = '(' then
      begin
        StartIndex := CharIndex + 1;
      end
      else if ALine[CharIndex] = ')' then
      begin
        Splitter.CommaText := Copy(ALine, StartIndex, CharIndex-StartIndex);
        if Splitter.Count = 2 then
        begin
          Coord[Count].CoordType := sct2D;
          Coord[Count].z := 0;
        end
        else
        begin
          Assert(Splitter.Count = 3);
          Coord[Count].CoordType := sct3D;
          Coord[Count].z := FortranStrToFloat(Splitter[2]);
        end;
        Coord[Count].x := FortranStrToFloat(Splitter[0]);
        Coord[Count].y := FortranStrToFloat(Splitter[1]);

        if Count > 0 then
        begin
          Assert(Coord[Count-1].CoordType = Coord[Count].CoordType);
        end;

        Inc(Count);
      end;
    end;
  finally
    Splitter.Free;
  end;          
end;

procedure TfrmExtract.ReadNewSutraFile2;
var
  SutraFile: TStringList; // the Sutra output file
  ADatum: double; // a number read from the formatted head or drawdown file
  AString: string;
  RowIndex: integer; // indexes to the colums and rows
  PointsIndex: integer;
    // an index to the point for which a hydrograph is to be extracted.
  DataString: string;
    // This is used to store the formatted data that will be placed on a line of the TMemo
  ATime: double; // elapsed time for the current time step
  CaptionString: string;
  ASeries: TLineSeries; // a hydrograph plotted on the chart
  RealArray: array of double; // an array of values read from
  RealArrayLength: integer;
  ChartIndex: integer;
  AChartSeries: TChartSeries;
  Lines: TStringlist;
  ALine: string;
  LineIndex: integer;
  CellIndex: integer;
  CorrectType: boolean;
  InnerSeriesIndex: integer;
  CaptionLine: integer;
  Captions: TStringList;
  ItemIndex: integer;
  Node, TimeStep: string;
  TypeIndex: integer;
  TitleLine: string;
  ObservationStart: integer;
  ColorIndex: integer;
  TimeConstant: double;
  EquivHeadUsed: boolean;
  PressureIndex, EquivHeadIndex: integer;
  ElevationString: string;
  Elevation: double;
  ObservationNames, Values: TStringList;
  ValueIndex: integer;
  LabelStart: integer;
  LabelEnd: integer;
  LinesPerTimeStep: integer;
  InnerIndex: integer;
  CoordinatesStart: integer;
  CoordinatesEnd: integer;
  Coordinates: TSutraCoordArray;
  XLabelLine: string;
  YLabelLine: string;
  ZLabelLine: string;
begin
  // read SUTRA 2.1 observation file.
  EquivHeadUsed := false;
  EquivHeadIndex := -1;
  case comboUnits.ItemIndex of
    0: TimeConstant := 1;
    1: TimeConstant := 60;
    2: TimeConstant := 3600;
    3: TimeConstant := 3600 * 24;
    4: TimeConstant := 3600 * 24 * 7;
    5: TimeConstant := 3600 * 24 * 365.25 / 12;
    6: TimeConstant := 3600 * 24 * 365.25;
  else
    Assert(False);
  end;

  //  Put the name of the file on the caption of the form.
  Caption := 'GW_Chart: ' + OpenDialog1.FileName;
  // disable thing that shouldn't be changed while reading the file.
  btnSave.Enabled := True;
  btnCancel.Enabled := True;
  btnRead.Enabled := False;
  adeCellCount.Enabled := False;
  dgDataPoints.Enabled := False;
  SutraFile := TStringlist.Create;
  Lines := TStringlist.Create;
  ObservationNames := TStringlist.Create;
  Values := TStringlist.Create;
  // this may take a while so change the cursor.
  Screen.Cursor := crHourGlass;

  try
    begin
      Values.Delimiter := ' ';
      // ClearSeriesList destroys all the TLineSeries on the chart and
      // clears SeriesList
      ClearSeriesList;

      // initialize CancelProcess. CancelProcess will be tested while the
      // file is being read to see if the user has pressed the cancel button.
      CancelProcess := False;

      // initialize RichEdit1
      RichEdit1.Clear;
      RichEdit1.Lines.Clear;
      Application.ProcessMessages;

      SutraFile.LoadFromFile(OpenDialog1.FileName);
      try
        LabelStart := GetNextLine(
          '## =================',
          0, SutraFile);
        LabelStart := GetNextLine(
          '## =================',
          LabelStart+1, SutraFile);
        LabelStart := GetNextLine(
          '## =================',
          LabelStart+1, SutraFile)+2;
        LabelEnd := GetNextLine(
          '##                                   ----------',
          LabelStart+1, SutraFile)-1;

        LinesPerTimeStep := LabelEnd - LabelStart + 1;

        CoordinatesStart := LabelEnd + 2;
        CoordinatesEnd := GetNextLine(
          '##                                   ----------',
          CoordinatesStart+1, SutraFile)-1;
        Assert(LinesPerTimeStep = CoordinatesEnd - CoordinatesStart + 1);

        ObservationStart := GetNextLine(
          '##       Time Step',
          LabelEnd+1, SutraFile);

        if ObservationStart = -1 then
        begin
          Beep;
          MessageDlg(OpenDialog1.FileName + ' contains no observation nodes.',
            mtWarning, [mbOK], 0);
        end
        else
        begin
          Inc(ObservationStart);
          if not CancelProcess then
          begin
            Captions := TStringList.Create;
            try
              ALine := '';
              for LineIndex := LabelStart to LabelEnd do
              begin
                ALine := ALine + Copy(SutraFile[LineIndex], 3, MAXINT)
              end;
              LineIndex := LabelEnd+1;

//              LineIndex := ObservationStart - 5;

//              ALine := SutraFile[LineIndex];
//              Assert(Copy(ALine,1,2) = '##');
              ObservationNames.Delimiter := ' ';
//              ObservationNames.DelimitedText := Trim(Copy(ALine, 3, MAXINT));
              ObservationNames.DelimitedText := Trim(ALine);

              // Get coordinates
              ALine := '';
              for LineIndex := CoordinatesStart to CoordinatesEnd do
              begin
                ALine := ALine + Copy(SutraFile[LineIndex], 3, MAXINT)
              end;
              GetSutraCoord(ALine, Coordinates);
              LineIndex := LabelEnd+1;

              adeCellCount.Text := IntToStr(ObservationNames.Count);
              adeCellCountChange(adeCellCount);
              for RowIndex := 0 to ObservationNames.Count -1 do
              begin
                dgDataNodes.Cells[0, RowIndex+1] := ObservationNames[RowIndex];
                if (dgDataNodes.Cells[1, RowIndex+1] = '') then
                begin
                  if cbPlot.Checked then
                  begin
                    dgDataNodes.Cells[1, RowIndex+1]
                      := dgDataNodes.Columns[1].PickList[1];
                  end
                  else
                  begin
                    dgDataNodes.Cells[1, RowIndex+1]
                      := dgDataNodes.Columns[1].PickList[0];
                  end;
                end;
              end;

              LineIndex := LineIndex + 1;
              ALine := '';
              for InnerIndex := 0 to LinesPerTimeStep -1 do
              begin
                ALine := ALine + SutraFile[LineIndex];
                Inc(LineIndex);
              end;


//              ALine := SutraFile[LineIndex];
              RowIndex := 0;
              while (Length(ALine) > 0) do
              begin
                Inc(RowIndex);
                ElevationString := GetStringBetween(ALine, '(', ')');
                ElevationString := GetStringBetween(ElevationString, ',', '');
                ElevationString := GetStringBetween(ElevationString, ',', '');
                ALine := Copy(Aline, 2, MAXINT);

                dgDataNodes.Cells[2, RowIndex] := ElevationString;
              end;

              CaptionLine := ObservationStart - 1;

              ALine := SutraFile[CaptionLine];
              GetStringBetween(ALine, 'Time (sec)', '');
              while Pos(' ', ALine) <> 0 do
              begin
                AString := GetStringBetween(ALine, '', ' ');
                if Captions.IndexOf(AString) = -1 then
                begin
                  Captions.Add(AString);
                end;
              end;
              if (Captions.IndexOf(ALine) = -1) and (ALine <> '') then
              begin
                Captions.Add(ALine);
              end;

              PressureIndex := Captions.IndexOf('Pressure');
              if PressureIndex > -1 then
              begin
                EquivHeadIndex := Captions.Add('Equivalent Head');
                EquivHeadUsed := True;
              end;

              ItemIndex := rgSutra.ItemIndex;
              rgSutra.Items := Captions;
              rgSutra.ItemIndex := ItemIndex;
            finally
              Captions.Free;
            end;

            RealArrayLength := StrToInt(adeCellCount.Text)
              * rgSutra.Items.Count;
            SetLength(RealArray, RealArrayLength);

            TitleLine := ' Time Step' + Chr(9) + 'Time (' + comboUnits.Text +
              ')';
            XLabelLine := ' X ' + Chr(9) + ' ';
            YLabelLine := ' Y ' + Chr(9) + ' ';
            if Coordinates[0].CoordType = sct2D then
            begin
              ZLabelLine := '';
            end
            else
            begin
              ZLabelLine := ' Z ' + Chr(9) + ' ';
            end;

            ColorIndex := -1;
            for RowIndex := 1 to dgDataNodes.RowCount - 1 do
            begin
              for TypeIndex := 0 to rgSutra.Items.Count - 1 do
              begin
                Inc(ColorIndex);
                if ColorIndex >= Length(ColorPalette) then
                begin
                  ColorIndex := 0;
                end;
                CaptionString := dgDataNodes.Cells[0, RowIndex];
                TitleLine := TitleLine + Chr(9) + rgSutra.Items[TypeIndex]
                  + ' Node ' + CaptionString;
                XLabelLine := XLabelLine + Chr(9) + FloatToStr(Coordinates[RowIndex-1].x);
                YLabelLine := YLabelLine + Chr(9) + FloatToStr(Coordinates[RowIndex-1].y);
                if Coordinates[RowIndex-1].CoordType = sct3D then
                begin
                  ZLabelLine := ZLabelLine + Chr(9) + FloatToStr(Coordinates[RowIndex-1].z);
                end;

                ASeries := TLineSeries.Create(ChartHydExtractor);
                ASeries.XValues.Order := loNone;

                ASeries.SeriesColor := ColorPalette[ColorIndex];
                ASeries.ParentChart := ChartHydExtractor;
                MySeriesList.Add(ASeries);
                ASeries.Title := CaptionString;
                ASeries.Pointer.Visible := True;
                ASeries.Active := cbPlot.Checked and (dgDataNodes.Cells[1,
                  RowIndex]
                  = dgDataNodes.Columns[1].PickList.Strings[1])
                  and (rgSutra.ItemIndex = TypeIndex);

              end;
            end;

            Lines.Add(TitleLine);
            Lines.Add(XLabelLine);
            Lines.Add(YLabelLine);
            if ZLabelLine <> '' then
            begin
              Lines.Add(ZLabelLine);
            end;              

            LineIndex := ObservationStart;

            while LineIndex < SutraFile.Count do
            begin
              Application.ProcessMessages;
              if CancelProcess then
              begin
                break;
              end;
              ALine := '';
              for InnerIndex := 0 to LinesPerTimeStep-1 do
              begin
                ALine := ALine + SutraFile[LineIndex];
                Inc(LineIndex)
              end;

//              ALine := (SutraFile[LineIndex]);
//              Values.DelimitedText := Trim(Copy(ALine, 3, MAXINT));
              Values.DelimitedText := Trim(ALine);
              TimeStep := Values[0];
              AString := Values[1];
              if Pos('D', AString) > 0 then
              begin
                AString[Pos('D', AString)] := 'E';
              end;
              if Pos('d', AString) > 0 then
              begin
                AString[Pos('d', AString)] := 'e';
              end;
              ATime := InternationalStrToFloat(AString) / TimeConstant;
              RowIndex := 0;
              ValueIndex := 1;
              for InnerSeriesIndex := 0 to MySeriesList.Count - 1 do
              begin
                if EquivHeadUsed and (InnerSeriesIndex mod rgSutra.Items.Count =
                  EquivHeadIndex) then
                begin
                  Continue;
                end;
                Inc(ValueIndex);

                AString := Values[ValueIndex];
                if Pos('D', AString) > 0 then
                begin
                  AString[Pos('D', AString)] := 'E';
                end;
                if Pos('d', AString) > 0 then
                begin
                  AString[Pos('d', AString)] := 'e';
                end;
                ADatum := InternationalStrToFloat(AString);
                RealArray[InnerSeriesIndex] := ADatum;
                // Add the data point to the appropriate TLineSeries
                ASeries := MySeriesList[InnerSeriesIndex];
                ASeries.AddXY(ATime, ADatum, '', clTeeColor);

                if EquivHeadUsed and (InnerSeriesIndex mod rgSutra.Items.Count =
                  PressureIndex) then
                begin
                  Inc(RowIndex);
                  Elevation := 0;
                  try
                    if dgDataNodes.Cells[2, RowIndex] <> '' then
                    begin
                      Elevation := StrToFloat(dgDataNodes.Cells[2, RowIndex])
                    end
                    else
                    begin
                      Elevation := 0;
                    end;
                  except on EConvertError do
                    begin
                      Elevation := 0;
                    end;
                  end;
                  ADatum := ADatum / Density / g + Elevation;
                  RealArray[InnerSeriesIndex + EquivHeadIndex - PressureIndex]
                    := ADatum;
                  ASeries := MySeriesList[InnerSeriesIndex + EquivHeadIndex -
                    PressureIndex];
                  ASeries.AddXY(ATime, ADatum, '', clTeeColor);
                end;

              end;

              DataString := TimeStep + Chr(9) + FloatToStr(ATime);

              for PointsIndex := 0 to Length(RealArray) - 1 do
              begin
                DataString := DataString + Chr(9) +
                  FloatToStr(RealArray[PointsIndex]);
              end;
              Lines.Add(DataString);

//              Inc(LineIndex);
            end;

          end;
        end;

      finally
        begin
          // close the SUTRA file

          for ChartIndex := 0 to ChartHydExtractor.SeriesList.Count - 1 do
          begin
            AChartSeries := ChartHydExtractor.Series[ChartIndex];
            CellIndex := (ChartIndex div rgSutra.Items.Count) + 1;
            CorrectType := ((ChartIndex mod rgSutra.Items.Count) =
              rgSutra.ItemIndex);
            AChartSeries.Active := CorrectType
              and (dgDataNodes.Cells[1, CellIndex]
              = dgDataNodes.Columns[1].PickList.Strings[1]);
          end;

          RichEdit1.Lines.AddStrings(Lines);
          FLinesToSave.Assign(Lines);
        end;
      end;
    end;
  finally
    // enable or disable controls as appropriate.
    btnCancel.Enabled := False;
    btnRead.Enabled := True;
    adeCellCount.Enabled := True;
    dgDataNodes.Visible := True;
    Splitter2.Left := dgDataNodes.Left + dgDataNodes.Width;
    dgDataPoints.Enabled := True;
    Lines.Free;
    SutraFile.Free;
    ObservationNames.Free;
    Values.Free;
    // restore the cursor
    Screen.Cursor := crDefault;
  end;
end;

function TfrmExtract.GetStringBetween(var AString: string; Before, After:
  string): string;
var
  Position: integer;
begin
  result := AString;
  if Before <> '' then
  begin
    result := Copy(AString,
      Pos(Before, AString) + Length(Before),
      MAXINT);
    AString := result;
  end;

  if After <> '' then
  begin
    Position := Pos(After, result) - 1;
    if Position = -1 then
    begin
      Position := Length(result);
    end;
    result := Copy(result, 1, Position);
    AString := Copy(AString, Length(result) + 1, MAXINT);
  end;

  result := Trim(result);
  AString := Trim(AString);
end;

function TfrmExtract.GetNextLine(SearchTerm: string; Start: integer;
  AStringList: TStringList): integer;
var
  Index: integer;
begin
  result := -1;
  for Index := Start to AStringList.Count - 1 do
  begin
    Application.ProcessMessages;
    if CancelProcess or (Pos(SearchTerm, AStringList.Strings[Index]) > 0) then
    begin
      result := Index;
      break;
    end;
  end;
end;

procedure TfrmExtract.dgDataNodesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  ASeries: TChartSeries;
  ChartIndex: integer;
  procedure PlotChart;
  begin
    if ChartHydExtractor.SeriesList.Count > ChartIndex then
    begin
      ASeries := ChartHydExtractor.Series[ChartIndex];
      ASeries.Active := dgDataNodes.Cells[1, ARow]
        = dgDataNodes.Columns[1].PickList.Strings[1];
    end;
  end;
begin
  if TProgramChoice(rgProgramChoice.ItemIndex) in [pcHydMode, pcMf6Obs] then
  begin
    ChartIndex := (ARow - 1);
    PlotChart;
  end
  else
  begin
    if (ACol = 1) and (ARow > 0) then
    begin
      ChartIndex := (ARow - 1) * rgSutra.Items.Count + rgSutra.ItemIndex;
      PlotChart;
    end;
  end;            

end;

procedure TfrmExtract.rgSutraClick(Sender: TObject);
var
  ASeries: TChartSeries;
  ChartIndex, CellIndex: integer;
  CorrectType: boolean;
begin
  if (TProgramChoice(rgProgramChoice.ItemIndex)
    in [pcSutra1, pcsutra2, pcSutra2_1]) then
  begin
    ChartHydExtractor.LeftAxis.Title.Caption :=
      UpperCase(rgSutra.Items[rgSutra.ItemIndex]);
    for ChartIndex := 0 to ChartHydExtractor.SeriesList.Count - 1 do
    begin
      ASeries := ChartHydExtractor.Series[ChartIndex];

      CellIndex := (ChartIndex div rgSutra.Items.Count) + 1;
      CorrectType := ((ChartIndex mod rgSutra.Items.Count) = rgSutra.ItemIndex);

      ASeries.Active := CorrectType
        and (dgDataNodes.Cells[1, CellIndex]
        = dgDataNodes.Columns[1].PickList.Strings[1]);

    end;
  end

end;

procedure TfrmExtract.sbFormatClick(Sender: TObject);
begin
  mHHelp.ChmFile := ChartHelpFileName;
  try
    ChartEditor1.Execute;
  finally
    mHHelp.ChmFile := HelpFileName;
  end;
  {  frmFormat.GetData(ChartHydExtractor, 20);
    frmFormat.ShowModal; }
end;

procedure TfrmExtract.ToolBar1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TfrmExtract.sbImageClick(Sender: TObject);
var
  OldFilter: string;
  OldDefaultExtension: string;
  OldFilterIndex: integer;
  Extension: string;
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
      Extension := LowerCase(ExtractFileExt(SaveDialog1.FileName));
      if Extension = '.bmp' then
      begin
        ChartHydExtractor.SaveToBitmapFile(SaveDialog1.FileName);
      end
      else if Extension = '.emf' then
      begin
        ChartHydExtractor.SaveToMetafileEnh(SaveDialog1.FileName);
      end
      else if Extension = '.wmf' then
      begin
        ChartHydExtractor.SaveToMetafile(SaveDialog1.FileName);
      end
      else
      begin
        MessageDlg('The file must have one of the following extensions: '
          + '.bmp, .emf, .wmf', mtError, [mbOK], 0);
      end;
    end;
  finally
    SaveDialog1.Filter := OldFilter;
    SaveDialog1.DefaultExt := OldDefaultExtension;
    SaveDialog1.FilterIndex := OldFilterIndex;
  end;

end;

procedure TfrmExtract.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmExtract.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Visible then
  begin
    frmModChart.Close
  end;
end;

procedure TfrmExtract.FormShow(Sender: TObject);
begin
  MainMenu1.Merge(frmModChart.mainMenuFormChoice);
end;

procedure TfrmExtract.ChartHydExtractorAfterDraw(Sender: TObject);
var
  TempFont: TFont;
begin
  if ExplanationVisible and ChartHydExtractor.Legend.Visible then
  begin
    TempFont := TFont.Create;
    try
      TempFont.Assign(ChartHydExtractor.Canvas.Font);
      try
        ChartHydExtractor.Canvas.Font.Assign(ChartHydExtractor.Legend.Font);
        ChartHydExtractor.Canvas.TextOut(ExplanationLeft, ExplanationTop,
          edExplanation.Text);
      finally
        ChartHydExtractor.Canvas.Font.Assign(TempFont);
      end;
    finally
      TempFont.Free;
    end;
  end;
end;

procedure TfrmExtract.ChartHydExtractorGetLegendPos(Sender: TCustomChart;
  Index: Integer; var X, Y, XColor: Integer);
var
  ExplanationHeight: integer;
  TempFont: TFont;
begin
  TempFont := TFont.Create;
  try
    TempFont.Assign(ChartHydExtractor.Canvas.Font);
    try
      ChartHydExtractor.Canvas.Font.Assign(ChartHydExtractor.Legend.Font);
      ExplanationHeight :=
        ChartHydExtractor.Canvas.TextHeight(edExplanation.Text);
    finally
      ChartHydExtractor.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
  Y := Y + ExplanationHeight;
end;

procedure TfrmExtract.ChartHydExtractorGetLegendRect(Sender: TCustomChart;
  var Rect: TRect);
var
  ExplanationWidth: integer;
  ExplanationHeight: integer;
  TempFont: TFont;
begin
  //  LegendRect := Rect;
  ExplanationTop := Rect.Top + 5;
  ExplanationLeft := Rect.Left + 5;
  TempFont := TFont.Create;
  try
    TempFont.Assign(ChartHydExtractor.Canvas.Font);
    try
      ChartHydExtractor.Canvas.Font.Assign(ChartHydExtractor.Legend.Font);
      ExplanationWidth := ChartHydExtractor.Canvas.TextWidth('_' +
        edExplanation.Text + '_');
      ExplanationHeight :=
        ChartHydExtractor.Canvas.TextHeight(edExplanation.Text);
    finally
      ChartHydExtractor.Canvas.Font.Assign(TempFont);
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

{Initialization
GetDecimalChar;}

procedure TfrmExtract.comboUnitsChange(Sender: TObject);
begin
  inherited;
  ChartHydExtractor.BottomAxis.Title.Caption := 'time ('
    + comboUnits.Text + ')';
  Beep;
  ShowMessage('Read data from the file again to change the units in which the '
    + 'data is displayed');

end;

procedure TfrmExtract.edExplanationChange(Sender: TObject);
begin
  inherited;
  ChartHydExtractor.Invalidate;
end;

function TfrmExtract.Density: double;
begin
  result := StrToFloat(adeDensity.Text);
end;

function TfrmExtract.G: double;
begin
  result := StrToFloat(adeG.Text);
end;

procedure TfrmExtract.dgDataNodesExit(Sender: TObject);
begin
  inherited;
  if TProgramChoice(rgProgramChoice.ItemIndex) in
    [pcSutra1, pcsutra2, pcSutra2_1] then
  begin
    ReadFile;
  end;
  ChartHydExtractor.Invalidate;
end;

procedure TfrmExtract.SetAllPlots(Value: Boolean);
var
  ItemIndex: integer;
  RowIndex: integer;
  SeriesIndex: integer;
begin
  ItemIndex := Ord(Value);
  if dgDataNodes.Visible then
  begin
    for RowIndex := 1 to dgDataNodes.RowCount -1 do
    begin
      dgDataNodes.Cells[1, RowIndex]
        := dgDataNodes.Columns[1].PickList.Strings[ItemIndex];
    end;
  end;
  if dgDataPoints.Visible then
  begin
    for RowIndex := 1 to dgDataPoints.RowCount -1 do
    begin
      dgDataPoints.Cells[3, RowIndex]
        := dgDataPoints.Columns[3].PickList.Strings[ItemIndex];
    end;
  end;
  for SeriesIndex := 0 to ChartHydExtractor.SeriesCount -1 do
  begin
    ChartHydExtractor.Series[SeriesIndex].Active := Value
  end;


end;

procedure TfrmExtract.btnPlotAllClick(Sender: TObject);
begin
  inherited;
  SetAllPlots(True);
end;

procedure TfrmExtract.btnPlotNoneClick(Sender: TObject);
begin
  inherited;
  SetAllPlots(False);
end;

procedure TfrmExtract.ReadHydmodFile;
var
  HydModData: THydModData;
  LabelIndex: Integer;
  ALabel: string;
  ASeries: TLineSeries;
  ColorIndex: Integer;
  TimeIndex: Integer;
  Lines: TStringList;
begin
  HydModData := THydModData.Create;
  Lines := TStringList.Create;
  try
    RichEdit1.Clear;
    Lines.Add('Observation Name'#9'Time'#9'Value');
    // ClearSeriesList destroys all the TLineSeries on the chart and
    // clears SeriesList
    ClearSeriesList;
    HydModData.ReadFile(OpenDialog1.FileName);
    dgDataNodes.RowCount := HydModData.LabelCount+1;
    ColorIndex := -1;
    for LabelIndex := 0 to HydModData.LabelCount-1 do
    begin
      Inc(ColorIndex);
      if ColorIndex >= Length(ColorPalette) then
      begin
        ColorIndex := 0;
      end;
      ALabel := HydModData.Labels[LabelIndex];
      dgDataNodes.Cells[0, LabelIndex+1] := ALabel;
      dgDataNodes.Cells[1, LabelIndex+1] :=
        dgDataNodes.Columns[1].PickList.Strings[0];
      dgDataNodes.Cells[2, LabelIndex+1] := '';

      ASeries := TLineSeries.Create(ChartHydExtractor);
      ASeries.XValues.Order := loNone;

      ASeries.SeriesColor := ColorPalette[ColorIndex];
      ASeries.ParentChart := ChartHydExtractor;
      MySeriesList.Add(ASeries);
      ASeries.Title := ALabel;
      ASeries.Pointer.Visible := True;
      ASeries.Active := False;
      for TimeIndex := 0 to HydModData.TimeCount-1 do
      begin
        ASeries.AddXY(HydModData.Times[TimeIndex],
        HydModData.Values[LabelIndex, TimeIndex], '', clTeeColor);
        Lines.Add(ALabel + #9 + FloatToStr(HydModData.Times[TimeIndex])
          + #9 + FloatToStr(HydModData.Values[LabelIndex, TimeIndex]));
      end;

    end;

    RichEdit1.Lines := Lines;
  finally
    HydModData.Free;
    Lines.Free;
  end;
end;

procedure TfrmExtract.ReadMt3dConcFile;
var
//  ModflowOutput: TextFile; // the formatted head or drawdown file
//  DescriptorLine: string; // line describing the model characteristics.
  ADatum: double; // a number read from the formatted head or drawdown file
  AString: string;
  TOTIM: double; // elapsed time
  NCOL, NROW, ILAY: integer;
    // the number of columns, number of rows and layer number
  ColIndex, RowIndex: integer; // indexes to the colums and rows
  PointsIndex: integer;
    // an index to the point for which a hydrograph is to be extracted.
  DataString: string;
    // This is used to store the formatted data that will be placed on a line of the TMemo
  ATime: double; // elapsed time for the current time step
  CaptionString: string;
  ASeries: TLineSeries; // a hydrograph plotted on the chart
//  RealArray: array of double; // an array of values read from
//  RealArrayLength: integer;
  ChartIndex: integer;
  Lines: TStringlist;
  ColorIndex: integer;
  PointCount: integer;
  PointIndex: integer;
  SeriesIndex: integer;
//  DescriptorItems: TStringList;
  FileStream: TFileStream;
//  Precision: TModflowPrecision;
  KSTP: integer;
  KPER: integer;
  DESC: TModflowDesc;
  AnArray: TModflowDoubleArray;
  Columns, Rows, Layers: array of integer;
//  DataArray: array of array of double;
  NTRANS: integer;
begin
  //  Put the name of the file on the caption of the form.
  Caption := 'GW_Chart: ' + OpenDialog1.FileName;
  // disable thing that shouldn't be changed while reading the file.
  btnSave.Enabled := True;
  btnCancel.Enabled := True;
  btnRead.Enabled := False;
  adeCellCount.Enabled := False;
  dgDataPoints.Enabled := False;
  Lines := TStringlist.Create;
  try
    begin
      // ClearSeriesList destroys all the TLineSeries on the chart and
      // clears SeriesList
      ClearSeriesList;

      // initialize CancelProcess. CancelProcess will be tested while the
      // file is being read to see if the user has pressed the cancel button.
      CancelProcess := False;

      // initialize the memo
      RichEdit1.Clear;
      Lines.Add(Chr(9) + '(Column,Row,Layer)');

      // create a headers for the data in the memo and create a TLineSeries
      // for each MODFLOW cell.
      AString := 'Time';
      ColorIndex := -1;

      for RowIndex := 1 to dgDataPoints.RowCount - 1 do
      begin
        Inc(ColorIndex);
        if ColorIndex >= Length(ColorPalette) then
        begin
          ColorIndex := 0;
        end;
        CaptionString := '(' + dgDataPoints.Cells[0, RowIndex] + ',' +
          dgDataPoints.Cells[1, RowIndex] + ',' +
          dgDataPoints.Cells[2, RowIndex] + ')';
        AString := AString + Chr(9) + CaptionString;

        ASeries := TLineSeries.Create(ChartHydExtractor);
        ASeries.XValues.Order := loNone;
        ASeries.SeriesColor := ColorPalette[ColorIndex];
        ASeries.ParentChart := ChartHydExtractor;
        MySeriesList.Add(ASeries);
        ASeries.Title := CaptionString;
        ASeries.Pointer.Visible := True;
        ASeries.Active := cbPlot.Checked and (dgDataPoints.Cells[3, RowIndex]
          = dgDataPoints.Columns[3].PickList.Strings[1]);
        ASeries.BeginUpdate;
      end;
      //    Memo1.Lines.Add(AString);
      Lines.Add(AString);

      // this may take a while so change the cursor.

      Screen.Cursor := crHourGlass;
      try

        SetLength(Columns, dgDataPoints.RowCount - 1);
        SetLength(Rows, dgDataPoints.RowCount - 1);
        SetLength(Layers, dgDataPoints.RowCount - 1);
        for PointsIndex := 1 to dgDataPoints.RowCount - 1 do
        begin
          Columns[PointsIndex-1] := StrToInt(dgDataPoints.Cells[0, PointsIndex]);
          Rows[PointsIndex-1] := StrToInt(dgDataPoints.Cells[1, PointsIndex]);
          Layers[PointsIndex-1] := StrToInt(dgDataPoints.Cells[2, PointsIndex]);
        end;


        // binary file.
        FileStream := TFileStream.Create(OpenDialog1.FileName,
          fmOpenRead	or fmShareDenyWrite);
        try
          While FileStream.Position < FileStream.Size do
          begin
            ReadSinglePrecisionMt3dmsBinaryRealArray(FileStream,
              NTRANS, KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);

            ATime := TOTIM;
            for PointsIndex := 1 to dgDataPoints.RowCount - 1 do
            begin
              if (ILAY = Layers[PointsIndex-1]) or (ILAY < 0) then
              begin
                ColIndex := Columns[PointsIndex-1]-1;
                if ColIndex >= NCOL then
                begin
                  Continue;
                end;

                if ILAY < 0 then
                begin
                  RowIndex := Layers[PointsIndex-1]-1;
                end
                else
                begin
                  RowIndex := Rows[PointsIndex-1]-1;
                  if RowIndex >= NROW then
                  begin
                    Continue;
                  end;

                end;
                ADatum := AnArray[RowIndex, ColIndex];

                // store the data value temporarily in RealArray
//                  RealArray[PointsIndex - 1] := ADatum;
                // Add the data point to the appropriate TLineSeries
                ASeries := MySeriesList[PointsIndex - 1];
                ASeries.AddXY(ATime, ADatum, '', clTeeColor);
              end;
            end;
          end;

        finally
          FileStream.Free;
        end;
      finally
        // store the data in the memo.

        if MySeriesList.Count > 0 then
        begin
          ASeries := MySeriesList[0];
          PointCount := ASeries.Count;
          for PointIndex := 0 to PointCount -1 do
          begin
            DataString := '';
            for SeriesIndex := 0 to MySeriesList.Count -1 do
            begin
              ASeries := MySeriesList[SeriesIndex];
              if SeriesIndex = 0 then
              begin
                DataString := FloatToStr(ASeries.XValues[PointIndex])+ Chr(9);
              end;
              if PointIndex < ASeries.Count then
              begin
                DataString := DataString + FloatToStr(ASeries.YValues[PointIndex]);
              end
              else
              begin
                DataString := DataString + ' ';
              end;
              if SeriesIndex < MySeriesList.Count -1 then
              begin
                DataString := DataString + Chr(9);
              end;

            end;
            Lines.Add(DataString);
          end;
        end;
        RichEdit1.Lines.AddStrings(Lines);
        FLinesToSave.Assign(Lines);

        for ChartIndex := 0 to ChartHydExtractor.SeriesList.Count - 1 do
        begin
          ASeries
            {AChartSeries}:= ChartHydExtractor.Series[ChartIndex] as
              TLineSeries;
          ASeries.Active := dgDataPoints.Cells[3, ChartIndex + 1]
            = dgDataPoints.Columns[3].PickList.Strings[1];
          ASeries.EndUpdate;
        end;

        // restore the cursor
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    // enable or disable controls as appropriate.
    btnCancel.Enabled := False;
    btnRead.Enabled := True;
    adeCellCount.Enabled := True;
    dgDataPoints.Enabled := True;
    Lines.Free;
    ChartHydExtractor.Invalidate;
    //    Chart1.Invalidate;
  end;

end;

end.

