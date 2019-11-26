unit frmConvertFlowsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, ExtCtrls, StdCtrls, Mask, JvExMask,
  JvToolEdit, ReadModflowArrayUnit, Menus, MyFormUnit, JvSpin, IntListUnit,
  ComCtrls;

type
  TfrmConvertFlows = class(TMyForm)
    Panel3: TPanel;
    Panel4: TPanel;
    rdgExportFiles: TRbwDataGrid4;
    feFlowFile: TJvFilenameEdit;
    lblFlowFile: TLabel;
    btnConvert: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    About1: TMenuItem;
    rdgCellsToExport: TRbwDataGrid4;
    pnlCellsToExport: TPanel;
    rgCellsToExport: TRadioGroup;
    spinCellCount: TJvSpinEdit;
    lblCellCount: TLabel;
    spl1: TSplitter;
    pbProgress: TProgressBar;
    procedure feFlowFileChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rdgExportFilesSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure rdgExportFilesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgExportFilesStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure rgCellsToExportClick(Sender: TObject);
    procedure spinCellCountChange(Sender: TObject);
    procedure rdgCellsToExportEndUpdate(Sender: TObject);
  private
    FMessageHidden: boolean;
    FlowNames: TStringList;
    procedure GetFlowNames(const FileName: string);
    procedure WriteArrayLabel(KSTP,KPER: Integer; PERTIM, TOTIM: TModflowDouble;
      TEXT: TModflowDesc; NCOL, NROW,ILAY: Integer; AFile: TFileStream);
    procedure WriteLayerArray(AnArray: T3DTModflowArray; ILAY: Integer;
      AFile: TFileStream);
    procedure WriteCells(AnArray: T3DTModflowArray; KSTP, KPER: Integer;
      TOTIM: TModflowDouble; AFile: TFileStream; Columns, Rows,
      Layers: TIntegerList; NCOL, NROW, NLAY: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmConvertFlows: TfrmConvertFlows;

const
  ConvertOption = '-convert';

procedure HandleParams;

implementation

uses frmModChartUnit, Contnrs, frmAboutUnit, frmBudgetPrecisionQueryUnit;

{$R *.dfm}

procedure HandleParams;
var
  Option: string;
  Index: integer;
  FileName: string;
begin
  if ParamCount > 0 then
  begin
    Option := ParamStr(1);
    if SameText(ConvertOption, Option) then
    begin
      frmModChart.FormChoiceClick(frmModChart.ConvertCellWaterBudgets1);
      frmConvertFlows.feFlowFile.DialogFiles.Clear;
      for Index := 2 to ParamCount do
      begin
        FileName := Trim(ParamStr(Index));
        if FileExists(FileName) then
        begin
          frmConvertFlows.feFlowFile.DialogFiles.Add(FileName);
        end;
      end;
      frmConvertFlows.feFlowFileChange(nil);
      for Index := 1 to frmConvertFlows.rdgExportFiles.RowCount -1 do
      begin
        frmConvertFlows.rdgExportFiles.Checked[0,Index] := True;
      end;
      frmConvertFlows.FMessageHidden := True;
      frmConvertFlows.btnConvertClick(nil);
      frmModChart.Close;
    end;
  end;
end;

procedure TfrmConvertFlows.GetFlowNames(const FileName: string);
var
  BudgetFile: TFileStream;
  Precision: TModflowPrecision;
  FileLength: Int64;
  KSTP, KPER: Integer;
  PERTIM, TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL, NROW, NLAY: Integer;
  AnArray: T3DTModflowArray;
  IRESULT: integer;
  Description: string;
begin
  FlowNames.Clear;
  if FileExists(FileName) then
  begin
    BudgetFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      FileLength := BudgetFile.Size;
      Precision := QueryBudgetPrecision(BudgetFile);
      while BudgetFile.Position < FileLength do
      begin
        case Precision of
          mpSingle:
            begin
              ReadModflowSinglePrecFluxArray(BudgetFile, KSTP, KPER, PERTIM,
                TOTIM, DESC, NCOL, NROW, NLAY, AnArray, IRESULT);
            end;
          mpDouble:
            begin
              ReadModflowDoublePrecFluxArray(BudgetFile, KSTP, KPER, PERTIM,
                TOTIM, DESC, NCOL, NROW, NLAY, AnArray, IRESULT);
            end;
        else Assert(False);
        end;
        if (IRESULT <> 0) then
        begin
          Exit;
        end;
        Description := DESC;
        Description := Trim(Description);
        if FlowNames.IndexOf(Description) < 0 then
        begin
          FlowNames.Add(Description);
        end
        else
        begin
          Exit;
        end;
      end;
    finally
      BudgetFile.Free;
    end;
  end;
end;

procedure TfrmConvertFlows.feFlowFileChange(Sender: TObject);
var
  FileRoot: string;
  Index: integer;
  FileIndex: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    if FlowNames = nil then
    begin
      FlowNames := TStringList.Create;
    end;
    for FileIndex := 0 to feFlowFile.DialogFiles.Count -1 do
    begin
      GetFlowNames(feFlowFile.DialogFiles[FileIndex]);
    end;

    if FlowNames.Count >= 1 then
    begin
      FileRoot := ChangeFileExt(feFlowFile.FileName, '');
      rdgExportFiles.BeginUpdate;
      try
        rdgExportFiles.RowCount := FlowNames.Count + 1;
        for Index := 0 to FlowNames.Count -1 do
        begin
          rdgExportFiles.Cells[0,Index+1] := FlowNames[Index];
          rdgExportFiles.Cells[1,Index+1] := '.f' + IntToStr(Index + 1);
          rdgExportFiles.Checked[0,Index+1] := False;
        end;
      finally
        rdgExportFiles.EndUpdate;
      end;
      btnConvert.Enabled := True;
    end
    else
    begin
      btnConvert.Enabled := False;
      rdgExportFiles.RowCount := 2;
      rdgExportFiles.Checked[0,1] := False;
      rdgExportFiles.Cells[0,1] := '';
      rdgExportFiles.Cells[1,1] := '';
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmConvertFlows.FormDestroy(Sender: TObject);
begin
  FlowNames.Free;
end;

procedure TfrmConvertFlows.rdgExportFilesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if ACol = 1 then
  begin
    CanSelect := rdgExportFiles.Checked[0,ARow];
  end;
end;

procedure TfrmConvertFlows.rdgExportFilesBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  FileNames: TStringList;
  Index: integer;
begin
  if (ACol = 1) and (ARow >= 1) and rdgExportFiles.Checked[0, ARow] then
  begin
    if Trim(rdgExportFiles.Cells[ACol, ARow]) = '' then
    begin
      rdgExportFiles.Canvas.Brush.Color := clRed;
    end
    else
    begin
      FileNames := TStringList.Create;
      try
        FileNames.CaseSensitive := False;
        for Index := 1 to ARow-1 do
        begin
          if rdgExportFiles.Checked[0, Index] then
          begin
            FileNames.Add(rdgExportFiles.Cells[ACol, Index])
          end;
        end;
        if FileNames.IndexOf(rdgExportFiles.Cells[ACol, ARow]) >= 0 then
        begin
          rdgExportFiles.Canvas.Brush.Color := clRed;
        end;
      finally
        FileNames.Free;
      end;
    end;
  end;
end;

procedure TfrmConvertFlows.rdgExportFilesStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  rdgExportFiles.Invalidate;
end;

procedure TfrmConvertFlows.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Visible then
  begin
    frmModChart.Close
  end;
end;

procedure TfrmConvertFlows.WriteArrayLabel(KSTP, KPER: Integer; PERTIM,
  TOTIM: TModflowDouble; TEXT: TModflowDesc; NCOL, NROW, ILAY: Integer;
  AFile: TFileStream);
const
  FMTOUT = '(10(1X1PE13.5))';
var
  ALine: AnsiString;
  Description: AnsiString;
begin
  Description := TEXT;
  ALine := Format(' %5d%5d %14g %14g %s%6d%6d%6d %s'#13#10,
    [KSTP, KPER, PERTIM, TOTIM, Description, NCOL, NROW, ILAY+1, FMTOUT]);

  AFile.Write(ALine[1], Length(ALine)*SizeOf(AnsiChar));
end;

procedure TfrmConvertFlows.WriteLayerArray(AnArray: T3DTModflowArray;
  ILAY: Integer; AFile: TFileStream);
var
  AValue: AnsiString;
  ColCount: integer;
  ColIndex, RowIndex: integer;
begin
  for RowIndex := 0 to Length(AnArray[ILAY]) -1 do
  begin
    ColCount := Length(AnArray[ILAY, RowIndex]);
    for ColIndex := 0 to ColCount -1 do
    begin
      if AnArray[ILAY, RowIndex, ColIndex] >= 0 then
      begin
        AValue := Format(' %13.7e', [AnArray[ILAY, RowIndex, ColIndex]]);
      end
      else
      begin
        AValue := Format(' %13.6e', [AnArray[ILAY, RowIndex, ColIndex]]);
      end;
      AFile.Write(AValue[1], Length(AValue)*SizeOf(AnsiChar));
      if (((ColIndex + 1) mod 10) = 0) or (ColIndex = (ColCount -1)) then
      begin
        AValue := #13#10;
        AFile.Write(AValue[1], Length(AValue)*SizeOf(AnsiChar));
      end;
    end;
  end;
end;

procedure TfrmConvertFlows.WriteCells(AnArray: T3DTModflowArray;
  KSTP,KPER: Integer; TOTIM: TModflowDouble; AFile: TFileStream;
  Columns, Rows, Layers : TIntegerList; NCOL, NROW, NLAY: Integer);
var
  CellIndex: integer;
  AColumn, ARow, ALayer: integer;
  AValue: AnsiString;
  KPER_String, KSTP_String: AnsiString;
begin
  KPER_String := IntToStr(KPER);
  while Length(KPER_String) < 13 do
  begin
    KPER_String := ' ' + KPER_String;
  end;
  KSTP_String := IntToStr(KSTP);
  while Length(KSTP_String) < 9 do
  begin
    KSTP_String := ' ' + KSTP_String;
  end;

  AValue := Format('%s '#9'%s '#9'%13.7e ', [KPER_String, KSTP_String, TOTIM]);
  AFile.Write(AValue[1], Length(AValue)*SizeOf(AnsiChar));
  for CellIndex := 0 to Columns.Count -1 do
  begin
    AColumn := Columns[CellIndex]-1;
    ARow := Rows[CellIndex]-1;
    ALayer := Layers[CellIndex]-1;
    if (AColumn < NCOL) and (ARow < NROW) and (ALayer < Abs(NLAY)) then
    begin
      AValue := Format(#9'%13.7e ', [AnArray[ALayer, ARow, AColumn]]);
    end
    else
    begin
      AValue := #9'Invalid location';
    end;
    AFile.Write(AValue[1], Length(AValue)*SizeOf(AnsiChar));
  end;
  AValue := #13#10;
  AFile.Write(AValue[1], Length(AValue)*SizeOf(AnsiChar));
end;

procedure TfrmConvertFlows.btnConvertClick(Sender: TObject);
var
  RowIndex: integer;
  Files: TList;
  AFile: TFileStream;
  BudgetFile: TFileStream;
  FileLength: Int64;
  Precision: TModflowPrecision;
  KSTP, KPER: Integer;
  PERTIM, TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL, NROW, NLAY: Integer;
  AnArray: T3DTModflowArray;
  IRESULT: integer;
  Description: string;
  Position: integer;
  LayerIndex: integer;
  FileIndex: integer;
  FileName, NewFileName: string;
  Extension: string;
  Columns, Rows, Layers : TIntegerList;
  AColumn, ARow, ALayer: integer;
  CellIndex: integer;
  ALabel: AnsiString;
  Divider: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Columns := nil;
    Rows := nil;
    Layers := nil;
    try
      if rgCellsToExport.ItemIndex = 1 then
      begin
        Columns := TIntegerList.Create;
        Rows := TIntegerList.Create;
        Layers := TIntegerList.Create;
        for RowIndex := 1 to rdgCellsToExport.RowCount -1 do
        begin
          if TryStrToInt(rdgCellsToExport.Cells[2,RowIndex], AColumn)
            and TryStrToInt(rdgCellsToExport.Cells[1,RowIndex], ARow)
            and TryStrToInt(rdgCellsToExport.Cells[0,RowIndex], ALayer) then
          begin
            Columns.Add(AColumn);
            Rows.Add(ARow);
            Layers.Add(ALayer);
          end;
        end;
        if Columns.Count = 0 then
        begin
          Beep;
          MessageDlg('No cells specified. Aborting.', mtWarning, [mbOK], 0);
          Exit;
        end;

      end;


      for FileIndex := 0 to feFlowFile.DialogFiles.Count -1 do
      begin
        FileName := feFlowFile.DialogFiles[FileIndex];

        if FileExists(FileName) then
        begin
          Files := TObjectList.Create;
          try
            for RowIndex := 1 to rdgExportFiles.RowCount -1 do
            begin
              if rdgExportFiles.Checked[0,RowIndex] then
              begin
                Extension := rdgExportFiles.Cells[1,RowIndex];
                if Extension = '' then
                begin
                  Extension := '.F' + IntToStr(RowIndex);
                end;
                if Extension[1] <> '.' then
                begin
                  Extension := '.' + Extension;
                end;
                NewFileName := ChangeFileExt(FileName, Extension);
                AFile := TFileStream.Create(NewFileName,
                  fmCreate or fmShareDenyWrite);
                Files.Add(AFile);

                if Columns <> nil then
                begin
                  ALabel := rdgExportFiles.Cells[0,RowIndex] + #9'(Layer, Row, Column)'#13#10;
                  AFile.Write(ALabel[1], Length(ALabel)*SizeOf(AnsiChar));

                  ALabel := 'Stress Period '#9'Time Step '#9'Total Time ';
                  AFile.Write(ALabel[1], Length(ALabel)*SizeOf(AnsiChar));

                  for CellIndex := 0 to Columns.Count -1 do
                  begin
                    ALabel := Format(#9'( %d, %d, %d)',
                      [Layers[CellIndex], Rows[CellIndex], Columns[CellIndex]]);
                    AFile.Write(ALabel[1], Length(ALabel)*SizeOf(AnsiChar));
                  end;
                  ALabel := #13#10;
                  AFile.Write(ALabel[1], Length(ALabel)*SizeOf(AnsiChar));
                end;

              end
              else
              begin
                Files.Add(nil);
              end;
            end;

            BudgetFile := TFileStream.Create(FileName,
              fmOpenRead or fmShareDenyWrite);
            try
              FileLength := BudgetFile.Size;
              Precision := QueryBudgetPrecision(BudgetFile);

              Divider := 1;
              While FileLength div Divider > MAXINT do
              begin
                Divider := Divider * 1024;
              end;
              pbProgress.Max := FileLength div Divider;

              while BudgetFile.Position < FileLength do
              begin
                case Precision of
                  mpSingle:
                    begin
                      ReadModflowSinglePrecFluxArray(BudgetFile, KSTP, KPER, PERTIM,
                        TOTIM, DESC, NCOL, NROW, NLAY, AnArray, IRESULT);
                    end;
                  mpDouble:
                    begin
                      ReadModflowDoublePrecFluxArray(BudgetFile, KSTP, KPER, PERTIM,
                        TOTIM, DESC, NCOL, NROW, NLAY, AnArray, IRESULT);
                    end;
                else Assert(False);
                end;
                if (IRESULT <> 0) then
                begin
                  Exit;
                end;
                pbProgress.Position := BudgetFile.Position div Divider;
                Application.ProcessMessages;
                Description := DESC;
                Description := Trim(Description);
                Position := FlowNames.IndexOf(Description);
                if (Position >= 0) then
                begin
                  AFile := Files[Position];
                  if AFile <> nil then
                  begin
                    if Columns <> nil then
                    begin
                      WriteCells(AnArray, KSTP,KPER, TOTIM, AFile,
                        Columns, Rows, Layers, NCOL, NROW, NLAY);
                    end
                    else
                    begin
                      for LayerIndex := 0 to Abs(NLAY)-1 do
                      begin
                        WriteArrayLabel(KSTP,KPER, PERTIM, TOTIM,
                          DESC, NCOL, NROW,LayerIndex, AFile);
                        WriteLayerArray(AnArray, LayerIndex, AFile);
                      end;
                    end;

                  end;
                end;
              end;
            finally
              BudgetFile.Free;
            end;
          finally
            Files.Free;
          end;
        end;
      end;
    finally
      Columns.Free;
      Rows.Free;
      Layers.Free;
    end;
  finally
    Screen.Cursor := crDefault;
    if not FMessageHidden then
    begin
      ShowMessage('Done');
    end;
  end;
end;

procedure TfrmConvertFlows.FormCreate(Sender: TObject);
begin
  FMessageHidden := False;
  rdgExportFiles.Cells[0,0] := 'Budget Term';
  rdgExportFiles.Cells[1,0] := 'Exported File Extension';

  rdgCellsToExport.Cells[0,0] := 'Layer';
  rdgCellsToExport.Cells[1,0] := 'Row';
  rdgCellsToExport.Cells[2,0] := 'Column';
end;

procedure TfrmConvertFlows.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmConvertFlows.FormShow(Sender: TObject);
begin
  MainMenu1.Merge(frmModChart.mainMenuFormChoice);
end;

procedure TfrmConvertFlows.Help2Click(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfrmConvertFlows.About1Click(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmConvertFlows.rgCellsToExportClick(Sender: TObject);
begin
  spinCellCount.Enabled := rgCellsToExport.ItemIndex = 1;
  rdgCellsToExport.Enabled := spinCellCount.Enabled;
end;

procedure TfrmConvertFlows.spinCellCountChange(Sender: TObject);
begin
  rdgCellsToExport.RowCount := spinCellCount.AsInteger + 1;
end;

procedure TfrmConvertFlows.rdgCellsToExportEndUpdate(Sender: TObject);
begin
  spinCellCount.AsInteger := rdgCellsToExport.RowCount-1;
end;

end.
