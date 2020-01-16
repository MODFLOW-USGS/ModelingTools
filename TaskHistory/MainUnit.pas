unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin, ComCtrls, Grids, RbwDataGrid4,
  ExtCtrls, Menus, TaskHistoryUnit, Clipbrd, TeeProcs, TeEngine, Chart, Series,
  Buttons, ImgList, ToolWin, VclTee.TeeGDIPlus, System.ImageList;

type
  TfrmTaskHistory = class(TForm)
    PageControl1: TPageControl;
    tabSetup: TTabSheet;
    rdgCategories: TRbwDataGrid4;
    tabData: TTabSheet;
    rdgTaskHistory: TRbwDataGrid4;
    dtpStartDate: TDateTimePicker;
    lblStartDate: TLabel;
    seCategoryCount: TJvSpinEdit;
    lblCategories: TLabel;
    Panel1: TPanel;
    seDayCount: TJvSpinEdit;
    lblDayCount: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    tabEffort: TTabSheet;
    chartEffort: TChart;
    serTotalEffort: TBarSeries;
    tabCumEffort: TTabSheet;
    chartCumEffort: TChart;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ImageList1: TImageList;
    btnCopy: TBitBtn;
    btnPaste: TBitBtn;
    procedure seCategoryCountChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure seDayCountChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure dtpStartDateChange(Sender: TObject);
    procedure rdgTaskHistoryStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rdgCategoriesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgCategoriesStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure rdgTaskHistoryEndUpdate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure chartCumEffortGetAxisLabel(Sender: TChartAxis;
      Series: TChartSeries; ValueIndex: Integer; var LabelText: string);
    procedure btnCopyClick(Sender: TObject);
    procedure rdgCategoriesExit(Sender: TObject);
  private
    FTaskHistory: TTaskHistory;
    FReadingFile: Boolean;
    FSettingDate: Boolean;
    FSettingClassification: Boolean;
    FSettingTaskHistoryGrid: Boolean;
    FModified: Boolean;
    procedure SetUpTaskHistoryGrid;
    procedure SetUpClassificationGrid;
    procedure UpdateEffortChart;
    procedure UpdateCumulativeEffortChart;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTaskHistory: TfrmTaskHistory;

implementation

uses
  Math;

{$R *.dfm}

procedure TfrmTaskHistory.btnCopyClick(Sender: TObject);
begin
  rdgTaskHistory.CopyAllCellsToClipboard;
end;

procedure TfrmTaskHistory.btnPasteClick(Sender: TObject);
begin
  rdgTaskHistory.DistributeText(1,1, Clipboard.AsText);
end;

procedure TfrmTaskHistory.chartCumEffortGetAxisLabel(Sender: TChartAxis;
  Series: TChartSeries; ValueIndex: Integer; var LabelText: string);
var
  ADate: TDate;
begin
  if Sender = chartCumEffort.BottomAxis then
  begin
    LabelText := StringReplace(LabelText, ',', '', [rfReplaceAll, rfIgnoreCase]);
    ADate := StrToFloat(LabelText);
    LabelText := DateToStr(ADate);
  end;
end;

procedure TfrmTaskHistory.dtpStartDateChange(Sender: TObject);
var
  ADate: TDate;
  RowIndex: Integer;
  DateString: string;
  Index: Integer;
  HistItem: TDateHistoryItem;
begin
  if FSettingDate then
  begin
    Exit;
  end;
  FSettingDate := True;
  try
    ADate := dtpStartDate.Date;
    rdgTaskHistory.BeginUpdate;
    try
      for RowIndex := 1 to rdgTaskHistory.RowCount - 1 do
      begin
        DateTimeToString(DateString, 'ddddd', ADate);
        rdgTaskHistory.Cells[0,RowIndex] := DateString;
        ADate := ADate + 1;
      end;
    finally
      rdgTaskHistory.EndUpdate
    end;
    if not FReadingFile then
    begin
      ADate := dtpStartDate.Date;
      for Index := 0 to FTaskHistory.History.Count - 1 do
      begin
        HistItem := FTaskHistory.History.Items[Index] as TDateHistoryItem;
        HistItem.Date := ADate + Index;
      end;
    end;
    FModified := True;
  finally
    FSettingDate := False;
  end;
end;

procedure TfrmTaskHistory.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmTaskHistory.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FModified then
  begin
    if MessageDlg('Do you want to save the changes to your file?',
      mtWarning, [mbYes, mbNo], 0, mbYes) = mrYes then
    begin
      Save1Click(Sender)

    end;
  end;
end;

procedure TfrmTaskHistory.FormCreate(Sender: TObject);
begin
  FTaskHistory := TTaskHistory.Create(nil);
  rdgCategories.Cells[0,0] := 'Categories';
  rdgCategories.Cells[1,0] := 'Display';
  rdgTaskHistory.Cells[0,0] := 'Date';
  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmTaskHistory.seDayCountChange(Sender: TObject);
var
  ADate: Extended;
  RowIndex: Integer;
  DateString: string;
begin
  rdgTaskHistory.RowCount := seDayCount.AsInteger + 1;
  if FReadingFile then
  begin
    Exit;
  end;
  ADate := dtpStartDate.Date;
  rdgTaskHistory.BeginUpdate;
  try
    for RowIndex := 1 to rdgTaskHistory.RowCount - 1 do
    begin
      DateTimeToString(DateString, 'ddddd', ADate);
      rdgTaskHistory.Cells[0,RowIndex] := DateString;
      ADate := ADate + 1;
    end;
  finally
    rdgTaskHistory.EndUpdate;
  end;
  FModified := True;
end;

procedure TfrmTaskHistory.SetUpClassificationGrid;
var
  RowCount: Integer;
  Index: Integer;
  Item: TClassificationItem;
begin
  if FSettingClassification then
  begin
    Exit;
  end;
  FSettingClassification := True;
  try
  RowCount := Max(FTaskHistory.Classification.Count,
    seCategoryCount.AsInteger) + 1;
  rdgCategories.BeginUpdate;
  try
    rdgCategories.RowCount := Max(RowCount, 2);
    for Index := 0 to FTaskHistory.Classification.Count - 1 do
    begin
      Item := FTaskHistory.Classification.Items[Index] as TClassificationItem;
      if rdgCategories.Cells[0,Index+1] <> Item.Name then
      begin
        rdgCategories.Cells[0,Index+1] := Item.Name;
      end;
      rdgCategories.Checked[1,Index+1] := Item.Display;
    end;
  finally
    rdgCategories.EndUpdate;
  end;
  finally
    FSettingClassification := False;
    SetUpTaskHistoryGrid;
  end;
end;

procedure TfrmTaskHistory.SetUpTaskHistoryGrid;
var
  NumberOfColumns: Integer;
  Index: Integer;
  ColIndex: Integer;
  HistItem: TDateHistoryItem;
  StartDate: TDate;
  Count: Int64;
  RowIndex: Integer;
  DateString: string;
  ADate: Extended;
  ItemIndex: Integer;
  AValue: string;
  ClassificationItem: TClassificationItem;
  Item: TClassificationItem;
  AColumn: TRbwColumn4;
begin
  if FSettingClassification then
  begin
    Exit;
  end;
  FSettingTaskHistoryGrid := True;
  try
    NumberOfColumns := 1;
    for Index := 0 to FTaskHistory.Classification.Count - 1 do
    begin
      Item := FTaskHistory.Classification.Items[Index] as TClassificationItem;
      if Item.Display then
      begin
        Inc(NumberOfColumns);
      end;
    end;
    rdgTaskHistory.ColCount := Max(NumberOfColumns,2);
    for Index := 1 to rdgTaskHistory.ColCount - 1 do
    begin
      AColumn := rdgTaskHistory.Columns[Index];
      AColumn.Format := rcf4Boolean;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
      AColumn.WordWrapCaptions := True;
    end;
    ColIndex := 0;
    for Index := 0 to FTaskHistory.Classification.Count - 1 do
    begin
      Item := FTaskHistory.Classification.Items[Index] as TClassificationItem;
      if Item.Display then
      begin
        Inc(ColIndex);
        rdgTaskHistory.Cells[ColIndex, 0] := Item.Name;
        rdgTaskHistory.Objects[ColIndex, 0] := Item;
        Item.Column := ColIndex;
      end;
    end;
    rdgTaskHistory.RowCount :=
      Max(FTaskHistory.History.Count, seDayCount.AsInteger)+1;
    seDayCount.AsInteger := rdgTaskHistory.RowCount -1;
    if not FReadingFile then
    begin
      while FTaskHistory.History.Count > 0 do
      begin
        HistItem := FTaskHistory.History.Items[0] as TDateHistoryItem;
        if HistItem.Date < dtpStartDate.Date then
        begin
          FTaskHistory.History.Delete(0);
        end
        else
        begin
          break;
        end;
      end;

    StartDate := dtpStartDate.Date;
    if FTaskHistory.History.Count > 0 then
    begin
      HistItem := FTaskHistory.History.Items[0] as TDateHistoryItem;
      if HistItem.Date > dtpStartDate.Date then
      begin
        Count := Trunc(HistItem.Date) - Trunc(StartDate);
        for Index := 0 to Count - 1 do
        begin
          HistItem := FTaskHistory.History.Add as TDateHistoryItem;
          HistItem.Date := StartDate + Index;
        end;
        FTaskHistory.History.Sort;
      end;
    end;

    ADate := StartDate;
    rdgTaskHistory.BeginUpdate;
    try
      for RowIndex := 1 to rdgTaskHistory.RowCount - 1 do
      begin
        DateTimeToString(DateString, 'ddddd', ADate);
        rdgTaskHistory.Cells[0,RowIndex] := DateString;
        ADate := ADate + 1;
      end;

      rdgTaskHistory.OnStateChange := nil;
      try
        for RowIndex := 1 to rdgTaskHistory.RowCount - 1 do
        begin
          for ColIndex := 0 to rdgTaskHistory.ColCount - 1 do
          begin
            rdgTaskHistory.Checked[ColIndex, RowIndex] := False;
          end;
        end;
        for Index := 0 to FTaskHistory.History.Count - 1 do
        begin
          HistItem := FTaskHistory.History.Items[Index] as TDateHistoryItem;
          for ItemIndex := 0 to HistItem.Categories.Count - 1 do
          begin
            AValue := HistItem.Categories[ItemIndex];
            ClassificationItem := FTaskHistory.Classification.GetItemByName(AValue);
            Assert(ClassificationItem <> nil);
            if ClassificationItem.Display then
            begin
              rdgTaskHistory.Checked[ClassificationItem.Column, Index+1] := True;
            end;
          end;
        end;
      finally
        rdgTaskHistory.OnStateChange := rdgTaskHistoryStateChange;
      end;
    finally
      rdgTaskHistory.EndUpdate
    end;
    end;
  finally
    FSettingTaskHistoryGrid := False;
  end;
end;

procedure TfrmTaskHistory.UpdateCumulativeEffortChart;
var
  ClassificationList: TList;
  ClassIndex: Integer;
  ClassItem: TClassificationItem;
  SeriesList: TList;
  ASeries: TLineSeries;
  TotalSeries: TLineSeries;
  HistIndex: Integer;
  HistItem: TDateHistoryItem;
  Values: array of double;
  Total: double;
begin
  chartCumEffort.SeriesList.Clear;
  ClassificationList := TList.Create;
  SeriesList := TList.Create;
  try
    for ClassIndex := 0 to FTaskHistory.Classification.Count - 1 do
    begin
      ClassItem := FTaskHistory.Classification.Items[ClassIndex]
        as TClassificationItem;
      if ClassItem.Display then
      begin
        ClassificationList.Add(ClassItem);
        ASeries := TLineSeries.Create(self);
        ASeries.ParentChart := chartCumEffort;
        SeriesList.Add(ASeries);
        ASeries.Title := ClassItem.Name;
        ASeries.LinePen.Width := 3;
      end;
    end;
    TotalSeries := TLineSeries.Create(self);
    TotalSeries.ParentChart := chartCumEffort;
    TotalSeries.Title := 'Total';
    TotalSeries.LinePen.Width := 3;

    SetLength(Values, ClassificationList.Count);
    for ClassIndex := 0 to Length(Values) - 1 do
    begin
      Values[ClassIndex] := 0;
    end;
    Total := 0.;

    for HistIndex := 0 to FTaskHistory.History.Count - 1 do
    begin
      HistItem := FTaskHistory.History.Items[HistIndex] as TDateHistoryItem;
      if HistItem.Categories.Count > 0 then
      begin
        for ClassIndex := 0 to ClassificationList.Count - 1 do
        begin
          ClassItem := ClassificationList[ClassIndex];
          if HistItem.Categories.IndexOf(ClassItem.Name) >= 0 then
          begin
            Values[ClassIndex] := Values[ClassIndex]
              + 1/HistItem.Categories.Count
          end;
        end;
        Total := Total + 1;
      end;
      for ClassIndex := 0 to SeriesList.Count - 1 do
      begin
        ASeries := SeriesList[ClassIndex];
        ASeries.AddXY(HistItem.Date, Values[ClassIndex])
      end;
      TotalSeries.AddXY(HistItem.Date, Total)
    end;
  finally
    ClassificationList.Free;
    SeriesList.Free;
  end;
end;

procedure TfrmTaskHistory.UpdateEffortChart;
var
  ClassIndex: Integer;
  ClassItem: TClassificationItem;
  HistIndex: Integer;
  AValue: double;
  HistItem: TDateHistoryItem;
begin
  serTotalEffort.Clear;
  for ClassIndex := 0 to FTaskHistory.Classification.Count - 1 do
  begin
    ClassItem := FTaskHistory.Classification.Items[ClassIndex]
      as TClassificationItem;
    if ClassItem.Display then
    begin
      AValue := 0.;
      for HistIndex := 0 to FTaskHistory.History.Count - 1 do
      begin
        HistItem := FTaskHistory.History.Items[HistIndex] as TDateHistoryItem;
        if HistItem.Categories.IndexOf(ClassItem.Name) >= 0 then
        begin
          AValue := AValue + 1/HistItem.Categories.Count;
        end;
      end;
      serTotalEffort.AddBar(AValue, ClassItem.Name, clRed);
    end;
  end;
end;

procedure TfrmTaskHistory.FormDestroy(Sender: TObject);
begin
  FTaskHistory.Free;
end;

procedure TfrmTaskHistory.Open1Click(Sender: TObject);
var
  AFileStream: TFileStream;
  AMemoryStream: TMemoryStream;
  HistItem: TDateHistoryItem;
begin
  if OpenDialog1.Execute then
  begin
    SaveDialog1.FileName := OpenDialog1.FileName;

    FReadingFile := True;
    AMemoryStream := TMemoryStream.Create;
    AFileStream := TFileStream.Create(OpenDialog1.FileName,
      fmOpenRead or fmShareDenyWrite);
    try
      ObjectTextToBinary(AFileStream, AMemoryStream);
      AMemoryStream.Position := 0;
      AMemoryStream.ReadComponent(FTaskHistory);

      seCategoryCount.AsInteger := FTaskHistory.Classification.Count;
      if FTaskHistory.History.Count > 0 then
      begin
        HistItem := FTaskHistory.History.Items[0] as TDateHistoryItem;
        dtpStartDate.Date := HistItem.Date;
      end;
      SetUpClassificationGrid;
    finally
      AFileStream.Free;
      AMemoryStream.Free;
      FReadingFile := False;
    end;
    SetUpTaskHistoryGrid;
    rdgTaskHistory.TopRow  := rdgTaskHistory.RowCount -5;
    FModified := False;
  end;
end;

procedure TfrmTaskHistory.PageControl1Change(Sender: TObject);
begin

  if PageControl1.ActivePage = tabEffort then
  begin
    UpdateEffortChart;
  end
  else if PageControl1.ActivePage = tabCumEffort then
  begin
    UpdateCumulativeEffortChart
  end;
end;

procedure TfrmTaskHistory.rdgCategoriesExit(Sender: TObject);
begin
  seCategoryCount.AsInteger := rdgCategories.RowCount -1;
  SetUpClassificationGrid;
end;

procedure TfrmTaskHistory.rdgCategoriesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  Item: TClassificationItem;
  HistIndex: Integer;
  HistItem: TDateHistoryItem;
  NamePosition: Integer;
begin
  rdgCategories.BeginUpdate;
  try
    if (ACol = 0) and (Value <> '') then
    begin
      while FTaskHistory.Classification.Count < ARow do
      begin
        Item := FTaskHistory.Classification.Add as TClassificationItem;
        rdgCategories.Checked[ACol, ARow] := True;
        Item.Display := True;
      end;
      Item := FTaskHistory.Classification.Items[ARow-1] as TClassificationItem;
      if Item.Name <> Value then
      begin
        for HistIndex := 0 to FTaskHistory.History.Count - 1 do
        begin
          HistItem := FTaskHistory.History.Items[HistIndex] as TDateHistoryItem;
          NamePosition := HistItem.Categories.IndexOf(Item.Name);
          if NamePosition >= 0 then
          begin
            HistItem.Categories[NamePosition] := Value;
          end;
        end;
        Item.Name := Value;
        SetUpTaskHistoryGrid;
      end;
    end;
  finally
    rdgCategories.EndUpdate
  end;
  FModified := True;
end;

procedure TfrmTaskHistory.rdgCategoriesStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  Item: TClassificationItem;
begin
  FModified := True;
  while FTaskHistory.Classification.Count < ARow-1 do
  begin
    FTaskHistory.Classification.Add;
  end;
  Item := FTaskHistory.Classification.Items[ARow-1] as TClassificationItem;
  Item.Display := (Value = cbChecked);
  SetUpTaskHistoryGrid;
end;

procedure TfrmTaskHistory.rdgTaskHistoryEndUpdate(Sender: TObject);
begin
  if FSettingTaskHistoryGrid then
  begin
    Exit;
  end;
  seDayCount.AsInteger := rdgTaskHistory.RowCount -1;
  dtpStartDateChange(nil);
end;

procedure TfrmTaskHistory.rdgTaskHistoryStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  StartDate: TDate;
  Item: TDateHistoryItem;
  AName: string;
  ItemPosition: Integer;
begin
  if FSettingTaskHistoryGrid then
  begin
    Exit;
  end;
  StartDate := dtpStartDate.Date;
  While FTaskHistory.History.Count < ARow do
  begin
    Item := FTaskHistory.History.Add as TDateHistoryItem;
    Item.Date := StartDate + FTaskHistory.History.Count -1;
  end;
  Item := FTaskHistory.History.Items[ARow-1] as TDateHistoryItem;

  AName := rdgTaskHistory.Cells[ACol,0];
  ItemPosition := Item.Categories.IndexOf(AName);
  if Value = cbChecked then
  begin
    if ItemPosition < 0 then
    begin
      Item.Categories.Add(AName);
      FModified := True;
    end;
  end
  else
  begin
    if ItemPosition >= 0 then
    begin
      Item.Categories.Delete(ItemPosition);
      FModified := True;
    end;
  end;
end;

procedure TfrmTaskHistory.Save1Click(Sender: TObject);
var
  AFileStream: TFileStream;
  AMemoryStream: TMemoryStream;
begin
  if SaveDialog1.FileName = '' then
  begin
    SaveAs1Click(Sender);
  end
  else
  begin
    AMemoryStream := TMemoryStream.Create;

    AFileStream := TFileStream.Create(SaveDialog1.FileName,
      fmCreate or fmShareDenyWrite);
    try
      AMemoryStream.WriteComponent(FTaskHistory);
      AMemoryStream.Position := 0;
      ObjectBinaryToText(AMemoryStream, AFileStream);
    finally
      AFileStream.Free;
      AMemoryStream.Free;
    end;
    FModified := False;
  end;
end;

procedure TfrmTaskHistory.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Save1Click(Sender)
  end;
end;

procedure TfrmTaskHistory.seCategoryCountChange(Sender: TObject);
begin
  SetUpClassificationGrid;
  FModified := True;
end;

end.
