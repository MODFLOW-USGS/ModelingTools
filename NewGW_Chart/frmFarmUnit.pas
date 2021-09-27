unit frmFarmUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, TeeProcs, TeEngine, Chart, StdCtrls, CheckLst,
  JvExCheckLst, JvCheckListBox, Contnrs, RealListUnit, IntListUnit, Series,
  TeeEdit, MyFormUnit, JvPageList, JvExControls;

type
  TFarmFileType = (fftNone, fftBudget, fftSupplyAndDemand);

  TSeriesList = class(TObjectList)
  private
    function GetItems(Index: Integer): TLineSeries;
    procedure SetItems(Index: Integer; const Value: TLineSeries);
  public
    function Add(ASeries: TLineSeries): integer;
    property Items[Index: Integer]: TLineSeries read GetItems
      write SetItems; default;
  end;

  TDetailedFarmBudgetItem = class(TObject)
    FTime: double;
    FInItems: TRealList;
    FOutItems: TRealList;
    FNetItems: TRealList;
    FDiscrepancyItems: TRealList;
    constructor Create;
    destructor Destroy; override;
  end;

  TDetailedFarmBudget = class(TObjectList)
  private
    FFarmID: Integer;
    function GetItems(Index: Integer): TDetailedFarmBudgetItem;
    procedure SetItems(Index: Integer;
      const Value: TDetailedFarmBudgetItem);
    procedure SetFarmID(const Value: Integer);
  public
    function Add(BudgetItem: TDetailedFarmBudgetItem): integer;
    property Items[Index: Integer]: TDetailedFarmBudgetItem read GetItems
      write SetItems; default;
    property FarmID: Integer read FFarmID write SetFarmID;
  end;

  TDetailedFarmBudgets = class(TObjectList)
  private
    function GetItems(Index: Integer): TDetailedFarmBudget;
    procedure SetItems(Index: Integer; const Value: TDetailedFarmBudget);
  public
    function Add(BudgetList: TDetailedFarmBudget): integer;
    property Items[Index: Integer]: TDetailedFarmBudget read GetItems
      write SetItems; default;
  end;

  TFarmSupplyDeficiencyItem = class(TObject)
    FTime: double;
    FInitialItems: TRealList;
    FFinalItems: TRealList;
    FOfe: Double;
    FDeficiencyFlag: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TFarmSupplyDeficiency = class(TObjectList)
  private
    FFarmID: Integer;
    FInitialSeries: TSeriesList;
    FFinalSeries: TSeriesList;
    function GetItems(Index: Integer): TFarmSupplyDeficiencyItem;
    procedure SetItems(Index: Integer;
      const Value: TFarmSupplyDeficiencyItem);
    procedure SetFarmID(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(SupplyDeficiencyItem: TFarmSupplyDeficiencyItem): integer;
    property Items[Index: Integer]: TFarmSupplyDeficiencyItem read GetItems
      write SetItems; default;
    property FarmID: Integer read FFarmID write SetFarmID;
  end;

  TFarmSupplyDeficiencies = class(TObjectList)
  private
    function GetItems(Index: Integer): TFarmSupplyDeficiency;
    procedure SetItems(Index: Integer; const Value: TFarmSupplyDeficiency);
  public
    function Add(FarmSupplyDeficiency: TFarmSupplyDeficiency): integer;
    property Items[Index: Integer]: TFarmSupplyDeficiency read GetItems
      write SetItems; default;
  end;


  TfrmFarm = class(TMyForm)
    chtFarmPlot: TChart;
    mmFarm: TMainMenu;
    miFile1: TMenuItem;
    miOpen: TMenuItem;
    dlgOpenFarm: TOpenDialog;
    pnl1: TPanel;
    pnl2: TPanel;
    spl1: TSplitter;
    spl2: TSplitter;
    pnl3: TPanel;
    clbDiscrepancy: TJvCheckListBox;
    pnl4: TPanel;
    spl3: TSplitter;
    pnl5: TPanel;
    clbInMinusOut: TJvCheckListBox;
    pnl6: TPanel;
    pnl7: TPanel;
    spl4: TSplitter;
    pnl8: TPanel;
    clbOut: TJvCheckListBox;
    pnl9: TPanel;
    pnl10: TPanel;
    clbIn: TJvCheckListBox;
    pnl11: TPanel;
    pnl12: TPanel;
    pnl13: TPanel;
    clbFarm: TJvCheckListBox;
    pnl14: TPanel;
    cbNet: TCheckBox;
    cbDiscrepancy: TCheckBox;
    cbIn: TCheckBox;
    cbOut: TCheckBox;
    cbFarm: TCheckBox;
    miExit1: TMenuItem;
    ChartEditor1: TChartEditor;
    miFormatChart1: TMenuItem;
    dlgPntSet1: TPrinterSetupDialog;
    dlgPnt1: TPrintDialog;
    miPrintSetup1: TMenuItem;
    miPrintPreview1: TMenuItem;
    ChartPreviewer1: TChartPreviewer;
    miPrint1: TMenuItem;
    miSaveasImage1: TMenuItem;
    sd1: TSaveDialog;
    pnlFarmBudgets: TPanel;
    pnl16: TPanel;
    pnl17: TPanel;
    clbFarmAggregates: TJvCheckListBox;
    pnl18: TPanel;
    cbFarmAggregates: TCheckBox;
    spl5: TSplitter;
    miHelp1: TMenuItem;
    miHelp2: TMenuItem;
    miAbout1: TMenuItem;
    jvplSeriestypes: TJvPageList;
    jvspFarmBudgets: TJvStandardPage;
    jvspDemandSupply: TJvStandardPage;
    cbOFE: TCheckBox;
    cbDefFlag: TCheckBox;
    pnl19: TPanel;
    spl6: TSplitter;
    pnl20: TPanel;
    clbDefFinal: TJvCheckListBox;
    pnl21: TPanel;
    cbDefFinal: TCheckBox;
    pnl22: TPanel;
    clbDefInitial: TJvCheckListBox;
    pnl23: TPanel;
    cbDefInitial: TCheckBox;
    pnl24: TPanel;
    pnl25: TPanel;
    clbFarmsSupplyDef: TJvCheckListBox;
    pnl26: TPanel;
    cbFarmsSupplyDef: TCheckBox;
    procedure miOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure clbInClickCheck(Sender: TObject);
    procedure clbOutClickCheck(Sender: TObject);
    procedure clbInMinusOutClickCheck(Sender: TObject);
    procedure clbDiscrepancyClickCheck(Sender: TObject);
    procedure clbFarmClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbInClick(Sender: TObject);
    procedure cbOutClick(Sender: TObject);
    procedure cbNetClick(Sender: TObject);
    procedure cbDiscrepancyClick(Sender: TObject);
    procedure cbFarmClick(Sender: TObject);
    procedure miExit1Click(Sender: TObject);
    procedure miFormatChart1Click(Sender: TObject);
    procedure chtFarmPlotGetLegendRect(Sender: TCustomChart;
      var Rect: TRect);
    procedure chtFarmPlotGetLegendPos(Sender: TCustomChart; Index: Integer;
      var X, Y, XColor: Integer);
    procedure chtFarmPlotAfterDraw(Sender: TObject);
    procedure miPrintSetup1Click(Sender: TObject);
    procedure miPrintPreview1Click(Sender: TObject);
    procedure miPrint1Click(Sender: TObject);
    procedure miSaveasImage1Click(Sender: TObject);
    procedure cbFarmAggregatesClick(Sender: TObject);
    procedure miHelp2Click(Sender: TObject);
    procedure miAbout1Click(Sender: TObject);
    procedure cbFarmsSupplyDefClick(Sender: TObject);
    procedure cbDefInitialClick(Sender: TObject);
    procedure cbDefFinalClick(Sender: TObject);
    procedure cbOFEClick(Sender: TObject);
    procedure cbDefFlagClick(Sender: TObject);
    procedure clbFarmsSupplyDefClickCheck(Sender: TObject);
    procedure clbDefInitialClickCheck(Sender: TObject);
    procedure clbDefFinalClickCheck(Sender: TObject);
  private
    // farm budgets
    FFarms: TDetailedFarmBudgets;
    FInIndicies: TIntegerList;
    FOutIndicies: TIntegerList;
    FNetIndicies: TIntegerList;
    FDiscrepancyIndicies: TIntegerList;
    FBusy: Integer;

    FSeriesList: TSeriesList;
    FAggregateSeriesList: TSeriesList;
    LegendRect : TRect;
    ExplantionVisible : boolean;
    ExplanationTop, ExplanationLeft : integer;
    DeltaLegendX: integer;

    // farm supply/deficiency
    FFarmIdIndex: integer;
    FOfeIndex: integer;
    FDefFlagIndex: integer;
    FInitialFdsIndicies: TIntegerList;
    FFinalFdsIndicies: TIntegerList;
    FarmSupplyDeficiencies: TFarmSupplyDeficiencies;
    FOfeSeriesList: TSeriesList;
    FDefFlagSeriesList: TSeriesList;
    procedure CreateBudgetSeries;
    procedure CreateFarmDemandSupplySeries;
    procedure FillAggregateSeries;
    procedure DisplayFarmBudgetSeries;
    procedure DisplayFarmDemandSeries;
    procedure ChangeAllBudgetCheckBoxesInList(CB: TJvCheckListBox;
      Checked: Boolean);
    procedure ChangeAllDemandCheckBoxesInList(CB: TJvCheckListBox;
      Checked: Boolean);
    procedure HandleFarmBudget;
    procedure HandleFarmDemandSupply;
    procedure ClearAllSeries;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFarm: TfrmFarm;

implementation

uses
  frmModChartUnit, frmAboutUnit, frmFarmFileTypeUnit;



{$R *.dfm}
procedure TfrmFarm.HandleFarmBudget;
var
  FarmBudgetFile: TStringList;
  LineSplitter: TStringList;
  TimeUnits: String;
  BudgetItemIndex: integer;
  Description: string;
  AFarm: TDetailedFarmBudget;
  ItemIndex: Integer;
  ABudgetItem: TDetailedFarmBudgetItem;
  AValue: double;
  LineIndex: integer;
  FarmID: Integer;
  Time: double;
begin
  jvplSeriestypes.ActivePage := jvspFarmBudgets;
  pnlFarmBudgets.Visible := true;
  FFarms.Clear;
  clbIn.Clear;
  clbOut.Clear;
  clbInMinusOut.Clear;
  clbDiscrepancy.Clear;
  clbFarm.Clear;

  FDiscrepancyIndicies.Clear;
  FNetIndicies.Clear;
  FInIndicies.Clear;
  FOutIndicies.Clear;

  FarmBudgetFile := TStringList.Create;
  LineSplitter := TStringList.Create;
  try
    LineSplitter.Delimiter := ' ';
    FarmBudgetFile.LoadFromFile(dlgOpenFarm.FileName);
    if FarmBudgetFile.Count > 0 then
    begin
      LineSplitter.DelimitedText := FarmBudgetFile[0];
      TimeUnits := LineSplitter[2];
      chtfarmPlot.BottomAxis.Title.Caption := Format('Time (%s)', [TimeUnits]);
      for BudgetItemIndex := 4 to LineSplitter.Count-1 do
      begin
        Description := LineSplitter[BudgetItemIndex];
        if Pos('Discrepancy', Description) > 0 then
        begin
          FDiscrepancyIndicies.Add(BudgetItemIndex);
          clbDiscrepancy.Items.Add(Description);
        end
        else if Pos('-in-out', Description) > 0 then
        begin
          FNetIndicies.Add(BudgetItemIndex);
          clbInMinusOut.Items.Add(Description);
        end
        else if Pos('-in', Description) > 0 then
        begin
          FInIndicies.Add(BudgetItemIndex);
          clbIn.Items.Add(Description);
        end
        else if Pos('-out', Description) > 0 then
        begin
          FOutIndicies.Add(BudgetItemIndex);
          clbOut.Items.Add(Description);
        end
        else
        begin
          Assert(False);
        end;
      end;

      for LineIndex := 1 to FarmBudgetFile.Count-1 do
      begin
        LineSplitter.DelimitedText := FarmBudgetFile[LineIndex];
        if LineSplitter.Count > 0 then
        begin
          FarmID := StrToInt(LineSplitter[3]);
          Time := FortranStrToFloat(LineSplitter[2]);
          if FarmID - 1 < FFarms.Count then
          begin
            AFarm := FFarms[FarmID-1];
          end
          else
          begin
            AFarm := TDetailedFarmBudget.Create;
            FFarms.Add(AFarm);
            AFarm.FarmID := FarmID;
            clbFarm.Items.Add(IntToStr(FarmID));
            clbFarmAggregates.Items.Add(IntToStr(FarmID));
          end;

          ABudgetItem := TDetailedFarmBudgetItem.Create;
          ABudgetItem.FTime := Time;
          AFarm.Add(ABudgetItem);

          for ItemIndex := 0 to FDiscrepancyIndicies.Count-1 do
          begin
            BudgetItemIndex := FDiscrepancyIndicies[ItemIndex];
            AValue := FortranStrToFloat(LineSplitter[BudgetItemIndex]);
            ABudgetItem.FDiscrepancyItems.Add(AValue);
          end;

          for ItemIndex := 0 to FNetIndicies.Count-1 do
          begin
            BudgetItemIndex := FNetIndicies[ItemIndex];
            AValue := FortranStrToFloat(LineSplitter[BudgetItemIndex]);
            ABudgetItem.FNetItems.Add(AValue);
          end;

          for ItemIndex := 0 to FInIndicies.Count-1 do
          begin
            BudgetItemIndex := FInIndicies[ItemIndex];
            AValue := FortranStrToFloat(LineSplitter[BudgetItemIndex]);
            ABudgetItem.FInItems.Add(AValue);
          end;

          for ItemIndex := 0 to FOutIndicies.Count-1 do
          begin
            BudgetItemIndex := FOutIndicies[ItemIndex];
            AValue := FortranStrToFloat(LineSplitter[BudgetItemIndex]);
            ABudgetItem.FOutItems.Add(AValue);
          end;

        end;
      end;
    end;
  finally
    FarmBudgetFile.Free;
    LineSplitter.Free;
  end;
  CreateBudgetSeries;
  ChangeAllBudgetCheckBoxesInList(clbIn, cbIn.Checked);
  ChangeAllBudgetCheckBoxesInList(clbOut, cbOut.Checked);
  ChangeAllBudgetCheckBoxesInList(clbInMinusOut, cbNet.Checked);
  ChangeAllBudgetCheckBoxesInList(clbDiscrepancy, cbDiscrepancy.Checked);
  ChangeAllBudgetCheckBoxesInList(clbFarm, cbFarm.Checked);
end;


procedure TfrmFarm.miOpenClick(Sender: TObject);
var
  FarmFileType: TFarmFileType;

begin
  FarmFileType := fftNone;
  Inc(FBusy);
  try
    if dlgOpenFarm.Execute then
    begin
      case dlgOpenFarm.FilterIndex of
        1,2: FarmFileType := TFarmFileType(dlgOpenFarm.FilterIndex);
      else
        frmFarmFileType.ShowModal;
        FarmFileType := TFarmFileType(frmFarmFileType.rgFarmFileType.ItemIndex+1);
        if FarmFileType = fftNone then
        begin
          Beep;
          MessageDlg('You must specify a file type that GW_Chart can read.', mtInformation, [mbOK], 0);
        end;
      end;

      case FarmFileType of
        fftNone:
          begin
            Exit;
          end;
        fftBudget:
          begin
            HandleFarmBudget;
          end;
        fftSupplyAndDemand:
          begin
            HandleFarmDemandSupply
          end;
      else
        Assert(False);
      end;
    end;
  finally
    Dec(FBusy)
  end;

  case FarmFileType of
    fftNone:
      begin
        Exit;
      end;
    fftBudget:
      begin
        DisplayFarmBudgetSeries;
      end;
    fftSupplyAndDemand:
      begin
        DisplayFarmDemandSeries;
      end;
  else
    Assert(False);
  end;


end;

{ TDetailedFarmBudgetItem }

constructor TDetailedFarmBudgetItem.Create;
begin
  FInItems := TRealList.Create;
  FOutItems := TRealList.Create;
  FNetItems := TRealList.Create;
  FDiscrepancyItems := TRealList.Create;
end;

destructor TDetailedFarmBudgetItem.Destroy;
begin
  FInItems.Free;
  FOutItems.Free;
  FNetItems.Free;
  FDiscrepancyItems.Free;
  inherited;
end;

{ TDetailedFarmBudget }

function TDetailedFarmBudget.Add(
  BudgetItem: TDetailedFarmBudgetItem): integer;
begin
  result := inherited Add(BudgetItem);
end;

function TDetailedFarmBudget.GetItems(
  Index: Integer): TDetailedFarmBudgetItem;
begin
  result := inherited Items[index] as TDetailedFarmBudgetItem;
end;

procedure TDetailedFarmBudget.SetFarmID(const Value: Integer);
begin
  FFarmID := Value;
end;

procedure TDetailedFarmBudget.SetItems(Index: Integer;
  const Value: TDetailedFarmBudgetItem);
begin
  inherited Items[index] := Value;
end;

{ TDetailedFarmBudgets }

function TDetailedFarmBudgets.Add(
  BudgetList: TDetailedFarmBudget): integer;
begin
  result := inherited Add(BudgetList);
end;

function TDetailedFarmBudgets.GetItems(
  Index: Integer): TDetailedFarmBudget;
begin
  result := inherited Items[Index] as TDetailedFarmBudget;
end;

procedure TDetailedFarmBudgets.SetItems(Index: Integer;
  const Value: TDetailedFarmBudget);
begin
  inherited Items[Index] := Value;
end;

procedure TfrmFarm.FormCreate(Sender: TObject);
begin
  jvplSeriestypes.ActivePageIndex := 0;
  // farm budgets
  FFarms := TDetailedFarmBudgets.Create;
  FInIndicies := TIntegerList.Create;
  FOutIndicies := TIntegerList.Create;
  FNetIndicies := TIntegerList.Create;
  FDiscrepancyIndicies := TIntegerList.Create;
  FSeriesList := TSeriesList.Create;
  FAggregateSeriesList := TSeriesList.Create;

  // farm supply/deficiency budgets
  FarmSupplyDeficiencies := TFarmSupplyDeficiencies.Create;
  FInitialFdsIndicies := TIntegerList.Create;
  FFinalFdsIndicies := TIntegerList.Create;
  FOfeSeriesList := TSeriesList.Create;
  FDefFlagSeriesList := TSeriesList.Create;


end;

procedure TfrmFarm.FormDestroy(Sender: TObject);
begin
  FFarms.Free;
  FInIndicies.Free;
  FOutIndicies.Free;
  FNetIndicies.Free;
  FDiscrepancyIndicies.Free;
  FSeriesList.Free;
  FAggregateSeriesList.Free;

  FarmSupplyDeficiencies.Free;
  FInitialFdsIndicies.Free;
  FFinalFdsIndicies.Free;
  FOfeSeriesList.Free;
  FDefFlagSeriesList.Free;
end;

{ TSeriesList }

function TSeriesList.Add(ASeries: TLineSeries): integer;
begin
  result := inherited Add(ASeries);
end;

function TSeriesList.GetItems(Index: Integer): TLineSeries;
begin
  Result := inherited Items[index] as TLineSeries;
end;

procedure TSeriesList.SetItems(Index: Integer; const Value: TLineSeries);
begin
  inherited Items[index] := Value;
end;

resourcestring
  rsSeriesID = '%0:s: Farm %1:d';
  rsAggregateSeries = 'Aggregate %0:s';

procedure TfrmFarm.ClearAllSeries;
begin
  FSeriesList.Clear;
  FAggregateSeriesList.Clear;
  FOfeSeriesList.Clear;
  FDefFlagSeriesList.Clear;
end;

procedure TfrmFarm.CreateBudgetSeries;
var
  FarmIndex: integer;
  AFarm: TDetailedFarmBudget;
  ItemIndex: integer;
  ADescription: String;
  ALineSeries: TLineSeries;
  TimeIndex: integer;
  ABudgetItem: TDetailedFarmBudgetItem;
  ColorIndex: integer;
  LineType: TPenStyle;
  Style: TSeriesPointerStyle;
//  Color: TColor;
begin
  ClearAllSeries;
  ColorIndex := 0;
  LineType := Low(TPenStyle);
  Style := Low(TSeriesPointerStyle);
  for FarmIndex := 0 to FFarms.Count - 1 do
  begin
    AFarm := FFarms[FarmIndex];
    for ItemIndex := 0 to clbIn.Items.Count -1 do
    begin
      ADescription :=
        Format(rsSeriesID, [clbIn.Items[ItemIndex], AFarm.FarmID]);
      ALineSeries := TLineSeries.Create(nil);
      FSeriesList.Add(ALineSeries);
      ALineSeries.ParentChart := chtfarmPlot;
      ALineSeries.Title := ADescription;
      ALineSeries.Color := ColorPalette[ColorIndex];
      IncrementColorIndex(ColorIndex);
      ALineSeries.Pointer.Style := Style;
      ALineSeries.LinePen.Style := LineType;
      IncrementLineType(LineType);
      IncrementStyle(Style);
      ALineSeries.Pointer.Visible := True;


      for TimeIndex := 0 to AFarm.Count -1 do
      begin
        ABudgetItem :=  AFarm[TimeIndex];
        ALineSeries.AddXY(ABudgetItem.FTime, ABudgetItem.FInItems[ItemIndex]);
      end;
    end;

    for ItemIndex := 0 to clbOut.Items.Count -1 do
    begin
      ADescription :=
        Format(rsSeriesID, [clbOut.Items[ItemIndex], AFarm.FarmID]);
      ALineSeries := TLineSeries.Create(nil);
      FSeriesList.Add(ALineSeries);
      ALineSeries.ParentChart := chtfarmPlot;
      ALineSeries.Title := ADescription;
      ALineSeries.Color := ColorPalette[ColorIndex];
      IncrementColorIndex(ColorIndex);
      ALineSeries.Pointer.Style := Style;
      ALineSeries.LinePen.Style := LineType;
      IncrementLineType(LineType);
      IncrementStyle(Style);
      ALineSeries.Pointer.Visible := True;

      for TimeIndex := 0 to AFarm.Count -1 do
      begin
        ABudgetItem :=  AFarm[TimeIndex];
        ALineSeries.AddXY(ABudgetItem.FTime, ABudgetItem.FOutItems[ItemIndex]);
      end;
    end;

    for ItemIndex := 0 to clbInMinusOut.Items.Count -1 do
    begin
      ADescription :=
        Format(rsSeriesID, [clbInMinusOut.Items[ItemIndex], AFarm.FarmID]);
      ALineSeries := TLineSeries.Create(nil);
      FSeriesList.Add(ALineSeries);
      ALineSeries.ParentChart := chtfarmPlot;
      ALineSeries.Title := ADescription;
      ALineSeries.Color := ColorPalette[ColorIndex];
      IncrementColorIndex(ColorIndex);
      ALineSeries.Pointer.Style := Style;
      ALineSeries.LinePen.Style := LineType;
      IncrementLineType(LineType);
      IncrementStyle(Style);
      ALineSeries.Pointer.Visible := True;

      for TimeIndex := 0 to AFarm.Count -1 do
      begin
        ABudgetItem :=  AFarm[TimeIndex];
        ALineSeries.AddXY(ABudgetItem.FTime, ABudgetItem.FNetItems[ItemIndex]);
      end;
    end;

    for ItemIndex := 0 to clbDiscrepancy.Items.Count -1 do
    begin
      ADescription :=
        Format(rsSeriesID, [clbDiscrepancy.Items[ItemIndex], AFarm.FarmID]);
      ALineSeries := TLineSeries.Create(nil);
      FSeriesList.Add(ALineSeries);
      ALineSeries.ParentChart := chtfarmPlot;
      ALineSeries.Title := ADescription;
      ALineSeries.Color := ColorPalette[ColorIndex];
      IncrementColorIndex(ColorIndex);
      ALineSeries.Pointer.Style := Style;
      ALineSeries.LinePen.Style := LineType;
      IncrementLineType(LineType);
      IncrementStyle(Style);
      ALineSeries.Pointer.Visible := True;

      for TimeIndex := 0 to AFarm.Count -1 do
      begin
        ABudgetItem :=  AFarm[TimeIndex];
        ALineSeries.AddXY(ABudgetItem.FTime, ABudgetItem.FDiscrepancyItems[ItemIndex]);
      end;
    end;
  end;

  for ItemIndex := 0 to clbIn.Items.Count -1 do
  begin
    ADescription :=
      Format(rsAggregateSeries, [clbIn.Items[ItemIndex]]);
    ALineSeries := TLineSeries.Create(nil);
    FAggregateSeriesList.Add(ALineSeries);
    ALineSeries.ParentChart := chtfarmPlot;
    ALineSeries.Title := ADescription;
    ALineSeries.Color := ColorPalette[ColorIndex];
    IncrementColorIndex(ColorIndex);
    ALineSeries.Pointer.Style := Style;
    ALineSeries.LinePen.Style := LineType;
    IncrementLineType(LineType);
    IncrementStyle(Style);
    ALineSeries.Pointer.Visible := True;
  end;

  for ItemIndex := 0 to clbOut.Items.Count -1 do
  begin
    ADescription :=
      Format(rsAggregateSeries, [clbOut.Items[ItemIndex]]);
    ALineSeries := TLineSeries.Create(nil);
    FAggregateSeriesList.Add(ALineSeries);
    ALineSeries.ParentChart := chtfarmPlot;
    ALineSeries.Title := ADescription;
    ALineSeries.Color := ColorPalette[ColorIndex];
    IncrementColorIndex(ColorIndex);
    ALineSeries.Pointer.Style := Style;
    ALineSeries.LinePen.Style := LineType;
    IncrementLineType(LineType);
    IncrementStyle(Style);
    ALineSeries.Pointer.Visible := True;
  end;

  for ItemIndex := 0 to clbInMinusOut.Items.Count -1 do
  begin
    ADescription :=
      Format(rsAggregateSeries, [clbInMinusOut.Items[ItemIndex]]);
    ALineSeries := TLineSeries.Create(nil);
    FAggregateSeriesList.Add(ALineSeries);
    ALineSeries.ParentChart := chtfarmPlot;
    ALineSeries.Title := ADescription;
    ALineSeries.Color := ColorPalette[ColorIndex];
    IncrementColorIndex(ColorIndex);
    ALineSeries.Pointer.Style := Style;
    ALineSeries.LinePen.Style := LineType;
    IncrementLineType(LineType);
    IncrementStyle(Style);
    ALineSeries.Pointer.Visible := True;
  end;

  for ItemIndex := 0 to clbDiscrepancy.Items.Count -1 do
  begin
    ADescription :=
      Format(rsAggregateSeries, [clbDiscrepancy.Items[ItemIndex]]);
    ALineSeries := TLineSeries.Create(nil);
    FAggregateSeriesList.Add(ALineSeries);
    ALineSeries.ParentChart := chtfarmPlot;
    ALineSeries.Title := ADescription;
    ALineSeries.Color := ColorPalette[ColorIndex];
    IncrementColorIndex(ColorIndex);
    ALineSeries.Pointer.Style := Style;
    ALineSeries.LinePen.Style := LineType;
    IncrementLineType(LineType);
    IncrementStyle(Style);
    ALineSeries.Pointer.Visible := True;
  end;

  FillAggregateSeries;

  ExplantionVisible := FSeriesList.Count > 0;

end;

procedure TfrmFarm.DisplayFarmBudgetSeries;
var
  CheckLists: TList;
  SeriesIndex: integer;
  FarmIndex: integer;
  CheckListIndex: integer;
  ItemIndex: integer;
  ACheckList: TJvCheckListBox;
  ASeries: TLineSeries;
begin
  if FBusy > 0 then
  begin
    Exit;
  end;

  CheckLists := TList.Create;
  try
    CheckLists.Add(clbIn);
    CheckLists.Add(clbOut);
    CheckLists.Add(clbInMinusOut);
    CheckLists.Add(clbDiscrepancy);
    SeriesIndex := 0;
    for FarmIndex := 0 to clbFarm.Items.Count -1 do
    begin
      for CheckListIndex := 0 to CheckLists.Count -1 do
      begin
        ACheckList := CheckLists[CheckListIndex];
        for ItemIndex := 0 to ACheckList.Count - 1 do
        begin
          ASeries := FSeriesList[SeriesIndex];
          Inc(SeriesIndex);
          ASeries.Visible := clbFarm.Checked[FarmIndex]
            and ACheckList.Checked[ItemIndex];
        end;
      end;
    end;

    SeriesIndex := 0;
    for CheckListIndex := 0 to CheckLists.Count -1 do
    begin
      ACheckList := CheckLists[CheckListIndex];
      for ItemIndex := 0 to ACheckList.Count - 1 do
      begin
        ASeries := FAggregateSeriesList[SeriesIndex];
        Inc(SeriesIndex);
        ASeries.Visible := ACheckList.Checked[ItemIndex]
          and (ASeries.Count > 0);
      end;
    end;
  finally
    CheckLists.Free;
  end;



end;

procedure TfrmFarm.clbInClickCheck(Sender: TObject);
begin
  DisplayFarmBudgetSeries;
end;

procedure TfrmFarm.clbOutClickCheck(Sender: TObject);
begin
  DisplayFarmBudgetSeries;
end;

procedure TfrmFarm.clbInMinusOutClickCheck(Sender: TObject);
begin
  DisplayFarmBudgetSeries;
end;

procedure TfrmFarm.clbDiscrepancyClickCheck(Sender: TObject);
begin
  DisplayFarmBudgetSeries;
end;

procedure TfrmFarm.clbFarmClickCheck(Sender: TObject);
begin
  FillAggregateSeries;
  DisplayFarmBudgetSeries;
end;

procedure TfrmFarm.FormShow(Sender: TObject);
begin
  mmFarm.Merge(frmModChart.mainMenuFormChoice);
end;

procedure TfrmFarm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Visible then
  begin
    frmModChart.Close
  end;
end;

procedure TfrmFarm.ChangeAllBudgetCheckBoxesInList(CB: TJvCheckListBox;
  Checked: Boolean);
begin
  Inc(FBusy);
  try
    if Checked then
    begin
      CB.CheckAll
    end
    else
    begin
      CB.UnCheckAll
    end;
  finally
    Dec(FBusy);
  end;
  DisplayFarmBudgetSeries;

end;

procedure TfrmFarm.cbInClick(Sender: TObject);
begin
  ChangeAllBudgetCheckBoxesInList(clbIn, cbIn.Checked);
end;

procedure TfrmFarm.cbOutClick(Sender: TObject);
begin
  ChangeAllBudgetCheckBoxesInList(clbOut, cbOut.Checked);
end;

procedure TfrmFarm.cbNetClick(Sender: TObject);
begin
  ChangeAllBudgetCheckBoxesInList(clbInMinusOut, cbNet.Checked);
end;

procedure TfrmFarm.cbDiscrepancyClick(Sender: TObject);
begin
  ChangeAllBudgetCheckBoxesInList(clbDiscrepancy, cbDiscrepancy.Checked);
end;

procedure TfrmFarm.cbFarmClick(Sender: TObject);
begin
  ChangeAllBudgetCheckBoxesInList(clbFarm, cbFarm.Checked);
end;

procedure TfrmFarm.miExit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmFarm.miFormatChart1Click(Sender: TObject);
begin
  mHHelp.ChmFile := ChartHelpFileName;
  try
    ChartEditor1.Execute;
  finally
    mHHelp.ChmFile := HelpFileName;
  end;
end;

procedure TfrmFarm.chtFarmPlotGetLegendRect(Sender: TCustomChart;
  var Rect: TRect);
var
  ExplanationWidth : integer;
  ExplanationHeight : integer;
  TempFont : TFont;
begin
  LegendRect := Rect;
  ExplanationTop := Rect.Top + 5;
  ExplanationLeft := Rect.Left + 5;
  TempFont := TFont.Create;
  try
    TempFont.Assign(chtfarmPlot.Canvas.Font);
    try
      chtfarmPlot.Canvas.Font.Assign(chtfarmPlot.Legend.Font);
      ExplanationWidth := chtfarmPlot.Canvas.TextWidth(StrExplanationUnderscore);
      ExplanationHeight := chtfarmPlot.Canvas.TextHeight(StrExplanation);
    finally
      chtfarmPlot.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
//  FXOffset := Rect.Left;
  if Rect.Right - Rect.Left < ExplanationWidth then
  begin
    Rect.Right := Rect.Left + ExplanationWidth;
  end;
//  if Rect.Right > chtFarmPlot.Width - 20 then
//  begin
//    Rect.Right := chtFarmPlot.Width - 20;
//    if Rect.Right - Rect.Left < ExplanationWidth then
//    begin
//      Rect.Left := Rect.Right - ExplanationWidth;
//    end;
//  end;
//  FXOffset := Rect.Left - FXOffset ;

  Rect.Bottom := Rect.Bottom + ExplanationHeight;
  if Rect.Right + 20 > chtFarmPlot.Width then
  begin
    DeltaLegendX := Rect.Right - chtFarmPlot.Width + 20;
    Rect.Left := Rect.Left - DeltaLegendX;
    Rect.Right := Rect.Right - DeltaLegendX;
    ExplanationLeft := ExplanationLeft - DeltaLegendX;
  end
  else
  begin
    DeltaLegendX := 0
  end;
end;

procedure TfrmFarm.chtFarmPlotGetLegendPos(Sender: TCustomChart;
  Index: Integer; var X, Y, XColor: Integer);
var
  ExplanationHeight : integer;
  TempFont : TFont;
  ASize: TSize;
//  SymbolSize: integer;
begin
  TempFont := TFont.Create;
  try
    TempFont.Assign(chtFarmPlot.Canvas.Font);
    try
      chtFarmPlot.Canvas.Font.Assign(chtFarmPlot.Legend.Font);
      ASize.cy := chtFarmPlot.Canvas.TextHeight(StrExplanation);
      ASize.cx := chtFarmPlot.Canvas.TextWidth(StrExplanation);
      ExplanationHeight := ASize.cy;
    finally
      chtFarmPlot.Canvas.Font.Assign(TempFont);
    end;
  finally
    TempFont.Free;
  end;
  Y := Y + ExplanationHeight;
//  if (FileType = ft_xyztwr) and rgDataToPlot.Enabled then
//  begin
//    chartModflowGetSymbolSize(Sender, Index, SymbolSize);
//    Y := Y + SymbolSize;
//    X := X - DeltaLegendX;
//    XColor := XColor - DeltaLegendX;
//  end;
//  X := X + FXOffset;
//  if X + ASize.cx + 20 > chtFarmPlot.Width then
//  begin
//    X := chtFarmPlot.Width - ASize.cx - 20
//  end;

end;

procedure TfrmFarm.chtFarmPlotAfterDraw(Sender: TObject);
var
  TempFont : TFont;
begin
  if ExplantionVisible and chtFarmPlot.Legend.Visible then
  begin

    TempFont := TFont.Create;
    try
      TempFont.Assign(chtFarmPlot.Canvas.Font);
      try
        chtFarmPlot.Canvas.Font.Assign(chtFarmPlot.Legend.Font);
        chtFarmPlot.Canvas.TextOut(ExplanationLeft,ExplanationTop,
          StrExplanation);
        
      finally
        chtFarmPlot.Canvas.Font.Assign(TempFont);
      end;

    finally
      TempFont.Free;
    end;

  end;
end;

procedure TfrmFarm.miPrintSetup1Click(Sender: TObject);
begin
  dlgPntSet1.Execute;
end;

procedure TfrmFarm.miPrintPreview1Click(Sender: TObject);
begin
  ChartPreviewer1.Execute;
end;

procedure TfrmFarm.miPrint1Click(Sender: TObject);
var
  Colors : array of TColor;
  Index : integer;
  ASeries : TChartSeries;
  PrintInBW : boolean;
begin
  PrintInBW := False;
  if FSeriesList.Count > 0 then
  begin
    If MessageDlg('Do you wish to print in Black and White?', mtInformation,
      [mbYes, mbNo], 0) = mrYes then
      begin
        PrintInBW := True;
        SetLength(Colors, FSeriesList.Count);
        for Index := 0 to FSeriesList.Count -1 do
        begin
          ASeries := FSeriesList[Index] as TLineSeries;
          Colors[Index] := ASeries.SeriesColor;
          ASeries.SeriesColor := clBlack;
        end;
      end;
  end;
  chtFarmPlot.print;
  if PrintInBW then
  begin
    for Index := 0 to FSeriesList.Count -1 do
    begin
      ASeries := FSeriesList[Index] as TLineSeries;
      ASeries.SeriesColor := Colors[Index];
    end;
  end;
end;

procedure TfrmFarm.miSaveasImage1Click(Sender: TObject);
begin
  if sd1.Execute then
  begin
    if LowerCase(ExtractFileExt(sd1.FileName)) = '.bmp' then
    begin
      chtFarmPlot.SaveToBitmapFile(sd1.FileName);
    end
    else if LowerCase(ExtractFileExt(sd1.FileName)) = '.emf' then
    begin
      chtFarmPlot.SaveToMetafileEnh(sd1.FileName);
    end
    else if LowerCase(ExtractFileExt(sd1.FileName)) = '.wmf' then
    begin
      chtFarmPlot.SaveToMetafile(sd1.FileName);
    end;
  end;

end;

procedure TfrmFarm.cbFarmAggregatesClick(Sender: TObject);
begin
  Inc(FBusy);
  try
    ChangeAllBudgetCheckBoxesInList(clbFarmAggregates, cbFarmAggregates.Checked);
    FillAggregateSeries;
  finally
    Dec(FBusy);
  end;
  DisplayFarmBudgetSeries;
end;

procedure TfrmFarm.FillAggregateSeries;
var
  SeriesIndex: integer;
  ItemIndex: Integer;
  ALineSeries: TLineSeries;
  FarmIndex: integer;
  AFarm: TDetailedFarmBudget;
  LocalFarms: TList;
  ABudgetItem: TDetailedFarmBudgetItem;
  TimeIndex: integer;
  Time: Double;
  AValue: double;
begin
  Inc(FBusy);
  try
    for SeriesIndex := 0 to FAggregateSeriesList.Count - 1 do
    begin
      ALineSeries := FAggregateSeriesList[SeriesIndex];
      ALineSeries.Clear;
    end;

    LocalFarms := TList.Create;
    try
      for FarmIndex := 0 to clbFarmAggregates.Items.Count -1 do
      begin
        if clbFarmAggregates.Checked[FarmIndex] then
        begin
          AFarm := FFarms[FarmIndex];
          LocalFarms.Add(AFarm);
        end;
      end;

      if LocalFarms.Count = 0 then
      begin
        Exit;
      end;

      SeriesIndex := 0;


      for ItemIndex := 0 to clbIn.Items.Count -1 do
      begin
        ALineSeries := FAggregateSeriesList[SeriesIndex];
        Inc(SeriesIndex);
        ALineSeries.Visible := clbIn.Checked[ItemIndex];

        AFarm := LocalFarms[0];
        for TimeIndex := 0 to AFarm.Count -1 do
        begin
          ABudgetItem := AFarm[TimeIndex];
          Time := ABudgetItem.FTime;
          AValue := ABudgetItem.FInItems[ItemIndex];
          for FarmIndex := 1 to LocalFarms.Count -1 do
          begin
            AFarm := LocalFarms[FarmIndex];
            ABudgetItem := AFarm[TimeIndex];
            AValue := AValue + ABudgetItem.FInItems[ItemIndex];
          end;
          ALineSeries.AddXY(Time, AValue)
        end;
      end;

      for ItemIndex := 0 to clbOut.Items.Count -1 do
      begin
        ALineSeries := FAggregateSeriesList[SeriesIndex];
        Inc(SeriesIndex);
        ALineSeries.Visible := clbOut.Checked[ItemIndex];

        AFarm := LocalFarms[0];
        for TimeIndex := 0 to AFarm.Count -1 do
        begin
          ABudgetItem := AFarm[TimeIndex];
          Time := ABudgetItem.FTime;
          AValue := ABudgetItem.FOutItems[ItemIndex];
          for FarmIndex := 1 to LocalFarms.Count -1 do
          begin
            AFarm := LocalFarms[FarmIndex];
            ABudgetItem := AFarm[TimeIndex];
            AValue := AValue + ABudgetItem.FOutItems[ItemIndex];
          end;
          ALineSeries.AddXY(Time, AValue)
        end;
      end;

      for ItemIndex := 0 to clbInMinusOut.Items.Count -1 do
      begin
        ALineSeries := FAggregateSeriesList[SeriesIndex];
        Inc(SeriesIndex);
        ALineSeries.Visible := clbInMinusOut.Checked[ItemIndex];

        AFarm := LocalFarms[0];
        for TimeIndex := 0 to AFarm.Count -1 do
        begin
          ABudgetItem := AFarm[TimeIndex];
          Time := ABudgetItem.FTime;
          AValue := ABudgetItem.FNetItems[ItemIndex];
          for FarmIndex := 1 to LocalFarms.Count -1 do
          begin
            AFarm := LocalFarms[FarmIndex];
            ABudgetItem := AFarm[TimeIndex];
            AValue := AValue + ABudgetItem.FNetItems[ItemIndex];
          end;
          ALineSeries.AddXY(Time, AValue)
        end;
      end;

      for ItemIndex := 0 to clbDiscrepancy.Items.Count -1 do
      begin
        ALineSeries := FAggregateSeriesList[SeriesIndex];
        Inc(SeriesIndex);
        ALineSeries.Visible := clbDiscrepancy.Checked[ItemIndex];

        AFarm := LocalFarms[0];
        for TimeIndex := 0 to AFarm.Count -1 do
        begin
          ABudgetItem := AFarm[TimeIndex];
          Time := ABudgetItem.FTime;
          AValue := ABudgetItem.FDiscrepancyItems[ItemIndex];
          for FarmIndex := 1 to LocalFarms.Count -1 do
          begin
            AFarm := LocalFarms[FarmIndex];
            ABudgetItem := AFarm[TimeIndex];
            AValue := AValue + ABudgetItem.FDiscrepancyItems[ItemIndex];
          end;
          ALineSeries.AddXY(Time, AValue/LocalFarms.Count)
        end;
      end;

    finally
      LocalFarms.Free;
    end;
  finally
    Dec(FBusy);
  end;
end;

procedure TfrmFarm.miHelp2Click(Sender: TObject);
begin
  inherited;
  Application.HelpContext(HelpContext);
end;

procedure TfrmFarm.miAbout1Click(Sender: TObject);
begin
  inherited;
  frmAbout.ShowModal;
end;

procedure TfrmFarm.HandleFarmDemandSupply;
var
  FarmFile: TStringList;
  LineSplitter: TStringList;
  TimeUnits: string;
  BudgetItemIndex: integer;
  Description: String;
  LineIndex: integer;
  FarmID: integer;
  Farm: TFarmSupplyDeficiency;
  Time: double;
  FarmItem: TFarmSupplyDeficiencyItem;
  ItemIndex: integer;
begin
  jvplSeriestypes.ActivePage := jvspDemandSupply;
  pnlFarmBudgets.Visible := False;

  FFarmIdIndex := -1;
  FOfeIndex := -1;
  FDefFlagIndex := -1;
  FInitialFdsIndicies.Clear;
  FFinalFdsIndicies.Clear;
  FarmSupplyDeficiencies.Clear;
  clbDefInitial.Items.Clear;
  clbDefFinal.Items.Clear;
  clbFarmsSupplyDef.Items.Clear;

  FarmFile := TStringList.Create;
  LineSplitter := TStringList.Create;
  try
    LineSplitter.Delimiter := ' ';
    FarmFile.LoadFromFile(dlgOpenFarm.FileName);
    if FarmFile.Count > 0 then
    begin
      LineSplitter.DelimitedText := FarmFile[0];
      TimeUnits := LineSplitter[2];
      chtfarmPlot.BottomAxis.Title.Caption := Format('Time (%s)', [TimeUnits]);
      for BudgetItemIndex := 3 to LineSplitter.Count-1 do
      begin
        Description := LineSplitter[BudgetItemIndex];
        if Description = 'FID' then
        begin
          FFarmIdIndex := BudgetItemIndex;
        end
        else if Description = 'OFE' then
        begin
          FOfeIndex := BudgetItemIndex;
        end
        else if Description = 'DEF-FLAG' then
        begin
          FDefFlagIndex := BudgetItemIndex;
        end
        else if Pos('-INI', Description) > 0 then
        begin
          FInitialFdsIndicies.Add(BudgetItemIndex);
          clbDefInitial.Items.Add(Description);
        end
        else if Pos('-FIN', Description) > 0 then
        begin
          FFinalFdsIndicies.Add(BudgetItemIndex);
          clbDefFinal.Items.Add(Description);
        end
        else
        begin
          Assert(False);
        end;
      end;

      Assert(FFarmIdIndex >= 0);
      Assert(FOfeIndex >= 0);
      Assert(FDefFlagIndex >= 0);
      Assert(FInitialFdsIndicies.Count > 0);
      Assert(FFinalFdsIndicies.Count > 0);


      for LineIndex := 1 to FarmFile.Count -1 do
      begin
        LineSplitter.DelimitedText := FarmFile[LineIndex];
        if LineSplitter.Count > 0 then
        begin
          FarmID := StrToInt(LineSplitter[FFarmIdIndex]);

          if FarmID - 1 < FarmSupplyDeficiencies.Count then
          begin
            Farm := FarmSupplyDeficiencies[FarmID - 1];
          end
          else
          begin
            Farm := TFarmSupplyDeficiency.Create;
            FarmSupplyDeficiencies.Add(Farm);
            Farm.FarmID := FarmID;
            clbFarmsSupplyDef.Items.Add(IntToStr(FarmID));
          end;

          Time := FortranStrToFloat(LineSplitter[2]);
          FarmItem := TFarmSupplyDeficiencyItem.Create;
          Farm.Add(FarmItem);
          FarmItem.FTime := Time;
          FarmItem.FOfe := FortranStrToFloat(LineSplitter[FOfeIndex]);
          FarmItem.FDeficiencyFlag := StrToInt(LineSplitter[FDefFlagIndex]);
          for ItemIndex := 0 to FInitialFdsIndicies.count -1 do
          begin
            FarmItem.FInitialItems.Add(
              FortranStrToFloat(LineSplitter[FInitialFdsIndicies[ItemIndex]]));
          end;
          for ItemIndex := 0 to FFinalFdsIndicies.count -1 do
          begin
            FarmItem.FFinalItems.Add(
              FortranStrToFloat(LineSplitter[FFinalFdsIndicies[ItemIndex]]));
          end;
        end;
      end;

    end;
  finally
    FarmFile.Free;
    LineSplitter.Free;
  end;
  CreateFarmDemandSupplySeries;
  ChangeAllDemandCheckBoxesInList(clbFarmsSupplyDef, cbFarmsSupplyDef.Checked);
  ChangeAllDemandCheckBoxesInList(clbDefInitial, cbDefInitial.Checked);
  ChangeAllDemandCheckBoxesInList(clbDefFinal, cbDefFinal.Checked);

end;

{ TFarmSupplyDeficiencyItem }

constructor TFarmSupplyDeficiencyItem.Create;
begin
  inherited;
  FInitialItems := TRealList.Create;
  FFinalItems := TRealList.Create;
end;

destructor TFarmSupplyDeficiencyItem.Destroy;
begin
  FInitialItems.Free;
  FFinalItems.Free;
  inherited;
end;

{ TFarmSupplyDeficiency }

function TFarmSupplyDeficiency.Add(
  SupplyDeficiencyItem: TFarmSupplyDeficiencyItem): integer;
begin
  result := inherited Add(SupplyDeficiencyItem);
end;

constructor TFarmSupplyDeficiency.Create;
begin
  inherited;
  FInitialSeries := TSeriesList.Create(False);
  FFinalSeries := TSeriesList.Create(False);
end;

destructor TFarmSupplyDeficiency.Destroy;
begin
  FInitialSeries.Free;
  FFinalSeries.Free;
  inherited;
end;

function TFarmSupplyDeficiency.GetItems(
  Index: Integer): TFarmSupplyDeficiencyItem;
begin
  result := inherited Items[index] as TFarmSupplyDeficiencyItem;
end;

procedure TFarmSupplyDeficiency.SetFarmID(const Value: Integer);
begin
  FFarmID := Value;
end;

procedure TFarmSupplyDeficiency.SetItems(Index: Integer;
  const Value: TFarmSupplyDeficiencyItem);
begin
  inherited Items[index] := Value;
end;

{ TFarmSupplyDeficiencies }

function TFarmSupplyDeficiencies.Add(
  FarmSupplyDeficiency: TFarmSupplyDeficiency): integer;
begin
  result := inherited Add(FarmSupplyDeficiency);
end;

function TFarmSupplyDeficiencies.GetItems(
  Index: Integer): TFarmSupplyDeficiency;
begin
  result := inherited Items[index] as TFarmSupplyDeficiency;
end;

procedure TFarmSupplyDeficiencies.SetItems(Index: Integer;
  const Value: TFarmSupplyDeficiency);
begin
  inherited Items[index] := Value;
end;

procedure TfrmFarm.CreateFarmDemandSupplySeries;
var
  ColorIndex: integer;
  LineType: TPenStyle;
  Style: TSeriesPointerStyle;
  FarmIndex: integer;
  AFarm: TFarmSupplyDeficiency;
  ADescription: String;
  ALineSeries: TLineSeries;
  TimeIndex: integer;
  ABudgetItem: TFarmSupplyDeficiencyItem;
  ItemIndex: Integer;
begin
  ClearAllSeries;
  for FarmIndex := 0 to FarmSupplyDeficiencies.Count - 1 do
  begin
    AFarm := FarmSupplyDeficiencies[FarmIndex];

    ADescription :=
      Format(rsSeriesID, [cbOFE.Caption, AFarm.FarmID]);
    ALineSeries := TLineSeries.Create(nil);
    FOfeSeriesList.Add(ALineSeries);
    ALineSeries.ParentChart := chtfarmPlot;
    ALineSeries.Title := ADescription;
    ALineSeries.Color := ColorPalette[ColorIndex];
    IncrementColorIndex(ColorIndex);
    ALineSeries.Pointer.Style := Style;
    ALineSeries.LinePen.Style := LineType;
    IncrementLineType(LineType);
    IncrementStyle(Style);
    ALineSeries.Pointer.Visible := True;

    for TimeIndex := 0 to AFarm.Count -1 do
    begin
      ABudgetItem :=  AFarm[TimeIndex];
      ALineSeries.AddXY(ABudgetItem.FTime, ABudgetItem.FOfe);
    end;

    ADescription :=
      Format(rsSeriesID, [cbDefFlag.Caption, AFarm.FarmID]);
    ALineSeries := TLineSeries.Create(nil);
    FDefFlagSeriesList.Add(ALineSeries);
    ALineSeries.ParentChart := chtfarmPlot;
    ALineSeries.Title := ADescription;
    ALineSeries.Color := ColorPalette[ColorIndex];
    IncrementColorIndex(ColorIndex);
    ALineSeries.Pointer.Style := Style;
    ALineSeries.LinePen.Style := LineType;
    IncrementLineType(LineType);
    IncrementStyle(Style);
    ALineSeries.Pointer.Visible := True;

    for TimeIndex := 0 to AFarm.Count -1 do
    begin
      ABudgetItem :=  AFarm[TimeIndex];
      ALineSeries.AddXY(ABudgetItem.FTime, ABudgetItem.FDeficiencyFlag);
    end;

    for ItemIndex := 0 to clbDefInitial.Items.Count -1 do
    begin
      ADescription :=
        Format(rsSeriesID, [clbDefInitial.Items[ItemIndex], AFarm.FarmID]);
      ALineSeries := TLineSeries.Create(nil);
      FSeriesList.Add(ALineSeries);
      AFarm.FInitialSeries.Add(ALineSeries);
      ALineSeries.ParentChart := chtfarmPlot;
      ALineSeries.Title := ADescription;
      ALineSeries.Color := ColorPalette[ColorIndex];
      IncrementColorIndex(ColorIndex);
      ALineSeries.Pointer.Style := Style;
      ALineSeries.LinePen.Style := LineType;
      IncrementLineType(LineType);
      IncrementStyle(Style);
      ALineSeries.Pointer.Visible := True;

      for TimeIndex := 0 to AFarm.Count -1 do
      begin
        ABudgetItem :=  AFarm[TimeIndex];
        ALineSeries.AddXY(ABudgetItem.FTime, ABudgetItem.FInitialItems[ItemIndex]);
      end;
    end;

    for ItemIndex := 0 to clbDefFinal.Items.Count -1 do
    begin
      ADescription :=
        Format(rsSeriesID, [clbDefFinal.Items[ItemIndex], AFarm.FarmID]);
      ALineSeries := TLineSeries.Create(nil);
      FSeriesList.Add(ALineSeries);
      AFarm.FFinalSeries.Add(ALineSeries);
      ALineSeries.ParentChart := chtfarmPlot;
      ALineSeries.Title := ADescription;
      ALineSeries.Color := ColorPalette[ColorIndex];
      IncrementColorIndex(ColorIndex);
      ALineSeries.Pointer.Style := Style;
      ALineSeries.LinePen.Style := LineType;
      IncrementLineType(LineType);
      IncrementStyle(Style);
      ALineSeries.Pointer.Visible := True;

      for TimeIndex := 0 to AFarm.Count -1 do
      begin
        ABudgetItem :=  AFarm[TimeIndex];
        ALineSeries.AddXY(ABudgetItem.FTime, ABudgetItem.FFinalItems[ItemIndex]);
      end;
    end;

  end;
end;

procedure TfrmFarm.DisplayFarmDemandSeries;
var
  FarmIndex: integer;
  AFarm: TFarmSupplyDeficiency;
  ALineSeries: TLineSeries;
  ItemIndex: integer;
begin
  for FarmIndex  := 0 to clbFarmsSupplyDef.Items.Count -1 do
  begin
    AFarm := FarmSupplyDeficiencies[FarmIndex];

    ALineSeries := FOfeSeriesList[FarmIndex];
    ALineSeries.Visible := clbFarmsSupplyDef.Checked[FarmIndex] and cbOFE.Checked;

    ALineSeries := FDefFlagSeriesList[FarmIndex];
    ALineSeries.Visible := clbFarmsSupplyDef.Checked[FarmIndex] and cbDefFlag.Checked;

    for ItemIndex := 0 to clbDefInitial.Items.Count -1 do
    begin
      ALineSeries := AFarm.FInitialSeries[ItemIndex];
      ALineSeries.Visible := clbFarmsSupplyDef.Checked[FarmIndex] and clbDefInitial.Checked[ItemIndex];
    end;

    for ItemIndex := 0 to clbDefFinal.Items.Count -1 do
    begin
      ALineSeries := AFarm.FFinalSeries[ItemIndex];
      ALineSeries.Visible := clbFarmsSupplyDef.Checked[FarmIndex] and clbDefFinal.Checked[ItemIndex];
    end;
  end;

end;

procedure TfrmFarm.ChangeAllDemandCheckBoxesInList(CB: TJvCheckListBox;
  Checked: Boolean);
begin
  Inc(FBusy);
  try
    if Checked then
    begin
      CB.CheckAll
    end
    else
    begin
      CB.UnCheckAll
    end;
  finally
    Dec(FBusy);
  end;
  DisplayFarmDemandSeries;
end;

procedure TfrmFarm.cbFarmsSupplyDefClick(Sender: TObject);
begin
  inherited;
  ChangeAllDemandCheckBoxesInList(clbFarmsSupplyDef, cbFarmsSupplyDef.Checked);
end;

procedure TfrmFarm.cbDefInitialClick(Sender: TObject);
begin
  inherited;
  ChangeAllDemandCheckBoxesInList(clbDefInitial, cbDefInitial.Checked);
end;

procedure TfrmFarm.cbDefFinalClick(Sender: TObject);
begin
  inherited;
  ChangeAllDemandCheckBoxesInList(clbDefFinal, cbDefFinal.Checked);
end;

procedure TfrmFarm.cbOFEClick(Sender: TObject);
begin
  inherited;
  DisplayFarmDemandSeries;
end;

procedure TfrmFarm.cbDefFlagClick(Sender: TObject);
begin
  inherited;
  DisplayFarmDemandSeries;
end;

procedure TfrmFarm.clbFarmsSupplyDefClickCheck(Sender: TObject);
begin
  inherited;
  DisplayFarmDemandSeries;
end;

procedure TfrmFarm.clbDefInitialClickCheck(Sender: TObject);
begin
  inherited;
  DisplayFarmDemandSeries;
end;

procedure TfrmFarm.clbDefFinalClickCheck(Sender: TObject);
begin
  inherited;
  DisplayFarmDemandSeries;
end;

end.
