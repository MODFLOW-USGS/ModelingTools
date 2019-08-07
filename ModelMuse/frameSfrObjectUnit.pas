unit frameSfrObjectUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, ArgusDataEntry, Grids, RbwDataGrid4,
  Mask, JvExMask, JvSpin, frameFlowTableUnit, JvPageList, JvExControls,
  frameCrossSectionUnit, Buttons;

type
  TFrameClass = class of TFrame;

  TframeSfrObject = class(TFrame)
    GridPanel1: TGridPanel;
    Label1: TLabel;
    Label2: TLabel;
    RbwDataEntry2: TRbwDataEntry;
    Label3: TLabel;
    ComboBox2: TComboBox;
    Label4: TLabel;
    RbwDataEntry3: TRbwDataEntry;
    Label5: TLabel;
    RbwDataEntry4: TRbwDataEntry;
    Label6: TLabel;
    RbwDataEntry1: TRbwDataEntry;
    RbwDataEntry5: TRbwDataEntry;
    Label7: TLabel;
    RbwDataEntry6: TRbwDataEntry;
    Label8: TLabel;
    RbwDataEntry7: TRbwDataEntry;
    Label9: TLabel;
    PageControl1: TPageControl;
    tabUpDown: TTabSheet;
    tabTable: TTabSheet;
    tabRoughness: TTabSheet;
    tabEquation: TTabSheet;
    dgSfrEquation: TRbwDataGrid4;
    comboIcalc: TComboBox;
    comboParam: TComboBox;
    Label13: TLabel;
    jvplTable: TJvPageList;
    jvplCrossSection: TJvPageList;
    btnInsertEquationTime: TBitBtn;
    btnDeleteEquationTime: TBitBtn;
    seSfrEquation: TJvSpinEdit;
    Label15: TLabel;
    Panel1: TPanel;
    seSfrUpDown: TJvSpinEdit;
    Label16: TLabel;
    btnInsertUpDownTime: TBitBtn;
    btnDeleteUpDownTime: TBitBtn;
    Panel2: TPanel;
    Label10: TLabel;
    dgUp: TRbwDataGrid4;
    Panel3: TPanel;
    Label11: TLabel;
    dgDown: TRbwDataGrid4;
    Splitter1: TSplitter;
    Panel4: TPanel;
    dgTableTime: TRbwDataGrid4;
    spTableTimes: TJvSpinEdit;
    Label12: TLabel;
    btnInsertTableTime: TBitBtn;
    btnDeleteTableTime: TBitBtn;
    Splitter2: TSplitter;
    Panel5: TPanel;
    dgSfrRough: TRbwDataGrid4;
    seRoughnessTimes: TJvSpinEdit;
    Label14: TLabel;
    btnInsertRoughnessTime: TBitBtn;
    btnDeleteRoughnessTime: TBitBtn;
    Splitter3: TSplitter;
    procedure comboIcalcChange(Sender: TObject);
    procedure dgTableTimeSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure spTableTimesChange(Sender: TObject);
    procedure seRoughnessTimesChange(Sender: TObject);
    procedure dgSfrRoughSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seSfrEquationChange(Sender: TObject);
    procedure seSfrUpDownChange(Sender: TObject);
    procedure btnInsertTableTimeClick(Sender: TObject);
    procedure btnDeleteTableTimeClick(Sender: TObject);
    procedure btnInsertRoughnessTimeClick(Sender: TObject);
    procedure btnDeleteRoughnessTimeClick(Sender: TObject);
    procedure btnInsertUpDownTimeClick(Sender: TObject);
    procedure btnDeleteUpDownTimeClick(Sender: TObject);
    procedure btnInsertEquationTimeClick(Sender: TObject);
    procedure btnDeleteEquationTimeClick(Sender: TObject);
  private
    procedure AddFrame(FrameClass: TFrameClass; PageList: TJvPageList);
    procedure InsertDataGridTime(DataGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit;
      PageList: TJvPageList);
    procedure DeleteDataGridTime(DataGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit;
      PageList: TJvPageList);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrame22 }
procedure TframeSfrObject.DeleteDataGridTime(DataGrid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit; PageList: TJvPageList);
var
  Row: integer;
  Page: TJvCustomPage;
  Dummy: boolean;
begin
  Row := DataGrid.SelectedRow;
  if Row >= 1 then
  begin
    Page := PageList.Pages[Row-1];
    Assert(Page.ControlCount = 1);
    Page.Controls[0].Free;
    Page.Free;
    DataGrid.DeleteRow(Row);
    SpinEdit.AsInteger := SpinEdit.AsInteger - 1;
    Dummy := True;
    DataGrid.OnSelectCell(DataGrid, 0, DataGrid.SelectedRow, Dummy);
  end;
end;


procedure TframeSfrObject.btnDeleteEquationTimeClick(Sender: TObject);
begin
  if dgSfrEquation.SelectedRow > 0 then
  begin
    dgSfrEquation.DeleteRow(dgSfrEquation.SelectedRow);
    seSfrEquation.AsInteger := seSfrEquation.AsInteger -1;
    seSfrEquation.OnChange(nil);
  end;
end;

procedure TframeSfrObject.btnDeleteRoughnessTimeClick(Sender: TObject);
begin
  DeleteDataGridTime(dgSfrRough, seRoughnessTimes, jvplCrossSection);
end;

procedure TframeSfrObject.btnDeleteTableTimeClick(Sender: TObject);
//var
//  Row: integer;
//  Page: TJvCustomPage;
begin
  DeleteDataGridTime(dgTableTime, spTableTimes, jvplTable);
//  Row := dgTableTime.SelectedRow;
//  if Row >= 1 then
//  begin
//    Page := jvplTable.Pages[Row-1];
//    Assert(Page.ControlCount = 1);
//    Page.Controls[0].Free;
//    Page.Free;
//    spTableTimes.AsInteger := spTableTimes.AsInteger - 1;
//  end;
end;

procedure TframeSfrObject.InsertDataGridTime(DataGrid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit; PageList: TJvPageList);
var
  Page: TJvCustomPage;
  Row: integer;
  Dummy: boolean;
begin
  Row := DataGrid.SelectedRow;
  if Row >= 1 then
  begin
    SpinEdit.AsInteger := SpinEdit.AsInteger + 1;
    SpinEdit.OnChange(SpinEdit);
    Page := PageList.Pages[SpinEdit.AsInteger-1];
    Page.PageIndex := Row-1;
    Dummy := True;
    DataGrid.OnSelectCell(DataGrid, 0, DataGrid.SelectedRow, Dummy);
  end;
end;

procedure TframeSfrObject.btnInsertEquationTimeClick(Sender: TObject);
begin
  if dgSfrEquation.SelectedRow > 0 then
  begin
    dgSfrEquation.InsertRow(dgSfrEquation.SelectedRow);
    seSfrEquation.AsInteger := seSfrEquation.AsInteger +1;
    seSfrEquation.OnChange(nil);
  end;
end;

procedure TframeSfrObject.btnInsertRoughnessTimeClick(Sender: TObject);
begin
    InsertDataGridTime(dgSfrRough, seRoughnessTimes, jvplCrossSection);

end;

procedure TframeSfrObject.btnInsertTableTimeClick(Sender: TObject);
//var
//  Page: TJvCustomPage;
//  Row: integer;
//  Dummy: boolean;
begin
  InsertDataGridTime(dgTableTime, spTableTimes, jvplTable);
//  Row := dgTableTime.SelectedRow;
//  if Row >= 1 then
//  begin
//    spTableTimes.AsInteger := spTableTimes.AsInteger + 1;
//    spTableTimes.OnChange(spTableTimes);
//    Page := jvplTable.Pages[spTableTimes.AsInteger-1];
//    Page.PageIndex := Row-1;
//    Dummy := True;
//    dgTableTime.OnSelectCell(dgTableTime, 0, Row, Dummy);
//  end;
end;

procedure TframeSfrObject.btnInsertUpDownTimeClick(Sender: TObject);
begin
  if dgUp.SelectedRow > 0 then
  begin
    dgUp.InsertRow(dgUp.SelectedRow);
    dgDown.InsertRow(dgUp.SelectedRow);
    seSfrUpDown.AsInteger := seSfrUpDown.AsInteger +1;
    seSfrUpDown.OnChange(nil);
  end;
end;

procedure TframeSfrObject.btnDeleteUpDownTimeClick(Sender: TObject);
begin
  if dgUp.SelectedRow > 0 then
  begin
    dgUp.DeleteRow(dgUp.SelectedRow);
    dgDown.DeleteRow(dgUp.SelectedRow);
    seSfrUpDown.AsInteger := seSfrUpDown.AsInteger -1;
    seSfrUpDown.OnChange(nil);
  end;
end;

procedure TframeSfrObject.comboIcalcChange(Sender: TObject);
begin
  case comboIcalc.ItemIndex of
    0:
      begin

      end;
    1:
      begin

      end;
    2:
      begin

      end;
    3:
      begin

      end;
    4:
      begin

      end;
  end;
end;

Type
  TSfrColumns = (scStartTime, scEndTime, scK, scBedThickness, scBedElevation,
    scStreamWidth, scStreamDepth, scSatWatCont, scInitWaterCont, scBrooksCorey,
    scVK);


  TsfrRoughColumns = (srStartTime, srEndTime, srChannelRough, srBankRough);

  TSfrEquationColumns = (seStartTime, seEndTime, seDepthCoeff, seDepthExp,
    seWidthCoeff, seWidthExp);

  TSfrTableTime = (sttStartTime, sttEndTime);

constructor TframeSfrObject.Create(AOwner: TComponent);
  procedure AssignHeadings(dg: TStringGrid);
  begin
    dg.Cells[Ord(scStartTime),0]     := 'Start time';
    dg.Cells[Ord(scEndTime),0]       := 'End time';
    dg.Cells[Ord(scK),0]             := 'Hydraulic conductivity';
    dg.Cells[Ord(scBedThickness),0]  := 'Streambed thickness';
    dg.Cells[Ord(scBedElevation),0]  := 'Streambed elevation';
    dg.Cells[Ord(scStreamWidth),0]   := 'Stream width';
    dg.Cells[Ord(scStreamDepth),0]   := 'Stream depth';
    dg.Cells[Ord(scSatWatCont),0]    := 'Saturated water content';
    dg.Cells[Ord(scInitWaterCont),0] := 'Initial water content';
    dg.Cells[Ord(scBrooksCorey),0]   := 'Brooks-Corey exponent ';
    dg.Cells[Ord(scVK),0]            := 'Vertical saturated hydraulic conductivity ';
  end;
begin
  inherited;
  spTableTimesChange(nil);
  seRoughnessTimesChange(nil);
  AssignHeadings(dgUp);
  AssignHeadings(dgDown);

  dgSfrRough.Cells[Ord(srStartTime),0]    := 'Start time';
  dgSfrRough.Cells[Ord(srEndTime),0]      := 'End time';
  dgSfrRough.Cells[Ord(srChannelRough),0] := 'Channel roughness';
  dgSfrRough.Cells[Ord(srBankRough),0]    := 'Bank roughness';


  dgSfrEquation.Cells[Ord(seStartTime),0]  := 'Start time';
  dgSfrEquation.Cells[Ord(seEndTime),0]    := 'End time';
  dgSfrEquation.Cells[Ord(seDepthCoeff),0] := 'Depth coefficient';
  dgSfrEquation.Cells[Ord(seDepthExp),0]   := 'Depth exponent';
  dgSfrEquation.Cells[Ord(seWidthCoeff),0] := 'Width coefficient';
  dgSfrEquation.Cells[Ord(seWidthExp),0]   := 'Width exponent';

  dgTableTime.Cells[Ord(sttStartTime),0]    := 'Start time';
  dgTableTime.Cells[Ord(sttEndTime),0]      := 'End time';
end;

procedure TframeSfrObject.dgSfrRoughSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  TimePeriod: integer;
begin
  if not dgSfrRough.Drawing then
  begin
    TimePeriod := ARow -1;
    jvplCrossSection.ActivePageIndex := TimePeriod;
  end;
end;

procedure TframeSfrObject.dgTableTimeSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  TimePeriod: integer;
begin
  if not dgTableTime.Drawing then
  begin
    TimePeriod := ARow -1;
    jvplTable.ActivePageIndex := TimePeriod;
  end;
end;


procedure TframeSfrObject.seSfrEquationChange(Sender: TObject);
begin
  dgSfrEquation.RowCount := seSfrEquation.AsInteger + 1;
  btnDeleteEquationTime.Enabled := (seSfrEquation.AsInteger > 1);
end;

procedure TframeSfrObject.seSfrUpDownChange(Sender: TObject);
begin
  dgUp.RowCount := seSfrUpDown.AsInteger + 1;
  dgDown.RowCount := seSfrUpDown.AsInteger + 1;
  btnDeleteUpDownTime.Enabled := (seSfrUpDown.AsInteger > 1);
end;

procedure TframeSfrObject.AddFrame(FrameClass: TFrameClass;
  PageList: TJvPageList);
var
  Page: TJvCustomPage;
  Frame: TFrame;
begin
  Page := PageList.GetPageClass.Create(self);
  Page.PageList := PageList;
  Frame := FrameClass.Create(self);
  Frame.Parent := Page;
  Frame.Align := alClient;
end;

procedure TframeSfrObject.seRoughnessTimesChange(Sender: TObject);
var
//  Page: TJvCustomPage;
//  Frame: TframeCrossSection;
  NumberOfTimes: integer;
begin
  NumberOfTimes := seRoughnessTimes.AsInteger;
  while jvplCrossSection.PageCount < NumberOfTimes do
  begin
    AddFrame(TframeCrossSection, jvplCrossSection);
//    Page := jvplCrossSection.GetPageClass.Create(self);
//    Page.PageList := jvplCrossSection;
//    Frame := TframeCrossSection.Create(self);
//    Frame.Parent := Page;
//    Frame.Align := alClient;
  end;
  dgSfrRough.RowCount := NumberOfTimes + 1;
  btnDeleteRoughnessTime.Enabled := NumberOfTimes > 1;
end;

procedure TframeSfrObject.spTableTimesChange(Sender: TObject);
var
//  Page: TJvCustomPage;
//  Frame: TframeFlowTable;
  NumberOfTimes: integer;
begin
  NumberOfTimes := spTableTimes.AsInteger;
  while jvplTable.PageCount < NumberOfTimes do
  begin
    AddFrame(TframeFlowTable, jvplTable);
//    Page := jvplTable.GetPageClass.Create(self);
//    Page.PageList := jvplTable;
//    Frame := TframeFlowTable.Create(self);
//    Frame.Parent := Page;
//    Frame.Align := alClient;
  end;
  dgTableTime.RowCount := NumberOfTimes + 1;
  btnDeleteTableTime.Enabled := NumberOfTimes > 1;
end;

end.
