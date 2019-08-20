unit frameScreenObjectMt3dSftUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, Vcl.ComCtrls, UndoItemsScreenObjects, JvToolEdit,
  System.UITypes;

type
  TframeScreenObjectMt3dSft = class(TframeScreenObjectNoParam)
    pgcSft: TPageControl;
    tsSteady: TTabSheet;
    tsHeadWaters: TTabSheet;
    tsPrecipitation: TTabSheet;
    tsRunoff: TTabSheet;
    tsConstantConcentration: TTabSheet;
    pnlBottomPrecip: TPanel;
    lblNumberOfTimesPrecip: TLabel;
    seNumberOfTimesPrecip: TJvSpinEdit;
    btnDeletePrecip: TBitBtn;
    btnInsertPrecip: TBitBtn;
    pnlPrecip: TPanel;
    pnlPrecipFormula: TPanel;
    lblPrecipFormula: TLabel;
    rdePrecipFormula: TRbwDataEntry;
    rdgPrecip: TRbwDataGrid4;
    pnlBottomRunoff: TPanel;
    lblNumberOfTimesRunoff: TLabel;
    seNumberOfTimesRunoff: TJvSpinEdit;
    btnDeleteRunoff: TBitBtn;
    btnInsertRunoff: TBitBtn;
    pnlRunoff: TPanel;
    pnlFormulaRunoff: TPanel;
    lblFormulaRunoff: TLabel;
    rdeFormulaRunoff: TRbwDataEntry;
    rdgRunoff: TRbwDataGrid4;
    pnlBottomConstConc: TPanel;
    lblNumberOfTimesConstConc: TLabel;
    seNumberOfTimesConstConc: TJvSpinEdit;
    btnDeleteConstConc: TBitBtn;
    btnInsertConstConc: TBitBtn;
    pnlGridConstConc: TPanel;
    pnlFormulaConstConc: TPanel;
    lblFormulaConstConc: TLabel;
    rdeFormulaConstConc: TRbwDataEntry;
    rdgConstConc: TRbwDataGrid4;
    rdgSftInitConcAndDisp: TRbwDataGrid4;
    comboObsLocation: TComboBox;
    lblObsLocation: TLabel;
    procedure rdgPrecipSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgPrecipSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seNumberOfTimesPrecipChange(Sender: TObject);
    procedure btnInsertPrecipClick(Sender: TObject);
    procedure btnDeletePrecipClick(Sender: TObject);
    procedure rdgPrecipBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rdgPrecipColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgPrecipHorizontalScroll(Sender: TObject);
    procedure rdgPrecipMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdePrecipFormulaChange(Sender: TObject);
    procedure seNumberOfTimesRunoffChange(Sender: TObject);
    procedure btnInsertRunoffClick(Sender: TObject);
    procedure btnDeleteRunoffClick(Sender: TObject);
    procedure rdeFormulaRunoffChange(Sender: TObject);
    procedure rdgRunoffBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rdgRunoffColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgRunoffHorizontalScroll(Sender: TObject);
    procedure rdgRunoffMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgRunoffSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgRunoffSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure seNumberOfTimesConstConcChange(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteConstConcClick(Sender: TObject);
    procedure rdeFormulaConstConcChange(Sender: TObject);
    procedure rdgConstConcBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rdgConstConcColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgConstConcHorizontalScroll(Sender: TObject);
    procedure rdgConstConcMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgConstConcSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgConstConcSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FDeletingPrecip: Boolean;
    FSelectedTextPrecip: string;
    FDeletingRunoff: Boolean;
    FSelectedTextRunoff: string;
    FDeletingConstConc: Boolean;
    FSelectedTextConstConc: string;
    procedure InitializeControls;
    procedure LayoutMultiRowEditControlsPrecip;
    procedure LayoutMultiRowEditControlsRunoff;
    procedure LayoutMultiRowEditControlsConstConc;
//    function ShouldEnableMultisetControlsPrecip: Boolean;
    procedure ChangeTimes(SpinEdit: TJvSpinEdit; var Deleting: Boolean;
      AGrid: TRbwDataGrid4; Deletebutton: TBitBtn);
    procedure InsertTime(AGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
    procedure Deletetime(AGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
    procedure MultiFormulaChange(AGrid: TRbwDataGrid4; EdFormula: TRbwDataEntry);
    procedure BeforeDrawTimeCell(AGrid: TRbwDataGrid4; ACol: Integer; ARow: Integer);
    procedure LayoutMultEditControls(AGrid: TRbwDataGrid4; Edit: TRbwDataEntry; ALabel: TLabel);
    function ShouldEnableMultEditControls(AGrid: TRbwDataGrid4): Boolean;
    procedure SelectTimeCell(AGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit;
      ARow: Integer; ACol: Integer; var CanSelect: Boolean;
      var SelectedText: string);
    procedure SetTimeText(Deleting: Boolean; SpinEdit: TJvSpinEdit; AGrid: TRbwDataGrid4;
      SelectedText: string; const Value: string; ACol: Integer; ARow: Integer);
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectMt3dSft: TframeScreenObjectMt3dSft;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, GoPhastTypes, ScreenObjectUnit, Mt3dSftUnit,
  frmCustomGoPhastUnit, System.Math;

{$R *.dfm}

resourcestring
  StrChemicalSpecies = 'Chemical species';
  StrInitialConcentratio = 'Initial Conc.';
  StrDispersion = 'Dispersion';

type
  TSftInitDispColumns = (sidcSpecies, sidcInitialConc, sidcDispersion);
  TSftBoundaryColumns = (sbcStartTime, sbcEndTime, sbcConc1);

{ TframeScreenObjectMt3dSft }

procedure TframeScreenObjectMt3dSft.btnDeleteConstConcClick(Sender: TObject);
begin
  inherited;
  Deletetime(rdgConstConc, seNumberOfTimesConstConc);
end;

procedure TframeScreenObjectMt3dSft.btnDeletePrecipClick(Sender: TObject);
begin
  inherited;
  Deletetime(rdgPrecip, seNumberOfTimesPrecip);
end;

procedure TframeScreenObjectMt3dSft.btnDeleteRunoffClick(Sender: TObject);
begin
  inherited;
  Deletetime(rdgRunoff, seNumberOfTimesRunoff);
end;

procedure TframeScreenObjectMt3dSft.btnInsertClick(Sender: TObject);
begin
  inherited;
  InsertTime(rdgConstConc, seNumberOfTimesConstConc);
end;

procedure TframeScreenObjectMt3dSft.btnInsertPrecipClick(Sender: TObject);
begin
  inherited;
  InsertTime(rdgPrecip, seNumberOfTimesPrecip);
end;

procedure TframeScreenObjectMt3dSft.btnInsertRunoffClick(Sender: TObject);
begin
  inherited;
  InsertTime(rdgRunoff, seNumberOfTimesRunoff);
end;

procedure TframeScreenObjectMt3dSft.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  Model: TPhastModel;
  NCOMP: Integer;
  FoundFirst: Boolean;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Mt3dSftConcBoundary: TMt3dSftBoundary;
  ComponentIndex: Integer;
  HeadWatersConc: THeadWaterMt3dSftReachCollection;
  TimeIndex: Integer;
  FirstHeadWatersConc: THeadWaterMt3dSftReachCollection;
  ConcItem: TSftReachItem;
  Precipitation: TPrecipitationMt3dSftReachCollection;
  FirstPrecipitation: TPrecipitationMt3dSftReachCollection;
  RunOff: TRunoffMt3dSftReachCollection;
  FirstRunOff: TRunoffMt3dSftReachCollection;
  ConstConc: TConstConcMt3dSftReachCollection;
  FirstConstConc: TConstConcMt3dSftReachCollection;
  HeadWater: THeadWaterMt3dSftReachCollection;
begin
  InitializeControls;

  Model := frmGoPhast.PhastModel;
  NCOMP := Model.MobileComponents.Count + Model.ImmobileComponents.Count;

  FoundFirst := False;
  FirstHeadWatersConc := nil;
  FirstPrecipitation := nil;
  FirstRunOff := nil;
  FirstConstConc := nil;

  for ScreenObjectIndex := 0 to ScreenObjectList.Count - 1 do
  begin
    AScreenObject := ScreenObjectList[ScreenObjectIndex].ScreenObject;
    Mt3dSftConcBoundary := AScreenObject.Mt3dSftConcBoundary;
    if (Mt3dSftConcBoundary <> nil) and Mt3dSftConcBoundary.Used then
    begin
      if not FoundFirst then
      begin
        FoundFirst := True;

        comboObsLocation.ItemIndex := Ord(Mt3dSftConcBoundary.ObsLocation);

        for ComponentIndex := 0 to NCOMP - 1 do
        begin
          rdgSftInitConcAndDisp.Cells[Ord(sidcInitialConc), ComponentIndex+1] :=
            Mt3dSftConcBoundary.InitialConcentration[ComponentIndex].BoundaryFormula[0];
          rdgSftInitConcAndDisp.Cells[Ord(sidcDispersion), ComponentIndex+1] :=
            Mt3dSftConcBoundary.DispersionCoefficient[ComponentIndex].BoundaryFormula[0];
        end;

        HeadWatersConc :=  Mt3dSftConcBoundary.Values as THeadWaterMt3dSftReachCollection;
        FirstHeadWatersConc := HeadWatersConc;
        seNumberOfTimes.AsInteger := HeadWatersConc.Count;
        for TimeIndex := 0 to HeadWatersConc.Count - 1 do
        begin
          ConcItem := HeadWatersConc[TimeIndex] as TSftReachItem;
          rdgModflowBoundary.RealValue[Ord(sbcStartTime), TimeIndex+1] := ConcItem.StartTime;
          rdgModflowBoundary.RealValue[Ord(sbcEndTime), TimeIndex+1] := ConcItem.EndTime;
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            rdgModflowBoundary.Cells[ComponentIndex + Ord(sbcConc1),
              TimeIndex+1] := ConcItem.Mt3dmsConcRate[ComponentIndex];
          end;
        end;

        Precipitation :=  Mt3dSftConcBoundary.Precipitation;
        FirstPrecipitation := Precipitation;
        seNumberOfTimesPrecip.AsInteger := Precipitation.Count;
        for TimeIndex := 0 to Precipitation.Count - 1 do
        begin
          ConcItem := Precipitation[TimeIndex] as TSftReachItem;
          rdgPrecip.RealValue[Ord(sbcStartTime), TimeIndex+1] := ConcItem.StartTime;
          rdgPrecip.RealValue[Ord(sbcEndTime), TimeIndex+1] := ConcItem.EndTime;
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            rdgPrecip.Cells[ComponentIndex + Ord(sbcConc1),
              TimeIndex+1] := ConcItem.Mt3dmsConcRate[ComponentIndex];
          end;
        end;

        RunOff :=  Mt3dSftConcBoundary.RunOff;
        FirstRunOff := RunOff;
        seNumberOfTimesRunoff.AsInteger := RunOff.Count;
        for TimeIndex := 0 to RunOff.Count - 1 do
        begin
          ConcItem := RunOff[TimeIndex] as TSftReachItem;
          rdgRunoff.RealValue[Ord(sbcStartTime), TimeIndex+1] := ConcItem.StartTime;
          rdgRunoff.RealValue[Ord(sbcEndTime), TimeIndex+1] := ConcItem.EndTime;
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            rdgRunoff.Cells[ComponentIndex + Ord(sbcConc1),
              TimeIndex+1] := ConcItem.Mt3dmsConcRate[ComponentIndex];
          end;
        end;

        ConstConc :=  Mt3dSftConcBoundary.ConstConc;
        FirstConstConc := ConstConc;
        seNumberOfTimesConstConc.AsInteger := ConstConc.Count;
        for TimeIndex := 0 to ConstConc.Count - 1 do
        begin
          ConcItem := ConstConc[TimeIndex] as TSftReachItem;
          rdgConstConc.RealValue[Ord(sbcStartTime), TimeIndex+1] := ConcItem.StartTime;
          rdgConstConc.RealValue[Ord(sbcEndTime), TimeIndex+1] := ConcItem.EndTime;
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            rdgConstConc.Cells[ComponentIndex + Ord(sbcConc1),
              TimeIndex+1] := ConcItem.Mt3dmsConcRate[ComponentIndex];
          end;
        end;
      end
      else
      begin
        for ComponentIndex := 0 to NCOMP - 1 do
        begin
          if rdgSftInitConcAndDisp.Cells[Ord(sidcInitialConc), ComponentIndex+1] <>
              Mt3dSftConcBoundary.InitialConcentration[ComponentIndex].BoundaryFormula[0] then
          begin
            rdgSftInitConcAndDisp.Cells[Ord(sidcInitialConc), ComponentIndex+1] := ''
          end;
          if rdgSftInitConcAndDisp.Cells[Ord(sidcDispersion), ComponentIndex+1] <>
              Mt3dSftConcBoundary.DispersionCoefficient[ComponentIndex].BoundaryFormula[0] then
          begin
            rdgSftInitConcAndDisp.Cells[Ord(sidcDispersion), ComponentIndex+1] := ''
          end;
        end;

        if comboObsLocation.ItemIndex <> Ord(Mt3dSftConcBoundary.ObsLocation) then
        begin
          comboObsLocation.ItemIndex := -1;
        end;

        HeadWater :=  Mt3dSftConcBoundary.Values as THeadWaterMt3dSftReachCollection;
        if not HeadWater.IsSame(FirstHeadWatersConc) then
        begin
          ClearGrid(rdgModflowBoundary);
        end;

        Precipitation :=  Mt3dSftConcBoundary.Precipitation;
        if not Precipitation.IsSame(FirstPrecipitation) then
        begin
          ClearGrid(rdgPrecip);
        end;

        RunOff :=  Mt3dSftConcBoundary.RunOff;
        if not RunOff.IsSame(FirstRunOff) then
        begin
          ClearGrid(rdgRunoff);
        end;

        ConstConc :=  Mt3dSftConcBoundary.ConstConc;
        if not ConstConc.IsSame(FirstConstConc) then
        begin
          ClearGrid(rdgConstConc);
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectMt3dSft.InitializeControls;
var
  Model: TPhastModel;
  NCOMP: Integer;
  CIndex: Integer;
  InitCIndex: Integer;
  SpeciesName: string;
  AColumn: TRbwColumn4;
  ComponentIndex: Integer;
  procedure AdjustColWidth(AGrid: TRbwDataGrid4; Adjust: Boolean);
  var
    ColIndex: Integer;
  begin
    AGrid.BeginUpdate;
    try
      for ColIndex := 0 to AGrid.ColCount - 1 do
      begin
        AGrid.Columns[ColIndex].AutoAdjustColWidths := Adjust;
      end;
    finally
      AGrid.EndUpdate;
    end;
  end;
begin
  pgcSft.ActivePageIndex := 0;
  comboObsLocation.itemIndex := 0;

  pnlBottom.Parent := tsHeadWaters;
  pnlGrid.Parent := tsHeadWaters;
  pgcSft.Align := alClient;

  ClearGrid(rdgSftInitConcAndDisp);
  ClearGrid(rdgModflowBoundary);
  ClearGrid(rdgPrecip);
  ClearGrid(rdgRunoff);
  ClearGrid(rdgConstConc);

  Model := frmGoPhast.PhastModel;
  NCOMP := Model.MobileComponents.Count + Model.ImmobileComponents.Count;

  rdgModflowBoundary.ColCount := NCOMP+2;
  rdgPrecip.ColCount := NCOMP+2;
  rdgRunoff.ColCount := NCOMP+2;
  rdgConstConc.ColCount := NCOMP+2;

  rdgSftInitConcAndDisp.RowCount := NCOMP + 1;

  seNumberOfTimes.AsInteger := 0;
  seNumberOfTimesPrecip.AsInteger := 0;
  seNumberOfTimesRunoff.AsInteger := 0;
  seNumberOfTimesConstConc.AsInteger := 0;

  AdjustColWidth(rdgSftInitConcAndDisp, True);
  AdjustColWidth(rdgModflowBoundary, True);
  AdjustColWidth(rdgPrecip, True);
  AdjustColWidth(rdgRunoff, True);
  AdjustColWidth(rdgConstConc, True);

  rdgSftInitConcAndDisp.BeginUpdate;
  rdgModflowBoundary.BeginUpdate;
  rdgPrecip.BeginUpdate;
  rdgRunoff.BeginUpdate;
  rdgConstConc.BeginUpdate;
  try
    rdgSftInitConcAndDisp.FixedCols := 1;
    rdgSftInitConcAndDisp.Cells[Ord(sidcSpecies),0] := StrChemicalSpecies;
    rdgSftInitConcAndDisp.Cells[Ord(sidcInitialConc),0] := StrInitialConcentratio;
    rdgSftInitConcAndDisp.Cells[Ord(sidcDispersion),0] := StrDispersion;

    rdgModflowBoundary.Cells[Ord(sbcStartTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(sbcEndTime), 0] := StrEndingTime;

    rdgPrecip.Cells[Ord(sbcStartTime), 0] := StrStartingTime;
    rdgPrecip.Cells[Ord(sbcEndTime), 0] := StrEndingTime;

    rdgRunoff.Cells[Ord(sbcStartTime), 0] := StrStartingTime;
    rdgRunoff.Cells[Ord(sbcEndTime), 0] := StrEndingTime;

    rdgConstConc.Cells[Ord(sbcStartTime), 0] := StrStartingTime;
    rdgConstConc.Cells[Ord(sbcEndTime), 0] := StrEndingTime;

    Model.ModflowStressPeriods.FillPickListWithStartTimes(rdgModflowBoundary, Ord(sbcStartTime));
    Model.ModflowStressPeriods.FillPickListWithEndTimes(rdgModflowBoundary, Ord(sbcEndTime));

    Model.ModflowStressPeriods.FillPickListWithStartTimes(rdgPrecip, Ord(sbcStartTime));
    Model.ModflowStressPeriods.FillPickListWithEndTimes(rdgPrecip, Ord(sbcEndTime));

    Model.ModflowStressPeriods.FillPickListWithStartTimes(rdgRunoff, Ord(sbcStartTime));
    Model.ModflowStressPeriods.FillPickListWithEndTimes(rdgRunoff, Ord(sbcEndTime));

    Model.ModflowStressPeriods.FillPickListWithStartTimes(rdgConstConc, Ord(sbcStartTime));
    Model.ModflowStressPeriods.FillPickListWithEndTimes(rdgConstConc, Ord(sbcEndTime));


    CIndex := Ord(sbcConc1);
    InitCIndex := 1;
    for ComponentIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      SpeciesName := Model.MobileComponents[ComponentIndex].Name;
      rdgModflowBoundary.Cells[CIndex, 0] := SpeciesName;
      rdgPrecip.Cells[CIndex, 0] := SpeciesName;
      rdgRunoff.Cells[CIndex, 0] := SpeciesName;
      rdgConstConc.Cells[CIndex, 0] := SpeciesName;
      rdgSftInitConcAndDisp.Cells[0, InitCIndex] := SpeciesName;

      AColumn := rdgModflowBoundary.Columns[CIndex];
      AColumn.ButtonUsed := true;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonCaption := 'F()';

      AColumn := rdgPrecip.Columns[CIndex];
      AColumn.ButtonUsed := true;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonCaption := 'F()';

      AColumn := rdgRunoff.Columns[CIndex];
      AColumn.ButtonUsed := true;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonCaption := 'F()';

      AColumn := rdgConstConc.Columns[CIndex];
      AColumn.ButtonUsed := true;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonCaption := 'F()';

      Inc(CIndex);
      Inc(InitCIndex);
    end;
    for ComponentIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      SpeciesName := Model.ImmobileComponents[ComponentIndex].Name;
      rdgModflowBoundary.Cells[CIndex, 0] := SpeciesName;
      rdgPrecip.Cells[CIndex, 0] := SpeciesName;
      rdgRunoff.Cells[CIndex, 0] := SpeciesName;
      rdgConstConc.Cells[CIndex, 0] := SpeciesName;
      rdgSftInitConcAndDisp.Cells[0, InitCIndex] := SpeciesName;

      AColumn := rdgModflowBoundary.Columns[CIndex];
      AColumn.ButtonUsed := true;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonCaption := 'F()';

      AColumn := rdgPrecip.Columns[CIndex];
      AColumn.ButtonUsed := true;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonCaption := 'F()';

      AColumn := rdgRunoff.Columns[CIndex];
      AColumn.ButtonUsed := true;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonCaption := 'F()';

      AColumn := rdgConstConc.Columns[CIndex];
      AColumn.ButtonUsed := true;
      AColumn.ButtonWidth := 35;
      AColumn.ButtonCaption := 'F()';

      Inc(CIndex);
      Inc(InitCIndex);
    end;
  finally
    rdgSftInitConcAndDisp.EndUpdate;
    rdgModflowBoundary.EndUpdate;
    rdgPrecip.EndUpdate;
    rdgRunoff.EndUpdate;
    rdgConstConc.EndUpdate;
  end;

  AdjustColWidth(rdgSftInitConcAndDisp, False);
  AdjustColWidth(rdgModflowBoundary, False);
  AdjustColWidth(rdgPrecip, False);
  AdjustColWidth(rdgRunoff, False);
  AdjustColWidth(rdgConstConc, False);

end;

procedure TframeScreenObjectMt3dSft.LayoutMultiRowEditControlsConstConc;
begin
  LayoutMultEditControls(rdgConstConc, rdeFormulaConstConc, lblFormulaConstConc);
end;

procedure TframeScreenObjectMt3dSft.LayoutMultiRowEditControlsPrecip;
begin
  LayoutMultEditControls(rdgPrecip, rdePrecipFormula, lblPrecipFormula);
end;

procedure TframeScreenObjectMt3dSft.LayoutMultiRowEditControlsRunoff;
begin
  LayoutMultEditControls(rdgRunoff, rdeFormulaRunoff, lblFormulaRunoff);
end;

procedure TframeScreenObjectMt3dSft.rdeFormulaConstConcChange(Sender: TObject);
begin
  inherited;
  MultiFormulaChange(rdgConstConc, rdeFormulaConstConc);
end;

procedure TframeScreenObjectMt3dSft.rdeFormulaRunoffChange(Sender: TObject);
begin
  inherited;
  MultiFormulaChange(rdgRunoff, rdeFormulaRunoff);
end;

procedure TframeScreenObjectMt3dSft.rdePrecipFormulaChange(Sender: TObject);
begin
  MultiFormulaChange(rdgPrecip, rdePrecipFormula);
end;

procedure TframeScreenObjectMt3dSft.rdgConstConcBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  BeforeDrawTimeCell(rdgConstConc, ACol, ARow);
end;

procedure TframeScreenObjectMt3dSft.rdgConstConcColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControlsConstConc
end;

procedure TframeScreenObjectMt3dSft.rdgConstConcHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControlsConstConc
end;

procedure TframeScreenObjectMt3dSft.rdgConstConcMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  rdeFormulaConstConc.Enabled := ShouldEnableMultEditControls(rdgConstConc);
end;

procedure TframeScreenObjectMt3dSft.rdgConstConcSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  SelectTimeCell(rdgConstConc, seNumberOfTimesConstConc, ARow, ACol, CanSelect, FSelectedTextConstConc);
end;

procedure TframeScreenObjectMt3dSft.rdgConstConcSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  SetTimeText(FDeletingconstConc, seNumberOfTimesConstConc, rdgConstConc,
    FSelectedTextConstConc, Value, ACol, ARow);
end;

procedure TframeScreenObjectMt3dSft.rdgPrecipBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  BeforeDrawTimeCell(rdgPrecip, ACol, ARow);
end;

procedure TframeScreenObjectMt3dSft.rdgPrecipColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControlsPrecip;
end;

procedure TframeScreenObjectMt3dSft.rdgPrecipHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControlsPrecip;
end;

procedure TframeScreenObjectMt3dSft.rdgPrecipMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  rdePrecipFormula.Enabled := ShouldEnableMultEditControls(rdgPrecip);
end;

procedure TframeScreenObjectMt3dSft.rdgPrecipSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  SelectTimeCell(rdgPrecip, seNumberOfTimesPrecip, ARow, ACol, CanSelect, FSelectedTextPrecip);
end;

procedure TframeScreenObjectMt3dSft.rdgPrecipSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  SetTimeText(FDeletingPrecip, seNumberOfTimesPrecip, rdgPrecip,
    FSelectedTextPrecip, Value, ACol, ARow);
end;


procedure TframeScreenObjectMt3dSft.rdgRunoffBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  BeforeDrawTimeCell(rdgRunoff, ACol, ARow);
end;

procedure TframeScreenObjectMt3dSft.rdgRunoffColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControlsRunoff
end;

procedure TframeScreenObjectMt3dSft.rdgRunoffHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControlsRunoff
end;

procedure TframeScreenObjectMt3dSft.rdgRunoffMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  rdeFormulaRunoff.Enabled := ShouldEnableMultEditControls(rdgRunoff);
end;

procedure TframeScreenObjectMt3dSft.rdgRunoffSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  SelectTimeCell(rdgRunoff, seNumberOfTimesRunoff, ARow, ACol, CanSelect, FSelectedTextRunoff);
end;

procedure TframeScreenObjectMt3dSft.rdgRunoffSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  SetTimeText(FDeletingRunoff, seNumberOfTimesRunoff, rdgRunoff,
    FSelectedTextRunoff, Value, ACol, ARow);
end;

procedure TframeScreenObjectMt3dSft.seNumberOfTimesConstConcChange(
  Sender: TObject);
begin
  inherited;
  ChangeTimes(seNumberOfTimesConstConc, FDeletingConstConc, rdgConstConc, btnDeleteConstConc);
end;

procedure TframeScreenObjectMt3dSft.seNumberOfTimesPrecipChange(
  Sender: TObject);
begin
  inherited;
  ChangeTimes(seNumberOfTimesPrecip, FDeletingPrecip, rdgPrecip, btnDeletePrecip);
end;

procedure TframeScreenObjectMt3dSft.seNumberOfTimesRunoffChange(
  Sender: TObject);
begin
  inherited;
  ChangeTimes(seNumberOfTimesRunoff, FDeletingRunoff, rdgRunoff, btnDeleteRunoff);

end;

procedure TframeScreenObjectMt3dSft.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Model: TPhastModel;
  NCOMP: Integer;
  HeadWatersConc: THeadWaterMt3dSftReachCollection;
  Precipitation: TPrecipitationMt3dSftReachCollection;
  RunOff: TRunoffMt3dSftReachCollection;
  ConstConc: TConstConcMt3dSftReachCollection;
  TimeIndex: Integer;
  ConcItem: TSftReachItem;
  Index: Integer;
  BoundaryUsed: Boolean;
  AScreenObject: TScreenObject;
  Mt3dSftConcBoundary: TMt3dSftBoundary;
  ComponentIndex: Integer;
  function SfrUsed(AScreenObject: TScreenObject): Boolean;
  begin
    result := ((AScreenObject.ModflowSfrBoundary <> nil)
            and AScreenObject.ModflowSfrBoundary.Used)
//          or ((AScreenObject.ModflowSfr6Boundary <> nil)
//            and AScreenObject.ModflowSfr6Boundary.Used);
  end;
  function RowOk(Grid: TRbwDataGrid4; Row: Integer): Boolean;
  var
    ColIndex: Integer;
  begin
    result := True;
    for ColIndex := 0 to Grid.ColCount - 1 do
    begin
      if Trim(Grid.Cells[ColIndex, Row]) = '' then
      begin
        result := False;
        Exit;
      end;
    end;
  end;
begin
  Model := frmGoPhast.PhastModel;
  NCOMP := Model.MobileComponents.Count + Model.ImmobileComponents.Count;

  HeadWatersConc := THeadWaterMt3dSftReachCollection.Create(nil, nil, nil);
  Precipitation := TPrecipitationMt3dSftReachCollection.Create(nil, nil, nil);
  RunOff := TRunoffMt3dSftReachCollection.Create(nil, nil, nil);
  ConstConc := TConstConcMt3dSftReachCollection.Create(nil, nil, nil);
  try
    if not ClearAll then
    begin
      for TimeIndex := 0 to seNumberOfTimes.AsInteger - 1 do
      begin
        if RowOk(rdgModflowBoundary, TimeIndex+1) then
        begin
          ConcItem := HeadWatersConc.Add as TSftReachItem;
          ConcItem.StartTime := rdgModflowBoundary.RealValue[Ord(sbcStartTime), TimeIndex+1];
          ConcItem.EndTime := rdgModflowBoundary.RealValue[Ord(sbcEndTime), TimeIndex+1];
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            ConcItem.Mt3dmsConcRate[ComponentIndex] :=
              rdgModflowBoundary.Cells[ComponentIndex + Ord(sbcConc1),
              TimeIndex+1];
          end;
        end;
      end;

      for TimeIndex := 0 to seNumberOfTimesPrecip.AsInteger - 1 do
      begin
        if RowOk(rdgPrecip, TimeIndex+1) then
        begin
          ConcItem := Precipitation.Add as TSftReachItem;
          ConcItem.StartTime := rdgPrecip.RealValue[Ord(sbcStartTime), TimeIndex+1];
          ConcItem.EndTime := rdgPrecip.RealValue[Ord(sbcEndTime), TimeIndex+1];
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            ConcItem.Mt3dmsConcRate[ComponentIndex] :=
              rdgPrecip.Cells[ComponentIndex + Ord(sbcConc1),
              TimeIndex+1];
          end;
        end;
      end;

      for TimeIndex := 0 to seNumberOfTimesRunoff.AsInteger - 1 do
      begin
        if RowOk(rdgRunoff, TimeIndex+1) then
        begin
          ConcItem := RunOff.Add as TSftReachItem;
          ConcItem.StartTime := rdgRunoff.RealValue[Ord(sbcStartTime), TimeIndex+1];
          ConcItem.EndTime := rdgRunoff.RealValue[Ord(sbcEndTime), TimeIndex+1];
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            ConcItem.Mt3dmsConcRate[ComponentIndex] :=
              rdgRunoff.Cells[ComponentIndex + Ord(sbcConc1),
              TimeIndex+1];
          end;
        end;
      end;

      for TimeIndex := 0 to seNumberOfTimesConstConc.AsInteger - 1 do
      begin
        if RowOk(rdgConstConc, TimeIndex+1) then
        begin
          ConcItem := ConstConc.Add as TSftReachItem;
          ConcItem.StartTime := rdgConstConc.RealValue[Ord(sbcStartTime), TimeIndex+1];
          ConcItem.EndTime := rdgConstConc.RealValue[Ord(sbcEndTime), TimeIndex+1];
          for ComponentIndex := 0 to NCOMP - 1 do
          begin
            ConcItem.Mt3dmsConcRate[ComponentIndex] :=
              rdgConstConc.Cells[ComponentIndex + Ord(sbcConc1),
              TimeIndex+1];
          end;
        end;
      end;
    end;

    for Index := 0 to List.Count - 1 do
    begin
      BoundaryUsed := False;
      AScreenObject := List.Items[Index].ScreenObject;
      Mt3dSftConcBoundary := AScreenObject.Mt3dSftConcBoundary;
      if (Mt3dSftConcBoundary <> nil) and Mt3dSftConcBoundary.Used then
      begin
        BoundaryUsed := True;
      end;
      if ClearAll then
      begin
        if BoundaryUsed then
        begin
          Mt3dSftConcBoundary.Clear;
        end;
      end
      else if SetAll or BoundaryUsed then
      begin
        if (Mt3dSftConcBoundary = nil) and SfrUsed(AScreenObject)
          then
        begin
          AScreenObject.CreateMt3dSftConcBoundary;
          Mt3dSftConcBoundary := AScreenObject.Mt3dSftConcBoundary;
        end;

        if Mt3dSftConcBoundary <> nil then
        begin
          if not SfrUsed(AScreenObject) then
          begin
            Mt3dSftConcBoundary.Clear;
          end
          else
          begin
            if comboObsLocation.ItemIndex >= 0 then
            begin
              Mt3dSftConcBoundary.ObsLocation :=
                TSftObsLocation(comboObsLocation.ItemIndex);
            end;

            while Mt3dSftConcBoundary.InitialConcentration.Count < NCOMP do
            begin
              Mt3dSftConcBoundary.InitialConcentration.Add;
            end;
            while Mt3dSftConcBoundary.InitialConcentration.Count > NCOMP do
            begin
              Mt3dSftConcBoundary.InitialConcentration.Last.Free;
            end;
            while Mt3dSftConcBoundary.DispersionCoefficient.Count < NCOMP do
            begin
              Mt3dSftConcBoundary.DispersionCoefficient.Add;
            end;
            while Mt3dSftConcBoundary.DispersionCoefficient.Count > NCOMP do
            begin
              Mt3dSftConcBoundary.DispersionCoefficient.Last.Free;
            end;
            for ComponentIndex := 1 to rdgSftInitConcAndDisp.RowCount - 1 do
            begin
              if rdgSftInitConcAndDisp.Cells[Ord(sidcInitialConc), ComponentIndex] <> '' then
              begin
                Mt3dSftConcBoundary.InitialConcentration[ComponentIndex-1].BoundaryFormula[0]
                  := rdgSftInitConcAndDisp.Cells[Ord(sidcInitialConc), ComponentIndex];
              end;
              if rdgSftInitConcAndDisp.Cells[Ord(sidcDispersion), ComponentIndex] <> '' then
              begin
                Mt3dSftConcBoundary.DispersionCoefficient[ComponentIndex-1].BoundaryFormula[0]
                  := rdgSftInitConcAndDisp.Cells[Ord(sidcDispersion), ComponentIndex];
              end;
            end;

            if (HeadWatersConc.Count > 0) or (List.Count = 1) then
            begin
              Mt3dSftConcBoundary.Values := HeadWatersConc;
            end;

            if (Precipitation.Count > 0) or (List.Count = 1) then
            begin
              Mt3dSftConcBoundary.Precipitation := Precipitation;
            end;

            if (RunOff.Count > 0) or (List.Count = 1) then
            begin
              Mt3dSftConcBoundary.RunOff := RunOff;
            end;

            if (ConstConc.Count > 0) or (List.Count = 1) then
            begin
              Mt3dSftConcBoundary.ConstConc := ConstConc;
            end;
          end;
        end;
      end;
    end;
  finally
    HeadWatersConc.Free;
    Precipitation.Free;
    RunOff.Free;
    ConstConc.Free;
  end;
end;

procedure TframeScreenObjectMt3dSft.SetTimeText(Deleting: Boolean;
  SpinEdit: TJvSpinEdit; AGrid: TRbwDataGrid4;
  SelectedText: string; const Value: string; ACol: Integer; ARow: Integer);
begin
  if Deleting then
  begin
    Exit;
  end;
  if SpinEdit.AsInteger < AGrid.RowCount - 1 then
  begin
    SpinEdit.AsInteger := AGrid.RowCount - 1;
    SpinEdit.OnChange(SpinEdit);
  end;
  if SelectedText <> Value then
  begin
    DeletedCells[ACol, ARow] := Value = '';
  end;
  UpdateNextTimeCell(AGrid, ACol, ARow);
end;

procedure TframeScreenObjectMt3dSft.SelectTimeCell(AGrid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit; ARow: Integer; ACol: Integer; var CanSelect: Boolean; var SelectedText: string);
begin
  if (ARow = AGrid.FixedRows) and (SpinEdit.AsInteger = 0) then
  begin
    FSelectedTextPrecip := AGrid.Cells[ACol, ARow];
    CanSelect := False;
  end;
end;

function TframeScreenObjectMt3dSft.ShouldEnableMultEditControls(AGrid: TRbwDataGrid4): Boolean;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  result := False;
  for RowIndex := AGrid.FixedRows to AGrid.RowCount - 1 do
  begin
    for ColIndex := FLastTimeColumn + 1 to AGrid.ColCount - 1 do
    begin
      result := AGrid.IsSelectedCell(ColIndex, RowIndex);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TframeScreenObjectMt3dSft.LayoutMultEditControls(AGrid: TRbwDataGrid4;
  Edit: TRbwDataEntry; ALabel: TLabel);
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit;
  end;
  LayoutControls(AGrid, Edit, ALabel, Max(FLastTimeColumn + 1, AGrid.LeftCol));
end;

procedure TframeScreenObjectMt3dSft.BeforeDrawTimeCell(AGrid: TRbwDataGrid4;
  ACol: Integer; ARow: Integer);
var
  EndTime: Double;
  NextStartTime: Double;
begin
  if (ACol = 1) and (ARow >= AGrid.FixedRows) and (ARow < AGrid.RowCount - 1) then
  begin
    if TryStrToFloat(AGrid.Cells[ACol, ARow], EndTime)
      and TryStrToFloat(AGrid.Cells[0, ARow + 1], NextStartTime) then
    begin
      if NextStartTime < EndTime then
      begin
        AGrid.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TframeScreenObjectMt3dSft.MultiFormulaChange(AGrid: TRbwDataGrid4; EdFormula: TRbwDataEntry);
var
  RowIndex: Integer;
  ColIndex: Integer;
  TempOptions: Vcl.Grids.TGridOptions;
begin
  AGrid.BeginUpdate;
  try
    for RowIndex := AGrid.FixedRows to AGrid.RowCount - 1 do
    begin
      for ColIndex := FLastTimeColumn + 1 to AGrid.ColCount - 1 do
      begin
        if AGrid.IsSelectedCell(ColIndex, RowIndex) then
        begin
          AGrid.Cells[ColIndex, RowIndex] := EdFormula.Text;
          if Assigned(AGrid.OnSetEditText) then
          begin
            AGrid.OnSetEditText(AGrid, ColIndex, RowIndex, EdFormula.Text);
          end;
        end;
      end;
    end;
  finally
    AGrid.EndUpdate;
  end;
  TempOptions := AGrid.Options;
  try
    AGrid.Options := [goEditing, goAlwaysShowEditor];
    AGrid.UpdateEditor;
  finally
    AGrid.Options := TempOptions;
  end;
end;

procedure TframeScreenObjectMt3dSft.Deletetime(AGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
begin
  if (AGrid.RowCount > 2) and (AGrid.Row > 0) then
  begin
    AGrid.DeleteRow(AGrid.Row);
  end;
  SpinEdit.AsInteger := SpinEdit.AsInteger - 1;
end;

procedure TframeScreenObjectMt3dSft.InsertTime(AGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
begin
  if (AGrid.SelectedRow <= 0) or (AGrid.SelectedRow >= AGrid.RowCount) then
  begin
    Beep;
    MessageDlg(StrYouNeedToSelectA, mtInformation, [mbOK], 0);
    Exit;
  end;
  if (SpinEdit.AsInteger > 0) then
  begin
    AGrid.InsertRow(AGrid.SelectedRow);
  end;
  SpinEdit.AsInteger := SpinEdit.AsInteger + 1;
end;

procedure TframeScreenObjectMt3dSft.ChangeTimes(SpinEdit: TJvSpinEdit;
  var Deleting: Boolean; AGrid: TRbwDataGrid4; Deletebutton: TBitBtn);
begin
  Deleting := True;
  try
    if SpinEdit.AsInteger = 0 then
    begin
      AGrid.RowCount := 2;
    end
    else
    begin
      AGrid.RowCount := SpinEdit.AsInteger + 1;
    end;
    Deletebutton.Enabled := SpinEdit.AsInteger >= 1;
    AGrid.Invalidate;
  finally
    Deleting := False;
  end;
end;

//function TframeScreenObjectMt3dSft.ShouldEnableMultisetControlsPrecip: Boolean;
//var
//  AGrid: TRbwDataGrid4;
//begin
//  Result := ShouldEnableMultEditControls(rdgPrecip);
//end;

end.
