unit frameScreenObjectMawUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectUnit, Vcl.ComCtrls,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, JvExStdCtrls, JvCombobox, JvListComb,
  JvToolEdit, frameGridUnit, UndoItemsScreenObjects, GrayTabs;

type
  TWellScreenColumn = (wscTop, wscBottom, wscSkinK, wscSkinRadius);

  TWellFlow = (wfStartTime, wfEndTime, wfStatus, wfRate, wfSpecifiedHead,
    wfFlowingWell, wfFlowingWellElev, wfFlowingWellCond,
    wtRateLimitation, wtPumpElev, wtScalingLength,
    wtMinRate, wtMaxRate, wtHeadLimitChoice, wtHeadLimit);

  TframeScreenObjectMAW = class(TframeScreenObject)
    pnlBottom: TPanel;
    lblNumTimes: TLabel;
    seNumberOfTimes: TJvSpinEdit;
    btnDelete: TBitBtn;
    btnInsert: TBitBtn;
    pnlGrid: TPanel;
    pnlEditGrid: TPanel;
    lblFormula: TLabel;
    rdeFormula: TRbwDataEntry;
    rdgModflowBoundary: TRbwDataGrid4;
    pnlTop: TPanel;
    pnlCaption: TPanel;
    pgcMain: TPageControl;
    tabWell: TTabSheet;
    tabTransient: TTabSheet;
    lblRadius: TLabel;
    lblBottom: TLabel;
    edWellRadius: TJvComboEdit;
    edBottom: TJvComboEdit;
    edStartingHead: TJvComboEdit;
    lblStartingHead: TLabel;
    lblConductanceEquation: TLabel;
    comboConductEq: TJvImageComboBox;
    tabWellScreens: TTabSheet;
    frameWellScreens: TframeGrid;
    comboStatus: TJvImageComboBox;
    cbFlowingWell: TCheckBox;
    lblStatus: TLabel;
    comboRateLimitation: TJvImageComboBox;
    cbHeadLimit: TCheckBox;
    lblRateLimitation: TLabel;
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameWellScreensGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure edWellRadiusChange(Sender: TObject);
    procedure edBottomChange(Sender: TObject);
    procedure edStartingHeadChange(Sender: TObject);
    procedure comboConductEqChange(Sender: TObject);
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgModflowBoundaryBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure rdgModflowBoundaryStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure rdgModflowBoundaryColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgModflowBoundaryHorizontalScroll(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure rdeFormulaChange(Sender: TObject);
    procedure comboStatusChange(Sender: TObject);
    procedure comboRateLimitationChange(Sender: TObject);
    procedure cbFlowingWellClick(Sender: TObject);
    procedure cbHeadLimitClick(Sender: TObject);
    procedure rdgModflowBoundaryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FOnEdited: TNotifyEvent;
    FGettingData: Boolean;
    FWellScreensCleared: Boolean;
    FWellTimeDataCleared: Boolean;
    { Private declarations }
    procedure Edited;
    procedure CanSelectTimeCell(ARow: Integer; ACol: Integer; var CanSelect: Boolean);
    procedure InitializeLabels;
    procedure ClearSelectedRow;
    procedure UpdateNumTimes;
    procedure ApplyComboTextToColumn(ColIndex: TWellFlow; NewText: string);
    procedure ApplyCheckBoxStateToColumn(ColIndex: TWellFlow; NewState: TCheckBoxState);
    procedure UpdateTransientEditor;
  protected
    procedure LayoutMultiRowEditControls;
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
    { Public declarations }
  end;

//var
//  frameScreenObjectMAW: TframeScreenObjectMAW;

implementation

uses
  ScreenObjectUnit, ModflowMawUnit, System.Math, GoPhastTypes, frmGoPhastUnit,
  frmCustomGoPhastUnit;

resourcestring
  StrStatus = 'Status';
  StrFlowRate = 'Flow rate';
  StrSpecifiedHead = 'Specified head';
  StrFlowingWellFW = 'Flowing well (FW)';
  StrFWElevation = 'FW elevation';
  StrFWConductance = 'FW conductance';
  StrRateLimitation = 'Rate limitation';
  StrPumpElevation = 'Pump elevation';
  StrScalingLength = 'Scaling length';
  StrMinFlowRate = 'Min flow rate';
  StrMaxFlowRate = 'Max flow rate';
  StrUseHeadLimit = 'Use head limit';
  StrLimitingHead = 'Limiting head';
  StrScreenTop = 'Screen top';
  StrScreenBottom = 'Screen bottom';
  StrSkinK = 'Skin K';
  StrSkinRadius = 'Skin radius';

{$R *.dfm}

{ TframeScreenObjectMAW }

procedure TframeScreenObjectMAW.comboConductEqChange(Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectMAW.comboRateLimitationChange(Sender: TObject);
begin
  inherited;
  ApplyComboTextToColumn(wtRateLimitation, comboRateLimitation.Text);
end;

procedure TframeScreenObjectMAW.comboStatusChange(Sender: TObject);
begin
  inherited;
  ApplyComboTextToColumn(wfStatus, comboStatus.Text);
end;

procedure TframeScreenObjectMAW.Edited;
begin
  if Assigned(FOnEdited) and not FGettingData then
  begin
    FOnEdited(self);
  end;
end;

procedure TframeScreenObjectMAW.edStartingHeadChange(Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectMAW.edBottomChange(Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectMAW.edWellRadiusChange(Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectMAW.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectMAW.frameWellScreensGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  Edited;
  FWellScreensCleared := False;
end;

procedure TframeScreenObjectMAW.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  FoundFirst: Boolean;
  MawBound: TMawBoundary;
  ScreenIndex: Integer;
  AWellScreen: TMawWellScreenItem;
  TimeIndex: Integer;
  MawItem: TMawItem;
  FirstMawBound: TMawBoundary;
begin
  pgcMain.ActivePageIndex := 0;
  FGettingData := True;
  try
    InitializeLabels;
    rdgModflowBoundary.BeginUpdate;
    frameWellScreens.Grid.BeginUpdate;
    try
      FirstMawBound := nil;
      FoundFirst := False;
      FWellScreensCleared := False;
      FWellTimeDataCleared := False;
      for ScreenObjectIndex := 0 to ScreenObjectList.Count - 1 do
      begin
        AScreenObject := ScreenObjectList[ScreenObjectIndex].ScreenObject;
        if (AScreenObject.ModflowMawBoundary <> nil)
          and AScreenObject.ModflowMawBoundary.Used then
        begin
          MawBound := AScreenObject.ModflowMawBoundary;
          if not FoundFirst then
          begin
            FirstMawBound := MawBound;
            FoundFirst := True;
            edWellRadius.Text := MawBound.Radius;
            edBottom.Text := MawBound.Bottom;
            edStartingHead.Text := MawBound.InitialHead;
            comboConductEq.ItemIndex := Ord(MawBound.ConductanceMethod);

            frameWellScreens.seNumber.AsInteger := MawBound.WellScreens.Count;
            for ScreenIndex := 0 to MawBound.WellScreens.Count - 1 do
            begin
              AWellScreen := MawBound.WellScreens[ScreenIndex]
                as TMawWellScreenItem;
              frameWellScreens.Grid.Cells[Ord(wscTop), ScreenIndex+1] :=
                AWellScreen.ScreenTop;
              frameWellScreens.Grid.Cells[Ord(wscBottom), ScreenIndex+1] :=
                AWellScreen.ScreenBottom;
              frameWellScreens.Grid.Cells[Ord(wscSkinK), ScreenIndex+1] :=
                 AWellScreen.SkinK;
              frameWellScreens.Grid.Cells[Ord(wscSkinRadius), ScreenIndex+1] :=
                AWellScreen.SkinRadius;
            end;

            seNumberOfTimes.AsInteger := MawBound.Values.Count;
            for TimeIndex := 0 to MawBound.Values.Count - 1 do
            begin
              MawItem := MawBound.Values[TimeIndex] as TMawItem;
              rdgModflowBoundary.RealValue[Ord(wfStartTime), TimeIndex+1]
                := MawItem.StartTime;
              rdgModflowBoundary.RealValue[Ord(wfEndTime), TimeIndex+1]
                := MawItem.EndTime;
              rdgModflowBoundary.ItemIndex[Ord(wfStatus), TimeIndex+1]
                := Ord(MawItem.MawStatus);
              rdgModflowBoundary.Cells[Ord(wfRate), TimeIndex+1]
                := MawItem.Rate;
              rdgModflowBoundary.Cells[Ord(wfSpecifiedHead), TimeIndex+1]
                := MawItem.WellHead;
              rdgModflowBoundary.Checked[Ord(wfFlowingWell), TimeIndex+1]
                := (MawItem.FlowingWell = fwFlowing);
              rdgModflowBoundary.Cells[Ord(wfFlowingWellElev), TimeIndex+1]
                := MawItem.FlowingWellElevation;
              rdgModflowBoundary.Cells[Ord(wfFlowingWellCond), TimeIndex+1]
                := MawItem.FlowingWellConductance;
              rdgModflowBoundary.ItemIndex[Ord(wtRateLimitation), TimeIndex+1]
                := Ord(MawItem.RateLimitation);
              rdgModflowBoundary.Cells[Ord(wtPumpElev), TimeIndex+1]
                := MawItem.PumpElevation;
              rdgModflowBoundary.Cells[Ord(wtScalingLength), TimeIndex+1]
                := MawItem.ScalingLength;
              rdgModflowBoundary.Cells[Ord(wtMinRate), TimeIndex+1]
                := MawItem.MinRate;
              rdgModflowBoundary.Cells[Ord(wtMaxRate), TimeIndex+1]
                := MawItem.MaxRate;
              rdgModflowBoundary.Checked[Ord(wtHeadLimitChoice), TimeIndex+1]
                := MawItem.HeadLimitChoice;
              rdgModflowBoundary.Cells[Ord(wtHeadLimit), TimeIndex+1]
                := MawItem.HeadLimit;
            end;
          end
          else
          begin
            if edWellRadius.Text <> MawBound.Radius then
            begin
              edWellRadius.Text := ''
            end;
            if edBottom.Text <> MawBound.Bottom then
            begin
              edBottom.Text := ''
            end;
            if edStartingHead.Text <> MawBound.InitialHead then
            begin
              edStartingHead.Text := ''
            end;
            if comboConductEq.ItemIndex <> Ord(MawBound.ConductanceMethod) then
            begin
              comboConductEq.ItemIndex := -1;
            end;

            if not FWellScreensCleared then
            begin
              if not FirstMawBound.WellScreens.IsSame(MawBound.WellScreens) then
              begin
                FWellScreensCleared := True;
                ClearGrid(frameWellScreens.Grid);
              end;
            end;

            if not FWellTimeDataCleared then
            begin
              if not FirstMawBound.Values.IsSame(MawBound.Values) then
              begin
                FWellTimeDataCleared := True;
                ClearGrid(rdgModflowBoundary);
              end;
            end;

          end;
        end;
      end;

    finally
      rdgModflowBoundary.EndUpdate;
      frameWellScreens.Grid.EndUpdate;
    end;
  finally
    FGettingData := False;
  end;

end;

procedure TframeScreenObjectMAW.rdeFormulaChange(Sender: TObject);
var
  RowIndex: Integer;
  ColIndex: TWellFlow;
begin
  inherited;
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex in [wfRate, wfSpecifiedHead, wfFlowingWellElev,
        wfFlowingWellCond, wtPumpElev, wtScalingLength, wtMinRate, wtMaxRate,
        wtHeadLimit] do
      begin
        if rdgModflowBoundary.IsSelectedCell(Ord(ColIndex), RowIndex) then
        begin
          rdgModflowBoundary.Cells[Ord(ColIndex), RowIndex] := rdeFormula.Text;
          if Assigned(rdgModflowBoundary.OnSetEditText) then
          begin
            rdgModflowBoundary.OnSetEditText(
              rdgModflowBoundary,Ord(ColIndex),RowIndex, rdeFormula.Text);
          end;
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
  UpdateTransientEditor;
end;

procedure TframeScreenObjectMAW.rdgModflowBoundaryBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  CanSelect: Boolean;
begin
  inherited;
  CanSelect := True;
  CanSelectTimeCell(ARow, ACol, CanSelect);
  if not CanSelect then
  begin
    rdgModflowBoundary.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TframeScreenObjectMAW.rdgModflowBoundaryColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectMAW.rdgModflowBoundaryHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectMAW.rdgModflowBoundaryMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: boolean;
  RowIndex: Integer;
  ColIndex: TWellFlow;
begin
  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows to rdgModflowBoundary.RowCount -1 do
  begin
    for ColIndex in [wfRate, wfSpecifiedHead, wfFlowingWellElev,
      wfFlowingWellCond, wtPumpElev, wtScalingLength, wtMinRate, wtMaxRate,
      wtHeadLimit] do
    begin
      ShouldEnable := rdgModflowBoundary.IsSelectedCell(Ord(ColIndex),RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  rdeFormula.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows to rdgModflowBoundary.RowCount -1 do
  begin
    ShouldEnable := rdgModflowBoundary.IsSelectedCell(Ord(wfStatus),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboStatus.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows to rdgModflowBoundary.RowCount -1 do
  begin
    ShouldEnable := rdgModflowBoundary.IsSelectedCell(Ord(wfFlowingWell),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  cbFlowingWell.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows to rdgModflowBoundary.RowCount -1 do
  begin
    ShouldEnable := rdgModflowBoundary.IsSelectedCell(Ord(wtRateLimitation),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboRateLimitation.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows to rdgModflowBoundary.RowCount -1 do
  begin
    ShouldEnable := rdgModflowBoundary.IsSelectedCell(Ord(wtHeadLimitChoice),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  cbHeadLimit.Enabled := ShouldEnable;
end;

procedure TframeScreenObjectMAW.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelectTimeCell(ARow, ACol, CanSelect);
end;

procedure TframeScreenObjectMAW.rdgModflowBoundarySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  UpdateNextTimeCell(rdgModflowBoundary, ACol, ARow);
  Edited;
  FWellTimeDataCleared := False;

  seNumberOfTimes.AsInteger := rdgModflowBoundary.RowCount -1;
  if TWellFlow(ACol) in [wfStatus, wfFlowingWell, wtRateLimitation, wtHeadLimitChoice] then
  begin
    rdgModflowBoundary.Invalidate;
  end;
end;

procedure TframeScreenObjectMAW.rdgModflowBoundaryStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  FWellTimeDataCleared := False;
end;

procedure TframeScreenObjectMAW.seNumberOfTimesChange(Sender: TObject);
begin
  inherited;
  rdgModflowBoundary.RowCount := Max(2, seNumberOfTimes.AsInteger + 1);
  if seNumberOfTimes.AsInteger = 0 then
  begin
    ClearGrid(rdgModflowBoundary);
  end;
end;

procedure TframeScreenObjectMAW.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TMawBoundary;
  BoundaryUsed: Boolean;
  ScreenIndex: Integer;
  AWellScreen: TMawWellScreenItem;
  TimeIndex: Integer;
  MawItem: TMawItem;
  ItemIndex: Integer;
  ItemBool: Boolean;
  function NonBlank(const Formula: string): string;
  begin
    if Formula = '' then
    begin
      result := '0';
    end
    else
    begin
      result := Formula;
    end;
  end;
begin
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
//    ScreenObject := Item.ScreenObject;
    Boundary := Item.ScreenObject.ModflowMawBoundary;
    BoundaryUsed := (Boundary <> nil) and Boundary.Used;

    if ClearAll then
    begin
      if BoundaryUsed then
      begin
        Boundary.Clear;
      end;
    end
    else if SetAll or BoundaryUsed then
    begin
      if Boundary = nil then
      begin
        Item.ScreenObject.CreateMawBoundary;
        Boundary := Item.ScreenObject.ModflowMawBoundary;
      end;

      if edWellRadius.Text <> '' then
      begin
        Boundary.Radius := edWellRadius.Text;
      end;
      if edBottom.Text <> '' then
      begin
        Boundary.Bottom := edBottom.Text;
      end;
      if edStartingHead.Text <> '' then
      begin
        Boundary.InitialHead := edStartingHead.Text;
      end;
      if comboConductEq.ItemIndex >= 0 then
      begin
        Boundary.ConductanceMethod :=
          TMawConductanceMethod(comboConductEq.ItemIndex);
      end;

      if not FWellScreensCleared then
      begin
        Boundary.WellScreens.Count := frameWellScreens.seNumber.AsInteger;
        for ScreenIndex := 0 to Boundary.WellScreens.Count - 1 do
        begin
          AWellScreen := Boundary.WellScreens[ScreenIndex]
            as TMawWellScreenItem;
          AWellScreen.ScreenTop := frameWellScreens.Grid.Cells[Ord(wscTop), ScreenIndex+1];
          AWellScreen.ScreenBottom := frameWellScreens.Grid.Cells[Ord(wscBottom), ScreenIndex+1];
          AWellScreen.SkinK := frameWellScreens.Grid.Cells[Ord(wscSkinK), ScreenIndex+1];
          AWellScreen.SkinRadius := frameWellScreens.Grid.Cells[Ord(wscSkinRadius), ScreenIndex+1];
        end;
      end;

      if not FWellTimeDataCleared then
      begin
        Boundary.Values.Count := seNumberOfTimes.AsInteger;
        for TimeIndex := 0 to Boundary.Values.Count - 1 do
        begin
          MawItem := Boundary.Values[TimeIndex] as TMawItem;
          MawItem.StartTime := rdgModflowBoundary.RealValueDefault[Ord(wfStartTime), TimeIndex+1, 0];
          MawItem.EndTime := rdgModflowBoundary.RealValueDefault[Ord(wfEndTime), TimeIndex+1, 0];
          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(wfStatus), TimeIndex+1];
          if ItemIndex >= 0 then
          begin
            MawItem.MawStatus := TMawStatus(ItemIndex);
          end;
          MawItem.Rate := NonBlank(rdgModflowBoundary.Cells[Ord(wfRate), TimeIndex+1]);
          MawItem.WellHead := NonBlank(rdgModflowBoundary.Cells[Ord(wfSpecifiedHead), TimeIndex+1]);
          ItemBool := rdgModflowBoundary.Checked[Ord(wfFlowingWell), TimeIndex+1];
          MawItem.FlowingWell := TFlowingWell(ItemBool);
          MawItem.FlowingWellElevation := NonBlank(rdgModflowBoundary.Cells[Ord(wfFlowingWellElev), TimeIndex+1]);
          MawItem.FlowingWellConductance := NonBlank(rdgModflowBoundary.Cells[Ord(wfFlowingWellCond), TimeIndex+1]);
          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(wtRateLimitation), TimeIndex+1];
          if ItemIndex >= 0 then
          begin
            MawItem.RateLimitation := TRateLimitation(ItemIndex);
          end;
          MawItem.PumpElevation := NonBlank(rdgModflowBoundary.Cells[Ord(wtPumpElev), TimeIndex+1]);
          MawItem.ScalingLength := NonBlank(rdgModflowBoundary.Cells[Ord(wtScalingLength), TimeIndex+1]);
          MawItem.MinRate := NonBlank(rdgModflowBoundary.Cells[Ord(wtMinRate), TimeIndex+1]);
          MawItem.MaxRate := NonBlank(rdgModflowBoundary.Cells[Ord(wtMaxRate), TimeIndex+1]);
          MawItem.HeadLimitChoice := rdgModflowBoundary.Checked[Ord(wtHeadLimitChoice), TimeIndex+1];
          MawItem.HeadLimit := NonBlank(rdgModflowBoundary.Cells[Ord(wtHeadLimit), TimeIndex+1]);
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectMAW.UpdateTransientEditor;
var
  TempOptions: TGridOptions;
begin
  TempOptions := rdgModflowBoundary.Options;
  try
    rdgModflowBoundary.Options := [goEditing, goAlwaysShowEditor];
    rdgModflowBoundary.UpdateEditor;
  finally
    rdgModflowBoundary.Options := TempOptions;
  end;
end;

procedure TframeScreenObjectMAW.UpdateNumTimes;
begin
  if seNumberOfTimes <> nil then
  begin
    seNumberOfTimes.AsInteger := rdgModflowBoundary.RowCount - 1;
  end;
end;

procedure TframeScreenObjectMAW.ClearSelectedRow;
var
  ColIndex: Integer;
begin
  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Cells[ColIndex, rdgModflowBoundary.SelectedRow] := '';
    rdgModflowBoundary.Checked[ColIndex, rdgModflowBoundary.SelectedRow] := False;
    rdgModflowBoundary.Objects[ColIndex, rdgModflowBoundary.SelectedRow] := nil;
  end;
end;

procedure TframeScreenObjectMAW.InitializeLabels;
var
  ColIndex: Integer;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    ClearGrid(frameWellScreens.Grid);

    frameWellScreens.Grid.Cells[Ord(wscTop), 0] := StrScreenTop;
    frameWellScreens.Grid.Cells[Ord(wscBottom), 0] := StrScreenBottom;
    frameWellScreens.Grid.Cells[Ord(wscSkinK), 0] := StrSkinK;
    frameWellScreens.Grid.Cells[Ord(wscSkinRadius), 0] := StrSkinRadius;

    ClearGrid(rdgModflowBoundary);
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillPickListWithStartTimes(rdgModflowBoundary, Ord(wfStartTime));
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillPickListWithEndTimes(rdgModflowBoundary, Ord(wfEndTime));
    rdgModflowBoundary.Cells[Ord(wfStartTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(wfEndTime), 0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(wfStatus), 0] := StrStatus;
    rdgModflowBoundary.Cells[Ord(wfRate), 0] := StrFlowRate;
    rdgModflowBoundary.Cells[Ord(wfSpecifiedHead), 0] := StrSpecifiedHead;
    rdgModflowBoundary.Cells[Ord(wfFlowingWell), 0] := StrFlowingWellFW;
    rdgModflowBoundary.Cells[Ord(wfFlowingWellElev), 0] := StrFWElevation;
    rdgModflowBoundary.Cells[Ord(wfFlowingWellCond), 0] := StrFWConductance;
    rdgModflowBoundary.Cells[Ord(wtRateLimitation), 0] := StrRateLimitation;
    rdgModflowBoundary.Cells[Ord(wtPumpElev), 0] := StrPumpElevation;
    rdgModflowBoundary.Cells[Ord(wtScalingLength), 0] := StrScalingLength;
    rdgModflowBoundary.Cells[Ord(wtMinRate), 0] := StrMinFlowRate;
    rdgModflowBoundary.Cells[Ord(wtMaxRate), 0] := StrMaxFlowRate;
    rdgModflowBoundary.Cells[Ord(wtHeadLimitChoice), 0] := StrUseHeadLimit;
    rdgModflowBoundary.Cells[Ord(wtHeadLimit), 0] := StrLimitingHead;
  finally
    rdgModflowBoundary.EndUpdate
  end;
  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := False;
  end;

  comboStatus.Items.Assign(rdgModflowBoundary.Columns[Ord(wfStatus)].PickList);
  comboRateLimitation.Items.Assign(rdgModflowBoundary.
    Columns[Ord(wtRateLimitation)].PickList);
end;

procedure TframeScreenObjectMAW.LayoutMultiRowEditControls;
var
  FormulaColumn: Integer;
begin
  inherited;
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;

  FormulaColumn := Max(FLastTimeColumn+1,rdgModflowBoundary.LeftCol);
  while TWellFlow(FormulaColumn) in [wfStatus, wfFlowingWell, wtRateLimitation,
    wtHeadLimitChoice] do
  begin
    Inc(FormulaColumn);
  end;
  LayoutControls(rdgModflowBoundary, rdeFormula, lblFormula, FormulaColumn);
//  TWellFlow = (wfStartTime, wfEndTime, wfStatus, wfRate, wfSpecifiedHead,
//    wfFlowingWell, wfFlowingWellElev, wfFlowingWellCond,
//    wtRateLimitation, wtPumpElev, wtScalingLength,
//    wtMinRate, wtMaxRate, wtHeadLimitChoice, wtHeadLimit);
  LayoutControls(rdgModflowBoundary, comboStatus, lblStatus, Ord(wfStatus));
  LayoutControls(rdgModflowBoundary, cbFlowingWell, nil, Ord(wfFlowingWell));
  LayoutControls(rdgModflowBoundary, comboRateLimitation, lblRateLimitation, Ord(wtRateLimitation));
  LayoutControls(rdgModflowBoundary, cbHeadLimit, nil, Ord(wtHeadLimitChoice));


end;

procedure TframeScreenObjectMAW.ApplyCheckBoxStateToColumn(ColIndex: TWellFlow;
  NewState: TCheckBoxState);
var
  RowIndex: Integer;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows to
      rdgModflowBoundary.RowCount - 1 do
    begin
      if rdgModflowBoundary.IsSelectedCell(Ord(ColIndex), RowIndex) then
      begin
        rdgModflowBoundary.Checked[Ord(ColIndex), RowIndex] :=
          NewState = cbChecked;
        if Assigned(rdgModflowBoundary.OnStateChange) then
        begin
          rdgModflowBoundary.OnStateChange(rdgModflowBoundary, Ord(ColIndex),
            RowIndex, NewState);
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate
  end;
  UpdateTransientEditor;
end;

procedure TframeScreenObjectMAW.ApplyComboTextToColumn(ColIndex: TWellFlow;
  NewText: string);
var
  RowIndex: Integer;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows to
      rdgModflowBoundary.RowCount - 1 do
    begin
      if rdgModflowBoundary.IsSelectedCell(Ord(ColIndex), RowIndex) then
      begin
        rdgModflowBoundary.Cells[Ord(ColIndex), RowIndex] := NewText;
        if Assigned(rdgModflowBoundary.OnSetEditText) then
        begin
          rdgModflowBoundary.OnSetEditText(rdgModflowBoundary, Ord(ColIndex),
            RowIndex, NewText);
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate
  end;
  UpdateTransientEditor;
end;

procedure TframeScreenObjectMAW.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if rdgModflowBoundary.SelectedRow >= rdgModflowBoundary.FixedRows  then
  begin
    if rdgModflowBoundary.RowCount > rdgModflowBoundary.FixedRows + 1 then
    begin
      ClearSelectedRow;
      rdgModflowBoundary.DeleteRow(rdgModflowBoundary.SelectedRow);
      UpdateNumTimes;

    end
    else
    begin
      ClearSelectedRow;
      seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger -1;
    end;
  end;
end;

procedure TframeScreenObjectMAW.btnInsertClick(Sender: TObject);
begin
  if rdgModflowBoundary.SelectedRow >= rdgModflowBoundary.FixedRows  then
  begin
    rdgModflowBoundary.InsertRow(rdgModflowBoundary.SelectedRow);
    ClearSelectedRow;
    UpdateNumTimes;
  end;
end;

procedure TframeScreenObjectMAW.CanSelectTimeCell(ARow: Integer; ACol: Integer;
  var CanSelect: Boolean);
var
  FlowCol: TWellFlow;
  MawStatus: TMawStatus;
  FlowingWell: Boolean;
  RateLimitation: TRateLimitation;
  HeadLimitChoice: Boolean;
begin
  if ARow >= 1 then
  begin
    FlowCol := TWellFlow(ACol);
    MawStatus := TMawStatus(rdgModflowBoundary.ItemIndex[Ord(wfStatus), ARow]);
    FlowingWell := rdgModflowBoundary.Checked[Ord(wfFlowingWell), ARow];
    RateLimitation := TRateLimitation(rdgModflowBoundary.ItemIndex[
      Ord(wtRateLimitation), ARow]);
    HeadLimitChoice := rdgModflowBoundary.Checked[Ord(wtHeadLimitChoice), ARow];
    case FlowCol of
      wfStartTime, wfEndTime, wfStatus:
        begin
          CanSelect := True;
        end;
      wfRate:
        begin
          CanSelect := MawStatus = mwActive;
        end;
      wfSpecifiedHead:
        begin
          CanSelect := MawStatus in [mwInactive, mwConstantHead];
        end;
      wfFlowingWell:
        begin
          CanSelect := MawStatus in [mwActive, mwConstantHead];
        end;
      wfFlowingWellElev, wfFlowingWellCond:
        begin
          CanSelect := FlowingWell
            and (MawStatus in [mwActive, mwConstantHead]);
        end;
      wtRateLimitation:
        begin
          CanSelect := MawStatus in [mwActive, mwConstantHead];
        end;
      wtPumpElev, wtScalingLength:
        begin
          CanSelect := (RateLimitation = rlScaling)
            and (MawStatus in [mwActive, mwConstantHead]);
        end;
      wtMinRate, wtMaxRate:
        begin
          CanSelect := (RateLimitation = rlShutoff)
            and (MawStatus in [mwActive, mwConstantHead]);
        end;
      wtHeadLimitChoice:
        begin
          CanSelect := (MawStatus in [mwActive, mwConstantHead])
            and (RateLimitation <> rlScaling);
        end;
      wtHeadLimit:
        begin
          CanSelect := HeadLimitChoice
            and (MawStatus in [mwActive, mwConstantHead])
            and (RateLimitation <> rlScaling);
        end;
    end;
  end;
end;

procedure TframeScreenObjectMAW.cbFlowingWellClick(Sender: TObject);
begin
  inherited;
  ApplyCheckBoxStateToColumn(wfFlowingWell, cbFlowingWell.State);
end;

procedure TframeScreenObjectMAW.cbHeadLimitClick(Sender: TObject);
begin
  inherited;
  ApplyCheckBoxStateToColumn(wtHeadLimitChoice, cbHeadLimit.State);
end;

end.
