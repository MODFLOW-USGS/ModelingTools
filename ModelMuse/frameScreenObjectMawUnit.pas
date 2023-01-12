unit frameScreenObjectMawUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectUnit, Vcl.ComCtrls,
  Vcl.Grids, RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask,
  JvExMask, JvSpin, Vcl.ExtCtrls, JvExStdCtrls, JvCombobox, JvListComb,
  JvToolEdit, frameGridUnit, UndoItemsScreenObjects, GrayTabs, JvExControls,
  JvPageList, JvExComCtrls, JvPageListTreeView, frameMawGwtConcentrationsUnit;

type
  TWellScreenColumn = (wscTop, wscBottom, wscSkinK, wscSkinRadius);

  TWellFlow = (wfStartTime, wfEndTime, wfStatus, wfRate, wfSpecifiedHead,
    wfFlowingWell, wfFlowingWellElev, wfFlowingWellCond, wfFlowingWellReductionLength,
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
    tabGwt: TTabSheet;
    splSplit: TSplitter;
    tvGwt: TJvPageListTreeView;
    jplGwt: TJvPageList;
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
    FOnCheckPestCell: TSelectCellEvent;
    FGwtFrameList: TMawGwtObjectList;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
    property OnCheckPestCell: TSelectCellEvent read FOnCheckPestCell
      write FOnCheckPestCell;
    { Public declarations }
  end;

//var
//  frameScreenObjectMAW: TframeScreenObjectMAW;

implementation

uses
  ScreenObjectUnit, ModflowMawUnit, System.Math, GoPhastTypes, frmGoPhastUnit,
  frmCustomGoPhastUnit, Mt3dmsChemSpeciesUnit;

resourcestring
  StrStatus = 'Status (status)';
  StrFlowRate = 'Flow rate (rate)';
  StrSpecifiedHead = 'Specified head (well_head)';
  StrFlowingWellFW = 'Flowing well (FW FLOWING_WELL)';
  StrFWElevation = 'FW elevation (fwelev)';
  StrFWConductance = 'FW conductance (fwcond)';
  StrFWRedLength = 'FW reduction length (fwrlen)';
  StrRateLimitation = 'Rate limitation (SHUT_OFF, RATE_SCALING)';
  StrPumpElevation = 'Pump elevation (pump_elevation)';
  StrScalingLength = 'Scaling length (scaling_length)';
  StrMinFlowRate = 'Min flow rate (minrate)';
  StrMaxFlowRate = 'Max flow rate (maxrate)';
  StrUseHeadLimit = 'Use head limit (HEAD_LIMIT)';
  StrLimitingHead = 'Limiting head (head_limit)';
  StrScreenTop = 'Screen top (scrn_top)';
  StrScreenBottom = 'Screen bottom (scrn_bot)';
  StrSkinK = 'Skin K (hk_skin)';
  StrSkinRadius = 'Skin radius (radius_skin)';

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

constructor TframeScreenObjectMAW.Create(AOwner: TComponent);
begin
  inherited;
  FGwtFrameList := TMawGwtObjectList.Create;
end;

destructor TframeScreenObjectMAW.Destroy;
begin
  FGwtFrameList.Free;
  inherited;
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
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
  APage: TJvStandardPage;
  AGwtFrame: TframeMawGwtConcentrations;
  ANode: TJvPageIndexNode;
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
              rdgModflowBoundary.RealValue[Ord(wfStartTime), TimeIndex+1 + PestRowOffset]
                := MawItem.StartTime;
              rdgModflowBoundary.RealValue[Ord(wfEndTime), TimeIndex+1 + PestRowOffset]
                := MawItem.EndTime;
              rdgModflowBoundary.ItemIndex[Ord(wfStatus), TimeIndex+1 + PestRowOffset]
                := Ord(MawItem.MawStatus);
              rdgModflowBoundary.Cells[Ord(wfRate), TimeIndex+1 + PestRowOffset]
                := MawItem.Rate;
              rdgModflowBoundary.Cells[Ord(wfSpecifiedHead), TimeIndex+1 + PestRowOffset]
                := MawItem.WellHead;
              rdgModflowBoundary.Checked[Ord(wfFlowingWell), TimeIndex+1 + PestRowOffset]
                := (MawItem.FlowingWell = fwFlowing);
              rdgModflowBoundary.Cells[Ord(wfFlowingWellElev), TimeIndex+1 + PestRowOffset]
                := MawItem.FlowingWellElevation;
              rdgModflowBoundary.Cells[Ord(wfFlowingWellCond), TimeIndex+1 + PestRowOffset]
                := MawItem.FlowingWellConductance;
              rdgModflowBoundary.Cells[Ord(wfFlowingWellReductionLength), TimeIndex+1 + PestRowOffset]
                := MawItem.FlowingWellReductionLength;
              rdgModflowBoundary.ItemIndex[Ord(wtRateLimitation), TimeIndex+1 + PestRowOffset]
                := Ord(MawItem.RateLimitation);
              rdgModflowBoundary.Cells[Ord(wtPumpElev), TimeIndex+1 + PestRowOffset]
                := MawItem.PumpElevation;
              rdgModflowBoundary.Cells[Ord(wtScalingLength), TimeIndex+1 + PestRowOffset]
                := MawItem.ScalingLength;
              rdgModflowBoundary.Cells[Ord(wtMinRate), TimeIndex+1 + PestRowOffset]
                := MawItem.MinRate;
              rdgModflowBoundary.Cells[Ord(wtMaxRate), TimeIndex+1 + PestRowOffset]
                := MawItem.MaxRate;
              rdgModflowBoundary.Checked[Ord(wtHeadLimitChoice), TimeIndex+1 + PestRowOffset]
                := MawItem.HeadLimitChoice;
              rdgModflowBoundary.Cells[Ord(wtHeadLimit), TimeIndex+1 + PestRowOffset]
                := MawItem.HeadLimit;
            end;

            PestModifier[rdgModflowBoundary, Ord(wfRate)] := MawBound.PestRateFormula;
            PestMethod[rdgModflowBoundary, Ord(wfRate)] := MawBound.PestRateMethod;

            PestModifier[rdgModflowBoundary, Ord(wfSpecifiedHead)] := MawBound.PestWellHeadFormula;
            PestMethod[rdgModflowBoundary, Ord(wfSpecifiedHead)] := MawBound.PestWellHeadMethod;

            PestModifier[rdgModflowBoundary, Ord(wfFlowingWellElev)] := MawBound.PestFlowingWellElevationFormula;
            PestMethod[rdgModflowBoundary, Ord(wfFlowingWellElev)] := MawBound.PestFlowingWellElevationMethod;

            PestModifier[rdgModflowBoundary, Ord(wfFlowingWellCond)] := MawBound.PestFlowingWellConductanceFormula;
            PestMethod[rdgModflowBoundary, Ord(wfFlowingWellCond)] := MawBound.PestFlowingWellConductanceMethod;

            PestModifier[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)] := MawBound.PestFlowingWellReductionLengthFormula;
            PestMethod[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)] := MawBound.PestFlowingWellReductionLengthMethod;

            PestModifier[rdgModflowBoundary, Ord(wtPumpElev)] := MawBound.PestPumpElevationFormula;
            PestMethod[rdgModflowBoundary, Ord(wtPumpElev)] := MawBound.PestPumpElevationMethod;

            PestModifier[rdgModflowBoundary, Ord(wtScalingLength)] := MawBound.PestScalingLengthFormula;
            PestMethod[rdgModflowBoundary, Ord(wtScalingLength)] := MawBound.PestScalingLengthMethod;

            PestModifier[rdgModflowBoundary, Ord(wtMinRate)] := MawBound.PestMinRateFormula;
            PestMethod[rdgModflowBoundary, Ord(wtMinRate)] := MawBound.PestMinRateMethod;

            PestModifier[rdgModflowBoundary, Ord(wtMaxRate)] := MawBound.PestMaxRateFormula;
            PestMethod[rdgModflowBoundary, Ord(wtMaxRate)] := MawBound.PestMaxRateMethod;

            PestModifier[rdgModflowBoundary, Ord(wtHeadLimit)] := MawBound.PestHeadLimitFormula;
            PestMethod[rdgModflowBoundary, Ord(wtHeadLimit)] := MawBound.PestHeadLimitMethod;
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

            if FirstMawBound.PestRateFormula <> MawBound.PestRateFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wfRate)] := False
            end;
            if FirstMawBound.PestRateMethod <> MawBound.PestRateMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wfRate)] := False;
            end;

            if FirstMawBound.PestWellHeadFormula <> MawBound.PestWellHeadFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wfSpecifiedHead)] := False
            end;
            if FirstMawBound.PestWellHeadMethod <> MawBound.PestWellHeadMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wfSpecifiedHead)] := False;
            end;

            if FirstMawBound.PestFlowingWellElevationFormula <> MawBound.PestFlowingWellElevationFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wfFlowingWellElev)] := False
            end;
            if FirstMawBound.PestFlowingWellElevationMethod <> MawBound.PestFlowingWellElevationMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wfFlowingWellElev)] := False;
            end;

            if FirstMawBound.PestFlowingWellConductanceFormula <> MawBound.PestFlowingWellConductanceFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wfFlowingWellCond)] := False
            end;
            if FirstMawBound.PestFlowingWellConductanceMethod <> MawBound.PestFlowingWellConductanceMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wfFlowingWellCond)] := False;
            end;

            if FirstMawBound.PestFlowingWellReductionLengthFormula <> MawBound.PestFlowingWellReductionLengthFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)] := False
            end;
            if FirstMawBound.PestFlowingWellReductionLengthMethod <> MawBound.PestFlowingWellReductionLengthMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)] := False;
            end;

            if FirstMawBound.PestPumpElevationFormula <> MawBound.PestPumpElevationFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wtPumpElev)] := False
            end;
            if FirstMawBound.PestPumpElevationMethod <> MawBound.PestPumpElevationMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wtPumpElev)] := False;
            end;

            if FirstMawBound.PestScalingLengthFormula <> MawBound.PestScalingLengthFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wtScalingLength)] := False
            end;
            if FirstMawBound.PestScalingLengthMethod <> MawBound.PestScalingLengthMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wtScalingLength)] := False;
            end;

            if FirstMawBound.PestMinRateFormula <> MawBound.PestMinRateFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wtMinRate)] := False
            end;
            if FirstMawBound.PestMinRateMethod <> MawBound.PestMinRateMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wtMinRate)] := False;
            end;

            if FirstMawBound.PestMaxRateFormula <> MawBound.PestMaxRateFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wtMaxRate)] := False
            end;
            if FirstMawBound.PestMaxRateMethod <> MawBound.PestMaxRateMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wtMaxRate)] := False;
            end;

            if FirstMawBound.PestHeadLimitFormula <> MawBound.PestHeadLimitFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(wtHeadLimit)] := False
            end;
            if FirstMawBound.PestHeadLimitMethod <> MawBound.PestHeadLimitMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(wtHeadLimit)] := False;
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

    tabGWT.TabVisible := frmGoPhast.PhastModel.GwtUsed;
    if tabGWT.TabVisible then
    begin
      tvGwt.Items.Clear;
      for SpeciesIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
      begin
        ASpecies := frmGoPhast.PhastModel.MobileComponents[SpeciesIndex];
        if SpeciesIndex >= jplGwt.PageCount then
        begin
          APage := TJvStandardPage.Create(self);
          APage.PageList := jplGwt;
          AGwtFrame := TframeMawGwtConcentrations.Create(nil);
          FGwtFrameList.Add(AGwtFrame);
          AGwtFrame.Parent := APage;
          AGwtFrame.Align := alClient;
        end
        else
        begin
          AGwtFrame := FGwtFrameList[SpeciesIndex];
        end;
        ANode := tvGwt.Items.Add(nil, ASpecies.Name) as TJvPageIndexNode;
        ANode.PageIndex := SpeciesIndex;
        AGwtFrame.GetData(ScreenObjectList, SpeciesIndex);
        if SpeciesIndex = 0 then
        begin
          ANode.Selected := True;
        end;
      end;
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
    for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex in [wfRate, wfSpecifiedHead, wfFlowingWellElev,
        wfFlowingWellCond, wfFlowingWellReductionLength, wtPumpElev,
        wtScalingLength, wtMinRate, wtMaxRate, wtHeadLimit] do
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
  if Assigned(rdgModflowBoundary.OnSelectCell) then
  begin
    rdgModflowBoundary.OnSelectCell(rdgModflowBoundary, ACol, ARow, CanSelect);
  end;
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
  for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset
    to rdgModflowBoundary.RowCount -1 do
  begin
    for ColIndex in [wfRate, wfSpecifiedHead, wfFlowingWellElev,
      wfFlowingWellCond, wfFlowingWellReductionLength, wtPumpElev,
      wtScalingLength, wtMinRate, wtMaxRate, wtHeadLimit] do
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
  for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset
    to rdgModflowBoundary.RowCount -1 do
  begin
    ShouldEnable := rdgModflowBoundary.IsSelectedCell(Ord(wfStatus),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboStatus.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset
    to rdgModflowBoundary.RowCount -1 do
  begin
    ShouldEnable := rdgModflowBoundary.IsSelectedCell(Ord(wfFlowingWell),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  cbFlowingWell.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset
    to rdgModflowBoundary.RowCount -1 do
  begin
    ShouldEnable := rdgModflowBoundary.IsSelectedCell(Ord(wtRateLimitation),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboRateLimitation.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset
    to rdgModflowBoundary.RowCount -1 do
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
  if Assigned(OnCheckPestCell) then
  begin
    OnCheckPestCell(Sender, ACol, ARow, CanSelect);
  end;
end;

procedure TframeScreenObjectMAW.rdgModflowBoundarySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  SpeciesIndex: Integer;
begin
  inherited;
  UpdateNextTimeCell(rdgModflowBoundary, ACol, ARow);
  Edited;
  FWellTimeDataCleared := False;

  seNumberOfTimes.AsInteger := rdgModflowBoundary.RowCount -1 - PestRowOffset;
  if TWellFlow(ACol) in [wfStatus, wfFlowingWell, wtRateLimitation, wtHeadLimitChoice] then
  begin
    rdgModflowBoundary.Invalidate;
  end;

  if (ARow >= rdgModflowBoundary.FixedRows + PestRowOffset)
    and (ACol in [Ord(wfStartTime), Ord(wfEndTime)]) then
  begin
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.Cells[ACol, ARow]
        := rdgModflowBoundary.Cells[ACol, ARow];
    end;
  end;
end;

procedure TframeScreenObjectMAW.rdgModflowBoundaryStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  FWellTimeDataCleared := False;
end;

procedure TframeScreenObjectMAW.seNumberOfTimesChange(Sender: TObject);
var
  SpeciesIndex: Integer;
begin
  inherited;
  rdgModflowBoundary.RowCount := Max(2, seNumberOfTimes.AsInteger + 1) + PestRowOffset;
  if seNumberOfTimes.AsInteger = 0 then
  begin
    ClearGrid(rdgModflowBoundary);
  end;
  for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
  begin
    FGwtFrameList[SpeciesIndex].rdgConcentrations.RowCount := rdgModflowBoundary.RowCount;
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
  SpeciesIndex: Integer;
  AGwtFrame: TframeMawGwtConcentrations;
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

      if PestModifierAssigned[rdgModflowBoundary, Ord(wfRate)] then
      begin
        Boundary.PestRateFormula := PestModifier[rdgModflowBoundary, Ord(wfRate)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wfRate)] then
      begin
        Boundary.PestRateMethod := PestMethod[rdgModflowBoundary, Ord(wfRate)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wfSpecifiedHead)] then
      begin
        Boundary.PestWellHeadFormula := PestModifier[rdgModflowBoundary, Ord(wfSpecifiedHead)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wfSpecifiedHead)] then
      begin
        Boundary.PestWellHeadMethod := PestMethod[rdgModflowBoundary, Ord(wfSpecifiedHead)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wfFlowingWellElev)] then
      begin
        Boundary.PestFlowingWellElevationFormula := PestModifier[rdgModflowBoundary, Ord(wfFlowingWellElev)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wfFlowingWellElev)] then
      begin
        Boundary.PestFlowingWellElevationMethod := PestMethod[rdgModflowBoundary, Ord(wfFlowingWellElev)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wfFlowingWellCond)] then
      begin
        Boundary.PestFlowingWellConductanceFormula := PestModifier[rdgModflowBoundary, Ord(wfFlowingWellCond)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wfFlowingWellCond)] then
      begin
        Boundary.PestFlowingWellConductanceMethod := PestMethod[rdgModflowBoundary, Ord(wfFlowingWellCond)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)] then
      begin
        Boundary.PestFlowingWellReductionLengthFormula := PestModifier[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)] then
      begin
        Boundary.PestFlowingWellReductionLengthMethod := PestMethod[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wtPumpElev)] then
      begin
        Boundary.PestPumpElevationFormula := PestModifier[rdgModflowBoundary, Ord(wtPumpElev)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wtPumpElev)] then
      begin
        Boundary.PestPumpElevationMethod := PestMethod[rdgModflowBoundary, Ord(wtPumpElev)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wtScalingLength)] then
      begin
        Boundary.PestScalingLengthFormula := PestModifier[rdgModflowBoundary, Ord(wtScalingLength)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wtScalingLength)] then
      begin
        Boundary.PestScalingLengthMethod := PestMethod[rdgModflowBoundary, Ord(wtScalingLength)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wtMinRate)] then
      begin
        Boundary.PestMinRateFormula := PestModifier[rdgModflowBoundary, Ord(wtMinRate)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wtMinRate)] then
      begin
        Boundary.PestMinRateMethod := PestMethod[rdgModflowBoundary, Ord(wtMinRate)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wtMaxRate)] then
      begin
        Boundary.PestMaxRateFormula := PestModifier[rdgModflowBoundary, Ord(wtMaxRate)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wtMaxRate)] then
      begin
        Boundary.PestMaxRateMethod := PestMethod[rdgModflowBoundary, Ord(wtMaxRate)];
      end;

      if PestModifierAssigned[rdgModflowBoundary, Ord(wtHeadLimit)] then
      begin
        Boundary.PestHeadLimitFormula := PestModifier[rdgModflowBoundary, Ord(wtHeadLimit)];
      end;
      if PestMethodAssigned[rdgModflowBoundary, Ord(wtHeadLimit)] then
      begin
        Boundary.PestHeadLimitMethod := PestMethod[rdgModflowBoundary, Ord(wtHeadLimit)];
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
          MawItem.StartTime := rdgModflowBoundary.RealValueDefault[Ord(wfStartTime), TimeIndex+1 + PestRowOffset, 0];
          MawItem.EndTime := rdgModflowBoundary.RealValueDefault[Ord(wfEndTime), TimeIndex+1 + PestRowOffset, 0];
          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(wfStatus), TimeIndex+1 + PestRowOffset];
          if ItemIndex >= 0 then
          begin
            MawItem.MawStatus := TMawStatus(ItemIndex);
          end;
          MawItem.Rate := NonBlank(rdgModflowBoundary.Cells[Ord(wfRate), TimeIndex+1 + PestRowOffset]);
          MawItem.WellHead := NonBlank(rdgModflowBoundary.Cells[Ord(wfSpecifiedHead), TimeIndex+1 + PestRowOffset]);
          ItemBool := rdgModflowBoundary.Checked[Ord(wfFlowingWell), TimeIndex+1 + PestRowOffset];
          MawItem.FlowingWell := TFlowingWell(ItemBool);
          MawItem.FlowingWellElevation := NonBlank(rdgModflowBoundary.Cells[Ord(wfFlowingWellElev), TimeIndex+1 + PestRowOffset]);
          MawItem.FlowingWellConductance := NonBlank(rdgModflowBoundary.Cells[Ord(wfFlowingWellCond), TimeIndex+1 + PestRowOffset]);
          MawItem.FlowingWellReductionLength := NonBlank(rdgModflowBoundary.Cells[Ord(wfFlowingWellReductionLength), TimeIndex+1 + PestRowOffset]);
          ItemIndex := rdgModflowBoundary.ItemIndex[Ord(wtRateLimitation), TimeIndex+1 + PestRowOffset];
          if ItemIndex >= 0 then
          begin
            MawItem.RateLimitation := TRateLimitation(ItemIndex);
          end;
          MawItem.PumpElevation := NonBlank(rdgModflowBoundary.Cells[Ord(wtPumpElev), TimeIndex+1 + PestRowOffset]);
          MawItem.ScalingLength := NonBlank(rdgModflowBoundary.Cells[Ord(wtScalingLength), TimeIndex+1 + PestRowOffset]);
          MawItem.MinRate := NonBlank(rdgModflowBoundary.Cells[Ord(wtMinRate), TimeIndex+1 + PestRowOffset]);
          MawItem.MaxRate := NonBlank(rdgModflowBoundary.Cells[Ord(wtMaxRate), TimeIndex+1 + PestRowOffset]);
          MawItem.HeadLimitChoice := rdgModflowBoundary.Checked[Ord(wtHeadLimitChoice), TimeIndex+1 + PestRowOffset];
          MawItem.HeadLimit := NonBlank(rdgModflowBoundary.Cells[Ord(wtHeadLimit), TimeIndex+1 + PestRowOffset]);
        end;
      end;
    end;
  end;
  if tabGWT.TabVisible then
  begin
    for SpeciesIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
    begin
      AGwtFrame := FGwtFrameList[SpeciesIndex];
      AGwtFrame.setData(List, SpeciesIndex);
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
    seNumberOfTimes.AsInteger := rdgModflowBoundary.RowCount - 1 - PestRowOffset;
  end;
end;

procedure TframeScreenObjectMAW.ClearSelectedRow;
var
  ColIndex: Integer;
  SpeciesIndex: Integer;
  Grid: TRbwDataGrid4;
begin
  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Cells[ColIndex, rdgModflowBoundary.SelectedRow] := '';
    rdgModflowBoundary.Checked[ColIndex, rdgModflowBoundary.SelectedRow] := False;
    rdgModflowBoundary.Objects[ColIndex, rdgModflowBoundary.SelectedRow] := nil;
  end;
  for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
  begin
    Grid := FGwtFrameList[SpeciesIndex].rdgConcentrations;
    for ColIndex := 0 to Grid.ColCount - 1 do
    begin
      Grid.Cells[ColIndex, rdgModflowBoundary.SelectedRow] := '';
      Grid.Checked[ColIndex, rdgModflowBoundary.SelectedRow] := False;
      Grid.Objects[ColIndex, rdgModflowBoundary.SelectedRow] := nil;
    end;
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
    seNumberOfTimes.AsInteger := 0;
    seNumberOfTimesChange(seNumberOfTimes);

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
    rdgModflowBoundary.Cells[Ord(wfFlowingWellReductionLength), 0] := StrFWRedLength;
    rdgModflowBoundary.Cells[Ord(wtRateLimitation), 0] := StrRateLimitation;
    rdgModflowBoundary.Cells[Ord(wtPumpElev), 0] := StrPumpElevation;
    rdgModflowBoundary.Cells[Ord(wtScalingLength), 0] := StrScalingLength;
    rdgModflowBoundary.Cells[Ord(wtMinRate), 0] := StrMinFlowRate;
    rdgModflowBoundary.Cells[Ord(wtMaxRate), 0] := StrMaxFlowRate;
    rdgModflowBoundary.Cells[Ord(wtHeadLimitChoice), 0] := StrUseHeadLimit;
    rdgModflowBoundary.Cells[Ord(wtHeadLimit), 0] := StrLimitingHead;

    rdgModflowBoundary.UseSpecialFormat[0, PestModifierRow] := True;
    rdgModflowBoundary.UseSpecialFormat[0, PestMethodRow] := True;
    rdgModflowBoundary.SpecialFormat[0, PestModifierRow] := rcf4String;
    rdgModflowBoundary.SpecialFormat[0, PestMethodRow] := rcf4String;
    rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
    rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;

    PestMethod[rdgModflowBoundary, Ord(wfRate)] :=
      TMawBoundary.DefaultBoundaryMethod(MawRatePosition);
    PestMethod[rdgModflowBoundary, Ord(wfSpecifiedHead)] :=
      TMawBoundary.DefaultBoundaryMethod(MawWellHeadPosition);
    PestMethod[rdgModflowBoundary, Ord(wfFlowingWellElev)] :=
      TMawBoundary.DefaultBoundaryMethod(MawFlowingWellElevationPosition);
    PestMethod[rdgModflowBoundary, Ord(wfFlowingWellCond)] :=
      TMawBoundary.DefaultBoundaryMethod(MawFlowingWellConductancePosition);
    PestMethod[rdgModflowBoundary, Ord(wfFlowingWellReductionLength)] :=
      TMawBoundary.DefaultBoundaryMethod(MawFlowingWellReductionLengthPosition);
    PestMethod[rdgModflowBoundary, Ord(wtPumpElev)] :=
      TMawBoundary.DefaultBoundaryMethod(MawPumpElevationPosition);
    PestMethod[rdgModflowBoundary, Ord(wtScalingLength)] :=
      TMawBoundary.DefaultBoundaryMethod(MawScalingLengthPosition);
    PestMethod[rdgModflowBoundary, Ord(wtMinRate)] :=
      TMawBoundary.DefaultBoundaryMethod(MawMinRatePosition);
    PestMethod[rdgModflowBoundary, Ord(wtMaxRate)] :=
      TMawBoundary.DefaultBoundaryMethod(MawMaxRatePosition);
    PestMethod[rdgModflowBoundary, Ord(wtHeadLimit)] :=
      TMawBoundary.DefaultBoundaryMethod(MawHeadLimitPosition);
  finally
    rdgModflowBoundary.EndUpdate
  end;
  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := False;
  end;

  {$IF CompilerVersion > 28}
  comboStatus.Items.ClearAndResetID;
  {$ENDIF}
  comboStatus.Items.Assign(rdgModflowBoundary.Columns[Ord(wfStatus)].PickList);
  {$IF CompilerVersion > 28}
  comboRateLimitation.Items.ClearAndResetID;
  {$ENDIF}
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
    for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset to
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
    for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset to
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
var
  SpeciesIndex: Integer;
begin
  inherited;
  if rdgModflowBoundary.SelectedRow >= rdgModflowBoundary.FixedRows + PestRowOffset  then
  begin
    if rdgModflowBoundary.RowCount > rdgModflowBoundary.FixedRows + 1 + PestRowOffset then
    begin
      ClearSelectedRow;
      for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
      begin
        FGwtFrameList[SpeciesIndex].rdgConcentrations.DeleteRow(rdgModflowBoundary.SelectedRow);
      end;
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
var
  SpeciesIndex: Integer;
begin
  if rdgModflowBoundary.SelectedRow >= rdgModflowBoundary.FixedRows + PestRowOffset  then
  begin
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.InsertRow(rdgModflowBoundary.SelectedRow);
    end;
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
  if ARow >= 1 + PestRowOffset then
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
        // Formerly, head was exported for inactive cells.
        // Was this to compensate for a bug in MODFLOW that has now been fixed?
//          CanSelect := MawStatus in [mwInactive, mwConstantHead];
          CanSelect := MawStatus = mwConstantHead;
        end;
      wfFlowingWell:
        begin
          CanSelect := MawStatus in [mwActive, mwConstantHead];
        end;
      wfFlowingWellElev, wfFlowingWellCond, wfFlowingWellReductionLength:
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
