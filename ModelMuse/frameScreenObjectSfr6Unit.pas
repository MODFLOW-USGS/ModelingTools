unit frameScreenObjectSfr6Unit;

interface

uses System.UITypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ModflowBoundaryUnit, Vcl.ComCtrls, frameGridUnit,
  UndoItemsScreenObjects, GrayTabs;

type
  TSfr6Columns = (s6cStartTime, s6cEndtime, s6cStatus, s6cStage, s6cInflow, s6cRainfall,
    s6cEvaporation, s6cRunoff, s6cRoughness, s6cUpstreamFraction, s6cDiversionStart);

  Tsfr6DiversionCol = (s6dcSegment, d6dcPriority);

  TSfr6BoundaryRows = (s6brNone, s6brReachLength, s6brReachWidth, s6brGradient,
    s6brStreambedTop, s6brStreambedThickness, s6brHydraulicConductivity{,
    s6brRoughness, s6brUpstreamFraction});


  TframeScreenObjectSfr6 = class(TframeScreenObject)
    pnlTop: TPanel;
    pnlCaption: TPanel;
    pgcSfr6: TPageControl;
    tabRates: TTabSheet;
    pnlGrid: TPanel;
    pnlEditGrid: TPanel;
    lblFormula: TLabel;
    rdeFormula: TRbwDataEntry;
    rdgModflowBoundary: TRbwDataGrid4;
    tabDownstreamSegments: TTabSheet;
    frmgrdDownstreamSegments: TframeGrid;
    tabDiversions: TTabSheet;
    frmgrdDiversions: TframeGrid;
    tabConfiguration: TTabSheet;
    pnlBottom: TPanel;
    lblNumTimes: TLabel;
    seNumberOfTimes: TJvSpinEdit;
    btnDelete: TBitBtn;
    btnInsert: TBitBtn;
    lblSegmentNumber: TLabel;
    rdgFormulas: TRbwDataGrid4;
    rdeSegmentNumber: TRbwDataEntry;
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure rdgModflowBoundaryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeFormulaChange(Sender: TObject);
    procedure rdgModflowBoundaryHorizontalScroll(Sender: TObject);
    procedure rdgModflowBoundaryColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure FrameResize(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure rdgModflowBoundaryBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure frmgrdDiversionsseNumberChange(Sender: TObject);
    procedure rdgFormulasSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frmgrdDownstreamSegmentsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frmgrdDownstreamSegmentsseNumberChange(Sender: TObject);
    procedure frmgrdDiversionsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure rdgModflowBoundaryStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure rdgModflowBoundaryEnter(Sender: TObject);
    procedure rdeSegmentNumberChange(Sender: TObject);
  private
    FSelectedText: string;
    FDeleting: Boolean;
    FDeletedCells: array of array of boolean;
    FConductanceColumn: Integer;
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    FPriorRowCount: Integer;
    FValuesCleared: Boolean;
    FOnCheckPestCell: TSelectCellEvent;
    procedure LayoutMultiRowEditControls;
    function GetDeletedCells(ACol, ARow: integer): boolean;
    procedure SetDeletedCells(ACol, ARow: integer; const Value: boolean);
    procedure InitializeGrids;
    procedure SetChanging(const Value: Boolean);
    property Changing: Boolean read FChanging write SetChanging;
    procedure DoChange;
    { Private declarations }
  public
    property ConductanceColumn: Integer read FConductanceColumn write FConductanceColumn;
    procedure ClearDeletedCells;
    property DeletedCells[ACol, ARow: integer]: boolean read GetDeletedCells
      write SetDeletedCells;
    function ConductanceCaption(DirectCaption: string): string; virtual;
    procedure InitializeNoParamFrame(
      Boundary: TModflowBoundary);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetStartTimes(Col: integer);
    procedure GetEndTimes(Col: Integer);
    procedure SetButtonCaptions;
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCheckPestCell: TSelectCellEvent read FOnCheckPestCell
      write FOnCheckPestCell;
    { Public declarations }
  end;

resourcestring
  StrSpecifiedHead = 'Specified head';

var
  frameScreenObjectSfr6: TframeScreenObjectSfr6;

implementation

{$R *.dfm}

uses frmGoPhastUnit, GoPhastTypes, frmCustomGoPhastUnit, System.Math,
  ScreenObjectUnit, ModflowSfr6Unit, PhastModelUnit, DataSetUnit;

resourcestring
  StrDiversionSegmentI = 'Diversion Segment (iconr)';
  StrFormulas = 'Formulas';
  StrReachLengthRlen = 'Reach length (rlen)';
  StrReachWidthRwid = 'Reach width (rwid)';
  StrGradientRgrd = 'Gradient (rgrd)';
  StrStreambedTopRtp = 'Streambed top (rtp)';
  StrStreambedThickness = 'Streambed thickness (rbth)';
  StrHydraulicConductivi = 'Hydraulic conductivity (rhk)';
  StrInactive = 'Inactive';
  StrActive = 'Active';

procedure TframeScreenObjectSfr6.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if (rdgModflowBoundary.RowCount > 2 + PestRowOffset)
    and (rdgModflowBoundary.Row> 0 + PestRowOffset) then
  begin
    rdgModflowBoundary.DeleteRow(rdgModflowBoundary.Row);
  end;
  seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger -1;

end;

procedure TframeScreenObjectSfr6.btnInsertClick(Sender: TObject);
begin
  inherited;
  if (rdgModflowBoundary.SelectedRow <= 0+ PestRowOffset)
    or (rdgModflowBoundary.SelectedRow >= rdgModflowBoundary.RowCount) then
  begin
    Beep;
    MessageDlg(StrYouNeedToSelectA, mtInformation, [mbOK], 0);
    Exit;
  end;
  if (seNumberOfTimes.AsInteger > 0) then
  begin
    rdgModflowBoundary.InsertRow(rdgModflowBoundary.SelectedRow);
    rdgModflowBoundary.ItemIndex[Ord(s6cStatus), rdgModflowBoundary.SelectedRow] := 1;
  end;
  FPriorRowCount := seNumberOfTimes.AsInteger +1+PestRowOffset;
  seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger +1;
end;

procedure TframeScreenObjectSfr6.DoChange;
begin
  if not Changing and Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TframeScreenObjectSfr6.ClearDeletedCells;
begin
  SetLength(FDeletedCells, 0, 0);
end;

function TframeScreenObjectSfr6.ConductanceCaption(
  DirectCaption: string): string;
begin
  result := DirectCaption;
end;

constructor TframeScreenObjectSfr6.Create(AOwner: TComponent);
begin
  inherited;
  ConductanceColumn := -1;
end;

destructor TframeScreenObjectSfr6.Destroy;
begin

  inherited;
end;

procedure TframeScreenObjectSfr6.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectSfr6.frmgrdDiversionsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectSfr6.frmgrdDiversionsseNumberChange(
  Sender: TObject);
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
begin
  inherited;
  frmgrdDiversions.seNumberChange(Sender);
  rdgModflowBoundary.ColCount := frmgrdDiversions.seNumber.AsInteger
    + Ord(s6cDiversionStart);

  for ColIndex := 0 to frmgrdDiversions.seNumber.AsInteger - 1 do
  begin
    rdgModflowBoundary.Cells[Ord(s6cDiversionStart) + ColIndex, 0]
      := Format('Diversion rate %d', [ColIndex+1]);
    AColumn := rdgModflowBoundary.Columns[Ord(s6cDiversionStart) + ColIndex];
    AColumn.WordWrapCaptions := True;
    AColumn.UseButton := True;
    AColumn.ButtonCaption := 'F()';
    AColumn.AutoAdjustColWidths := True;
    AColumn.ButtonWidth := 50;
  end;
  DoChange;
end;

procedure TframeScreenObjectSfr6.frmgrdDownstreamSegmentsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectSfr6.frmgrdDownstreamSegmentsseNumberChange(
  Sender: TObject);
begin
  inherited;
  frmgrdDownstreamSegments.seNumberChange(Sender);
  DoChange;
end;

procedure TframeScreenObjectSfr6.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
  Index: Integer;
  FoundFirst: Boolean;
  Sf6Boundary: TSfrMf6Boundary;
  DSIndex: Integer;
  DiverIndex: Integer;
  ADiversion: TSDiversionItem;
  TimeIndex: Integer;
  Sfr6Item: TSfrMf6Item;
  Values: TCustomMF_BoundColl;
  DownstreamSegments: TIntegerCollection;
  Diversions: TDiversionCollection;
  FirstBoundary: TSfrMf6Boundary;
begin
  Changing := True;
  tabDownstreamSegments.TabVisible := True;
  tabDiversions.TabVisible := True;
  tabRates.TabVisible := True;
  rdgModflowBoundary.LeftCol := 0;
  DownstreamSegments := nil;
  Diversions := nil;

  FValuesCleared := False;
  try
    pgcSfr6.ActivePageIndex := 0;
    InitializeGrids;

    Assert(ScreenObjectList.Count >= 1);
    Values := nil;
    FoundFirst := False;
    rdeSegmentNumber.Enabled := True;

    rdgFormulas.BeginUpdate;
    frmgrdDownstreamSegments.Grid.BeginUpdate;
    rdgModflowBoundary.BeginUpdate;
    try
//    VerticalState := cbGrayed;
      FirstBoundary := nil;
      for Index := 0 to ScreenObjectList.Count - 1 do
      begin
        Item := ScreenObjectList[Index];
        AScreenObject := Item.ScreenObject;
        Sf6Boundary := AScreenObject.ModflowSfr6Boundary;
        if (Sf6Boundary <> nil) and Sf6Boundary.Used then
        begin
          if not FoundFirst then
          begin
            FoundFirst := True;
            rdeSegmentNumber.IntegerValue := Sf6Boundary.SegmentNumber;
            rdgFormulas.Cells[1, Ord(s6brReachLength)] := Sf6Boundary.ReachLength;
            rdgFormulas.Cells[1, Ord(s6brReachWidth)] := Sf6Boundary.ReachWidth;
            rdgFormulas.Cells[1, Ord(s6brGradient)] := Sf6Boundary.Gradient;
            rdgFormulas.Cells[1, Ord(s6brStreambedTop)] := Sf6Boundary.StreambedTop;
            rdgFormulas.Cells[1, Ord(s6brStreambedThickness)] := Sf6Boundary.StreambedThickness;
            rdgFormulas.Cells[1, Ord(s6brHydraulicConductivity)] := Sf6Boundary.HydraulicConductivity;

            DownstreamSegments := Sf6Boundary.DownstreamSegments;
            frmgrdDownstreamSegments.seNumber.AsInteger :=
              DownstreamSegments.Count;
            frmgrdDownstreamSegments.seNumber.OnChange(nil);
            for DSIndex := 0 to DownstreamSegments.Count - 1 do
            begin
              frmgrdDownstreamSegments.Grid.IntegerValue[0,DSIndex+1]
                := DownstreamSegments[DSIndex].Value;
            end;

            Diversions := Sf6Boundary.Diversions;
            frmgrdDiversions.seNumber.AsInteger := Sf6Boundary.Diversions.Count;
            frmgrdDiversions.seNumber.OnChange(nil);
            for DiverIndex := 0 to Diversions.Count - 1 do
            begin
              ADiversion := Diversions[DiverIndex];
              frmgrdDiversions.Grid.IntegerValue[Ord(s6dcSegment), DiverIndex+1]
                := ADiversion.DownstreamSegment;
              frmgrdDiversions.Grid.ItemIndex[Ord(d6dcPriority), DiverIndex+1]
                := Ord(ADiversion.Priority);
            end;

            {$IFDEF PEST}
            PestModifier[rdgModflowBoundary, Ord(s6cStage)] := Sf6Boundary.PestStageFormula;
            PestMethod[rdgModflowBoundary, Ord(s6cStage)] := Sf6Boundary.PestStageMethod;
            PestModifier[rdgModflowBoundary, Ord(s6cInflow)] := Sf6Boundary.PestInflowFormula;
            PestMethod[rdgModflowBoundary, Ord(s6cInflow)] := Sf6Boundary.PestInflowMethod;
            PestModifier[rdgModflowBoundary, Ord(s6cRainfall)] := Sf6Boundary.PestRainfallFormula;
            PestMethod[rdgModflowBoundary, Ord(s6cRainfall)] := Sf6Boundary.PestRainfallMethod;
            PestModifier[rdgModflowBoundary, Ord(s6cEvaporation)] := Sf6Boundary.PestEvaporationFormula;
            PestMethod[rdgModflowBoundary, Ord(s6cEvaporation)] := Sf6Boundary.PestEvaporationMethod;
            PestModifier[rdgModflowBoundary, Ord(s6cRunoff)] := Sf6Boundary.PestRunoffFormula;
            PestMethod[rdgModflowBoundary, Ord(s6cRunoff)] := Sf6Boundary.PestRunoffMethod;
            PestModifier[rdgModflowBoundary, Ord(s6cRoughness)] := Sf6Boundary.PestRoughnessFormula;
            PestMethod[rdgModflowBoundary, Ord(s6cRoughness)] := Sf6Boundary.PestRoughnessMethod;
            PestModifier[rdgModflowBoundary, Ord(s6cUpstreamFraction)] := Sf6Boundary.PestUpstreamFractionFormula;
            PestMethod[rdgModflowBoundary, Ord(s6cUpstreamFraction)] := Sf6Boundary.PestUpstreamFractionMethod;
            {$ENDIF}

            FirstBoundary := Sf6Boundary;

            seNumberOfTimes.AsInteger := Sf6Boundary.Values.Count;
            seNumberOfTimes.OnChange(nil);
            Values := Sf6Boundary.Values;

            for TimeIndex := 0 to Sf6Boundary.Values.Count - 1 do
            begin
              Sfr6Item := Sf6Boundary.Values[TimeIndex] as TSfrMf6Item;
              rdgModflowBoundary.RealValue[Ord(s6cStartTime), TimeIndex+1+PestRowOffset]
                := Sfr6Item.StartTime;
              rdgModflowBoundary.RealValue[Ord(s6cEndtime), TimeIndex+1+PestRowOffset]
                := Sfr6Item.EndTime;
              rdgModflowBoundary.ItemIndex[Ord(s6cStatus), TimeIndex+1+PestRowOffset]
                := Ord(Sfr6Item.StreamStatus);
              rdgModflowBoundary.Cells[Ord(s6cStage), TimeIndex+1+PestRowOffset]
                := Sfr6Item.Stage;
              rdgModflowBoundary.Cells[Ord(s6cInflow), TimeIndex+1+PestRowOffset]
                := Sfr6Item.Inflow;
              rdgModflowBoundary.Cells[Ord(s6cRainfall), TimeIndex+1+PestRowOffset]
                := Sfr6Item.Rainfall;
              rdgModflowBoundary.Cells[Ord(s6cEvaporation), TimeIndex+1+PestRowOffset]
                := Sfr6Item.Evaporation;
              rdgModflowBoundary.Cells[Ord(s6cRunoff), TimeIndex+1+PestRowOffset]
                := Sfr6Item.Runoff;
              rdgModflowBoundary.Cells[Ord(s6cUpstreamFraction), TimeIndex+1+PestRowOffset]
                := Sfr6Item.UpstreamFraction;
              rdgModflowBoundary.Cells[Ord(s6cRoughness), TimeIndex+1+PestRowOffset]
                := Sfr6Item.Roughness;

              for DiverIndex := 0 to Sfr6Item.Diversions.Count - 1 do
              begin
                rdgModflowBoundary.Cells[Ord(s6cDiversionStart) + DiverIndex, TimeIndex+1+PestRowOffset]
                  := Sfr6Item.Diversions[DiverIndex];
              end;
            end;
          end
          else
          begin
            rdeSegmentNumber.Enabled := False;

            {$IFDEF PEST}

            if Sf6Boundary.PestStageFormula <> FirstBoundary.PestStageFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(s6cStage)] := False
            end;
            if Sf6Boundary.PestStageMethod <> FirstBoundary.PestStageMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(s6cStage)] := False;
            end;
            if Sf6Boundary.PestInflowFormula <> FirstBoundary.PestInflowFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(s6cInflow)] := False;
            end;
            if Sf6Boundary.PestInflowMethod <> FirstBoundary.PestInflowMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(s6cInflow)] := False;
            end;
            if Sf6Boundary.PestRainfallFormula <> FirstBoundary.PestRainfallFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(s6cRainfall)] := False;
            end;
            if Sf6Boundary.PestRainfallMethod <> FirstBoundary.PestRainfallMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(s6cRainfall)] := False;
            end;
            if Sf6Boundary.PestEvaporationFormula <> FirstBoundary.PestEvaporationFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(s6cEvaporation)] := False;
            end;
            if Sf6Boundary.PestEvaporationMethod <> FirstBoundary.PestEvaporationMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(s6cEvaporation)] := False;
            end;
            if Sf6Boundary.PestRunoffFormula <> FirstBoundary.PestRunoffFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(s6cRunoff)] := False;
            end;
            if Sf6Boundary.PestRunoffMethod <> FirstBoundary.PestRunoffMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(s6cRunoff)] := False;
            end;
            if Sf6Boundary.PestRoughnessFormula <> FirstBoundary.PestRoughnessFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(s6cRoughness)] := False;
            end;
            if Sf6Boundary.PestRoughnessMethod <> FirstBoundary.PestRoughnessMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(s6cRoughness)] := False;
            end;
            if Sf6Boundary.PestUpstreamFractionFormula <> FirstBoundary.PestUpstreamFractionFormula then
            begin
              PestModifierAssigned[rdgModflowBoundary, Ord(s6cUpstreamFraction)] := False;
            end;
            if Sf6Boundary.PestUpstreamFractionMethod <> FirstBoundary.PestUpstreamFractionMethod then
            begin
              PestMethodAssigned[rdgModflowBoundary, Ord(s6cUpstreamFraction)] := False;
            end;
            {$ENDIF}

            if not Values.isSame(Sf6Boundary.Values) then
            begin
              ClearGrid(rdgModflowBoundary);
              FValuesCleared := True;
            end;

            if not DownstreamSegments.isSame(Sf6Boundary.DownstreamSegments) then
            begin
              tabDownstreamSegments.TabVisible := False;
            end;

            if not Diversions.isSame(Sf6Boundary.Diversions) then
            begin
              tabDiversions.TabVisible := False;
            end;

            if tabRates.TabVisible and not Values.IsSame(Sf6Boundary.Values) then
            begin
              tabRates.TabVisible := False;
            end;

            if rdgFormulas.Cells[1, Ord(s6brReachLength)] <> Sf6Boundary.ReachLength then
            begin
              rdgFormulas.Cells[1, Ord(s6brReachLength)] := '';
            end;
            if rdgFormulas.Cells[1, Ord(s6brReachWidth)] <> Sf6Boundary.ReachLength then
            begin
              rdgFormulas.Cells[1, Ord(s6brReachWidth)] := '';
            end;
            if rdgFormulas.Cells[1, Ord(s6brGradient)] <> Sf6Boundary.Gradient then
            begin
              rdgFormulas.Cells[1, Ord(s6brGradient)] := '';
            end;
            if rdgFormulas.Cells[1, Ord(s6brStreambedTop)] <> Sf6Boundary.StreambedTop then
            begin
              rdgFormulas.Cells[1, Ord(s6brStreambedTop)] := '';
            end;
            if rdgFormulas.Cells[1, Ord(s6brStreambedThickness)] <> Sf6Boundary.StreambedThickness then
            begin
              rdgFormulas.Cells[1, Ord(s6brStreambedThickness)] := '';
            end;
            if rdgFormulas.Cells[1, Ord(s6brHydraulicConductivity)] <> Sf6Boundary.HydraulicConductivity then
            begin
              rdgFormulas.Cells[1, Ord(s6brHydraulicConductivity)] := '';
            end;
          end;
        end;
      end;
    finally
      rdgModflowBoundary.EndUpdate;
      frmgrdDownstreamSegments.Grid.EndUpdate;
      rdgFormulas.EndUpdate;
    end;
  finally
    Changing := False;
  end;
end;

function TframeScreenObjectSfr6.GetDeletedCells(ACol, ARow: integer): boolean;
begin
  if (ACol < 0) or (ARow < 0) then
  begin
    result := False;
    Exit;
  end;
  if (Length(FDeletedCells) = 0) or (Length(FDeletedCells[0]) = 0) then
  begin
    result := False;
    Exit;
  end;
  if (ACol < Length(FDeletedCells))
    and (ARow < Length(FDeletedCells[0])) then
  begin
    result := FDeletedCells[ACol,ARow];
  end
  else
  begin
    result := False;
  end;
end;

procedure TframeScreenObjectSfr6.GetEndTimes(Col: Integer);
begin
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes(rdgModflowBoundary, Col);
end;

procedure TframeScreenObjectSfr6.GetStartTimes(Col: integer);
begin
  frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes(rdgModflowBoundary, Col);
end;

procedure TframeScreenObjectSfr6.InitializeGrids;
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
  PickList: TStrings;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    rdgModflowBoundary.ColCount := Ord(s6cDiversionStart);

    ClearGrid(rdgModflowBoundary);
    seNumberOfTimes.AsInteger := 0;
    seNumberOfTimesChange(seNumberOfTimes);

    rdgModflowBoundary.Cells[Ord(s6cStartTime),0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(s6cEndtime),0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(s6cStatus),0] := StrStatusMf6;

    rdgModflowBoundary.Cells[Ord(s6cStage),0] := StrStageMf6;
    rdgModflowBoundary.Cells[Ord(s6cInflow),0] := StrInflowMf6L3;
    rdgModflowBoundary.Cells[Ord(s6cRainfall),0] := StrRainfallMf6L;
    rdgModflowBoundary.Cells[Ord(s6cEvaporation),0] := StrEvaporationMf6L;
    rdgModflowBoundary.Cells[Ord(s6cRunoff),0] := StrRunoffMf6L3;
    rdgModflowBoundary.Cells[Ord(s6cUpstreamFraction),0] := StrUpstreamFractionMf6;
    rdgModflowBoundary.Cells[Ord(s6cRoughness),0] := StrRoughnessMf6;

    {$IFDEF PEST}
    rdgModflowBoundary.UseSpecialFormat[0, PestModifierRow] := True;
    rdgModflowBoundary.UseSpecialFormat[0, PestMethodRow] := True;
    rdgModflowBoundary.SpecialFormat[0, PestModifierRow] := rcf4String;
    rdgModflowBoundary.SpecialFormat[0, PestMethodRow] := rcf4String;
    rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
    rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;

    PestMethod[rdgModflowBoundary, Ord(s6cStage)] :=
      TSfrMf6Boundary.DefaultBoundaryMethod(SfrMf6StagePosition);
    PestMethod[rdgModflowBoundary, Ord(s6cInflow)] :=
      TSfrMf6Boundary.DefaultBoundaryMethod(SfrMf6InflowPosition);
    PestMethod[rdgModflowBoundary, Ord(s6cRainfall)] :=
      TSfrMf6Boundary.DefaultBoundaryMethod(SfrMf6RainfallPosition);
    PestMethod[rdgModflowBoundary, Ord(s6cEvaporation)] :=
      TSfrMf6Boundary.DefaultBoundaryMethod(SfrMf6EvaporationPosition);
    PestMethod[rdgModflowBoundary, Ord(s6cRunoff)] :=
      TSfrMf6Boundary.DefaultBoundaryMethod(SfrMf6RunoffPosition);
    PestMethod[rdgModflowBoundary, Ord(s6cRoughness)] :=
      TSfrMf6Boundary.DefaultBoundaryMethod(SfrMf6RoughnessPosition);
    PestMethod[rdgModflowBoundary, Ord(s6cUpstreamFraction)] :=
      TSfrMf6Boundary.DefaultBoundaryMethod(SfrMf6UpstreamFractionPosition);
    {$ENDIF}

//    rdgModflowBoundary.ItemIndex[Ord(s6cStatus),0] := 0;
    rdgModflowBoundary.Cells[Ord(s6cStage),1+PestRowOffset] := '0';
    rdgModflowBoundary.Cells[Ord(s6cInflow),1+PestRowOffset] := '0';
    rdgModflowBoundary.Cells[Ord(s6cRainfall),1+PestRowOffset] := '0';
    rdgModflowBoundary.Cells[Ord(s6cEvaporation),1+PestRowOffset] := '0';
    rdgModflowBoundary.Cells[Ord(s6cRunoff),1+PestRowOffset] := '0';
    rdgModflowBoundary.Cells[Ord(s6cUpstreamFraction),1+PestRowOffset] := '1';
    rdgModflowBoundary.Cells[Ord(s6cRoughness),1+PestRowOffset] := '0.03';

    rdgModflowBoundary.Columns[Ord(s6cStatus)].Format := rcf4String;
    PickList := rdgModflowBoundary.Columns[Ord(s6cStatus)].PickList;
    PickList.Clear;
    PickList.Add(StrInactive);
    PickList.Add(StrActive);
    PickList.Add(StrSpecifiedHead);
    rdgModflowBoundary.Columns[Ord(s6cStatus)].ComboUsed := True;
    rdgModflowBoundary.Columns[Ord(s6cStatus)].LimitToList := True;

    for ColIndex := Ord(s6cStage) to rdgModflowBoundary.ColCount - 1 do
    begin
      AColumn := rdgModflowBoundary.Columns[ColIndex];
      AColumn.WordWrapCaptions := True;
      AColumn.UseButton := True;
      AColumn.ButtonCaption := 'F()';
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustRowHeights := True;
      AColumn.ButtonWidth := 50;
      AColumn.WordWrapCells := False;
    end;

    GetStartTimes(Ord(s6cStartTime));
    GetEndTimes(Ord(s6cEndtime));
  finally
    rdgModflowBoundary.EndUpdate;
  end;

  rdgModflowBoundary.BeginUpdate;
  try
    for ColIndex := Ord(s6cStage) to rdgModflowBoundary.ColCount - 1 do
    begin
      AColumn := rdgModflowBoundary.Columns[ColIndex];
      AColumn.AutoAdjustColWidths := False;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;

  frmgrdDownstreamSegments.Grid.Cells[0,0] := StrDownstreamSegmentsMf6;
  ClearGrid(frmgrdDownstreamSegments.Grid);
  frmgrdDownstreamSegments.seNumber.AsInteger := 0;

  frmgrdDiversions.Grid.BeginUpdate;
  try
    ClearGrid(frmgrdDiversions.Grid);
    frmgrdDiversions.seNumber.AsInteger := 0;
    frmgrdDiversions.Grid.Cells[Ord(s6dcSegment), 0] := StrDiversionSegmentI;
    frmgrdDiversions.Grid.Cells[Ord(d6dcPriority), 0] := StrPriorityCprior;
  finally
    frmgrdDiversions.Grid.EndUpdate;
  end;
  frmgrdDiversions.seNumber.AsInteger := 0;

  rdgFormulas.BeginUpdate;
  try
    rdgFormulas.FixedCols := 1;
    ClearGrid(rdgFormulas);

    rdgFormulas.Cells[1,Ord(s6brNone)] := StrFormulas;
    rdgFormulas.Cells[0,Ord(s6brReachLength)] := StrReachLengthRlen;
    rdgFormulas.Cells[0,Ord(s6brReachWidth)] := StrReachWidthRwid;
    rdgFormulas.Cells[0,Ord(s6brGradient)] := StrGradientRgrd;
    rdgFormulas.Cells[0,Ord(s6brStreambedTop)] := StrStreambedTopRtp;
    rdgFormulas.Cells[0,Ord(s6brStreambedThickness)] := StrStreambedThickness;
    rdgFormulas.Cells[0,Ord(s6brHydraulicConductivity)] := StrHydraulicConductivi;
//    rdgFormulas.Cells[0,Ord(s6brRoughness)] := 'Roughness (man)';
//    rdgFormulas.Cells[0,Ord(s6brUpstreamFraction)] := 'Upstream fraction (ustrf)';

    rdgFormulas.Cells[1, Ord(s6brReachLength)] := StrObjectIntersectLength;
    rdgFormulas.Cells[1, Ord(s6brReachWidth)] := '1';
    rdgFormulas.Cells[1, Ord(s6brGradient)] := '0.001';
    rdgFormulas.Cells[1, Ord(s6brStreambedTop)] := '0';
    rdgFormulas.Cells[1, Ord(s6brStreambedThickness)] := '1';
    rdgFormulas.Cells[1, Ord(s6brHydraulicConductivity)] := rsKx;
//    rdgFormulas.Cells[1, Ord(s6brRoughness)] := '0';
//    rdgFormulas.Cells[1, Ord(s6brUpstreamFraction)] := '1';
  finally
    rdgFormulas.EndUpdate;
  end;
  rdgFormulas.ColWidths[1] := rdgFormulas.Width - rdgFormulas.ColWidths[0] - 16;


end;

procedure TframeScreenObjectSfr6.InitializeNoParamFrame(
  Boundary: TModflowBoundary);
var
  Index: integer;
  TimeList: TModflowTimeList;
  GridRect: TGridRect;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  seNumberOfTimes.AsInteger := 0;
  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Objects[ColIndex,0] := nil;
    rdgModflowBoundary.Columns[ColIndex].WordWrapCaptions := True;
    for RowIndex := 1 to rdgModflowBoundary.RowCount - 1 do
    begin
      rdgModflowBoundary.Cells[ColIndex,RowIndex] := '';
    end;
  end;
  rdgModflowBoundary.Columns[0].Format := rcf4Real;
  rdgModflowBoundary.Columns[1].Format := rcf4Real;
  rdgModflowBoundary.Columns[0].ComboUsed := true;
  rdgModflowBoundary.Columns[1].ComboUsed := true;
  for Index := FLastTimeColumn+2 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Columns[Index].ButtonUsed := true;
  end;
  rdgModflowBoundary.Cells[0, 0] := StrStartingTime;
  rdgModflowBoundary.Cells[1, 0] := StrEndingTime;
  if Boundary <> nil then
  begin
    for Index := 0 to Boundary.Values.TimeListCount(frmGoPhast.PhastModel) - 1 do
    begin
      ColIndex := FLastTimeColumn+2+Index;
//      if ColIndex >= rdgModflowBoundary.ColCount then
//      begin
//        Continue;
//      end;
//      rdgModflowBoundary.Columns[2+Index].AutoAdjustColWidths := True;
      TimeList := Boundary.Values.TimeLists[Index, frmGoPhast.PhastModel];
      if Index = ConductanceColumn then
      begin
        rdgModflowBoundary.Cells[ColIndex, 0] :=
          ConductanceCaption(TimeList.NonParamDescription);
      end
      else
      begin
        rdgModflowBoundary.Cells[ColIndex, 0] := TimeList.NonParamDescription;
      end;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := False;
      rdgModflowBoundary.ColWidths[ColIndex] :=
        rdgModflowBoundary.WidthNeededToFitText(ColIndex,0);
    end;
  end;
  GridRect.Left := 2;
  GridRect.Right := 2;
  GridRect.Top := 1;
  GridRect.Bottom := 1;
  rdgModflowBoundary.Selection := GridRect;
  SetButtonCaptions;
end;

procedure TframeScreenObjectSfr6.LayoutMultiRowEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  LayoutControls(rdgModflowBoundary, rdeFormula, lblFormula,
    Max(FLastTimeColumn+2,rdgModflowBoundary.LeftCol));
end;

procedure TframeScreenObjectSfr6.rdeFormulaChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: Vcl.Grids.TGridOptions;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows + PestRowOffset to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex := FLastTimeColumn+2 to rdgModflowBoundary.ColCount - 1 do
      begin
        if rdgModflowBoundary.IsSelectedCell(ColIndex, RowIndex) then
        begin
          rdgModflowBoundary.Cells[ColIndex, RowIndex] := rdeFormula.Text;
          if Assigned(rdgModflowBoundary.OnSetEditText) then
          begin
            rdgModflowBoundary.OnSetEditText(
              rdgModflowBoundary,ColIndex,RowIndex, rdeFormula.Text);
          end;
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
  TempOptions := rdgModflowBoundary.Options;
  try
    rdgModflowBoundary.Options := [goEditing, goAlwaysShowEditor];
    rdgModflowBoundary.UpdateEditor;
  finally
    rdgModflowBoundary.Options := TempOptions;
  end;
end;

procedure TframeScreenObjectSfr6.rdeSegmentNumberChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectSfr6.rdgFormulasSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundaryBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  EndTime: double;
  NextStartTime: double;
begin
  if (ACol = 1) and (ARow >= rdgModflowBoundary.FixedRows)
    and (ARow < rdgModflowBoundary.RowCount -1) then
  begin
    if TryStrToFloat(rdgModflowBoundary.Cells[ACol, ARow], EndTime)
      and TryStrToFloat(rdgModflowBoundary.Cells[0, ARow+1], NextStartTime) then
    begin
      if NextStartTime < EndTime then
      begin
        rdgModflowBoundary.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundaryColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundaryEnter(Sender: TObject);
begin
  inherited;
  FPriorRowCount := seNumberOfTimes.AsInteger + 1+PestRowOffset;
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundaryHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundaryMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: boolean;
  ColIndex, RowIndex: Integer;
begin
  ShouldEnable := False;
  for RowIndex := rdgModflowBoundary.FixedRows to rdgModflowBoundary.RowCount -1 do
  begin
    for ColIndex := FLastTimeColumn+2 to rdgModflowBoundary.ColCount - 1 do
    begin
      ShouldEnable := rdgModflowBoundary.IsSelectedCell(ColIndex,RowIndex);
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
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow = rdgModflowBoundary.FixedRows)
    and (seNumberOfTimes.AsInteger = 0) then
  begin
    FSelectedText := rdgModflowBoundary.Cells[ACol, ARow];
    CanSelect := False;
    Exit;
  end;
  if Assigned(OnCheckPestCell)
    and not (csCustomPaint in rdgModflowBoundary.ControlState) then
  begin
    OnCheckPestCell(Sender, ACol, ARow, CanSelect);
  end;
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundarySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if FDeleting  then
  begin
    Exit;
  end;
  if seNumberOfTimes.AsInteger < rdgModflowBoundary.RowCount -1-PestRowOffset then
  begin
    seNumberOfTimes.AsInteger := rdgModflowBoundary.RowCount -1-PestRowOffset;
    seNumberOfTimes.OnChange(seNumberOfTimes);
  end;
  if FSelectedText <> Value then
  begin
    DeletedCells[ACol, ARow] := Value = '';
  end;

  if ARow >= rdgModflowBoundary.FixedRows + PestRowOffset then
  begin
    FValuesCleared := False;
  end;

  UpdateNextTimeCell(rdgModflowBoundary, ACol, ARow);
  DoChange;
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundaryStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectSfr6.seNumberOfTimesChange(Sender: TObject);
var
  RowIndex: Integer;
begin
  FDeleting := True;
  try
    if seNumberOfTimes.AsInteger = 0 then
    begin
      rdgModflowBoundary.RowCount := 2+PestRowOffset;
      ClearGrid(rdgModflowBoundary);
    end
    else
    begin
      rdgModflowBoundary.RowCount := seNumberOfTimes.AsInteger + 1+PestRowOffset;
      if not Changing then
      begin
        for RowIndex := FPriorRowCount to rdgModflowBoundary.RowCount - 1 do
        begin
          rdgModflowBoundary.ItemIndex[Ord(s6cStatus), RowIndex] := 1;
        end;
      end;
    end;
    btnDelete.Enabled := seNumberOfTimes.AsInteger >= 1;
    rdgModflowBoundary.Invalidate;
  finally
    FDeleting := False;
    DoChange;
  end;
end;

procedure TframeScreenObjectSfr6.SetButtonCaptions;
var
  Index: Integer;
begin
  for Index := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    if rdgModflowBoundary.Columns[Index].ButtonCaption = '...' then
    begin
      rdgModflowBoundary.Columns[Index].ButtonCaption := StrF;
      rdgModflowBoundary.Columns[Index].ButtonWidth := 35;
    end;
  end;
end;

procedure TframeScreenObjectSfr6.SetChanging(const Value: Boolean);
begin
  FChanging := Value;
end;

procedure TframeScreenObjectSfr6.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TSfrMf6Boundary;
  BoundaryUsed: Boolean;
  DSIndex: Integer;
  DiverIndex: Integer;
  ADiversion: TSDiversionItem;
  RateFormula: string;
  TimeIndex: Integer;
  Sfr6Item: TSfrMf6Item;
  NewCount: Integer;
  SegmentIndex: Integer;
  ItemIndex: Integer;
//  ReachLengthDataArray: TDataArray;
//  ItemPostion: Integer;
begin
//  ReachLengthDataArray := frmGophast.PhastModel.DataArrayManager.GetDataSetByName(KReachLengthSFR);
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Boundary := Item.ScreenObject.ModflowSfr6Boundary;
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
        Item.ScreenObject.CreateSfr6Boundary;
        Boundary := Item.ScreenObject.ModflowSfr6Boundary;
      end;
      if rdeSegmentNumber.Enabled then
      begin
        Boundary.SegmentNumber := rdeSegmentNumber.IntegerValue;
      end;

      if Trim(rdgFormulas.Cells[1, Ord(s6brReachLength)]) <> '' then
      begin
        Boundary.ReachLength := rdgFormulas.Cells[1, Ord(s6brReachLength)];
      end;
      if Trim(rdgFormulas.Cells[1, Ord(s6brReachWidth)]) <> '' then
      begin
        Boundary.ReachWidth := rdgFormulas.Cells[1, Ord(s6brReachWidth)];
      end;
      if Trim(rdgFormulas.Cells[1, Ord(s6brGradient)]) <> '' then
      begin
        Boundary.Gradient := rdgFormulas.Cells[1, Ord(s6brGradient)];
      end;
      if Trim(rdgFormulas.Cells[1, Ord(s6brStreambedTop)]) <> '' then
      begin
        Boundary.StreambedTop := rdgFormulas.Cells[1, Ord(s6brStreambedTop)];
      end;
      if Trim(rdgFormulas.Cells[1, Ord(s6brStreambedThickness)]) <> '' then
      begin
        Boundary.StreambedThickness := rdgFormulas.Cells[1, Ord(s6brStreambedThickness)];
      end;
      if Trim(rdgFormulas.Cells[1, Ord(s6brHydraulicConductivity)]) <> '' then
      begin
        Boundary.HydraulicConductivity := rdgFormulas.Cells[1, Ord(s6brHydraulicConductivity)];
      end;

      if tabDownstreamSegments.TabVisible then
      begin
        Boundary.DownstreamSegments.Count := frmgrdDownstreamSegments.seNumber.AsInteger;
        NewCount := 0;
        for DSIndex := 0 to frmgrdDownstreamSegments.seNumber.AsInteger - 1 do
        begin
          if TryStrToInt(frmgrdDownstreamSegments.Grid.Cells[0,DSIndex+1], SegmentIndex) then
          begin
            Boundary.DownstreamSegments[NewCount].Value := SegmentIndex;
            Inc(NewCount);
          end;
        end;
        Boundary.DownstreamSegments.Count := NewCount;
      end;

      if tabDiversions.TabVisible then
      begin
        Boundary.Diversions.Count := frmgrdDiversions.seNumber.AsInteger;
        NewCount := 0;
        for DiverIndex := 0 to frmgrdDiversions.seNumber.AsInteger - 1 do
        begin
          ItemIndex := frmgrdDiversions.Grid.ItemIndex[Ord(d6dcPriority),
            DiverIndex+1];
          if TryStrToInt(frmgrdDiversions.Grid.Cells[0,DiverIndex+1], SegmentIndex)
            and (ItemIndex >= 0) then
          begin
            ADiversion := Boundary.Diversions[NewCount];
            ADiversion.DownstreamSegment := SegmentIndex;
            ADiversion.Priority := TDivisionPriority(ItemIndex);
            Inc(NewCount);
          end;
        end;
        Boundary.Diversions.Count := NewCount;
      end;

      if tabRates.TabVisible then
      begin
        {$IFDEF PEST}
        if PestModifierAssigned[rdgModflowBoundary, Ord(s6cStage)] then
        begin
          Boundary.PestStageFormula := PestModifier[rdgModflowBoundary, Ord(s6cStage)];
        end;
        if PestMethodAssigned[rdgModflowBoundary, Ord(s6cStage)] then
        begin
          Boundary.PestStageMethod := PestMethod[rdgModflowBoundary, Ord(s6cStage)];
        end;

        if PestModifierAssigned[rdgModflowBoundary, Ord(s6cInflow)] then
        begin
          Boundary.PestInflowFormula := PestModifier[rdgModflowBoundary, Ord(s6cInflow)];
        end;
        if PestMethodAssigned[rdgModflowBoundary, Ord(s6cInflow)] then
        begin
          Boundary.PestInflowMethod := PestMethod[rdgModflowBoundary, Ord(s6cInflow)];
        end;

        if PestModifierAssigned[rdgModflowBoundary, Ord(s6cRainfall)] then
        begin
          Boundary.PestRainfallFormula := PestModifier[rdgModflowBoundary, Ord(s6cRainfall)];
        end;
        if PestMethodAssigned[rdgModflowBoundary, Ord(s6cRainfall)] then
        begin
          Boundary.PestRainfallMethod := PestMethod[rdgModflowBoundary, Ord(s6cRainfall)];
        end;

        if PestModifierAssigned[rdgModflowBoundary, Ord(s6cEvaporation)] then
        begin
          Boundary.PestEvaporationFormula := PestModifier[rdgModflowBoundary, Ord(s6cEvaporation)];
        end;
        if PestMethodAssigned[rdgModflowBoundary, Ord(s6cEvaporation)] then
        begin
          Boundary.PestEvaporationMethod := PestMethod[rdgModflowBoundary, Ord(s6cEvaporation)];
        end;

        if PestModifierAssigned[rdgModflowBoundary, Ord(s6cRunoff)] then
        begin
          Boundary.PestRunoffFormula := PestModifier[rdgModflowBoundary, Ord(s6cRunoff)];
        end;
        if PestMethodAssigned[rdgModflowBoundary, Ord(s6cRunoff)] then
        begin
          Boundary.PestRunoffMethod := PestMethod[rdgModflowBoundary, Ord(s6cRunoff)];
        end;

        if PestModifierAssigned[rdgModflowBoundary, Ord(s6cRoughness)] then
        begin
          Boundary.PestRoughnessFormula := PestModifier[rdgModflowBoundary, Ord(s6cRoughness)];
        end;
        if PestMethodAssigned[rdgModflowBoundary, Ord(s6cRoughness)] then
        begin
          Boundary.PestRoughnessMethod := PestMethod[rdgModflowBoundary, Ord(s6cRoughness)];
        end;

        if PestModifierAssigned[rdgModflowBoundary, Ord(s6cUpstreamFraction)] then
        begin
          Boundary.PestUpstreamFractionFormula := PestModifier[rdgModflowBoundary, Ord(s6cUpstreamFraction)];
        end;
        if PestMethodAssigned[rdgModflowBoundary, Ord(s6cUpstreamFraction)] then
        begin
          Boundary.PestUpstreamFractionMethod := PestMethod[rdgModflowBoundary, Ord(s6cUpstreamFraction)];
        end;
        {$ENDIF}

        if not FValuesCleared then
        begin
          Boundary.Values.Count := seNumberOfTimes.AsInteger;
          for TimeIndex := 0 to Boundary.Values.Count - 1 do
          begin
            Sfr6Item := Boundary.Values[TimeIndex] as TSfrMf6Item;
            Sfr6Item.StartTime := rdgModflowBoundary.
              RealValue[Ord(s6cStartTime), TimeIndex+1+PestRowOffset];
            Sfr6Item.EndTime := rdgModflowBoundary.
              RealValue[Ord(s6cEndtime), TimeIndex+1+PestRowOffset];
            if rdgModflowBoundary.ItemIndex[Ord(s6cStatus), TimeIndex+1+PestRowOffset] >= 0 then
            begin
              Sfr6Item.StreamStatus := TStreamStatus(rdgModflowBoundary.
                ItemIndex[Ord(s6cStatus), TimeIndex+1+PestRowOffset]);
            end;
            if rdgModflowBoundary.Cells[Ord(s6cStage), TimeIndex+1+PestRowOffset] <> '' then
            begin
              Sfr6Item.Stage := rdgModflowBoundary.Cells[Ord(s6cStage), TimeIndex+1+PestRowOffset];
            end;
            if rdgModflowBoundary.Cells[Ord(s6cInflow), TimeIndex+1+PestRowOffset] <> '' then
            begin
              Sfr6Item.Inflow := rdgModflowBoundary.Cells[Ord(s6cInflow), TimeIndex+1+PestRowOffset];
            end;
            if rdgModflowBoundary.Cells[Ord(s6cRainfall), TimeIndex+1+PestRowOffset] <> '' then
            begin
              Sfr6Item.Rainfall := rdgModflowBoundary.Cells[Ord(s6cRainfall), TimeIndex+1+PestRowOffset];
            end;
            if rdgModflowBoundary.Cells[Ord(s6cEvaporation), TimeIndex+1+PestRowOffset] <> '' then
            begin
              Sfr6Item.Evaporation := rdgModflowBoundary.Cells[Ord(s6cEvaporation), TimeIndex+1+PestRowOffset];
            end;
            if rdgModflowBoundary.Cells[Ord(s6cRunoff), TimeIndex+1+PestRowOffset] <> '' then
            begin
              Sfr6Item.Runoff := rdgModflowBoundary.Cells[Ord(s6cRunoff), TimeIndex+1+PestRowOffset];
            end;
            if rdgModflowBoundary.Cells[Ord(s6cUpstreamFraction), TimeIndex+1+PestRowOffset] <> '' then
            begin
              Sfr6Item.UpstreamFraction := rdgModflowBoundary.Cells[Ord(s6cUpstreamFraction), TimeIndex+1+PestRowOffset];
            end;
            if rdgModflowBoundary.Cells[Ord(s6cRoughness), TimeIndex+1+PestRowOffset] <> '' then
            begin
              Sfr6Item.Roughness := rdgModflowBoundary.Cells[Ord(s6cRoughness), TimeIndex+1+PestRowOffset];
            end;

            if tabDiversions.TabVisible then
            begin
              Sfr6Item.DiversionCount := frmgrdDiversions.seNumber.AsInteger;
              for DiverIndex := 0 to frmgrdDiversions.seNumber.AsInteger - 1 do
              begin
                RateFormula := rdgModflowBoundary.Cells[Ord(s6cDiversionStart) + DiverIndex, TimeIndex+1];
                if RateFormula <> '' then
                begin
                  Sfr6Item.DiversionFormulas[DiverIndex] := RateFormula;
                end
                else if Sfr6Item.DiversionFormulas[DiverIndex] = '' then
                begin
                  Sfr6Item.DiversionFormulas[DiverIndex] := '0';
                end;
              end;
            end;
          end
        end;
      end;

    end;

  end;

end;

procedure TframeScreenObjectSfr6.SetDeletedCells(ACol, ARow: integer;
  const Value: boolean);
var
  OldColCount: integer;
  OldRowCount: integer;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  if (ACol < 0) or (ARow < 0) or (ACol >= rdgModflowBoundary.ColCount)
    or (ARow >= rdgModflowBoundary.RowCount) then
  begin
    Exit;
  end;
  Assert(ACol >= 0);
  Assert(ARow >= 0);
  Assert(ACol < rdgModflowBoundary.ColCount);
  Assert(ARow < rdgModflowBoundary.RowCount);
  OldColCount := Length(FDeletedCells);
  if OldColCount = 0 then
  begin
    OldRowCount := 0;
  end
  else
  begin
    OldRowCount := Length(FDeletedCells[0])
  end;
  if (ACol >= OldColCount) or (ARow >= OldRowCount) then
  begin
    SetLength(FDeletedCells, rdgModflowBoundary.ColCount,
      rdgModflowBoundary.RowCount);
    for ColIndex := OldColCount to rdgModflowBoundary.ColCount - 1 do
    begin
      for RowIndex := 0 to rdgModflowBoundary.RowCount - 1 do
      begin
        FDeletedCells[ColIndex,RowIndex] := False;
      end;
    end;
    for ColIndex := 0 to OldColCount - 1 do
    begin
      for RowIndex := OldRowCount to rdgModflowBoundary.RowCount - 1 do
      begin
        FDeletedCells[ColIndex,RowIndex] := False;
      end;
    end;
  end;
  FDeletedCells[ACol, ARow] := Value;
end;

end.
