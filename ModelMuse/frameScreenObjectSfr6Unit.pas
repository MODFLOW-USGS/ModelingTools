unit frameScreenObjectSfr6Unit;

interface

uses System.UITypes,
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, ModflowBoundaryUnit, Vcl.ComCtrls, frameGridUnit,
  UndoItemsScreenObjects, GrayTabs, JvExControls, JvPageList, JvExComCtrls,
  JvPageListTreeView, frameSfrGwtConcentrationsUnit, ZoomBox2, ModflowSfr6Unit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TSfr6Columns = (s6cStartTime, s6cEndtime, s6cStatus, s6cStage, s6cInflow, s6cRainfall,
    s6cEvaporation, s6cRunoff, s6cRoughness, s6cUpstreamFraction, s6cDiversionStart);

  Tsfr6DiversionCol = (s6dcSegment, d6dcPriority);

  TSfr6BoundaryRows = (s6brNone, s6brReachLength, s6brReachWidth, s6brGradient,
    s6brStreambedTop, s6brStreambedThickness, s6brHydraulicConductivity);

  TSfr6CrossSectionCol = (scsXFraction, scsHeight, scsManningFraction);

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
    tabGWT: TTabSheet;
    splSplit: TSplitter;
    tvGwt: TJvPageListTreeView;
    jplGwt: TJvPageList;
    tabCrossSection: TTabSheet;
    frameCrossSection: TframeGrid;
    Panel1: TPanel;
    cbSpecifyRoughnessFraction: TCheckBox;
    zbChannel: TQRbwZoomBox2;
    Splitter1: TSplitter;
    frameCrossSectionTime: TframeGrid;
    comboCrossSection: TComboBox;
    lblCrossSection: TLabel;
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure cbSpecifyRoughnessFractionClick(Sender: TObject);
    procedure frameCrossSectionGridSelectCell(Sender: TObject; ACol, ARow: Integer;
        var CanSelect: Boolean);
    procedure frameCrossSectionGridSetEditText(Sender: TObject; ACol, ARow:
        Integer; const Value: string);
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
    procedure comboCrossSectionChange(Sender: TObject);
    procedure frameCrossSectionTimeGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameCrossSectionTimeGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameCrossSectionseNumberChange(Sender: TObject);
    procedure frameCrossSectionsbAddClick(Sender: TObject);
    procedure frameCrossSectionsbInsertClick(Sender: TObject);
    procedure rdgModflowBoundaryExit(Sender: TObject);
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
    FGwtFrameList: TSfrGwtObjectList;
    FBuoyancyOffset: Integer;
    FViscosityOffset: Integer;
    FXSecSpecified: Boolean;
    FCrossSections: TSfr6CrossSections;
    FCrossCurrentItem: TimeVaryingSfr6CrossSectionItem;
    procedure LayoutMultiRowEditControls;
    function GetDeletedCells(ACol, ARow: integer): boolean;
    procedure SetDeletedCells(ACol, ARow: integer; const Value: boolean);
    procedure InitializeControls;
    procedure SetChanging(const Value: Boolean);
    function GetCrossCurrentItem: TimeVaryingSfr6CrossSectionItem;
    procedure SetCrossCurrentItem(const Value: TimeVaryingSfr6CrossSectionItem);
    property Changing: Boolean read FChanging write SetChanging;
    procedure DoChange;
    procedure PaintCrossSection(Sender: TObject; Buffer: TBitmap32);
    procedure DrawCrossSection(ABitMap: TBitmap32);
    property CrossCurrentItem: TimeVaryingSfr6CrossSectionItem
      read GetCrossCurrentItem write SetCrossCurrentItem;
    procedure GetDataForFirstCrossSection(CrossSection: TSfr6CrossSections;
      var FirstCrossSection: TSfr6CrossSections);
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

const
  BuoyancyColumn = s6cDiversionStart;
var
  ViscosityColumn: Integer = Ord(BuoyancyColumn) + 1;

implementation

{$R *.dfm}

uses frmGoPhastUnit, GoPhastTypes, frmCustomGoPhastUnit, System.Math,
  ScreenObjectUnit, PhastModelUnit,
  Mt3dmsChemSpeciesUnit, DataSetNamesUnit, BigCanvasMethods, ColorSchemes;

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
  StrTheMinimumSFRCros = 'The minimum SFR cross section height must be zero.';

procedure TframeScreenObjectSfr6.btnDeleteClick(Sender: TObject);
var
  SpeciesIndex: Integer;
begin
  inherited;
  if (rdgModflowBoundary.RowCount > 2 + PestRowOffset)
    and (rdgModflowBoundary.Row> 0 + PestRowOffset) then
  begin
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.DeleteRow(rdgModflowBoundary.SelectedRow);
    end;
    rdgModflowBoundary.DeleteRow(rdgModflowBoundary.Row);
  end;
  seNumberOfTimes.AsInteger := seNumberOfTimes.AsInteger -1;

end;

procedure TframeScreenObjectSfr6.btnInsertClick(Sender: TObject);
var
  SpeciesIndex: Integer;
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
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.InsertRow(rdgModflowBoundary.SelectedRow);
    end;
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

procedure TframeScreenObjectSfr6.DrawCrossSection(ABitMap: TBitmap32);
var
  XArray: array of double;
  YArray: array of double;
  X, Y: double;
  Index: integer;
  MinX, MaxX, MinY, MaxY: double;
  MagX, MagY: double;
  SectionPoints: TPointArray;
  Grid: TRbwDataGrid4;
  AValue: Double;
  MinValue: Double;
  MaxValue: Double;
  RowIndex: Integer;
  Fraction: Extended;
  ColorAdjustmentFactor: Extended;
  AColor: TColor32;
begin
  Grid := frameCrossSection.Grid;
  MinX := 0;
  MaxX := 0;
  MinY := 0;
  MaxY := 0;
  SetLength(XArray, frameCrossSection.seNumber.AsInteger);
  SetLength(YArray, frameCrossSection.seNumber.AsInteger);
  MinValue := 0;
  MaxValue := 0;
  if cbSpecifyRoughnessFraction.Checked then
  begin
    MinValue := Grid.RealValueDefault[Ord(scsManningFraction), 1, 0];
    MaxValue := MinValue;
    for RowIndex := 2 to frameCrossSection.seNumber.AsInteger - 1 do
    begin
      AValue := Grid.RealValueDefault[Ord(scsManningFraction), RowIndex, 0];
      if MinValue > AValue then
      begin
        MinValue := AValue;
      end;
      if MaxValue < AValue then
      begin
        MaxValue := AValue;
      end;
    end;
  end;
  for Index := 1 to frameCrossSection.seNumber.AsInteger do
  begin
    X := Grid.RealValueDefault[Ord(scsXFraction), Index, 0];
    Y := Grid.RealValueDefault[Ord(scsHeight), Index, 0];
    XArray[Index-1] := X;
    YArray[Index-1] := Y;
    if Index > 1 then
    begin
      if MinX > X then
      begin
        MinX := X;
      end;
      if MaxX < X then
      begin
        MaxX := X;
      end;
      if MinY > Y then
      begin
        MinY := Y;
      end;
      if MaxY < Y then
      begin
        MaxY := Y;
      end;
    end
    else
    begin
      MinX := X;
      MaxX := X;
      MinY := Y;
      MaxY := Y;
    end;
  end;

  if (MinX <> MaxX) and (MinY <> MaxY) then
  begin
    if cbSpecifyRoughnessFraction.Checked and (MinValue <> MaxValue) then
    begin
      SetLength(SectionPoints, 2);

      zbChannel.OriginX := MinX - (MaxX-MinX)/10;
      zbChannel.OriginY := MinY - (MaxY-MinY)/10;
      MagX := zbChannel.Width/((MaxX-MinX)*1.2);
      MagY := zbChannel.Height/((MaxY-MinY)*1.2);
      zbChannel.Magnification := MagX;
      zbChannel.Exaggeration := MagY/MagX;
      for Index := 0 to frameCrossSection.seNumber.AsInteger -2 do
      begin
        SectionPoints[0].X := zbChannel.XCoord(XArray[Index]);
        SectionPoints[0].Y := zbChannel.YCoord(YArray[Index]);
        SectionPoints[1].X := zbChannel.XCoord(XArray[Index+1]);
        SectionPoints[1].Y := zbChannel.YCoord(YArray[Index+1]);
        AValue := Grid.RealValueDefault[Ord(scsManningFraction), Index+1, 0];

        Fraction := 1 - (AValue - MinValue) / (MaxValue - MinValue);
        ColorAdjustmentFactor := 0.6;

        AColor := Color32( FracAndSchemeToColor(0, Fraction,
          ColorAdjustmentFactor, 1));

        DrawBigPolyline32(ABitMap, AColor, 2, SectionPoints, True);
      end;
    end
    else
    begin
      SetLength(SectionPoints, frameCrossSection.seNumber.AsInteger);

      zbChannel.OriginX := MinX - (MaxX-MinX)/10;
      zbChannel.OriginY := MinY - (MaxY-MinY)/10;
      MagX := zbChannel.Width/((MaxX-MinX)*1.2);
      MagY := zbChannel.Height/((MaxY-MinY)*1.2);
      zbChannel.Magnification := MagX;
      zbChannel.Exaggeration := MagY/MagX;
      for Index := 0 to Length(SectionPoints) -1 do
      begin
        SectionPoints[Index].X := zbChannel.XCoord(XArray[Index]);
        SectionPoints[Index].Y := zbChannel.YCoord(YArray[Index]);
      end;
      DrawBigPolyline32(ABitMap, clBlack32, 2, SectionPoints, True);

    end;
  end;
end;

procedure TframeScreenObjectSfr6.ClearDeletedCells;
begin
  SetLength(FDeletedCells, 0, 0);
end;

procedure TframeScreenObjectSfr6.comboCrossSectionChange(Sender: TObject);
begin
  inherited;
  if not Changing then
  begin
    tabCrossSection.TabVisible := comboCrossSection.ItemIndex <> 0;
    frameCrossSectionTime.Visible := (comboCrossSection.ItemIndex <> 1);
    FXSecSpecified := True;
  end;
end;

function TframeScreenObjectSfr6.ConductanceCaption(
  DirectCaption: string): string;
begin
  result := DirectCaption;
end;

constructor TframeScreenObjectSfr6.Create(AOwner: TComponent);
var
  TempLayer: TPositionedLayer;
begin
  inherited;
  ConductanceColumn := -1;
  FGwtFrameList := TSfrGwtObjectList.Create;

  TempLayer := zbChannel.Image32.Layers.Add(TPositionedLayer) as
    TPositionedLayer;
  TempLayer.OnPaint := PaintCrossSection;
  FCrossSections := TSfr6CrossSections.Create(nil);
end;

destructor TframeScreenObjectSfr6.Destroy;
begin
  FCrossSections.Free;
  FGwtFrameList.Free;
  inherited;
end;

procedure TframeScreenObjectSfr6.cbSpecifyRoughnessFractionClick(Sender:
    TObject);
begin
  inherited;
  cbSpecifyRoughnessFraction.AllowGrayed := false;
  if cbSpecifyRoughnessFraction.Checked then
  begin
    frameCrossSection.Grid.BeginUpdate;
    try
      frameCrossSection.Grid.ColCount := 3;
      frameCrossSection.Grid.Columns[2] := frameCrossSection.Grid.Columns[1];
    finally
      frameCrossSection.Grid.EndUpdate;
    end;
  end
  else
  begin
    frameCrossSection.Grid.ColCount := 2;
  end;
  if not Changing then
  begin
    CrossCurrentItem.CrossSection.UseManningFraction :=
      cbSpecifyRoughnessFraction.Checked;
  end;
end;

procedure TframeScreenObjectSfr6.frameCrossSectionGridSelectCell(Sender:
    TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  zbChannel.InvalidateImage32;
  inherited;
end;

procedure TframeScreenObjectSfr6.frameCrossSectionGridSetEditText(Sender:
    TObject; ACol, ARow: Integer; const Value: string);
var
  CurrentCrossSection: TSfr6CrossSection;
  CrossSectRow: TSfr6CrossSectionPoint;
  AValue: Double;
begin
  inherited;
  if not Changing then
  begin
    FXSecSpecified := True;
    zbChannel.InvalidateImage32;

    if (ARow > 0) and TryStrToFloat(Value, AValue) then    
    begin
      CurrentCrossSection  := CrossCurrentItem.CrossSection;
      while CurrentCrossSection.Count < ARow do
      begin
        CurrentCrossSection.Add;
      end;
      CrossSectRow := CurrentCrossSection[ARow-1];
      // TSfr6CrossSectionCol = (scsXFraction, scsHeight, scsManningFraction);
      case TSfr6CrossSectionCol(ACol) of
        scsXFraction:
          begin
            CrossSectRow.XFraction :=  AValue;
          end;
        scsHeight:
          begin
            CrossSectRow.Height :=  AValue;
          end;
        scsManningFraction:
          begin
            CrossSectRow.ManningsFraction :=  AValue;
          end;
      end;
    end;      
  end;
end;

procedure TframeScreenObjectSfr6.frameCrossSectionsbAddClick(Sender: TObject);
begin
  inherited;
  frameCrossSection.sbAddClick(Sender);

end;

procedure TframeScreenObjectSfr6.frameCrossSectionsbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameCrossSection.sbInsertClick(Sender);

end;

procedure TframeScreenObjectSfr6.frameCrossSectionseNumberChange(
  Sender: TObject);
var
  RowIndex: Integer;
  CrossSection: TSfr6CrossSection;
  ARowItem: TSfr6CrossSectionPoint;
  Grid: TRbwDataGrid4;
  AFloat: Double;
begin
  inherited;
  frameCrossSection.seNumberChange(Sender);
  if FCrossCurrentItem <> nil then
  begin
    CrossSection := FCrossCurrentItem.CrossSection;
    CrossSection.Count := frameCrossSection.seNumber.AsInteger;
    if not Changing then
    begin
      Grid := frameCrossSection.Grid;
      for RowIndex := 1 to frameCrossSection.Grid.RowCount - 1 do
      begin
        ARowItem := CrossSection[RowIndex-1];
        if TryStrToFloat(Grid.Cells[Ord(scsXFraction), RowIndex], AFloat) then
        begin
          ARowItem.XFraction := AFloat;
        end;
        if TryStrToFloat(Grid.Cells[Ord(scsHeight), RowIndex], AFloat) then
        begin
          ARowItem.Height := AFloat;
        end;
        if cbSpecifyRoughnessFraction.Checked
          and TryStrToFloat(Grid.Cells[Ord(scsManningFraction), RowIndex], AFloat) then
        begin
          ARowItem.ManningsFraction := AFloat;
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectSfr6.frameCrossSectionTimeGridSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  Item: TimeVaryingSfr6CrossSectionItem;
  RowIndex: Integer;
  PriorItem: TimeVaryingSfr6CrossSectionItem;
begin
  inherited;
  if frameCrossSectionTime.Grid.Drawing then
  begin
    Exit;
  end;
  if ARow > 0 then
  begin
    Item := nil;
    for RowIndex := 1 to ARow do
    begin
      Item := frameCrossSectionTime.Grid.Objects[Ord(s6cStartTime), RowIndex]
        as TimeVaryingSfr6CrossSectionItem;
      if Item = nil then
      begin
        if RowIndex > FCrossSections.Count then
        begin
          Item := FCrossSections.Add as TimeVaryingSfr6CrossSectionItem;
        end
        else
        begin
          Item := FCrossSections.Insert(RowIndex -1) as TimeVaryingSfr6CrossSectionItem;
        end;
        frameCrossSectionTime.Grid.Objects[Ord(s6cStartTime), RowIndex] := Item;
        if RowIndex > 1 then
        begin
          PriorItem := frameCrossSectionTime.Grid.Objects[Ord(s6cStartTime), RowIndex-1]
            as TimeVaryingSfr6CrossSectionItem;
          Item.CrossSection := PriorItem.CrossSection;
        end;
        if (RowIndex > 1) and
          (frameCrossSectionTime.Grid.Cells[Ord(s6cEndTime), RowIndex-1] <> '') then
        begin
          frameCrossSectionTime.Grid.Cells[Ord(s6cStartTime), RowIndex] :=
            frameCrossSectionTime.Grid.Cells[Ord(s6cEndTime), RowIndex-1];
        end
        else
        begin
          frameCrossSectionTime.Grid.Cells[Ord(s6cStartTime), RowIndex] :=
            FortranFloatToStr(frmGoPhast.PhastModel.ModflowStressPeriods.First.StartTime);
        end;
        frameCrossSectionTimeGridSetEditText(frameCrossSectionTime.Grid,
          Ord(s6cStartTime), RowIndex, frameCrossSectionTime.Grid.Cells[Ord(s6cStartTime), RowIndex]);

        if (RowIndex < frameCrossSectionTime.Grid.RowCount -1)  and
          (frameCrossSectionTime.Grid.Cells[Ord(s6cStartTime), RowIndex+1] <> '') then
        begin
          frameCrossSectionTime.Grid.Cells[Ord(s6cEndTime), RowIndex] :=
            frameCrossSectionTime.Grid.Cells[Ord(s6cStartTime), RowIndex+1];
        end
        else
        begin
          frameCrossSectionTime.Grid.Cells[Ord(s6cEndTime), RowIndex] :=
            FortranFloatToStr(frmGoPhast.PhastModel.ModflowStressPeriods.Last.EndTime);
        end;
        frameCrossSectionTimeGridSetEditText(frameCrossSectionTime.Grid,
          Ord(s6cEndTime), RowIndex, frameCrossSectionTime.Grid.Cells[Ord(s6cEndTime), RowIndex]);
      end;
    end;
    CrossCurrentItem := Item;
  end;
end;

procedure TframeScreenObjectSfr6.frameCrossSectionTimeGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  ATime: Double;
begin
  inherited;
  if ARow > 0 then
  begin
    if TryStrToFloat(Value, ATime) then
    begin
      if ACol = Ord(s6cStartTime) then
      begin
        CrossCurrentItem.StartTime := ATime;
      end
      else
      begin
        CrossCurrentItem.EndTime := ATime;
      end;
    end;
  end;
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
    + Ord(s6cDiversionStart)+FBuoyancyOffset+FViscosityOffset;

  for ColIndex := 0 to frmgrdDiversions.seNumber.AsInteger - 1 do
  begin
    rdgModflowBoundary.Cells[Ord(s6cDiversionStart) + ColIndex + FBuoyancyOffset+FViscosityOffset, 0]
      := Format('Diversion rate %d', [ColIndex+1]);
    AColumn := rdgModflowBoundary.Columns[Ord(s6cDiversionStart) + ColIndex + FBuoyancyOffset+FViscosityOffset];
    AColumn.WordWrapCaptions := True;
    AColumn.ButtonUsed := True;
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

function TframeScreenObjectSfr6.GetCrossCurrentItem: TimeVaryingSfr6CrossSectionItem;
begin
  result := FCrossCurrentItem;
  if result = nil then
  begin
    if FCrossSections.Count = 0 then
    begin
      result := FCrossSections.Add as TimeVaryingSfr6CrossSectionItem;
      frameCrossSectionTime.Grid.Objects[Ord(s6cStartTime), 1] := result;
      frameCrossSectionTime.seNumber.AsInteger := 1;
      result.StartTime := frmGoPhast.PhastModel.ModflowStressPeriods.First.StartTime;
      result.EndTime   := frmGoPhast.PhastModel.ModflowStressPeriods.Last.EndTime;
      if frameCrossSectionTime.Grid.Cells[Ord(s6cStartTime), 1] = '' then
      begin
        frameCrossSectionTime.Grid.Cells[Ord(s6cStartTime), 1] := FloatToStr(result.StartTime);
        frameCrossSectionTime.Grid.Cells[Ord(s6cEndtime), 1] := FloatToStr(result.EndTime);
      end;
    end
    else
    begin
      result := FCrossSections.First as TimeVaryingSfr6CrossSectionItem;
    end;
    FCrossCurrentItem := result;
  end;
end;

procedure TframeScreenObjectSfr6.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
const
  DefaultRect: TGridRect = (Left: 0; Top: 1; Right: 1; Bottom: 1);
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
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
  APage: TJvStandardPage;
  AGwtFrame: TframeSfrGwtConcentrations;
  ANode: TJvPageIndexNode;
  DensityUsed: Boolean;
  IgnoredNames: TStringList;
  FrameIndex: Integer;
  ViscosityUsed: Boolean;
  CrossSection: TSfr6CrossSections;
  FirstCrossSection: TSfr6CrossSections;
begin
  FirstCrossSection := nil;
  FCrossCurrentItem := nil;
  FCrossSections.Clear;
  tabCrossSection.TabVisible := False;
  DensityUsed := frmGoPhast.PhastModel.BuoyancyDensityUsed;
  ViscosityUsed := frmGoPhast.PhastModel.ViscosityPkgViscUsed;

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
    InitializeControls;

    Assert(ScreenObjectList.Count >= 1);
    Values := nil;
    FoundFirst := False;
    rdeSegmentNumber.Enabled := True;

    rdgFormulas.BeginUpdate;
    frmgrdDownstreamSegments.Grid.BeginUpdate;
    rdgModflowBoundary.BeginUpdate;
    frameCrossSection.Grid.BeginUpdate;
    try
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
            if DensityUsed then
            begin
              if Sf6Boundary.PestDensity.Count = 0 then
              begin
                Sf6Boundary.PestDensity.Add
              end;
              PestModifier[rdgModflowBoundary, Ord(BuoyancyColumn)] := Sf6Boundary.PestDensity[0].Value;
              if Sf6Boundary.PestDensityMethods.Count = 0 then
              begin
                Sf6Boundary.PestDensityMethods.Add;
              end;
              PestMethod[rdgModflowBoundary, Ord(BuoyancyColumn)] := Sf6Boundary.PestDensityMethods[0].PestParamMethod;
            end;
            if ViscosityUsed then
            begin
              if Sf6Boundary.PestViscosity.Count = 0 then
              begin
                Sf6Boundary.PestViscosity.Add
              end;
              PestModifier[rdgModflowBoundary, Ord(ViscosityColumn)] := Sf6Boundary.PestViscosity[0].Value;
              if Sf6Boundary.PestViscosityMethods.Count = 0 then
              begin
                Sf6Boundary.PestViscosityMethods.Add;
              end;
              PestMethod[rdgModflowBoundary, Ord(ViscosityColumn)] := Sf6Boundary.PestViscosityMethods[0].PestParamMethod;
            end;

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

              if DensityUsed then
              begin
                if Sfr6Item.Density.Count = 0 then
                begin
                  Sfr6Item.Density.Add;
                end;
                rdgModflowBoundary.Cells[Ord(BuoyancyColumn), TimeIndex+1+PestRowOffset]
                  := Sfr6Item.Density[0].Value;
              end;

              if ViscosityUsed then
              begin
                if Sfr6Item.Viscosity.Count = 0 then
                begin
                  Sfr6Item.Viscosity.Add;
                end;
                rdgModflowBoundary.Cells[Ord(ViscosityColumn), TimeIndex+1+PestRowOffset]
                  := Sfr6Item.Viscosity[0].Value;
              end;

              for DiverIndex := 0 to Sfr6Item.Diversions.Count - 1 do
              begin
                rdgModflowBoundary.Cells[Ord(s6cDiversionStart) + FBuoyancyOffset+FViscosityOffset + DiverIndex,
                  TimeIndex+1+PestRowOffset]
                  := Sfr6Item.Diversions[DiverIndex];
              end;
            end;


            comboCrossSection.ItemIndex := Ord(Sf6Boundary.CrossSectionUsage);
            tabCrossSection.TabVisible := comboCrossSection.ItemIndex <> 0;
            if tabCrossSection.TabVisible then
            begin
              CrossSection := Sf6Boundary.CrossSections;
              GetDataForFirstCrossSection(CrossSection, FirstCrossSection);
             end;
            frameCrossSectionTime.Visible := (comboCrossSection.ItemIndex <> 1);
          end
          else
          begin
            rdeSegmentNumber.Enabled := False;

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

            if DensityUsed then
            begin
              if not Sf6Boundary.PestDensity.IsSame(FirstBoundary.PestDensity) then
              begin
                PestModifierAssigned[rdgModflowBoundary, Ord(BuoyancyColumn)] := False;
              end;
              if not Sf6Boundary.PestDensityMethods.IsSame(FirstBoundary.PestDensityMethods) then
              begin
                PestMethodAssigned[rdgModflowBoundary, Ord(BuoyancyColumn)] := False;
              end;
            end;

            if ViscosityUsed then
            begin
              if not Sf6Boundary.PestViscosity.IsSame(FirstBoundary.PestViscosity) then
              begin
                PestModifierAssigned[rdgModflowBoundary, Ord(ViscosityColumn)] := False;
              end;
              if not Sf6Boundary.PestViscosityMethods.IsSame(FirstBoundary.PestViscosityMethods) then
              begin
                PestMethodAssigned[rdgModflowBoundary, Ord(ViscosityColumn)] := False;
              end;
            end;

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

            if (comboCrossSection.ItemIndex <> -1) then
            begin
              if comboCrossSection.ItemIndex <> Ord(Sf6Boundary.CrossSectionUsage) then
              begin
                comboCrossSection.ItemIndex := -1;
                tabCrossSection.TabVisible := True;
                if Sf6Boundary.CrossSectionUsage = csuMultiple then
                begin
                  frameCrossSectionTime.Visible := True;
                end;
              end;
            end;
            if Sf6Boundary.CrossSectionUsage <> csuNotUsed then
            begin
              CrossSection := Sf6Boundary.CrossSections;
              if FirstCrossSection = nil then
              begin
                GetDataForFirstCrossSection(CrossSection, FirstCrossSection);
              end
              else
              begin
                if not CrossSection.IsSame(FirstCrossSection) then
                begin
                  ClearGrid(frameCrossSection.Grid);
                  ClearGrid(frameCrossSectionTime.Grid);
                  FXSecSpecified := False;
                end;
              end;
            end;
          end;

        end;
      end;
    finally
      rdgModflowBoundary.EndUpdate;
      frmgrdDownstreamSegments.Grid.EndUpdate;
      rdgFormulas.EndUpdate;
      frameCrossSection.Grid.EndUpdate;
    end;

    tabGWT.TabVisible := frmGoPhast.PhastModel.GwtUsed;
    if tabGWT.TabVisible then
    begin
      tvGwt.Items.Clear;
      IgnoredNames := TStringList.Create;
      try
        frmGoPhast.PhastModel.GetIgnoredSpeciesNames(IgnoredNames);
        FrameIndex := 0;
        for SpeciesIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
        begin
          ASpecies := frmGoPhast.PhastModel.MobileComponents[SpeciesIndex];
          if IgnoredNames.IndexOf(ASpecies.Name) >= 0 then
          begin
            Continue;
          end;
          if FrameIndex >= jplGwt.PageCount then
          begin
            APage := TJvStandardPage.Create(self);
            APage.PageList := jplGwt;
            AGwtFrame := TframeSfrGwtConcentrations.Create(nil);
            FGwtFrameList.Add(AGwtFrame);
            AGwtFrame.Parent := APage;
            AGwtFrame.Align := alClient;
          end
          else
          begin
            AGwtFrame := FGwtFrameList[FrameIndex];
          end;
          ANode := tvGwt.Items.Add(nil, ASpecies.Name) as TJvPageIndexNode;
          ANode.PageIndex := FrameIndex;
          AGwtFrame.GetData(ScreenObjectList, FrameIndex);
          if FrameIndex = 0 then
          begin
            ANode.Selected := True;
          end;
          Inc(FrameIndex);
        end;
      finally
        IgnoredNames.Free;
      end;
    end;
  finally
    Changing := False;
  end;
  if frameCrossSectionTime.Visible then
  begin
    frameCrossSectionTime.Grid.Selection := DefaultRect;
  end;
  rdgModflowBoundaryExit(nil);
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

procedure TframeScreenObjectSfr6.InitializeControls;
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
  PickList: TStrings;
  DensityUsed: Boolean;
  ViscosityUsed: Boolean;
begin
  ClearGrid(frameCrossSectionTime.Grid);
  ClearGrid(frameCrossSection.Grid);
  DensityUsed := frmGoPhast.PhastModel.BuoyancyDensityUsed;
  ViscosityUsed := frmGoPhast.PhastModel.ViscosityPkgViscUsed;
  rdgModflowBoundary.BeginUpdate;
  try
    if DensityUsed then
    begin
      rdgModflowBoundary.ColCount := Ord(s6cDiversionStart) + 1;
      FBuoyancyOffset := 1;
      rdgModflowBoundary.Cells[Ord(BuoyancyColumn), 0] := StrDensity;
    end
    else
    begin
      rdgModflowBoundary.ColCount := Ord(s6cDiversionStart);
      FBuoyancyOffset := 0;
    end;

    if ViscosityUsed then
    begin
      rdgModflowBoundary.ColCount := rdgModflowBoundary.ColCount + 1;
      FViscosityOffset := 1;
      ViscosityColumn := Ord(BuoyancyColumn) + FBuoyancyOffset;
      rdgModflowBoundary.Cells[ViscosityColumn, 0] := StrViscosity;
    end
    else
    begin
      FViscosityOffset := 0;
    end;

    ClearGrid(rdgModflowBoundary);
    seNumberOfTimes.AsInteger := 0;
    seNumberOfTimesChange(seNumberOfTimes);
    if DensityUsed then
    begin
      PestMethod[rdgModflowBoundary, Ord(BuoyancyColumn)] := ppmMultiply;
    end;
    if ViscosityUsed then
    begin
      PestMethod[rdgModflowBoundary, Ord(ViscosityColumn)] := ppmMultiply;
    end;

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
      AColumn.ButtonUsed := True;
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

  frameCrossSectionTime.Grid.Cells[Ord(s6cStartTime),0] := StrStartingTime;
  frameCrossSectionTime.Grid.Cells[Ord(s6cEndtime),0] := StrEndingTime;

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

  frameCrossSection.Grid.BeginUpdate;
  try
    frameCrossSection.Grid.ColCount := 3;
    frameCrossSection.Grid.Columns[2] := frameCrossSection.Grid.Columns[1];
    ClearGrid(frameCrossSection.Grid);
    frameCrossSection.Grid.Cells[Ord(scsXFraction), 0] := 'X Fraction';
    frameCrossSection.Grid.Cells[Ord(scsHeight), 0] := 'Height';
    frameCrossSection.Grid.Cells[Ord(scsManningFraction), 0] := 'Manning''s Roughness Fraction';
  finally
    frameCrossSection.Grid.EndUpdate;
    frameCrossSection.Grid.ColCount := 2;
  end;
//  cbSpecifyCrossSection.Checked := False;
  comboCrossSection.ItemIndex := 0;
  cbSpecifyRoughnessFraction.Checked := False;
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

procedure TframeScreenObjectSfr6.PaintCrossSection(Sender: TObject;
  Buffer: TBitmap32);
var
  ABitMap: TBitmap32;
begin
  Buffer.BeginUpdate;
  try
    ABitMap := TBitmap32.Create;
    try
      ABitMap.Height := Buffer.Height;
      ABitMap.Width := Buffer.Width;
      ABitMap.DrawMode := dmBlend;
      DrawCrossSection(ABitMap);
      Buffer.Draw(0, 0, ABitMap);
    finally
      ABitMap.Free;
    end;
  finally
    Buffer.EndUpdate;
  end;
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
  CanSelect: Boolean;
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
  if Assigned(rdgModflowBoundary.OnSelectCell) then
  begin
    CanSelect := True;
    rdgModflowBoundary.OnSelectCell(rdgModflowBoundary, ACol, ARow, CanSelect);
    if not CanSelect then
    begin
      rdgModflowBoundary.Canvas.Brush.Color := clBtnFace;
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

procedure TframeScreenObjectSfr6.rdgModflowBoundaryExit(Sender: TObject);
var
  AStringList: TStringList;
  Index: Integer;
begin
  inherited;
  AStringList := TStringList.Create;
  try
    AStringList.Assign(rdgModflowBoundary.Cols[Ord(s6cStartTime)]);
    for Index := 0 to PestRowOffset do
    begin
      AStringList.Delete(0);
    end;
    frameCrossSectionTime.Grid.Columns[Ord(s6cStartTime)].PickList := AStringList;

    AStringList.Assign(rdgModflowBoundary.Cols[Ord(s6cEndtime)]);
    for Index := 0 to PestRowOffset do
    begin
      AStringList.Delete(0);
    end;
    frameCrossSectionTime.Grid.Columns[Ord(s6cEndtime)].PickList := AStringList;
  finally
    AStringList.Free;
  end;
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
  if Assigned(OnCheckPestCell) then
  begin
    OnCheckPestCell(Sender, ACol, ARow, CanSelect);
  end;
end;

procedure TframeScreenObjectSfr6.rdgModflowBoundarySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  SpeciesIndex: Integer;
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

  if (ARow >= rdgModflowBoundary.FixedRows + PestRowOffset)
    and (ACol in [Ord(s6cStartTime), Ord(s6cEndtime)]) then
  begin
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.Cells[ACol, ARow]
        := rdgModflowBoundary.Cells[ACol, ARow];
    end;
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
  SpeciesIndex: Integer;
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
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.RowCount := rdgModflowBoundary.RowCount;
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

procedure TframeScreenObjectSfr6.SetCrossCurrentItem(
  const Value: TimeVaryingSfr6CrossSectionItem);
var
  CrossSection: TSfr6CrossSection;
  ItemIndex: Integer;
  RowIndex: Integer;
  ItemRow: TSfr6CrossSectionPoint;
begin
  if FCrossCurrentItem <> Value then
  begin  
    FCrossCurrentItem := Value;
    if FCrossCurrentItem <> nil then  
    begin   
      CrossSection := FCrossCurrentItem.CrossSection;
      Changing := True;
      try
        cbSpecifyRoughnessFraction.Checked := CrossSection.UseManningFraction;
        frameCrossSection.seNumber.AsInteger := CrossSection.Count;
        for ItemIndex := 0 to CrossSection.Count - 1 do
        begin
          RowIndex := ItemIndex + 1;
          ItemRow := CrossSection[ItemIndex];
          frameCrossSection.Grid.Cells[Ord(scsXFraction), RowIndex]
            := FloatToStr(ItemRow.XFraction);
          frameCrossSection.Grid.Cells[Ord(scsHeight), RowIndex]
            := FloatToStr(ItemRow.Height);
          if CrossSection.UseManningFraction then        
          begin      
            frameCrossSection.Grid.Cells[Ord(scsManningFraction), RowIndex]
              := FloatToStr(ItemRow.ManningsFraction);
          end;          
        end;
      finally        
        Changing := False;
      end;
//      FXSecSpecified := CrossSection.Count > 1;
      zbChannel.InvalidateImage32;
    end;        
  end;    
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
  SpeciesIndex: Integer;
  AGwtFrame: TframeSfrGwtConcentrations;
  DensityUsed: Boolean;
  IgnoredNames: TStringList;
  FrameIndex: Integer;
  SpeciesName: string;
  ViscosityUsed: Boolean;
  XSecIndex: Integer;
  CrossSection: TSfr6CrossSections;
  XSecItem: TSfr6CrossSectionPoint;
  MinHeight: double;
  ACrossSection: TSfr6CrossSection;
  CrossSectionCount: Integer;
  CrossIndex: Integer;
  AUsage: TCrossSectionUsage;
  CrossSectionItems: TList<TimeVaryingSfr6CrossSectionItem>;
begin
  if not (comboCrossSection.ItemIndex in [0,1]) then
  begin
    CrossSectionItems := TList<TimeVaryingSfr6CrossSectionItem>.Create;
    try
      for Index := 0 to FCrossSections.Count - 1 do
      begin
        CrossSectionItems.Add(FCrossSections.Items[Index] as TimeVaryingSfr6CrossSectionItem);
      end;
      CrossSectionItems.Sort(
          TComparer<TimeVaryingSfr6CrossSectionItem>.Construct(
            function(const Left, Right: TimeVaryingSfr6CrossSectionItem): Integer
            begin
              Result := Sign(Left.StartTime - Right.StartTime);
              if Result = 0 then
              begin
                Result := Sign(Left.EndTime - Right.EndTime);
              end;
            end
          ));
      for Index := 0 to CrossSectionItems.Count - 1 do
      begin
        CrossSectionItems[Index].Index := Index;
      end;
    finally
      CrossSectionItems.Free;
    end;
  end;

  DensityUsed := frmGoPhast.PhastModel.BuoyancyDensityUsed;
  ViscosityUsed := frmGoPhast.PhastModel.ViscosityPkgViscUsed;

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

        if DensityUsed then
        begin
          if PestModifierAssigned[rdgModflowBoundary, Ord(BuoyancyColumn)] then
          begin
            if Boundary.PestDensity.Count = 0 then
            begin
              Boundary.PestDensity.Add
            end;
            Boundary.PestDensity[0].Value := PestModifier[rdgModflowBoundary, Ord(BuoyancyColumn)];
          end;
          if PestMethodAssigned[rdgModflowBoundary, Ord(BuoyancyColumn)] then
          begin
            if Boundary.PestDensityMethods.Count = 0 then
            begin
              Boundary.PestDensityMethods.Add;
            end;
            Boundary.PestDensityMethods[0].PestParamMethod := PestMethod[rdgModflowBoundary, Ord(BuoyancyColumn)];
          end;
        end;

        if ViscosityUsed then
        begin
          if PestModifierAssigned[rdgModflowBoundary, Ord(ViscosityColumn)] then
          begin
            if Boundary.PestViscosity.Count = 0 then
            begin
              Boundary.PestViscosity.Add
            end;
            Boundary.PestViscosity[0].Value := PestModifier[rdgModflowBoundary, Ord(ViscosityColumn)];
          end;
          if PestMethodAssigned[rdgModflowBoundary, Ord(ViscosityColumn)] then
          begin
            if Boundary.PestViscosityMethods.Count = 0 then
            begin
              Boundary.PestViscosityMethods.Add;
            end;
            Boundary.PestViscosityMethods[0].PestParamMethod := PestMethod[rdgModflowBoundary, Ord(ViscosityColumn)];
          end;
        end;

        if not FValuesCleared then
        begin
          Boundary.Values.Count := seNumberOfTimes.AsInteger;
          for TimeIndex := 0 to Boundary.Values.Count - 1 do
          begin
            Sfr6Item := Boundary.Values[TimeIndex] as TSfrMf6Item;
            Sfr6Item.StartTime := rdgModflowBoundary.
              RealValueDefault[Ord(s6cStartTime), TimeIndex+1+PestRowOffset, Sfr6Item.StartTime];
            Sfr6Item.EndTime := rdgModflowBoundary.
              RealValueDefault[Ord(s6cEndtime), TimeIndex+1+PestRowOffset, Sfr6Item.EndTime];
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

            if DensityUsed then
            begin
              if Sfr6Item.Density.Count = 0 then
              begin
                Sfr6Item.Density.Add;
              end;
              Sfr6Item.Density[0].Value :=
                rdgModflowBoundary.Cells[Ord(BuoyancyColumn), TimeIndex+1+PestRowOffset];
            end;

            if ViscosityUsed then
            begin
              if Sfr6Item.Viscosity.Count = 0 then
              begin
                Sfr6Item.Viscosity.Add;
              end;
              Sfr6Item.Viscosity[0].Value :=
                rdgModflowBoundary.Cells[Ord(ViscosityColumn), TimeIndex+1+PestRowOffset];
            end;

            if tabDiversions.TabVisible then
            begin
              Sfr6Item.DiversionCount := frmgrdDiversions.seNumber.AsInteger;
              for DiverIndex := 0 to frmgrdDiversions.seNumber.AsInteger - 1 do
              begin
                RateFormula := rdgModflowBoundary.Cells[Ord(s6cDiversionStart)
                  + FBuoyancyOffset+FViscosityOffset + DiverIndex,
                  TimeIndex+1+PestRowOffset];
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

      AUsage := csuNotUsed;
      if comboCrossSection.ItemIndex <> -1 then
      begin
        AUsage := TCrossSectionUsage(comboCrossSection.ItemIndex);
      end;

      if (Boundary.CrossSectionUsage <> csuNotUsed) or (AUsage <> csuNotUsed) then
      begin
        if FXSecSpecified then
        begin
          CrossSection := Boundary.CrossSections;
          CrossSection.Assign(FCrossSections);
          if comboCrossSection.ItemIndex <> -1 then
          begin
            Boundary.CrossSectionUsage := TCrossSectionUsage(comboCrossSection.ItemIndex);
          end;

          if Boundary.CrossSectionUsage = csuMultiple then
          begin
            CrossSectionCount := CrossSection.Count;
          end
          else
          begin
            CrossSectionCount := Min(CrossSection.Count, 1);
          end;
          for CrossIndex := 0 to CrossSectionCount -1 do
          begin
            ACrossSection := (CrossSection.Items[CrossIndex]
              as TimeVaryingSfr6CrossSectionItem).CrossSection;
            MinHeight := 0.0;
            for XSecIndex := 0 to ACrossSection.Count -1 do
            begin
              XSecItem := ACrossSection[XSecIndex];
              if XSecIndex = 0 then
              begin
                MinHeight := XSecItem.Height;
              end
              else
              begin
                if XSecItem.Height < MinHeight then
                begin
                  MinHeight := XSecItem.Height;
                end;
              end;
            end;
            if MinHeight <> 0 then
            begin
              Beep;
              MessageDlg(StrTheMinimumSFRCros, mtError, [mbOK], 0);
            end;
          end;
        end;
      end;
    end;
  end;

  if tabGWT.TabVisible then
  begin
    IgnoredNames := TStringList.Create;
    try
      frmGoPhast.PhastModel.GetIgnoredSpeciesNames(IgnoredNames);
      FrameIndex := 0;
      for SpeciesIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
      begin
        SpeciesName := frmGoPhast.PhastModel.MobileComponents[SpeciesIndex].Name;
        if IgnoredNames.IndexOf(SpeciesName) >= 0 then
        begin
          Continue;
        end;
        AGwtFrame := FGwtFrameList[FrameIndex];
        AGwtFrame.setData(List, SpeciesIndex);
        Inc(FrameIndex);
      end;
    finally
      IgnoredNames.Free;
    end;
  end;
end;

procedure TframeScreenObjectSfr6.GetDataForFirstCrossSection(
  CrossSection: TSfr6CrossSections;
  var FirstCrossSection: TSfr6CrossSections);
var
  CrossSectionItem: TimeVaryingSfr6CrossSectionItem;
  CrossSection1: TSfr6CrossSection;
  XSIndex: Integer;
  AXsecPoint: TSfr6CrossSectionPoint;
  CrossGrid: TRbwDataGrid4;
  CrossIndex: Integer;
  CrossRowIndex: Integer;
begin
  FXSecSpecified := True;
  FirstCrossSection := CrossSection;
  FCrossSections.Assign(FirstCrossSection);
  if CrossSection.Count > 0 then
  begin
    frameCrossSectionTime.seNumber.AsInteger := CrossSection.Count;
    frameCrossSectionTime.seNumberChange(nil);
    CrossSectionItem := CrossSection.First as TimeVaryingSfr6CrossSectionItem;
    CrossSection1 := CrossSectionItem.CrossSection;
    cbSpecifyRoughnessFraction.Checked := CrossSection1.UseManningFraction;
    if CrossSection1.UseManningFraction then
    begin
      frameCrossSection.Grid.ColCount := 3;
      frameCrossSection.Grid.Columns[2] := frameCrossSection.Grid.Columns[1];
    end
    else
    begin
      frameCrossSection.Grid.ColCount := 2;
    end;
    frameCrossSection.seNumber.AsInteger := CrossSection1.Count;
    frameCrossSection.seNumberChange(nil);
    for XSIndex := 0 to CrossSection1.Count - 1 do
    begin
      AXsecPoint := CrossSection1[XSIndex];
      frameCrossSection.Grid.RealValue[Ord(scsXFraction), XSIndex + 1] := AXsecPoint.XFraction;
      frameCrossSection.Grid.RealValue[Ord(scsHeight), XSIndex + 1] := AXsecPoint.Height;
      frameCrossSection.Grid.RealValue[Ord(scsManningFraction), XSIndex + 1] := AXsecPoint.ManningsFraction;
    end;
  end;
  CrossGrid := frameCrossSectionTime.Grid;
  for CrossIndex := 0 to FCrossSections.Count - 1 do
  begin
    CrossSectionItem := FCrossSections.Items[CrossIndex] as TimeVaryingSfr6CrossSectionItem;
    CrossRowIndex := CrossIndex + 1;
    CrossGrid.Cells[Ord(s6cStartTime), CrossRowIndex] := FloatToStr(CrossSectionItem.StartTime);
    CrossGrid.Cells[Ord(s6cEndtime), CrossRowIndex] := FloatToStr(CrossSectionItem.EndTime);
    CrossGrid.Objects[Ord(s6cStartTime), CrossRowIndex] := CrossSectionItem;
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
