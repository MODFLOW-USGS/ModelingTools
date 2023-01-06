unit frameModpathTimeSeriesDisplayUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, RbwDataGrid4, StdCtrls, ExtCtrls, JvSpin, JvExControls,
  JvxSlider, ComCtrls, JvExComCtrls, JvUpDown, Mask, JvExMask, JvToolEdit,
  frameModpathDisplayUnit, UndoItems, PathlineReader, RealListUnit,
  PhastModelUnit, GrayTabs;

type
  TTimeSeriesLimits = (tslNone, tslColors, tslLayer, tslRow, tslColumn,
    tslParticleGroup);

const
  TableCaptions: array[Low(TTimeSeriesLimits)..
    High(TTimeSeriesLimits)] of string =
    ('', Colorlimits, Layer, Row, Column, Group);

type
  TUndoImportTimeSeries = class(TCustomUndo)
  private
    FExistingTimeSeries: TTimeSeriesReader;
    FNewTimeSeries: TTimeSeriesReader;
    FImportedNewFile: Boolean;
    FModel: TCustomModel;
    procedure ForceRedraw;
    procedure EnableMenuItems;
  public
    Constructor Create(Model: TCustomModel; var NewTimeSeries: TTimeSeriesReader;
      ImportedNewFile: boolean);
    Destructor Destroy; override;
    function Description: string; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TframeModpathTimeSeriesDisplay = class(TFrame)
    pcMain: TPageControl;
    tabBasic: TTabSheet;
    lblModpathFile: TLabel;
    lblTimeToPlot: TLabel;
    lblColorScheme: TLabel;
    pbColorScheme: TPaintBox;
    lblColorAdjustment: TLabel;
    lblCycles: TLabel;
    fedModpathFile: TJvFilenameEdit;
    comboTimeToPlot: TComboBox;
    udTimeToPlot: TJvUpDown;
    cbShowPathlines: TCheckBox;
    cbLimitToCurrentIn2D: TCheckBox;
    comboColorScheme: TComboBox;
    jsColorExponent: TJvxSlider;
    seColorExponent: TJvSpinEdit;
    seCycles: TJvSpinEdit;
    tabOptions: TTabSheet;
    rgShow2D: TRadioGroup;
    rgColorBy: TRadioGroup;
    rdgLimits: TRbwDataGrid4;
    comboModelSelection: TComboBox;
    lblModelSelection: TLabel;
    setimeSeriesSize: TJvSpinEdit;
    lbltimeSeriesSize: TLabel;
    btnColorSchemes: TButton;
    procedure rdgLimitsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLimitsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLimitsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rgShow2DClick(Sender: TObject);
    procedure rgColorByClick(Sender: TObject);
    procedure fedModpathFileBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure fedModpathFileChange(Sender: TObject);
    procedure udTimeToPlotChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    procedure pbColorSchemePaint(Sender: TObject);
    procedure comboColorSchemeChange(Sender: TObject);
    procedure seColorExponentChange(Sender: TObject);
    procedure seCyclesChange(Sender: TObject);
    procedure jsColorExponentChanged(Sender: TObject);
    procedure comboTimeToPlotChange(Sender: TObject);
    procedure comboModelSelectionChange(Sender: TObject);
    procedure btnColorSchemesClick(Sender: TObject);
  protected
    procedure Loaded; override;
  private
    FTimeSeriesList: TimeSeriesObjectList;
    procedure AssignTimesToComboBox(Times: TRealList);
    procedure ReadIntLimit(IntLimits: TShowIntegerLimit;
      ALimitRow: TTimeSeriesLimits);
    procedure SetIntLimit(LimitRow: TTimeSeriesLimits; DefaultLimit: integer;
      IntLimit: TShowIntegerLimit);
    { Private declarations }
  public
    procedure GetData;
    procedure SetData;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateColorSchemes;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, ColorSchemes, ModflowGridUnit, ModelMuseUtilities,
  LayerStructureUnit, frmCustomGoPhastUnit, frmColorSchemesUnit;

resourcestring
  StrTheTimeSeriesFile = 'The time series file on disk has a different date ' +
  'than the file that was imported into ModelMuse.  Do you want to import th' +
  'e new file?';
  StrLimitingFactor = 'Limiting factor';
  StrLowerLimit = 'Lower limit';
  StrUpperLimit = 'Upper limit';
  StrImportMODPATHTime = 'import MODPATH time series';

{$R *.dfm}

{ TUndoImportTimeSeries }

constructor TUndoImportTimeSeries.Create(Model: TCustomModel; var NewTimeSeries: TTimeSeriesReader;
  ImportedNewFile: boolean);
begin
  FModel := Model;
  FImportedNewFile := ImportedNewFile;
  FExistingTimeSeries:= TTimeSeriesReader.Create(Model);
  FExistingTimeSeries.Assign(Model.TimeSeries);
  // Take ownership of NewTimeSeries.
  FNewTimeSeries := NewTimeSeries;
  NewTimeSeries := nil;
end;

function TUndoImportTimeSeries.Description: string;
begin
  result := StrImportMODPATHTime
end;

destructor TUndoImportTimeSeries.Destroy;
begin
  FExistingTimeSeries.Free;
  FNewTimeSeries.Free;
  inherited;
end;

procedure TUndoImportTimeSeries.DoCommand;
begin
  FModel.TimeSeries := FNewTimeSeries;
  EnableMenuItems;
  ForceRedraw;
end;

procedure TUndoImportTimeSeries.ForceRedraw;
begin
  FModel.TimeSeries.Invalidate;
  frmGoPhast.frame3DView.glWidModelView.Invalidate;

  frmGoPhast.frameTopView.ModelChanged := True;
//  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  frmGoPhast.frameFrontView.ModelChanged := True;
//  frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  frmGoPhast.frameSideView.ModelChanged := True;
//  frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TUndoImportTimeSeries.Undo;
begin
  FModel.TimeSeries := FExistingTimeSeries;
  EnableMenuItems;
  ForceRedraw;
  inherited;

end;

procedure TUndoImportTimeSeries.EnableMenuItems;
//var
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
begin
  frmGoPhast.EnableModpathToShapefile;
end;

{ TframeModpathTimeSeriesDisplay }

procedure TframeModpathTimeSeriesDisplay.AssignTimesToComboBox(
  Times: TRealList);
var
  Index: Integer;
begin
  comboTimeToPlot.Items.Clear;
  comboTimeToPlot.Items.Capacity := Times.Count;
  for Index := 0 to Times.Count - 1 do
  begin
    comboTimeToPlot.Items.Add(FloatToStr(Times[Index]));
  end;
  if Times.Count > 0 then
  begin
    udTimeToPlot.Max := Times.Count - 1;
    udTimeToPlot.Min := -udTimeToPlot.Max;
    udTimeToPlot.Position := 0;
  end;

  if (comboTimeToPlot.ItemIndex < 0) and (comboTimeToPlot.Items.Count > 0) then
  begin
    comboTimeToPlot.ItemIndex := 0;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.btnColorSchemesClick(Sender: TObject);
begin
  ShowAForm(TfrmColorSchemes)
end;

procedure TframeModpathTimeSeriesDisplay.comboColorSchemeChange(
  Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.comboModelSelectionChange(
  Sender: TObject);
var
  TimeSeries : TTimeSeriesReader;
  Times: TRealList;
  DisplayLimits: TTimeSeriesDisplayLimits;
  ColorLimits: TTimeSeriesColorLimits;
  ALimitRow: TTimeSeriesLimits;
  ARow: Integer;
  ColorParameters: TColorParameters;
  LocalModel: TCustomModel;
begin
  LocalModel := comboModelSelection.Items.Objects[
    comboModelSelection.ItemIndex] as TCustomModel;
  if LocalModel.ModflowPackages.ModPath.Binary then
  begin
    fedModpathFile.DefaultExt := '.ts_bin';
  end
  else
  begin
    fedModpathFile.DefaultExt := '.ts';
  end;

  TimeSeries := FTimeSeriesList[comboModelSelection.ItemIndex];
  setimeSeriesSize.AsInteger := TimeSeries.TimeSeriesSize;

  try
    fedModpathFile.FileName := TimeSeries.FileName;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  Times := TimeSeries.Times;
  AssignTimesToComboBox(Times);
  udTimeToPlot.Position := TimeSeries.TimeIndex;
  comboTimeToPlot.ItemIndex := TimeSeries.TimeIndex;
  cbShowPathlines.Checked := TimeSeries.Visible;

  DisplayLimits := TimeSeries.DisplayLimits;
  cbLimitToCurrentIn2D.Checked := DisplayLimits.LimitToCurrentIn2D;
  rgShow2D.ItemIndex := Ord(DisplayLimits.ShowChoice);

  ColorLimits := TimeSeries.ColorLimits;

  rgColorBy.ItemIndex := Ord(ColorLimits.ColoringChoice);

  ReadIntLimit(DisplayLimits.ColumnLimits, tslColumn);
  ReadIntLimit(DisplayLimits.RowLimits, tslRow);
  ReadIntLimit(DisplayLimits.LayerLimits, tslLayer);
  ReadIntLimit(DisplayLimits.ParticleGroupLimits, tslParticleGroup);

  ALimitRow := tslColors;

  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := ColorLimits.UseLimit;
  if ColorLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := FloatToStr(ColorLimits.MinColorLimit);
    rdgLimits.Cells[2, ARow] := FloatToStr(ColorLimits.MaxColorLimit);
  end;

  ColorParameters := TimeSeries.ColorParameters;
  comboColorScheme.ItemIndex := ColorParameters.ColorScheme;
  seCycles.AsInteger := ColorParameters.ColorCycles;
  seColorExponent.Value := ColorParameters.ColorExponent;
  jsColorExponent.Value := Round(ColorParameters.ColorExponent*100);

end;

procedure TframeModpathTimeSeriesDisplay.comboTimeToPlotChange(Sender: TObject);
begin
  udTimeToPlot.Position := 0;
end;

constructor TframeModpathTimeSeriesDisplay.Create(Owner: TComponent);
begin
  inherited;
  FTimeSeriesList := TimeSeriesObjectList.Create;
end;

destructor TframeModpathTimeSeriesDisplay.Destroy;
begin
  FTimeSeriesList.Free;
  inherited;
end;

procedure TframeModpathTimeSeriesDisplay.fedModpathFileBeforeDialog(
  Sender: TObject; var AName: string; var AAction: Boolean);
begin
  if AName = '' then
  begin
    if frmGoPhast.sdModpathInput.FileName <> '' then
    begin
      AName := ChangeFileExt(frmGoPhast.sdModpathInput.FileName,
        fedModpathFile.DefaultExt);
    end
    else if frmGoPhast.sdModflowInput.FileName <> '' then
    begin
      AName := ChangeFileExt(frmGoPhast.sdModflowInput.FileName,
        fedModpathFile.DefaultExt);
    end
    else if frmGoPhast.sdSaveDialog.FileName <> '' then
    begin
      AName := ChangeFileExt(frmGoPhast.sdSaveDialog.FileName,
        fedModpathFile.DefaultExt);
    end;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.fedModpathFileChange(Sender: TObject);
var
  TimeSeries: TTimeSeriesReader;
begin
  if FileExists(fedModpathFile.FileName) then
  begin
    TimeSeries := TTimeSeriesReader.Create(frmGoPhast.PhastModel);
    try
      TimeSeries.FileName := fedModpathFile.FileName;
      TimeSeries.ReadFile;
      AssignTimesToComboBox(TimeSeries.Times);
    finally
      TimeSeries.Free;
    end;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.GetData;
var
  TimeSeries : TTimeSeriesReader;
  LocalTimeSeries: TTimeSeriesReader;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Handle;
  if frmGoPhast.PhastModel.ColorSchemes.Count > 0 then
  begin
    UpdateColorSchemes;
  end;
  if frmGoPhast.PhastModel.ModflowPackages.ModPath.Binary then
  begin
    fedModpathFile.DefaultExt := '.ts_bin';
  end
  else
  begin
    fedModpathFile.DefaultExt := '.ts';
  end;
  TimeSeries := frmGoPhast.PhastModel.TimeSeries;

  FTimeSeriesList.Clear;
  comboModelSelection.Items.Clear;
  LocalTimeSeries := TTimeSeriesReader.Create(frmGoPhast.PhastModel);
  FTimeSeriesList.Add(LocalTimeSeries);
  LocalTimeSeries.Assign(TimeSeries);
  comboModelSelection.Items.AddObject(frmGoPhast.PhastModel.DisplayName,
    frmGoPhast.PhastModel);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    comboModelSelection.Visible := True;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        comboModelSelection.Items.AddObject(ChildModel.DisplayName, ChildModel);
        LocalTimeSeries := TTimeSeriesReader.Create(ChildModel);
        FTimeSeriesList.Add(LocalTimeSeries);
        TimeSeries := ChildModel.TimeSeries;
        LocalTimeSeries.Assign(TimeSeries);
      end;
    end;
  end
  else
  begin
    comboModelSelection.Visible := False;
  end;
  lblModelSelection.Visible := comboModelSelection.Visible;
  comboModelSelection.ItemIndex := 0;
  comboModelSelectionChange(nil);

end;

procedure TframeModpathTimeSeriesDisplay.jsColorExponentChanged(
  Sender: TObject);
begin
  if Sender <> seColorExponent then
  begin
    seColorExponent.Value := jsColorExponent.Value / 100
  end;
  pbColorScheme.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.Loaded;
var
  Index: TTimeSeriesLimits;
begin
  inherited;
  pcMain.ActivePageIndex := 0;
  rdgLimits.BeginUpdate;
  try
    rdgLimits.RowCount := Succ(Ord(High(TTimeSeriesLimits)));
    for Index := Low(TTimeSeriesLimits) to High(TTimeSeriesLimits) do
    begin
      rdgLimits.Cells[0,Ord(Index)] := TableCaptions[Index];
    end;
    rdgLimits.Cells[0,0] := StrLimitingFactor;
    rdgLimits.Cells[1,0] := StrLowerLimit;
    rdgLimits.Cells[2,0] := StrUpperLimit;
    Index := tslColors;
    rdgLimits.UseSpecialFormat[1,Ord(Index)] := True;
    rdgLimits.UseSpecialFormat[2,Ord(Index)] := True;
    rdgLimits.SpecialFormat[1,Ord(Index)] := rcf4Real;
    rdgLimits.SpecialFormat[2,Ord(Index)] := rcf4Real;
  finally
    rdgLimits.EndUpdate;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.pbColorSchemePaint(Sender: TObject);
var
  X: integer;
  Fraction: Real;
  AColor: TColor;
  ColorAdjustmentFactor: Real;
begin
  for X := 0 to pbColorScheme.Width - 1 do
  begin
    Fraction := 1 - X / pbColorScheme.Width;
    ColorAdjustmentFactor := seColorExponent.Value;

    AColor := FracAndSchemeToColor(comboColorScheme.ItemIndex,
      Fraction, ColorAdjustmentFactor, seCycles.AsInteger);

    with pbColorScheme.Canvas do
    begin
      Pen.Color := AColor;
      MoveTo(X, 0);
      LineTo(X, pbColorScheme.Height - 1);
    end;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.rdgLimitsSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow >= rdgLimits.FixedRows) then
  begin
    if ARow = Ord(tslColors) then
    begin
      CanSelect := rgColorBy.ItemIndex <> 0;
    end
    else
    begin
      CanSelect := rgShow2D.ItemIndex <> 0;
    end;
    if CanSelect then
    begin
      case ACol of
        0:
          begin
            // do nothing.
          end;
        1,2:
          begin
            CanSelect := rdgLimits.Checked[0,ARow];
          end;
        else Assert(False);
      end;
    end;
  end;

end;

procedure TframeModpathTimeSeriesDisplay.rdgLimitsSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if (ARow in [Ord(tslLayer)..Ord(tslParticleGroup)]) and (ACol in [1,2]) then
  begin
    rdgLimits.Columns[ACol].CheckACell(ACol, ARow, False, True, 0, 1);
  end;
end;

procedure TframeModpathTimeSeriesDisplay.rdgLimitsStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.ReadIntLimit(
  IntLimits: TShowIntegerLimit; ALimitRow: TTimeSeriesLimits);
var
  ARow: Integer;
begin
  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := IntLimits.UseLimit;
  if IntLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := IntToStr(IntLimits.StartLimit);
    rdgLimits.Cells[2, ARow] := IntToStr(IntLimits.EndLimit);
  end;
end;

procedure TframeModpathTimeSeriesDisplay.rgColorByClick(Sender: TObject);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.rgShow2DClick(Sender: TObject);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.seColorExponentChange(Sender: TObject);
begin
  jsColorExponent.Value := Round(seColorExponent.Value * 100);
  pbColorScheme.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.seCyclesChange(Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.SetData;
var
  ImportedNewFile: Boolean;
  Grid: TModflowGrid;
  ExistingTimeSeries: TTimeSeriesReader;
  TimeSeries: TTimeSeriesReader;
  ADate: TDateTime;
  Limits: TTimeSeriesDisplayLimits;
  ColorLimits: TTimeSeriesColorLimits;
  ARow: Integer;
  ColorParameters: TColorParameters;
  Undo: TUndoImportTimeSeries;
  LocalModel: TCustomModel;
begin
  if comboModelSelection.ItemIndex < 0 then
  begin
    Exit;
  end;
  ImportedNewFile := False;
  LocalModel := comboModelSelection.Items.Objects[comboModelSelection.ItemIndex] as TCustomModel;

  Grid := LocalModel.ModflowGrid;
  ExistingTimeSeries := LocalModel.TimeSeries;
  TimeSeries := TTimeSeriesReader.Create(LocalModel);
  try
    TimeSeries.Assign(ExistingTimeSeries);

    TimeSeries.TimeSeriesSize := setimeSeriesSize.AsInteger;
    TimeSeries.FileName := fedModpathFile.FileName;
    if TimeSeries.FileName = '' then
    begin
      TimeSeries.Series.Clear;
      TimeSeries.SeriesV6.Clear;
    end
    else
    begin
      if FileExists(TimeSeries.FileName) then
      begin
        if(TimeSeries.FileName <> ExistingTimeSeries.FileName) then
        begin
          try
            TimeSeries.ReadFile;
          except  on E: EInvalidLayer do
            begin
              Beep;
              MessageDlg(E.message, mtError, [mbOK], 0);
              TimeSeries.FileName := '';
              Exit;
            end;
          end;
          ImportedNewFile := True;
        end
        else
        begin
          if FileAge(TimeSeries.FileName, ADate)
            and (TimeSeries.FileDate <> ADate) then
          begin
            if (MessageDlg(StrTheTimeSeriesFile,
              mtInformation, [mbYes, mbNo], 0) = mrYes) then
            begin
              TimeSeries.ReadFile;
              ImportedNewFile := True;
            end;
          end;
        end;
      end;
      TimeSeries.TimeIndex := comboTimeToPlot.ItemIndex;
      TimeSeries.Visible := cbShowPathlines.Checked;

      Limits := TimeSeries.DisplayLimits;
      Limits.LimitToCurrentIn2D := cbLimitToCurrentIn2D.Checked;
      Limits.ShowChoice := TShowChoice(rgShow2D.ItemIndex);

      if Limits.ShowChoice <> scAll then
      begin
        SetIntLimit(tslColumn, Grid.ColumnCount, Limits.ColumnLimits);
        SetIntLimit(tslRow, Grid.RowCount, Limits.RowLimits);
        SetIntLimit(tslLayer, Grid.LayerCount, Limits.LayerLimits);
        SetIntLimit(tslParticleGroup, TimeSeries.MaxParticleGroup,
          Limits.ParticleGroupLimits);
      end;

      ColorLimits := TimeSeries.ColorLimits;
      ColorLimits.ColoringChoice :=
        TTimeSeriesColorLimitChoice(rgColorBy.ItemIndex);

      if ColorLimits.ColoringChoice <> tscNone then
      begin
        ARow := Ord(tslColors);
        ColorLimits.UseLimit := rdgLimits.Checked[0, ARow];
        if ColorLimits.UseLimit then
        begin
          ColorLimits.MinColorLimit := StrToFloatDef(rdgLimits.Cells[1, ARow], 0);
          ColorLimits.MaxColorLimit := StrToFloatDef(rdgLimits.Cells[2, ARow], 1);
        end;
      end;

      ColorParameters := TimeSeries.ColorParameters;
      ColorParameters.ColorScheme := comboColorScheme.ItemIndex;
      ColorParameters.ColorCycles := seCycles.AsInteger;
      ColorParameters.ColorExponent := seColorExponent.Value;
    end;

    Undo := TUndoImportTimeSeries.Create(LocalModel, TimeSeries, ImportedNewFile);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    TimeSeries.Free
  end;

end;

procedure TframeModpathTimeSeriesDisplay.SetIntLimit(
  LimitRow: TTimeSeriesLimits; DefaultLimit: integer;
  IntLimit: TShowIntegerLimit);
var
  ARow: Integer;
begin
  ARow := Ord(LimitRow);
  IntLimit.UseLimit := rdgLimits.Checked[0, ARow];
  if IntLimit.UseLimit then
  begin
    IntLimit.StartLimit := StrToIntDef(rdgLimits.Cells[1, ARow], 1);
    IntLimit.EndLimit := StrToIntDef(rdgLimits.Cells[2, ARow], DefaultLimit);
  end;
end;

procedure TframeModpathTimeSeriesDisplay.udTimeToPlotChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
begin
  case Direction of
    updNone: ;
    updUp:
      begin
        if comboTimeToPlot.ItemIndex < comboTimeToPlot.Items.Count -1 then
        begin
          comboTimeToPlot.ItemIndex := comboTimeToPlot.ItemIndex+1;
          SetData;
        end;
      end;
    updDown:
      begin
        if comboTimeToPlot.ItemIndex > 0 then
        begin
          comboTimeToPlot.ItemIndex := comboTimeToPlot.ItemIndex-1;
          SetData;
        end;
      end;
  end;
  udTimeToPlot.ControlStyle := udTimeToPlot.ControlStyle - [csCaptureMouse];
end;

procedure TframeModpathTimeSeriesDisplay.UpdateColorSchemes;
begin
  UpdateColorScheme(comboColorScheme, pbColorScheme);
end;

end.

