unit frameModpathEndpointDisplayUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, RbwDataGrid4, StdCtrls, ExtCtrls, JvSpin, JvExControls,
  JvxSlider, Mask, JvExMask, JvToolEdit, ComCtrls, frameModpathDisplayUnit,
  PathlineReader, UndoItems, PhastModelUnit, SubscriptionUnit, LegendUnit,
  GrayTabs;

type
  TEndLimits = (elNone, elColors, elStartLayer, elStartRow, elStartColumn, elStartZone,
    elEndLayer, elEndRow, elEndColumn, elEndZone,
    elTrackingTime, elReleaseTime, elGroup);

resourcestring
  StartingLayer = 'Starting layer';
  StartingRow = 'Starting row';
  StartingColumn = 'Starting column';
  StartingZone = 'Starting zone';
  EndingLayer = 'Ending layer';
  EndingRow = 'Ending row';
  EndingColumn = 'Ending column';
  EndingZone = 'Ending zone';
  TrackingTime = 'Tracking time';
  ReleaseTime = 'Release time';
  Group = 'Group';

const
  TableCaptions: array[Low(TEndLimits)..High(TEndLimits)] of string =
    ('', Colorlimits,
    StartingLayer, StartingRow, StartingColumn, StartingZone,
    EndingLayer, EndingRow, EndingColumn, EndingZone,
    TrackingTime, ReleaseTime, Group);

type
  TUndoImportEndpoints = class(TCustomUndo)
  private
    FExistingEndPoints: TEndPointReader;
    FNewEndPoints: TEndPointReader;
    FImportedNewFile: Boolean;
    FModel: TCustomModel;
    procedure ForceRedraw;
    procedure EnableMenuItems;
  public
    Constructor Create(Model: TCustomModel; var NewEndPoints: TEndPointReader;
      ImportedNewFile: boolean);
    Destructor Destroy; override;
    function Description: string; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TframeModpathEndpointDisplay = class(TFrame)
    pcMain: TPageControl;
    tabBasic: TTabSheet;
    lblModpathFile: TLabel;
    lblColorScheme: TLabel;
    pbColorScheme: TPaintBox;
    lblColorAdjustment: TLabel;
    lblCycles: TLabel;
    fedModpathFile: TJvFilenameEdit;
    cbShowPathlines: TCheckBox;
    cbLimitToCurrentIn2D: TCheckBox;
    comboColorScheme: TComboBox;
    jsColorExponent: TJvxSlider;
    seColorExponent: TJvSpinEdit;
    seCycles: TJvSpinEdit;
    tabOptions: TTabSheet;
    rgShow2D: TRadioGroup;
    rgWhereToPlot: TRadioGroup;
    rgColorBy: TRadioGroup;
    rdgLimits: TRbwDataGrid4;
    comboModelSelection: TComboBox;
    lblModelSelection: TLabel;
    tabLegend: TTabSheet;
    imLegend: TImage;
    pnlLegend: TPanel;
    lblMethod: TLabel;
    lblColorLegendRows: TLabel;
    comboMethod: TComboBox;
    seLegendRows: TJvSpinEdit;
    rdgLegend: TRbwDataGrid4;
    dlgFontLegend: TFontDialog;
    splColor: TSplitter;
    tmrLegend: TTimer;
    btnFont: TButton;
    seEndPointSize: TJvSpinEdit;
    lblEndPointSize: TLabel;
    btnColorSchemes: TButton;
    procedure rdgLimitsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLimitsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLimitsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rgShow2DClick(Sender: TObject);
    procedure rgColorByClick(Sender: TObject);
    procedure pbColorSchemePaint(Sender: TObject);
    procedure comboColorSchemeChange(Sender: TObject);
    procedure jsColorExponentChanged(Sender: TObject);
    procedure seColorExponentChange(Sender: TObject);
    procedure seCyclesChange(Sender: TObject);
    procedure fedModpathFileBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure comboModelSelectionChange(Sender: TObject);
    procedure comboMethodChange(Sender: TObject);
    procedure rdgLegendEndUpdate(Sender: TObject);
    procedure rdgLegendSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLegendStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure seLegendRowsChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure tmrLegendTimer(Sender: TObject);
    procedure btnColorSchemesClick(Sender: TObject);
  protected
    procedure Loaded; override;
  private
    FStoredLegend: TLegend;
    FLegend: TLegend;
    FEndPointsList: TEndPointObjectList;
    FLegendFont: TFont;
    FUpdatingLegend: Boolean;
    FStartTime: TDateTime;
    FFontAssigned: Boolean;
    FGettingData: Boolean;
    procedure SetIntLimit(LimitRow: TEndLimits; DefaultLimit: integer;
      IntLimit: TShowIntegerLimit);
    procedure SetFloatLimit(LimitRow: TEndLimits; DefaultLimit: Double;
      FloatLimit: TShowFloatLimit);
    procedure ReadIntLimit(IntLimits: TShowIntegerLimit; ALimitRow: TEndLimits);
    procedure ReadFloatLimits(FloatLimits: TShowFloatLimit; ALimitRow: TEndLimits);
    procedure UpdateLegend;
    procedure UpdateLegendAfterDelay;
//    function GetLegendDataSource: TObserver;
//    procedure SetLegendDataSource(const Value: TObserver);
    { Private declarations }
  public
    procedure GetData;
    procedure SetData;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property LegendFont: TFont read FLegendFont;
    procedure UpdateLabelsAndLegend;
    procedure UpdateColorSchemes;
//    property LegendDataSource: TObserver read GetLegendDataSource
//      write SetLegendDataSource;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, ColorSchemes, ModflowGridUnit, ModelMuseUtilities, RbwParser,
  System.Math, LayerStructureUnit, frmCustomGoPhastUnit, frmColorSchemesUnit;

resourcestring
  StrLimitingFactor = 'Limiting factor';
  StrLowerLimit = 'Lower limit';
  StrUpperLimit = 'Upper limit';
  StrImportEndpoints = 'import endpoints';
  StrConfigureEndpoints = 'configure endpoints';
  StrTheEndpointFileOn = 'The endpoint file on disk has a different date tha' +
  'n the file that was imported into ModelMuse.  Do you want to import the n' +
  'ew file?';

{$R *.dfm}

const
  OneSecond = 1/24/3600;

{ TUndoImportEndpoints }

constructor TUndoImportEndpoints.Create(Model: TCustomModel; var NewEndPoints: TEndPointReader;
  ImportedNewFile: boolean);
begin
  FModel := Model;
  FImportedNewFile := ImportedNewFile;
  FExistingEndPoints:= TEndPointReader.Create(Model);
  FExistingEndPoints.Assign(frmGoPhast.PhastModel.EndPoints);
  // Take ownership of NewEndPoints.
  FNewEndPoints := NewEndPoints;
  NewEndPoints := nil;
end;

function TUndoImportEndpoints.Description: string;
begin
  if FImportedNewFile then
  begin
    result := StrImportEndpoints;
  end
  else
  begin
    result := StrConfigureEndpoints;
  end;
end;

destructor TUndoImportEndpoints.Destroy;
begin
  FExistingEndPoints.Free;
  FNewEndPoints.Free;
  inherited;
end;

procedure TUndoImportEndpoints.DoCommand;
begin
  FModel.EndPoints := FNewEndPoints;
  EnableMenuItems;
  ForceRedraw;
end;

procedure TUndoImportEndpoints.ForceRedraw;
begin
  FModel.EndPoints.Invalidate;
  frmGoPhast.frame3DView.glWidModelView.Invalidate;

  frmGoPhast.frameTopView.ModelChanged := True;
//  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  frmGoPhast.frameFrontView.ModelChanged := True;
//  frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  frmGoPhast.frameSideView.ModelChanged := True;
//  frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TUndoImportEndpoints.Undo;
begin
  FModel.EndPoints := FExistingEndPoints;
  EnableMenuItems;
  ForceRedraw;
end;

procedure TUndoImportEndpoints.EnableMenuItems;
//var
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
begin
  frmGoPhast.EnableModpathToShapefile;
//  frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled :=
//    (frmGoPhast.PhastModel.EndPoints.Points.Count > 0)
//    or (frmGoPhast.PhastModel.EndPoints.Points.Count > 0);
//  if not frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled
//    and frmGoPhast.PhastModel.LgrUsed then
//  begin
//    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
//      frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled :=
//        (ChildModel.EndPoints.Points.Count > 0)
//        or (ChildModel.EndPoints.Points.Count > 0);
//      if frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled then
//      begin
//        break;
//      end;
//    end;
//  end;
//  frmGoPhast.miEndpointsatEndingLocationstoShapefile.Enabled :=
//    frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled;
end;

{ TframeModpathEndpointDisplay }

procedure TframeModpathEndpointDisplay.btnColorSchemesClick(Sender: TObject);
begin
  ShowAForm(TfrmColorSchemes)
end;

procedure TframeModpathEndpointDisplay.btnFontClick(Sender: TObject);
begin
  dlgFontLegend.Font := FLegendFont;
  if dlgFontLegend.Execute then
  begin
    FLegendFont.Assign(dlgFontLegend.Font);
    UpdateLegend
  end;
end;

procedure TframeModpathEndpointDisplay.comboColorSchemeChange(Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeModpathEndpointDisplay.comboMethodChange(Sender: TObject);
begin
  rdgLegend.Enabled := comboMethod.ItemIndex = 1;
  seLegendRows.Enabled := rdgLegend.Enabled;
  if rdgLegend.Enabled then
  begin
    rdgLegend.Color := clWindow;
  end
  else
  begin
    rdgLegend.Color := clBtnFace;
  end;
  UpdateLegend;
end;

procedure TframeModpathEndpointDisplay.comboModelSelectionChange(
  Sender: TObject);
var
  EndPoints: TEndPointReader;
  Limits: TEndPointDisplayLimits;
  ColorParameters: TColorParameters;
  ALimitRow: TEndLimits;
  ARow: Integer;
  LocalModel: TCustomModel;
begin
  LocalModel := comboModelSelection.Items.Objects[
    comboModelSelection.ItemIndex] as TCustomModel;
  if LocalModel.ModflowPackages.ModPath.Binary then
  begin
    fedModpathFile.DefaultExt := '.end_bin';
  end
  else
  begin
    fedModpathFile.DefaultExt := '.end';
  end;
  EndPoints := FEndPointsList[comboModelSelection.ItemIndex];
  fedModpathFile.FileName := EndPoints.FileName;
  seEndPointSize.AsInteger := EndPoints.EndPointSize;

  cbShowPathlines.Checked := EndPoints.Visible;
  Limits := EndPoints.DisplayLimits;

  cbLimitToCurrentIn2D.Checked := Limits.LimitToCurrentIn2D;
  rgShow2D.ItemIndex := Ord(Limits.ShowChoice);
  rgWhereToPlot.ItemIndex := Ord(Limits.WhereToPlot);

  ReadIntLimit(Limits.StartColumnLimits, elStartColumn);
  ReadIntLimit(Limits.StartRowLimits, elStartRow);
  ReadIntLimit(Limits.StartLayerLimits, elStartLayer);
  ReadIntLimit(Limits.StartZoneLimits, elStartZone);

  ReadIntLimit(Limits.EndColumnLimits, elEndColumn);
  ReadIntLimit(Limits.EndRowLimits, elEndRow);
  ReadIntLimit(Limits.EndLayerLimits, elEndLayer);
  ReadIntLimit(Limits.EndZoneLimits, elEndZone);
  ReadIntLimit(Limits.ParticleGroupLimits, elGroup);

  ReadFloatLimits(Limits.ReleaseTimeLimits, elReleaseTime);
  ReadFloatLimits(Limits.TrackingTimeLimits, elTrackingTime);

  rgColorBy.ItemIndex := Ord(EndPoints.ColorLimits.ColoringChoice);

  ALimitRow := elColors;
  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := EndPoints.ColorLimits.UseLimit;
  if EndPoints.ColorLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := FloatToStr(EndPoints.ColorLimits.MinColorLimit);
    rdgLimits.Cells[2, ARow] := FloatToStr(EndPoints.ColorLimits.MaxColorLimit);
  end;

  ColorParameters := EndPoints.ColorParameters;
  comboColorScheme.ItemIndex := ColorParameters.ColorScheme;
  seCycles.AsInteger := ColorParameters.ColorCycles;
  seColorExponent.Value := ColorParameters.ColorExponent;
  jsColorExponent.Value := Round(ColorParameters.ColorExponent*100);
end;

constructor TframeModpathEndpointDisplay.Create(Owner: TComponent);
begin
  inherited;
  FEndPointsList := TEndPointObjectList.Create;
  FLegendFont := TFont.Create
end;

destructor TframeModpathEndpointDisplay.Destroy;
begin
  FLegendFont.Free;
  FEndPointsList.Free;
  inherited;
end;

procedure TframeModpathEndpointDisplay.fedModpathFileBeforeDialog(
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

procedure TframeModpathEndpointDisplay.GetData;
var
  EndPoints: TEndPointReader;
  LocalEndPoints: TEndPointReader;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  index: Integer;
begin
  FGettingData := True;
  try
    Handle;

    if frmGoPhast.PhastModel.ColorSchemes.Count > 0 then
    begin
      UpdateColorSchemes;
    end;

    if not FFontAssigned then
    begin
      FFontAssigned := True;
      FLegendFont.Assign(Font);
    end;
    if FLegend <> nil then
    begin
      comboMethod.ItemIndex := Ord(FLegend.ValueAssignmentMethod)-1;
      seLegendRows.Enabled := FLegend.ValueAssignmentMethod = vamManual;
      if FLegend.ValueAssignmentMethod = vamManual then
      begin
        seLegendRows.AsInteger := FLegend.Values.Count;
        seLegendRows.OnChange(nil);
        case FLegend.Values.DataType of
          rdtDouble:
            begin
              for index := 0 to FLegend.Values.Count - 1 do
              begin
                rdgLegend.Cells[0,index+1] := FloatToStr(FLegend.Values.RealValues[index]);
              end;
            end;
          rdtInteger:
            begin
              for index := 0 to FLegend.Values.Count - 1 do
              begin
                rdgLegend.Cells[0,index+1] := IntToStr(FLegend.Values.IntValues[index]);
              end;
            end;
          rdtBoolean:
            begin
              for index := 0 to FLegend.Values.Count - 1 do
              begin
                if FLegend.Values.BooleanValues[index] then
                begin
                  rdgLegend.Cells[0,index+1] := 'True';
                end
                else
                begin
                  rdgLegend.Cells[0,index+1] := 'False';
                end;
              end;
            end;
          rdtString:
            begin
              for index := 0 to FLegend.Values.Count - 1 do
              begin
                rdgLegend.Cells[0,index+1] := FLegend.Values.StringValues[index];
              end;
            end;
        end;
      end;
    end;

    if frmGoPhast.PhastModel.ModflowPackages.ModPath.Binary then
    begin
      fedModpathFile.DefaultExt := '.end_bin';
    end
    else
    begin
      fedModpathFile.DefaultExt := '.end';
    end;
    EndPoints := frmGoPhast.PhastModel.EndPoints;

    FEndPointsList.Clear;
    comboModelSelection.Items.Clear;
    LocalEndPoints := TEndPointReader.Create(frmGoPhast.PhastModel);
    FEndPointsList.Add(LocalEndPoints);
    LocalEndPoints.Assign(EndPoints);
    comboModelSelection.Items.AddObject(frmGoPhast.PhastModel.DisplayName, frmGoPhast.PhastModel);
    if frmGoPhast.PhastModel.LgrUsed then
    begin
      comboModelSelection.Visible := True;
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        comboModelSelection.Items.AddObject(ChildModel.DisplayName, ChildModel);
        LocalEndPoints := TEndPointReader.Create(ChildModel);
        FEndPointsList.Add(LocalEndPoints);
        EndPoints := ChildModel.EndPoints;
        LocalEndPoints.Assign(EndPoints);
      end;
    end
    else
    begin
      comboModelSelection.Visible := False;
    end;
    lblModelSelection.Visible := comboModelSelection.Visible;
    comboModelSelection.ItemIndex := 0;
    comboModelSelectionChange(nil);
  finally
    FGettingData := False;
  end;

end;

//function TframeModpathEndpointDisplay.GetLegendDataSource: TObserver;
//begin
//  if FLegend = nil then
//  begin
//    result := nil
//  end
//  else
//  begin
//    result := FLegend.ValueSource;
//  end;
//end;

procedure TframeModpathEndpointDisplay.jsColorExponentChanged(Sender: TObject);
begin
  if Sender <> seColorExponent then
  begin
    seColorExponent.Value := jsColorExponent.Value / 100
  end;
  pbColorScheme.Invalidate;
end;

procedure TframeModpathEndpointDisplay.Loaded;
var
  Index: TEndLimits;
begin
  inherited;
  pcMain.ActivePageIndex := 0;
  rdgLimits.BeginUpdate;
  try
    rdgLimits.RowCount := Succ(Ord(High(TEndLimits)));
    for Index := Low(TEndLimits) to High(TEndLimits) do
    begin
      rdgLimits.Cells[0,Ord(Index)] := TableCaptions[Index];
    end;
    rdgLimits.Cells[0,0] := StrLimitingFactor;
    rdgLimits.Cells[1,0] := StrLowerLimit;
    rdgLimits.Cells[2,0] := StrUpperLimit;
    for Index := elTrackingTime to elReleaseTime do
    begin
      rdgLimits.UseSpecialFormat[1,Ord(Index)] := True;
      rdgLimits.UseSpecialFormat[2,Ord(Index)] := True;
      rdgLimits.SpecialFormat[1,Ord(Index)] := rcf4Real;
      rdgLimits.SpecialFormat[2,Ord(Index)] := rcf4Real;
    end;
    Index := elColors;
    rdgLimits.UseSpecialFormat[1,Ord(Index)] := True;
    rdgLimits.UseSpecialFormat[2,Ord(Index)] := True;
    rdgLimits.SpecialFormat[1,Ord(Index)] := rcf4Real;
    rdgLimits.SpecialFormat[2,Ord(Index)] := rcf4Real;
  finally
    rdgLimits.EndUpdate;
  end;
  FLegend := frmGoPhast.PhastModel.EndPointLegend;
  FLegend.LegendType := ltEndpoints;
end;

procedure TframeModpathEndpointDisplay.pbColorSchemePaint(Sender: TObject);
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

procedure TframeModpathEndpointDisplay.rdgLegendEndUpdate(Sender: TObject);
begin
  if seLegendRows <> nil then
  begin
    seLegendRows.AsInteger := rdgLegend.RowCount -1;
  end;
end;

procedure TframeModpathEndpointDisplay.rdgLegendSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  UpdateLegendAfterDelay;
end;

procedure TframeModpathEndpointDisplay.rdgLegendStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  UpdateLegendAfterDelay;
end;

procedure TframeModpathEndpointDisplay.rdgLimitsSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if (ARow >= rdgLimits.FixedRows) then
  begin
    if ARow = Ord(elColors) then
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

procedure TframeModpathEndpointDisplay.rdgLimitsSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if (ARow in [Ord(elStartLayer)..Ord(elEndZone), Ord(elGroup)]) and (ACol in [1,2]) then
  begin
    rdgLimits.Columns[ACol].CheckACell(ACol, ARow, False, True, 0, 1);
  end;
end;

procedure TframeModpathEndpointDisplay.rdgLimitsStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathEndpointDisplay.ReadFloatLimits(
  FloatLimits: TShowFloatLimit; ALimitRow: TEndLimits);
var
  ARow: Integer;
begin
  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := FloatLimits.UseLimit;
  if FloatLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := FloatToStr(FloatLimits.StartLimit);
    rdgLimits.Cells[2, ARow] := FloatToStr(FloatLimits.EndLimit);
  end;
end;

procedure TframeModpathEndpointDisplay.ReadIntLimit(
  IntLimits: TShowIntegerLimit; ALimitRow: TEndLimits);
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

procedure TframeModpathEndpointDisplay.rgColorByClick(Sender: TObject);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathEndpointDisplay.rgShow2DClick(Sender: TObject);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathEndpointDisplay.seColorExponentChange(Sender: TObject);
begin
  jsColorExponent.Value := Round(seColorExponent.Value * 100);
  pbColorScheme.Invalidate;
end;

procedure TframeModpathEndpointDisplay.seCyclesChange(Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeModpathEndpointDisplay.seLegendRowsChange(Sender: TObject);
begin
  if rdgLegend.RowCount <> seLegendRows.AsInteger + 1 then
  begin
    rdgLegend.RowCount := seLegendRows.AsInteger + 1;
    UpdateLegendAfterDelay;
  end;
end;

procedure TframeModpathEndpointDisplay.SetData;
var
  EndPoints: TEndPointReader;
  Limits: TEndPointDisplayLimits;
  ColorParameters: TColorParameters;
  Grid: TModflowGrid;
  ExistingEndPoints: TEndPointReader;
  ADate: TDateTime;
  Undo: TUndoImportEndPoints;
  ColorLimits: TEndPointColorLimits;
  ImportedNewFile: Boolean;
  ARow: Integer;
  LocalModel: TCustomModel;
begin
  inherited;
  if comboModelSelection.ItemIndex < 0 then
  begin
    Exit;
  end;
  FLegend.ValueSourceInterface := nil;
  LocalModel := comboModelSelection.Items.Objects[
    comboModelSelection.ItemIndex] as TCustomModel;
  ImportedNewFile := False;
  Grid := LocalModel.ModflowGrid;
  ExistingEndPoints := LocalModel.EndPoints;
  EndPoints := TEndPointReader.Create(LocalModel);
  try
    EndPoints.Assign(ExistingEndPoints);

    EndPoints.EndPointSize := seEndPointSize.AsInteger;
    EndPoints.FileName := fedModpathFile.FileName;
    if EndPoints.FileName = '' then
    begin
      EndPoints.Clear;
    end
    else
    begin
      if FileExists(EndPoints.FileName) then
      begin
        if(EndPoints.FileName <> ExistingEndPoints.FileName) then
        begin
          try
            EndPoints.ReadFile;
          except  on E: EInvalidLayer do
            begin
              Beep;
              MessageDlg(E.message, mtError, [mbOK], 0);
              EndPoints.FileName := '';
              Exit;
            end;
          end;
          ImportedNewFile := True;
        end
        else
        begin
          if FileAge(EndPoints.FileName, ADate)
            and (EndPoints.FileDate <> ADate) then
          begin
            if (MessageDlg(StrTheEndpointFileOn,
              mtInformation, [mbYes, mbNo], 0) = mrYes) then
            begin
              EndPoints.ReadFile;
              ImportedNewFile := True;
            end;
          end;
        end;
      end;
      EndPoints.Visible := cbShowPathlines.Checked;

      Limits := EndPoints.DisplayLimits;

      Limits.LimitToCurrentIn2D := cbLimitToCurrentIn2D.Checked;
      Limits.ShowChoice := TEndpointShowChoice(rgShow2D.ItemIndex);
      Limits.WhereToPlot := TWhereToPlot(rgWhereToPlot.ItemIndex);

      if Limits.ShowChoice <> escAll then
      begin
        SetIntLimit(elStartColumn, Grid.ColumnCount, Limits.StartColumnLimits);
        SetIntLimit(elStartRow, Grid.RowCount, Limits.StartRowLimits);
        SetIntLimit(elStartLayer, Grid.LayerCount, Limits.StartLayerLimits);
        SetIntLimit(elStartZone, EndPoints.MaxStartZone, Limits.StartZoneLimits);

        SetIntLimit(elEndColumn, Grid.ColumnCount, Limits.EndColumnLimits);
        SetIntLimit(elEndRow, Grid.RowCount, Limits.EndRowLimits);
        SetIntLimit(elEndLayer, Grid.LayerCount, Limits.EndLayerLimits);
        SetIntLimit(elEndZone, EndPoints.MaxEndZone, Limits.EndZoneLimits);
        SetIntLimit(elGroup, EndPoints.MaxParticleGroup, Limits.ParticleGroupLimits);

        SetFloatLimit(elReleaseTime, EndPoints.MaxReleaseTime,
          Limits.ReleaseTimeLimits);
        SetFloatLimit(elTrackingTime, EndPoints.MaxTrackingTime,
          Limits.TrackingTimeLimits);
      end;

      ColorLimits := EndPoints.ColorLimits;
      ColorLimits.ColoringChoice :=
        TEndpointColorLimitChoice(rgColorBy.ItemIndex);

      if ColorLimits.ColoringChoice <> elcNone then
      begin
        ARow := Ord(elColors);
        ColorLimits.UseLimit := rdgLimits.Checked[0, ARow];
        if ColorLimits.UseLimit then
        begin
          ColorLimits.MinColorLimit := StrToFloatDef(rdgLimits.Cells[1, ARow], 0);
          ColorLimits.MaxColorLimit := StrToFloatDef(rdgLimits.Cells[2, ARow], 1);
        end;
      end;

      ColorParameters := EndPoints.ColorParameters;
      ColorParameters.ColorScheme := comboColorScheme.ItemIndex;
      ColorParameters.ColorCycles := seCycles.AsInteger;
      ColorParameters.ColorExponent := seColorExponent.Value;
    end;

    Undo := TUndoImportEndPoints.Create(LocalModel, EndPoints, ImportedNewFile);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    EndPoints.Free;
  end;

  UpdateLabelsAndLegend;

end;

procedure TframeModpathEndpointDisplay.SetFloatLimit(LimitRow: TEndLimits;
  DefaultLimit: Double; FloatLimit: TShowFloatLimit);
var
  ARow: Integer;
begin
  ARow := Ord(LimitRow);
  FloatLimit.UseLimit := rdgLimits.Checked[0, ARow];
  if FloatLimit.UseLimit then
  begin
    FloatLimit.StartLimit := StrToFloatDef(rdgLimits.Cells[1, ARow], 0);
    FloatLimit.EndLimit := StrToFloatDef(rdgLimits.Cells[2, ARow], DefaultLimit);
  end;
end;

procedure TframeModpathEndpointDisplay.SetIntLimit(LimitRow: TEndLimits;
  DefaultLimit: integer; IntLimit: TShowIntegerLimit);
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

//procedure TframeModpathEndpointDisplay.SetLegendDataSource(
//  const Value: TObserver);
//begin
//  if FLegend <> nil then
//  begin
//    FLegend.ValueSource := Value;
//  end;
//end;

procedure TframeModpathEndpointDisplay.tmrLegendTimer(Sender: TObject);
begin
  if Now - FStartTime > OneSecond then
  begin
    tmrLegend.Enabled := False;
    UpdateLegend;
  end;
end;

procedure TframeModpathEndpointDisplay.UpdateColorSchemes;
begin
  UpdateColorScheme(comboColorScheme, pbColorScheme);
end;

procedure TframeModpathEndpointDisplay.UpdateLabelsAndLegend;
var
  LocalModel: TCustomModel;
begin
  if FGettingData or (frmGoPhast.PhastModel = nil)
    or (csDestroying in frmGoPhast.PhastModel.ComponentState)
    or frmGoPhast.PhastModel.Clearing or (FLegend = nil)  then
  begin
    Exit;
  end;

  if comboModelSelection.ItemIndex < 0 then
  begin
    Exit;
  end;

  LocalModel := comboModelSelection.Items.Objects[
    comboModelSelection.ItemIndex] as TCustomModel;

  FLegend.ValueSourceInterface := LocalModel.EndPoints;

//  SetMinMaxLabels;
  UpdateLegend;
end;

procedure TframeModpathEndpointDisplay.UpdateLegend;
var
  BitMap: TBitmap;
  Index: Integer;
  DummyRect: TRect;
begin
  if FUpdatingLegend or (csDestroying in ComponentState)
    or (frmGoPhast = nil) or (frmGoPhast.PhastModel = nil)
    or frmGoPhast.PhastModel.Clearing
    or (csDestroying in frmGoPhast.PhastModel.ComponentState) then
  begin
    Exit;
  end;

  FUpdatingLegend := True;
  try
    tabLegend.TabVisible := Assigned(FLegend.ValueSourceInterface);
    if tabLegend.TabVisible then
    begin
      if FStoredLegend <> nil then
      begin
        FLegend.Assign(FStoredLegend);
        Exit;
      end;
      FLegend.ValueAssignmentMethod :=
        TValueAssignmentMethod(comboMethod.ItemIndex + 1);
      rdgLegend.BeginUpdate;
      try
        FLegend.ColoringLimits.LogTransform :=
          rgColorBy.ItemIndex = Ord(elcLogTrackingTime);
        case FLegend.ValueAssignmentMethod of
          vamNoLegend: Exit;
          vamAutomatic:
            begin
//              if not FLegend.ValueSource.UpToDate then
//              begin
//                Exit;
//              end;
              FLegend.AutoAssignValues;
              case FLegend.Values.DataType of
                rdtDouble: rdgLegend.Columns[0].Format := rcf4Real;
                rdtInteger: rdgLegend.Columns[0].Format := rcf4Integer;
                rdtBoolean: rdgLegend.Columns[0].Format := rcf4Boolean;
                rdtString: rdgLegend.Columns[0].Format := rcf4String;
                else Assert(False);
              end;
              seLegendRows.AsInteger := FLegend.Values.Count;
              seLegendRowsChange(nil);
              for Index := 0 to FLegend.Values.Count - 1 do
              begin
                case FLegend.Values.DataType of
                  rdtDouble:
                    begin
                      rdgLegend.Cells[0,Index + 1] :=
                        FloatToStr(FLegend.Values.RealValues[Index]);
                    end;
                  rdtInteger:
                    begin
                      rdgLegend.Cells[0,Index + 1] :=
                        IntToStr(FLegend.Values.IntValues[Index]);
                    end;
                  rdtBoolean:
                    begin
                      rdgLegend.Cells[0,Index + 1] := '';
                      rdgLegend.Checked[0,Index + 1] :=
                        FLegend.Values.BooleanValues[Index];
                    end;
                  rdtString:
                    begin
                      rdgLegend.Cells[0,Index + 1] :=
                        FLegend.Values.StringValues[Index];
                    end;
                  else Assert(False);
                end;
              end;
            end;
          vamManual:
            begin
              FLegend.Values.Count := seLegendRows.AsInteger;
              for Index := 0 to FLegend.Values.Count - 1 do
              begin
                case FLegend.Values.DataType of
                  rdtDouble:
                    begin
                      if rgColorBy.ItemIndex = Ord(elcLogTrackingTime) then
                      begin
                        FLegend.Values.RealValues[Index] := (
                          StrToFloatDef(rdgLegend.Cells[0,Index + 1], 0));
                      end
                      else
                      begin
                        FLegend.Values.RealValues[Index] :=
                          StrToFloatDef(rdgLegend.Cells[0,Index + 1], 0);
                      end;
                    end;
                  rdtInteger:
                    begin
                      FLegend.Values.IntValues[Index] :=
                        StrToIntDef(rdgLegend.Cells[0,Index + 1], 0);
                    end;
                  rdtBoolean:
                    begin
                      FLegend.Values.BooleanValues[Index] :=
                        rdgLegend.Checked[0,Index + 1];
                    end;
                  rdtString:
                    begin
                      FLegend.Values.StringValues[Index] :=
                        rdgLegend.Cells[0,Index + 1];
                    end;
                  else Assert(False);
                end;
              end;

            end;
          else Assert(False);
        end;
      finally
        rdgLegend.EndUpdate;
      end;

      if FLegend.AssignFractions then
      begin
        BitMap := TBitMap.Create;
        try
          BitMap.Canvas.Font := FLegendFont;
          BitMap.Width := imLegend.Width;
          BitMap.Height := imLegend.Height;
          FLegend.Draw(BitMap.Canvas, 10, 10, DummyRect, FLegendFont);
          imLegend.Picture.Assign(BitMap);
        finally
          BitMap.Free;
        end;
      end;
    end;
  finally
    FUpdatingLegend := False;
  end;
end;

procedure TframeModpathEndpointDisplay.UpdateLegendAfterDelay;
begin
  FStartTime := Now;
  tmrLegend.Enabled := True;
end;

end.

