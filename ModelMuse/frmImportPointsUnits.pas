{@name defines @link(TfrmImportPoints) which is used to import scattered point
 data into GoPhast.}
unit frmImportPointsUnits;

interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ExtCtrls, Grids,
  RbwDataGrid4, Buttons, ComCtrls, frmImportShapefileUnit, JvExControls,
  JvxCheckListBox, Mask, JvExMask, JvSpin, DataSetUnit, ScreenObjectUnit,
  GrayTabs;

type
  TChdColumns = (ccStartTime, ccEndTime, ccStartHead, ccEndHead);
  TDrnColumns = (dcStartTime, dcEndTime, dcElevation, dcConductance);
  TGhbColumns = (gcStartTime, gcEndTime, gcBoundaryHead, gcConductance);
  TRivColumns = (rcStartTime, rcEndTime, rcStage, rcConductance, rcBottom);
  TWelColumns = (wcStartTime, wcEndTime, wcPumpingRate);
  THobColumns = (hcName, hcTime, hcHead, hcStatistic, hcStatFlag);
  TFootprintWellColumns = (fwcWithdrawal, fwcObjectName);
  TBoundaryArrangment = (baOneSP_PerLine, bsOneBoundaryPerLine);
  TUzf6SteadyColumns = (u6scSurfaceDepressionDepth, u6scVerticalSaturatedK,
    u6scResidualWaterContent,u6scSaturatedWaterContent,u6scInitialWaterContent,
    u6scBrooksCoreyEpsilon);
  TUzf6TransientColumns = (u6tcStartTime,u6tcEndTime,u6tcInfiltration,
    u6tcPotentialET,u6tcExtinctionDepth, u6tcExtinctionWaterContent,
    u6tcAirEntryPotential,u6tcRootPotential, u6tcRootActivity);

{
    SurfaceDepressionDepth
    VerticalSaturatedK
    ResidualWaterContent
    SaturatedWaterContent
    InitialWaterContent
    BrooksCoreyEpsilon

    Infiltration: double;
    PotentialET: double;
    ExtinctionDepth: double;
    ExtinctionWaterContent: double;
    AirEntryPotential: double;
    RootPotential: double;
    RootActivity: double;

    GwtStatus
    SpecifiedConcentrations: TGwtCellData;
    InfiltrationConcentrations: TGwtCellData;
    EvapConcentrations: TGwtCellData;
}

  TObsUtilColumns = (oucName, oucObsType);
  TCalibObsColumns = (cocGroup, cocTime, cocValue, cocWeight);

  {@abstract(@name is the command used to import
    points or reverse the import.)}
  TUndoImportPoints = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  {@name imports scattered point data into GoPhast.
  @unOrderedList(
  @Item(OnCreate = @link(FormCreate).)
  @Item(OnKeyUp = @link(FormKeyUp).)
  )
  }
  TfrmImportPoints = class(TfrmCustomGoPhast)
    // The Cancel button.
    btnCancel: TBitBtn;
    // @name is the Help button.
    btnHelp: TBitBtn;
    // The OK button.
    btnOK: TBitBtn;
    // @name is the button the user uses to open
    // a file that contains the point data.
    btnOpenFile: TBitBtn;
    // @name specifies whether the imported data points will set values
    //  of data sets by interpolation.
    cbInterpolation: TCheckBox;
    { TODO : Change name to remove "cell". }
    // @name specifies whether the imported data points will set
    // values of data sets at intersected cells.
    cbIntersectedCells: TCheckBox;
    // @name specifies a "root" that is used to create names for the imported
    // objects.
    edRoot: TEdit;
    // @name labels @link(edRoot).
    lblRoot: TLabel;
    // @name is the label for @link(seRows).
    lblRows: TLabel;
    // @name is used to select a file that contains the point data.
    OpenDialogImportFile: TOpenDialog;
    // The TPageControl that contains @link(tabControls) and @link(tabData).
    pcImportPoints: TPageControl;
    // @name is the bottom panel that holds the OK (@link(btnOK))
    // and Cancel (@link(btnCancel)) buttons.
    pnlBottom: TPanel;
    // @name is the panel at the bottom of @link(tabData) that holds
    // @link(seRows) and @link(btnOpenFile).
    pnlDataTabControls: TPanel;
    // @name is the panel that holds @link(rgEvaluatedAt)
    // and @link(rgViewDirection).
    pnlRadioGroups: TPanel;
    // @name specifies how many elevation formulas will be used with each
    // imported point object.
    rgElevationCount: TRadioGroup;
    { @name indicates whether the points are to be evaluated
     at nodes or elements.
    @unOrderedList(
    @Item(OnClick = @link(rgEvaluatedAtClick).)
    )
    }
    rgEvaluatedAt: TRadioGroup;
    { @name indicates the direction from which the data will be viewed.
    @unOrderedList(
    @Item(OnClick = @link(rgViewDirectionClick).)
    )
    }
    rgViewDirection: TRadioGroup;
    // @name holds the controls used to specify properties of the imported
    // objects.
    tabControls: TTabSheet;
    // @name holds @link(dgData) and @link(pnlDataTabControls).
    tabData: TTabSheet;
    dgData: TRbwDataGrid4;
    cbImportAsSingleObject: TCheckBox;
    cbVisible: TCheckBox;
    seRows: TJvSpinEdit;
    pnlData: TPanel;
    jvclbDataSets: TJvxCheckListBox;
    pnlLabelDataSets: TPanel;
    lblBoundaryChoice: TLabel;
    comboBoundaryChoice: TComboBox;
    lblParameter: TLabel;
    comboParameter: TComboBox;
    cbLayer: TCheckBox;
    cbRowCol: TCheckBox;
    pnlFeatureOptions: TPanel;
    rgFeatureDataFormat: TRadioGroup;
    seTimePeriods: TJvSpinEdit;
    lblTimePeriods: TLabel;
    // @name makes sure that at least one of the following checkboxes is
    // checked: @link(cbIntersectedCells), and
    // @link(cbInterpolation).  If not, their fonts are changed to emphasize
    // them and @link(btnOK) is disabled.
    procedure cbIntersectedCellsClick(Sender: TObject);
    // @name updates the columns in @link(dgData)
    procedure jvclbDataSetsClickCheck(Sender: TObject);
    // @name initializes the @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name spreads the data in the keyboard into the cells of @link(dgData).
    // KeyPreview has to be @True for this to work.
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // @name calls @link(UpdateDataSets);
    procedure rgEvaluatedAtClick(Sender: TObject);
    // @name calls @link(UpdateDataSets);
    procedure rgViewDirectionClick(Sender: TObject);
    // @name is used to prevent the columns specifying coordinates
    // from being moved.
    procedure dgDataColMoving(Sender: TObject; const Origin,
      Destination: Integer; var CanMove: Boolean);
    // @name calls @link(UpdateDimensionColumns) and @link(UpdateDataSets).
    procedure rgElevationCountClick(Sender: TObject);
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name reads data from a file into @link(dgData).
    procedure btnOpenFileClick(Sender: TObject);
    procedure seRowsChange(Sender: TObject);
    procedure dgDataDistributeTextProgress(Sender: TObject; Position,
      Max: Integer);
    procedure dgDataEndUpdate(Sender: TObject);
    procedure comboBoundaryChoiceChange(Sender: TObject);
    procedure cbLayerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbRowColClick(Sender: TObject);
  private
    // @name is the column that specifies the X coordinate.
    XCol: integer;
    // @name is the column that specifies the Y coordinate.
    YCol: integer;
    // @name is the column that specifies the Z coordinate.
    ZCol: integer;
    // @name is the column that specifies the first X formula.
    X1Col: integer;
    // @name is the column that specifies the first Y formula.
    Y1Col: integer;
    // @name is the column that specifies the first Z formula.
    Z1Col: integer;
    // @name is the column that specifies the second X formula.
    X2Col: integer;
    // @name is the column that specifies the second Y formula.
    Y2Col: integer;
    // @name is the column that specifies the second Z formula.
    Z2Col: integer;
    FImportFileName: string;
    StartTime: TDateTime;
    FRequiredCols: Integer;
    FMultiValueList: TList;
    FObsCount: Integer;
    FObsRoot: string;
    FFirstColumn: Integer;
    FFirstUzf6SteadyColumn: Integer;
    // @name updates the contents of @link(jvclbDataSets).
    procedure UpdateDataSets;
    // @name updates the column captions for the columns that specify
    // dimensions in @link(dgData).
    procedure UpdateDimensionColumns;
    // @name imports the data into GoPhast.
    procedure SetData;
    { Set the captions of @link(cbIntersectedCells)
      and @link(cbInterpolation) based on @link(rgEvaluatedAt).ItemIndex.}
    procedure SetCheckBoxCaptions;
    procedure EnableOkButton;
    procedure UpdateChdColumns;
    procedure UpdateDrnColumns;
    procedure UpdateGhbColumns;
    procedure UpdateRivColumns;
    procedure UpdateWelColumns;
    procedure UpdateHobColumns;
    procedure UpdateObsUtilColumns;
    procedure UpdateUzfMf6Columns;
    procedure SetBoundaryColumnFormats;
    procedure ImportDataArrayValues(var InvalidRow: Boolean;
      RowIndex: Integer;
      PointCount: Integer; var AScreenObject: TScreenObject);
    procedure ImportModflowBoundary1RowPerTime(var InvalidRow: Boolean;
      AScreenObject: TScreenObject; RowIndex: Integer);
    procedure ImportModflowBoundary1RowPerLocation(var InvalidRow: Boolean;
      AScreenObject: TScreenObject; RowIndex: Integer);
    procedure GetData;
    procedure UpdateFootprintWellColumns;
    procedure EnableRowCol;
    function GwtColumnCount: Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses Clipbrd, GoPhastTypes, frmGoPhastUnit, RbwParser,
  frmProgressUnit, UndoItems, FastGEO, GIS_Functions,
  ValueArrayStorageUnit, PhastModelUnit, ModflowPackagesUnit,
  ModflowPackageSelectionUnit, ModflowTransientListParameterUnit,
  OrderedCollectionUnit, RealListUnit, ModflowBoundaryUnit,
  ModflowConstantHeadBoundaryUnit, ModflowGhbUnit, ModflowWellUnit,
  ModflowRivUnit, ModelMuseUtilities, ModflowDrnUnit, AbstractGridUnit,
  frameHeadObservationsUnit, IntListUnit, ModflowHobUnit,
  frameCustomCellObservationUnit, FootprintPropertiesUnit, FootprintBoundary,
  System.Character, MeshRenumberingTypes, ModflowBoundaryDisplayUnit,
  Modflow6ObsUnit, PestPropertiesUnit, DataArrayManagerUnit, DataSetNamesUnit,
  ModflowUzfMf6Unit, Mt3dmsChemSpeciesUnit, GwtStatusUnit;

{$R *.dfm}

resourcestring
  rsX = 'X';
  rsY = 'Y';
  rsZ = 'Z';
  rsX1 = 'Higher X';
  rsY1 = 'Higher Y';
  rsZ1 = 'Higher Z';
  rsX2 = 'Lower X';
  rsY2 = 'Lower Y';
  rsZ2 = 'Lower Z';
  rsColumn = 'Column';
  rsRow = 'Row';
  StrMODFLOWBoundaryCon1 = 'MODFLOW boundary conditions can not be imported a' +
  's a single object in this dialog box.';
  StrMODFLOWBoundaryCon2 = 'MODFLOW boundary conditions can not be set by int' +
  'erpolation.';
  StrObservationName = 'Observation Name';
  StrImportPoints = 'import points';
  StrYouHaveNotSelecte = 'You have not selected a data set on the "Controls"' +
  ' tab.';
  StrYouHaveChoosenTo = 'You have choosen to set values of data sets using i' +
  'nterpolation but interpolation is not used in one or more of the data set' +
  's for which you are importing values.  Do you want to import the data any' +
  'way?';
  StrNumberOfZFormulas = 'Number of Z formulas';
  StrNumberOfYFormulas = 'Number of Y formulas';
  StrNumberOfXFormulas = 'Number of X formulas';
  StrInvalidDataInRow = 'Invalid data in row %d.';
  StrReadingData = 'Reading Data';
  StrProgress = 'Progress';
  StrNone = 'none';
  StrFootprintWithdrawal = 'Withdrawal';
  StrThereWasAnErrorI = 'There was an error in the data you specified: "%s".';
  StrObjectNameOptiona = 'Object name (optional)';
  Str0s1d = '%0:s_%1:d';
  StrInvalidDataInRowEMessage = 'Invalid data in row %0:d. The error message' +
  ' was "%1:s".';

procedure TfrmImportPoints.seRowsChange(Sender: TObject);
begin
  inherited;
  dgData.RowCount := seRows.AsInteger + 1;
end;

procedure TfrmImportPoints.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

function TfrmImportPoints.GwtColumnCount: Integer;
begin
  result := 0;
  if frmGoPhast.PhastModel.Mf6GwtUsed(nil) then
  begin
    result := frmGoPhast.PhastModel.MobileComponents.Count;
  end;
end;

procedure TfrmImportPoints.UpdateChdColumns;
const
  RequiredColumns = 4;
var
  ColIndex: TChdColumns;
  ACol: Integer;
  Index: Integer;
  GwtColumns: Integer;
  FirstGwtCol: Integer;
  GwtIndex: Integer;
begin
  GwtColumns := GwtColumnCount;
  FFirstColumn := rgElevationCount.ItemIndex + 2;
  Assert(rgFeatureDataFormat.ItemIndex >= 0);

  case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
    baOneSP_PerLine:
      begin
        dgData.ColCount := FFirstColumn + RequiredColumns + GwtColumns;
      end;
    bsOneBoundaryPerLine:
      begin
        dgData.ColCount := FFirstColumn +
          (RequiredColumns + GwtColumns) * seTimePeriods.AsInteger;
      end;
    else
      Assert(False);
  end;
  SetBoundaryColumnFormats;
  case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
    baOneSP_PerLine:
      begin
        FirstGwtCol := FFirstColumn + Ord(High(TChdColumns)) + 1;
        for ColIndex := Low(TChdColumns) to High(TChdColumns) do
        begin
          ACol := FFirstColumn + Ord(ColIndex);
          Assert(ACol < dgData.ColCount);
          case ColIndex of
            ccStartTime: dgData.Cells[ACol, 0] := StrStartingTime;
            ccEndTime: dgData.Cells[ACol, 0] := StrEndingTime;
            ccStartHead: dgData.Cells[ACol, 0] := StrStartingHead;
            ccEndHead: dgData.Cells[ACol, 0] := StrEndingHead;
            else Assert(False);
          end;
        end;
        for GwtIndex := 0 to GwtColumns - 1 do
        begin
          ACol := FirstGwtCol + GwtIndex;
          dgData.Cells[ACol, 0] := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name;
        end;
      end;
    bsOneBoundaryPerLine:
      begin
        ACol := FFirstColumn;
        for Index := 1 to seTimePeriods.AsInteger do
        begin
          for ColIndex := Low(TChdColumns) to High(TChdColumns) do
          begin
            Assert(ACol < dgData.ColCount);
            case ColIndex of
              ccStartTime: dgData.Cells[ACol, 0] := StrStartingTime + IntToStr(Index);
              ccEndTime: dgData.Cells[ACol, 0] := StrEndingTime + IntToStr(Index);
              ccStartHead: dgData.Cells[ACol, 0] := StrStartingHead + IntToStr(Index);
              ccEndHead: dgData.Cells[ACol, 0] := StrEndingHead + IntToStr(Index);
              else Assert(False);
            end;
            Inc(ACol)
          end;
          for GwtIndex := 0 to GwtColumns - 1 do
          begin
            dgData.Cells[ACol, 0] := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name + IntToStr(Index);
            Inc(ACol);
          end;
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure TfrmImportPoints.UpdateFootprintWellColumns;
const
  RequiredColumns = 2;
var
  FirstColumn: Integer;
  ColIndex: TFootprintWellColumns;
  ACol: Integer;
begin
  FirstColumn := rgElevationCount.ItemIndex + 2;
  dgData.ColCount := FirstColumn + RequiredColumns;
  SetBoundaryColumnFormats;

  dgData.Columns[dgData.ColCount-1].Format := rcf4String;

  for ColIndex := Low(TFootprintWellColumns) to High(TFootprintWellColumns) do
  begin
    ACol := FirstColumn + Ord(ColIndex);
    Assert(ACol < dgData.ColCount);
    case ColIndex of
      fwcWithdrawal: dgData.Cells[ACol, 0] := StrFootprintWithdrawal;
      fwcObjectName: dgData.Cells[ACol, 0] := StrObjectNameOptiona;
    end;
  end;

end;

procedure TfrmImportPoints.UpdateHobColumns;
const
  RequiredColumns = Ord(High(THobColumns)) + 1;
var
  ColIndex: THobColumns;
  ACol: Integer;
  AColumn: TRbwColumn4;
  ColumnIndex: Integer;
  Index: Integer;
begin
  FFirstColumn := rgElevationCount.ItemIndex + 2;
  if rgFeatureDataFormat.ItemIndex  = 0 then
  begin
    dgData.ColCount := FFirstColumn + RequiredColumns;
  end
  else
  begin
    dgData.ColCount := FFirstColumn + RequiredColumns * seTimePeriods.AsInteger;
  end;
  SetBoundaryColumnFormats;


  if rgFeatureDataFormat.ItemIndex  = 0 then
  begin
    ColumnIndex := FFirstColumn+ Ord(hcName);
    AColumn := dgData.Columns[ColumnIndex];
    AColumn.Format := rcf4String;
    AColumn.MaxLength := 12;

    ColumnIndex := FFirstColumn+ Ord(hcStatFlag);
    AColumn := dgData.Columns[ColumnIndex];
    AColumn.Format := rcf4Integer;
    AColumn.Min := Ord(Low(TStatFlag));
    AColumn.Max := Ord(High(TStatFlag));
    AColumn.CheckMin := True;
    AColumn.CheckMax := True;

    for ColIndex := Low(THobColumns) to High(THobColumns) do
    begin
      ACol := FFirstColumn + Ord(ColIndex);
      Assert(ACol < dgData.ColCount);
      case ColIndex of
        hcName: dgData.Cells[ACol, 0] := StrObservationName;
        hcTime: dgData.Cells[ACol, 0] := StrTime;
        hcHead: dgData.Cells[ACol, 0] := StrObservedHead;
        hcStatistic: dgData.Cells[ACol, 0] := StrStatistic;
        hcStatFlag: dgData.Cells[ACol, 0] := StrStatFlag;
      end;
    end;
  end
  else
  begin
    ACol := FFirstColumn;
    for Index := 1 to seTimePeriods.AsInteger do
    begin
      for ColIndex := Low(THobColumns) to High(THobColumns) do
      begin
        Assert(ACol < dgData.ColCount);
        case ColIndex of
          hcName:
            begin
              dgData.Cells[ACol, 0] := StrObservationName + IntToStr(Index);
              AColumn := dgData.Columns[ACol];
              AColumn.Format := rcf4String;
              AColumn.MaxLength := 12;
            end;
          hcTime: dgData.Cells[ACol, 0] := StrTime + IntToStr(Index);
          hcHead: dgData.Cells[ACol, 0] := StrObservedHead + IntToStr(Index);
          hcStatistic: dgData.Cells[ACol, 0] := StrStatistic + IntToStr(Index);
          hcStatFlag:
            begin
              dgData.Cells[ACol, 0] := StrStatFlag + IntToStr(Index);
              AColumn := dgData.Columns[ACol];
              AColumn.Format := rcf4Integer;
              AColumn.Min := Ord(Low(TStatFlag));
              AColumn.Max := Ord(High(TStatFlag));
              AColumn.CheckMin := True;
              AColumn.CheckMax := True;
            end;
        end;
        Inc(ACol);
      end;
    end
  end;
end;

procedure TfrmImportPoints.UpdateObsUtilColumns;
const
  RequiredColumns = Ord(High(TObsUtilColumns)) + 1;
  RequiredPestColumns = Ord(High(TCalibObsColumns)) + 1;
var
  ColumnIndex: Integer;
  AColumn: TRbwColumn4;
  ACol: Integer;
  Index: Integer;
  ColIndex: TCalibObsColumns;
begin
   //TObsUtilColumns = (oucName, oucObsType);

  FFirstColumn := rgElevationCount.ItemIndex + 2;
  if rgFeatureDataFormat.ItemIndex  = 0 then
  begin
    if frmGoPhast.PhastModel.PestProperties.PestStatus = psInactive then
    begin
      dgData.ColCount := FFirstColumn + RequiredColumns;
    end
    else
    begin
      dgData.ColCount := FFirstColumn + RequiredColumns + RequiredPestColumns;
    end;
  end
  else
  begin
    if frmGoPhast.PhastModel.PestProperties.PestStatus = psInactive then
    begin
      dgData.ColCount := FFirstColumn + RequiredColumns
    end
    else
    begin
      dgData.ColCount := FFirstColumn + RequiredColumns
        + (RequiredPestColumns * seTimePeriods.AsInteger);
    end;
  end;
  for ColumnIndex := FFirstColumn to dgData.ColCount - 1 do
  begin
    AColumn := dgData.Columns[ColumnIndex];
    AColumn.Format := rcf4String;
    AColumn.AutoAdjustRowHeights := True;
    AColumn.AutoAdjustColWidths := True;
    AColumn.WordWrapCaptions := True;
  end;
  dgData.Cells[FFirstColumn,0] := 'Name';
  dgData.Cells[FFirstColumn+1,0] := 'Observation type';
  if rgFeatureDataFormat.ItemIndex  = 0 then
  begin
    if frmGoPhast.PhastModel.PestProperties.PestStatus <> psInactive then
    begin
      dgData.Cells[FFirstColumn+2+Ord(cocGroup),0] := 'Observation group';
      dgData.Cells[FFirstColumn+2+Ord(cocTime),0] := 'Time';
      dgData.Cells[FFirstColumn+2+Ord(cocValue),0] := 'Observed value';
      dgData.Cells[FFirstColumn+2+Ord(cocWeight),0] := 'Weight';

      dgData.Columns[FFirstColumn+2+Ord(cocTime)].Format := rcf4Real;
      dgData.Columns[FFirstColumn+2+Ord(cocValue)].Format := rcf4Real;
      dgData.Columns[FFirstColumn+2+Ord(cocWeight)].Format := rcf4Real;
    end;
  end
  else
  begin
    ACol := FFirstColumn+2+Ord(cocGroup);
    if frmGoPhast.PhastModel.PestProperties.PestStatus <> psInactive then
    begin
      for Index := 1 to seTimePeriods.AsInteger do
      begin
        for ColIndex := Low(TCalibObsColumns) to High(TCalibObsColumns) do
        begin
          Assert(ACol < dgData.ColCount);
          case ColIndex of
            cocGroup: dgData.Cells[ACol, 0] := 'Observation group' + IntToStr(Index);
            cocTime:
              begin
                dgData.Cells[ACol, 0] := StrTime + IntToStr(Index);
                dgData.Columns[ACol].Format := rcf4Real;
              end;
            cocValue:
            begin
              dgData.Cells[ACol, 0] := 'Observed value' + IntToStr(Index);
              dgData.Columns[ACol].Format := rcf4Real;
            end;
            cocWeight:
            begin
              dgData.Cells[ACol, 0] := 'Weight' + IntToStr(Index);
              dgData.Columns[ACol].Format := rcf4Real;
            end;
          end;
          Inc(ACol);
        end;
      end
    end;
  end;

  AColumn := dgData.Columns[FFirstColumn+1];
  AColumn.PickList.Add('Head');
  AColumn.PickList.Add('Drawdown');
  AColumn.PickList.Add('CHD Flows');
  AColumn.PickList.Add('DRN flows');
  AColumn.PickList.Add('EVT flows');
  AColumn.PickList.Add('GHB flows');
  AColumn.PickList.Add('RCH flows');
  AColumn.PickList.Add('RIV flows');
  AColumn.PickList.Add('WEL flows');
  AColumn.PickList.Add('To MVR flows');
  AColumn.ComboUsed := True;
  AColumn.LimitToList := True;

end;

procedure TfrmImportPoints.UpdateDataSets;
var
  Index: integer;
  EvalAt: TEvaluatedAt;
  DataSet: TDataArray;
  ViewDirection: TViewDirection;
  ShouldIncludeDataSet: boolean;
  DataArrayManager: TDataArrayManager;
  LayerIndex: Integer;
begin
  jvclbDataSets.Items.Clear;
  if comboBoundaryChoice.ItemIndex <= 0 then
  begin
    jvclbDataSets.Color := clRed;
  end;
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  ViewDirection := TViewDirection(rgViewDirection.ItemIndex);
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount -1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
    if (DataSet.EvaluatedAt = EvalAt) and not (dcFormula in DataSet.Lock)
      and DataSet.Visible then
    begin
      ShouldIncludeDataSet := false;
      case DataSet.Orientation of
        dsoTop:
          begin
            ShouldIncludeDataSet := ViewDirection = vdTop;
          end;
        dsoFront:
          begin
            ShouldIncludeDataSet := ViewDirection = vdFront;
          end;
        dsoSide:
          begin
            ShouldIncludeDataSet := ViewDirection = vdSide;
          end;
        dso3D:
          begin
            ShouldIncludeDataSet := rgElevationCount.ItemIndex > 0;
          end;
      else
        Assert(False);
      end;

      if rgElevationCount.ItemIndex > 0 then
      begin
        if ShouldIncludeDataSet then
        begin
          for LayerIndex := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
          begin
            if frmGoPhast.PhastModel.LayerStructure[LayerIndex].DataArrayName
              = DataSet.Name then
            begin
              ShouldIncludeDataSet := False;
              break;
            end;
          end;
        end;

        if ShouldIncludeDataSet then
        begin
          for LayerIndex := 0 to frmGoPhast.PhastModel.SutraLayerStructure.Count - 1 do
          begin
            if frmGoPhast.PhastModel.SutraLayerStructure[LayerIndex].DataArrayName
              = DataSet.Name then
            begin
              ShouldIncludeDataSet := False;
              break;
            end;
          end;
        end;
      end;

      if DataSet is TFootprintWithdrawalDataArray then
      begin
        ShouldIncludeDataSet := False;
      end;

      if ShouldIncludeDataSet then
      begin
        jvclbDataSets.Items.AddObject(DataSet.Name, DataSet);
      end;
    end;
  end;
  for Index := dgData.ColCount -1 downto rgElevationCount.ItemIndex + 2 do
  begin
    dgData.DeleteColumn(Index);
  end;
end;

procedure TfrmImportPoints.SetCheckBoxCaptions;
var
  NodeElemString: string;
begin
  NodeElemString := EvalAtToString(TEvaluatedAt(rgEvaluatedAt.ItemIndex),
       frmGoPhast.ModelSelection, True, False);
  cbIntersectedCells.Caption := rsSetValueOfIntersected + NodeElemString;
  cbInterpolation.Caption := rsSetValueOf + NodeElemString + rsByInterpolation;
end;

procedure TfrmImportPoints.EnableOkButton;
var
  RowIndex: Integer;
begin
  if cbIntersectedCells.Checked or cbInterpolation.Checked then
  begin
    btnOK.Enabled := False;
    for RowIndex := 1 to dgData.RowCount - 1 do
    begin
      case rgElevationCount.ItemIndex of
        0:
          begin
            if (Trim(dgData.Cells[0,RowIndex]) <> '')
              and (Trim(dgData.Cells[1,RowIndex]) <> '') then
            begin
              btnOK.Enabled := True;
              break;
            end;
          end;
        1:
          begin
            if (Trim(dgData.Cells[0,RowIndex]) <> '')
              and (Trim(dgData.Cells[1,RowIndex]) <> '')
              and (Trim(dgData.Cells[2,RowIndex]) <> '')
              then
            begin
              btnOK.Enabled := True;
              break;
            end;
          end;
        2:
          begin
            if (Trim(dgData.Cells[0,RowIndex]) <> '')
              and (Trim(dgData.Cells[1,RowIndex]) <> '')
              and (Trim(dgData.Cells[2,RowIndex]) <> '')
              and (Trim(dgData.Cells[3,RowIndex]) <> '')
              then
            begin
              btnOK.Enabled := True;
              break;
            end;
          end;
      end;
    end;
  end
  else
  begin
    btnOK.Enabled := False;
  end;
end;

procedure TfrmImportPoints.EnableRowCol;
begin
  cbRowCol.Enabled := (rgViewDirection.ItemIndex = 0)
    and (frmGoPhast.Grid <> nil);
  if not cbRowCol.Enabled then
  begin
    cbRowCol.Checked := False;
  end;
  cbRowColClick(nil);
end;

procedure TfrmImportPoints.rgEvaluatedAtClick(Sender: TObject);
begin
  inherited;
  SetCheckBoxCaptions;
  UpdateDataSets;
end;

procedure TfrmImportPoints.jvclbDataSetsClickCheck(Sender: TObject);
var
  Captions: TStringList;
  Index: integer;
  ACaption: string;
  Position: Integer;
  DataSet: TDataArray;
  ItemChecked: boolean;
begin
  inherited;
  ItemChecked := False;
  Captions := TStringList.Create;
  try
    Captions.AddStrings(dgData.Rows[0]);
    for Index := jvclbDataSets.Items.Count -1 downto 0 do
    begin
      ACaption := jvclbDataSets.Items[Index];
      Position := Captions.IndexOf(ACaption);
      if jvclbDataSets.Checked[Index] then
      begin
        ItemChecked := True;
        if Position < 0 then
        begin
          dgData.ColCount := dgData.ColCount + 1;
          dgData.Columns[dgData.ColCount-1].AutoAdjustColWidths := True;
          dgData.Cells[dgData.ColCount-1, 0] := jvclbDataSets.Items[Index];
          DataSet := jvclbDataSets.Items.Objects[Index] as TDataArray;
          dgData.Objects[dgData.ColCount-1, 0] := DataSet;
          if DataSet = nil then
          begin
            dgData.Columns[dgData.ColCount-1].Format := rcf4Real;
          end
          else
          begin
            case DataSet.DataType of
              rdtDouble:
                begin
                  dgData.Columns[dgData.ColCount-1].Format := rcf4Real;
                end;
              rdtInteger:
                begin
                  dgData.Columns[dgData.ColCount-1].Format := rcf4Integer;
                end;
              rdtBoolean:
                begin
                  dgData.Columns[dgData.ColCount-1].Format := rcf4Boolean;
                end;
              rdtString:
                begin
                  dgData.Columns[dgData.ColCount-1].Format := rcf4String;
                end;
            else
              Assert(False);
            end;
          end;

        end;
      end
      else
      begin
        if Position >= 0 then
        begin
          dgData.DeleteColumn(Position);
        end;
      end;
    end;
  finally
    Captions.Free;
  end;
  if ItemChecked or (comboBoundaryChoice.ItemIndex > 0) then
  begin
    jvclbDataSets.Color := clWindow;
  end
  else
  begin
    jvclbDataSets.Color := clRed;
  end;
end;

procedure TfrmImportPoints.rgViewDirectionClick(Sender: TObject);
begin
  inherited;
  case rgViewDirection.ItemIndex of
    0: rgElevationCount.Caption := StrNumberOfZFormulas;
    1: rgElevationCount.Caption := StrNumberOfYFormulas;
    2: rgElevationCount.Caption := StrNumberOfXFormulas;
    else Assert(False);
  end;

  cbLayer.Enabled := rgViewDirection.ItemIndex = 0;
  if not cbLayer.Enabled then
  begin
    cbLayer.Checked := False;
  end;
  cbLayerClick(nil);

  EnableRowCol;

  UpdateDimensionColumns;
  UpdateDataSets;
end;

procedure TfrmImportPoints.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Lines: TStringList;
  RequiredRows: integer;
begin
  inherited;
  If (Key = 86) and (Shift = [ssCtrl])
    and (ActiveControl is TRbwInplaceEdit4) then
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := Clipboard.AsText;
      RequiredRows := dgData.Row + Lines.Count;
      if RequiredRows > dgData.RowCount then
      begin
        seRows.Value := RequiredRows -1;
      end;
      dgData.DistributeText(dgData.Col, dgData.Row, Clipboard.AsText)
    finally
      Lines.Free;
    end;
  end;
end;

procedure TfrmImportPoints.FormShow(Sender: TObject);
begin
  inherited;
  if frmGoPhast.PhastModel.ModelSelection in SutraSelection then
  begin
    rgViewDirection.Buttons[Ord(vdSide)].Enabled := False;
  end;
end;

procedure TfrmImportPoints.UpdateDimensionColumns;
var
  Col: integer;
  PriorCol: integer;
  ColIndex: integer;
  procedure UpdateColumn(const ColCaption: string;
    const ShouldBePresent: boolean; out SavedCol: integer);
  begin
    if ShouldBePresent then
    begin
      Col := PriorCol + 1;
      dgData.Cells[Col, 0] := ColCaption;
      PriorCol := Col;
      SavedCol := Col;
    end
    else
    begin
      SavedCol := -1;
    end;
  end;
begin
  Col := 0;
  PriorCol := -1;

  dgData.BeginUpdate;
  try
    dgData.ColCount := rgElevationCount.ItemIndex + 2;
    for ColIndex := 0 to dgData.ColCount -1 do
    begin
      dgData.Columns[ColIndex].Format := rcf4Real;
      dgData.Columns[ColIndex].AutoAdjustColWidths := True;
    end;

    if cbRowCol.Checked then
    begin
      UpdateColumn(rsColumn, (rgViewDirection.ItemIndex = 0), XCol);
    end
    else
    begin
      UpdateColumn(rsX, (rgViewDirection.ItemIndex in [0,1])
        or (rgElevationCount.ItemIndex = 1), XCol);
    end;
    UpdateColumn(rsX1, (rgViewDirection.ItemIndex = 2)
      and (rgElevationCount.ItemIndex = 2) and not cbRowCol.Checked, X1Col);
    UpdateColumn(rsX2, (rgViewDirection.ItemIndex = 2)
      and (rgElevationCount.ItemIndex = 2) and not cbRowCol.Checked, X2Col);

    if cbRowCol.Checked then
    begin
      UpdateColumn(rsRow, (rgViewDirection.ItemIndex = 0), YCol);
    end
    else
    begin
      UpdateColumn(rsY, (rgViewDirection.ItemIndex in [0,2])
        or (rgElevationCount.ItemIndex = 1), YCol);
    end;
    UpdateColumn(rsY1, (rgViewDirection.ItemIndex = 1)
      and (rgElevationCount.ItemIndex = 2) and not cbRowCol.Checked, Y1Col);
    UpdateColumn(rsY2, (rgViewDirection.ItemIndex = 1)
      and (rgElevationCount.ItemIndex = 2) and not cbRowCol.Checked, Y2Col);

    UpdateColumn(rsZ, (rgViewDirection.ItemIndex in [1,2])
      or (rgElevationCount.ItemIndex = 1), ZCol);
    UpdateColumn(rsZ1, (rgViewDirection.ItemIndex = 0)
      and (rgElevationCount.ItemIndex = 2), Z1Col);
    UpdateColumn(rsZ2, (rgViewDirection.ItemIndex = 0)
      and (rgElevationCount.ItemIndex = 2), Z2Col);

    if cbLayer.Checked then
    begin
      dgData.Cells[ZCol, 0] := 'Layer';
      dgData.Columns[ZCol].Format := rcf4Integer;
    end;
  finally
    dgData.EndUpdate;
  end;
end;

procedure TfrmImportPoints.UpdateDrnColumns;
const
  RequiredColumns = 4;
var
  ColIndex: TDrnColumns;
  ACol: Integer;
  Index: Integer;
begin
  FFirstColumn := rgElevationCount.ItemIndex + 2;
  if rgFeatureDataFormat.ItemIndex  = 0 then
  begin
    dgData.ColCount := FFirstColumn + RequiredColumns;
  end
  else
  begin
    dgData.ColCount := FFirstColumn + RequiredColumns * seTimePeriods.AsInteger;
  end;
  SetBoundaryColumnFormats;
  if rgFeatureDataFormat.ItemIndex  = 0 then
  begin
    for ColIndex := Low(TDrnColumns) to High(TDrnColumns) do
    begin
      ACol := FFirstColumn + Ord(ColIndex);
      Assert(ACol < dgData.ColCount);
      case ColIndex of
        dcStartTime: dgData.Cells[ACol, 0] := StrStartingTime;
        dcEndTime: dgData.Cells[ACol, 0] := StrEndingTime;
        dcElevation: dgData.Cells[ACol, 0] := StrDrainElevation;
        dcConductance: dgData.Cells[ACol, 0] := StrConductance;
      end;
    end;
  end
  else
  begin
    ACol := FFirstColumn;
    for Index := 1 to seTimePeriods.AsInteger do
    begin
      for ColIndex := Low(TDrnColumns) to High(TDrnColumns) do
      begin
        Assert(ACol < dgData.ColCount);
        case ColIndex of
          dcStartTime: dgData.Cells[ACol, 0] := StrStartingTime + IntToStr(Index);
          dcEndTime: dgData.Cells[ACol, 0] := StrEndingTime + IntToStr(Index);
          dcElevation: dgData.Cells[ACol, 0] := StrDrainElevation + IntToStr(Index);
          dcConductance: dgData.Cells[ACol, 0] := StrConductance + IntToStr(Index);
        end;
        Inc(ACol)
      end;
    end;
  end;
end;

procedure TfrmImportPoints.UpdateGhbColumns;
const
  RequiredColumns = 4;
var
  ColIndex: TGhbColumns;
  ACol: Integer;
  Index: Integer;
  GwtColumns: Integer;
  FirstGwtCol: Integer;
  GwtIndex: Integer;
begin
  GwtColumns := GwtColumnCount;
  FFirstColumn := rgElevationCount.ItemIndex + 2;

  case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
    baOneSP_PerLine:
      begin
        dgData.ColCount := FFirstColumn + RequiredColumns + GwtColumns;
      end;
    bsOneBoundaryPerLine:
      begin
        dgData.ColCount := FFirstColumn +
          (RequiredColumns + GwtColumns) * seTimePeriods.AsInteger;
      end;
    else
      Assert(False);
  end;
  SetBoundaryColumnFormats;
  case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
    baOneSP_PerLine:
      begin
        FirstGwtCol := FFirstColumn + Ord(High(TGhbColumns)) + 1;
        for ColIndex := Low(TGhbColumns) to High(TGhbColumns) do
        begin
          ACol := FFirstColumn + Ord(ColIndex);
          Assert(ACol < dgData.ColCount);
          case ColIndex of
            gcStartTime: dgData.Cells[ACol, 0] := StrStartingTime;
            gcEndTime: dgData.Cells[ACol, 0] := StrEndingTime;
            gcBoundaryHead: dgData.Cells[ACol, 0] := StrBoundaryHead;
            gcConductance: dgData.Cells[ACol, 0] := StrConductance;
          end;
        end;
        for GwtIndex := 0 to GwtColumns - 1 do
        begin
          ACol := FirstGwtCol + GwtIndex;
          dgData.Cells[ACol, 0] := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name;
        end;
      end;
    bsOneBoundaryPerLine:
      begin
        ACol := FFirstColumn;
        for Index := 1 to seTimePeriods.AsInteger do
        begin
          for ColIndex := Low(TGhbColumns) to High(TGhbColumns) do
          begin
            Assert(ACol < dgData.ColCount);
            case ColIndex of
              gcStartTime: dgData.Cells[ACol, 0] := StrStartingTime + IntToStr(Index);
              gcEndTime: dgData.Cells[ACol, 0] := StrEndingTime + IntToStr(Index);
              gcBoundaryHead: dgData.Cells[ACol, 0] := StrBoundaryHead + IntToStr(Index);
              gcConductance: dgData.Cells[ACol, 0] := StrConductance + IntToStr(Index);
            end;
            Inc(ACol)
          end;
          for GwtIndex := 0 to GwtColumns - 1 do
          begin
            dgData.Cells[ACol, 0] := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name + IntToStr(Index);
            Inc(ACol);
          end;
        end
      end;
    else
      Assert(False);
  end;
end;

procedure TfrmImportPoints.UpdateRivColumns;
const
  RequiredColumns = 5;
var
  ColIndex: TRivColumns;
  ACol: Integer;
  Index: Integer;
  GwtColumns: Integer;
  FirstGwtCol: Integer;
  GwtIndex: Integer;
begin
  GwtColumns := GwtColumnCount;
  FFirstColumn := rgElevationCount.ItemIndex + 2;

  case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
    baOneSP_PerLine:
      begin
        dgData.ColCount := FFirstColumn + RequiredColumns + GwtColumns;
      end;
    bsOneBoundaryPerLine:
      begin
        dgData.ColCount := FFirstColumn +
          (RequiredColumns + GwtColumns) * seTimePeriods.AsInteger;
      end;
    else
      Assert(False);
  end;

  SetBoundaryColumnFormats;
  case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
    baOneSP_PerLine:
      begin
        FirstGwtCol := FFirstColumn + Ord(High(TRivColumns)) + 1;
        for ColIndex := Low(TRivColumns) to High(TRivColumns) do
        begin
          ACol := FFirstColumn + Ord(ColIndex);
          Assert(ACol < dgData.ColCount);
          case ColIndex of
            rcStartTime: dgData.Cells[ACol, 0] := StrStartingTime;
            rcEndTime: dgData.Cells[ACol, 0] := StrEndingTime;
            rcStage: dgData.Cells[ACol, 0] := StrRiverStage;
            rcConductance: dgData.Cells[ACol, 0] := StrConductance;
            rcBottom: dgData.Cells[ACol, 0] := StrRiverBottom;
          end;
        end;
        for GwtIndex := 0 to GwtColumns - 1 do
        begin
          ACol := FirstGwtCol + GwtIndex;
          dgData.Cells[ACol, 0] := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name;
        end;
      end;
    bsOneBoundaryPerLine:
      begin
        ACol := FFirstColumn;
        for Index := 1 to seTimePeriods.AsInteger do
        begin
          for ColIndex := Low(TRivColumns) to High(TRivColumns) do
          begin
            Assert(ACol < dgData.ColCount);
            case ColIndex of
              rcStartTime: dgData.Cells[ACol, 0] := StrStartingTime + IntToStr(Index);
              rcEndTime: dgData.Cells[ACol, 0] := StrEndingTime + IntToStr(Index);
              rcStage: dgData.Cells[ACol, 0] := StrRiverStage + IntToStr(Index);
              rcConductance: dgData.Cells[ACol, 0] := StrConductance + IntToStr(Index);
              rcBottom: dgData.Cells[ACol, 0] := StrRiverBottom + IntToStr(Index);
            end;
            Inc(ACol);
          end;
          for GwtIndex := 0 to GwtColumns - 1 do
          begin
            dgData.Cells[ACol, 0] := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name + IntToStr(Index);
            Inc(ACol);
          end;
        end
      end;
    else
      Assert(False);
  end;
end;

procedure TfrmImportPoints.UpdateUzfMf6Columns;
const
  RequiredColumns = 9;
var
  GwtColumns: Integer;
  SteadyColIndex: TUzf6SteadyColumns;
  ACol: Integer;
  ColIndex: TUzf6TransientColumns;
  GwtIndex: Integer;
  SpeciesName: string;
  Index: Integer;
  TimeString: string;
  IgnoredNames: TStringList;
  NumberOfSpecies: Integer;
  StatusChoices: TStringList;
  AColumn: TRbwColumn4;
begin
  StatusChoices := TStringList.Create;
  IgnoredNames := TStringList.Create;
  try
    StatusChoices.Add('Active');
    StatusChoices.Add('Inactive');
    StatusChoices.Add('Constant');
    frmGophast.PhastModel.GetIgnoredSpeciesNames(IgnoredNames);
    NumberOfSpecies := GwtColumnCount;
    GwtColumns := NumberOfSpecies - IgnoredNames.Count;
    FFirstUzf6SteadyColumn := rgElevationCount.ItemIndex + 2;
    FFirstColumn := FFirstUzf6SteadyColumn + Ord(High(TUzf6SteadyColumns))
      + GwtColumns + 1;

    Assert(rgFeatureDataFormat.ItemIndex >= 0);

    case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
      baOneSP_PerLine:
        begin
          dgData.ColCount := FFirstColumn + RequiredColumns + GwtColumns*4;
        end;
      bsOneBoundaryPerLine:
        begin
          dgData.ColCount := FFirstColumn +
            (RequiredColumns + GwtColumns*4) * seTimePeriods.AsInteger;
        end;
      else
        Assert(False);
    end;
    SetBoundaryColumnFormats;
    for SteadyColIndex := Low(TUzf6SteadyColumns) to High(TUzf6SteadyColumns) do
    begin
      ACol := FFirstUzf6SteadyColumn + Ord(SteadyColIndex);
      Assert(ACol < dgData.ColCount);
      case SteadyColIndex of
        u6scSurfaceDepressionDepth: dgData.Cells[ACol, 0] := 'Surface Depression Depth';
        u6scVerticalSaturatedK: dgData.Cells[ACol, 0] := 'Vertical Saturated K';
        u6scResidualWaterContent: dgData.Cells[ACol, 0] := 'Residual Water Content';
        u6scSaturatedWaterContent: dgData.Cells[ACol, 0] := 'Saturated Water Content';
        u6scInitialWaterContent: dgData.Cells[ACol, 0] := 'Initial Water Content';
        u6scBrooksCoreyEpsilon: dgData.Cells[ACol, 0] := 'Brooks-Corey Epsilon';
      end;
    end;
    ACol := FFirstUzf6SteadyColumn + Ord(High(TUzf6SteadyColumns)) + 1;
    for GwtIndex := 0 to NumberOfSpecies - 1 do
    begin
      SpeciesName := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name;
      if IgnoredNames.IndexOf(SpeciesName) >= 0 then
      begin
        Continue
      end;
      dgData.Cells[ACol, 0] := 'Initial Concentration ' + SpeciesName;
      Inc(ACol);
    end;

    case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
      baOneSP_PerLine:
        begin
          for ColIndex := Low(TUzf6TransientColumns) to High(TUzf6TransientColumns) do
          begin
            ACol := FFirstColumn + Ord(ColIndex);
            Assert(ACol < dgData.ColCount);
            case ColIndex of
              u6tcStartTime: dgData.Cells[ACol, 0] := StrStartingTime;
              u6tcEndTime: dgData.Cells[ACol, 0] := StrEndingTime;
              u6tcInfiltration: dgData.Cells[ACol, 0] := 'Infiltration';
              u6tcPotentialET: dgData.Cells[ACol, 0] := 'Potential ET';
              u6tcExtinctionDepth: dgData.Cells[ACol, 0] := 'Extinction Depth';
              u6tcExtinctionWaterContent: dgData.Cells[ACol, 0] := 'Extinction Water Content';
              u6tcAirEntryPotential: dgData.Cells[ACol, 0] := 'Air Entry Potential';
              u6tcRootPotential: dgData.Cells[ACol, 0] := 'Root Potential';
              u6tcRootActivity: dgData.Cells[ACol, 0] := 'Root Activity';
              else Assert(False);
            end;
          end;
          Inc(ACol);
          for GwtIndex := 0 to NumberOfSpecies - 1 do
          begin
            SpeciesName := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name;
            if IgnoredNames.IndexOf(SpeciesName) >= 0 then
            begin
              Continue
            end;
            dgData.Cells[ACol, 0] := 'Status ' + SpeciesName;
            AColumn := dgData.Columns[ACol];
            AColumn.Format := rcf4String;
            AColumn.PickList := StatusChoices;
            AColumn.ComboUsed := True;
            AColumn.LimitToList := True;
            Inc(ACol);
            dgData.Cells[ACol, 0] := 'Specified Conc ' + SpeciesName;
            Inc(ACol);
            dgData.Cells[ACol, 0] := 'Infiltration Conc ' + SpeciesName;
            Inc(ACol);
            dgData.Cells[ACol, 0] := 'Evap Conc ' + SpeciesName;
            Inc(ACol);
          end;
        end;
      bsOneBoundaryPerLine:
        begin
          ACol := FFirstColumn;
          for Index := 1 to seTimePeriods.AsInteger do
          begin
            TimeString := ' Period ' + IntToStr(Index);
            for ColIndex := Low(TUzf6TransientColumns) to High(TUzf6TransientColumns) do
            begin
              Assert(ACol < dgData.ColCount);
              case ColIndex of
                u6tcStartTime: dgData.Cells[ACol, 0] := StrStartingTime + TimeString;
                u6tcEndTime: dgData.Cells[ACol, 0] := StrEndingTime + TimeString;
                u6tcInfiltration: dgData.Cells[ACol, 0] := 'Infiltration' + TimeString;
                u6tcPotentialET: dgData.Cells[ACol, 0] := 'Potential ET' + TimeString;
                u6tcExtinctionDepth: dgData.Cells[ACol, 0] := 'Extinction Depth' + TimeString;
                u6tcExtinctionWaterContent: dgData.Cells[ACol, 0] := 'Extinction Water Content' + TimeString;
                u6tcAirEntryPotential: dgData.Cells[ACol, 0] := 'Air Entry Potential' + TimeString;
                u6tcRootPotential: dgData.Cells[ACol, 0] := 'Root Potential' + TimeString;
                u6tcRootActivity: dgData.Cells[ACol, 0] := 'Root Activity' + TimeString;
                else Assert(False);
              end;
              Inc(ACol)
            end;
            for GwtIndex := 0 to NumberOfSpecies - 1 do
            begin
              SpeciesName := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name;
              if IgnoredNames.IndexOf(SpeciesName) >= 0 then
              begin
                Continue
              end;
              SpeciesName := SpeciesName + TimeString;
              dgData.Cells[ACol, 0] := 'Status ' + SpeciesName;
              AColumn := dgData.Columns[ACol];
              AColumn.Format := rcf4String;
              AColumn.PickList := StatusChoices;
              AColumn.ComboUsed := True;
              AColumn.LimitToList := True;
              Inc(ACol);
              dgData.Cells[ACol, 0] := 'Specified Conc ' + SpeciesName;
              Inc(ACol);
              dgData.Cells[ACol, 0] := 'Infiltration Conc ' + SpeciesName;
              Inc(ACol);
              dgData.Cells[ACol, 0] := 'Evap Conc ' + SpeciesName;
              Inc(ACol);
            end;
          end;
        end;
      else
        Assert(False);
    end;
  finally
    IgnoredNames.Free;
    StatusChoices.Free;
  end;
end;

procedure TfrmImportPoints.UpdateWelColumns;
const
  RequiredColumns = 3;
var
  ColIndex: TWelColumns;
  ACol: Integer;
  Index: Integer;
  GwtColumns: Integer;
  FirstGwtCol: Integer;
  GwtIndex: Integer;
begin
  GwtColumns := GwtColumnCount;
  FFirstColumn := rgElevationCount.ItemIndex + 2;
  case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
    baOneSP_PerLine:
      begin
        dgData.ColCount := FFirstColumn + RequiredColumns + GwtColumns;
      end;
    bsOneBoundaryPerLine:
      begin
        dgData.ColCount := FFirstColumn +
          (RequiredColumns + GwtColumns) * seTimePeriods.AsInteger;
      end;
    else
      Assert(False);
  end;
  SetBoundaryColumnFormats;
  case TBoundaryArrangment(rgFeatureDataFormat.ItemIndex) of
    baOneSP_PerLine:
      begin
        FirstGwtCol := FFirstColumn + Ord(High(TWelColumns)) + 1;
        for ColIndex := Low(TWelColumns) to High(TWelColumns) do
        begin
          ACol := FFirstColumn + Ord(ColIndex);
          Assert(ACol < dgData.ColCount);
          case ColIndex of
            wcStartTime: dgData.Cells[ACol, 0] := StrStartingTime;
            wcEndTime: dgData.Cells[ACol, 0] := StrEndingTime;
            wcPumpingRate: dgData.Cells[ACol, 0] := StrPumpingRate;
          end;
        end;
        for GwtIndex := 0 to GwtColumns - 1 do
        begin
          ACol := FirstGwtCol + GwtIndex;
          dgData.Cells[ACol, 0] := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name;
        end;
      end;
    bsOneBoundaryPerLine:
      begin
        ACol := FFirstColumn;
        for Index := 1 to seTimePeriods.AsInteger do
        begin
          for ColIndex := Low(TWelColumns) to High(TWelColumns) do
          begin
            Assert(ACol < dgData.ColCount);
            case ColIndex of
              wcStartTime: dgData.Cells[ACol, 0] := StrStartingTime + IntToStr(Index);
              wcEndTime: dgData.Cells[ACol, 0] := StrEndingTime + IntToStr(Index);
              wcPumpingRate: dgData.Cells[ACol, 0] := StrPumpingRate + IntToStr(Index);
            end;
            Inc(ACol);
          end;
          for GwtIndex := 0 to GwtColumns - 1 do
          begin
            dgData.Cells[ACol, 0] := frmGoPhast.PhastModel.MobileComponents[GwtIndex].Name + IntToStr(Index);
            Inc(ACol);
          end;
        end
      end;
    else
      Assert(False);
  end;
end;

procedure TfrmImportPoints.SetBoundaryColumnFormats;
var
  ColumnIndex: Integer;
  FirstColumn: Integer;
  AColumn: TRbwColumn4;
begin
  FirstColumn := rgElevationCount.ItemIndex + 2;
  for ColumnIndex := FirstColumn to dgData.ColCount - 1 do
  begin
    AColumn := dgData.Columns[ColumnIndex];
    AColumn.Format := rcf4Real;
    AColumn.AutoAdjustRowHeights := True;
    AColumn.AutoAdjustColWidths := True;
    AColumn.WordWrapCaptions := True;
  end;
end;

procedure TfrmImportPoints.ImportDataArrayValues(var InvalidRow: Boolean;
  RowIndex: Integer;
  PointCount: Integer; var AScreenObject: TScreenObject);
var
  Position: Integer;
  AFormula: string;
  ARealValue: Double;
  DataSetValues: TValueArrayStorage;
  AnIntValue: Integer;
  ColIndex: Integer;
  DataArray: TDataArray;
begin
  InvalidRow := False;
  try
    DataSetValues := nil;
    for ColIndex := FRequiredCols to dgData.ColCount - 1 do
    begin
      DataArray := dgData.Objects[ColIndex, 0] as TDataArray;
      Assert(DataArray <> nil);
      Position := AScreenObject.AddDataSet(DataArray);
      Assert(Position >= 0);
      if cbImportAsSingleObject.Checked then
      begin
        DataSetValues := FMultiValueList[ColIndex - FRequiredCols];
      end;
      case DataArray.DataType of
        rdtDouble:
          begin
            ARealValue := StrToFloat(Trim(dgData.Cells[ColIndex, RowIndex]));
            if cbImportAsSingleObject.Checked then
            begin
              DataSetValues.RealValues[PointCount - 1] := ARealValue;
            end
            else
            begin
              // don't assign dgData.Cells[ColIndex, RowIndex] directly
              // because the decimal separator might not be a period.
              AScreenObject.DataSetFormulas[Position] := FloatToStr(ARealValue);
            end;
          end;
        rdtInteger:
          begin
            AnIntValue := StrToInt(Trim(dgData.Cells[ColIndex, RowIndex]));
            if cbImportAsSingleObject.Checked then
            begin
              DataSetValues.IntValues[PointCount - 1] := AnIntValue;
            end
            else
            begin
              AScreenObject.DataSetFormulas[Position] := Trim(dgData.Cells[ColIndex, RowIndex]);
            end;
          end;
        rdtBoolean:
          begin
            if cbImportAsSingleObject.Checked then
            begin
              if dgData.Checked[ColIndex, RowIndex] then
              begin
                DataSetValues.BooleanValues[PointCount - 1] := True;
              end
              else
              begin
                DataSetValues.BooleanValues[PointCount - 1] := False;
              end;
            end
            else
            begin
              if dgData.Checked[ColIndex, RowIndex] then
              begin
                AScreenObject.DataSetFormulas[Position] := 'True';
              end
              else
              begin
                AScreenObject.DataSetFormulas[Position] := 'False';
              end;
            end;
          end;
        rdtString:
          begin
            AFormula := Trim(dgData.Cells[ColIndex, RowIndex]);
            if cbImportAsSingleObject.Checked then
            begin
              if Length(AFormula) > 0 then
              begin
                if AFormula[1] = '"' then
                begin
                  AFormula := Copy(AFormula, 2, MAXINT);
                end;
              end;
              if Length(AFormula) > 0 then
              begin
                if AFormula[Length(AFormula)] = '"' then
                begin
                  AFormula := Copy(AFormula, 1, Length(AFormula) - 1);
                end;
              end;
              DataSetValues.StringValues[PointCount - 1] := AFormula;
            end
            else
            begin
              if Length(AFormula) > 0 then
              begin
                if AFormula[1] <> '"' then
                begin
                  AFormula := '"' + AFormula;
                end;
                if AFormula[Length(AFormula)] <> '"' then
                begin
                  AFormula := AFormula + '"';
                end;
              end
              else
              begin
                AFormula := '""';
              end;
              AScreenObject.DataSetFormulas[Position] := AFormula;
            end;
          end;
      else
        Assert(False);
      end;
    end;
  except
    on EConvertError do
    begin
      FreeAndNil(AScreenObject);
      if cbImportAsSingleObject.Checked then
      begin
        Beep;
        MessageDlg(Format(StrInvalidDataInRow, [RowIndex+1]), mtError, [mbOK], 0);
        InvalidRow := True;
      end
      else
      begin
        InvalidRow := True;
      end;
      Exit;
    end;
    on E: ERbwParserError do
    begin
      MessageDlg(Format(StrInvalidDataInRowEMessage, [RowIndex+1, E.Message]),
        mtError, [mbOK], 0);
      InvalidRow := True;
      Exit;
    end;
  end;
end;

procedure TfrmImportPoints.ImportModflowBoundary1RowPerLocation(
  var InvalidRow: Boolean; AScreenObject: TScreenObject; RowIndex: Integer);
var
  Package: TModflowPackageSelection;
  Packages: TModflowPackages;
  ABoundary: TModflowParamBoundary;
  HobBoundary: THobBoundary;
  SpecifiedHeadArray: TDataArray;
  FormulaPosition: Integer;
  Mf6Obs: TModflow6Obs;
  AParam: TModflowTransientListParameter;
  TimeIndex: Integer;
  ACol: Integer;
  STime: Extended;
  ETime: Extended;
  Shead: Extended;
  EHead: Extended;
  ChdItem: TChdItem;
  Bhead: Extended;
  Conductance: Extended;
  GhbItem: TGhbItem;
  WelItem: TWellItem;
  Pump: Extended;
  Stage: Extended;
  Bottom: Extended;
  RivItem: TRivItem;
  Elevation: Extended;
  DrnItem: TDrnItem;
  HobItem: THobItem;
  ObsTime: Extended;
  Head: Extended;
  Statistic: Extended;
  StatFlag: Integer;
  LocalObsRoot: string;
  NewObsName: string;
  NewItemName: string;
  ObsType: Integer;
  ObGeneral: TObGeneral;
  Group: string;
  ObsValue: Extended;
  ObsWeight: Extended;
  CalibObs: TMf6CalibrationObs;
  AnItem: TModflowParamItem;
  GwtColumns: Integer;
  GwtIndex: Integer;
  DummyValue: Extended;
  UzfMf6Boundary: TUzfMf6Boundary;
  IgnoredNames: TStringList;
  NumberOfSpecies: Integer;
  StartTimeColumn: Integer;
  UzfTransientIndex: TUzf6TransientColumns;
  UzfTransientValues: array[Low(TUzf6TransientColumns) .. High(TUzf6TransientColumns)] of double;
  TransientOK: Boolean;
  Uzf6Item: TUzfMf6Item;
  SpeciesIndex: Integer;
  MobileComponents: TMobileChemSpeciesCollection;
  SpeciesName: string;
  AValue: Double;
  FSteadyOK: Boolean;
  UzfSteadyIndex: TUzf6SteadyColumns;
  UzfSteadyValues: array[Low(TUzf6SteadyColumns) .. High(TUzf6SteadyColumns)] of double;
  AFormula: string;
  SurfaceDepressionDepthDataArray: TDataArray;
  VerticalSaturatedKDataArray: TDataArray;
  ReisidualWaterContentDataArray: TDataArray;
  SaturatedWaterContentDataArray: TDataArray;
  InitialUnsaturatedWaterContentDataArray: TDataArray;
  BrooksCoreyEpsilonDataArray: TDataArray;
begin
  if frmGoPhast.ModelSelection in ModflowSelection then
  begin
    MobileComponents  := frmGoPhast.PhastModel.MobileComponents;
    IgnoredNames := TStringList.Create;
    try
      GwtColumns := GwtColumnCount;
      Package := comboBoundaryChoice.Items.Objects[comboBoundaryChoice.ItemIndex]
        as TModflowPackageSelection;
      Packages := frmGoPhast.PhastModel.ModflowPackages;
      ABoundary := nil;
      HobBoundary := nil;
      Mf6Obs := nil;
      ObGeneral := ogUndefined;
      UzfMf6Boundary := nil;
      if Package = Packages.ChdBoundary then
      begin
        AScreenObject.CreateChdBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowChdBoundary;
      end
      else if Package = Packages.GhbBoundary then
      begin
        AScreenObject.CreateGhbBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowGhbBoundary;
      end
      else if Package = Packages.WelPackage then
      begin
        AScreenObject.CreateWelBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowWellBoundary;
      end
      else if Package = Packages.RivPackage then
      begin
        AScreenObject.CreateRivBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowRivBoundary;
      end
      else if Package = Packages.DrnPackage then
      begin
        AScreenObject.CreateDrnBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowDrnBoundary;
      end
      else if Package = Packages.HobPackage then
      begin
        AScreenObject.CreateHeadObservations;
        HobBoundary := AScreenObject.ModflowBoundaries.ModflowHeadObservations;
        LocalObsRoot := Trim(dgData.Cells[FRequiredCols + Ord(hcName), RowIndex]);
        if LocalObsRoot = '' then
        begin
          NewObsName := FObsRoot + IntToStr(FObsCount) + '_';
          Inc(FObsCount);
          NewItemName := NewObsName {+ IntToStr(ObsCount+1)};
        end
        else
        begin
          NewItemName := LocalObsRoot {+ IntToStr(ObsCount+1)};
          NewObsName := LocalObsRoot;
        end;
        HobBoundary.ObservationName := NewObsName;
        if CharInSet(NewObsName[1], ['0'..'9']) then
        begin
          NewObsName := 'Obs_' + NewObsName;
        end;
        AScreenObject.Name := TScreenObject.ValidName(NewObsName);
      end
      else if Package = Packages.Mf6ObservationUtility then
      begin
        AScreenObject.CreateMf6Obs;
        Mf6Obs := AScreenObject.Modflow6Obs;
        Mf6Obs.Name :=  dgData.Cells[FRequiredCols,RowIndex];
        ObsType := dgData.ItemIndex[FRequiredCols +1, RowIndex];
        if (ObsType >= 0) and (ObsType < Ord(ogUndefined)) then
        begin
          ObGeneral := TObGeneral(ObsType);
          Mf6Obs.General := Mf6Obs.General + [ObGeneral];
        end;
      end
      else if Package = Packages.UzfMf6Package then
      begin
        AScreenObject.CreateUzfMf6Boundary;
        UzfMf6Boundary := AScreenObject.ModflowUzfMf6Boundary;
        frmGophast.PhastModel.GetIgnoredSpeciesNames(IgnoredNames);
        NumberOfSpecies := GwtColumns;
        GwtColumns := GwtColumns - IgnoredNames.Count;
        StartTimeColumn := FRequiredCols + Ord(u6scBrooksCoreyEpsilon)
          + NumberOfSpecies + 1;
        ACol := StartTimeColumn;

      end
      else
      begin
        Assert(False);
      end;

      if (ABoundary <> nil) and (ABoundary is TSpecificModflowBoundary) then
      begin
        TSpecificModflowBoundary(ABoundary).FormulaInterpretation := fiDirect;
      end;
      AParam := comboParameter.Items.Objects[comboParameter.ItemIndex]
        as TModflowTransientListParameter;
      ACol := FFirstColumn;
      for TimeIndex := 1 to seTimePeriods.AsInteger do
      begin
        if Package = Packages.ChdBoundary then
        begin
          if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], STime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(ccEndTime), RowIndex], ETime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(ccStartHead), RowIndex], Shead)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(ccEndHead), RowIndex], EHead)
            then
          begin
            if AParam <> nil then
            begin
              AnItem := ABoundary.Parameters.GetParamByName(AParam.ParameterName);
              if AnItem = nil then
              begin
                AnItem := ABoundary.Parameters.Add;
                AnItem.Param.Param := AParam;
              end;
              ChdItem := AnItem.Param.Add as TChdItem;
            end
            else
            begin
              ChdItem := ABoundary.Values.Add as TChdItem;
            end;

            ChdItem.StartTime := STime;
            ChdItem.EndTime := ETime;
            ChdItem.StartHead := dgData.Cells[ACol+Ord(ccStartHead), RowIndex];
            ChdItem.EndHead := dgData.Cells[ACol+Ord(ccEndHead), RowIndex];
          end
          else
          begin
            ChdItem := nil;
          end;
          Inc(ACol, 4);
          for GwtIndex := 0 to GwtColumns - 1 do
          begin
            if ChdItem <> nil then
            begin
              if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], DummyValue) then
              begin
                ChdItem.GwtConcentrations[GwtIndex].Value
                  := dgData.Cells[ACol, RowIndex];
              end;
            end;
            Inc(ACol);
          end;
        end
        else if Package = Packages.GhbBoundary then
        begin
          if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], STime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(gcEndTime), RowIndex], ETime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(gcBoundaryHead), RowIndex], Bhead)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(gcConductance), RowIndex], Conductance)
            then
          begin
            if AParam <> nil then
            begin
              AnItem := ABoundary.Parameters.GetParamByName(AParam.ParameterName);
              if AnItem = nil then
              begin
                AnItem := ABoundary.Parameters.Add;
                AnItem.Param.Param := AParam;
              end;
              GhbItem := AnItem.Param.Add as TGhbItem;
            end
            else
            begin
              GhbItem := ABoundary.Values.Add as TGhbItem;
            end;
            GhbItem.StartTime := STime;
            GhbItem.EndTime := ETime;
            GhbItem.BoundaryHead := dgData.Cells[ACol+Ord(gcBoundaryHead), RowIndex];
            GhbItem.Conductance := dgData.Cells[ACol+Ord(gcConductance), RowIndex];
          end
          else
          begin
            GhbItem := nil;
          end;
          Inc(ACol, 4);
          for GwtIndex := 0 to GwtColumns - 1 do
          begin
            if GhbItem <> nil then
            begin
              if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], DummyValue) then
              begin
                GhbItem.GwtConcentrations[GwtIndex].Value
                  := dgData.Cells[ACol, RowIndex];
              end;
            end;
            Inc(ACol);
          end;
        end
        else if Package = Packages.WelPackage then
        begin
          if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], STime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(wcEndTime), RowIndex], ETime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(wcPumpingRate), RowIndex], Pump)
            then
          begin
            if AParam <> nil then
            begin
              AnItem := ABoundary.Parameters.GetParamByName(AParam.ParameterName);
              if AnItem = nil then
              begin
                AnItem := ABoundary.Parameters.Add;
                AnItem.Param.Param := AParam;
              end;
              WelItem := AnItem.Param.Add as TWellItem;
            end
            else
            begin
              WelItem := ABoundary.Values.Add as TWellItem;
            end;
            WelItem.StartTime := STime;
            WelItem.EndTime := ETime;
            WelItem.PumpingRate := dgData.Cells[ACol+Ord(wcPumpingRate), RowIndex];
          end
          else
          begin
            WelItem := nil;
          end;
          Inc(ACol, 3);
          for GwtIndex := 0 to GwtColumns - 1 do
          begin
            if WelItem <> nil then
            begin
              if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], DummyValue) then
              begin
                WelItem.GwtConcentrations[GwtIndex].Value
                  := dgData.Cells[ACol, RowIndex];
              end;
            end;
            Inc(ACol);
          end;
        end
        else if Package = Packages.RivPackage then
        begin
          if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], STime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(rcEndTime), RowIndex], ETime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(rcStage), RowIndex], Stage)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(rcConductance), RowIndex], Conductance)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(rcBottom), RowIndex], Bottom)
            then
          begin
            if AParam <> nil then
            begin
              AnItem := ABoundary.Parameters.GetParamByName(AParam.ParameterName);
              if AnItem = nil then
              begin
                AnItem := ABoundary.Parameters.Add;
                AnItem.Param.Param := AParam;
              end;
              RivItem := AnItem.Param.Add as TRivItem;
            end
            else
            begin
              RivItem := ABoundary.Values.Add as TRivItem;
            end;
            RivItem.StartTime := STime;
            RivItem.EndTime := ETime;
            RivItem.RiverBottom := dgData.Cells[ACol+Ord(rcBottom), RowIndex];
            RivItem.Conductance := dgData.Cells[ACol+Ord(rcConductance), RowIndex];
            RivItem.RiverStage := dgData.Cells[ACol+Ord(rcStage), RowIndex];
          end
          else
          begin
            RivItem := nil;
          end;
          Inc(ACol, 5);
          for GwtIndex := 0 to GwtColumns - 1 do
          begin
            if RivItem <> nil then
            begin
              if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], DummyValue) then
              begin
                RivItem.GwtConcentrations[GwtIndex].Value
                  := dgData.Cells[ACol, RowIndex];
              end;
            end;
            Inc(ACol);
          end;
        end
        else if Package = Packages.DrnPackage then
        begin
          if TryFortranStrToFloat(dgData.Cells[ACol, RowIndex], STime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(dcEndTime), RowIndex], ETime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(dcElevation), RowIndex], Elevation)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(dcConductance), RowIndex], Conductance)
            then
          begin
            if AParam <> nil then
            begin
              AnItem := ABoundary.Parameters.GetParamByName(AParam.ParameterName);
              if AnItem = nil then
              begin
                AnItem := ABoundary.Parameters.Add;
                AnItem.Param.Param := AParam;
              end;
              DrnItem := AnItem.Param.Add as TDrnItem;
            end
            else
            begin
              DrnItem := ABoundary.Values.Add as TDrnItem;
            end;
            DrnItem.StartTime := STime;
            DrnItem.EndTime := ETime;
            DrnItem.Elevation := dgData.Cells[ACol+Ord(dcElevation), RowIndex];
            DrnItem.Conductance := dgData.Cells[ACol+Ord(dcConductance), RowIndex];
          end;
          Inc(ACol, 4);
        end
        else if Package = Packages.HobPackage then
        begin
          if (dgData.Cells[ACol, RowIndex] <> '')
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(hcTime), RowIndex], ObsTime)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(hcHead), RowIndex], Head)
            and TryFortranStrToFloat(dgData.Cells[ACol+Ord(hcStatistic), RowIndex], Statistic)
            and TryStrToInt(dgData.Cells[ACol+Ord(hcStatistic), RowIndex], StatFlag)
            then
          begin
            HobItem := HobBoundary.Values.Add as THobItem;
            HobItem.Time := ObsTime;
            HobItem.Head := Head;
            HobItem.Statistic := Statistic;
            if StatFlag < Ord(Low(TStatFlag)) then
            begin
              StatFlag := Ord(Low(TStatFlag))
            end;
            if StatFlag > Ord(High(TStatFlag)) then
            begin
              StatFlag := Ord(High(TStatFlag))
            end;
            HobItem.StatFlag := TStatFlag(StatFlag);
          end;
          Inc(ACol, 4);
        end
        else if Package = Packages.Mf6ObservationUtility then
        begin
          if frmGoPhast.PhastModel.PestProperties.PestStatus <> psInactive then
          begin
            Group := dgData.Cells[ACol+2,RowIndex];
            if TryFortranStrToFloat(dgData.Cells[ACol+2 + Ord(cocTime),RowIndex], ObsTime)
              and TryFortranStrToFloat(dgData.Cells[ACol+2 + Ord(cocValue),RowIndex], ObsValue)
              and TryFortranStrToFloat(dgData.Cells[ACol+2 + Ord(cocWeight),RowIndex], ObsWeight)
              then
            begin
              CalibObs := Mf6Obs.CalibrationObservations.Add;
              CalibObs.Name := Mf6Obs.Name + '_'
                + IntToStr(Mf6Obs.CalibrationObservations.Count);
              CalibObs.Time := ObsTime;
              CalibObs.ObservedValue := ObsValue;
              CalibObs.Weight := ObsWeight;
              CalibObs.ObSeries := osGeneral;
              CalibObs.ObGeneral := ObGeneral;
              CalibObs.ObservationGroup := Group;
            end;
          end;
          Inc(ACol, 4);
        end
        else if Package = Packages.UzfMf6Package then
        begin
          Assert(UzfMf6Boundary <> nil);
          TransientOK := True;
          for UzfTransientIndex := Low(TUzf6TransientColumns) to High(TUzf6TransientColumns) do
          begin
            if not TryStrToFloat(dgData.Cells[ACol, RowIndex], UzfTransientValues[UzfTransientIndex]) then
            begin
              TransientOK := False;
              if UzfMf6Boundary.Values.Count = 0 then
              begin
                InvalidRow := True;
              end;
              Exit;
            end;
            Inc(ACol);
          end;
          if TransientOK then
          begin
            Uzf6Item := UzfMf6Boundary.Values.Add as TUzfMf6Item;
            Uzf6Item.StartTime := UzfTransientValues[u6tcStartTime];
            Uzf6Item.EndTime := UzfTransientValues[u6tcEndTime];
            Uzf6Item.Infiltration := FortranFloatToStr(UzfTransientValues[u6tcInfiltration]);
            Uzf6Item.PotentialET := FortranFloatToStr(UzfTransientValues[u6tcPotentialET]);
            Uzf6Item.ExtinctionDepth := FortranFloatToStr(UzfTransientValues[u6tcExtinctionDepth]);
            Uzf6Item.ExtinctionWaterContent := FortranFloatToStr(UzfTransientValues[u6tcExtinctionWaterContent]);
            Uzf6Item.AirEntryPotential := FortranFloatToStr(UzfTransientValues[u6tcAirEntryPotential]);
            Uzf6Item.RootPotential := FortranFloatToStr(UzfTransientValues[u6tcRootPotential]);
            Uzf6Item.RootActivity := FortranFloatToStr(UzfTransientValues[u6tcRootActivity]);
            if GwtColumns > 0 then
            begin
              Uzf6Item.GwtStatus.Count := NumberOfSpecies;
              Uzf6Item.SpecifiedConcentrations.Count := NumberOfSpecies;
              Uzf6Item.InfiltrationConcentrations.Count := NumberOfSpecies;
              Uzf6Item.EvapConcentrations.Count := NumberOfSpecies;
  //            ACol := StartTimeColumn + Ord(High(TUzf6TransientColumns)) + 1;
              for SpeciesIndex := 0 to NumberOfSpecies - 1 do
              begin
                SpeciesName := MobileComponents[SpeciesIndex].Name;
                if IgnoredNames.IndexOf(SpeciesName) >= 0 then
                begin
                  Continue;
                end;
                if dgData.ItemIndex[ACol, RowIndex] >= 0 then
                begin
                  Uzf6Item.GwtStatus[SpeciesIndex].GwtBoundaryStatus :=
                    TGwtBoundaryStatus(dgData.ItemIndex[ACol, RowIndex]);
                end;
                Inc(ACol);
                if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue)  then
                begin
                  Uzf6Item.SpecifiedConcentrations[SpeciesIndex].Value
                    := FortranFloatToStr(AValue);
                end;
                Inc(ACol);
                if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue)  then
                begin
                  Uzf6Item.InfiltrationConcentrations[SpeciesIndex].Value
                    := FortranFloatToStr(AValue);
                end;
                Inc(ACol);
                if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue)  then
                begin
                  Uzf6Item.EvapConcentrations[SpeciesIndex].Value
                    := FortranFloatToStr(AValue);
                end;
                Inc(ACol);
              end;
            end;

          end;
        end
        else
        begin
          Assert(False);
        end;
      end;
      if Package = Packages.ChdBoundary then
      begin
        ABoundary := AScreenObject.ModflowBoundaries.ModflowChdBoundary;

        if (ABoundary <> nil) and (ABoundary.Values <> nil) then
        begin
          SpecifiedHeadArray := frmGoPhast.PhastModel.DataArrayManager.
            GetDataSetByName(rsModflowSpecifiedHead);
          if SpecifiedHeadArray <> nil then
          begin
            FormulaPosition := AScreenObject.AddDataSet(SpecifiedHeadArray);
            AScreenObject.DataSetFormulas[FormulaPosition] := 'True';
          end;
        end;
      end
      else if Package = Packages.UzfMf6Package then
      begin
        FSteadyOK := True;
        for UzfSteadyIndex := Low(TUzf6SteadyColumns) to High(TUzf6SteadyColumns) do
        begin
          if not TryStrToFloat(dgData.Cells[FRequiredCols + Ord(UzfSteadyIndex), RowIndex],
            UzfSteadyValues[UzfSteadyIndex]) then
          begin
            FSteadyOK := False;
          end;
        end;
        if FSteadyOK then
        begin
          AFormula := FortranFloatToStr(UzfSteadyValues[u6scSurfaceDepressionDepth]);
  //            FormulaObject = UzfMf6Boundary.SurfaceDepressionDepth;
          SurfaceDepressionDepthDataArray :=
            frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
          if SurfaceDepressionDepthDataArray <> nil then
          begin
            FormulaPosition := AScreenObject.AddDataSet(SurfaceDepressionDepthDataArray);
            AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
          end;
          UzfMf6Boundary.SurfaceDepressionDepth :=  AFormula;

          AFormula := FortranFloatToStr(UzfSteadyValues[u6scVerticalSaturatedK]);
          VerticalSaturatedKDataArray :=
            frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6VerticalSaturatedK);
          if VerticalSaturatedKDataArray <> nil then
          begin
            FormulaPosition := AScreenObject.AddDataSet(VerticalSaturatedKDataArray);
            AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
          end;
          UzfMf6Boundary.VerticalSaturatedK := AFormula;

          AFormula := FortranFloatToStr(UzfSteadyValues[u6scResidualWaterContent]);
          UzfMf6Boundary.ResidualWaterContent := AFormula;
          ReisidualWaterContentDataArray :=
            frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6ReisidualWaterContent);
          if ReisidualWaterContentDataArray <> nil then
          begin
            FormulaPosition := AScreenObject.AddDataSet(ReisidualWaterContentDataArray);
            AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
          end;

          AFormula := FortranFloatToStr(UzfSteadyValues[u6scSaturatedWaterContent]);
          UzfMf6Boundary.SaturatedWaterContent := AFormula;
          SaturatedWaterContentDataArray :=
            frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6SaturatedWaterContent);
          if SaturatedWaterContentDataArray <> nil then
          begin
            FormulaPosition := AScreenObject.AddDataSet(SaturatedWaterContentDataArray);
            AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
          end;

          AFormula := FortranFloatToStr(UzfSteadyValues[u6scInitialWaterContent]);
          UzfMf6Boundary.InitialWaterContent := AFormula;
          InitialUnsaturatedWaterContentDataArray :=
            frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6InitialUnsaturatedWaterContent);
          if InitialUnsaturatedWaterContentDataArray <> nil then
          begin
            FormulaPosition := AScreenObject.AddDataSet(InitialUnsaturatedWaterContentDataArray);
            AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
          end;

          AFormula := FortranFloatToStr(UzfSteadyValues[u6scBrooksCoreyEpsilon]);
          UzfMf6Boundary.BrooksCoreyEpsilon := AFormula;
          BrooksCoreyEpsilonDataArray :=
            frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6BrooksCoreyEpsilon);
          if BrooksCoreyEpsilonDataArray <> nil then
          begin
            FormulaPosition := AScreenObject.AddDataSet(BrooksCoreyEpsilonDataArray);
            AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
          end;
        end;
        ACol := FRequiredCols + Ord(u6scBrooksCoreyEpsilon) + 1;
        if GwtColumns > 0 then
        begin
          UzfMf6Boundary.StartingConcentrations.Count := NumberOfSpecies;
          for SpeciesIndex := 0 to NumberOfSpecies - 1 do
          begin
            SpeciesName := MobileComponents[SpeciesIndex].Name;
            if IgnoredNames.IndexOf(SpeciesName) >= 0 then
            begin
              Continue;
            end;
            if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue) then
            begin
              UzfMf6Boundary.StartingConcentrations[SpeciesIndex].Value :=
                FortranFloatToStr(AValue);
            end;
            Inc(ACol);
          end;
        end;

      end;
    finally
      IgnoredNames.Free;
    end;
  end;
end;

procedure TfrmImportPoints.ImportModflowBoundary1RowPerTime(var InvalidRow: Boolean;
  AScreenObject: TScreenObject; RowIndex: Integer);
var
  Packages: TModflowPackages;
  RivItem: TRivItem;
  DrnItem: TDrnItem;
  Values: TRealList;
  AnItem: TModflowParamItem;
  Package: TModflowPackageSelection;
  ChdItem: TChdItem;
  ABoundary: TModflowParamBoundary;
  BoundaryItem: TCustomModflowBoundaryItem;
  GhbItem: TGhbItem;
  WelItem: TWellItem;
  AParam: TModflowTransientListParameter;
  ColIndex: Integer;
  IntValues: TIntegerList;
  HobItem: THobItem;
  IntValue: Integer;
  HobBoundary: THobBoundary;
  NewObsName: string;
  ObsCount: Integer;
  NewItemName: string;
  LocalObsRoot: string;
  AFootPrint: TFootprintWell;
  AValue: double;
  DataArray: TDataArray;
  DataArrayPosition: Integer;
  Mf6Obs: TModflow6Obs;
  ObsType: Integer;
  ObGeneral: TObGeneral;
  SpecifiedHeadArray: TDataArray;
  FormulaPosition: Integer;
  CalibObs: TMf6CalibrationObs;
  GwtColumns: Integer;
  GwtIndex: Integer;
  GwtStart: Integer;
  UzfMf6Boundary: TUzfMf6Boundary;
  UzfSteadyValues: array[Low(TUzf6SteadyColumns) .. High(TUzf6SteadyColumns)] of double;
  UzfSteadyIndex: TUzf6SteadyColumns;
  FSteadyOK: Boolean;
  StartTimeColumn: Integer;
  Uzf6Item: TUzfMf6Item;
  UzfTransientIndex: TUzf6TransientColumns;
  ACol: Integer;
  UzfTransientValues: array[Low(TUzf6TransientColumns) .. High(TUzf6TransientColumns)] of double;
  IgnoredNames: TStringList;
  NumberOfSpecies: Integer;
  SpeciesIndex: Integer;
  MobileComponents: TMobileChemSpeciesCollection;
  SpeciesName: string;
  SurfaceDepressionDepthDataArray: TDataArray;
  VerticalSaturatedKDataArray: TDataArray;
  ReisidualWaterContentDataArray: TDataArray;
  SaturatedWaterContentDataArray: TDataArray;
  InitialUnsaturatedWaterContentDataArray: TDataArray;
  BrooksCoreyEpsilonDataArray: TDataArray;
  AFormula: string;
begin
  GwtColumns := GwtColumnCount;
  InvalidRow := False;
  Values := TRealList.Create;
  IntValues := TIntegerList.Create;
  try
    if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      Package := comboBoundaryChoice.Items.Objects[comboBoundaryChoice.ItemIndex]
        as TModflowPackageSelection;
      Packages := frmGoPhast.PhastModel.ModflowPackages;
      for ColIndex := FRequiredCols to dgData.ColCount - 1 do
      begin
        if (Package = Packages.HobPackage) and (ColIndex = dgData.ColCount - 1) then
        begin
          try
            IntValues.Add(StrToInt(Trim(dgData.Cells[ColIndex, RowIndex])));
          except
            on E: EConvertError do
            begin
              InvalidRow := True;
              Exit;
            end;
          end;
        end
        else
        if (Package = Packages.HobPackage) and (ColIndex = FRequiredCols) then
        begin
          // skip the name column.
          Values.Add(0);
        end
        else if (Package = Packages.Mf6ObservationUtility) then
        begin
          if (ColIndex = FRequiredCols +1) then
          begin
            ObsType := dgData.ItemIndex[ColIndex, RowIndex];
            if ObsType < 0 then
            begin
              InvalidRow := True;
              Exit;
            end;
          end
          else
          if (ColIndex = FRequiredCols) then
          begin
            // skip the name column.
            Values.Add(0);
          end
          else if (ColIndex > FRequiredCols+1) then
          begin
            // skip the calibration observations columns.
          end
        end
        else if (Package = Packages. UzfMf6Package) then
        begin
          // do nothing
        end
        else
        begin
          try
            Values.Add(StrToFloat(Trim(dgData.Cells[ColIndex, RowIndex])));
          except
            on E: EConvertError do
            begin
              InvalidRow := True;
              Exit;
            end;
          end;
        end;
      end;
      ABoundary := nil;
      HobBoundary := nil;
      UzfMf6Boundary := nil;
      if Package = Packages.ChdBoundary then
      begin
        AScreenObject.CreateChdBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowChdBoundary;
      end
      else if Package = Packages.GhbBoundary then
      begin
        AScreenObject.CreateGhbBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowGhbBoundary;
      end
      else if Package = Packages.WelPackage then
      begin
        AScreenObject.CreateWelBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowWellBoundary;
      end
      else if Package = Packages.RivPackage then
      begin
        AScreenObject.CreateRivBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowRivBoundary;
      end
      else if Package = Packages.DrnPackage then
      begin
        AScreenObject.CreateDrnBoundary;
        ABoundary := AScreenObject.ModflowBoundaries.ModflowDrnBoundary;
      end
      else if Package = Packages.HobPackage then
      begin
        AScreenObject.CreateHeadObservations;
        HobBoundary := AScreenObject.ModflowBoundaries.ModflowHeadObservations;
        ObsCount := HobBoundary.Values.Count;
        if ObsCount = 0 then
        begin
          Inc(FObsCount);
        end;
        LocalObsRoot := HobBoundary.ObservationName;
        if LocalObsRoot = '' then
        begin
          LocalObsRoot := Trim(dgData.Cells[FRequiredCols + Ord(hcName), RowIndex]);
        end;
        if LocalObsRoot = '' then
        begin
          NewObsName := FObsRoot + IntToStr(FObsCount) + '_';
          NewItemName := NewObsName + IntToStr(ObsCount+1);
          while (Length(NewItemName) > 12) and (FObsRoot <> '') do
          begin
            FObsRoot := Copy(FObsRoot, 1, Length(FObsRoot) -1);
            NewObsName := FObsRoot + IntToStr(FObsCount) + '_';
            NewItemName := NewObsName + IntToStr(ObsCount+1);
          end;
        end
        else
        begin
          NewItemName := LocalObsRoot + IntToStr(ObsCount+1);
          while (Length(NewItemName) > 12) and (LocalObsRoot <> '') do
          begin
            LocalObsRoot := Copy(LocalObsRoot, 1, Length(LocalObsRoot) -1);
            NewObsName := LocalObsRoot + IntToStr(FObsCount) + '_';
            NewItemName := NewObsName + IntToStr(ObsCount+1);
          end;
          NewObsName := LocalObsRoot;
        end;
        HobBoundary.ObservationName := NewObsName;
        if CharInSet(NewObsName[1], ['0'..'9']) then
        begin
          NewObsName := 'Obs_' + NewObsName;
        end;
        AScreenObject.Name := TScreenObject.ValidName(NewObsName);
      end
      else if Package = Packages.Mf6ObservationUtility then
      begin
        AScreenObject.CreateMf6Obs;
        Mf6Obs := AScreenObject.Modflow6Obs;
        if Mf6Obs.Name = '' then
        begin
          Mf6Obs.Name :=  dgData.Cells[FRequiredCols,RowIndex];
        end;
        ObsType := dgData.ItemIndex[FRequiredCols +1, RowIndex];
        if (ObsType >= 0) and (ObsType < Ord(ogUndefined)) then
        begin
          ObGeneral := TObGeneral(ObsType);
          Mf6Obs.General := Mf6Obs.General + [ObGeneral];
        end;

        if frmGoPhast.PhastModel.PestProperties.PestStatus <> psInactive then
        begin
          if (dgData.Cells[FRequiredCols+2 + Ord(cocTime),RowIndex] <> '')
            and (dgData.Cells[FRequiredCols+2 + Ord(cocValue),RowIndex] <> '') then
          begin
            CalibObs := Mf6Obs.CalibrationObservations.Add;
            if dgData.Cells[FRequiredCols,RowIndex] = '' then
            begin
              CalibObs.Name := Mf6Obs.Name + '_'
                + IntToStr(Mf6Obs.CalibrationObservations.Count);
            end
            else
            begin
              CalibObs.Name := dgData.Cells[FRequiredCols,RowIndex];
            end;
            CalibObs.Time :=
              dgData.RealValueDefault[FRequiredCols+2 + Ord(cocTime),RowIndex, 0];
            CalibObs.ObservedValue :=
              dgData.RealValueDefault[FRequiredCols+2 + Ord(cocValue),RowIndex, 0];
            CalibObs.Weight :=
              dgData.RealValueDefault[FRequiredCols+2 + Ord(cocWeight),RowIndex, 1];
            CalibObs.ObSeries := osGeneral;
            CalibObs.ObGeneral := ObGeneral;
          end;
        end;
      end
      else if Package = Packages.UzfMf6Package then
      begin
        AScreenObject.CreateUzfMf6Boundary;
        UzfMf6Boundary := AScreenObject.ModflowUzfMf6Boundary;
      end
      else
      begin
        Assert(False);
      end;
      if (ABoundary <> nil) and (ABoundary is TSpecificModflowBoundary) then
      begin
        TSpecificModflowBoundary(ABoundary).FormulaInterpretation := fiDirect;
      end;

      BoundaryItem := nil;
      HobItem := nil;
      AParam := comboParameter.Items.Objects[comboParameter.ItemIndex]
        as TModflowTransientListParameter;
      if AParam = nil then
      begin
        if ABoundary <> nil then
        begin
          BoundaryItem := ABoundary.Values.Add as TCustomModflowBoundaryItem;
        end;
        if HobBoundary <> nil then
        begin
          HobItem := HobBoundary.Values.Add as THobItem;
        end;
      end
      else
      begin
        AnItem := ABoundary.Parameters.GetParamByName(AParam.ParameterName);
        if AnItem = nil then
        begin
          AnItem := ABoundary.Parameters.Add;
          AnItem.Param.Param := AParam;
        end;
        BoundaryItem := AnItem.Param.Add as TCustomModflowBoundaryItem;
      end;
      if Package = Packages.ChdBoundary then
      begin
        GwtStart := Ord(ccEndHead) + 1;
        ChdItem := BoundaryItem as TChdItem;
        ChdItem.StartTime := Values[Ord(ccStartTime)];
        ChdItem.EndTime := Values[Ord(ccEndTime)];
        ChdItem.StartHead := FortranFloatToStr(Values[Ord(ccStartHead)]);
        ChdItem.EndHead := FortranFloatToStr(Values[Ord(ccEndHead)]);
        ChdItem.GwtConcentrations.Count := GwtColumns;
        for GwtIndex := 0 to GwtColumns - 1 do
        begin
          ChdItem.GwtConcentrations[GwtIndex].Value
            := FortranFloatToStr(Values[GwtIndex + GwtStart])
        end;

        SpecifiedHeadArray := frmGoPhast.PhastModel.DataArrayManager.
          GetDataSetByName(rsModflowSpecifiedHead);
        if SpecifiedHeadArray <> nil then
        begin
          FormulaPosition := AScreenObject.AddDataSet(SpecifiedHeadArray);
          AScreenObject.DataSetFormulas[FormulaPosition] := 'True';
        end;
      end
      else if Package = Packages.GhbBoundary then
      begin
        GwtStart := Ord(gcConductance) + 1;
        GhbItem := BoundaryItem as TGhbItem;
        GhbItem.StartTime := Values[Ord(gcStartTime)];
        GhbItem.EndTime := Values[Ord(gcEndTime)];
        GhbItem.BoundaryHead := FortranFloatToStr(Values[Ord(gcBoundaryHead)]);
        GhbItem.Conductance := FortranFloatToStr(Values[Ord(gcConductance)]);
        for GwtIndex := 0 to GwtColumns - 1 do
        begin
          GhbItem.GwtConcentrations[GwtIndex].Value
            := FortranFloatToStr(Values[GwtIndex + GwtStart])
        end;
      end
      else if Package = Packages.WelPackage then
      begin
        GwtStart := Ord(wcPumpingRate) + 1;
        WelItem := BoundaryItem as TWellItem;
        WelItem.StartTime := Values[Ord(wcStartTime)];
        WelItem.EndTime := Values[Ord(wcEndTime)];
        WelItem.PumpingRate := FortranFloatToStr(Values[Ord(wcPumpingRate)]);
        for GwtIndex := 0 to GwtColumns - 1 do
        begin
          WelItem.GwtConcentrations[GwtIndex].Value
            := FortranFloatToStr(Values[GwtIndex + GwtStart])
        end;
      end
      else if Package = Packages.RivPackage then
      begin
        GwtStart := Ord(rcBottom) + 1;
        RivItem := BoundaryItem as TRivItem;
        RivItem.StartTime := Values[Ord(rcStartTime)];
        RivItem.EndTime := Values[Ord(rcEndTime)];
        RivItem.RiverStage := FortranFloatToStr(Values[Ord(rcStage)]);
        RivItem.Conductance := FortranFloatToStr(Values[Ord(rcConductance)]);
        RivItem.RiverBottom := FortranFloatToStr(Values[Ord(rcBottom)]);
        for GwtIndex := 0 to GwtColumns - 1 do
        begin
          RivItem.GwtConcentrations[GwtIndex].Value
            := FortranFloatToStr(Values[GwtIndex + GwtStart])
        end;
      end
      else if Package = Packages.DrnPackage then
      begin
        DrnItem := BoundaryItem as TDrnItem;
        DrnItem.StartTime := Values[Ord(dcStartTime)];
        DrnItem.EndTime := Values[Ord(dcEndTime)];
        DrnItem.Elevation := FortranFloatToStr(Values[Ord(dcElevation)]);
        DrnItem.Conductance := FortranFloatToStr(Values[Ord(dcConductance)]);
      end
      else if Package = Packages.HobPackage then
      begin
        HobItem.Time := Values[Ord(hcTime)];
        HobItem.Head := Values[Ord(hcHead)];
        HobItem.Statistic := Values[Ord(hcStatistic)];
        IntValue := IntValues[0];
        if IntValue < Ord(Low(TStatFlag)) then
        begin
          IntValue := Ord(Low(TStatFlag))
        end;
        if IntValue > Ord(High(TStatFlag)) then
        begin
          IntValue := Ord(High(TStatFlag))
        end;
        HobItem.StatFlag := TStatFlag(IntValue);
      end
      else if Package = Packages.Mf6ObservationUtility then
      begin
        // do nothing
      end
      else if Package = Packages.UzfMf6Package then
      begin
        IgnoredNames := TStringList.Create;
        try
          frmGophast.PhastModel.GetIgnoredSpeciesNames(IgnoredNames);
          NumberOfSpecies := GwtColumns;
          GwtColumns := GwtColumns - IgnoredNames.Count;
          Assert(UzfMf6Boundary <> nil);
          ACol := FRequiredCols + Ord(u6scBrooksCoreyEpsilon) + 1;
          MobileComponents  := frmGoPhast.PhastModel.MobileComponents;
          if GwtColumns > 0 then
          begin
            UzfMf6Boundary.StartingConcentrations.Count := NumberOfSpecies;
            for SpeciesIndex := 0 to NumberOfSpecies - 1 do
            begin
              SpeciesName := MobileComponents[SpeciesIndex].Name;
              if IgnoredNames.IndexOf(SpeciesName) >= 0 then
              begin
                Continue;
              end;
              if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue) then
              begin
                UzfMf6Boundary.StartingConcentrations[SpeciesIndex].Value :=
                  FortranFloatToStr(AValue);
              end;
              Inc(ACol);
            end;
          end;
          StartTimeColumn := FRequiredCols + Ord(u6scBrooksCoreyEpsilon)
            + GwtColumns + 1;
          for UzfTransientIndex := Low(TUzf6TransientColumns) to High(TUzf6TransientColumns) do
          begin
            ACol := StartTimeColumn + Ord(UzfTransientIndex);
            if not TryStrToFloat(dgData.Cells[ACol, RowIndex], UzfTransientValues[UzfTransientIndex]) then
            begin
              InvalidRow := True;
              Exit;
            end;
          end;
          Uzf6Item := UzfMf6Boundary.Values.Add as TUzfMf6Item;
          Uzf6Item.StartTime := UzfTransientValues[u6tcStartTime];
          Uzf6Item.EndTime := UzfTransientValues[u6tcEndTime];
          Uzf6Item.Infiltration := FortranFloatToStr(UzfTransientValues[u6tcInfiltration]);
          Uzf6Item.PotentialET := FortranFloatToStr(UzfTransientValues[u6tcPotentialET]);
          Uzf6Item.ExtinctionDepth := FortranFloatToStr(UzfTransientValues[u6tcExtinctionDepth]);
          Uzf6Item.ExtinctionWaterContent := FortranFloatToStr(UzfTransientValues[u6tcExtinctionWaterContent]);
          Uzf6Item.AirEntryPotential := FortranFloatToStr(UzfTransientValues[u6tcAirEntryPotential]);
          Uzf6Item.RootPotential := FortranFloatToStr(UzfTransientValues[u6tcRootPotential]);
          Uzf6Item.RootActivity := FortranFloatToStr(UzfTransientValues[u6tcRootActivity]);
          if GwtColumns > 0 then
          begin
            Uzf6Item.GwtStatus.Count := NumberOfSpecies;
            Uzf6Item.SpecifiedConcentrations.Count := NumberOfSpecies;
            Uzf6Item.InfiltrationConcentrations.Count := NumberOfSpecies;
            Uzf6Item.EvapConcentrations.Count := NumberOfSpecies;
            ACol := StartTimeColumn + Ord(High(TUzf6TransientColumns)) + 1;
            for SpeciesIndex := 0 to NumberOfSpecies - 1 do
            begin
              SpeciesName := MobileComponents[SpeciesIndex].Name;
              if IgnoredNames.IndexOf(SpeciesName) >= 0 then
              begin
                Continue;
              end;
              if dgData.ItemIndex[ACol, RowIndex] >= 0 then
              begin
                Uzf6Item.GwtStatus[SpeciesIndex].GwtBoundaryStatus :=
                  TGwtBoundaryStatus(dgData.ItemIndex[ACol, RowIndex]);
              end;
              Inc(ACol);
              if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue)  then
              begin
                Uzf6Item.SpecifiedConcentrations[SpeciesIndex].Value
                  := FortranFloatToStr(AValue);
              end;
              Inc(ACol);
              if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue)  then
              begin
                Uzf6Item.InfiltrationConcentrations[SpeciesIndex].Value
                  := FortranFloatToStr(AValue);
              end;
              Inc(ACol);
              if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue)  then
              begin
                Uzf6Item.EvapConcentrations[SpeciesIndex].Value
                  := FortranFloatToStr(AValue);
              end;
              Inc(ACol);
            end;
          end;
          FSteadyOK := True;
          for UzfSteadyIndex := Low(TUzf6SteadyColumns) to High(TUzf6SteadyColumns) do
          begin
            if not TryStrToFloat(dgData.Cells[FRequiredCols + Ord(UzfSteadyIndex), RowIndex],
              UzfSteadyValues[UzfSteadyIndex]) then
            begin
              FSteadyOK := False;
            end;
          end;
          if FSteadyOK then
          begin
            AFormula := FortranFloatToStr(UzfSteadyValues[u6scSurfaceDepressionDepth]);
//            FormulaObject = UzfMf6Boundary.SurfaceDepressionDepth;
            SurfaceDepressionDepthDataArray :=
              frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
            if SurfaceDepressionDepthDataArray <> nil then
            begin
              FormulaPosition := AScreenObject.AddDataSet(SurfaceDepressionDepthDataArray);
              AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
            end;
            UzfMf6Boundary.SurfaceDepressionDepth :=  AFormula;

            AFormula := FortranFloatToStr(UzfSteadyValues[u6scVerticalSaturatedK]);
            VerticalSaturatedKDataArray :=
              frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6VerticalSaturatedK);
            if VerticalSaturatedKDataArray <> nil then
            begin
              FormulaPosition := AScreenObject.AddDataSet(VerticalSaturatedKDataArray);
              AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
            end;
            UzfMf6Boundary.VerticalSaturatedK := AFormula;

            AFormula := FortranFloatToStr(UzfSteadyValues[u6scResidualWaterContent]);
            UzfMf6Boundary.ResidualWaterContent := AFormula;
            ReisidualWaterContentDataArray :=
              frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6ReisidualWaterContent);
            if ReisidualWaterContentDataArray <> nil then
            begin
              FormulaPosition := AScreenObject.AddDataSet(ReisidualWaterContentDataArray);
              AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
            end;

            AFormula := FortranFloatToStr(UzfSteadyValues[u6scSaturatedWaterContent]);
            UzfMf6Boundary.SaturatedWaterContent := AFormula;
            SaturatedWaterContentDataArray :=
              frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6SaturatedWaterContent);
            if SaturatedWaterContentDataArray <> nil then
            begin
              FormulaPosition := AScreenObject.AddDataSet(SaturatedWaterContentDataArray);
              AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
            end;

            AFormula := FortranFloatToStr(UzfSteadyValues[u6scInitialWaterContent]);
            UzfMf6Boundary.InitialWaterContent := AFormula;
            InitialUnsaturatedWaterContentDataArray :=
              frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6InitialUnsaturatedWaterContent);
            if InitialUnsaturatedWaterContentDataArray <> nil then
            begin
              FormulaPosition := AScreenObject.AddDataSet(InitialUnsaturatedWaterContentDataArray);
              AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
            end;

            AFormula := FortranFloatToStr(UzfSteadyValues[u6scBrooksCoreyEpsilon]);
            UzfMf6Boundary.BrooksCoreyEpsilon := AFormula;
            BrooksCoreyEpsilonDataArray :=
              frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6BrooksCoreyEpsilon);
            if BrooksCoreyEpsilonDataArray <> nil then
            begin
              FormulaPosition := AScreenObject.AddDataSet(BrooksCoreyEpsilonDataArray);
              AScreenObject.DataSetFormulas[FormulaPosition] := AFormula;
            end;
          end;
          ACol := FRequiredCols + Ord(u6scBrooksCoreyEpsilon) + 1;
          MobileComponents  := frmGoPhast.PhastModel.MobileComponents;
          if GwtColumns > 0 then
          begin
            UzfMf6Boundary.StartingConcentrations.Count := NumberOfSpecies;
            for SpeciesIndex := 0 to NumberOfSpecies - 1 do
            begin
              SpeciesName := MobileComponents[SpeciesIndex].Name;
              if IgnoredNames.IndexOf(SpeciesName) >= 0 then
              begin
                Continue;
              end;
              if TryStrToFloat(dgData.Cells[ACol, RowIndex], AValue) then
              begin
                UzfMf6Boundary.StartingConcentrations[SpeciesIndex].Value :=
                  FortranFloatToStr(AValue);
              end;
              Inc(ACol);
            end;
          end;
        finally
          IgnoredNames.Free;
        end;
      end
      else
      begin
        Assert(False);
      end;
    end
    else if frmGoPhast.ModelSelection = msFootPrint then
    begin
      Assert(comboBoundaryChoice.Items.Objects[comboBoundaryChoice.ItemIndex]
        is TFootprintProperties);
      try
        AValue := StrToFloat(Trim(dgData.Cells[Ord(fwcWithdrawal)+2, RowIndex]));
      except
        on E: EConvertError do
        begin
          InvalidRow := True;
          Exit;
        end;
      end;
      AScreenObject.CreateFootprintWell;
      AFootPrint := AScreenObject.FootprintWell;
      AFootPrint.IsUsed := True;
      AFootPrint.Withdrawal := FortranFloatToStr(AValue);
      DataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KWithdrawals);
      DataArrayPosition := AScreenObject.AddDataSet(DataArray);
      AScreenObject.DataSetFormulas[DataArrayPosition] := AFootPrint.Withdrawal;
    end;
  finally
    Values.Free;
    IntValues.Free;
  end;
end;

procedure TfrmImportPoints.GetData;
var
  Packages: TModflowPackages;
begin
  rgEvaluatedAt.Items[Ord(eaBlocks)] := EvalAtToString(eaBlocks,
    frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Items[Ord(eaNodes)] := EvalAtToString(eaNodes,
    frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Enabled :=
    frmGoPhast.PhastModel.ModelSelection in [msPhast, msSutra22, msSutra30, msSutra40];
  if not rgEvaluatedAt.Enabled then
  begin
    rgEvaluatedAt.ItemIndex := 0;
  end;
  if (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Packages := frmGoPhast.PhastModel.ModflowPackages;
    if Packages.ChdBoundary.IsSelected then
    begin
      comboBoundaryChoice.Items.AddObject(
        Packages.ChdBoundary.PackageIdentifier, Packages.ChdBoundary);
    end;
    if Packages.DrnPackage.IsSelected then
    begin
      comboBoundaryChoice.Items.AddObject(
        Packages.DrnPackage.PackageIdentifier, Packages.DrnPackage);
    end;
    if Packages.GhbBoundary.IsSelected then
    begin
      comboBoundaryChoice.Items.AddObject(
        Packages.GhbBoundary.PackageIdentifier, Packages.GhbBoundary);
    end;
    if Packages.RivPackage.IsSelected then
    begin
      comboBoundaryChoice.Items.AddObject(
        Packages.RivPackage.PackageIdentifier, Packages.RivPackage);
    end;
    if Packages.WelPackage.IsSelected then
    begin
      comboBoundaryChoice.Items.AddObject(
        Packages.WelPackage.PackageIdentifier, Packages.WelPackage);
    end;
    if Packages.HobPackage.IsSelected
      and (frmGoPhast.PhastModel.ModelSelection <> msModflow2015) then
    begin
      comboBoundaryChoice.Items.AddObject(
        Packages.HobPackage.PackageIdentifier, Packages.HobPackage);
    end;
    if frmGoPhast.PhastModel.ModelSelection = msModflow2015 then
    begin
      if Packages.Mf6ObservationUtility.IsSelected then
      begin
        comboBoundaryChoice.Items.AddObject(
          Packages.Mf6ObservationUtility.PackageIdentifier,
          Packages.Mf6ObservationUtility);
      end;
      if Packages.UzfMf6Package.IsSelected then
      begin
        comboBoundaryChoice.Items.AddObject(
          Packages.UzfMf6Package.PackageIdentifier, Packages.UzfMf6Package);
      end;
    end;
  end
  else if frmGoPhast.PhastModel.ModelSelection = msFootPrint then
  begin
    rgElevationCount.Enabled := False;
    comboBoundaryChoice.Items.AddObject(
      StrFootprintWithdrawal, frmGoPhast.PhastModel.FootprintProperties);
  end;

  FImportFileName := '';
  cbIntersectedCellsClick(nil);
  SetCheckBoxCaptions;
  pcImportPoints.ActivePageIndex := 0;
  UpdateDimensionColumns;
  UpdateDataSets;
  rgElevationCountClick(nil);
  EnableRowCol;
end;

procedure TfrmImportPoints.dgDataColMoving(Sender: TObject; const Origin,
  Destination: Integer; var CanMove: Boolean);
var
  RequiredColumns: integer;
begin
  inherited;
  RequiredColumns := rgElevationCount.ItemIndex + 2;
  CanMove := (Origin >= RequiredColumns) and (Destination >= RequiredColumns);
end;

procedure TfrmImportPoints.dgDataDistributeTextProgress(Sender: TObject;
  Position, Max: Integer);
const
  OneSecond = 1/24/3600;
begin
  inherited;

  if Now - StartTime > OneSecond then
  begin
    if not frmProgressMM.Visible then
    begin
      frmProgressMM.Caption := StrReadingData;
    end;
    if Position < Max then
    begin
      frmProgressMM.pbProgress.Max := Max;
      frmProgressMM.pbProgress.Position := Position;
      frmProgressMM.Show;
      StartTime := Now;
      Application.ProcessMessages;
    end;
  end;
  if Position = Max then
  begin
    frmProgressMM.Hide;
    Application.ProcessMessages;
  end;
end;

procedure TfrmImportPoints.dgDataEndUpdate(Sender: TObject);
begin
  inherited;
  if seRows <> nil then
  begin
    seRows.Value := dgData.RowCount -1;
    EnableOkButton;
  end;
end;

procedure TfrmImportPoints.rgElevationCountClick(Sender: TObject);
begin
  inherited;
  if rgElevationCount.ItemIndex = 0 then
  begin
    comboBoundaryChoiceChange(nil);
  end;
  if frmGoPhast.PhastModel.ModelSelection
    in ModflowSelection then
  begin
    comboBoundaryChoice.Enabled :=
     (rgElevationCount.ItemIndex > 0)
      and (comboBoundaryChoice.Items.Count > 1);
  end
  else if frmGoPhast.PhastModel.ModelSelection = msFootPrint then
  begin
    comboBoundaryChoice.Enabled :=
      (comboBoundaryChoice.Items.Count > 1);
  end
  else
  begin
    comboBoundaryChoice.Enabled := False;
  end;

  if (not comboBoundaryChoice.Enabled)
    and (comboBoundaryChoice.ItemIndex > 0) then
  begin
    comboBoundaryChoice.ItemIndex := 0;
    comboBoundaryChoiceChange(nil);
  end;

  UpdateDimensionColumns;
  UpdateDataSets;
  if comboBoundaryChoice.Enabled then
  begin
    comboBoundaryChoiceChange(nil);
  end;
end;

{ TUndoImportPoints }

function TUndoImportPoints.Description: string;
begin
  result := StrImportPoints;
end;

procedure TfrmImportPoints.SetData;
var
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  ScreenObjectList: TList;
  Position: integer;
  DataSet: TDataArray;
  RowIndex: integer;
  Undo: TUndoImportPoints;
  Root: string;
  APoint: TPoint2D;
  AnXCol: integer;
  AYCol: integer;
  AZ1Col: integer;
  AZ2Col: integer;
  ColIndex: integer;
  ExistingObjectCount: integer;
  ElevValues1: TValueArrayStorage;
  ElevValues2: TValueArrayStorage;
  Item: TValueArrayItem;
  NewPoint: Boolean;
  FirstPoint: Boolean;
  PointCount: Integer;
  InvalidRow: Boolean;
  NewScreenObject: Boolean;
  ValueListIndex: Integer;
  VList: TValueArrayStorage;
  Elevation1: Extended;
  Elevation2: Extended;
  Grid: TCustomModelGrid;
  Layer: Integer;
  ACell: T2DTopCell;
  DummyInvalidIndex: Boolean;
  Column: Integer;
  Row: Integer;
  Index: Integer;
  NewName: string;
  NewNames: TStringList;
  Value: Integer;
  Mesh: IMesh3D;
begin
  FObsCount := 0;
  FRequiredCols := rgElevationCount.ItemIndex + 2;
  Root := TScreenObject.ValidName(edRoot.Text) + '_';
  if Trim(Root) = '_' then
  begin
    Root := ObjectPrefix;
  end;
  FObsRoot := Copy(Root, 1, 12);
  ExistingObjectCount :=
    frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);
  AnXCol := -1;
  AYCol := -1;
  AZ1Col := -1;
  AZ2Col := -1;
  case TViewDirection(rgViewDirection.ItemIndex) of
    vdTop:
      begin
        AnXCol := XCol;
        AYCol  := YCol;
        case TElevationCount(rgElevationCount.ItemIndex) of
          ecZero:
            begin
              AZ1Col := -1;
              AZ2Col := -1;
            end;
          ecOne:
            begin
              AZ1Col := ZCol;
              AZ2Col := -1;
            end;
          ecTwo:
            begin
              AZ1Col := Z1Col;
              AZ2Col := Z2Col;
            end;
        else
          Assert(False);
        end;
      end;
    vdFront:
      begin
        AnXCol := XCol;
        AYCol  := ZCol;
        case TElevationCount(rgElevationCount.ItemIndex) of
          ecZero:
            begin
              AZ1Col := -1;
              AZ2Col := -1;
            end;
          ecOne:
            begin
              AZ1Col := YCol;
              AZ2Col := -1;
            end;
          ecTwo:
            begin
              AZ1Col := Y1Col;
              AZ2Col := Y2Col;
            end;
        else
          Assert(False);
        end;
      end;
    vdSide:
      begin
        AnXCol := ZCol;
        AYCol  := YCol;
        case TElevationCount(rgElevationCount.ItemIndex) of
          ecZero:
            begin
              AZ1Col := -1;
              AZ2Col := -1;
            end;
          ecOne:
            begin
              AZ1Col := XCol;
              AZ2Col := -1;
            end;
          ecTwo:
            begin
              AZ1Col := X1Col;
              AZ2Col := X2Col;
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;

  Grid := frmGoPhast.Grid;
  Mesh := frmGoPhast.PhastModel.Mesh3D;

  FMultiValueList := TList.Create;
  ScreenObjectList := TList.Create;
  frmGoPhast.PhastModel.BeginScreenObjectUpdate;
  ElevValues1 := TValueArrayStorage.Create;
  ElevValues2 := TValueArrayStorage.Create;
  NewNames := TStringList.Create;
  try
    NewNames.CaseSensitive := False;
    NewNames.Sorted := True;
    Undo := TUndoImportPoints.Create;
    try
      ElevValues1.Count := seRows.AsInteger;
      ElevValues2.Count := seRows.AsInteger;
      if cbImportAsSingleObject.Checked then
      begin
        ScreenObjectList.Capacity := 1;
      end
      else
      begin
        ScreenObjectList.Capacity := seRows.AsInteger;
      end;
      frmProgressMM.Caption := StrProgress;
      frmProgressMM.pbProgress.Max := dgData.RowCount-1;
      frmProgressMM.pbProgress.Position := 0;
      frmProgressMM.PopupParent := frmGoPhast;
      frmProgressMM.Show;
      FirstPoint := True;
      PointCount := 0;
      for RowIndex := 1 to dgData.RowCount - 1 do
      begin
        NewPoint := (Trim(dgData.Cells[AnXCol, RowIndex]) <> '')
          and (Trim(dgData.Cells[AYCol, RowIndex]) <> '');
        if NewPoint and (AZ1Col >= 0) then
        begin
          NewPoint := Trim(dgData.Cells[AZ1Col, RowIndex]) <> '';
        end;
        if NewPoint and (AZ2Col >= 0) then
        begin
          NewPoint := dgData.Cells[AZ2Col, RowIndex] <> '';
        end;
        if NewPoint then
        begin
          try
            begin
              if cbRowCol.Checked then
              begin
                if Grid <> nil then
                begin
                  Assert(Grid <> nil);
                  // top view
                  Assert(rgViewDirection.ItemIndex = 0);
                  Column := StrToInt(Trim(dgData.Cells[AnXCol, RowIndex]))-1;
                  Row := StrToInt(Trim(dgData.Cells[AYCol, RowIndex]))-1;
                  if rgEvaluatedAt.ItemIndex = 0 then
                  begin
                    // elements
                    APoint := Grid.TwoDElementCenter(Column, Row);
                  end
                  else
                  begin
                    // nodes
                    APoint := Grid.TwoDCellCorner(Column, Row);
                  end;
                end
                else
                begin
                  Assert(Mesh <> nil);
                  Assert(rgViewDirection.ItemIndex = 0);
                  Column := StrToInt(dgData.Cells[AnXCol, RowIndex])-1;
                  if rgEvaluatedAt.ItemIndex = 0 then
                  begin
                    // elements
                    APoint := Mesh.Mesh2DI.ElementsI2D[Column].Center;
                  end
                  else
                  begin
                    // nodes
                    APoint := Mesh.Mesh2DI.NodesI2D[Column].Location;
                  end;
                end;
              end
              else
              begin
                APoint.X := StrToFloat(dgData.Cells[AnXCol, RowIndex]);
                APoint.Y := StrToFloat(dgData.Cells[AYCol, RowIndex]);
              end;
              if AZ1Col >= 0 then
              begin
                if cbLayer.Checked then
                begin
                  StrToInt(dgData.Cells[AZ1Col, RowIndex]);
                end
                else
                begin
                  StrToFloat(dgData.Cells[AZ1Col, RowIndex]);
                end;
              end;
              if AZ2Col >= 0 then
              begin
                StrToFloat(Trim(dgData.Cells[AZ2Col, RowIndex]));
              end;
            end;
          except on EConvertError do
            NewPoint := False;
          end;
        end;
        if NewPoint and ((not cbImportAsSingleObject.Checked) or FirstPoint) then
        begin
          AScreenObject :=
            TScreenObject.CreateWithViewDirection(frmGoPhast.PhastModel,
            TViewDirection(rgViewDirection.ItemIndex),
            UndoCreateScreenObject, False);
          AScreenObject.Comment := 'Imported on ' + DateTimeToStr(Now);
          Inc(ExistingObjectCount);
          NewName := '';
          
          if (frmGoPhast.ModelSelection = msFootPrint) and (comboBoundaryChoice.ItemIndex > 0) then
          begin
            NewName := Trim(dgData.Cells[dgData.ColCount-1, RowIndex]);
            if NewName <> '' then
            begin
              if NewName[1].IsDigit then
              begin
                NewName := '_' + NewName;
              end;
              NewName := TScreenObject.ValidName(NewName);
              if NewNames.IndexOf(NewName) >= 0 then
              begin
                Value := 1;
                while NewNames.IndexOf(Format(Str0s1d, [NewName, Value])) >= 0 do
                begin
                  Inc(Value);
                end;
                NewName := Format(Str0s1d, [NewName, Value]);
              end;
              NewNames.Add(NewName);
            end;
          end;
          
          if NewName = '' then
          begin
            AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
          end
          else
          begin
            AScreenObject.Name := NewName;
          end;
          AScreenObject.SetValuesOfEnclosedCells
            := False;
          AScreenObject.SetValuesOfIntersectedCells
            := cbIntersectedCells.Checked;
          AScreenObject.SetValuesByInterpolation
            := cbInterpolation.Checked;

          AScreenObject.EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
          if cbImportAsSingleObject.Checked then
          begin
            AScreenObject.Capacity := dgData.RowCount - 1;
          end
          else
          begin
            AScreenObject.Capacity := 1;
          end;
          AScreenObject.Visible := cbVisible.Checked;
          if cbImportAsSingleObject.Checked and FirstPoint then
          begin
            Assert(comboBoundaryChoice.ItemIndex <= 0);
            for ColIndex := FRequiredCols to dgData.ColCount -1 do
            begin
              DataSet := dgData.Objects[ColIndex, 0] as TDataArray;
              Item := AScreenObject.ImportedValues.Add as TValueArrayItem;
              Item.Name := DataSet.Name;
              Item.Values.DataType := DataSet.DataType;
              Item.Values.Count := dgData.RowCount - 1;
              FMultiValueList.Add(Item.Values);
            end;
          end;
        end;

        if NewPoint then
        begin
          AScreenObject.AddPoint(APoint, cbImportAsSingleObject.Checked);
        end;

        NewScreenObject := NewPoint and ((not cbImportAsSingleObject.Checked) or FirstPoint);
        if NewScreenObject then
        begin
          AScreenObject.ElevationCount := TElevationCount(rgElevationCount.ItemIndex);
          if cbImportAsSingleObject.Checked then
          begin
            case AScreenObject.ElevationCount of
              ecZero:
                begin
                  // do nothing
                end;
              ecOne:
                begin
                  AScreenObject.ElevationFormula :=
                    rsObjectImportedValuesR
                    + '("' + StrImportedElevations + '")';
                end;
              ecTwo:
                begin
                  AScreenObject.LowerElevationFormula :=
                    rsObjectImportedValuesR
                    + '("' + StrImportedLowerEleva + '")';
                  AScreenObject.HigherElevationFormula :=
                    rsObjectImportedValuesR
                    + '("' + StrImportedHigherElev + '")';
                end;
            else
              Assert(False);
            end;
          end;
        end;
        if NewPoint then
        begin
          case AScreenObject.ElevationCount of
            ecZero:
              begin
                if cbImportAsSingleObject.Checked then
                begin
                  Inc(PointCount);
                end;
                // do nothing
              end;
            ecOne:
              begin
                if cbLayer.Checked then
                begin
                  Layer := StrToInt(Trim(dgData.Cells[AZ1Col, RowIndex]))-1;
                  if Grid <> nil then
                  begin
                    Assert(Grid <> nil);
                    ACell := Grid.TopContainingCell(APoint,
                      TEvaluatedAt(rgEvaluatedAt.ItemIndex));
                  end
                  else
                  begin
                    Assert(Mesh <> nil);
                    ACell := Mesh.TopContainingCellOrElement(APoint,
                      TEvaluatedAt(rgEvaluatedAt.ItemIndex));
                  end;
                  Elevation1 := 0;
                  case TEvaluatedAt(rgEvaluatedAt.ItemIndex) of
                    eaBlocks:
                      begin
                        Elevation1 := GetLayerCenter(Layer, ACell.Row, ACell.Col);
                      end;
                    eaNodes:
                      begin
                        Elevation1 := GetLayerPosition(Layer,
                          ACell.Row, ACell.Col, DummyInvalidIndex);
                      end;
                    else
                      Assert(False);
                  end;
                end
                else
                begin
                  Elevation1 := StrToFloat(Trim(dgData.Cells[AZ1Col, RowIndex]));
                end;
                if cbImportAsSingleObject.Checked then
                begin
                  ElevValues1.RealValues[PointCount] := Elevation1;
                  Inc(PointCount);
                end
                else
                begin
                  AScreenObject.ElevationFormula :=
                    FortranFloatToStr(Elevation1);
                end;
              end;
            ecTwo:
              begin
                Elevation1 := StrToFloat(Trim(dgData.Cells[AZ1Col, RowIndex]));
                Elevation2 := StrToFloat(Trim(dgData.Cells[AZ2Col, RowIndex]));
                if cbImportAsSingleObject.Checked then
                begin
                  ElevValues1.RealValues[PointCount] := Elevation1;
                  ElevValues2.RealValues[PointCount] := Elevation2;
                  Inc(PointCount);
                end
                else
                begin
                  AScreenObject.HigherElevationFormula :=
                    FortranFloatToStr(Elevation1);
                  AScreenObject.LowerElevationFormula :=
                    FortranFloatToStr(Elevation2);
                end;
              end;
          else
            Assert(False);
          end;
        end;

        if comboBoundaryChoice.ItemIndex <= 0 then
        begin
          if NewPoint then
          begin
            ImportDataArrayValues(InvalidRow, RowIndex, PointCount, AScreenObject);
          end;
        end
        else
        begin
          if rgFeatureDataFormat.ItemIndex = 0 then
          begin
            ImportModflowBoundary1RowPerTime(InvalidRow, AScreenObject, RowIndex);
          end
          else
          begin
            ImportModflowBoundary1RowPerLocation(InvalidRow, AScreenObject, RowIndex);
          end;
        end;

        if InvalidRow then
        begin
          if cbImportAsSingleObject.Checked then
          begin
            Undo.Free;
            Exit;
          end
          else
          begin
            Continue;
          end;
        end;

        if NewScreenObject then
        begin
          ScreenObjectList.Add(AScreenObject);
        end;

        if NewPoint then
        begin
          FirstPoint := False;
        end;

        if RowIndex mod 100 = 0 then
        begin
          frmProgressMM.BringToFront;
          Application.ProcessMessages;
        end;
        frmProgressMM.StepIt;
      end;

      if cbImportAsSingleObject.Checked and (AScreenObject <> nil) then
      begin
        for ValueListIndex := 0 to FMultiValueList.Count - 1 do
        begin
          VList := FMultiValueList[ValueListIndex];
          VList.Count := PointCount;
          VList.CacheData;
        end;

        case AScreenObject.ElevationCount of
          ecZero:
            begin
              // do nothing
            end;
          ecOne:
            begin
              ElevValues1.Count := PointCount;
              AScreenObject.ImportedSectionElevations := ElevValues1;
              AScreenObject.ImportedSectionElevations.CacheData;
            end;
          ecTwo:
            begin
              ElevValues1.Count := PointCount;
              ElevValues2.Count := PointCount;
              AScreenObject.ImportedHigherSectionElevations := ElevValues1;
              AScreenObject.ImportedLowerSectionElevations := ElevValues2;
              AScreenObject.ImportedHigherSectionElevations.CacheData;
              AScreenObject.ImportedLowerSectionElevations.CacheData;
            end;
        else
          Assert(False);
        end;
        for ColIndex := FRequiredCols to dgData.ColCount -1 do
        begin
          DataSet := dgData.Objects[ColIndex, 0] as TDataArray;
          Position := AScreenObject.AddDataSet(DataSet);
          Assert(Position >= 0);
          case DataSet.DataType of
            rdtDouble:
              begin
                AScreenObject.DataSetFormulas[Position] :=
                  rsObjectImportedValuesR + '("' + DataSet.Name + '")';
              end;
            rdtInteger:
              begin
                AScreenObject.DataSetFormulas[Position] :=
                  rsObjectImportedValuesI + '("' + DataSet.Name + '")';
              end;
            rdtBoolean:
              begin
                AScreenObject.DataSetFormulas[Position] :=
                  rsObjectImportedValuesB + '("' + DataSet.Name + '")';
              end;
            rdtString:
              begin
                AScreenObject.DataSetFormulas[Position] :=
                  rsObjectImportedValuesT + '("' + DataSet.Name + '")';
              end;
            else Assert(False);
          end;
        end;
      end;

      if ScreenObjectList.Count > 0 then
      begin
        Undo.StoreNewScreenObjects(ScreenObjectList);
        frmGoPhast.UndoStack.Submit(Undo);
        if FImportFileName <> '' then
        begin
          frmGoPhast.PhastModel.AddFileToArchive(FImportFileName);
        end;
      end
      else
      begin
        Undo.Free
      end;  
    except
      on E: EInvalidGrid do
        begin
          Beep;
          Undo.Free;
          MessageDlg(Format(StrThereWasAnErrorI, [E.message]), mtError, [mbOK], 0);
          for Index := 0 to ScreenObjectList.Count - 1 do
          begin
            AScreenObject := ScreenObjectList[Index];
            AScreenObject.Free
          end;
        end;
      else
      begin
        for Index := 0 to ScreenObjectList.Count - 1 do
        begin
          AScreenObject := ScreenObjectList[Index];
          AScreenObject.Free
        end;
        Undo.Free;
        raise;
      end;
    end;
  finally
    frmProgressMM.Hide;
    NewNames.Free;
    ScreenObjectList.Free;
    ElevValues1.Free;
    ElevValues2.Free;
    FMultiValueList.Free;
    frmGoPhast.PhastModel.EndScreenObjectUpdate;
  end;
end;

procedure TfrmImportPoints.btnOKClick(Sender: TObject);
var
  RequiredCols: Integer;
  ColIndex: Integer;
  DataSet: TDataArray;
begin
  inherited;
  if (comboBoundaryChoice.ItemIndex > 0) and cbImportAsSingleObject.Checked then
  begin
    Beep;
    MessageDlg(StrMODFLOWBoundaryCon1, mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;

  if cbInterpolation.Checked then
  begin
    if comboBoundaryChoice.ItemIndex > 0 then
    begin
      if not cbIntersectedCells.Checked then
      begin
        Beep;
        MessageDlg(StrMODFLOWBoundaryCon2, mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      end;
    end
    else
    begin
      RequiredCols := rgElevationCount.ItemIndex + 2;
      for ColIndex := RequiredCols to dgData.ColCount -1 do
      begin
        DataSet := dgData.Objects[ColIndex, 0] as TDataArray;
        if DataSet.TwoDInterpolator = nil then
        begin
          if (MessageDlg(StrYouHaveChoosenTo,
            mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          begin
            break;
          end
          else
          begin
            ModalResult := mrNone;
            Exit;
          end;
        end;
      end;
    end;
  end;
  Hide;
  SetData;
end;

procedure TfrmImportPoints.btnOpenFileClick(Sender: TObject);
var
  Lines: TStringList;
  Index: Integer;
  DataSetSelected: boolean;
  ColIndex: Integer;
begin
  inherited;
  DataSetSelected := comboBoundaryChoice.ItemIndex > 0;
  if not DataSetSelected  then
  begin
    for Index := 0 to jvclbDataSets.Items.Count - 1 do
    begin
      DataSetSelected := jvclbDataSets.Checked[Index];
      if DataSetSelected then
      begin
        break;
      end;
    end;
  end;

  if not DataSetSelected then
  begin
    Beep;
    MessageDlg(StrYouHaveNotSelecte,
      MtWarning, [mbOK], 0);
  end;
  if OpenDialogImportFile.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      Lines := TStringList.Create;
      try
        FImportFileName := OpenDialogImportFile.FileName;
        try
          Lines.LoadFromFile(FImportFileName);
        except on EFOpenError do
          begin
            CantOpenFileMessage(FImportFileName);
            Exit;
          end;
        end;
        for Index := Lines.Count - 1 downto 0 do
        begin
          if (Length(Lines[Index]) = 0) or (Lines[Index][1] = '#') then
          begin
            Lines.Delete(Index);
          end;
        end;
        seRows.Value := Lines.Count;

        if OpenDialogImportFile.FilterIndex = 2 then
        begin
          for Index := 0 to Lines.Count - 1 do
          begin
            Lines[Index] := StringReplace(Lines[Index], ',', #9,
              [rfReplaceAll, rfIgnoreCase])
          end;
        end;

        dgData.BeginUpdate;
        try
          StartTime := Now;
          dgData.DistributeText(0,1, Lines.Text);
          frmProgressMM.Hide;
          for Index := dgData.RowCount - 1 downto 0 do
          begin
            for ColIndex := 0 to dgData.ColCount - 1 do
            begin
              if (dgData.Cells[ColIndex,Index] = '')
                and (dgData.Columns[ColIndex].Format in
                [rcf4Integer, rcf4Real]) then
              begin
                dgData.DeleteRow(Index);
                break;
              end;
            end;
          end;
          seRows.Value := dgData.RowCount - 1;
        finally
          dgData.EndUpdate;
        end;
      finally
        Lines.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmImportPoints.cbIntersectedCellsClick(Sender: TObject);
begin
  inherited;
  EmphasizeCheckBoxes([cbIntersectedCells, cbInterpolation]);
  EnableOkButton;

end;

procedure TfrmImportPoints.cbLayerClick(Sender: TObject);
begin
  inherited;
  rgElevationCount.Enabled := not cbLayer.Checked;
  if not rgElevationCount.Enabled  then
  begin
    rgElevationCount.ItemIndex := 1;
    rgElevationCountClick(nil);
  end;
  if comboBoundaryChoice.Enabled then
  begin
    comboBoundaryChoiceChange(nil);
  end;
end;

procedure TfrmImportPoints.cbRowColClick(Sender: TObject);
begin
  inherited;
  UpdateDimensionColumns;
  if cbRowCol.Checked then
  begin
    Assert(XCol >= 0);
    Assert(YCol >= 0);
    dgData.Columns[XCol].Format := rcf4Integer;
    dgData.Columns[YCol].Format := rcf4Integer;
  end
  else
  begin
    if XCol >= 0 then
    begin
      dgData.Columns[XCol].Format := rcf4Real;
    end;
    if YCol >= 0 then
    begin
      dgData.Columns[YCol].Format := rcf4Real;
    end;
  end;
  jvclbDataSetsClickCheck(nil);
end;

procedure TfrmImportPoints.comboBoundaryChoiceChange(Sender: TObject);
var
  ItemIndex: Integer;
  Package: TModflowPackageSelection;
  Packages: TModflowPackages;
  ParameterIndex: Integer;
  TransientParameters: TModflowTransientListParameters;
  AParam: TModflowTransientListParameter;
begin
  inherited;
  seTimePeriods.Enabled := rgFeatureDataFormat.ItemIndex = 1;
  if comboBoundaryChoice.ItemIndex > 0 then
  begin
    pnlFeatureOptions.Visible := True;
    dgData.Options := dgData.Options - [goColMoving];
    cbImportAsSingleObject.Checked := False;
    cbImportAsSingleObject.Enabled := False;
    for ItemIndex := 0 to jvclbDataSets.Items.Count - 1 do
    begin
      jvclbDataSets.Checked[ItemIndex] := False;
    end;
    comboParameter.Items.Clear;
    comboParameter.Items.Add(StrNone);
    comboParameter.ItemIndex := 0;
    if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      Package := comboBoundaryChoice.Items.Objects[
        comboBoundaryChoice.ItemIndex] as TModflowPackageSelection;
      Packages := frmGoPhast.PhastModel.ModflowPackages;
      comboParameter.Enabled := True;
      TransientParameters := frmGoPhast.PhastModel.ModflowTransientParameters;
      for ParameterIndex := 0 to TransientParameters.Count - 1 do
      begin
        AParam := TransientParameters[ParameterIndex];
        case AParam.ParameterType of
          ptCHD:
            begin
              if Package = Packages.ChdBoundary then
              begin
                comboParameter.Items.AddObject(AParam.ParameterName, AParam);
              end;
            end;
          ptGHB:
            begin
              if Package = Packages.GhbBoundary then
              begin
                comboParameter.Items.AddObject(AParam.ParameterName, AParam);
              end;
            end;
          ptQ:
            begin
              if Package = Packages.WelPackage then
              begin
                comboParameter.Items.AddObject(AParam.ParameterName, AParam);
              end;
            end;
          ptRIV:
            begin
              if Package = Packages.RivPackage then
              begin
                comboParameter.Items.AddObject(AParam.ParameterName, AParam);
              end;
            end;
          ptDRN:
            begin
              if Package = Packages.DrnPackage then
              begin
                comboParameter.Items.AddObject(AParam.ParameterName, AParam);
              end;
            end;
        end;
      end;
      if Package = Packages.ChdBoundary then
      begin
        UpdateChdColumns;
      end
      else if Package = Packages.GhbBoundary then
      begin
        UpdateGhbColumns;
      end
      else if Package = Packages.WelPackage then
      begin
        UpdateWelColumns;
      end
      else if Package = Packages.RivPackage then
      begin
        UpdateRivColumns;
      end
      else if Package = Packages.DrnPackage then
      begin
        UpdateDrnColumns;
      end
      else if Package = Packages.HobPackage then
      begin
        UpdateHobColumns;
      end
      else if Package = Packages.Mf6ObservationUtility then
      begin
        UpdateObsUtilColumns;
      end
      else if Package = Packages.UzfMf6Package then
      begin
        UpdateUzfMf6Columns;
      end
      else
      begin
        Assert(False);
      end;
    end
    else if frmGoPhast.ModelSelection = msFootPrint then
    begin
      comboParameter.Enabled := False;
      UpdateFootprintWellColumns;
    end;
  end
  else
  begin
    pnlFeatureOptions.Visible := False;
    dgData.Options := dgData.Options + [goColMoving];
    comboParameter.ItemIndex := 0;
    comboParameter.Enabled := False;
    cbImportAsSingleObject.Enabled := True;
//    FirstColumn := rgElevationCount.ItemIndex + 2;
    dgData.ColCount := rgElevationCount.ItemIndex + 2;
//    SetBoundaryColumnFormats;
  end;
  jvclbDataSets.Enabled := comboBoundaryChoice.ItemIndex <= 0;
  jvclbDataSetsClickCheck(nil);
end;

end.
