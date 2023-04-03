unit frmExportShapefileObjectsUnit;

interface

uses System.Types, System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomSelectObjectsUnit, VirtualTrees, StdCtrls, Buttons,
  ExtCtrls, Contnrs, ScreenObjectUnit, DataSetUnit, ArgusDataEntry,
  ShapefileUnit, XBase1, ValueArrayStorageUnit, CheckLst,
  frmCustomGoPhastUnit;

type
  TFhbBoundaryType = (fbtFlow, fbtHead);
  TFhbBoundaryTypes = set of TFhbBoundaryType;

  TFieldDefinition = record
    DataArray: TDataArray;
    FieldName: AnsiString;
    FieldType: AnsiChar;  // C = character, F = floating point, L = Logic, N = Number
  end;

  TBreakScreenObject = record
    BreakObject: boolean;
  end;

  TBoundaryName = class(TObject)
    Name: string;
    BoundaryType: TBoundaryType;
    FhbBoundaryType: TFhbBoundaryType;
  end;

  TfrmExportShapefileObjects = class(TfrmCustomSelectObjects)
    pnlTop: TPanel;
    lblObjects: TLabel;
    vstDataSets: TVirtualStringTree;
    lblDataArrays: TLabel;
    splLeft: TSplitter;
    BitBtn1: TBitBtn;
    rdeMissingData: TRbwDataEntry;
    lblMissingData: TLabel;
    gbExportAs: TGroupBox;
    rbPoints: TRadioButton;
    rbMultipoint: TRadioButton;
    rbPolyline: TRadioButton;
    rbPolygons: TRadioButton;
    sdShapefile: TSaveDialog;
    XBaseShapeFile: TXBase;
    cbExportName: TCheckBox;
    cbExportElevationFormulas: TCheckBox;
    chklstTimes: TCheckListBox;
    splRight: TSplitter;
    lblTimes: TLabel;
    rgView: TRadioGroup;
    btnToggleTimes: TButton;
    rgExportMethod: TRadioGroup;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure vstDataSetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure FormResize(Sender: TObject);
    procedure splLeftMoved(Sender: TObject);
    procedure vstDataSetsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstDataSetsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure vstObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnCloseClick(Sender: TObject);
    procedure splRightMoved(Sender: TObject);
    procedure rgViewClick(Sender: TObject);
    procedure btnToggleTimesClick(Sender: TObject);
  protected
    function ShouldCheckBoxBeChecked(ScreenObject: TScreenObject): boolean;
      override;
    procedure HandleChecked(AScreenObject: TScreenObject); override;
    procedure HandleUnchecked(AScreenObject: TScreenObject); override;
    function CanSelect(ScreenObject: TScreenObject): boolean; override;
  private
    FCurrentNodeName: string;
    FObjectOwner: TList;
    FSelectedDataSets: TList;
    FSelectedBoundaries: TBoundaryTypes;
    FSelectedScreenObjects: TList;
    FSettingChecked: Boolean;
    FFieldDefinitions: array of TFieldDefinition;
    FBreakScreenObjects: array of TBreakScreenObject;
    FShapeType: Integer;
    FMissingValueString: AnsiString;
    FMissingValue: Integer;
    FShowWarning: boolean;
    FShapeFileWriter: TShapefileGeometryWriter;
    FClassifiationList: TList;
    FEdgeList: TList;
    FBoundaryNames: TStringList;
    FBoundDataSetCount: Integer;
    FTimeCount: Integer;
    FTimeBoundaryFound: Boolean;
    FMaxHeadObsTimes: Integer;
    FFhbBoundaryTypes: TFhbBoundaryTypes;
    FFieldNames: TStringList;
    FCsvWriter: TStreamWriter;
    procedure GetData;
    procedure CenterLabels;
    procedure SetCheckedNodes(Sender: TBaseVirtualTree);
    procedure SetAllowableShapeTypes;
    procedure SetData;
    procedure DefinePointGeometry(AScreenObject: TScreenObject);
    procedure DefineMultipointGeometry(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure DefinePolylineGeometry(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure DefinePolygonGeometrySingleSection(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure DefinePolygonGeometryMultipleSections(
      AScreenObject: TScreenObject);
    procedure SetFieldType(DataArrayIndex: Integer);
    procedure AssignFieldName(FieldNames: TStringList; DataArrayIndex: Integer);
    procedure FillFieldDefinitions(FieldDefinitions: TStringList);
    procedure CreateDataBase(FieldDefinitions: TStringList);
    procedure CreateCsvFile(FieldNames: TStringList);
    procedure DefineShapeGeometry(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure InitializeDataBase;
    procedure GetShapeType(var ShapeType: Integer);
    procedure AssignFieldValues(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure InitializeBreakScreenObjects;
    function GetImportedValuesFromFormula(DataArray: TDataArray;
      ScreenObject: TScreenObject; Formula: string): TValueArrayStorage;
    procedure CreateShape(var Shape: TShapeObject);
    function GetExtraDataSetCount: Integer;
    function CanSelectBoundary(
      BoundaryClassification: TBoundaryClassification): Boolean;
    function CanSelectBoundaryType(Element: TBoundaryType): boolean;
    procedure EnableTimesCheckList;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses ClassificationUnit, PhastModelUnit, FastGEO,
  ConvexHullUnit, GPC_Classes, gpc, RbwParser, StrUtils,
  frmErrorsAndWarningsUnit, GIS_Functions, ModelMuseUtilities, frmGoPhastUnit,
  GoPhastTypes, ModflowTimeUnit,
  Generics.Collections, ModflowWellUnit, ModflowBoundaryUnit,
  ModflowGhbUnit, ModflowDrnUnit, ModflowDrtUnit, ModflowRivUnit,
  ModflowConstantHeadBoundaryUnit, ModflowEvtUnit, ModflowEtsUnit,
  ModflowRchUnit, ModflowUzfUnit, ModflowHfbUnit, ModflowHfbDisplayUnit,
  ModflowHobUnit, ModflowMnw2Unit, ModflowSfrUnit, ModflowSfrParamIcalcUnit,
  ModflowSfrReachUnit, ModflowSfrFlows, ModflowSfrChannelUnit,
  ModflowSfrEquationUnit, ModflowSfrSegment, ModflowSfrUnsatSegment,
  ModflowPackageSelectionUnit, ModflowMawUnit, ModflowSfr6Unit, ModflowStrUnit,
  Mt3dmsChemUnit, ModflowFhbUnit, System.IOUtils;

resourcestring
  StrDataSet0sOb = ' Data set = %0:s; Object = %1:s';
  StrAreYouSureYouWan = 'Are you sure you want to overwrite the existing Sha' +
  'pefile?';
  StrYouMustSelectOne = 'You must select one or more times at which you wish' +
  ' the MODFLOW Features to be exported.';
  StrYouMustSelectObject = 'You must select at least one object to export.';
  StrYouMustSelectAtL = 'You must select at least one object on the %s';
  StrThereWasAnErrorI = 'There was an error in opening the database file ass' +
  'ociated with this Shapefile. The error message was "%s"  If the Shapefile' +
  ' is open in another program, try closing the other program. If the error ' +
  'was about the size of the size of the records being to large, try ' +
  'exporting the data as a CSV file rather than Shapefile attributes.';
  StrTheFileSizeOfThe2GB = 'The file size of the dBASE file (%g) for this Sh' +
  'apefile exceeds 2 GB. It may not be useable in some software. Do you want' +
  ' to continue?';

const
  StrFormulaTruncatedTo = 'Formula truncated to 254 characters';

type
  TPointDirection = (pdIncrement, pdDecrement);

  { TfrmExportShapefileObjects }

procedure TfrmExportShapefileObjects.btnCloseClick(Sender: TObject);
var
  TimeIndex: Integer;
  TimesSelected: Boolean;
  BoundIndex: Integer;
  BoundaryName: TBoundaryName;
  TimeBoundaryUsed: Boolean;
begin
  inherited;
  if FSelectedScreenObjects.Count = 0 then
  begin
    Beep;
    MessageDlg(StrYouMustSelectObject, mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;

  if FBoundaryNames.Count > 0 then
  begin
    for BoundIndex := 0 to FBoundaryNames.Count - 1 do
    begin
      BoundaryName := FBoundaryNames.Objects[BoundIndex] as TBoundaryName;
      TimeBoundaryUsed := False;
      if not (BoundaryName.BoundaryType in [btMfHfb, btMfMnw, btMfObs]) then
      begin
        TimeBoundaryUsed := True;
      end;
      if BoundaryName.BoundaryType = btMfMnw then
      begin
        if BoundaryName.Name = StrMnw2PumpingRate then
        begin
          TimeBoundaryUsed := True;
        end
        else if BoundaryName.Name = StrMnw2HeadCapacityMultip then
        begin
          TimeBoundaryUsed := True;
        end
        else if BoundaryName.Name = StrMnw2LimitingWaterLevel then
        begin
          TimeBoundaryUsed := True;
        end
        else if BoundaryName.Name = StrMnw2InactivationPumping then
        begin
          TimeBoundaryUsed := True;
        end
        else if BoundaryName.Name = StrMnw2ReactivationPumping then
        begin
          TimeBoundaryUsed := True;
        end
      end;
      if TimeBoundaryUsed then
      begin
        TimesSelected := False;
        for TimeIndex := 0 to chklstTimes.Count - 1 do
        begin
          if chklstTimes.Checked[TimeIndex] then
          begin
            TimesSelected := True;
            Break;
          end;
        end;
        if not TimesSelected then
        begin
          Beep;
          MessageDlg(StrYouMustSelectOne, mtError, [mbOK], 0);
          ModalResult := mrNone;
          Exit;
        end;
        Break;
      end;
    end;
  end;

  if sdShapefile.Execute then
  begin
    if FileExists(sdShapefile.FileName)
      or FileExists(ChangeFileExt(sdShapefile.FileName, '.shx'))
      or FileExists(ChangeFileExt(sdShapefile.FileName, '.dbf')) then
    begin
      if MessageDlg(StrAreYouSureYouWan,
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        ModalResult := mrNone;
        Exit;
      end;
    end;
    SetData
  end
  else
  begin
    ModalResult := mrNone;
  end;
end;

procedure TfrmExportShapefileObjects.btnToggleTimesClick(Sender: TObject);
var
  TimeIndex: Integer;
begin
  inherited;
  for TimeIndex := 0 to chklstTimes.Items.Count - 1 do
  begin
    chklstTimes.Checked[TimeIndex] := not chklstTimes.Checked[TimeIndex];
  end;
end;

function TfrmExportShapefileObjects.CanSelectBoundary(BoundaryClassification: TBoundaryClassification): Boolean;
var
  SfrPackage: TSfrPackageSelection;
  StrPackage: TStrPackageSelection;
begin
  result := CanSelectBoundaryType(BoundaryClassification.BoundaryType);
  if result and (BoundaryClassification.BoundaryType = btMfSfr) then
  begin
    SfrPackage := frmGoPhast.PhastModel.ModflowPackages.SfrPackage;
    if BoundaryClassification.ClassificationName = StrModflowSfrReach then
    begin
      result := False;
    end
    else if (BoundaryClassification.ClassificationName = StrModflowSfrStreamTop)
      or (BoundaryClassification.ClassificationName = StrModflowSfrStreamSlope)
      or (BoundaryClassification.ClassificationName = StrModflowSfrStreamThickness)
      or (BoundaryClassification.ClassificationName = StrModflowSfrStreamK)
      then
    begin
      Result := SfrPackage.ModflowSfrSpatialVariationSelected(nil)
    end
    else if (BoundaryClassification.ClassificationName = StrModflowSfrSatWatCont)
      or (BoundaryClassification.ClassificationName = StrModflowSfrInitWatCont)
      or (BoundaryClassification.ClassificationName = StrModflowSfrBrooksCorey)
      then
    begin
      Result := SfrPackage.ModflowSfrUnsatSpatialVariationSelected(nil)
    end
    else if (BoundaryClassification.ClassificationName = StrModflowSfrVertK)
      then
    begin
      Result := SfrPackage.ModflowSfrUnsatKzSpatialVariationSelected(nil)
    end
    else if (BoundaryClassification.ClassificationName = StrModflowSfrUpstreamHydraulicConductivity)
      or (BoundaryClassification.ClassificationName = StrModflowSfrDownstreamHydraulicConductivity)
      or (BoundaryClassification.ClassificationName = StrModflowSfrUpstreamThickness)
      or (BoundaryClassification.ClassificationName = StrModflowSfrDownstreamThickness)
      or (BoundaryClassification.ClassificationName = StrModflowSfrUpstreamElevation)
      or (BoundaryClassification.ClassificationName = StrModflowSfrDownstreamElevation)
      then
    begin
      Result := SfrPackage.ModflowSfrUpstreamDownstreamUsed(nil)
    end
    else if (BoundaryClassification.ClassificationName = StrModflowSfrUpstreamSaturatedWaterContent)
      or (BoundaryClassification.ClassificationName = StrModflowSfrDownstreamSaturatedWaterContent)
      or (BoundaryClassification.ClassificationName = StrModflowSfrUpstreamInitialUnsaturatedWaterContent)
      or (BoundaryClassification.ClassificationName = StrModflowSfrDownstreamInitialUnsaturatedWaterContent)
      or (BoundaryClassification.ClassificationName = StrModflowSfrUpstreamBrooksCoreyExponent)
      or (BoundaryClassification.ClassificationName = StrModflowSfrDownstreamBrooksCoreyExponent)
      then
    begin
      Result := SfrPackage.ModflowSfrUpstreamDownstreamUnsatUsed(nil)
    end
    else if (BoundaryClassification.ClassificationName = StrModflowSfrUpstreamMaxUnsaturatedKz)
      or (BoundaryClassification.ClassificationName = StrModflowSfrDownstreamMaxUnsaturatedKz)
      then
    begin
      Result := SfrPackage.ModflowSfrUpstreamDownstreamUnsatKzUsed(nil)
    end
  end;
  if result and (BoundaryClassification.BoundaryType = btMfStr) then
  begin
    StrPackage := frmGoPhast.PhastModel.ModflowPackages.StrPackage;
    if BoundaryClassification.ClassificationName = StrModflowStrReach then
    begin
      result := False;
    end
    else if BoundaryClassification.ClassificationName = StrSTRStreamStage then
    begin
      Result := StrPackage.StageUsed(nil)
    end
  end;
  if result and (BoundaryClassification.BoundaryType = btSfr_MF6) then
  begin
    if BoundaryClassification.ClassificationName = StrSFR6ReachNumber then
    begin
      result := False;
    end
  end;
end;


function TfrmExportShapefileObjects.CanSelectBoundaryType(Element: TBoundaryType): boolean;
begin
  Result := False;
  case Element of
    btUndefined: ;
    btPhastSpecifiedHead:
      begin
          result := False;
      end;
    btPhastFlux:
      begin
          result := False;
      end;
    btPhastLeaky:
      begin
          result := False;
      end;
    btPhastRiver:
      begin
          result := False;
      end;
    btPhastWell:
      begin
          result := False;
      end;
    btMfWell:
      begin
          result := True;
      end;
    btMfGhb:
      begin
          result := True;
      end;
    btMfDrn:
      begin
          result := True;
      end;
    btMfDrt:
      begin
          result := True;
      end;
    btMfRiv:
      begin
          result := True;
      end;
    btMfChd:
      begin
          result := True;
      end;
    btMfEts:
      begin
          result := True;
      end;
    btMfEt:
      begin
          result := True;
      end;
    btMfRch:
      begin
          result := True;
      end;
    btMfSfr:
      begin
//          result := False;
          result := True;
      end;
    btMfStr:
      begin
          result := True;
      end;
    btMfUzf:
      begin
          result := True;
      end;
    btMfObs:
      begin
          result := True;
      end;
    btMfMnw:
      begin
          result := True;
      end;
    btMt3dSsm:
      begin
          result := True;
      end;
    btMfHfb:
      begin
          result := True;
      end;
    btMtmsObs:
      begin
        result := False;
      end;
    btSfr_MF6:
      begin
          result := True;
      end;
    btMAW:
      begin
          result := True;
      end;
    btMfFhb:
      begin
          result := True;
      end;
  end;
end;

procedure TfrmExportShapefileObjects.EnableTimesCheckList;
var
  TempBoundaries: TBoundaryTypes;
begin
  TempBoundaries := FSelectedBoundaries;
  Exclude(TempBoundaries, btMfObs);
  Exclude(TempBoundaries, btMtmsObs);
  chklstTimes.Enabled := TempBoundaries <> [];
end;

function TfrmExportShapefileObjects.CanSelect(
  ScreenObject: TScreenObject): boolean;
var
  Index: Integer;
  DataArray: TDataArray;
  Element: TBoundaryType;
begin
  result := False;
  if ScreenObject.ViewDirection <> TViewDirection(rgView.ItemIndex) then
  begin
    Exit;
  end;
  for Index := 0 to FSelectedDataSets.Count - 1 do
  begin
    DataArray := FSelectedDataSets[Index];
    if ScreenObject.IndexOfDataSet(DataArray) >= 0 then
    begin
      result := True;
      Exit;
    end;
  end;
  for Element in FSelectedBoundaries do
  begin
    case Element of
      btUndefined: ;
      btPhastSpecifiedHead:
        begin
  //          if ScreenObject.StoreSpecifiedHead then
          begin
            result := False;
  //          Exit;
          end;
        end;
      btPhastFlux:
        begin
  //          if ScreenObject.StoreFlux then
          begin
            result := False;
  //          Exit;
          end;
        end;
      btPhastLeaky:
        begin
  //          if ScreenObject.StoreLeaky then
          begin
            result := False;
  //          Exit;
          end;
        end;
      btPhastRiver:
        begin
  //          if ScreenObject.StoreRiver then
          begin
            result := False;
  //          Exit;
          end;
        end;
      btPhastWell:
        begin
  //          if ScreenObject.StoreWell then
          begin
            result := False;
  //          Exit;
          end;
        end;
      btMfWell:
        begin
          if ScreenObject.StoreModflowWellBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfGhb:
        begin
          if ScreenObject.StoreModflowGhbBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfDrn:
        begin
          if ScreenObject.StoreModflowDrnBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfDrt:
        begin
          if ScreenObject.StoreModflowDrtBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfRiv:
        begin
          if ScreenObject.StoreModflowRivBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfChd:
        begin
          if ScreenObject.StoreModflowChdBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfEts:
        begin
          if ScreenObject.StoreModflowEtsBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfEt:
        begin
          if ScreenObject.StoreModflowEvtBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfRch:
        begin
          if ScreenObject.StoreModflowRchBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfSfr:
        begin
//            result := False;
            result := True;
        end;
      btMfStr:
        begin
          if ScreenObject.StoreModflowStrBoundary then
          begin
            result := True;
          end;
        end;
      btMfUzf:
        begin
          if ScreenObject.StoreModflowUzfBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfObs:
        begin
  //          if ScreenObject.StoreModflowHeadObservations then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfMnw:
        begin
  //          if ScreenObject.StoreModflowMnw2Boundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMt3dSsm:
        begin
  //          if ScreenObject.StoreMt3dmsConcBoundary then
          begin
            result := True;
  //          Exit;
          end;
        end;
      btMfHfb:
        begin
          if ScreenObject.StoreModflowHfbBoundary then
          begin
            result := True;
          end;
        end;
      btSfr_MF6:
        begin
          if ScreenObject.StoreModflowSfr6Boundary then
          begin
            result := True;
          end;
        end;
      btMAW:
        begin
          if ScreenObject.StoreModflowMawBoundary then
          begin
            result := True;
          end;
        end;
      btMfFhb:
        begin
          if ScreenObject.StoreModflowFhbFlowBoundary and (fbtFlow in FFhbBoundaryTypes) then
          begin
            result := True;
          end;
          if ScreenObject.StoreModflowFhbHeadBoundary and (fbtHead in FFhbBoundaryTypes) then
          begin
            result := True;
          end;
        end;
    end;
    if result then
    begin
      Exit;
    end;
  end;

{
  TBoundaryType = (btUndefined, btPhastSpecifiedHead, btPhastFlux, btPhastLeaky,
    btPhastRiver, btPhastWell, btMfWell, btMfGhb, btMfDrn, btMfDrt, btMfRiv,
    btMfChd, btMfEts, btMfEt, btMfRch, btMfSfr, btMfStr, btMfUzf, btMfObs,
    btMfMnw, btMt3dSsm, btMfHfb, btSutraSpecifiedPressure, btSutraSpecifiedHead,
    btSutraSpecConcTemp, btSutraFluidFlux, btMassEnergyFlux, btSutraGeneralFlow,
    btSutraGenTransp, btMfFhb, btCFP, btMfFarm, btSWR, btMnw1, btMtmsObs, btRIP,
    btMt3dRchConc, mt3dUnsatConc, mt3dSatConc, btSfr_MF6, btMAW);

StoreModflowStrBoundary
}

end;

procedure TfrmExportShapefileObjects.CenterLabels;
begin
  lblObjects.Left := vstObjects.Left
    + (vstObjects.Width - lblObjects.Width) div 2;
  lblDataArrays.Left := vstDataSets.Left
    + (vstDataSets.Width - lblDataArrays.Width) div 2;
  lblTimes.Left := chklstTimes.Left
    + (chklstTimes.Width - lblTimes.Width) div 2;
end;

procedure TfrmExportShapefileObjects.SetCheckedNodes(Sender: TBaseVirtualTree);
var
  AScreenObject: TScreenObject;
  Index: Integer;
  ChildNode: PVirtualNode;
  Data: PMyRec;
  ANode: PVirtualNode;
begin
  ANode := Sender.GetFirst;
  while ANode <> nil do
  begin
    Data := Sender.GetNodeData(ANode);
    if (Data.ScreenObjects <> nil) and (Data.ScreenObjects.Count > 0) then
    begin
      ChildNode := Sender.GetFirstChild(ANode);
      for Index := 0 to Data.ScreenObjects.Count - 1 do
      begin
        Assert(ChildNode <> nil);
        AScreenObject := Data.ScreenObjects[Index];
        if FSelectedScreenObjects.IndexOf(AScreenObject) >= 0 then
        begin
          Sender.CheckState[ChildNode] := csCheckedNormal;
        end
        else
        begin
          Sender.CheckState[ChildNode] := csUncheckedNormal;
        end;
        ChildNode := Sender.GetNextSibling(ChildNode);
      end;
    end;
    ANode := Sender.GetNext(ANode);
  end;
end;


procedure TfrmExportShapefileObjects.SetData;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  DBaseFileSize: Int64;
//  ShapeFileWriter: TShapefileGeometryWriter;
begin
  FShowWarning := False;
  try
    try
      InitializeDataBase;
    except
      on E: EFOpenError do
      begin
        Beep;
        MessageDlg(Format(StrThereWasAnErrorI, [E.message]), mtError, [mbOK], 0);
        Exit;
      end;
      on E: EXBaseException do
      begin
        MessageDlg(Format(StrThereWasAnErrorI, [E.message]), mtError, [mbOK], 0);
        Exit;
      end;
    end;

    if rgExportMethod.ItemIndex = 0 then
    begin
      DBaseFileSize := FSelectedScreenObjects.Count * XBaseShapeFile.RecordLength;
      if DBaseFileSize >= MaxInt then
      begin
        Beep;
        if MessageDlg(Format(StrTheFileSizeOfThe2GB, [DBaseFileSize]),
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end;

    frmErrorsAndWarnings.RemoveWarningGroup(frmGoPhast.PhastModel,
      StrFormulaTruncatedTo);

    GetShapeType(FShapeType);
    FMissingValueString := AnsiString(rdeMissingData.Text);
    FMissingValue := StrToInt(string(FMissingValueString));

    FShapeFileWriter := TShapefileGeometryWriter.Create(FShapeType, True);
    try
      for ObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
      begin
        AScreenObject := FSelectedScreenObjects[ObjectIndex];
        DefineShapeGeometry(AScreenObject,
          FBreakScreenObjects[ObjectIndex].BreakObject);
        try
          AssignFieldValues(AScreenObject,
            FBreakScreenObjects[ObjectIndex].BreakObject);
        except on E: EXBaseException do
          begin
            Beep;
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
          end;
        end;
      end;
      FShapeFileWriter.WriteToFile(sdShapefile.FileName,
        ChangeFileExt(sdShapefile.FileName, '.shx'));
    finally
      FShapeFileWriter.Free;
    end;
  finally
    if rgExportMethod.ItemIndex = 0 then
    begin
      XBaseShapeFile.Active := False;
    end;
  end;

  if FShowWarning then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

procedure TfrmExportShapefileObjects.DefinePointGeometry(
  AScreenObject: TScreenObject);
var
  APoint: TPoint2D;
  Shape: TShapeObject;
begin
  CreateShape(Shape);
  Shape.FNumPoints := 1;
  Shape.FNumParts := 0;
  SetLength(Shape.FPoints, 1);
  APoint := AScreenObject.Points[0];
  Shape.FPoints[0].X := APoint.x;
  Shape.FPoints[0].Y := APoint.y;
end;

procedure TfrmExportShapefileObjects.DefineMultipointGeometry(
  AScreenObject: TScreenObject; 
  BreakObject: boolean);
var
  APoint: TPoint2D;
  PointPosition: Integer;
  SectionIndex: Integer;
  SectionEnd: Integer;
  PointIndex: Integer;
  Shape: TShapeObject;
  PointCount: Integer;
begin
  if BreakObject then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      if AScreenObject.SectionClosed[SectionIndex] then
      begin
        SectionEnd := AScreenObject.SectionEnd[SectionIndex] - 1;
      end
      else
      begin
        SectionEnd := AScreenObject.SectionEnd[SectionIndex];
      end;
      CreateShape(Shape);
      PointCount := SectionEnd - AScreenObject.SectionStart[SectionIndex]+1;
      SetLength(Shape.FPoints, PointCount);
      SetLength(Shape.FParts, PointCount);
      PointPosition := 0;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to SectionEnd do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        Shape.FParts[PointPosition] := PointPosition;
        Inc(PointPosition);
      end;
      Shape.FNumPoints := PointPosition;
      Shape.FNumParts := PointPosition;
      SetLength(Shape.FPoints, PointPosition);
      SetLength(Shape.FParts, PointPosition);
    end;
  end
  else
  begin
    CreateShape(Shape);
    SetLength(Shape.FPoints, AScreenObject.Count);
    SetLength(Shape.FParts, AScreenObject.Count);
    PointPosition := 0;
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      if AScreenObject.SectionClosed[SectionIndex] then
      begin
        SectionEnd := AScreenObject.SectionEnd[SectionIndex] - 1;
      end
      else
      begin
        SectionEnd := AScreenObject.SectionEnd[SectionIndex];
      end;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to SectionEnd do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        Shape.FParts[PointPosition] := PointPosition;
        Inc(PointPosition);
      end;
    end;
    Shape.FNumPoints := PointPosition;
    Shape.FNumParts := PointPosition;
    SetLength(Shape.FPoints, PointPosition);
    SetLength(Shape.FParts, PointPosition);
  end;
end;

procedure TfrmExportShapefileObjects.DefinePolylineGeometry(
  AScreenObject: TScreenObject; BreakObject: boolean);
var
  PointPosition: Integer;
  SectionIndex: Integer;
  PointIndex: Integer;
  APoint: TPoint2D;
  FirstPointInSection: Boolean;
  Shape: TShapeObject;
begin
  if BreakObject then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      CreateShape(Shape);
      SetLength(Shape.FPoints, AScreenObject.SectionLength[SectionIndex]);
      Shape.FNumParts := 1;
      SetLength(Shape.FParts, 1);
      Shape.FParts[0] := 0;
      PointPosition := 0;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to
        AScreenObject.SectionEnd[SectionIndex] do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        Inc(PointPosition);
      end;
      Shape.FNumPoints := PointPosition;
      Assert(Length(Shape.FPoints) = PointPosition);
    end;
  end
  else
  begin
    CreateShape(Shape);
    SetLength(Shape.FPoints, AScreenObject.Count);
    PointPosition := 0;
    Shape.FNumParts := AScreenObject.SectionCount;
    SetLength(Shape.FParts, AScreenObject.SectionCount);
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      FirstPointInSection := True;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to
        AScreenObject.SectionEnd[SectionIndex] do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        if FirstPointInSection then
        begin
          Shape.FParts[SectionIndex] := PointPosition;
          FirstPointInSection := False;
        end;
        Inc(PointPosition);
      end;
    end;
    Shape.FNumPoints := PointPosition;
    Assert(Length(Shape.FPoints) = PointPosition);
  end;
end;

procedure TfrmExportShapefileObjects.DefinePolygonGeometrySingleSection(
  AScreenObject: TScreenObject;
  BreakObject: boolean);
var
  OutputPoints: TPolygon2D;
  APoint: TPoint2D;
  InputOrientation: Integer;
  InputPoints: TPolygon2D;
  PointIndex: Integer;
  PointPosition: Integer;
  CopyCount: Integer;
  Shape: TShapeObject;
  SectionIndex : integer;
begin
  if BreakObject then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      CreateShape(Shape);

      CopyCount := AScreenObject.SectionLength[SectionIndex] - 1;
      SetLength(InputPoints, CopyCount);

      AScreenObject.CopyPoints(InputPoints, 0,
        AScreenObject.SectionStart[SectionIndex], CopyCount);
      ConvexHull2(InputPoints, InputOrientation, OutputPoints);
//      PointPosition := 0;
      if InputOrientation = Clockwise then
      begin
        PointPosition := 0;
      end
      else
      begin
        PointPosition := AScreenObject.SectionLength[SectionIndex] - 1;
        Assert(InputOrientation = CounterClockwise);
      end;

      Shape.FNumPoints := AScreenObject.SectionLength[SectionIndex];
      SetLength(Shape.FPoints, AScreenObject.SectionLength[SectionIndex]);
      Shape.FNumParts := 1;
      SetLength(Shape.FParts, 1);
      Shape.FParts[0] := 0;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to
        AScreenObject.SectionEnd[SectionIndex] do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        if InputOrientation = Clockwise then
        begin
          Inc(PointPosition);
        end
        else
        begin
          Dec(PointPosition);
        end;
      end;
//      Assert(Length(Shape.FPoints) = PointPosition);
    end;
  end
  else
  begin
    CreateShape(Shape);
    SetLength(InputPoints, AScreenObject.Count - 1);
    CopyCount := AScreenObject.Count - 1;
    AScreenObject.CopyPoints(InputPoints, 0, 0, CopyCount);
    ConvexHull2(InputPoints, InputOrientation, OutputPoints);
    if InputOrientation = Clockwise then
    begin
      PointPosition := 0;
    end
    else
    begin
      Assert(InputOrientation = CounterClockwise);
      PointPosition := AScreenObject.Count - 1;
    end;
    SetLength(Shape.FPoints, AScreenObject.Count);
    Shape.FNumParts := 1;
    SetLength(Shape.FParts, 1);
    Shape.FParts[0] := 0;
    for PointIndex := 0 to AScreenObject.Count - 1 do
    begin
      APoint := AScreenObject.Points[PointIndex];
      Shape.FPoints[PointPosition].X := APoint.x;
      Shape.FPoints[PointPosition].Y := APoint.y;
      if InputOrientation = Clockwise then
      begin
        Inc(PointPosition);
      end
      else
      begin
        Dec(PointPosition);
      end;
    end;
    Shape.FNumPoints := AScreenObject.Count;
  end;
end;

procedure TfrmExportShapefileObjects.DefinePolygonGeometryMultipleSections(
  AScreenObject: TScreenObject);
var
  ShapePoint: TShapePoint;
  PointDirection: TPointDirection;
  OutputPoints: TPolygon2D;
  InputDirection: Integer;
  InputPoints: TPolygon2D;
  SectionPosition: Integer;
  IntersectPolygon: TGpcPolygonClass;
  GpcPoint: Tgpc_vertex;
  APoint: TPoint2D;
  PointIndex: Integer;
  PointPosition: Integer;
  SectionIndex: Integer;
  GpcPolygon: TGpcPolygonClass;
  EmptyPolygon: TGpcPolygonClass;
  Shape: TShapeObject;
begin
  CreateShape(Shape);
  EmptyPolygon := TGpcPolygonClass.Create;
  GpcPolygon := TGpcPolygonClass.Create;
  try
    GpcPolygon.NumberOfContours := AScreenObject.SectionCount;
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      PointPosition := 0;
      GpcPolygon.VertexCount[SectionIndex] :=
        AScreenObject.SectionEnd[SectionIndex]
        - AScreenObject.SectionStart[SectionIndex];
      // Skip the last point in a section;
      // it is a duplicate of the first point.
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to
        AScreenObject.SectionEnd[SectionIndex] - 1 do
      begin
        APoint := AScreenObject.Points[PointIndex];
        GpcPoint.X := APoint.x;
        GpcPoint.Y := APoint.y;
        GpcPolygon.Vertices[SectionIndex, PointPosition] := GpcPoint;
        Inc(PointPosition);
      end;
    end;
    IntersectPolygon := TGpcPolygonClass.CreateFromOperation(
      GPC_DIFF, GpcPolygon, EmptyPolygon);
    try
      Shape.FNumPoints := IntersectPolygon.TotalVertexCount
        + IntersectPolygon.NumberOfContours;
      SetLength(Shape.FPoints, Shape.FNumPoints);
      Shape.FNumParts := IntersectPolygon.NumberOfContours;
      SetLength(Shape.FParts, IntersectPolygon.NumberOfContours);
      SectionPosition := 0;
      for SectionIndex := 0 to IntersectPolygon.NumberOfContours - 1 do
      begin
        SetLength(InputPoints, IntersectPolygon.VertexCount[SectionIndex]);
        for PointIndex := 0 to IntersectPolygon.VertexCount[SectionIndex] - 1 do
        begin
          GpcPoint := IntersectPolygon.Vertices[SectionIndex, PointIndex];
          InputPoints[PointIndex].x := GpcPoint.x;
          InputPoints[PointIndex].y := GpcPoint.y;
        end;
        ConvexHull2(InputPoints, InputDirection, OutputPoints);
//        PointDirection := pdIncrement;
        if IntersectPolygon.Holes[SectionIndex] then
        begin
          if InputDirection = ClockWise then
          begin
            PointDirection := pdDecrement;
          end
          else
          begin
            PointDirection := pdIncrement;
            Assert(InputDirection = counterClockWise);
          end;
        end
        else
        begin
          if InputDirection = ClockWise then
          begin
            PointDirection := pdIncrement;
          end
          else
          begin
            PointDirection := pdDecrement;
            Assert(InputDirection = counterClockWise);
          end;
        end;
        PointPosition := -1;
        case PointDirection of
          pdIncrement:
            PointPosition := 0;
          pdDecrement:
            PointPosition := IntersectPolygon.VertexCount[SectionIndex];
        else
          Assert(False);
        end;
        for PointIndex := 0 to IntersectPolygon.VertexCount[SectionIndex] - 1 do
        begin
          GpcPoint := IntersectPolygon.Vertices[SectionIndex, PointIndex];
          ShapePoint.X := GpcPoint.x;
          ShapePoint.Y := GpcPoint.y;
          Shape.FPoints[SectionPosition + PointPosition] := ShapePoint;
          case PointDirection of
            pdIncrement:
              Inc(PointPosition);
            pdDecrement:
              Dec(PointPosition);
          else
            Assert(False);
          end;
        end;
        GpcPoint := IntersectPolygon.Vertices[SectionIndex, 0];
        ShapePoint.X := GpcPoint.x;
        ShapePoint.Y := GpcPoint.y;
        Shape.FPoints[SectionPosition + PointPosition] := ShapePoint;
        Shape.FParts[SectionIndex] := SectionPosition;
        SectionPosition := SectionPosition
          + IntersectPolygon.VertexCount[SectionIndex] + 1;
      end;
    finally
      IntersectPolygon.Free;
    end;
  finally
    GpcPolygon.Free;
    EmptyPolygon.Free;
  end;
end;

procedure TfrmExportShapefileObjects.SetFieldType(DataArrayIndex: Integer);
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  DataArrayPosition: Integer;
  Formula: string;
  AValue: Double;
  AnInt: Integer;
  ImportedValues: TValueArrayStorage;
begin
  case FFieldDefinitions[DataArrayIndex].DataArray.DataType of
    rdtDouble:
      begin
        FFieldDefinitions[DataArrayIndex].FieldType := 'F';
      end;
    rdtInteger:
      begin
        FFieldDefinitions[DataArrayIndex].FieldType := 'N';
      end;
    rdtBoolean:
      begin
        FFieldDefinitions[DataArrayIndex].FieldType := 'N';
      end;
    rdtString:
      begin
        FFieldDefinitions[DataArrayIndex].FieldType := 'C';
      end;
  else
    Assert(False);
  end;
  if FFieldDefinitions[DataArrayIndex].DataArray.DataType <> rdtString then
  begin
    for ObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
    begin
      AScreenObject := FSelectedScreenObjects[ObjectIndex];
      DataArrayPosition := AScreenObject.IndexOfDataSet(
        FFieldDefinitions[DataArrayIndex].DataArray);
      if DataArrayPosition >= 0 then
      begin
        Formula := AScreenObject.DataSetFormulas[DataArrayPosition];
        if FBreakScreenObjects[ObjectIndex].BreakObject then
        begin
          ImportedValues := GetImportedValuesFromFormula(
            FFieldDefinitions[DataArrayIndex].DataArray,
            AScreenObject, Formula);
        end
        else
        begin
          ImportedValues := nil;
        end;
        if (ImportedValues = nil)
          or (ImportedValues.Count <> AScreenObject.SectionCount) then
        begin
          case FFieldDefinitions[DataArrayIndex].DataArray.DataType of
            rdtDouble:
              begin
                if not TryStrToFloat(Formula, AValue) then
                begin
                  FFieldDefinitions[DataArrayIndex].FieldType := 'C';
                  break;
                end;
              end;
            rdtInteger:
              begin
                if not TryStrToInt(Formula, AnInt) then
                begin
                  FFieldDefinitions[DataArrayIndex].FieldType := 'C';
                  break;
                end;
              end;
            rdtBoolean:
              begin
                if not SameText(Formula, 'True')
                  and not SameText(Formula, 'False') then
                begin
                  FFieldDefinitions[DataArrayIndex].FieldType := 'C';
                  break;
                end;
              end;
          else
            Assert(False);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmExportShapefileObjects.AssignFieldName(FieldNames: TStringList;
  DataArrayIndex: Integer);
var
  FieldName: AnsiString;
const
  MaximumFieldNameLength = 10;
begin
  if rgExportMethod.ItemIndex = 0 then
  begin
    FieldName := AnsiString(UpperCase(
      FFieldDefinitions[DataArrayIndex].DataArray.Name));
    if Length(FieldName) > MaximumFieldNameLength then
    begin
      SetLength(FieldName, MaximumFieldNameLength);
    end;
    FieldName := FixShapeFileFieldName(FieldName, FieldNames);
    FFieldDefinitions[DataArrayIndex].FieldName := FieldName;
    FieldNames.Add(string(FieldName));
  end
  else
  begin
    FFieldDefinitions[DataArrayIndex].FieldName :=
      AnsiString(FFieldDefinitions[DataArrayIndex].DataArray.Name);
    FieldNames.Add(FFieldDefinitions[DataArrayIndex].DataArray.Name);
  end;
end;

procedure TfrmExportShapefileObjects.FillFieldDefinitions(
  FieldDefinitions: TStringList);
var
  FieldDefinition: AnsiString;
  FieldIndex: Integer;
begin
  for FieldIndex := 0 to Length(FFieldDefinitions) - 1 do
  begin
    FieldDefinition := FFieldDefinitions[FieldIndex].FieldName + '=';
    case FFieldDefinitions[FieldIndex].FieldType of
      'C':
        begin
          FieldDefinition := FieldDefinition + 'C255';
        end;
      'F':
        begin
          FieldDefinition := FieldDefinition + 'N18,10';
        end;
      'L':
        begin
          FieldDefinition := FieldDefinition + 'N';
        end;
      'N':
        begin
          FieldDefinition := FieldDefinition + 'N';
        end;
    else
      Assert(False);
    end;
    FieldDefinitions.Add(string(FieldDefinition));
  end;
end;

procedure TfrmExportShapefileObjects.CreateCsvFile(FieldNames: TStringList);
var
  CsvFileName: string;
begin
  CsvFileName := ChangeFileExt(sdShapefile.FileName, '.csv');
  if FileExists(CsvFileName) then
  begin
    DeleteFile(CsvFileName);
  end;
  FCsvWriter := TFile.CreateText(CsvFileName);
  FCsvWriter.WriteLine(FieldNames.CommaText);
end;

procedure TfrmExportShapefileObjects.CreateDataBase(
  FieldDefinitions: TStringList);
var
  DataBaseFileName: string;
begin
  DataBaseFileName := ChangeFileExt(sdShapefile.FileName, '.dbf');
  if FileExists(DataBaseFileName) then
  begin
    DeleteFile(DataBaseFileName);
  end;
  XBaseShapeFile.DBFCreate(DataBaseFileName, FieldDefinitions);
  XBaseShapeFile.FileName := DataBaseFileName;
  XBaseShapeFile.Active := True;
  XBaseShapeFile.GotoBOF;
end;

procedure TfrmExportShapefileObjects.DefineShapeGeometry(
  AScreenObject: TScreenObject; 
  BreakObject: boolean);
begin
  case FShapeType of
    stPoint:
      begin
        Assert(not BreakObject);
        DefinePointGeometry(AScreenObject);
      end;
    stMultiPoint:
      begin
        DefineMultipointGeometry(AScreenObject, BreakObject);
      end;
    stPolyLine:
      begin
        DefinePolylineGeometry(AScreenObject, BreakObject);
      end;
    stPolygon:
      begin
        if BreakObject or (AScreenObject.SectionCount = 1) then
        begin
          DefinePolygonGeometrySingleSection(AScreenObject, BreakObject);
        end
        else
        begin
          DefinePolygonGeometryMultipleSections(AScreenObject);
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefileObjects.InitializeBreakScreenObjects;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  DataArrayIndex: Integer;
  DataArray: TDataArray;
  Position: integer;
  Formula: string;
  ImportedValues: TValueArrayStorage;
begin
  SetLength(FBreakScreenObjects, FSelectedScreenObjects.Count);
  for ScreenObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
  begin
    FBreakScreenObjects[ScreenObjectIndex].BreakObject := False;
  end;
  for ScreenObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
  begin
    ScreenObject := FSelectedScreenObjects[ScreenObjectIndex];
    if ScreenObject.SectionCount > 1 then
    begin
      for DataArrayIndex := 0 to FSelectedDataSets.Count - 1 do
      begin
        DataArray := FSelectedDataSets[DataArrayIndex];
        Position := ScreenObject.IndexOfDataSet(DataArray);
        if Position >= 0 then
        begin
          Formula := ScreenObject.DataSetFormulas[Position];
          ImportedValues := GetImportedValuesFromFormula(DataArray,
            ScreenObject, Formula);
          if (ImportedValues <> nil)
            and (ImportedValues.Count = ScreenObject.SectionCount) then
          begin
            FBreakScreenObjects[ScreenObjectIndex].BreakObject := True;
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TfrmExportShapefileObjects.GetImportedValuesFromFormula(
  DataArray: TDataArray; ScreenObject: TScreenObject;
  Formula: string): TValueArrayStorage;
var
  ImportedLength: Integer;
  ImportedPosition: Integer;
begin
  ImportedPosition := 0;
  ImportedLength := 0;
  case DataArray.DataType of
    rdtDouble:
      begin
        ImportedPosition := Pos(rsObjectImportedValuesR, Formula);
        if ImportedPosition <> 0 then
        begin
          ImportedLength := Length(rsObjectImportedValuesR);
        end
        else
        begin
          ImportedPosition := Pos(rsObjectImportedValuesI, Formula);
          if ImportedPosition <> 0 then
          begin
            ImportedLength := Length(rsObjectImportedValuesI);
          end;
        end;
      end;
    rdtInteger:
      begin
        ImportedPosition := Pos(rsObjectImportedValuesI, Formula);
        if ImportedPosition <> 0 then
        begin
          ImportedLength := Length(rsObjectImportedValuesI);
        end;
      end;
    rdtBoolean:
      begin
        ImportedPosition := Pos(rsObjectImportedValuesB, Formula);
        if ImportedPosition <> 0 then
        begin
          ImportedLength := Length(rsObjectImportedValuesB);
        end;
      end;
    rdtString:
      begin
        ImportedPosition := Pos(rsObjectImportedValuesT, Formula);
        if ImportedPosition <> 0 then
        begin
          ImportedLength := Length(rsObjectImportedValuesT);
        end;
      end;
  else
    Assert(False);
  end;
  result := nil;
  if ImportedPosition <> 0 then
  begin
    // Strip off the "ImportedValue?" and the quotes.
    Formula := Copy(Formula, ImportedLength + 1, MAXINT);
    if Length(Formula) >= 4 then
    begin
      if (Formula[1] = '(') and (Formula[Length(Formula)] = ')') then
      begin
        Formula := Copy(Formula, 2, Length(Formula) - 2);
        if (Formula[1] = '"') and (Formula[Length(Formula)] = '"') then
        begin
          Formula := Copy(Formula, 2, Length(Formula) - 2);
          result := ScreenObject.ImportedValues.ValuesByName(Formula);
        end;
      end;
    end;
  end;
end;

procedure TfrmExportShapefileObjects.CreateShape(var Shape: TShapeObject);
begin
  Shape := TShapeObject.Create;
  Shape.FShapeType := FShapeType;
  FShapeFileWriter.AddShape(Shape);
end;

function TfrmExportShapefileObjects.GetExtraDataSetCount: Integer;
begin
  result := 0;
  if cbExportName.Checked then
  begin
    result := 1;
  end;
  if cbExportElevationFormulas.Checked then
  begin
    Inc(result, 3);
  end;
end;

procedure TfrmExportShapefileObjects.InitializeDataBase;
const
  MaximumFieldNameLength = 10;
var
  FieldNames: TStringList;
  DataArrayIndex: Integer;
  DataArray: TDataArray;
  FieldDefinitions: TStringList;
  ExtraDataSetCount: integer;
  StartIndex: Integer;
  BoundIndex: Integer;
  BoundaryName: TBoundaryName;
  TimeIndex: Integer;
//  TimeBoundaryFound: Boolean;
  MaxLength: Integer;
  FieldName: string;
  HeadObsFound: Boolean;
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  HeadObservations: THobBoundary;
  HeadObsDataSetsCount: Integer;
  FieldType: AnsiChar;
begin
  FBoundDataSetCount := 0;
  FTimeBoundaryFound := False;
  HeadObsFound := False;
  if FBoundaryNames.Count > 0 then
  begin
    FTimeCount := 0;
    if chklstTimes.Enabled then
    begin
      for TimeIndex := 0 to chklstTimes.Items.Count - 1 do
      begin
        if chklstTimes.Checked[TimeIndex] then
        begin
          Inc(FTimeCount);
        end;
      end;
    end;

    for BoundIndex := 0 to FBoundaryNames.Count - 1 do
    begin
      BoundaryName := FBoundaryNames.Objects[BoundIndex] as TBoundaryName;
      if BoundaryName.BoundaryType = btMfObs then
      begin
        HeadObsFound := True;
      end
      else if BoundaryName.BoundaryType = btMfHfb then
      begin
        Inc(FBoundDataSetCount);
      end
      else if (BoundaryName.BoundaryType = btMfMnw)
        and (BoundaryName.Name <> StrMnw2PumpingRate)
        and (BoundaryName.Name <> StrMnw2HeadCapacityMultip)
        and (BoundaryName.Name <> StrMnw2LimitingWaterLevel)
        and (BoundaryName.Name <> StrMnw2InactivationPumping)
        and (BoundaryName.Name <> StrMnw2ReactivationPumping)
        then
      begin
        Inc(FBoundDataSetCount);
      end
      else
      begin
        if not FTimeBoundaryFound then
        begin
          Inc(FBoundDataSetCount,FTimeCount);
          FTimeBoundaryFound := True;
        end;
        Inc(FBoundDataSetCount,FTimeCount);
      end;
    end;
  end;

  FMaxHeadObsTimes := 0;
  HeadObsDataSetsCount := 0;
  if HeadObsFound then
  begin
    for ObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
    begin
      AScreenObject := FSelectedScreenObjects[ObjectIndex];
      HeadObservations :=
        AScreenObject.ModflowHeadObservations;
      if (HeadObservations <> nil)
        and (HeadObservations.Values.Count > FMaxHeadObsTimes) then
      begin
        FMaxHeadObsTimes := HeadObservations.Values.Count;
      end;
    end;
    if FMaxHeadObsTimes = 1 then
    begin
      HeadObsDataSetsCount := 6;
    end
    else
    begin
      HeadObsDataSetsCount := 3 + FMaxHeadObsTimes*4;
    end;
  end;
  Inc(FBoundDataSetCount, HeadObsDataSetsCount);

  FieldNames := TStringList.Create;
  try
    FieldNames.Sorted := True;
    FieldNames.CaseSensitive := False;
    InitializeBreakScreenObjects;
    ExtraDataSetCount := GetExtraDataSetCount;
    SetLength(FFieldDefinitions, FSelectedDataSets.Count
      + FBoundDataSetCount + ExtraDataSetCount);
    for DataArrayIndex := 0 to FSelectedDataSets.Count - 1 do
    begin
      DataArray := FSelectedDataSets[DataArrayIndex];
      FFieldDefinitions[DataArrayIndex].DataArray := DataArray;
      SetFieldType(DataArrayIndex);
      AssignFieldName(FieldNames, DataArrayIndex);
    end;
    StartIndex := FSelectedDataSets.Count;

    if FBoundaryNames.Count > 0 then
    begin
      if FTimeBoundaryFound then
      begin
        for TimeIndex := 1 to FTimeCount do
        begin
          FFieldDefinitions[StartIndex].DataArray := nil;
          FFieldDefinitions[StartIndex].FieldType := 'F';
          FFieldDefinitions[StartIndex].FieldName := 'TIME' + AnsiString(IntToStr(TimeIndex));
          FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
          Inc(StartIndex);
        end;
      end;
      MaxLength := MaximumFieldNameLength - Length(IntToStr(FTimeCount));

      for BoundIndex := 0 to FBoundaryNames.Count - 1 do
      begin
        BoundaryName := FBoundaryNames.Objects[BoundIndex] as TBoundaryName;
        FieldName := BoundaryName.Name;
        if FieldName = StrMAWWellElevation then
        begin
          FieldName := StrMAWWellElevationShort
        end
        else if FieldName = StrMAWWellConductance then
        begin
          FieldName := StrMAWWellConductanceShort
        end
        else if FieldName = StrMAWWellRedLength then
        begin
          FieldName := StrMAWWellRedLengthShort
        end;


        FieldName := StringReplace(FieldName,
          ' ', '', [rfReplaceAll, rfIgnoreCase]);
        if rgExportMethod.ItemIndex = 0 then
        begin
          FieldName := UpperCase(FieldName);
        end;
        if BoundaryName.BoundaryType = btMfObs then
        begin
          Continue;
        end
        else if BoundaryName.BoundaryType = btMfHfb then
        begin
          if rgExportMethod.ItemIndex = 0 then
          begin
            FieldName := Copy(FieldName, 1, MaximumFieldNameLength);
          end;
          FFieldDefinitions[StartIndex].DataArray := nil;
          FFieldDefinitions[StartIndex].FieldType := 'C';
          FFieldDefinitions[StartIndex].FieldName := AnsiString(FieldName);
          FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
          Inc(StartIndex);
        end
        else if (BoundaryName.BoundaryType = btMfMnw)
          and (BoundaryName.Name <> StrMnw2PumpingRate)
          and (BoundaryName.Name <> StrMnw2HeadCapacityMultip)
          and (BoundaryName.Name <> StrMnw2LimitingWaterLevel)
          and (BoundaryName.Name <> StrMnw2InactivationPumping)
          and (BoundaryName.Name <> StrMnw2ReactivationPumping)
          then
        begin
          if rgExportMethod.ItemIndex = 0 then
          begin
            FieldName := Copy(FieldName, 1, MaximumFieldNameLength);
          end;
          FFieldDefinitions[StartIndex].DataArray := nil;
          FFieldDefinitions[StartIndex].FieldType := 'C';
          FFieldDefinitions[StartIndex].FieldName := AnsiString(FieldName);
          FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
          Inc(StartIndex);
        end
        else if BoundaryName.BoundaryType = btMfSfr then
        begin
          if BoundaryName.Name = StrModflowSfrBankRoughness then
          begin
            FieldName := 'BANK_ROUGH';
          end
          else if BoundaryName.Name = StrModflowSfrReachLength then
          begin
            FieldName := 'REACH_LEN';
          end
          else if BoundaryName.Name = StrModflowSfrStreamTop then
          begin
            FieldName := 'BED_TOP';
          end
          else if BoundaryName.Name = StrModflowSfrStreamSlope then
          begin
            FieldName := 'SFR_SLOPE';
          end
          else if BoundaryName.Name = StrModflowSfrStreamThickness then
          begin
            FieldName := 'BED_THICK';
          end
          else if BoundaryName.Name = StrModflowSfrStreamK then
          begin
            FieldName := 'BED_KV';
          end
          else if BoundaryName.Name = StrModflowSfrSatWatCont then
          begin
            FieldName := 'SATWATCONT';
          end
          else if BoundaryName.Name = StrModflowSfrInitWatCont then
          begin
            FieldName := 'INIWATCONT';
          end
          else if BoundaryName.Name = StrModflowSfrBrooksCorey then
          begin
            FieldName := 'BR_COR_EXP';
          end
          else if BoundaryName.Name = StrModflowSfrVertK then
          begin
            FieldName := 'UNSAT_KV';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamSegment then
          begin
            FieldName := 'DOWN_SEG';
          end
//          else if BoundaryName.Name = StrModflowSfrDownstreamSegment then
//          begin
//            FieldName := 'DOWN_SEG';
//          end
          else if BoundaryName.Name = StrModflowSfrDiversionSegment then
          begin
            FieldName := 'DIV_SEG';
          end
          else if BoundaryName.Name = StrModflowSfrIprior then
          begin
            FieldName := 'DIV_PRIOR';
          end
          else if BoundaryName.Name = StrModflowSfrChannelRoughness then
          begin
            FieldName := 'CHAN_ROUGH';
          end
          else if BoundaryName.Name = StrModflowSfrBankRoughness then
          begin
            FieldName := 'BANK_ROUGH';
          end
          else if BoundaryName.Name = StrModflowSfrDepthCoefficient then
          begin
            FieldName := 'DEP_COEF';
          end
          else if BoundaryName.Name = StrModflowSfrDepthExponent then
          begin
            FieldName := 'DEP_EXP';
          end
          else if BoundaryName.Name = StrModflowSfrWidthCoefficient then
          begin
            FieldName := 'WID_COEF';
          end
          else if BoundaryName.Name = StrModflowSfrWidthExponent then
          begin
            FieldName := 'WID_EXP';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamHydraulicConductivity then
          begin
            FieldName := 'UP_KX';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamHydraulicConductivity then
          begin
            FieldName := 'DOWN_KX';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamWidth then
          begin
            FieldName := 'UP_WIDTH';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamWidth then
          begin
            FieldName := 'DOWN_WID';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamThickness then
          begin
            FieldName := 'UP_THICK';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamThickness then
          begin
            FieldName := 'DOWN_THICK';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamElevation then
          begin
            FieldName := 'UP_ELEV';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamElevation then
          begin
            FieldName := 'DOWN_ELEV';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamDepth then
          begin
            FieldName := 'UP_DEPTH';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamDepth then
          begin
            FieldName := 'DOWN_DEPTH';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamSaturatedWaterContent then
          begin
            FieldName := 'UP_SAT_CON';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamSaturatedWaterContent then
          begin
            FieldName := 'DO_SAT_CON';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamInitialUnsaturatedWaterContent then
          begin
            FieldName := 'UP_INI_CON';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamInitialUnsaturatedWaterContent then
          begin
            FieldName := 'DO_INI_CON';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamBrooksCoreyExponent then
          begin
            FieldName := 'UP_BRO_COR';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamBrooksCoreyExponent then
          begin
            FieldName := 'DO_BRO_COR';
          end
          else if BoundaryName.Name = StrModflowSfrUpstreamMaxUnsaturatedKz then
          begin
            FieldName := 'UP_UNSA_KZ';
          end
          else if BoundaryName.Name = StrModflowSfrDownstreamMaxUnsaturatedKz then
          begin
            FieldName := 'DO_UNSA_KZ';
          end;
          if rgExportMethod.ItemIndex = 0 then
          begin
            FieldName := Copy(FieldName, 1, MaxLength);
          end;
          for TimeIndex := 1 to FTimeCount do
          begin
            FFieldDefinitions[StartIndex].DataArray := nil;
            FFieldDefinitions[StartIndex].FieldType := 'C';
            FFieldDefinitions[StartIndex].FieldName := AnsiString(FieldName + IntToStr(TimeIndex));
            FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
            Inc(StartIndex);
          end;
        end
        else if BoundaryName.BoundaryType = btMfStr then
        begin
          FieldType := 'C';
          if BoundaryName.Name = StrModflowStrSegment then
          begin
            FieldName := 'SEGMENT';
            FieldType := 'N';
          end
          else if BoundaryName.Name = StrModflowStrDownstreamSegment then
          begin
            FieldName := 'DOWN_SEG';
            FieldType := 'N';
          end
          else if BoundaryName.Name = StrModflowStrDiversionSegment then
          begin
            FieldName := 'DIV_SEG';
            FieldType := 'N';
          end
          else if BoundaryName.Name = StrSTRStreamTopElev then
          begin
            FieldName := 'STR_TOP';
            FieldType := 'C';
          end
          else if BoundaryName.Name = StrSTRStreamBottomElev then
          begin
            FieldName := 'STR-BOT';
            FieldType := 'C';
          end
          else if BoundaryName.Name = StrSTRStreamStage then
          begin
            FieldName := 'STAGE';
            FieldType := 'C';
          end
          else if BoundaryName.Name = StrSTRStreamConductance then
          begin
            FieldName := 'STR_COND';
            FieldType := 'C';
          end
          else if BoundaryName.Name = StrSTRStreamFlow then
          begin
            FieldName := 'STR_IN';
            FieldType := 'C';
          end
          else if BoundaryName.Name = StrSTRStreamWidth then
          begin
            FieldName := 'WIDTH';
            FieldType := 'C';
          end
          else if BoundaryName.Name = StrSTRStreamSlope then
          begin
            FieldName := 'SLOPE';
            FieldType := 'C';
          end
          else if BoundaryName.Name = StrSTRStreamRoughness then
          begin
            FieldName := 'ROUGH';
            FieldType := 'C';
          end
          else
          begin
            Assert(False);
          end;
          for TimeIndex := 1 to FTimeCount do
          begin
            FFieldDefinitions[StartIndex].DataArray := nil;
            FFieldDefinitions[StartIndex].FieldType := FieldType;
            FFieldDefinitions[StartIndex].FieldName := AnsiString(FieldName + IntToStr(TimeIndex));
            FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
            Inc(StartIndex);
          end;
        end
//        else if BoundaryName.BoundaryType = btSfr_MF6 then
//        begin
//        end
        else
        begin
          if rgExportMethod.ItemIndex = 0 then
          begin
            FieldName := Copy(FieldName, 1, MaxLength);
          end;
          for TimeIndex := 1 to FTimeCount do
          begin
            FFieldDefinitions[StartIndex].DataArray := nil;
            FFieldDefinitions[StartIndex].FieldType := 'C';
            FFieldDefinitions[StartIndex].FieldName := AnsiString(FieldName + IntToStr(TimeIndex));
            FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
            Inc(StartIndex);
          end;
        end;
      end;
    end;

    if HeadObsFound then
    begin
      FFieldDefinitions[StartIndex].DataArray := nil;
      FFieldDefinitions[StartIndex].FieldType := 'C';
      FFieldDefinitions[StartIndex].FieldName := 'OBSNAME';
      FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
      Inc(StartIndex);

      FFieldDefinitions[StartIndex].DataArray := nil;
      FFieldDefinitions[StartIndex].FieldType := 'N';
      FFieldDefinitions[StartIndex].FieldName := 'OBSTYPE';
      FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
      Inc(StartIndex);

      if FMaxHeadObsTimes > 1 then
      begin
        FFieldDefinitions[StartIndex].DataArray := nil;
        FFieldDefinitions[StartIndex].FieldType := 'N';
        FFieldDefinitions[StartIndex].FieldName := 'ITT';
        FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
        Inc(StartIndex);
      end;

      for TimeIndex := 1 to FMaxHeadObsTimes do
      begin
        FFieldDefinitions[StartIndex].DataArray := nil;
        FFieldDefinitions[StartIndex].FieldType := 'F';
        FFieldDefinitions[StartIndex].FieldName := 'O_TIME' + AnsiString(IntToStr(TimeIndex));
        FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
        Inc(StartIndex);

        FFieldDefinitions[StartIndex].DataArray := nil;
        FFieldDefinitions[StartIndex].FieldType := 'F';
        FFieldDefinitions[StartIndex].FieldName := 'O_HEAD' + AnsiString(IntToStr(TimeIndex));
        FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
        Inc(StartIndex);

        FFieldDefinitions[StartIndex].DataArray := nil;
        FFieldDefinitions[StartIndex].FieldType := 'F';
        FFieldDefinitions[StartIndex].FieldName := 'O_STAT' + AnsiString(IntToStr(TimeIndex));
        FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
        Inc(StartIndex);

        FFieldDefinitions[StartIndex].DataArray := nil;
        FFieldDefinitions[StartIndex].FieldType := 'N';
        FFieldDefinitions[StartIndex].FieldName := 'O_SF' + AnsiString(IntToStr(TimeIndex));
        FieldNames.Add(string(FFieldDefinitions[StartIndex].FieldName));
        Inc(StartIndex);
      end;
    end;

    if cbExportName.Checked then
    begin
      FFieldDefinitions[StartIndex].DataArray := nil;
      FFieldDefinitions[StartIndex].FieldType := 'C';
      FFieldDefinitions[StartIndex].FieldName := 'OBJECTNAME';
      FieldNames.Add('OBJECTNAME');
      Inc(StartIndex);
    end;
    if cbExportElevationFormulas.Checked then
    begin
      FFieldDefinitions[StartIndex].DataArray := nil;
      FFieldDefinitions[StartIndex].FieldType := 'C';
      FFieldDefinitions[StartIndex].FieldName := 'Z_FORMULA';
      FieldNames.Add('Z_FORMULA');
      Inc(StartIndex);

      FFieldDefinitions[StartIndex].DataArray := nil;
      FFieldDefinitions[StartIndex].FieldType := 'C';
      FFieldDefinitions[StartIndex].FieldName := 'HIGH_Z';
      FieldNames.Add('HIGH_Z');
      Inc(StartIndex);

      FFieldDefinitions[StartIndex].DataArray := nil;
      FFieldDefinitions[StartIndex].FieldType := 'C';
      FFieldDefinitions[StartIndex].FieldName := 'LOW_Z';
      FieldNames.Add('LOW_Z');
//      Inc(StartIndex);
    end;

    // If anything else is added, StartIndex need to be incremented after the
    // last item above (LOW_Z).

    Assert(Length(FFieldDefinitions) = FieldNames.Count);
    if rgExportMethod.ItemIndex = 0 then
    begin
      FieldDefinitions := TStringList.Create;
      try
        FillFieldDefinitions(FieldDefinitions);
        CreateDataBase(FieldDefinitions);
      finally
        FieldDefinitions.Free;
      end;
    end
    else
    begin
      CreateCsvFile(FieldNames);
      FFieldNames.Assign(FieldNames);
      FFieldNames.caseSensitive := False;
    end;
  finally
    FieldNames.Free;
  end;
end;

procedure TfrmExportShapefileObjects.rgViewClick(Sender: TObject);
var
  ANode: PVirtualNode;
  Data: PClassificationNodeData;
begin
  inherited;

  ANode := vstDataSets.GetFirst;
  while ANode <> nil do
  begin
    Data := vstDataSets.GetNodeData(ANode);
    FCurrentNodeName := Data.ClassificationObject.ClassificationName;
    vstDataSetsChecked(vstDataSets,ANode);
    ANode := vstDataSets.GetNext(ANode)
  end;
end;

procedure TfrmExportShapefileObjects.GetShapeType(var ShapeType: Integer);
begin
  ShapeType := -1;
  if rbPoints.Checked then
  begin
    ShapeType := stPoint;
  end
  else if rbMultipoint.Checked then
  begin
    ShapeType := stMultiPoint;
  end
  else if rbPolyline.Checked then
  begin
    ShapeType := stPolyLine;
  end
  else if rbPolygons.Checked then
  begin
    ShapeType := stPolygon;
  end
  else
  begin
    Assert(False);
  end;
end;



procedure TfrmExportShapefileObjects.AssignFieldValues(
  AScreenObject: TScreenObject; BreakObject: boolean);
var
  IntValue: Integer;
  FloatValue: Double;
  Formula: AnsiString;
  APosition: Integer;
  DataArray: TDataArray;
  FieldIndex: Integer;
  SectionIndex: Integer;
  ImportedValues: TValueArrayStorage;
  ExtraDataSetCount: Integer;
  ExtraDataSetStart: Integer;
  StartIndex: Integer;
  SectionString: string;
  BoundaryDataSetStart: Integer;
  TimeList: TList<Double>;
  TimeIndex: Integer;
  FieldValues: TStringList;
  ALine: string;
  Procedure UpdFieldStr(FieldName, Data: AnsiString ) ;
  var
    FieldPosition: Integer;
  begin
    if rgExportMethod.ItemIndex = 0 then
    begin
      XBaseShapeFile.UpdFieldStr(FieldName, Data)
    end
    else
    begin
      FieldPosition := FFieldNames.IndexOf(string(FieldName));
      Assert(FieldPosition >= 0);
      FieldValues[FieldPosition] := string(Data);
    end;
  end;
  Procedure UpdFieldNum(FieldName : WString ; Data: Extended );
  var
    FieldPosition: Integer;
  begin
    if rgExportMethod.ItemIndex = 0 then
    begin
      XBaseShapeFile.UpdFieldNum(FieldName, Data);
    end
    else
    begin
      FieldPosition := FFieldNames.IndexOf(string(FieldName));
      Assert(FieldPosition >= 0);
      FieldValues[FieldPosition] := FortranFloatToStr(Data);
    end;
  end;
  Procedure UpdFieldInt(FieldName : AnsiString ; Data: Integer ) ;
  var
    FieldPosition: Integer;
  begin
    if rgExportMethod.ItemIndex = 0 then
    begin
      XBaseShapeFile.UpdFieldInt(FieldName, Data);
    end
    else
    begin
      FieldPosition := FFieldNames.IndexOf(string(FieldName));
      Assert(FieldPosition >= 0);
      FieldValues[FieldPosition] := IntToStr(Data);
    end;
  end;
  procedure AssignBoundaryData;
  var
    TimeIndex: Integer;
    BoundIndex: Integer;
    BoundaryName: TBoundaryName;
    MfWellBoundary: TMfWellBoundary;
    PValues: TModflowParamItem;
    Values: TCustomMF_BoundColl;
    WellTimeItem: TWellItem;
    GhbBoundary: TGhbBoundary;
    GhbTimeItem: TGhbItem;
    DrnBoundary: TDrnBoundary;
    DrnTimeItem: TDrnItem;
    DrtBoundary: TDrtBoundary;
    DrtTimeItem: TDrtItem;
    RivTimeItem: TRivItem;
    RivBoundary: TRivBoundary;
    ChdBoundary: TChdBoundary;
    ChdTimeItem: TChdItem;
    EtsBoundary: TEtsBoundary;
    EtTimeItem: TEvtItem;
    EtsSurfTimeItem: TEtsSurfDepthItem;
    FractionIndex: Integer;
    FormulaItem: TEtsStringValueItem;
    EtLayerItem: TEvtLayerItem;
    EvtBoundary: TEvtBoundary;
    RchBoundary: TRchBoundary;
    RchTimeItem: TRchItem;
    RchLayerItem: TRchLayerItem;
    UzfBoundary: TUzfBoundary;
    UzfExtinctDepthItem: TUzfExtinctDepthItem;
    UzfWaterContentItem: TUzfWaterContentItem;
    HfbBoundary: THfbBoundary;
    HeadObservations: THobBoundary;
    HobItem: THobItem;
    Mnw2Boundary: TMnw2Boundary;
    MnwSpatialItem: TMnw2SpatialItem;
    Mnw2TimeValues: TMnw2TimeCollection;
    Mnw2TimeItem: TMnw2TimeItem;
    Mnw2TimeItemIndex: integer;
    SfrBoundary: TSfrBoundary;
    ParamIcalc: TSfrParamIcalcCollection;
    ParamIcalcTimeItem: TSfrParamIcalcItem;
    SfrValues: TSfrCollection;
    SfrTimeItem: TSfrItem;
    SegmentFlows: TSfrSegmentFlowCollection;
    SegmentFlowItem: TSfrSegmentFlowItem;
    ChannelValues: TSfrChannelCollection;
    ChannelItem: TSfrChannelItem;
    EquationValues: TSfrEquationCollection;
    EquationItem: TSfrEquationItem;
    UpstreamSegmentValues: TSfrSegmentCollection;
    SegmentItem: TSfrSegmentItem;
    DownstreamSegmentValues: TSfrSegmentCollection;
    UpstreamUnsatSegmentValues: TSfrUnsatSegmentCollection;
    UnsatSegmentItem: TSfrUnsatSegmentItem;
    DownstreamUnsatSegmentValues: TSfrUnsatSegmentCollection;
    StrBoundary: TStrBoundary;
    Sfr6Boundary: TSfrMf6Boundary;
    MawBoundary: TMawBoundary;
    StrItem: TStrItem;
    Sfr6Item: TSfrMf6Item;
    MawItem: TMawItem;
    SSmBoundry: TMt3dmsConcBoundary;
    Mt3dConcItem: TMt3dmsConcItem;
    FhbFlowBoundary: TFhbFlowBoundary;
    FhbTimeItem: TFhbItem;
    FhbHeadBoundary: TFhbHeadBoundary;
  begin
    StartIndex := BoundaryDataSetStart;
    if FTimeBoundaryFound then
    begin
      for TimeIndex := 0 to TimeList.Count - 1 do
      begin
        FloatValue := TimeList[TimeIndex];
        UpdFieldNum(
          FFieldDefinitions[StartIndex].FieldName, FloatValue);
        Inc(StartIndex);
      end;
    end;
    for BoundIndex := 0 to FBoundaryNames.Count - 1 do
    begin
      BoundaryName := FBoundaryNames.Objects[BoundIndex] as TBoundaryName;
      case BoundaryName.BoundaryType of
        btUndefined: Assert(False);
        btPhastSpecifiedHead: ;
        btPhastFlux: ;
        btPhastLeaky: ;
        btPhastRiver: ;
        btPhastWell: ;
        btMfWell:
          begin
            MfWellBoundary := AScreenObject.ModflowWellBoundary;
            if MfWellBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              Values := nil;
              if MfWellBoundary.Values.Count > 0 then
              begin
                Values := MfWellBoundary.Values;
              end
              else if MfWellBoundary.Parameters.Count > 0 then
              begin
                PValues := MfWellBoundary.Parameters[0];
                Values := PValues.Param;
              end;
              Assert(Values.Count > 0);
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                WellTimeItem := Values.GetItemByStartTime(
                  TimeList[TimeIndex]) as TWellItem;
                if WellTimeItem = nil then
                begin
                  Formula := FMissingValueString;
                end
                else
                begin
                  Formula := AnsiString(WellTimeItem.PumpingRate);
                end;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end;
          end;
        btMfGhb:
          begin
            GhbBoundary := AScreenObject.ModflowGhbBoundary;
            if GhbBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              Values := nil;
              if GhbBoundary.Values.Count > 0 then
              begin
                Values := GhbBoundary.Values;
              end
              else if GhbBoundary.Parameters.Count > 0 then
              begin
                PValues := GhbBoundary.Parameters[0];
                Values := PValues.Param;
              end;
              Assert(Values.Count > 0);
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                GhbTimeItem := Values.GetItemByStartTime(
                  TimeList[TimeIndex]) as TGhbItem;
                if GhbTimeItem = nil then
                begin
                  Formula := FMissingValueString;
                end
                else
                begin
                  if BoundaryName.Name = StrMODFLOWGhbConductance then
                  begin
                    Formula := AnsiString(GhbTimeItem.Conductance);
                  end
                  else if BoundaryName.Name = StrMODFLOWGhbHead then
                  begin
                    Formula := AnsiString(GhbTimeItem.BoundaryHead);
                  end
                  else
                  begin
                    Assert(False);
                  end;
                end;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end;
          end;
        btMfDrn:
          begin
            DrnBoundary := AScreenObject.ModflowDrnBoundary;
            if DrnBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              Values := nil;
              if DrnBoundary.Values.Count > 0 then
              begin
                Values := DrnBoundary.Values;
              end
              else if DrnBoundary.Parameters.Count > 0 then
              begin
                PValues := DrnBoundary.Parameters[0];
                Values := PValues.Param;
              end;
              Assert(Values.Count > 0);
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                DrnTimeItem := Values.GetItemByStartTime(
                  TimeList[TimeIndex]) as TDrnItem;
                if DrnTimeItem = nil then
                begin
                  Formula := FMissingValueString;
                end
                else
                begin
                  if BoundaryName.Name = StrMODFLOWDrainConductance then
                  begin
                    Formula := AnsiString(DrnTimeItem.Conductance);
                  end
                  else if BoundaryName.Name = StrMODFLOWDrainElevation then
                  begin
                    Formula := AnsiString(DrnTimeItem.Elevation);
                  end
                  else
                  begin
                    Assert(False);
                  end;
                end;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end;
          end;
        btMfDrt:
          begin
            DrtBoundary := AScreenObject.ModflowDrtBoundary;
            if DrtBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              Values := nil;
              if DrtBoundary.Values.Count > 0 then
              begin
                Values := DrtBoundary.Values;
              end
              else if DrtBoundary.Parameters.Count > 0 then
              begin
                PValues := DrtBoundary.Parameters[0];
                Values := PValues.Param;
              end;
              Assert(Values.Count > 0);
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                DrtTimeItem := Values.GetItemByStartTime(
                  TimeList[TimeIndex]) as TDrtItem;
                if DrtTimeItem = nil then
                begin
                  Formula := FMissingValueString;
                end
                else
                begin
                  if BoundaryName.Name = StrMODFLOWDrainReturnConductance then
                  begin
                    Formula := AnsiString(DrtTimeItem.Conductance);
                  end
                  else if BoundaryName.Name = StrMODFLOWDrainReturnElevation then
                  begin
                    Formula := AnsiString(DrtTimeItem.Elevation);
                  end
                  else if BoundaryName.Name = StrMODFLOWDrainReturnFraction then
                  begin
                    Formula := AnsiString(DrtTimeItem.ReturnFraction);
                  end
                  else
                  begin
                    Assert(False);
                  end;
                end;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end;
          end;
        btMfRiv:
          begin
            RivBoundary := AScreenObject.ModflowRivBoundary;
            if RivBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              Values := nil;
              if RivBoundary.Values.Count > 0 then
              begin
                Values := RivBoundary.Values;
              end
              else if RivBoundary.Parameters.Count > 0 then
              begin
                PValues := RivBoundary.Parameters[0];
                Values := PValues.Param;
              end;
              Assert(Values.Count > 0);
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                RivTimeItem := Values.GetItemByStartTime(
                  TimeList[TimeIndex]) as TRivItem;
                if RivTimeItem = nil then
                begin
                  Formula := FMissingValueString;
                end
                else
                begin
                  if BoundaryName.Name = StrMODFLOWRiverConductance then
                  begin
                    Formula := AnsiString(RivTimeItem.Conductance);
                  end
                  else if BoundaryName.Name = StrMODFLOWRiverStage then
                  begin
                    Formula := AnsiString(RivTimeItem.RiverStage);
                  end
                  else if BoundaryName.Name = StrMODFLOWRiverBottom then
                  begin
                    Formula := AnsiString(RivTimeItem.RiverBottom);
                  end
                  else
                  begin
                    Assert(False);
                  end;
                end;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end;
          end;
        btMfChd:
          begin
            ChdBoundary := AScreenObject.ModflowChdBoundary;
            if ChdBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              Values := nil;
              if ChdBoundary.Values.Count > 0 then
              begin
                Values := ChdBoundary.Values;
              end
              else if ChdBoundary.Parameters.Count > 0 then
              begin
                PValues := ChdBoundary.Parameters[0];
                Values := PValues.Param;
              end;
              Assert(Values.Count > 0);
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                ChdTimeItem := Values.GetItemByStartTime(
                  TimeList[TimeIndex]) as TChdItem;
                if ChdTimeItem = nil then
                begin
                  Formula := FMissingValueString;
                end
                else
                begin
                  if BoundaryName.Name = StrMODFLOWCHDStartingHead then
                  begin
                    Formula := AnsiString(ChdTimeItem.StartHead);
                  end
                  else if BoundaryName.Name = StrMODFLOWCHDEndingHead then
                  begin
                    Formula := AnsiString(ChdTimeItem.EndHead);
                  end
                  else
                  begin
                    Assert(False);
                  end;
                end;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end;
          end;
        btMfEts:
          begin
            EtsBoundary := AScreenObject.ModflowEtsBoundary;
            if EtsBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              if BoundaryName.Name = StrMODFLOWEtsRate then
              begin
                Values := nil;
                if EtsBoundary.Values.Count > 0 then
                begin
                  Values := EtsBoundary.Values;
                end
                else if EtsBoundary.Parameters.Count > 0 then
                begin
                  PValues := EtsBoundary.Parameters[0];
                  Values := PValues.Param;
                end;
                Assert(Values.Count > 0);
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  EtTimeItem := Values.GetItemByStartTime(
                    TimeList[TimeIndex]) as TEvtItem;
                  if EtTimeItem = nil then
                  begin
                    Formula := FMissingValueString;
                  end
                  else
                  begin
                    Formula := AnsiString(EtTimeItem.EvapotranspirationRate);
                  end;
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end
              else if (BoundaryName.Name = StrMODFLOWEtsDepth)
                or (BoundaryName.Name = StrMODFLOWEtsSurface)
                or (Pos(StrMODFLOWEtsRateFraction, BoundaryName.Name) = 1)
                or (Pos(StrMODFLOWEtsDepthFraction, BoundaryName.Name) = 1) then
              begin
                Values := EtsBoundary.EtsSurfDepthCollection;
                Assert(Values.Count > 0);
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  EtsSurfTimeItem := Values.GetItemByStartTime(
                    TimeList[TimeIndex]) as TEtsSurfDepthItem;
                  if EtsSurfTimeItem = nil then
                  begin
                    Formula := FMissingValueString;
                  end
                  else
                  begin
                    if BoundaryName.Name = StrMODFLOWEtsDepth then
                    begin
                      Formula := AnsiString(EtsSurfTimeItem.EvapotranspirationDepth);
                    end
                    else if BoundaryName.Name = StrMODFLOWEtsSurface then
                    begin
                      Formula := AnsiString(EtsSurfTimeItem.EvapotranspirationSurface);
                    end
                    else if (Pos(StrMODFLOWEtsRateFraction, BoundaryName.Name) = 1) then
                    begin
                      FractionIndex := StrToInt(Copy(BoundaryName.Name,
                        Length(StrMODFLOWEtsRateFraction), MaxInt))-1;
                      FormulaItem := EtsSurfTimeItem.EtFractions.Items
                        [FractionIndex] as TEtsStringValueItem;
                      Formula := AnsiString(FormulaItem.Value);
                    end
                    else if (Pos(StrMODFLOWEtsDepthFraction, BoundaryName.Name) = 1) then
                    begin
                      FractionIndex := StrToInt(Copy(BoundaryName.Name,
                        Length(StrMODFLOWEtsDepthFraction), MaxInt))-1;
                      FormulaItem := EtsSurfTimeItem.DepthFractions.Items
                        [FractionIndex] as TEtsStringValueItem;
                      Formula := AnsiString(FormulaItem.Value);
                    end
                    else
                    begin
                      Assert(False);
                    end;
                  end;
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
              end
              else if (BoundaryName.Name = StrMODFLOWEtsLayer) then
              begin
                Values := EtsBoundary.EvapotranspirationLayers;
                if (Values.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    EtLayerItem := Values.GetItemByStartTime(
                      TimeList[TimeIndex]) as TEvtLayerItem;
                    if EtLayerItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(EtLayerItem.EvapotranspirationLayer);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end;
              end
              else
              begin
                Assert(False);
              end;
            end;
          end;
        btMfEt:
          begin
            EvtBoundary := AScreenObject.ModflowEvtBoundary;
            if EvtBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              if BoundaryName.Name = StrMODFLOWEvtRate then
              begin
                Values := nil;
                if EvtBoundary.Values.Count > 0 then
                begin
                  Values := EvtBoundary.Values;
                end
                else if EvtBoundary.Parameters.Count > 0 then
                begin
                  PValues := EvtBoundary.Parameters[0];
                  Values := PValues.Param;
                end;
                Assert(Values.Count > 0);
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  EtTimeItem := Values.GetItemByStartTime(
                    TimeList[TimeIndex]) as TEvtItem;
                  if EtTimeItem = nil then
                  begin
                    Formula := FMissingValueString;
                  end
                  else
                  begin
                    Formula := AnsiString(EtTimeItem.EvapotranspirationRate);
                  end;
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end
              else if (BoundaryName.Name = StrMODFLOWEvtDepth)
                or (BoundaryName.Name = StrMODFLOWEvtSurface) then
              begin
                Values := EvtBoundary.EvtSurfDepthCollection;
                Assert(Values.Count > 0);
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  EtsSurfTimeItem := Values.GetItemByStartTime(
                    TimeList[TimeIndex]) as TEtsSurfDepthItem;
                  if EtsSurfTimeItem = nil then
                  begin
                    Formula := FMissingValueString;
                  end
                  else
                  begin
                    if BoundaryName.Name = StrMODFLOWEvtDepth then
                    begin
                      Formula := AnsiString(EtsSurfTimeItem.EvapotranspirationDepth);
                    end
                    else if BoundaryName.Name = StrMODFLOWEvtSurface then
                    begin
                      Formula := AnsiString(EtsSurfTimeItem.EvapotranspirationSurface);
                    end
                    else
                    begin
                      Assert(False);
                    end;
                  end;
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
              end
              else if (BoundaryName.Name = StrMODFLOWEvtLayer) then
              begin
                Values := EvtBoundary.EvapotranspirationLayers;
                if (Values.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    EtLayerItem := Values.GetItemByStartTime(
                      TimeList[TimeIndex]) as TEvtLayerItem;
                    if EtLayerItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(EtLayerItem.EvapotranspirationLayer);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end;
              end
              else
              begin
                Assert(False);
              end;
            end;
          end;
        btMfRch:
          begin
            RchBoundary := AScreenObject.ModflowRchBoundary;
            if RchBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              if BoundaryName.Name = StrMODFLOWRchRate then
              begin
                Values := nil;
                if RchBoundary.Values.Count > 0 then
                begin
                  Values := RchBoundary.Values;
                end
                else if RchBoundary.Parameters.Count > 0 then
                begin
                  PValues := RchBoundary.Parameters[0];
                  Values := PValues.Param;
                end;
                Assert(Values.Count > 0);
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  RchTimeItem := Values.GetItemByStartTime(
                    TimeList[TimeIndex]) as TRchItem;
                  if RchTimeItem = nil then
                  begin
                    Formula := FMissingValueString;
                  end
                  else
                  begin
                    Formula := AnsiString(RchTimeItem.RechargeRate);
                  end;
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end
              else if (BoundaryName.Name = StrMODFLOWRchLayer) then
              begin
                Values := RchBoundary.RechargeLayers;
                if (Values.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    RchLayerItem := Values.GetItemByStartTime(
                      TimeList[TimeIndex]) as TRchLayerItem;
                    if RchLayerItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(RchLayerItem.RechargeLayer);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end;
              end
              else
              begin
                Assert(False);
              end;
            end;
          end;
        btMfSfr:
          begin
//            Assert(False);
            SfrBoundary := AScreenObject.ModflowSfrBoundary;
            if SfrBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              if BoundaryName.Name =  StrModflowSfrSegment then
              begin
                Formula := AnsiString(IntToStr(SfrBoundary.SegmentNumber));
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrReach then
              begin
                Assert(False);
              end
              else if BoundaryName.Name =  StrModflowSfrIcalc then
              begin
                ParamIcalc := SfrBoundary.ParamIcalc;
                if (ParamIcalc.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    ParamIcalcTimeItem := ParamIcalc.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrParamIcalcItem;
                    if ParamIcalcTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(IntToStr(ParamIcalcTimeItem.ICalc));
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrReachLength then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.ReachLength);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrStreamTop then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.StreambedElevation);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrStreamSlope then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.StreamSlope);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrStreamThickness then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.StreamBedThickness);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrStreamK then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.HydraulicConductivity);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrSatWatCont then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.SaturatedWaterContent);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrInitWatCont then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.InitialWaterContent);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrBrooksCorey then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.BrooksCoreyExponent);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrVertK then
              begin
                SfrValues := SfrBoundary.Values as TSfrCollection;
                if (SfrValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SfrTimeItem := SfrValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrItem;
                    if SfrTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(SfrTimeItem.VerticalK);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrDownstreamSegment then
              begin
                ParamIcalc := SfrBoundary.ParamIcalc;
                if (ParamIcalc.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    ParamIcalcTimeItem := ParamIcalc.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrParamIcalcItem;
                    if ParamIcalcTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(IntToStr(ParamIcalcTimeItem.OutflowSegment));
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrDiversionSegment then
              begin
                ParamIcalc := SfrBoundary.ParamIcalc;
                if (ParamIcalc.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    ParamIcalcTimeItem := ParamIcalc.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrParamIcalcItem;
                    if ParamIcalcTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(IntToStr(ParamIcalcTimeItem.DiversionSegment));
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name =  StrModflowSfrIprior then
              begin
                ParamIcalc := SfrBoundary.ParamIcalc;
                if (ParamIcalc.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    ParamIcalcTimeItem := ParamIcalc.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrParamIcalcItem;
                    if ParamIcalcTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(IntToStr(ParamIcalcTimeItem.IPRIOR));
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if (BoundaryName.Name =  StrModflowSfrFlow)
                or (BoundaryName.Name =  StrModflowSfrRunoff)
                or (BoundaryName.Name =  StrModflowSfrPrecipitation)
                or (BoundaryName.Name =  StrModflowSfrEvapotranspiration) then
              begin
                SegmentFlows := SfrBoundary.SegmentFlows;
                if (SegmentFlows.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SegmentFlowItem := SegmentFlows.GetItemByStartTime(
                      TimeList[TimeIndex]);
                    if SegmentFlowItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      if BoundaryName.Name = StrModflowSfrFlow then
                      begin
                        Formula := AnsiString(SegmentFlowItem.Flow);
                      end
                      else if BoundaryName.Name = StrModflowSfrRunoff then
                      begin
                        Formula := AnsiString(SegmentFlowItem.Runnoff);
                      end
                      else if BoundaryName.Name =  StrModflowSfrPrecipitation then
                      begin
                        Formula := AnsiString(SegmentFlowItem.Precipitation);
                      end
                      else if BoundaryName.Name =  StrModflowSfrEvapotranspiration then
                      begin
                        Formula := AnsiString(SegmentFlowItem.Evapotranspiration);
                      end
                      else
                      begin
                        Assert(False);
                      end;
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if (BoundaryName.Name = StrModflowSfrChannelRoughness)
                or (BoundaryName.Name = StrModflowSfrBankRoughness) then
              begin
                ChannelValues := SfrBoundary.ChannelValues;
                if (ChannelValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    ChannelItem := ChannelValues.GetItemByStartTime(
                      TimeList[TimeIndex]) as TSfrChannelItem;
                    if ChannelItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      if BoundaryName.Name = StrModflowSfrChannelRoughness then
                      begin
                        Formula := AnsiString(ChannelItem.ChannelRoughness);
                      end
                      else if BoundaryName.Name = StrModflowSfrBankRoughness then
                      begin
                        Formula := AnsiString(ChannelItem.BankRoughness);
                      end
                      else
                      begin
                        Assert(False);
                      end;
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if (BoundaryName.Name = StrModflowSfrDepthCoefficient)
                or (BoundaryName.Name = StrModflowSfrDepthExponent)
                or (BoundaryName.Name = StrModflowSfrWidthCoefficient)
                or (BoundaryName.Name = StrModflowSfrWidthExponent)
                then
              begin
                EquationValues := SfrBoundary.EquationValues;
                if (EquationValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    EquationItem := EquationValues.GetItemByStartTime(
                      TimeList[TimeIndex]);
                    if EquationItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      if (BoundaryName.Name = StrModflowSfrDepthCoefficient) then
                      begin
                        Formula := AnsiString(EquationItem.DepthCoefficient);
                      end
                      else if BoundaryName.Name = StrModflowSfrDepthExponent then
                      begin
                        Formula := AnsiString(EquationItem.DepthExponent);
                      end
                      else if BoundaryName.Name = StrModflowSfrWidthCoefficient then
                      begin
                        Formula := AnsiString(EquationItem.WidthCoefficient);
                      end
                      else if BoundaryName.Name = StrModflowSfrWidthExponent then
                      begin
                        Formula := AnsiString(EquationItem.WidthExponent);
                      end
                      else
                      begin
                        Assert(False);
                      end;
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if (BoundaryName.Name = StrModflowSfrUpstreamHydraulicConductivity)
                or (BoundaryName.Name = StrModflowSfrUpstreamWidth)
                or (BoundaryName.Name = StrModflowSfrUpstreamThickness)
                or (BoundaryName.Name = StrModflowSfrUpstreamElevation)
                or (BoundaryName.Name = StrModflowSfrUpstreamDepth)
                then
              begin
                UpstreamSegmentValues := SfrBoundary.UpstreamSegmentValues;
                if (UpstreamSegmentValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SegmentItem := UpstreamSegmentValues.GetItemByStartTime(
                      TimeList[TimeIndex]);
                    if SegmentItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      if (BoundaryName.Name = StrModflowSfrUpstreamHydraulicConductivity) then
                      begin
                        Formula := AnsiString(SegmentItem.HydraulicConductivity);
                      end
                      else if BoundaryName.Name = StrModflowSfrUpstreamWidth then
                      begin
                        Formula := AnsiString(SegmentItem.StreamWidth);
                      end
                      else if BoundaryName.Name = StrModflowSfrUpstreamThickness then
                      begin
                        Formula := AnsiString(SegmentItem.StreamBedThickness);
                      end
                      else if BoundaryName.Name = StrModflowSfrUpstreamElevation then
                      begin
                        Formula := AnsiString(SegmentItem.StreambedElevation);
                      end
                      else if BoundaryName.Name = StrModflowSfrUpstreamDepth then
                      begin
                        Formula := AnsiString(SegmentItem.StreamDepth);
                      end
                      else
                      begin
                        Assert(False);
                      end;
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if (BoundaryName.Name = StrModflowSfrDownstreamHydraulicConductivity)
                or (BoundaryName.Name = StrModflowSfrDownstreamWidth)
                or (BoundaryName.Name = StrModflowSfrDownstreamThickness)
                or (BoundaryName.Name = StrModflowSfrDownstreamElevation)
                or (BoundaryName.Name = StrModflowSfrDownstreamDepth)
                then
              begin
                DownstreamSegmentValues := SfrBoundary.DownstreamSegmentValues;
                if (DownstreamSegmentValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    SegmentItem := DownstreamSegmentValues.GetItemByStartTime(
                      TimeList[TimeIndex]);
                    if SegmentItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      if (BoundaryName.Name = StrModflowSfrDownstreamHydraulicConductivity) then
                      begin
                        Formula := AnsiString(SegmentItem.HydraulicConductivity);
                      end
                      else if BoundaryName.Name = StrModflowSfrDownstreamWidth then
                      begin
                        Formula := AnsiString(SegmentItem.StreamWidth);
                      end
                      else if BoundaryName.Name = StrModflowSfrDownstreamThickness then
                      begin
                        Formula := AnsiString(SegmentItem.StreamBedThickness);
                      end
                      else if BoundaryName.Name = StrModflowSfrDownstreamElevation then
                      begin
                        Formula := AnsiString(SegmentItem.StreambedElevation);
                      end
                      else if BoundaryName.Name = StrModflowSfrDownstreamDepth then
                      begin
                        Formula := AnsiString(SegmentItem.StreamDepth);
                      end
                      else
                      begin
                        Assert(False);
                      end;
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if (BoundaryName.Name = StrModflowSfrUpstreamSaturatedWaterContent)
                or (BoundaryName.Name = StrModflowSfrUpstreamInitialUnsaturatedWaterContent)
                or (BoundaryName.Name = StrModflowSfrUpstreamBrooksCoreyExponent)
                or (BoundaryName.Name = StrModflowSfrUpstreamMaxUnsaturatedKz)
                then
              begin
                UpstreamUnsatSegmentValues := SfrBoundary.UpstreamUnsatSegmentValues;
                if (UpstreamUnsatSegmentValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UnsatSegmentItem := UpstreamUnsatSegmentValues.GetItemByStartTime(
                      TimeList[TimeIndex]);
                    if UnsatSegmentItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      if (BoundaryName.Name = StrModflowSfrUpstreamSaturatedWaterContent) then
                      begin
                        Formula := AnsiString(UnsatSegmentItem.SaturatedWaterContent);
                      end
                      else if BoundaryName.Name = StrModflowSfrUpstreamInitialUnsaturatedWaterContent then
                      begin
                        Formula := AnsiString(UnsatSegmentItem.InitialWaterContent);
                      end
                      else if BoundaryName.Name = StrModflowSfrUpstreamBrooksCoreyExponent then
                      begin
                        Formula := AnsiString(UnsatSegmentItem.BrooksCoreyExponent);
                      end
                      else if BoundaryName.Name = StrModflowSfrUpstreamMaxUnsaturatedKz then
                      begin
                        Formula := AnsiString(UnsatSegmentItem.VerticalSaturatedK);
                      end
                      else
                      begin
                        Assert(False);
                      end;
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if (BoundaryName.Name = StrModflowSfrDownstreamSaturatedWaterContent)
                or (BoundaryName.Name = StrModflowSfrDownstreamInitialUnsaturatedWaterContent)
                or (BoundaryName.Name = StrModflowSfrDownstreamBrooksCoreyExponent)
                or (BoundaryName.Name = StrModflowSfrDownstreamMaxUnsaturatedKz)
                then
              begin
                DownstreamUnsatSegmentValues := SfrBoundary.DownstreamUnsatSegmentValues;
                if (DownstreamUnsatSegmentValues.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UnsatSegmentItem := DownstreamUnsatSegmentValues.GetItemByStartTime(
                      TimeList[TimeIndex]);
                    if UnsatSegmentItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      if (BoundaryName.Name = StrModflowSfrDownstreamSaturatedWaterContent) then
                      begin
                        Formula := AnsiString(UnsatSegmentItem.SaturatedWaterContent);
                      end
                      else if BoundaryName.Name = StrModflowSfrDownstreamInitialUnsaturatedWaterContent then
                      begin
                        Formula := AnsiString(UnsatSegmentItem.InitialWaterContent);
                      end
                      else if BoundaryName.Name = StrModflowSfrDownstreamBrooksCoreyExponent then
                      begin
                        Formula := AnsiString(UnsatSegmentItem.BrooksCoreyExponent);
                      end
                      else if BoundaryName.Name = StrModflowSfrDownstreamMaxUnsaturatedKz then
                      begin
                        Formula := AnsiString(UnsatSegmentItem.VerticalSaturatedK);
                      end
                      else
                      begin
                        Assert(False);
                      end;
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end            end;
          end;
        btMfUzf:
          begin
            UzfBoundary := AScreenObject.ModflowUzfBoundary;
            if UzfBoundary = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              if BoundaryName.Name = StrUzfInfiltrationRate then
              begin
                Values := UzfBoundary.Values;
                if (Values.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    RchTimeItem := Values.GetItemByStartTime(
                      TimeList[TimeIndex]) as TRchItem;
                    if RchTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(RchTimeItem.RechargeRate);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if BoundaryName.Name = StrUzfEtDemand then
              begin
                Values := UzfBoundary.EvapotranspirationDemand;
                if (Values.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    EtTimeItem := Values.GetItemByStartTime(
                      TimeList[TimeIndex]) as TEvtItem;
                    if EtTimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(EtTimeItem.EvapotranspirationRate);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
              end
              else if (BoundaryName.Name = StrUzfExtinctionDepth) then
              begin
                Values := UzfBoundary.ExtinctionDepth;
                if (Values.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UzfExtinctDepthItem := Values.GetItemByStartTime(
                      TimeList[TimeIndex]) as TUzfExtinctDepthItem;
                    if UzfExtinctDepthItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(UzfExtinctDepthItem.UzfExtinctDepth);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end;
              end
              else if (BoundaryName.Name = StrUzfWaterContent) then
              begin
                Values := UzfBoundary.WaterContent;
                if (Values.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UzfWaterContentItem := Values.GetItemByStartTime(
                      TimeList[TimeIndex]) as TUzfWaterContentItem;
                    if UzfWaterContentItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(UzfWaterContentItem.UzfWaterContent);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end;
              end
              else
              begin
                Assert(False);
              end;
            end;
          end;
        btMfObs:
          begin
            HeadObservations :=
              AScreenObject.ModflowHeadObservations;
            if (HeadObservations = nil) then
            begin
              Formula := FMissingValueString;
            end
            else
            begin
              Formula := AnsiString(HeadObservations.ObservationName);
            end;
            UpdFieldStr('OBSNAME', Formula);

            if (HeadObservations = nil) then
            begin
              IntValue := FMissingValue;
            end
            else
            begin
              IntValue := Ord(HeadObservations.Purpose);
            end;
            UpdFieldInt('OBSTYPE', IntValue);

            if FMaxHeadObsTimes > 1 then
            begin
              if (HeadObservations = nil) then
              begin
                IntValue := FMissingValue;
              end
              else
              begin
                IntValue := Ord(HeadObservations.MultiObsMethod);
              end;
              UpdFieldInt('ITT', IntValue);
            end;

            for TimeIndex := 1 to FMaxHeadObsTimes do
            begin
              if (HeadObservations = nil) or (TimeIndex > HeadObservations.Values.Count) then
              begin
                HobItem := nil;
              end
              else
              begin
                HobItem := HeadObservations.Values.HobItems[TimeIndex-1];
              end;

              if HobItem = nil then
              begin
                FloatValue := FMissingValue;
              end
              else
              begin
                FloatValue := HobItem.Time;
              end;
              UpdFieldNum(WString('O_TIME' + IntToStr(TimeIndex)),
                FloatValue);

              if HobItem = nil then
              begin
                FloatValue := FMissingValue;
              end
              else
              begin
                FloatValue := HobItem.Head;
              end;
              UpdFieldNum(WString('O_HEAD' + IntToStr(TimeIndex)),
                FloatValue);

              if HobItem = nil then
              begin
                FloatValue := FMissingValue;
              end
              else
              begin
                FloatValue := HobItem.Statistic;
              end;
              UpdFieldNum(WString('O_STAT' + IntToStr(TimeIndex)),
                FloatValue);

              if HobItem = nil then
              begin
                IntValue := FMissingValue;
              end
              else
              begin
                IntValue := Ord(HobItem.StatFlag);
              end;
              UpdFieldInt('O_SF' + AnsiString(IntToStr(TimeIndex)),
                IntValue);
            end;

          end;
        btMfMnw:
          begin
//          Assert(False);
            Mnw2Boundary := AScreenObject.ModflowMnw2Boundary;
            if Mnw2Boundary = nil then
            begin
              Formula := FMissingValueString;
              UpdFieldStr(
                FFieldDefinitions[StartIndex].FieldName, Formula);
              Inc(StartIndex);
            end
            else
            begin
              Values := Mnw2Boundary.Values;
              Assert(Values.Count > 0);

              MnwSpatialItem := Values[0] as TMnw2SpatialItem;
              Mnw2TimeValues := Mnw2Boundary.TimeValues;
              if MnwSpatialItem = nil then
              begin
                Formula := FMissingValueString;
              end
              else
              begin
                if BoundaryName.Name = StrWellRadius then
                begin
                  Formula := AnsiString(MnwSpatialItem.WellRadius);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrSkinRadius then
                begin
                  Formula := AnsiString(MnwSpatialItem.SkinRadius);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrSkinK then
                begin
                  Formula := AnsiString(MnwSpatialItem.SkinK);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrB then
                begin
                  Formula := AnsiString(MnwSpatialItem.B);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrC then
                begin
                  Formula := AnsiString(MnwSpatialItem.C);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrP then
                begin
                  Formula := AnsiString(MnwSpatialItem.P);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrCellToWellConductance then
                begin
                  Formula := AnsiString(MnwSpatialItem.CellToWellConductance);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrPartialPenetration then
                begin
                  Formula := AnsiString(MnwSpatialItem.CellToWellConductance);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrPartialPenetration then
                begin
                  Formula := AnsiString(MnwSpatialItem.PartialPenetration);
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end
                else if BoundaryName.Name = StrMnw2PumpingRate then
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    Mnw2TimeItemIndex := Mnw2TimeValues.IndexOfContainedStartTime(
                      TimeList[TimeIndex]);
                    if Mnw2TimeItemIndex < 0 then
                    begin
                      Mnw2TimeItem := nil
                    end
                    else
                    begin
                      Mnw2TimeItem := Mnw2TimeValues.Items[Mnw2TimeItemIndex]
                        as TMnw2TimeItem
                    end;
                    if Mnw2TimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(Mnw2TimeItem.PumpingRate);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end
                else if BoundaryName.Name = StrMnw2HeadCapacityMultip then
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    Mnw2TimeItemIndex := Mnw2TimeValues.IndexOfContainedStartTime(
                      TimeList[TimeIndex]);
                    if Mnw2TimeItemIndex < 0 then
                    begin
                      Mnw2TimeItem := nil
                    end
                    else
                    begin
                      Mnw2TimeItem := Mnw2TimeValues.Items[Mnw2TimeItemIndex]
                        as TMnw2TimeItem
                    end;
                    if Mnw2TimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(Mnw2TimeItem.HeadCapacityMultiplier);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end
                else if BoundaryName.Name = StrMnw2LimitingWaterLevel then
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    Mnw2TimeItemIndex := Mnw2TimeValues.IndexOfContainedStartTime(
                      TimeList[TimeIndex]);
                    if Mnw2TimeItemIndex < 0 then
                    begin
                      Mnw2TimeItem := nil
                    end
                    else
                    begin
                      Mnw2TimeItem := Mnw2TimeValues.Items[Mnw2TimeItemIndex]
                        as TMnw2TimeItem
                    end;
                    if Mnw2TimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(Mnw2TimeItem.LimitingWaterLevel);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end
                else if BoundaryName.Name = StrMnw2InactivationPumping then
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    Mnw2TimeItemIndex := Mnw2TimeValues.IndexOfContainedStartTime(
                      TimeList[TimeIndex]);
                    if Mnw2TimeItemIndex < 0 then
                    begin
                      Mnw2TimeItem := nil
                    end
                    else
                    begin
                      Mnw2TimeItem := Mnw2TimeValues.Items[Mnw2TimeItemIndex]
                        as TMnw2TimeItem
                    end;
                    if Mnw2TimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(Mnw2TimeItem.InactivationPumpingRate);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end
                else if BoundaryName.Name = StrMnw2ReactivationPumping then
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    Mnw2TimeItemIndex := Mnw2TimeValues.IndexOfContainedStartTime(
                      TimeList[TimeIndex]);
                    if Mnw2TimeItemIndex < 0 then
                    begin
                      Mnw2TimeItem := nil
                    end
                    else
                    begin
                      Mnw2TimeItem := Mnw2TimeValues.Items[Mnw2TimeItemIndex]
                        as TMnw2TimeItem
                    end;
                    if Mnw2TimeItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(Mnw2TimeItem.ReactivationPumpingRate);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end
                end
                else
                begin
                  Assert(False);
                end;
              end;
            end;
          end;
        btMt3dSsm:
          begin
            SSmBoundry := AScreenObject.Mt3dmsConcBoundary;
            if SSmBoundry = nil then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else if BoundaryName.Name = StrMT3DMSSSMConcentra then
            begin
                Values := SSmBoundry.Values;
                if (Values.Count = 0) then
                begin
                  Formula := FMissingValueString;
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end
                else
                begin
                  for TimeIndex := 0 to TimeList.Count - 1 do
                  begin
                    Mt3dConcItem := Values.GetItemByStartTime(
                      TimeList[TimeIndex]) as TMt3dmsConcItem;
                    if Mt3dConcItem = nil then
                    begin
                      Formula := FMissingValueString;
                    end
                    else
                    begin
                      Formula := AnsiString(Mt3dConcItem.Mt3dmsConcRate[0]);
                    end;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                    Inc(StartIndex);
                  end;
                end;
            end;
          end;
        btMfHfb:
          begin
            HfbBoundary := AScreenObject.ModflowHfbBoundary;
            if HfbBoundary = nil then
            begin
              Formula := FMissingValueString;
            end
            else
            begin
              if (BoundaryName.Name = StrHydrConductivity) then
              begin
                Formula := AnsiString(HfbBoundary.HydraulicConductivityFormula);
              end
              else if (BoundaryName.Name = StrThickness) then
              begin
                Formula := AnsiString(HfbBoundary.ThicknessFormula);
              end
              else if (BoundaryName.Name = StrHydrCharacteristic) then
              begin
                Formula := AnsiString('('
                  + HfbBoundary.HydraulicConductivityFormula
                  + ') * ('
                  + HfbBoundary.ThicknessFormula
                  + ')');
              end
              else
              begin
                Assert(False);
              end;
            end;
            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            Inc(StartIndex);
          end;
        btMfStr:
          begin
            StrBoundary := AScreenObject.ModflowStrBoundary;
            if BoundaryName.Name = StrModflowStrSegment then
            begin
              if StrBoundary <> nil then
              begin
                UpdFieldInt(
                  FFieldDefinitions[StartIndex].FieldName,
                  StrBoundary.SegmentNumber);
              end
              else
              begin
                Formula := FMissingValueString;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
              end;
              Inc(StartIndex);
            end
            else if (BoundaryName.Name = StrModflowStrDownstreamSegment)
              or (BoundaryName.Name = StrModflowStrDiversionSegment)
              or (BoundaryName.Name = StrSTRStreamTopElev)
              or (BoundaryName.Name = StrSTRStreamBottomElev)
              or (BoundaryName.Name = StrSTRStreamStage)
              or (BoundaryName.Name = StrSTRStreamConductance)
              or (BoundaryName.Name = StrSTRStreamFlow)
              or (BoundaryName.Name = StrSTRStreamWidth)
              or (BoundaryName.Name = StrSTRStreamSlope)
              or (BoundaryName.Name = StrSTRStreamRoughness)
              then
            begin
              if StrBoundary = nil then
              begin
                Values := nil
              end
              else
              begin
                Values := StrBoundary.Values;
              end;
              if (Values = nil) or (Values.Count = 0) then
              begin
                Formula := FMissingValueString;
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end
              else
              begin
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  StrItem := Values.GetItemByStartTime(
                    TimeList[TimeIndex]) as TStrItem;
                  if StrItem = nil then
                  begin
                    Formula := FMissingValueString;
                    UpdFieldStr(
                      FFieldDefinitions[StartIndex].FieldName, Formula);
                  end
                  else
                  begin
                    if BoundaryName.Name = StrModflowStrDownstreamSegment then
                    begin
                      UpdFieldInt(
                        FFieldDefinitions[StartIndex].FieldName,
                        StrItem.OutflowSegment);
                    end
                    else if BoundaryName.Name = StrModflowStrDiversionSegment then
                    begin
                      UpdFieldInt(
                        FFieldDefinitions[StartIndex].FieldName,
                        StrItem.DiversionSegment);
                    end
                    else if BoundaryName.Name = StrSTRStreamTopElev then
                    begin
                      Formula := AnsiString(StrItem.BedTop);
                      UpdFieldStr(
                        FFieldDefinitions[StartIndex].FieldName, Formula);
                    end
                    else if BoundaryName.Name = StrSTRStreamBottomElev then
                    begin
                      Formula := AnsiString(StrItem.BedBottom);
                      UpdFieldStr(
                        FFieldDefinitions[StartIndex].FieldName, Formula);
                    end
                    else if BoundaryName.Name = StrSTRStreamStage then
                    begin
                      Formula := AnsiString(StrItem.Stage);
                      UpdFieldStr(
                        FFieldDefinitions[StartIndex].FieldName, Formula);
                    end
                    else if BoundaryName.Name = StrSTRStreamConductance then
                    begin
                      Formula := AnsiString(StrItem.Conductance);
                      UpdFieldStr(
                        FFieldDefinitions[StartIndex].FieldName, Formula);
                    end
                    else if BoundaryName.Name = StrSTRStreamFlow then
                    begin
                      Formula := AnsiString(StrItem.Flow);
                      UpdFieldStr(
                        FFieldDefinitions[StartIndex].FieldName, Formula);
                    end
                    else if BoundaryName.Name = StrSTRStreamWidth then
                    begin
                      Formula := AnsiString(StrItem.Width);
                      UpdFieldStr(
                        FFieldDefinitions[StartIndex].FieldName, Formula);
                    end
                    else if BoundaryName.Name = StrSTRStreamSlope then
                    begin
                      Formula := AnsiString(StrItem.Slope);
                      UpdFieldStr(
                        FFieldDefinitions[StartIndex].FieldName, Formula);
                    end
                    else if BoundaryName.Name = StrSTRStreamRoughness then
                    begin
                      Formula := AnsiString(StrItem.Roughness);
                      UpdFieldStr(
                        FFieldDefinitions[StartIndex].FieldName, Formula);
                    end
                    else
                    begin
                      Assert(False);
                    end;
                  end;
                  Inc(StartIndex);
                end
              end;
            end
            else
            begin
              Assert(False);
            end;
          end;
        btSfr_MF6:
          begin
            Sfr6Boundary := AScreenObject.ModflowSfr6Boundary;
            if Sfr6Boundary = nil then
            begin
              Values := nil
            end
            else
            begin
              Values := Sfr6Boundary.Values;
            end;
            if (Values = nil) or (Values.Count = 0) then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                Sfr6Item := Values.GetItemByStartTime(
                  TimeList[TimeIndex]) as TSfrMf6Item;
                if Sfr6Item = nil then
                begin
                  Formula := FMissingValueString;
                end
                else
                begin
                  if BoundaryName.Name = StrSFR6Inflow then
                  begin
                    Formula := AnsiString(Sfr6Item.Inflow);
                  end
                  else if BoundaryName.Name = StrSFR6Rainfall then
                  begin
                    Formula := AnsiString(Sfr6Item.Rainfall);
                  end
                  else if BoundaryName.Name = StrSFR6Evaporation then
                  begin
                    Formula := AnsiString(Sfr6Item.Evaporation);
                  end
                  else if BoundaryName.Name = StrSFR6Runoff then
                  begin
                    Formula := AnsiString(Sfr6Item.Runoff);
                  end
                  else if BoundaryName.Name = StrSFR6UpstreamFracti then
                  begin
                    Formula := AnsiString(Sfr6Item.UpstreamFraction);
                  end
                  else if BoundaryName.Name = StrSFR6Stage then
                  begin
                    Formula := AnsiString(Sfr6Item.Stage);
                  end
                  else if BoundaryName.Name = StrSFR6Roughness then
                  begin
                    Formula := AnsiString(Sfr6Item.Roughness);
                  end
                  else if BoundaryName.Name = StrSFR6StreamStatus then
                  begin
                    case Sfr6Item.StreamStatus of
                      ssInactive:
                        begin
                          Formula := 'Inactive';
                        end;
                      ssActive:
                        begin
                          Formula := 'Active';
                        end;
                      ssSimple:
                        begin
                          Formula := 'Simple';
                        end;
                    end;
                  end
                  else
                  begin
                    Assert(False);
                  end;
                end;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end
            end;
          end;
        btMAW:
          begin
            MawBoundary := AScreenObject.ModflowMawBoundary;
            if MawBoundary = nil then
            begin
              Values := nil
            end
            else
            begin
              Values := MawBoundary.Values;
            end;
            if (Values = nil) or (Values.Count = 0) then
            begin
              Formula := FMissingValueString;
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end;
            end
            else
            begin
              for TimeIndex := 0 to TimeList.Count - 1 do
              begin
                MawItem := Values.GetItemByStartTime(
                  TimeList[TimeIndex]) as TMawItem;
                if MawItem = nil then
                begin
                  Formula := FMissingValueString;
                end
                else
                begin
                  if BoundaryName.Name = StrMAWWellElevation then
                  begin
                    Formula := AnsiString(MawItem.FlowingWellElevation);
                  end
                  else if BoundaryName.Name = StrMAWWellConductance then
                  begin
                    Formula := AnsiString(MawItem.FlowingWellConductance);
                  end
                  else if BoundaryName.Name = StrMAWWellRate then
                  begin
                    Formula := AnsiString(MawItem.Rate);
                  end
                  else if BoundaryName.Name = StrMAWWellHead then
                  begin
                    Formula := AnsiString(MawItem.WellHead);
                  end
                  else if BoundaryName.Name = StrMAWWellLimit then
                  begin
                    Formula := AnsiString(MawItem.HeadLimit);
                  end
                  else if BoundaryName.Name = StrMAWWellMinimumPum then
                  begin
                    Formula := AnsiString(MawItem.MinRate);
                  end
                  else if BoundaryName.Name = StrMAWWellMaximumPum then
                  begin
                    Formula := AnsiString(MawItem.MaxRate);
                  end
                  else if BoundaryName.Name = StrMAWPumpElevation then
                  begin
                    Formula := AnsiString(MawItem.PumpElevation);
                  end
                  else if BoundaryName.Name = StrMAWScalingLength then
                  begin
                    Formula := AnsiString(MawItem.ScalingLength);
                  end
                  else if BoundaryName.Name = StrMAWWellRedLength then
                  begin
                    Formula := AnsiString(MawItem.FlowingWellReductionLength);
                  end
                  else
                  begin
                    Assert(False);
                  end;
                end;
                UpdFieldStr(
                  FFieldDefinitions[StartIndex].FieldName, Formula);
                Inc(StartIndex);
              end
            end;
          end;
        btMfFhb:
          begin
            if (fbtFlow in FFhbBoundaryTypes) and (BoundaryName.FhbBoundaryType = fbtFlow) then
            begin
              FhbFlowBoundary := AScreenObject.ModflowFhbFlowBoundary;
              if FhbFlowBoundary = nil then
              begin
                Formula := FMissingValueString;
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end
              else
              begin
                Values := nil;
                if FhbFlowBoundary.Values.Count > 0 then
                begin
                  Values := FhbFlowBoundary.Values;
                end;
                Assert(Values.Count > 0);
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  FhbTimeItem := Values.GetItemByStartTime(
                    TimeList[TimeIndex]) as TFhbItem;
                  if FhbTimeItem = nil then
                  begin
                    Formula := FMissingValueString;
                  end
                  else
                  begin
                    Formula := AnsiString(FhbTimeItem.BoundaryValue);
                  end;
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end;
            end;
            if (fbtHead in FFhbBoundaryTypes) and (BoundaryName.FhbBoundaryType = fbtHead) then
            begin
              FhbHeadBoundary := AScreenObject.ModflowFhbHeadBoundary;
              if FhbHeadBoundary = nil then
              begin
                Formula := FMissingValueString;
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end
              else
              begin
                Values := nil;
                if FhbHeadBoundary.Values.Count > 0 then
                begin
                  Values := FhbHeadBoundary.Values;
                end;
                Assert(Values.Count > 0);
                for TimeIndex := 0 to TimeList.Count - 1 do
                begin
                  FhbTimeItem := Values.GetItemByStartTime(
                    TimeList[TimeIndex]) as TFhbItem;
                  if FhbTimeItem = nil then
                  begin
                    Formula := FMissingValueString;
                  end
                  else
                  begin
                    Formula := AnsiString(FhbTimeItem.BoundaryValue);
                  end;
                  UpdFieldStr(
                    FFieldDefinitions[StartIndex].FieldName, Formula);
                  Inc(StartIndex);
                end;
              end;
            end;
          end
        else
          Assert(False);
      end;
    end;
  end;
  procedure AssignExtraData;
  begin
    StartIndex := ExtraDataSetStart;
    if cbExportName.Checked then
    begin
      Formula := AnsiString(AScreenObject.Name + SectionString);
      UpdFieldStr(
        FFieldDefinitions[StartIndex].FieldName, Formula);
      if Length(Formula) > 254 then
      begin
        frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
          StrFormulaTruncatedTo, Format(' Object Name; Object = %s',
          [AScreenObject.Name]), AScreenObject);
        FShowWarning := True;
      end;
      Inc(StartIndex);
    end;
    if cbExportElevationFormulas.Checked then
    begin
      case AScreenObject.ElevationCount of
        ecZero:
          begin
            Formula := AnsiString(FMissingValueString);
            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            Inc(StartIndex);

            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            Inc(StartIndex);

            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            Inc(StartIndex);
          end;
        ecOne:
          begin
            Formula := AnsiString(AScreenObject.ElevationFormula);
            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            if Length(Formula) > 254 then
            begin
              frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
                StrFormulaTruncatedTo, Format(' Z Formula; Object = %s',
                [AScreenObject.Name]), AScreenObject);
              FShowWarning := True;
            end;
            Inc(StartIndex);

            Formula := AnsiString(FMissingValueString);
            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            Inc(StartIndex);

            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            Inc(StartIndex);
          end;
        ecTwo:
          begin
            Formula := AnsiString(FMissingValueString);
            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            Inc(StartIndex);

            Formula := AnsiString(AScreenObject.HigherElevationFormula);
            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            if Length(Formula) > 254 then
            begin
              frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
                StrFormulaTruncatedTo, Format(' Higher Z Formula; Object = %s',
                [AScreenObject.Name]), AScreenObject);
              FShowWarning := True;
            end;
            Inc(StartIndex);

            Formula := AnsiString(AScreenObject.LowerElevationFormula);
            UpdFieldStr(
              FFieldDefinitions[StartIndex].FieldName, Formula);
            if Length(Formula) > 254 then
            begin
              frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
                StrFormulaTruncatedTo, Format(' Lower Z Formula; Object = %s',
                [AScreenObject.Name]), AScreenObject);
              FShowWarning := True;
            end;
            Inc(StartIndex);
          end;
        else
          Assert(False);
      end;
    end;

  end;
begin
  TimeList:= TList<Double>.Create;
  FieldValues := TStringList.Create;
  try
    if rgExportMethod.ItemIndex = 1 then
    begin
      FieldValues.Capacity := FFieldNames.Count;
      for FieldIndex := 0 to FFieldNames.Count - 1 do
      begin
        FieldValues.Add('');
      end;
    end;
    TimeList.Capacity := FTimeCount;
    if FTimeCount > 0 then
    begin
      for TimeIndex := 0 to chklstTimes.Items.Count - 1 do
      begin
        if chklstTimes.Checked[TimeIndex] then
        begin
          FloatValue := StrToFloat(chklstTimes.Items[TimeIndex]);
          TimeList.Add(FloatValue);
        end;
      end;
    end;
    ExtraDataSetCount := GetExtraDataSetCount;
    ExtraDataSetStart := Length(FFieldDefinitions) - ExtraDataSetCount;
    BoundaryDataSetStart := ExtraDataSetStart - FBoundDataSetCount;

    if BreakObject then
    begin
      for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
      begin
        if rgExportMethod.ItemIndex = 0 then
        begin
          XBaseShapeFile.AppendBlank;
        end;
        for FieldIndex := 0 to BoundaryDataSetStart - 1 do
        begin
          DataArray := FFieldDefinitions[FieldIndex].DataArray;
          APosition := AScreenObject.IndexOfDataSet(DataArray);
          FloatValue := FMissingValue;
          IntValue := FMissingValue;
          Formula := AnsiString(FMissingValueString);
          if APosition < 0 then
          begin
            case DataArray.DataType of
              rdtString:
                begin
                  Formula := AnsiString(FMissingValueString);
                end;
              rdtDouble:
                begin
                  FloatValue := FMissingValue;
                end;
              rdtBoolean:
                begin
                  IntValue := FMissingValue;
                end;
              rdtInteger:
                begin
                  IntValue := FMissingValue;
                end;
            else
              Assert(False);
            end;
          end
          else
          begin
            Formula := AnsiString(AScreenObject.DataSetFormulas[APosition]);
            ImportedValues := GetImportedValuesFromFormula(DataArray,
              AScreenObject, string(Formula));
            if (ImportedValues <> nil)
              and (ImportedValues.Count = AScreenObject.SectionCount) then
            begin
              case DataArray.DataType of
                rdtDouble:
                  begin
                    FloatValue := ImportedValues.RealValues[SectionIndex];
                    Formula := AnsiString(FortranFloatToStr(FloatValue));
                  end;
                rdtInteger:
                  begin
                    IntValue := ImportedValues.IntValues[SectionIndex];
                    Formula := AnsiString(IntToStr(IntValue));
                  end;
                rdtBoolean:
                  begin
                    if ImportedValues.BooleanValues[SectionIndex] then
                    begin
                      IntValue := 1;
                      Formula := '1';
                    end
                    else
                    begin
                      IntValue := 0;
                      Formula := '0';
                    end;
                  end;
                rdtString:
                  begin
                    Formula := AnsiString(ImportedValues.StringValues[SectionIndex]);
                    if Length(Formula) >= 2 then
                    begin
                      if (Formula[1] = '"')
                        and (Formula[Length(Formula)] = '"')
                        and (PosEx('"', String(Formula), 2) = Length(Formula)) then
                      begin
                        Formula := Copy(Formula, 2, Length(Formula) - 2);
                      end;
                    end;
                  end;
              end;
            end
            else
            begin
              case DataArray.DataType of
                rdtDouble:
                  begin
                    if FFieldDefinitions[FieldIndex].FieldType = 'F' then
                    begin
                      FloatValue := StrToFloat(String(Formula));
                    end;
                  end;
                rdtInteger:
                  begin
                    if FFieldDefinitions[FieldIndex].FieldType = 'N' then
                    begin
                      IntValue := StrToInt(String(Formula));
                    end;
                  end;
                rdtBoolean:
                  begin
                    if FFieldDefinitions[FieldIndex].FieldType = 'N' then
                    begin
                      if SameText(String(Formula), 'True') then
                      begin
                        IntValue := 1;
                      end
                      else
                      begin
                        IntValue := 0;
                      end;
                    end;
                  end;
                rdtString:
                  begin
                    if Length(Formula) >= 2 then
                    begin
                      if (Formula[1] = '"')
                        and (Formula[Length(Formula)] = '"')
                        and (PosEx('"', String(Formula), 2) = Length(Formula)) then
                      begin
                        Formula := Copy(Formula, 2, Length(Formula) - 2);
                      end;
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
          end;
          if rgExportMethod.ItemIndex = 0 then
          begin
            case FFieldDefinitions[FieldIndex].FieldType of
              'C':
                begin
                  UpdFieldStr(
                    FFieldDefinitions[FieldIndex].FieldName, Formula);
                  if Length(Formula) > 254 then
                  begin
                    frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
                      StrFormulaTruncatedTo, Format(StrDataSet0sOb,
                      [DataArray.Name, AScreenObject.Name]), AScreenObject);
                    FShowWarning := True;
                  end;
                end;
              'F':
                begin
                  UpdFieldNum(
                    FFieldDefinitions[FieldIndex].FieldName, FloatValue);
                end;
        //      'L':
        //        begin
        //          XBaseShapeFile.UpdFieldInt(
        //            FFieldDefinitions[FieldIndex].FieldName, IntValue);
        //        end;
              'N':
                begin
                  UpdFieldInt(
                    FFieldDefinitions[FieldIndex].FieldName, IntValue);
                end;
            else
              Assert(False);
            end;
          end
          else
          begin

          end;
        end;
        SectionString := '_' + IntToStr(SectionIndex+1);
        AssignBoundaryData;
        AssignExtraData;

        if rgExportMethod.ItemIndex = 0 then
        begin
          XBaseShapeFile.PostChanges;
          XBaseShapeFile.GotoNext;
        end
        else
        begin
          ALine := FieldValues.CommaText;
          FCsvWriter.WriteLine(ALine);
        end;
      end;
    end
    else
    begin
      if rgExportMethod.ItemIndex = 0 then
      begin
        XBaseShapeFile.AppendBlank;
      end;
      for FieldIndex := 0 to BoundaryDataSetStart - 1 do
      begin
        DataArray := FFieldDefinitions[FieldIndex].DataArray;
        APosition := AScreenObject.IndexOfDataSet(DataArray);
        FloatValue := FMissingValue;
        IntValue := FMissingValue;
        if APosition < 0 then
        begin
          case DataArray.DataType of
            rdtString:
              begin
                Formula := AnsiString(FMissingValueString);
              end;
            rdtDouble:
              begin
                FloatValue := FMissingValue;
              end;
            rdtBoolean:
              begin
                IntValue := FMissingValue;
              end;
            rdtInteger:
              begin
                IntValue := FMissingValue;
              end;
          else
            Assert(False);
          end;
        end
        else
        begin
          Formula := AnsiString(AScreenObject.DataSetFormulas[APosition]);
          case DataArray.DataType of
            rdtString:
              begin
                if Length(Formula) >= 2 then
                begin
                  if (Formula[1] = '"')
                    and (Formula[Length(Formula)] = '"')
                    and (PosEx('"', string(Formula), 2) = Length(Formula)) then
                  begin
                    Formula := Copy(Formula, 2, Length(Formula) - 2);
                  end;
                end;
              end;
            rdtDouble:
              begin
                if FFieldDefinitions[FieldIndex].FieldType = 'F' then
                begin
                  FloatValue := StrToFloat(string(Formula));
                end;
              end;
            rdtBoolean:
              begin
                if FFieldDefinitions[FieldIndex].FieldType = 'N' then
                begin
                  if SameText(string(Formula), 'True') then
                  begin
                    IntValue := 1;
                  end
                  else
                  begin
                    IntValue := 0;
                  end;
                end;
              end;
            rdtInteger:
              begin
                if FFieldDefinitions[FieldIndex].FieldType = 'N' then
                begin
                  IntValue := StrToInt(string(Formula));
                end;
              end;
          else
            Assert(False);
          end;
        end;
        case FFieldDefinitions[FieldIndex].FieldType of
          'C':
            begin
              UpdFieldStr(
                FFieldDefinitions[FieldIndex].FieldName, Formula);
              if Length(Formula) > 254 then
              begin
                frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
                  StrFormulaTruncatedTo, Format(StrDataSet0sOb,
                  [DataArray.Name, AScreenObject.Name]), AScreenObject);
                FShowWarning := True;
              end;
            end;
          'F':
            begin
              UpdFieldNum(
                FFieldDefinitions[FieldIndex].FieldName, FloatValue);
            end;
    //      'L':
    //        begin
    //          XBaseShapeFile.UpdFieldInt(
    //            FFieldDefinitions[FieldIndex].FieldName, IntValue);
    //        end;
          'N':
            begin
              UpdFieldInt(
                FFieldDefinitions[FieldIndex].FieldName, IntValue);
            end;
        else
          Assert(False);
        end;
      end;
      SectionString := '';
      AssignBoundaryData;
      AssignExtraData;
      if rgExportMethod.ItemIndex = 0 then
      begin
        XBaseShapeFile.PostChanges;
        XBaseShapeFile.GotoNext;
      end
      else
      begin
        ALine := FieldValues.CommaText;
        FCsvWriter.WriteLine(ALine);
      end;
    end;
  finally
    TimeList.Free;
    FieldValues.Free;
  end;
end;

procedure TfrmExportShapefileObjects.SetAllowableShapeTypes;
var
  AScreenObject: TScreenObject;
  Index: Integer;
  PolygonOk: Boolean;
  PointOK: Boolean;
  MultiPointPreferred: boolean;
begin
  PointOK := True;
  PolygonOk := True;
  MultiPointPreferred := True;
  for Index := 0 to FSelectedScreenObjects.Count - 1 do
  begin
    AScreenObject := FSelectedScreenObjects[Index];
    if AScreenObject.Count > 1 then
    begin
      PointOK := False;
    end;
    if PolygonOk and not AScreenObject.AllSectionsClosed then
    begin
      PolygonOk := False;
    end;
    if AScreenObject.Count <> AScreenObject.SectionCount then
    begin
      MultiPointPreferred := False;
    end;
  end;
  rbPoints.Enabled := PointOK;
  rbPolygons.Enabled := PolygonOk;
  if PointOK then
  begin
    rbPoints.Checked := True;
  end
  else if PolygonOk then
  begin
    rbPolygons.Checked := True;
  end
  else if MultiPointPreferred then
  begin
    rbMultipoint.Checked := True;
  end
  else
  begin
    rbPolyline.Checked := True;
  end;
       
end;

procedure TfrmExportShapefileObjects.FormCreate(Sender: TObject);
begin
  inherited;
  FFieldNames := TStringList.Create;
  FBoundaryNames := TStringList.Create;
  FBoundaryNames.OwnsObjects := True;
  FBoundaryNames.Sorted := true;
  vstDataSets.Width := (vstDataSets.Width + vstObjects.Width) div 2;
  CenterLabels;

  vstDataSets.NodeDataSize := SizeOf(TClassificationNodeData);
  FObjectOwner := TObjectList.Create;
  FSelectedDataSets := TList.Create;
  FSelectedScreenObjects := TList.Create;

  FClassifiationList := TObjectList.Create;
  FEdgeList := TObjectList.Create;

  GetData;
end;

procedure TfrmExportShapefileObjects.FormDestroy(Sender: TObject);
begin
  inherited;
  FEdgeList.Free;
  FClassifiationList.Free;
  FObjectOwner.Free;
  FSelectedDataSets.Free;
  FSelectedScreenObjects.Free;

  FBoundaryNames.Free;
  FFieldNames.Free;
  FCsvWriter.Free;
end;

procedure TfrmExportShapefileObjects.FormResize(Sender: TObject);
begin
  inherited;
  CenterLabels;
end;

procedure TfrmExportShapefileObjects.GetData;
var
  Node: PVirtualNode;
  StressPeriods: TModflowStressPeriods;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
begin
  inherited;

  if frmGoPhast.PhastModel.ModelSelection in ModflowSelection then
  begin
    StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
    chklstTimes.Items.Capacity := StressPeriods.Count;
    for TimeIndex := 0 to StressPeriods.Count - 1 do
    begin
      StressPeriod := StressPeriods[TimeIndex];
      chklstTimes.Items.Add(FloatToStr(StressPeriod.StartTime));
    end;
  end;

  FSelectedBoundaries := [];
  FillVirtualStringTreeWithDataSets(vstDataSets, FObjectOwner, nil, CanDisplayDataSet);
  FillVirtStrTreeWithBoundaryConditions(nil, nil, nil, FClassifiationList,
    FEdgeList, vstDataSets, CanSelectBoundary, True);
  Node := vstDataSets.GetFirst;
  While Node <> nil do
  begin
    if vstDataSets.HasChildren[Node] then
    begin
      Node.CheckType := ctTriStateCheckBox;
    end
    else
    begin
      Node.CheckType := ctCheckBox;
    end;
    Node := vstDataSets.GetNext(Node)
  end;
end;

procedure TfrmExportShapefileObjects.HandleChecked(
  AScreenObject: TScreenObject);
begin
  if FSelectedScreenObjects.IndexOf(AScreenObject) < 0 then
  begin
    FSelectedScreenObjects.Add(AScreenObject)
  end;
end;

procedure TfrmExportShapefileObjects.HandleUnchecked(
  AScreenObject: TScreenObject);
var
  Position: integer;
begin
  Position := FSelectedScreenObjects.IndexOf(AScreenObject);
  if Position >= 0 then
  begin
    FSelectedScreenObjects.Delete(Position);
  end;
  EnableTimesCheckList;
end;

function TfrmExportShapefileObjects.ShouldCheckBoxBeChecked(
  ScreenObject: TScreenObject): boolean;
begin
  result := ScreenObject.Visible;
end;

procedure TfrmExportShapefileObjects.splLeftMoved(Sender: TObject);
begin
  inherited;
  CenterLabels;
end;

procedure TfrmExportShapefileObjects.splRightMoved(Sender: TObject);
begin
  inherited;
  CenterLabels;
end;

procedure TfrmExportShapefileObjects.vstDataSetsChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PClassificationNodeData;
  DataSetClassificationObject: TDataSetClassification;
  DataArray: TDataArray;
  AllObjectData: PMyRec;
  Index: Integer;
  BoundaryClassificationObject: TBoundaryClassification;
  BoundName: TBoundaryName;
  BoundIndex: Integer;
  ParentNode: PVirtualNode;
  ChildNode: PVirtualNode;
  ChildChecked: Boolean;
begin
  inherited;
  FSettingChecked := True;
  Data := Sender.GetNodeData(Node);
  if Data.ClassificationObject <> nil then
  begin
    if Data.ClassificationObject is TDataSetClassification then
    begin
      DataSetClassificationObject := TDataSetClassification(Data.ClassificationObject);
      DataArray := DataSetClassificationObject.DataArray;
      Assert(DataArray <> nil);
      if Sender.CheckState[Node] in [csCheckedNormal, csCheckedPressed] then
      begin
        FSelectedDataSets.Add(DataArray);
      end
      else
      begin
        FSelectedDataSets.Remove(DataArray);
      end;
    end
    else if Data.ClassificationObject is TBoundaryClassification then
    begin
      BoundaryClassificationObject := TBoundaryClassification(Data.ClassificationObject);
      if Sender.CheckState[Node] in [csCheckedNormal, csCheckedPressed] then
      begin
        Include(FSelectedBoundaries, BoundaryClassificationObject.BoundaryType);
        BoundName := TBoundaryName.Create;
        BoundName.Name := BoundaryClassificationObject.ClassificationName;
        BoundName.BoundaryType := BoundaryClassificationObject.BoundaryType;
        FBoundaryNames.AddObject(BoundName.Name, BoundName);
        if BoundName.BoundaryType= btMfFhb then
        begin
          if BoundaryClassificationObject.ClassificationName = 'FHB Flows' then
          begin
            Include(FFhbBoundaryTypes, fbtFlow);
            BoundName.FhbBoundaryType := fbtFlow;
          end
          else
          begin
            Include(FFhbBoundaryTypes, fbtHead);
            BoundName.FhbBoundaryType := fbtHead;
          end;
        end;
      end
      else if not (Sender.CheckState[Node] in [csCheckedNormal, csCheckedPressed]) then
      begin
        BoundIndex := FBoundaryNames.IndexOf(BoundaryClassificationObject.ClassificationName);
        if BoundIndex >= 0 then
        begin
          FBoundaryNames.Delete(BoundIndex);
        end;

        ChildChecked := False;
        ParentNode := Sender.NodeParent[Node];
        ChildNode := Sender.GetFirstChild(ParentNode);
        while ChildNode <> nil do
        begin
          ChildChecked := Sender.CheckState[ChildNode]
            in [csCheckedNormal, csCheckedPressed];
          if ChildChecked then
          begin
            Break;
          end;
          ChildNode := Sender.GetNextSibling(ChildNode);
        end;

        if not ChildChecked then
        begin
          Exclude(FSelectedBoundaries, BoundaryClassificationObject.BoundaryType);
        end;

        if BoundaryClassificationObject.BoundaryType= btMfFhb then
        begin
          if BoundaryClassificationObject.ClassificationName = 'FHB Flows' then
          begin
            Exclude(FFhbBoundaryTypes, fbtFlow);
          end
          else
          begin
            Exclude(FFhbBoundaryTypes, fbtHead);
          end;
        end;
      end;
    end;
  end;
  if FCurrentNodeName = Data.ClassificationObject.ClassificationName then
  begin
    inherited GetData;
    AllObjectData := vstObjects.GetNodeData(FvstAllObjectsNode);
    for Index := FSelectedScreenObjects.Count - 1 downto 0 do
    begin
      if AllObjectData.ScreenObjects.IndexOf(
        FSelectedScreenObjects[Index]) < 0 then
      begin
        FSelectedScreenObjects.Delete(Index);
      end;
    end;
    SetCheckedNodes(vstObjects);
    FSettingChecked := False;
  end;
  EnableTimesCheckList;
end;

procedure TfrmExportShapefileObjects.vstDataSetsChecking(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
  var Allowed: Boolean);
var
  Data: PClassificationNodeData;
begin
  inherited;
  if FSettingChecked then
  begin
    Exit;
  end;
  Data := Sender.GetNodeData(Node);
  if Data.ClassificationObject <> nil then
  begin
    FCurrentNodeName := Data.ClassificationObject.ClassificationName;
    if FCurrentNodeName <> '' then
    begin
      FSettingChecked := True;
    end;
  end;
end;

procedure TfrmExportShapefileObjects.vstDataSetsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  Data: PClassificationNodeData;
begin
  inherited;
  if csDestroying in ComponentState then
  begin
    CellText := '';
    Exit;
  end;
  // A handler for the OnGetText event is always needed
  // as it provides the tree with the string data to display.
  // Note that we are now  using string instead of WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    CellText := Data.ClassificationObject.ClassificationName;
  end;
end;

procedure TfrmExportShapefileObjects.vstObjectsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  inherited;
  if FSettingData or FSettingData2 or FSettingData3 then
  begin
    Exit;
  end;
  if (Sender.NodeParent[Node] = nil) then
  begin
    Exit;
  end;
  if not FOkToDoCheck then
  begin
    Exit;
  end;
  FSettingData := True;
  Sender.BeginUpdate;
  try
    HandleCheckChange(Node, Sender);
    SetCheckedNodes(Sender);
    SetAllowableShapeTypes;
  finally
    Sender.EndUpdate;
    FSettingData := False;
  end;
end;

end.
