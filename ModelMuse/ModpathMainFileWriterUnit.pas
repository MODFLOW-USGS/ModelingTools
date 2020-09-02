unit ModpathMainFileWriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, DataSetUnit, PhastModelUnit,
  ModflowPackageSelectionUnit, IntListUnit, Classes;

type
  // MODPATH version 5
  TModpathMainFileWriter = class(TCustomModflowWriter)
  private
    XSECTION: Boolean;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet(const DataSetName: string; DataArray: TDataArray);
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6a;
    procedure WriteDataSet6b;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    class function Extension: string; override;
    procedure WriteFile(const AFileName: string);
  end;

  // MODPATH version 6 and 7
  TModpathBasicFileWriter = class(TCustomModflowWriter)
  private
    FOptions: TModpathSelection;
    FFlowTerms: TStringList;
    FIfaceTerms: TIntegerList;
    FMpathVersion: TMpathVersion;
    procedure WriteDataSet(const DataSetName: string; DataArray: TDataArray);
    procedure Evaluate;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3and4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSets7and8;
  protected
    function PackageID_Comment(APackage: TModflowPackageSelection): string; override;
  public
    class function Extension: string; override;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmErrorsAndWarningsUnit, LayerStructureUnit, ModflowPackagesUnit, Forms,
  frmProgressUnit, GoPhastTypes, ArchiveNodeInterface, ModflowGridUnit;

resourcestring
  StrUndefinedLengthUni = 'Undefined length units';
  StrTheLengthUnitsOf = 'The length units of the model are undefined. MODPAT' +
  'H-PLOT will treat the units as feet';
  StrUnsupportedLengthU = 'Unsupported length units for MODPATH-PLOT';
  StrTheLengthUnitsOf1 = 'The length units of the model are centimeters. MODP' +
  'ATH-PLOT will treat the units as feet';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
  StrWritingDataSets7and8 = '  Writing Data Sets 7 and 8';
  StrInvalidGridStructu = 'Invalid grid structure';
  StrNonsimulatedLayers = 'Non-simulated layers are not supported in this ve' +
  'rsion of MODPATH. Try using MODPATH version 6.';
  StrInvalidDefaultIFace = 'Invalid DefaultIFaceValue';
  StrDefaultIFaceValueMu = 'DefaultIFaceValue must be in the range 0 to 6 in' +
  ' this version of MODPATH.';
  StrMOSPATH7RequiresA = 'MODPATH 7 requires a uniform grid (except for quad' +
  'tree refinement). In your model the largest and smallest %0:s widths are' +
  ' %1:g and %2:g.';
  StrColumn = 'column';
  StrRow = 'row';
  StrDataSet1HNOFL = ' # Data Set 1: HNOFLO  HDRY';
  StrDataSet2Defau = ' # Data Set 2: DefaultIFaceCount';
  StrDataSet6IBOUND = 'Data Set 6: IBOUND';
  StrDataSet7Porosity = 'Data Set 7: Porosity Layer ';
  StrPorosity = 'Porosity';
  StrDataSet8Porosity = 'Data Set 8: PorosityCB Layer ';
  StrPorosityCB = 'PorosityCB';
  StrDataSet1MAXSI = ' # Data Set 1: MAXSIZ HNOFLO  HDRY  NPART  IRCHTP  IEV' +
  'TTP';
  StrDataSet2OPTIO = ' # Data Set 2: OPTION';
  StrDataSet6ATBEG = ' # Data Set 6A: TBEGIN';
  StrDataSet6BBegi = ' # Data set 6B: BeginPeriod, BeginStep, EndPeriod, End' +
  'Step';
  StrDataSet5PORLaye = 'Data Set 5: POR Layer ';
  StrPOR = 'POR';
  StrDataSet5PorCBLa = 'Data Set 5: PorCB Layer ';
  StrPorCB = 'PorCB';
  StrDataSet4IBOUND = 'Data Set 4: IBOUND';

const
  ETS_ID = '     ET SEGMENTS';
  EVT_ID = '              ET';
  RCH_ID = '        RECHARGE';
  MNW2_ID = '            MNW2';
  LAK_ID = '   LAKE  SEEPAGE';
  UZF_ID1 = '    UZF INFILTR.';
  UZF_ID2 = 'SFR-DIV. INFLTR.';
  UZF_ID3 = '    UZF RECHARGE';
  UZF_ID4 = '           GW ET';
  UZF_ID5 = ' SURFACE LEAKAGE';
  UZF_ID6 = '    INFILTRATION';
  UZF_ID7 = '  STORAGE CHANGE';
  UZF_ID8 = '          UZF ET';
  SFR_ID1 = '  STREAM LEAKAGE';
  SFR_ID2 = 'STREAMFLOW OUT  ';
  SFR_ID3 = 'STREAM LISTING  ';
  RES_ID = ' RESERV. LEAKAGE';

// unused
  MNW1_ID = '             MNW';
  STR_ID = '  STREAM LEAKAGE';

{ TModpathMainFileWriter }

constructor TModpathMainFileWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FArrayWritingFormat := awfModflow;
end;

class function TModpathMainFileWriter.Extension: string;
begin
  result := '.mpm';
end;

procedure TModpathMainFileWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrUndefinedLengthUni);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrUnsupportedLengthU);

    NameOfFile := FileName(AFileName);
    Model.AddModpathInputFile(NameOfFile);
    FInputFileName := NameOfFile; 
    OpenFile(NameOfFile);
    try
      WriteDataSet1;
      WriteDataSet2;
      WriteDataSet3;
      WriteDataSet4;
      WriteDataSet5;
      WriteDataSet6a;
      WriteDataSet6b;
    finally
      CloseFile;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModpathMainFileWriter.WriteDataSet6a;
var
  TBEGIN: Double;
  FirstTime: Double;
//  ReferenceTime: Real;
begin
//  ReferenceTime := Model.ModflowPackages.ModPath.ReferenceTime;
  FirstTime := Model.ModflowFullStressPeriods.Items[0].StartTime;
  TBEGIN := FirstTime { - ReferenceTime};
  WriteFloat(TBEGIN);
  WriteString(StrDataSet6ATBEG);
  NewLine;
end;

procedure TModpathMainFileWriter.WriteDataSet6b;
var
  BeginPeriod, BeginStep, EndPeriod, EndStep: integer;
  ATime: Real;
begin
    if Model.ModflowStressPeriods.CompletelyTransient then
    begin
      ATime := Model.ModflowPackages.ModPath.BeginningTime;
      Model.ModflowFullStressPeriods.TimeToPeriodAndStep(
        ATime, BeginPeriod, BeginStep);
      Inc(BeginPeriod);
      Inc(BeginStep);
    end
    else
    begin
      BeginPeriod := 1;
      BeginStep := 1;
    end;
    ATime := Model.ModflowPackages.ModPath.EndingTime;
    Model.ModflowFullStressPeriods.TimeToPeriodAndStep(
      ATime, EndPeriod, EndStep);

    Inc(EndPeriod);
    Inc(EndStep);
    WriteInteger(BeginPeriod);
    WriteInteger(BeginStep);
    WriteInteger(EndPeriod);
    WriteInteger(EndStep);
    WriteString(StrDataSet6BBegi);
    NewLine;
end;

procedure TModpathMainFileWriter.WriteDataSet5;
var
//  LayerIndex: Integer;
  LayerGroup: TLayerGroup;
  Index: Integer;
  Layer: Integer;
  LayerCount: Integer;
  PorosityArray: TDataArray;
begin
  PorosityArray := Model.DataArrayManager.GetDataSetByName(rsPorosity);
  LayerCount := 0;
  Layer := -1;
  for Index := 0 to Model.Grid.LayerCount - 1 do
  begin
    LayerGroup := Model.GetLayerGroupByLayer(Index);
    if LayerGroup.RunTimeSimulated then
    begin
//      for LayerIndex := 0 to LayerGroup.ModflowLayerCount - 1 do
      begin
        Inc(LayerCount);
        Inc(Layer);
        WriteArray(PorosityArray, Layer, StrDataSet5PORLaye
          + IntToStr(LayerCount) + ': '
          + LayerGroup.AquiferName, StrNoValueAssigned, StrPOR);
      end;
    end
    else
    begin
      Inc(Layer);
      WriteArray(PorosityArray, Layer, StrDataSet5PorCBLa
        + IntToStr(LayerCount) + ': '
        + LayerGroup.AquiferName, StrNoValueAssigned, StrPorCB);
    end;
  end;
end;

procedure TModpathMainFileWriter.WriteDataSet4;
var
  ZoneDataArray: TDataArray;
begin
  ZoneDataArray := Model.DataArrayManager.GetDataSetByName(StrModpathZone);
  WriteDataSet(StrDataSet4IBOUND, ZoneDataArray);
end;

// copied from TModflowBasicWriter
procedure TModpathMainFileWriter.WriteDataSet(const DataSetName: string;
  DataArray: TDataArray);
var
  LayerIndex: Integer;
begin
  Assert(DataArray <> nil);
  if XSECTION then
  begin
    WriteCrossSectionArray(DataArray, DataSetName, DataSetName);
  end
  else
  begin
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, DataSetName + ' '
          + Model.ModflowLayerBottomDescription(LayerIndex), StrNoValueAssigned,
          DataSetName);
      end;
    end;
  end;
end;


procedure TModpathMainFileWriter.WriteDataSet3;
var
//  LayerIndex: Integer;
  LAYCON: Integer;
  LayerGroup: TLayerGroup;
  Index: Integer;
  LayerCount: Integer;
begin
  LayerCount := 0;

  for Index := 0 to Model.Grid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(Index) then
    begin
      LayerGroup := Model.GetLayerGroupByLayer(Index);
      LAYCON := LayerGroup.AquiferType;
//      for LayerIndex := 0 to LayerGroup.ModflowLayerCount - 1 do
      begin
        WriteInteger(LAYCON);
        Inc(LayerCount);
        if (LayerCount mod 10) = 0 then
        begin
          NewLine;
        end;
      end;
    end;
  end;
  if (LayerCount mod 10) <> 0 then
  begin
    NewLine;
  end;
end;

procedure TModpathMainFileWriter.WriteDataSet2;
var
  Option: string;
begin
  XSECTION := Model.ModflowGrid.RowCount = 1;
  Option := '';
  if XSECTION then
  begin
    Option := Option + ' XSECTION';
  end;
  if Model.ModflowPackages.ModPath.Compact then
  begin
    Option := Option + ' COMPACT';
  end;
  if Model.ModflowPackages.ModPath.Binary then
  begin
    Option := Option + ' BINARY';
  end;
  case Model.ModflowOptions.LengthUnit of
    0:
      // undefined
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrUndefinedLengthUni,
          StrTheLengthUnitsOf);
      end;
    1:
      // feet
      begin
      end;
    2:
      // meters
      begin
        Option := Option + ' METERS';
      end;
    3:
      // centimeters
      begin
        frmErrorsAndWarnings.AddWarning(Model, StrUnsupportedLengthU,
          StrTheLengthUnitsOf1);
      end;
  else
    Assert(False);
  end;
  Option := Trim(Option + StrDataSet2OPTIO);
  WriteString(Option);
  NewLine;
end;

procedure TModpathMainFileWriter.WriteDataSet1;
var
  MAXSIZ: Integer;
  HNOFLO: Real;
  HDRY: Real;
  NPART: Integer;
  IRCHTP: Integer;
  IEVTTP: Integer;
begin
  MAXSIZ := Model.ModflowPackages.ModPath.MaximumSize;
  HNOFLO := Model.ModflowOptions.HNoFlow;
  HDRY := Model.ModflowOptions.HDry;
  NPART := 0;
  IRCHTP := Ord(Model.ModflowPackages.ModPath.RCH_Source);
  IEVTTP := Ord(Model.ModflowPackages.ModPath.EVT_Sink);
  WriteInteger(MAXSIZ);
  WriteFloat(HNOFLO);
  WriteFloat(HDRY);
  WriteInteger(NPART);
  WriteInteger(IRCHTP);
  WriteInteger(IEVTTP);
  WriteString(StrDataSet1MAXSI);
  NewLine;
end;

{ TModpathBasicFileWriter }

constructor TModpathBasicFileWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited Create(Model, EvaluationType);
  FArrayWritingFormat := awfModflow;
  FOptions := Model.ModflowPackages.ModPath;
  FFlowTerms:= TStringList.Create;
  FIfaceTerms := TIntegerList.Create;
end;

destructor TModpathBasicFileWriter.Destroy;
begin
  FIfaceTerms.Free;
  FFlowTerms.Free;
  inherited;
end;

procedure TModpathBasicFileWriter.Evaluate;
const
  Tolerance = 0.0001;
var
  Packages: TModflowPackages;
  IFaceIndex: integer;
  ModflowGrid: TModflowGrid;
  UniformGrid: Boolean;
  MaxWidth: double;
  MinWidth: double;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidGridStructu);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidDefaultIFace);
  FMpathVersion := FOptions.MpathVersion;

  if FMpathVersion = mp7 then
  begin
    ModflowGrid := Model.ModflowGrid;
    UniformGrid := ModflowGrid.UniformColumns(MaxWidth, MinWidth);
    if not UniformGrid then
    begin
      if Abs(MinWidth/MaxWidth -1) > Tolerance then
      begin
        frmErrorsAndWarnings.AddError(Model, StrInvalidGridStructu,
          Format(StrMOSPATH7RequiresA, [StrColumn, MaxWidth, MinWidth]));
      end;
    end;
    UniformGrid := ModflowGrid.UniformRows(MaxWidth, MinWidth);
    if not UniformGrid then
    begin
      if Abs(MinWidth/MaxWidth -1) > Tolerance then
      begin
        frmErrorsAndWarnings.AddError(Model, StrInvalidGridStructu,
          Format(StrMOSPATH7RequiresA, [StrRow, MaxWidth, MinWidth]));
      end;
    end;
  end;

  FFlowTerms.Clear;
  FIfaceTerms.Clear;
  Packages := Model.ModflowPackages;
  if Packages.RchPackage.IsSelected then
  begin
    FFlowTerms.Add(RCH_ID);
    case Model.ModflowPackages.ModPath.RCH_Source of
      sapInternal: FIfaceTerms.Add(0);
      sapVertical: FIfaceTerms.Add(6);
      else Assert(False);
    end;
  end;
  if Packages.EvtPackage.IsSelected and (Model.ModelSelection <> msModflow2015)
     and (FOptions.MpathVersion <> mp7) then
  begin
    FFlowTerms.Add(EVT_ID);
    case Model.ModflowPackages.ModPath.EVT_Sink of
      sapInternal: FIfaceTerms.Add(0);
      sapVertical: FIfaceTerms.Add(6);
      else Assert(False);
    end;
  end;
  if Packages.EtsPackage.IsSelected then
  begin
    if (Model.ModelSelection = msModflow2015) and (FOptions.MpathVersion = mp7) then
    begin
      FFlowTerms.Add(EVT_ID);
    end
    else
    begin
      FFlowTerms.Add(ETS_ID);
    end;
    case Model.ModflowPackages.ModPath.Ets_Sink of
      sapInternal: FIfaceTerms.Add(0);
      sapVertical: FIfaceTerms.Add(6);
      else Assert(False);
    end;
  end;
  if Packages.Mnw2Package.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    FFlowTerms.Add(MNW2_ID);
    case Model.ModflowPackages.ModPath.Mnw2_Source of
      sapInternal: FIfaceTerms.Add(0);
      sapVertical: FIfaceTerms.Add(6);
      else Assert(False);
    end;
  end;
  if Packages.LakPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    FFlowTerms.Add(LAK_ID);
    case Model.ModflowPackages.ModPath.Lak_Source of
      sapInternal: FIfaceTerms.Add(0);
      sapVertical: FIfaceTerms.Add(6);
      else Assert(False);
    end;
  end;
  if Packages.ResPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    FFlowTerms.Add(RES_ID);
    case Model.ModflowPackages.ModPath.Res_Source of
      sapInternal: FIfaceTerms.Add(0);
      sapVertical: FIfaceTerms.Add(6);
      else Assert(False);
    end;
  end;
  if Packages.SfrPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    FFlowTerms.Add(SFR_ID1);
    FFlowTerms.Add(SFR_ID2);
    FFlowTerms.Add(SFR_ID3);
    for IFaceIndex := 1 to 3 do
    begin
      case Model.ModflowPackages.ModPath.Sfr_Source of
        sapInternal:
          begin
            FIfaceTerms.Add(0);
          end;
        sapVertical:
          begin
            FIfaceTerms.Add(6);
          end
        else Assert(False);
      end;
    end;
  end;
  if Packages.UzfPackage.IsSelected and (Model.ModelSelection <> msModflow2015) then
  begin
    FFlowTerms.Add(UZF_ID1);
//    FIfaceTerms.Add(6);
    FFlowTerms.Add(UZF_ID2);
//    FIfaceTerms.Add(6);
    FFlowTerms.Add(UZF_ID3);
//    FIfaceTerms.Add(6);
    FFlowTerms.Add(UZF_ID4);
//    FIfaceTerms.Add(6);
    FFlowTerms.Add(UZF_ID5);
//    FIfaceTerms.Add(6);
    FFlowTerms.Add(UZF_ID6);
//    FIfaceTerms.Add(6);
    FFlowTerms.Add(UZF_ID7);
//    FIfaceTerms.Add(6);
    FFlowTerms.Add(UZF_ID8);
//    FIfaceTerms.Add(6);
    for IFaceIndex := 1 to 8 do
    begin
      case Model.ModflowPackages.ModPath.Uzf_Source of
        sapInternal:
          begin
            FIfaceTerms.Add(0);
          end;
        sapVertical:
          begin
            FIfaceTerms.Add(6);
          end
        else Assert(False);
      end;
    end;
  end;
end;

class function TModpathBasicFileWriter.Extension: string;
begin
  result := '.mpbas';
end;

function TModpathBasicFileWriter.PackageID_Comment(
  APackage: TModflowPackageSelection): string;
begin
  result := File_Comment(APackage.PackageIdentifier + ' Basic Data file');
end;

procedure TModpathBasicFileWriter.WriteDataSet(const DataSetName: string;
  DataArray: TDataArray);
var
  LayerIndex: Integer;
begin
  Assert(DataArray <> nil);
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      WriteArray(DataArray, LayerIndex, DataSetName + ' '
        + Model.ModflowLayerBottomDescription(LayerIndex), StrNoValueAssigned,
        DataSetName);
    end;
  end;
end;

procedure TModpathBasicFileWriter.WriteDataSet0;
begin
  WriteCommentLine(PackageID_Comment(FOptions));
  WriteCommentLines(FOptions.Comments);
end;

procedure TModpathBasicFileWriter.WriteDataSet1;
var
  HNOFLO: Real;
  HDRY: Real;
begin
  if (Model.ModelSelection = msModflow2015) and (FOptions.MpathVersion = mp7) then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet1);
  HNOFLO := Model.ModflowOptions.HNoFlow;
  HDRY := Model.ModflowOptions.HDry;
  WriteFloat(HNOFLO);
  WriteFloat(HDRY);
  WriteString(StrDataSet1HNOFL);
  NewLine;
end;

procedure TModpathBasicFileWriter.WriteDataSet2;
begin
  frmProgressMM.AddMessage(StrWritingDataSet2);
  Assert(FFlowTerms.Count = FIfaceTerms.Count);
  WriteInteger(FFlowTerms.Count);
  WriteString(StrDataSet2Defau);
  NewLine;
end;

procedure TModpathBasicFileWriter.WriteDataSet5;
var
  LAYCON: Integer;
  LayerGroup: TLayerGroup;
  Index: Integer;
  LayerCount: Integer;
begin
  if (Model.ModelSelection = msModflow2015) and (FOptions.MpathVersion = mp7) then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet5);
  LayerCount := 0;

  for Index := 0 to Model.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(Index) then
    begin
      LayerGroup := Model.GetLayerGroupByLayer(Index);
      LAYCON := LayerGroup.AquiferType;
      if not (LAYCON in [0,1]) then
      begin
        LAYCON := 1;
      end;
      WriteInteger(LAYCON);
      Inc(LayerCount);
      if ((LayerCount mod 10) = 0) or (Index = Model.LayerCount - 1) then
      begin
        NewLine;
      end;
    end;
  end;
end;

procedure TModpathBasicFileWriter.WriteDataSet6;
var
  ZoneDataArray: TDataArray;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet6);
  ZoneDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  WriteDataSet(StrDataSet6IBOUND, ZoneDataArray);
end;

procedure TModpathBasicFileWriter.WriteDataSets3and4;
var
  index: Integer;
begin
  frmProgressMM.AddMessage(StrWritingDataSets3and4);
  for index := 0 to FFlowTerms.Count - 1 do
  begin
    WriteString(FFlowTerms[index]);
    NewLine;
    WriteInteger(FIfaceTerms[index]);
    NewLine;
    if (FMpathVersion = mp5) and (FIfaceTerms[index] < 0)then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidDefaultIFace,
        StrDefaultIFaceValueMu);
    end;
  end;
end;

procedure TModpathBasicFileWriter.WriteDataSets7and8;
var
  LayerGroup: TLayerGroup;
  Index: Integer;
  Layer: Integer;
  LayerCount: Integer;
  PorosityArray: TDataArray;
  WrongVersion: Boolean;
begin
  frmProgressMM.AddMessage(StrWritingDataSets7and8);
  PorosityArray := Model.DataArrayManager.GetDataSetByName(rsPorosity);
  LayerCount := 0;
  Layer := -1;
  WrongVersion := False;
  for Index := 0 to Model.LayerCount - 1 do
  begin
    LayerGroup := Model.GetLayerGroupByLayer(Index);
    if LayerGroup.RunTimeSimulated then
    begin
      Inc(LayerCount);
      Inc(Layer);
      WriteArray(PorosityArray, Layer, StrDataSet7Porosity
        + IntToStr(LayerCount) + ': '
        + LayerGroup.AquiferName, StrNoValueAssigned, StrPorosity);
    end
    else
    begin
      Inc(Layer);
      WriteArray(PorosityArray, Layer, StrDataSet8Porosity
        + IntToStr(LayerCount) + ': '
        + LayerGroup.AquiferName, StrNoValueAssigned, StrPorosityCB);

      if FMpathVersion <> mp6 then
      begin
        WrongVersion := True;
      end;
    end;
  end;
  if WrongVersion then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidGridStructu,
      StrNonsimulatedLayers);
  end;
end;

procedure TModpathBasicFileWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  Evaluate;
  NameOfFile := FileName(AFileName);
  Model.AddModpathInputFile(NameOfFile);
  FInputFileName := NameOfFile; 
  OpenFile(NameOfFile);
  try
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet1;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets3and4;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet5;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet6;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets7and8;
  finally
    CloseFile;
  end;
end;

end.
