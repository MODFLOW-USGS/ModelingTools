unit Modflow6ModelImporter;

interface

uses
  Winapi.Windows, System.Classes, System.IOUtils, Vcl.Dialogs, System.SysUtils, System.UITypes,
  Mf6.SimulationNameFileReaderUnit, System.Math, Mf6.CustomMf6PersistentUnit,
  ScreenObjectUnit, DataSetUnit, System.Generics.Collections,
  System.Generics.Defaults, Mf6.ObsFileReaderUnit, ModflowLakMf6Unit,
  Mf6.MvrFileReaderUnit, GoPhastTypes, ModflowPackageSelectionUnit, FastGEO;

  // The first name in NameFiles must be the name of the groundwater flow
  // simulation name file (mfsim.nam). Any additional names must be associated
  // transport simulation name files (mfsim.nam)

type
  TMvrSourcePackageChoice = (mspcWel, mspcDrn, mspcRiv, mspcGhb, mspcLak, mspcMaw,
    mspcSfr, mspcUzf);

  TMvrReceiverPackageChoice = (mrpcLak, mrpcMaw, mrpcSfr, mrpcUzf);


  TimeSeriesMap = TDictionary<string, string>;
  TBoundNameDictionary = TDictionary<string, TObservationList>;
  TCellIdObsDictionary = TDictionary<TMfCellId, TObservationList>;
  TNumberDictionary = TDictionary<Integer, TObservationList>;
  TObsLists = TObjectList<TObservationList>;

  TMvrKey = record
    ID: Integer;
    PackageName: string;
    Period: Integer;
  end;

  TTMvrKeyyComparer = class(TEqualityComparer<TMvrKey>)
    function Equals(const Left, Right: TMvrKey): Boolean; override;
    function GetHashCode(const Value: TMvrKey): Integer; override;
  end;

  TMvrSource = record
    ScreenObject: TScreenObject;
    LakeOutlet: TLakeOutletItem;
    PackageName: string;
    Period: Integer;
    IDs: TArray<Integer>;
    SourceType: TMvrSourcePackageChoice;
    function Key(Index: Integer): TMvrKey;
  end;

  TMvrSourceList = class(TList<TMvrSource>)
    procedure Sort;
  end;

  TMvrReceiver = record
    ScreenObject: TScreenObject;
    PackageName: string;
    Period: Integer;
    IDs: TArray<Integer>;
    ReceiverType: TMvrReceiverPackageChoice;
    function Key(Index: Integer): TMvrKey;
  end;

  TMvrReceiverList = class(TList<TMvrReceiver>)
    procedure Sort;
  end;

  TMvrSourceDictionary = TDictionary<TMvrKey, TMvrSource>;
  TMvrReceiverDictionary = TDictionary<TMvrKey, TMvrReceiver>;

  TModflow6Importer = class(TObject)
  private
    FErrorMessages: TStringList;
    FSimulation: TMf6Simulation;
    FFlowModel: TModel;
    FAllTopCellsScreenObject: TScreenObject;
    FModelNameFile: string;
    TSIndex: Integer;
    FSimulations: TObjectList<TMf6Simulation>;
    FMvrSources: TMvrSourceList;
    FMvrReceivers: TMvrReceiverList;
    FOnUpdataStatusBar: TOnUpdataStatusBar;
    FMinPoint: TPoint2D;
    FMinPointAssigned: Boolean;
    procedure ImportFlowModelTiming;
    procedure ImportSimulationOptions;
    procedure ImportSolutionGroups;
    function ImportFlowModel: Boolean;
    procedure ImportDis(Package: TPackage);
    procedure ImportDisV(Package: TPackage);
    procedure UpdateLayerStructure(NumberOfLayers: Integer);
    procedure CreateAllTopCellsScreenObject;
    function GetAllTopCellsScreenObject: TScreenObject;
    procedure SetOnUpdataStatusBar(const Value: TOnUpdataStatusBar);
    function BoundaryValuesToFormula(Values: TMf6BoundaryValueArray;
      Name: string; Map: TimeSeriesMap; ScreenObject: TScreenObject = nil): string;
    function RealValuesToFormula(Values: TOneDRealArray; Name: string;
      ScreenObject: TScreenObject = nil): string;
    property AllTopCellsScreenObject: TScreenObject read GetAllTopCellsScreenObject;
    procedure AssignRealValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: TDArray2D);
    procedure AssignIntegerValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: TIArray2D);
    procedure AssignBooleanValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: TBArray2D); overload;
    procedure AssignBooleanValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: TIArray2D); overload;
    procedure AssignIDomain(IDOMAIN: TIArray3D; NumberOfLayers: Integer);
    procedure AssignBOTM(BOTM: TDArray3D);
    procedure AssignTOP(TOP: TDArray2D);
    procedure ImportIc(Package: TPackage);
    procedure Assign3DRealDataSet(DsName: string; Data: TDArray3D);
    procedure Assign3DIntegerDataSet(DsName: string; Data: TIArray3D);
    procedure Assign3DBooleanDataSet(DsName: string; Data: TIArray3D);
    procedure ImportOc(Package: TPackage);
    procedure ImportGwfObs(Package: TPackage);
    procedure ImportNpf(Package: TPackage);
    procedure ImportTvk(Package: TPackage);
    procedure ImportTimeSeries(Package: TPackage; Map: TimeSeriesMap);
    procedure ImportHfb(Package: TPackage);
    procedure ImportSto(Package: TPackage);
    procedure ImportTvs(Package: TPackage);
    procedure ImportCSub(Package: TPackage);
    procedure ImportBuy(Package: TPackage);
    procedure ImportVsc(Package: TPackage);
    procedure ImportChd(Package: TPackage; TransportModels: TModelList);
    procedure GetObservations(NumberObsDictionary: TNumberDictionary;
      BoundNameObsDictionary: TBoundNameDictionary;
      CellIdObsDictionary: TCellIdObsDictionary; ObsLists: TObsLists;
      ObsFiles: TObs);
    procedure AddPointsToScreenObject(CellIds: TCellIdList;
      AScreenObject: TScreenObject; ThreeD: Boolean = True);
    procedure ImportWel(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportDrn(Package: TPackage; MvrPackage: TPackage);
    procedure ImportRiv(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportGhb(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportRch(Package: TPackage; TransportModels: TModelList);
    procedure ImportEvt(Package: TPackage; TransportModels: TModelList);
    procedure ImportMaw(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportSfr(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportLak(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportUzf(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    function GetMvr(MvrPackage, Package: TPackage): TMvr;
    procedure ImportMvr(Package: TPackage);
    procedure ImportGnc(Package: TPackage);
    function GetIms(ModelName: string): TSmsPackageSelection;
    procedure ImportIMS;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure ImportModflow6Model(NameFiles, ErrorMessages: TStringList);
    property OnUpdateStatusBar: TOnUpdataStatusBar read FOnUpdataStatusBar
      write SetOnUpdataStatusBar;
  end;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, frmSelectFlowModelUnit,
  Mf6.TDisFileReaderUnit, ModflowTimeUnit, ModflowOptionsUnit,
  Mf6.AtsFileReaderUnit, ModflowOutputControlUnit,
  Mf6.NameFileReaderUnit, Mf6.DisFileReaderUnit, LayerStructureUnit,
  UndoItems, AbstractGridUnit, ValueArrayStorageUnit,
  InterpolationUnit, GIS_Functions, RbwParser, DataSetNamesUnit,
  Mf6.DisvFileReaderUnit, ModflowIrregularMeshUnit, Mf6.IcFileReaderUnit,
  Mf6.OcFileReaderUnit, Modflow6ObsUnit,
  Mf6.NpfFileReaderUnit, Mf6.TvkFileReaderUnit, ModflowTvkUnit,
  Mf6.TimeSeriesFileReaderUnit, Modflow6TimeSeriesCollectionsUnit,
  Modflow6TimeSeriesUnit, Mf6.HfbFileReaderUnit, ModflowHfbUnit,
  Mf6.StoFileReaderUnit, Mf6.TvsFileReaderUnit, ModflowTvsUnit,
  Mf6.CSubFileReaderUnit, ModflowCSubInterbed, ModflowCsubUnit,
  DataArrayManagerUnit, Mf6.BuyFileReaderUnit, Mt3dmsChemSpeciesUnit,
  Mf6.VscFileReaderUnit, Mf6.ChdFileReaderUnit, Mf6.CncFileReaderUnit,
  Mf6.SsmFileReaderUnit, ModflowBoundaryUnit, ModflowConstantHeadBoundaryUnit,
  Mf6.WelFileReaderUnit, ModflowWellUnit,
  Mf6.DrnFileReaderUnit, ModflowDrnUnit, Mf6.RivFileReaderUnit, ModflowRivUnit,
  Mf6.GhbFileReaderUnit, ModflowGhbUnit, Mf6.RchFileReaderUnit, ModflowRchUnit,
  ModflowEtsUnit, Mf6.EvtFileReaderUnit, ModflowEvtUnit, Mf6.MawFileReaderUnit,
  ModflowMawUnit, ModflowGridUnit, ModflowSfr6Unit, Mf6.SfrFileReaderUnit,
  Mf6.CrossSectionFileReaderUnit, Mf6.LakFileReaderUnit,
  Mf6.LakeTableFileReaderUnit, Mf6.UzfFileReaderUnit, IntListUnit,
  ConvexHullUnit, CellLocationUnit, ModflowUzfMf6Unit, System.Hash,
  ModflowMvrUnit, frmErrorsAndWarningsUnit, Mf6.GncFileReaderUnit,
  ModflowGncUnit, Mf6.ImsFileReaderUnit, frmImportWarningsUnit;

resourcestring
  StrTheNameFileSDoe = 'The name file %s does not exist.';
  StrModelMuseCanNotIm = 'ModelMuse can not import AUXMULTNAME specified as ' +
  'a time series.';
  StrModelMuseCanNotAp = 'ModelMuse can not apply AUXMULTNAME values to data' +
  ' specified as a time series.';
  StrModelMuseCanNotSpPetm0 = 'ModelMuse can not specify a separate value fo' +
  'r petm0 in the EVt package.';

//type
//  TMvrLinks = class(TObject)
//  private
//    WellDonors: TMvrDictionarys;
//    DrainDonors: TMvrDictionarys;
//    GhbDonors: TMvrDictionarys;
//    RivDonors: TMvrDictionarys;
//    MawDonors: TMvrDictionarys;
//    LakDonors: TMvrDictionarys;
//    SfrDonors: TMvrDictionarys;
//    UzfDonors: TMvrDictionarys;
//    MawReceivers: TMvrDictionarys;
//    LakReceivers: TMvrDictionarys;
//    SfrReceivers: TMvrDictionarys;
//    UzfReceivers: TMvrDictionarys;
//  public
//    constructor Create;
//    destructor Destroy; override;
//  end;


procedure TModflow6Importer.AssignBooleanValuesToCellCenters(
  DataArray: TDataArray; ScreenObject: TScreenObject; ImportedData: TBArray2D);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesB
    + '("' + DataArray.Name + '")';
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtBoolean;
  ImportedValues.Values.Count := Model.RowCount * Model.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
      ImportedValues.Values.BooleanValues[PointIndex] :=
        ImportedData[RowIndex, ColIndex];
      Inc(PointIndex);
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.Values.CacheData;
end;

procedure TModflow6Importer.AssignIntegerValuesToCellCenters(
  DataArray: TDataArray; ScreenObject: TScreenObject; ImportedData: TIArray2D);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesI;
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtInteger;
  ImportedValues.Values.Count := Model.RowCount * Model.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
//      APoint := Grid.TwoDElementCenter(ColIndex, RowIndex);
//      if (FImporter.FImportParameters.Outline = nil)
//        or FImporter.FImportParameters.Outline.PointInside(APoint) then
      begin
        ImportedValues.Values.IntValues[PointIndex] :=
          ImportedData[RowIndex, ColIndex];
        Inc(PointIndex);
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.Values.CacheData;
end;

procedure TModflow6Importer.AssignRealValuesToCellCenters(DataArray: TDataArray;
  ScreenObject: TScreenObject; ImportedData: TDArray2D);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + DataArray.Name + '")';
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := Model.RowCount * Model.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
//      APoint := Grid.TwoDElementCenter(ColIndex, RowIndex);
//      if (FImporter.FImportParameters.Outline = nil)
//        or FImporter.FImportParameters.Outline.PointInside(APoint) then
      begin
        ImportedValues.Values.RealValues[PointIndex] :=
          ImportedData[RowIndex, ColIndex];
        Inc(PointIndex);
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.Values.CacheData;
end;

constructor TModflow6Importer.Create;
begin
  TSIndex := 0;
  FSimulations := TObjectList<TMf6Simulation>.Create;
  FMvrSources := TMvrSourceList.Create;
  FMvrReceivers := TMvrReceiverList.Create;

end;

procedure TModflow6Importer.CreateAllTopCellsScreenObject;
var
  UndoCreateScreenObject: TCustomUndo;
  RowIndex: Integer;
  ColIndex: Integer;
  APoint: TPoint2D;
  Model: TPhastModel;
begin
  Assert(FAllTopCellsScreenObject = nil);
    Model := frmGoPhast.PhastModel;
    FAllTopCellsScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    FAllTopCellsScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(FAllTopCellsScreenObject);
    FAllTopCellsScreenObject.ElevationCount := ecZero;
//    if FImporter.FImportParameters.AssignmentMethod = camInterpolate then
//    begin
//      FAllTopCellsScreenObject.SetValuesByInterpolation := True;
//    end
//    else
//    begin
      FAllTopCellsScreenObject.SetValuesOfIntersectedCells := True;
//    end;
    FAllTopCellsScreenObject.EvaluatedAt := eaBlocks;
    FAllTopCellsScreenObject.Visible := False;
    FAllTopCellsScreenObject.Capacity := Model.RowCount * Model.ColumnCount;
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        APoint := Model.TwoDElementCenter(ColIndex, RowIndex);
        FAllTopCellsScreenObject.AddPoint(APoint, True);
      end;
    end;
    FAllTopCellsScreenObject.Name := 'Imported_Arrays';
    FAllTopCellsScreenObject.SectionStarts.CacheData;
end;

destructor TModflow6Importer.Destroy;
begin
  FMvrSources.Free;
  FMvrReceivers.Free;
  FSimulations.Free;
  inherited;
end;

function TModflow6Importer.GetAllTopCellsScreenObject: TScreenObject;
begin
  if FAllTopCellsScreenObject = nil then
  begin
    CreateAllTopCellsScreenObject;
  end;
  result := FAllTopCellsScreenObject;
end;

function TModflow6Importer.GetIms(ModelName: string): TSmsPackageSelection;
var
  SimIndex: Integer;
  Simulation: TMf6Simulation;
  ChemIndex: Integer;
  ModelIndex: Integer;
  AModel: TModel;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  ChemIndex := 0;
  result := nil;
  for SimIndex := 0 to FSimulations.Count - 1 do
  begin
    Simulation := FSimulations[SimIndex];
    for ModelIndex := 0 to Simulation.Models.Count - 1 do
    begin
      AModel := Simulation.Models[ModelIndex];
      if AnsiSameText(AModel.ModelName, ModelName) then
      begin
        if AnsiSameText(AModel.ModelType, 'GWF6') then
        begin
          result := Model.ModflowPackages.SmsPackage;
          Exit;
        end
        else if AnsiSameText(AModel.ModelType, 'GWT6') then
        begin
          While Model.ModflowPackages.GwtPackages.Count <= ChemIndex do
          begin
            Model.ModflowPackages.GwtPackages.Add;
          end;
          result := Model.ModflowPackages.GwtPackages[ChemIndex].GwtIms;
          Exit;
        end
        else
        begin
          Assert(False);
        end;
      end;
      if AnsiSameText(AModel.ModelType, 'GWT6') then
      begin
        Inc(ChemIndex);
      end;
    end;
  end;
end;

procedure TModflow6Importer.ImportBuy(Package: TPackage);
var
  Buy: TBuy;
  Model: TPhastModel;
  BuoyancyPackage: TBuoyancyPackage;
  Options: TBuyOptions;
  PackageData: TBuyPackageData;
  index: Integer;
  Item: TBuyItem;
  ChemComponents: TMobileChemSpeciesCollection;
  ChemItem: TMobileChemSpeciesItem;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing BUY package');
  end;
  Model := frmGoPhast.PhastModel;
  BuoyancyPackage := Model.ModflowPackages.BuoyancyPackage;
  BuoyancyPackage.IsSelected := True;

  Buy := Package.Package as TBuy;
  Options := Buy.Options;
  if Options.HHFORMULATION_RHS.Used then
  begin
    BuoyancyPackage.RightHandSide := True;
  end;
  if Options.DENSEREF.Used then
  begin
    BuoyancyPackage.RefDensity := Options.DENSEREF.Value;
  end;
  if Options.DENSITY.Used then
  begin
    BuoyancyPackage.WriteDensity := True;
  end;

  ChemComponents := Model.MobileComponents;
  PackageData := Buy.PackageData;
  for index := 0 to PackageData.Count - 1 do
  begin
    Item := PackageData[index];
    ChemItem := ChemComponents.GetItemByName(Item.auxspeciesname);
    if ChemItem = nil then
    begin
      ChemItem := ChemComponents.Add;
      ChemItem.Name := Item.auxspeciesname;
    end;
    if SameText(ChemItem.Name, 'Density') then
    begin
      BuoyancyPackage.DensitySpecified := True;
    end;
    ChemItem.DensitySlope := Item.drhodc;
    ChemItem.RefConcentration := Item.crhoref;
  end;
end;

type
  TChdConnection = class(TObject)
    ScreenObject: TScreenObject;
    List: TChdTimeItemList;
    IFACE: Integer;
    destructor Destroy; override;
  end;

  TChdConnectionObjectList = TObjectList<TChdConnection>;
  TChdConnectionObjectLists = TObjectList<TChdConnectionObjectList>;

procedure TModflow6Importer.ImportChd(Package: TPackage; TransportModels: TModelList);
var
  Model: TPhastModel;
  Chd: TChd;
  AModel: TModel;
  APackage: TPackage;
  ModelIndex: Integer;
  TransportModel: TTransportNameFile;
  PackageIndex: Integer;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary;
  Ssm: TSsm;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ObsLists: TObsLists;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  SourceIndex: Integer;
  FoundMatch: Boolean;
  TransportAuxNames: TStringList;
  PeriodIndex: Integer;
  APeriod: TChdPeriod;
  CellIndex: Integer;
  ACell: TChdTimeItem;
  IfaceIndex: Integer;
  KeyStringDictionary: TDictionary<string, TChdTimeItemList>;
  CellLists: TObjectList<TChdTimeItemList>;
  ACellList: TChdTimeItemList;
  Options: TChdOptions;
  IFace: Integer;
  LastTime: Double;
  Imported_Heads: TValueArrayItem;
  ItemList: TList<TChdItem>;
  StartTime: Double;
  ChdIndex: Integer;
  AnItem: TChdItem;
  KeyString: string;
  TimeSeries: string;
  ImportedTimeSeries: string;
  ObjectCount: Integer;
  ObjectIndex: Integer;
  FirstCell: TChdTimeItem;
  BoundName: string;
  ConnectionObjectLists: TChdConnectionObjectLists;
  ConnectionDictionary: TDictionary<string, TChdConnectionObjectList>;
  AConnectionList: TChdConnectionObjectList;
  ConnectionIndex: Integer;
  ConnectionItem: TChdConnection;
  AScreenObject: TScreenObject;
  AuxIFACE: TMf6BoundaryValue;
  AuxIndex: Integer;
  ChemSpeciesName: string;
  Aux: TMf6BoundaryValue;
  GwtAuxIndex: Integer;
  Values: TValueArrayStorage;
  CellIds: TCellIdList;
  AddCells: Boolean;
  OtherCellLists: TObjectList<TChdTimeItemList>;
  CellListIndex: Integer;
  procedure AddItem(AScreenObject: TScreenObject; ACell: TChdTimeItem; Period: Integer);
  var
    ChdItem: TChdItem;
    ImportedName: string;
    Concentrations: TChdGwtConcCollection;
    ChemSpeciesName: string;
    ConcItem: TGwtConcStringValueItem;
    GwtAuxIndex: Integer;
    AuxIndex: Integer;
    Aux: TMf6BoundaryValue;
    Imported_Chem: TValueArrayItem;
  begin
    ChdItem := AScreenObject.ModflowChdBoundary.Values.Add as TChdItem;
    ItemList.Add(ChdItem);
    ChdItem.EndTime := LastTime;
    ChdItem.StartTime := StartTime;

    if ACell.Head.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_%s_ CHD_Period_%d', [Package.PackageName, Period]);
      Imported_Heads := AScreenObject.ImportedValues.Add;
      Imported_Heads.Name := ImportedName;
      Imported_Heads.Values.DataType := rdtDouble;
      ChdItem.StartHead := rsObjectImportedValuesR + '("' + Imported_Heads.Name + '")';
    end
    else
    begin
      Imported_Heads := nil;
      TimeSeries := ACell.Head.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      ChdItem.StartHead := ImportedTimeSeries;
    end;
    ChdItem.EndHead := ChdItem.StartHead;

    if TransportAuxNames.Count > 0 then
    begin
      Concentrations := ChdItem.GwtConcentrations;
      Concentrations.Count := TransportAuxNames.Count;
      for AuxIndex := 0 to TransportAuxNames.Count - 1 do
      begin
        ChemSpeciesName := TransportAuxNames[AuxIndex];
        ConcItem := Concentrations[AuxIndex];

        GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
        Assert(GwtAuxIndex >= 0);
        Aux := ACell[GwtAuxIndex];
        if Aux.ValueType = vtNumeric then
        begin
          ImportedName := 'Imported_' + ChemSpeciesName ;
          Imported_Chem := AScreenObject.ImportedValues.Add;
          Imported_Chem.Name := ImportedName;
          Imported_Chem.Values.DataType := rdtDouble;
          ConcItem.Value := rsObjectImportedValuesR + '("' + Imported_Chem.Name + '")';
          TransportAuxNames.Objects[AuxIndex] := Imported_Chem.Values;
        end
        else
        begin
          TransportAuxNames.Objects[AuxIndex] := nil;
          TimeSeries := Aux.StringValue;
          if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
          begin
            Assert(False);
          end;
          ConcItem.Value := ImportedTimeSeries;
        end;
      end;
    end;
  end;
  procedure CreateObsScreenObject(ACell: TChdTimeItem);
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    CellId: TMfCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_CHD_Obs_%d', [Package.PackageName, ObjectCount]));
    AScreenObject.Name := NewName;
    AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(AScreenObject);
    AScreenObject.ElevationCount := ecOne;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.EvaluatedAt := eaBlocks;
    AScreenObject.Visible := False;
    AScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    AScreenObject.CreateMf6Obs;
    AScreenObject.Modflow6Obs.Name := ACell.Boundname;
    AScreenObject.Modflow6Obs.General := [ogCHD];

    CellId := ACell.Cellid;
    if Model.DisvUsed then
    begin
      CellId.Row := 1;
    end;
    ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
    APoint.x := ElementCenter.RotatedLocation.x;
    APoint.y := ElementCenter.RotatedLocation.y;
    AScreenObject.AddPoint(APoint, True);
    AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

  end;
  function CreateScreenObject(ACell: TChdTimeItem; Period: Integer): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    AuxIFACE: TMf6BoundaryValue;
    BoundName: string;
    ObsList: TObservationList;
    AnObs: TObservation;
    ObsIndex: Integer;
  begin
    Inc(ObjectCount);
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_CHD_%d_Period_%d', [Package.PackageName, ObjectCount, Period]));
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateChdBoundary;
    if IfaceIndex >= 0 then
    begin
      AuxIFACE := ACell[IfaceIndex];
      Assert(AuxIFACE.ValueType = vtNumeric);
      IFACE := Round(AuxIFACE.NumericValue);
    end
    else
    begin
      IFACE := 0;
    end;
    result.IFACE := TIface(IFACE+2);

    AddItem(result, ACell, Period);

    BoundName := UpperCase(ACell.Boundname);
    if BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      result.CreateMf6Obs;
      for ObsIndex := 0 to ObsList.Count - 1 do
      begin
        AnObs := ObsList[ObsIndex];
        Assert(AnsiSameText(AnObs.ObsType, 'chd'));
      end;
      result.Modflow6Obs.Name := ACell.Boundname;
      result.Modflow6Obs.General := [ogCHD];
    end;
  end;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing CHD package');
  end;
  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.ChdBoundary.IsSelected := True;

  Chd := Package.Package as TChd;
  Options := Chd.Options;

  OtherCellLists := TObjectList<TChdTimeItemList>.Create;
  CellIds := TCellIdList.Create;
  ConnectionObjectLists := TChdConnectionObjectLists.Create;
  ConnectionDictionary := TDictionary<string, TChdConnectionObjectList>.Create;
  ItemList := TList<TChdItem>.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  CellIdObsDictionary := TCellIdObsDictionary.Create;
  Map := TimeSeriesMap.Create;
  ObsLists := TObsLists.Create;
  KeyStringDictionary := TDictionary<string, TChdTimeItemList>.Create;
  CellLists := TObjectList<TChdTimeItemList>.Create;
  try
    OtherCellLists.OwnsObjects := False;
  try
    IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
    for TimeSeriesIndex := 0 to Chd.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := Chd.TimeSeries[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;

    if Chd.ObservationCount > 0 then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    end;
    for ObsPackageIndex := 0 to Chd.ObservationCount - 1 do
    begin
      ObsFiles := Chd.Observations[ObsPackageIndex].Package as TObs;
      GetObservations(nil, BoundNameObsDictionary,
        CellIdObsDictionary, ObsLists, ObsFiles);
    end;
    if Assigned(OnUpdateStatusBar) then
    begin
      OnUpdateStatusBar(self, 'importing CHD package');
    end;

    TransportAuxNames := TStringList.Create;
    try
      TransportAuxNames.CaseSensitive := False;
      for ModelIndex := 0 to TransportModels.Count - 1 do
      begin
        AModel := TransportModels[ModelIndex];
        TransportModel := AModel.FName as TTransportNameFile;
        for PackageIndex := 0 to TransportModel.NfPackages.Count  - 1 do
        begin
          APackage := TransportModel.NfPackages[PackageIndex];
          FoundMatch := False;
          if APackage.FileType = 'SSM6' then
          begin
            Ssm := APackage.Package as TSsm;
            for SourceIndex := 0 to Ssm.Sources.Count - 1 do
            begin
              if SameText(Ssm.Sources[SourceIndex].pname, Package.PackageName) then
              begin
                FoundMatch := True;
                TransportAuxNames.Add(Ssm.Sources[SourceIndex].auxname);
                break;
              end;
            end;
//            Assert(FoundMatch);
            break;
          end;
        end;
      end;

      LastTime := Model.ModflowStressPeriods.Last.EndTime;

      ObjectCount := 0;
      for PeriodIndex := 0 to Chd.PeriodCount - 1 do
      begin
        APeriod := Chd.Periods[PeriodIndex];
        StartTime := Model.ModflowStressPeriods[APeriod.Period-1].StartTime;
        for ChdIndex := 0 to ItemList.Count - 1 do
        begin
          AnItem := ItemList[ChdIndex];
          AnItem.EndTime := StartTime;
        end;
        ItemList.Clear;
        for CellListIndex := 0 to CellLists.Count - 1 do
        begin
          CellLists[CellListIndex].Clear;
        end;

        for CellIndex := 0 to APeriod.Count - 1 do
        begin
          ACell := APeriod[CellIndex];

          if (ACell.Boundname <> '')
            and BoundNameObsDictionary.ContainsKey(UpperCase(ACell.Boundname)) then
          begin
            KeyString := 'BN:' + UpperCase(ACell.Boundname) + ' ';
          end
          else
          begin
            KeyString := '';
          end;

          if IfaceIndex < 0 then
          begin
            IFACE := 0;
          end
          else
          begin
            AuxIFACE := ACell[IfaceIndex];
            Assert(AuxIFACE.ValueType = vtNumeric);
            IFACE := Round(AuxIFACE.NumericValue);
          end;
          KeyString := KeyString + ACell.Keystring + ' IFACE:' + IntToStr(IFACE);
          if not KeyStringDictionary.TryGetValue(KeyString, ACellList) then
          begin
            ACellList := TChdTimeItemList.Create;
            CellLists.Add(ACellList);
            KeyStringDictionary.Add(KeyString, ACellList);
          end;
          ACellList.Add(ACell);
        end;
        for ObjectIndex := 0 to CellLists.Count - 1 do
        begin
          AddCells := True;
          ACellList := CellLists[ObjectIndex];
          if ACellList.Count > 0 then
          begin
            FirstCell := ACellList[0];
            if (FirstCell.Boundname <> '')
              and BoundNameObsDictionary.ContainsKey(UpperCase(FirstCell.Boundname)) then
            begin
              if IfaceIndex < 0 then
              begin
                IFACE := 0;
              end
              else
              begin
                AuxIFACE := FirstCell[IfaceIndex];
                Assert(AuxIFACE.ValueType = vtNumeric);
                IFACE := Round(AuxIFACE.NumericValue);
              end;
              BoundName := UpperCase(FirstCell.Boundname);
              if not ConnectionDictionary.TryGetValue(BoundName, AConnectionList) then
              begin
                AConnectionList := TChdConnectionObjectList.Create;
                ConnectionObjectLists.Add(AConnectionList);
                ConnectionDictionary.Add(BoundName, AConnectionList)
              end;
              ACellList.Sort;
              AScreenObject := nil;
              for ConnectionIndex := 0 to AConnectionList.Count - 1 do
              begin
                ConnectionItem := AConnectionList[ConnectionIndex];
                if (IFACE = ConnectionItem.IFACE)
                  and ACellList.SameCells(ConnectionItem.List) then
                begin
                  AScreenObject := ConnectionItem.ScreenObject;
                  AddCells := False;
                  Break;
                end;
              end;
              if AScreenObject = nil then
              begin
                AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
                ConnectionItem := TChdConnection.Create;
                ConnectionItem.ScreenObject := AScreenObject;
                ConnectionItem.IFACE := IFACE;
                ConnectionItem.List := ACellList;
                AConnectionList.Add(ConnectionItem);
                OtherCellLists.Add(ACellList);
  //                CellLists.OwnsObjects := False;
  //                try
  //                  CellLists[ObjectIndex] := nil;
  //                finally
  //                  CellLists.OwnsObjects := True;
  //                end;
              end
              else
              begin
                AddItem(AScreenObject, FirstCell, APeriod.Period);
              end;
            end
            else
            begin
              AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
            end;
          end;

          CellIds.Clear;
          for CellIndex := 0 to ACellList.Count - 1 do
          begin
            ACell := ACellList[CellIndex];
            if ACell.Head.ValueType = vtNumeric then
            begin
              Imported_Heads.Values.Add(ACell.Head.NumericValue);
            end;

            for AuxIndex := 0 to TransportAuxNames.Count - 1 do
            begin
              ChemSpeciesName := TransportAuxNames[AuxIndex];
              GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
              Aux := ACell[GwtAuxIndex];
              if Aux.ValueType = vtNumeric then
              begin
                Values := TransportAuxNames.Objects[AuxIndex] as TValueArrayStorage;
                Values.Add(Aux.NumericValue);
              end;
            end;

            CellIds.Add(ACell.Cellid);

            if CellIdObsDictionary.ContainsKey(ACell.Cellid) then
            begin
              CreateObsScreenObject(ACell);
            end;
          end;
          if AddCells then
          begin
            AddPointsToScreenObject(CellIds, AScreenObject);
          end;
        end;
      end;

    finally
      TransportAuxNames.Free;
    end;

  finally
    for CellListIndex := 0 to OtherCellLists.Count - 1 do
    begin
      CellLists.Extract(OtherCellLists[CellListIndex])
    end;
  end;
  finally
    BoundNameObsDictionary.Free;
    CellIdObsDictionary.Free;
    Map.Free;
    ObsLists.Free;
    KeyStringDictionary.Free;
    CellLists.Free;
    ItemList.Free;
    ConnectionObjectLists.Free;
    ConnectionDictionary.Free;
    CellIds.Free;
    OtherCellLists.Free;
  end;
end;

procedure TModflow6Importer.ImportCSub(Package: TPackage);
var
  Model: TPhastModel;
  CSub: TCSub;
  CSubPackage: TCSubPackageSelection;
  Options: TCSubOptions;
  OutputTypes: TCsubOutputTypes;
  DelayCounts: array of array of array of Integer;
  NoDelayCounts: array of array of array of Integer;
  PackageData: TMf6CSubPackageData;
  Index: Integer;
  Item: TMf6CSubItem;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColumnIndex: Integer;
  MaxDelay: Integer;
  MaxNoDelay: Integer;
  NoDelayLists: TObjectList<TCSubItemList>;
  DelayLists: TObjectList<TCSubItemList>;
  List: TCSubItemList;
  Interbed: TCSubInterbed;
  GridData: TCSubGridData;
  DataArrayName: string;
  Map: TimeSeriesMap;
  TimeSeriesPackage: TPackage;
  TimeSeriesIndex: Integer;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  IcsubnoObsDictionary: TNumberDictionary;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary;
  ObsLists: TObsLists;
  ObsList: TObservationList;
  PriorItem: TMf6CSubItem;
  PriorBoundName: string;
  BoundName: string;
  PriorItemAssigned: Boolean;
  ObjectCount: Integer;
  StartTime: double;
  LastTime: double;
  AScreenObject: TScreenObject;
  NoDelayInterbeds: TList<TCSubInterbed>;
  DelayInterbeds: TList<TCSubInterbed>;
  PackageItem: TCSubPackageData;
  InterbedIndex: Integer;
  pcs0: TValueArrayItem;
  thick_frac: TValueArrayItem;
  rnb: TValueArrayItem;
  ssv_cc: TValueArrayItem;
  sse_cr: TValueArrayItem;
  theta: TValueArrayItem;
  kv: TValueArrayItem;
  h0: TValueArrayItem;
  CellId: TMfCellId;
  ElementCenter: TDualLocation;
  APoint: TPoint2D;
  BName: TStringOption;
  Obs: TObservation;
  PeriodIndex: Integer;
  APeriod: TCSubPeriod;
  CellIndex: Integer;
  ACell: TCSubTimeItem;
  PriorTimeSeriesAssigned: Boolean;
  PriorTimeSeries: string;
  TimeSeries: string;
  PriorScreenObjects: TScreenObjectList;
  ScreenObjectIndex: Integer;
  TimeItem: TCSubItem;
  Formula: string;
  ImportedTimeSeriesName: String;
  ImportedName: string;
  sig0: TValueArrayItem;
  ObsListIndex: Integer;
  ObsNameIndex: Integer;
  function CreateScreenObject(BoundName: String; Period: Integer): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    NewItem: TCSubItem;
    CSubPackageData: TCSubPackageDataCollection;
    Index: Integer;
    ImportedName: string;
  begin
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    if BoundName <> '' then
    begin
      NewName := ValidName('ImportedCSUB_' + BoundName);
    end
    else
    begin
      if Period > 0 then
      begin
        NewName := ValidName('ImportedCSUB_Period_' + IntToStr(Period));
      end
      else
      begin
        Inc(ObjectCount);
        NewName := ValidName('ImportedCSUB_Obs'  + IntToStr(ObjectCount));
      end;
    end;
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    if Period > 0 then
    begin
      result.CreateCsubBoundary;
      NewItem := result.ModflowCSub.Values.Add as TCsubItem;
      NewItem.StartTime := StartTime;
      NewItem.EndTime := LastTime;
    end
    else if Period = 0 then
    begin
      result.CreateCsubBoundary;
      CSubPackageData := result.ModflowCSub.CSubPackageData;
      for Index := 0 to CSubPackage.Interbeds.Count - 1 do
      begin
        PackageItem := CSubPackageData.Add;
        PackageItem.InterbedSystemName := CSubPackage.Interbeds[Index].Name;
      end;

      ImportedName := 'Imported_pcs0';
      pcs0 := result.ImportedValues.Add;
      pcs0.Name := ImportedName;
      pcs0.Values.DataType := rdtDouble;

      ImportedName := 'Imported_thick_frac';
      thick_frac := result.ImportedValues.Add;
      thick_frac.Name := ImportedName;
      thick_frac.Values.DataType := rdtDouble;

      ImportedName := 'Imported_rnb';
      rnb := result.ImportedValues.Add;
      rnb.Name := ImportedName;
      rnb.Values.DataType := rdtDouble;

      ImportedName := 'Imported_ssv_cc';
      ssv_cc := result.ImportedValues.Add;
      ssv_cc.Name := ImportedName;
      ssv_cc.Values.DataType := rdtDouble;


      ImportedName := 'Imported_sse_cr';
      sse_cr := result.ImportedValues.Add;
      sse_cr.Name := ImportedName;
      sse_cr.Values.DataType := rdtDouble;

      ImportedName := 'Imported_theta';
      theta := result.ImportedValues.Add;
      theta.Name := ImportedName;
      theta.Values.DataType := rdtDouble;

      ImportedName := 'Imported_kv';
      kv := result.ImportedValues.Add;
      kv.Name := ImportedName;
      kv.Values.DataType := rdtDouble;

      ImportedName := 'Imported_h0';
      h0 := result.ImportedValues.Add;
      h0.Name := ImportedName;
      h0.Values.DataType := rdtDouble;
    end
    else if Period = -1 then
    begin
      // do nothing
    end;
  end;
  procedure IncludeObservations(ObsList: TObservationList;
    AScreenObject: TScreenObject; Name: string = '');
  var
    Modflow6Obs: TModflow6Obs;
    CSubObsSet: TSubObsSet;
    ObsIndex: Integer;
    CSubDelayCells: TIntegerCollection;
  begin
    Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    AScreenObject.CreateMf6Obs;
    Modflow6Obs := AScreenObject.Modflow6Obs;
    if Modflow6Obs.Name = '' then
    begin
      if Name = '' then
      begin
        Inc(ObsNameIndex);
        Modflow6Obs.Name := 'CSUB_Obs_' + IntToStr(ObsNameIndex);
      end
      else
      begin
        Modflow6Obs.Name := Name;
      end;
    end;
    CSubObsSet := Modflow6Obs.CSubObs.CSubObsSet;
    CSubDelayCells := Modflow6Obs.CSubDelayCells;
    for ObsIndex := 0 to ObsList.Count - 1 do
    begin
      Obs := ObsList[ObsIndex];
      if Obs.ObsType = 'csub' then
      begin
        Include(CSubObsSet, coCSub)
      end
      else if Obs.ObsType = 'inelastic-csub' then
      begin
        Include(CSubObsSet, coInelastCSub)
      end
      else if Obs.ObsType = 'elastic-csub' then
      begin
        Include(CSubObsSet, coElastCSub)
      end
      else if Obs.ObsType = 'coarse-csub' then
      begin
        Include(CSubObsSet, coCoarseCSub)
      end
      else if Obs.ObsType = 'csub-cell' then
      begin
        Include(CSubObsSet, coCSubCell)
      end
      else if Obs.ObsType = 'wcomp-csub-cell' then
      begin
        Include(CSubObsSet, coWcompCSubCell)
      end
      else if Obs.ObsType = 'sk' then
      begin
        Include(CSubObsSet, coSk)
      end
      else if Obs.ObsType = 'ske' then
      begin
        Include(CSubObsSet, coSke)
      end
      else if Obs.ObsType = 'sk-cell' then
      begin
        Include(CSubObsSet, coSkCell)
      end
      else if Obs.ObsType = 'ske-cell' then
      begin
        Include(CSubObsSet, coSkeCell)
      end
      else if Obs.ObsType = 'estress-cell' then
      begin
        Include(CSubObsSet, coEStressCell)
      end
      else if Obs.ObsType = 'gstress-cell' then
      begin
        Include(CSubObsSet, coGStressCell)
      end
      else if Obs.ObsType = 'interbed-compaction' then
      begin
        Include(CSubObsSet, coIntbedComp)
      end
      else if Obs.ObsType = 'elastic-compaction' then
      begin
        Include(CSubObsSet, coElastComp)
      end
      else if Obs.ObsType = 'coarse-compaction' then
      begin
        Include(CSubObsSet, coCoarseCompaction)
      end
      else if Obs.ObsType = 'inelastic-compaction-cell' then
      begin
//        Include(CSubObsSet, coCompCell)
      end
      else if Obs.ObsType = 'elastic-compaction-cell' then
      begin
//        Include(CSubObsSet, coCompCell)
      end
      else if Obs.ObsType = 'compaction-cell' then
      begin
        Include(CSubObsSet, coCompCell)
      end
      else if Obs.ObsType = 'thickness' then
      begin
        Include(CSubObsSet, coThickness)
      end
      else if Obs.ObsType = 'coarse-thickness' then
      begin
        Include(CSubObsSet, coCoarseThickness)
      end
      else if Obs.ObsType = 'thickness-cell' then
      begin
        Include(CSubObsSet, coThickCell)
      end
      else if Obs.ObsType = 'theta' then
      begin
        Include(CSubObsSet, coTheta)
      end
      else if Obs.ObsType = 'coarse-theta' then
      begin
        Include(CSubObsSet, coCoarseTheta)
      end
      else if Obs.ObsType = 'theta-cell' then
      begin
        Include(CSubObsSet, coThetaCell)
      end
      else if Obs.ObsType = 'delay-flowtop' then
      begin
        Include(CSubObsSet, coDelayFlowTop)
      end
      else if Obs.ObsType = 'delay-flowbot' then
      begin
        Include(CSubObsSet, coDelayFlowBot)
      end
      else if Obs.ObsType = 'delay-head' then
      begin
        Include(CSubObsSet, coDelayHead);
        Assert(Obs.IdType2 = itNumber);
        if CSubDelayCells.IndexOf(Obs.Num2) < 0 then
        begin
          CSubDelayCells.Add.Value := Obs.Num2;
        end;
      end
      else if Obs.ObsType = 'delay-gstress' then
      begin
        Include(CSubObsSet, coDelayGStress);
        Assert(Obs.IdType2 = itNumber);
        if CSubDelayCells.IndexOf(Obs.Num2) < 0 then
        begin
          CSubDelayCells.Add.Value := Obs.Num2;
        end;
      end
      else if Obs.ObsType = 'delay-estress' then
      begin
        Include(CSubObsSet, coDelayEStress);
        Assert(Obs.IdType2 = itNumber);
        if CSubDelayCells.IndexOf(Obs.Num2) < 0 then
        begin
          CSubDelayCells.Add.Value := Obs.Num2;
        end;
      end
      else if Obs.ObsType = 'delay-preconstress' then
      begin
        Include(CSubObsSet, coDelayPreConStress);
        Assert(Obs.IdType2 = itNumber);
        if CSubDelayCells.IndexOf(Obs.Num2) < 0 then
        begin
          CSubDelayCells.Add.Value := Obs.Num2;
        end;
      end
      else if Obs.ObsType = 'delay-compaction' then
      begin
        Include(CSubObsSet, coDelayComp);
        Assert(Obs.IdType2 = itNumber);
        if CSubDelayCells.IndexOf(Obs.Num2) < 0 then
        begin
          CSubDelayCells.Add.Value := Obs.Num2;
        end;
      end
      else if Obs.ObsType = 'delay-thickness' then
      begin
        Include(CSubObsSet, coDelayThickness);
        Assert(Obs.IdType2 = itNumber);
        if CSubDelayCells.IndexOf(Obs.Num2) < 0 then
        begin
          CSubDelayCells.Add.Value := Obs.Num2;
        end;
      end
      else if Obs.ObsType = 'delay-theta' then
      begin
        Include(CSubObsSet, coDelayTheta);
        Assert(Obs.IdType2 = itNumber);
        if CSubDelayCells.IndexOf(Obs.Num2) < 0 then
        begin
          CSubDelayCells.Add.Value := Obs.Num2;
        end;
      end
      else if Obs.ObsType = 'preconstress-cell' then
      begin
        Include(CSubObsSet, coPreConsStressCell);
      end
      else
      begin
        FErrorMessages.Add(Format('Unrecognized UZF observation type "%s".', [Obs.ObsType]))
      end;
    end;
    Modflow6Obs.CSubObs.CSubObsSet := CSubObsSet;
  end;
  procedure AssignPackageData(PackageDataList: TObjectList<TCSubItemList>);
  var
    ListIndex: Integer;
    ItemIndex: Integer;
    AnInterBed: TCSubInterbed;
    DataArrayManager: TDataArrayManager;
    ADataArray: TDataArray;
    DataSetIndex: Integer;
  begin
    DataArrayManager := Model.DataArrayManager;
    for ListIndex := 0 to PackageDataList.Count - 1 do
    begin
      AnInterBed := CSubPackage.Interbeds[InterbedIndex];
      PriorBoundName := '';
      PriorItemAssigned := False;
      List := PackageDataList[ListIndex];
      AScreenObject := nil;
      PackageItem := nil;
      for ItemIndex := 0 to List.Count - 1 do
      begin
        Item := List[ItemIndex];
        if Item.boundname.Used then
        begin
          BoundName := UpperCase(Item.boundname.Value);
        end
        else
        begin
          BoundName := '';
        end;

        if (not PriorItemAssigned) or (BoundName <> PriorBoundName) then
        begin
          AScreenObject := CreateScreenObject(BoundName, 0);
          PackageItem := AScreenObject.ModflowCSub.CSubPackageData[InterbedIndex];

          PackageItem.Used := True;
          PackageItem.InitialOffset := rsObjectImportedValuesR + '("' + pcs0.Name + '")';
          PackageItem.Thickness := rsObjectImportedValuesR + '("' + thick_frac.Name + '")';
          PackageItem.EquivInterbedNumber := rsObjectImportedValuesR + '("' + rnb.Name + '")';
          PackageItem.InitialInelasticSpecificStorage := rsObjectImportedValuesR + '("' + ssv_cc.Name + '")';
          PackageItem.InitialElasticSpecificStorage := rsObjectImportedValuesR + '("' + sse_cr.Name + '")';
          PackageItem.InitialPorosity := rsObjectImportedValuesR + '("' + theta.Name + '")';
          PackageItem.DelayKv := rsObjectImportedValuesR + '("' + kv.Name + '")';
          PackageItem.InitialDelayHeadOffset := rsObjectImportedValuesR + '("' + h0.Name + '")';

          if BoundName <> '' then
          begin
            if not BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
            begin
              Assert(False);
            end;
            IncludeObservations(ObsList, AScreenObject, BoundName);
          end;

          if AnInterBed.InterbedType = itDelay then
          begin
            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.DelayKvName);
            Assert(ADataArray <> nil);
            DataSetIndex := AScreenObject.AddDataSet(ADataArray);
            AScreenObject.DataSetFormulas[DataSetIndex] := PackageItem.DelayKv;
          end;

          if AnInterBed.InterbedType = itDelay then
          begin
            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.EquivInterbedNumberName);
            Assert(ADataArray <> nil);
            DataSetIndex := AScreenObject.AddDataSet(ADataArray);
            AScreenObject.DataSetFormulas[DataSetIndex] := PackageItem.EquivInterbedNumber;
          end;

          if AnInterBed.InterbedType = itDelay then
          begin
            ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialDelayHeadOffset);
            Assert(ADataArray <> nil);
            DataSetIndex := AScreenObject.AddDataSet(ADataArray);
            AScreenObject.DataSetFormulas[DataSetIndex] := PackageItem.InitialDelayHeadOffset;
          end;

          ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialElasticSpecificStorage);
          Assert(ADataArray <> nil);
          DataSetIndex := AScreenObject.AddDataSet(ADataArray);
          AScreenObject.DataSetFormulas[DataSetIndex] := PackageItem.InitialElasticSpecificStorage;

          ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialInelasticSpecificStorage);
          Assert(ADataArray <> nil);
          DataSetIndex := AScreenObject.AddDataSet(ADataArray);
          AScreenObject.DataSetFormulas[DataSetIndex] := PackageItem.InitialInelasticSpecificStorage;

          ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialOffset);
          Assert(ADataArray <> nil);
          DataSetIndex := AScreenObject.AddDataSet(ADataArray);
          AScreenObject.DataSetFormulas[DataSetIndex] := PackageItem.InitialOffset;

          ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.InitialPorosity);
          Assert(ADataArray <> nil);
          DataSetIndex := AScreenObject.AddDataSet(ADataArray);
          AScreenObject.DataSetFormulas[DataSetIndex] := PackageItem.InitialPorosity;

          ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.Thickness);
          Assert(ADataArray <> nil);
          DataSetIndex := AScreenObject.AddDataSet(ADataArray);
          AScreenObject.DataSetFormulas[DataSetIndex] := PackageItem.Thickness;

          ADataArray := DataArrayManager.GetDataSetByName(AnInterBed.CSubBoundName);
          Assert(ADataArray <> nil);
          DataSetIndex := AScreenObject.AddDataSet(ADataArray);
          AScreenObject.DataSetFormulas[DataSetIndex] := Format('"%s"', [AScreenObject.Name]);
        end;

        pcs0.Values.Add(Item.pcs0);
        thick_frac.Values.Add(Item.thick_frac);
        rnb.Values.Add(Item.rnb);
        ssv_cc.Values.Add(Item.ssv_cc);
        sse_cr.Values.Add(Item.sse_cr);
        theta.Values.Add(Item.theta);
        kv.Values.Add(Item.kv);
        h0.Values.Add(Item.h0);

        CellId := Item.cellid;
        ElementCenter := Model.ElementLocation[CellId.Layer-1, CellId.Row-1, CellId.Column-1];
        APoint.x := ElementCenter.RotatedLocation.x;
        APoint.y := ElementCenter.RotatedLocation.y;
        AScreenObject.AddPoint(APoint, True);
        AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

        PriorItem := Item;
        PriorBoundName := BoundName;
        PriorItemAssigned := True;
      end;
      Inc(InterbedIndex);
    end
  end;
begin
  ObsNameIndex := 0;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing CSUB package');
  end;
  StartTime := 0.0;
  ObjectCount := 0;
  Model := frmGoPhast.PhastModel;
  CSubPackage := Model.ModflowPackages.CSubPackage;
  CSubPackage.IsSelected := True;

  CSub := Package.Package as TCSub;

  NoDelayInterbeds := TList<TCSubInterbed>.Create;
  DelayInterbeds := TList<TCSubInterbed>.Create;
  IcsubnoObsDictionary := TNumberDictionary.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  CellIdObsDictionary := TCellIdObsDictionary.Create;
  ObsLists := TObsLists.Create;
  Map := TimeSeriesMap.Create;
  try
    for TimeSeriesIndex := 0 to CSub.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := CSub.TimeSeries[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;

    if CSub.ObservationCount > 0 then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    end;
    for ObsPackageIndex := 0 to CSub.ObservationCount - 1 do
    begin
      ObsFiles := CSub.Observations[ObsPackageIndex].Package as TObs;
      GetObservations(IcsubnoObsDictionary, BoundNameObsDictionary,
        CellIdObsDictionary, ObsLists, ObsFiles);
    end;
    if Assigned(OnUpdateStatusBar) then
    begin
      OnUpdateStatusBar(self, 'importing CSUB package');
    end;

    Options := CSub.Options;

    if Options.GAMMAW.Used then
    begin
      CSubPackage.Gamma := Options.GAMMAW.Value;
    end;
    if Options.Beta.Used then
    begin
      CSubPackage.Beta := Options.Beta.Value;
    end;
    CSubPackage.HeadBased := Options.HEAD_BASED;
    CSubPackage.PreconsolidationHeadUsed := Options.INITIAL_PRECONSOLIDATION_HEAD;
    if Options.NDELAYCELLS.Used then
    begin
      CSubPackage.NumberOfDelayCells := Options.NDELAYCELLS.Value;
    end;
    CSubPackage.CompressionMethod := TCompressionMethod(Options.COMPRESSION_INDICES);
    CSubPackage.UpdateMaterialProperties := Options.UPDATE_MATERIAL_PROPERTIES;
    CSubPackage.InterbedThicknessMethod := TInterbedThicknessMethod(Options.CELL_FRACTION);

    CSubPackage.SpecifyInitialPreconsolidationStress := Options.SPECIFIED_INITIAL_PRECONSOLIDATION_STRESS;
    CSubPackage.SpecifyInitialDelayHead := Options.SPECIFIED_INITIAL_DELAY_HEAD;
    if Options.SPECIFIED_INITIAL_INTERBED_STATE then
    begin
      CSubPackage.SpecifyInitialPreconsolidationStress := True;
      CSubPackage.SpecifyInitialDelayHead := True;
    end;
    CSubPackage.EffectiveStressLag := Options.EFFECTIVE_STRESS_LAG;
    OutputTypes := [];
    if Options.STRAIN_CSV_INTERBED then
    begin
      Include(OutputTypes, coInterbedStrain);
    end;
    if Options.STRAIN_CSV_COARSE then
    begin
      Include(OutputTypes, coCourseStrain);
    end;
    if Options.COMPACTION then
    begin
      Include(OutputTypes, coCompaction);
    end;
    if Options.COMPACTION_ELASTIC then
    begin
      Include(OutputTypes, coElasticComp);
    end;
    if Options.COMPACTION_INELASTIC then
    begin
      Include(OutputTypes, coInelasticComp);
    end;
    if Options.COMPACTION_INTERBED then
    begin
      Include(OutputTypes, coInterbedComp);
    end;
    if Options.COMPACTION_COARSE then
    begin
      Include(OutputTypes, coCoarseComp);
    end;
    if Options.ZDISPLACEMENT then
    begin
      Include(OutputTypes, coZDisplacement);
    end;
    CSubPackage.OutputTypes := OutputTypes;
    CSubPackage.WriteConvergenceData := Options.PACKAGE_CONVERGENCE;

    SetLength(DelayCounts, Model.LayerCount, Model.RowCount, Model.ColumnCount);
    SetLength(NoDelayCounts, Model.LayerCount, Model.RowCount, Model.ColumnCount);
    for LayerIndex := 0 to Model.LayerCount - 1 do
    begin
      for RowIndex := 0 to Model.RowCount - 1 do
      begin
        for ColumnIndex := 0 to Model.ColumnCount - 1 do
        begin
          DelayCounts[LayerIndex, RowIndex, ColumnIndex] := 0;
          NoDelayCounts[LayerIndex, RowIndex, ColumnIndex] := 0;
        end;
      end;
    end;
    MaxDelay := 0;
    MaxNoDelay := 0;
    PackageData := CSub.PackageData;
    for Index := 0 to PackageData.Count - 1 do
    begin
      Item := PackageData[Index];
      if Item.boundname.Used then
      begin
        BoundName := UpperCase(Item.boundname.Value);
      end
      else
      begin
        BoundName := '';
      end;
      if BoundName <> '' then
      begin
        if not BoundNameObsDictionary.ContainsKey(BoundName) then
        begin
          BName := Item.boundname;
          BName.Used := False;
          Item.boundname := BName;
          PackageData[Index] := Item;
        end;
      end;
    end;
    PackageData.sort;
    DelayLists := TObjectList<TCSubItemList>.Create;
    NoDelayLists := TObjectList<TCSubItemList>.Create;
    try
      for Index := 0 to PackageData.Count - 1 do
      begin
        Item := PackageData[Index];
        if Item.cdelay = 'DELAY' then
        begin
          Inc(DelayCounts[Item.cellid.Layer-1, Item.cellid.Row-1, Item.cellid.Column-1]);
          if DelayCounts[Item.cellid.Layer-1, Item.cellid.Row-1, Item.cellid.Column-1] > MaxDelay then
          begin
            MaxDelay := DelayCounts[Item.cellid.Layer-1, Item.cellid.Row-1, Item.cellid.Column-1];
            List := TCSubItemList.Create;
            DelayLists.Add(List);
          end;

          List := DelayLists[DelayCounts[Item.cellid.Layer-1, Item.cellid.Row-1, Item.cellid.Column-1] -1];
          List.Add(Item);
        end
        else if Item.cdelay = 'NODELAY' then
        begin
          Inc(NoDelayCounts[Item.cellid.Layer-1, Item.cellid.Row-1, Item.cellid.Column-1]);
          if NoDelayCounts[Item.cellid.Layer-1, Item.cellid.Row-1, Item.cellid.Column-1] > MaxNoDelay then
          begin
            MaxNoDelay := NoDelayCounts[Item.cellid.Layer-1, Item.cellid.Row-1, Item.cellid.Column-1];
            List := TCSubItemList.Create;
            NoDelayLists.Add(List);
          end;

          List := NoDelayLists[NoDelayCounts[Item.cellid.Layer-1, Item.cellid.Row-1, Item.cellid.Column-1] -1];
          List.Add(Item);
        end
        else
        begin
          FErrorMessages.Add(Format('Invalid cdelay value "%s"', [Item.cdelay]))
        end;
      end;

      CSubPackage.Interbeds.Capacity := MaxDelay + MaxNoDelay;
      for Index := 1 to MaxNoDelay do
      begin
        Interbed := CSubPackage.Interbeds.Add;
        Interbed.Name := Format('No_Delay_%d', [Index]);
        Interbed.InterbedType := itNoDelay;
        NoDelayInterbeds.Add(Interbed);
      end;
      for Index := 1 to MaxDelay do
      begin
        Interbed := CSubPackage.Interbeds.Add;
        Interbed.Name := Format('Delay_%d', [Index]);
        Interbed.InterbedType := itDelay;
        DelayInterbeds.Add(Interbed);
      end;

      Model.DataArrayManager.CreateInitialDataSets;

      GridData := CSub.GridData;

      if CSubPackage.CompressionMethod = coRecompression then
      begin
        DataArrayName := KInitialElasticReco;
      end
      else
      begin
        DataArrayName := KInitialElasticSpec;
      end;
      Assign3DRealDataSet(DataArrayName, GridData.CG_SKE_CR);
      Assign3DRealDataSet(KInitialCoarsePoros, GridData.CG_THETA);
      Assign3DRealDataSet(KMoistSpecificGravi, GridData.SGM);
      Assign3DRealDataSet(KSaturatedSpecificG, GridData.SGS);

      Assert(NoDelayLists.Count = NoDelayInterbeds.Count);
      InterbedIndex := 0;

      AssignPackageData(NoDelayLists);
      AssignPackageData(DelayLists);


    finally
      DelayLists.Free;
      NoDelayLists.Free;
    end;

    LastTime := Model.ModflowStressPeriods.Last.EndTime;
    PriorScreenObjects := TScreenObjectList.Create;
    try
      for PeriodIndex := 0 to CSub.PeriodCount - 1 do
      begin
        APeriod := CSub[PeriodIndex];
        StartTime := Model.ModflowStressPeriods[APeriod.Period-1].StartTime;
        for ScreenObjectIndex := 0 to PriorScreenObjects.Count - 1 do
        begin
          TimeItem := PriorScreenObjects[ScreenObjectIndex].ModflowCSub.Values.Last as TCsubItem;
          TimeItem.EndTime := StartTime;
        end;
        PriorScreenObjects.Clear;

        PriorTimeSeriesAssigned := False;
        PriorTimeSeries := '';
        AScreenObject := nil;
        sig0 := nil;

        if APeriod.Count > 0 then
        begin
          APeriod.Sort;
          for CellIndex := 0 to APeriod.Count - 1 do
          begin
            ACell := APeriod[CellIndex];
            case ACell.ValueType of
              vtNumeric:
                begin
                  TimeSeries := '';
                end;
              vtString:
                begin
                  TimeSeries := ACell.StringValue;
                end;
            end;

            if (not PriorTimeSeriesAssigned) or (TimeSeries <> PriorTimeSeries) then
            begin
              AScreenObject := CreateScreenObject(TimeSeries, APeriod.Period);
              PriorScreenObjects.Add(AScreenObject);
              case ACell.ValueType of
                vtNumeric:
                  begin
                    ImportedName := 'ImportedCSub_sig0_' + IntToStr(APeriod.Period);

                    sig0 := AScreenObject.ImportedValues.Add;
                    sig0.Name := ImportedName;
                    sig0.Values.DataType := rdtDouble;

                    Formula := rsObjectImportedValuesR + '("' + ImportedName + '")'
                  end;
                vtString:
                  begin
                    if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeriesName) then
                    begin
                      Assert(False);
                    end;
                    Formula := ImportedTimeSeriesName;
                  end;
              end;

              if ACell.ValueType = vtNumeric then
              begin
                sig0.Values.Add(ACell.sig0);
              end;

              CellId := ACell.cellid;
              if Model.DisvUsed then
              begin
                CellId.Row := 1;
              end;

              ElementCenter := Model.ElementLocation[CellId.Layer-1, CellId.Row-1, CellId.Column-1];
              APoint.x := ElementCenter.RotatedLocation.x;
              APoint.y := ElementCenter.RotatedLocation.y;
              AScreenObject.AddPoint(APoint, True);
              AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);
            end;

            PriorTimeSeries := TimeSeries;
            PriorTimeSeriesAssigned := True;
          end;
        end;
      end;
    finally
      PriorScreenObjects.Free;
    end;

    for ObsListIndex := 0 to ObsLists.Count - 1 do
    begin
      ObsList := ObsLists[ObsListIndex];
      case ObsList[0].IdType1 of
        itCell:
          begin
            CellId := ObsList[0].CellId1;
          end;
        itNumber:
          begin
            CellId := PackageData.Items[ObsList[0].Num1-1].cellid;
          end;
        itName:
          begin
            Continue;
          end;
        else
          Assert(False)
      end;
      if Model.DisvUsed then
      begin
        CellId.Row := 1;
      end;
      AScreenObject := CreateScreenObject('', -1);

      ElementCenter := Model.ElementLocation[CellId.Layer-1, CellId.Row-1, CellId.Column-1];
      APoint.x := ElementCenter.RotatedLocation.x;
      APoint.y := ElementCenter.RotatedLocation.y;
      AScreenObject.AddPoint(APoint, True);
      AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

      IncludeObservations(ObsList, AScreenObject)
    end;

  finally
    Map.Free;
    IcsubnoObsDictionary.Free;
    BoundNameObsDictionary.Free;
    CellIdObsDictionary.Free;
    ObsLists.Free;
    NoDelayInterbeds.Free;
    DelayInterbeds.Free;
  end;
end;

procedure TModflow6Importer.ImportDis(Package: TPackage);
var
  Dis: TDis;
  XOrigin: Extended;
  YOrigin: Extended;
  GridAngle: Extended;
  Model: TPhastModel;
  MfOptions: TModflowOptions;
  ColumnPositions: TOneDRealArray;
  RowPositions: TOneDRealArray;
  Delr: TDArray1D;
  Position: Extended;
  ColIndex: Integer;
  Delc: TDArray1D;
  AngleToLL: Extended;
  DistanceToLL: Extended;
  RowIndex: Integer;
  TOP: TDArray2D;
  BOTM: TDArray3D;
  NumberOfLayers: Integer;
  IDOMAIN: TIArray3D;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing DIS package');
  end;
  Model := frmGoPhast.PhastModel;
  MfOptions := Model.ModflowOptions;

  Dis := Package.Package as TDis;
  MfOptions.LengthUnit := Dis.Options.LENGTH_UNITS;
  MfOptions.WriteBinaryGridFile := not Dis.Options.NOGRB;

  XOrigin := Dis.Options.XORIGIN;
  YOrigin := Dis.Options.YORIGIN;
  GridAngle := Dis.Options.ANGROT * Pi / 180;

  Delr := Dis.GridData.DELR;
  SetLength(ColumnPositions, Length(Delr) + 1);
  Delc := Dis.GridData.DELC;
  SetLength(RowPositions, Length(Delc) + 1);

  if GridAngle = 0 then
  begin
    Position := XOrigin;
    ColumnPositions[0] := Position;
    for ColIndex := 0 to Length(Delr) - 1 do
    begin
      Position := Position + Delr[ColIndex];
      ColumnPositions[ColIndex+1] := Position;
    end;

    Position := YOrigin;
    RowPositions[Length(RowPositions)-1] := Position;
    for RowIndex := 0 to Length(Delc) - 1 do
    begin
      Position := Position + Delc[RowIndex];
      RowPositions[Length(RowPositions) - RowIndex -2] := Position;
    end;
  end
  else
  begin
    AngleToLL := ArcTan2(YOrigin, XOrigin);
    DistanceToLL := Sqrt(Sqr(XOrigin) + Sqr(YOrigin));

    Position := DistanceToLL * Cos(AngleToLL - GridAngle);
    ColumnPositions[0] := Position;
    for ColIndex := 0 to Length(Delr) - 1 do
    begin
      Position := Position + Delr[ColIndex];
      ColumnPositions[ColIndex+1] := Position;
    end;

    Position := DistanceToLL * Sin(AngleToLL - GridAngle);
    RowPositions[Length(RowPositions)-1] := Position;
    for RowIndex := 0 to Length(Delc) - 1 do
    begin
      Position := Position + Delc[RowIndex];
      RowPositions[Length(RowPositions) - RowIndex - 2] := Position;
    end;
  end;

  TOP := Dis.GridData.TOP;
  BOTM  := Dis.GridData.BOTM;
  IDOMAIN := Dis.GridData.IDOMAIN;
  NumberOfLayers := Length(BOTM);
  UpdateLayerStructure(NumberOfLayers);

  Model.ModflowGrid.BeginGridChange;
  try
    Model.ModflowGrid.GridAngle := GridAngle;
    Model.ModflowGrid.ColumnPositions := ColumnPositions;
    Model.ModflowGrid.RowPositions := RowPositions;
  finally
    Model.ModflowGrid.EndGridChange;
  end;
  AssignTOP(TOP);
  AssignBOTM(BOTM);
  AssignIDomain(IDOMAIN, NumberOfLayers);

end;

procedure TModflow6Importer.ImportDisV(Package: TPackage);
var
  Model: TPhastModel;
  MfOptions: TModflowOptions;
  Disv: TDisv;
  XOrigin: Extended;
  YOrigin: Extended;
  GridAngle: Extended;
  Mesh3D: TModflowDisvGrid;
  Mesh2D: TModflowIrregularGrid2D;
  CellCorners: TModflowNodes;
  Verticies: TDisvVertices;
  Index: Integer;
  Vertex: TVertex;
  Node: TModflowNode;
  Cells: TDisvCells;
  ModelCells: TModflowIrregularCell2DCollection;
  Cell: TDisvCell;
  IrregularCell: TModflowIrregularCell2D;
  NodeIndex: Integer;
  NodeNumber: Integer;
  ModelNode: TIntegerItem;
  TOP: TDArray2D;
  BOTM: TDArray3D;
  IDOMAIN: TIArray3D;
  APoint: TPoint2D;
  NumberOfLayers: Integer;
  function ConvertLocation(X, Y: Extended): TPoint2D;
  begin
    if GridAngle = 0 then
    begin
      result.x := XOrigin + X;
      result.Y := YOrigin + Y;
    end
    else
    begin
      result.x := XOrigin + X * Cos(GridAngle);
      result.Y := YOrigin + Y * Sin(GridAngle);
    end;
  end;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing DISV package');
  end;
  Model := frmGoPhast.PhastModel;
  MfOptions := Model.ModflowOptions;

  Model.Mf6GridType := mgtLayered;

  Disv := Package.Package as TDisv;
  MfOptions.LengthUnit := Disv.Options.LENGTH_UNITS;
  MfOptions.WriteBinaryGridFile := not Disv.Options.NOGRB;

  XOrigin := Disv.Options.XORIGIN;
  YOrigin := Disv.Options.YORIGIN;
  GridAngle := Disv.Options.ANGROT * Pi / 180;
  
  NumberOfLayers := Disv.Dimensions.NLay;
  UpdateLayerStructure(NumberOfLayers);

  Mesh3D := Model.DisvGrid;
  Mesh2D := Mesh3D.TwoDGrid;
  CellCorners := Mesh2D.CellCorners;
  
  Verticies := Disv.Verticies;
  CellCorners.Capacity := Verticies.Count;
  for Index := 0 to Verticies.Count - 1 do
  begin
    Vertex := Verticies[Index];
    Node := CellCorners.Add;
    APoint := ConvertLocation(Vertex.xv, Vertex.yv);
    Node.X := APoint.x;
    Node.Y := APoint.y;
    Node.Number := Vertex.iv -1; 
  end;

  Cells := Disv.Cells;
  ModelCells := Mesh2D.Cells;

  ModelCells.Capacity := Cells.Count;
  for Index := 0 to Cells.Count - 1 do
  begin
    Cell := Cells[Index];
    IrregularCell := ModelCells.Add;
    IrregularCell.ElementNumber := Cell.icell2d -1;
    APoint := ConvertLocation(Cell.xc, Cell.yc);
    IrregularCell.X := APoint.x;
    IrregularCell.Y := APoint.y; 
    IrregularCell.NodeNumbers.Capacity := Cell.ncvert;
    for NodeIndex := 0 to Cell.ncvert - 1 do
    begin
      NodeNumber := Cell.icvert[NodeIndex] -1;
      ModelNode := IrregularCell.NodeNumbers.Add;
      ModelNode.Value := NodeNumber;
    end;
  end;

  Mesh3D.Loaded;

  TOP := Disv.GridData.TOP;
  BOTM := Disv.GridData.BOTM;
  IDOMAIN := Disv.GridData.IDOMAIN;
  
  AssignTOP(TOP);
  AssignBOTM(BOTM);
  AssignIDomain(IDOMAIN, NumberOfLayers);
end;

type
  TDrnConnection = class(TObject)
    ScreenObject: TScreenObject;
    List: TDrnTimeItemList;
    IFACE: Integer;
    destructor Destroy; override;
  end;

  TDrnConnectionObjectList = TObjectList<TDrnConnection>;
  TDrnConnectionObjectLists = TObjectList<TDrnConnectionObjectList>;

  TDrnMvrLink = record
    DrnPeriod: TDrnPeriod;
    MvrPeriod: TMvrPeriod;
    function Period: Integer;
  end;
  TDrnlMvrLinkArray = TArray<TDrnMvrLink>;
  TDrnMvrLinkList = TList<TDrnMvrLink>;

  TMvrDrnTimeItemList = Class(TDrnTimeItemList)
    FIds: TGenericIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure Sort;
  end;

procedure TModflow6Importer.ImportDrn(Package: TPackage;
  MvrPackage: TPackage);
var
  Model: TPhastModel;
  Drn: TDrn;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ObsLists: TObsLists;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  PeriodIndex: Integer;
  APeriod: TDrnPeriod;
  CellIndex: Integer;
  ACell: TDrnTimeItem;
  IfaceIndex: Integer;
  KeyStringDictionary: TDictionary<string, TMvrDrnTimeItemList>;
  CellLists: TObjectList<TMvrDrnTimeItemList>;
  OtherCellLists: TObjectList<TMvrDrnTimeItemList>;
  ACellList: TMvrDrnTimeItemList;
  Options: TDrnOptions;
  IFace: Integer;
  LastTime: Double;
  Imported_Drain_Elevations: TValueArrayItem;
  Imported_Drain_Conductance: TValueArrayItem;
  ItemList: TList<TDrnItem>;
  StartTime: Double;
  DrnIndex: Integer;
  AnItem: TDrnItem;
  KeyString: string;
  TimeSeries: string;
  ImportedTimeSeries: string;
  ObjectCount: Integer;
  ObjectIndex: Integer;
  FirstCell: TDrnTimeItem;
  BoundName: string;
  ConnectionObjectLists: TDrnConnectionObjectLists;
  ConnectionDictionary: TDictionary<string, TDrnConnectionObjectList>;
  AConnectionList: TDrnConnectionObjectList;
  ConnectionIndex: Integer;
  ConnectionItem: TDrnConnection;
  AScreenObject: TScreenObject;
  AuxIFACE: TMf6BoundaryValue;
  Aux: TMf6BoundaryValue;
  CellIds: TCellIdList;
  Mvr: TMvr;
  Index: Integer;
  FoundMvr: Boolean;
  DrnMvrLink: TDrnMvrLink;
  DrnlMvrLinkList: TDrnMvrLinkList;
  MvrPeriod: TMvrPeriod;
  MvrUsed: Boolean;
  NewScreenObject: Boolean;
  MvrSource: TMvrSource;
  AuxMultIndex: Integer;
  AuxMultiplier: Extended;
  DrnlMvrLinkArray: TDrnlMvrLinkArray;
  DrnlPeriod: TDrnPeriod;
  NextDrnPeriod: TDrnPeriod;
  EndPeriod: Integer;
  NextMvrPeriod: TMvrPeriod;
  AuxDepthIndex: Integer;
  CellListIndex: Integer;
  Imported_Ddrn: TValueArrayItem;
  procedure AddItem(AScreenObject: TScreenObject; ACell: TDrnTimeItem; Period: Integer);
  var
    DrnItem: TDrnItem;
    ImportedName: string;
    Aux: TMf6BoundaryValue;
    AuxMultiplier: Extended;
  begin
    DrnItem := AScreenObject.ModflowDrnBoundary.Values.Add as TDrnItem;
    ItemList.Add(DrnItem);
    DrnItem.EndTime := LastTime;
    DrnItem.StartTime := StartTime;

    if AuxMultIndex >= 0 then
    begin
      Aux := ACell.Aux[AuxMultIndex];
      if Aux.ValueType = vtNumeric then
      begin
        AuxMultiplier := Aux.NumericValue
      end
      else
      begin
        AuxMultiplier := 1;
        FErrorMessages.Add(StrModelMuseCanNotIm);
      end;
    end
    else
    begin
      AuxMultiplier := 1;
    end;

    if AuxDepthIndex >= 0 then
    begin
      Aux := ACell.Aux[AuxDepthIndex];
      if Aux.ValueType = vtNumeric then
      begin
        ImportedName := Format('Imported_DDRNs_Period_%d', [Period]);
        Imported_Ddrn := AScreenObject.ImportedValues.Add;
        Imported_Ddrn.Name := ImportedName;
        Imported_Ddrn.Values.DataType := rdtDouble;
        DrnItem.DDRN := rsObjectImportedValuesR + '("' + Imported_Ddrn.Name + '")';
      end
      else
      begin
        Imported_Ddrn := nil;

        TimeSeries := Aux.StringValue;
        if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
        begin
          Assert(False);
        end;
        DrnItem.DDRN := ImportedTimeSeries;
      end;
    end
    else
    begin
      Imported_Ddrn := nil;
    end;

    if ACell.elev.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Drain_Elevations_Period_%d', [Period]);
      Imported_Drain_Elevations := AScreenObject.ImportedValues.Add;
      Imported_Drain_Elevations.Name := ImportedName;
      Imported_Drain_Elevations.Values.DataType := rdtDouble;
      DrnItem.Elevation := rsObjectImportedValuesR + '("' + Imported_Drain_Elevations.Name + '")';
    end
    else
    begin
      Imported_Drain_Elevations := nil;
      TimeSeries := ACell.elev.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      DrnItem.Elevation := ImportedTimeSeries;
    end;

    if ACell.cond.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Drain_Conductance_Period_%d', [Period]);
      Imported_Drain_Conductance := AScreenObject.ImportedValues.Add;
      Imported_Drain_Conductance.Name := ImportedName;
      Imported_Drain_Conductance.Values.DataType := rdtDouble;
      DrnItem.Conductance := rsObjectImportedValuesR + '("' + Imported_Drain_Conductance.Name + '")';
    end
    else
    begin
      Imported_Drain_Conductance := nil;
      TimeSeries := ACell.cond.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      DrnItem.Conductance := ImportedTimeSeries;
      if AuxMultiplier <> 1 then
      begin
        FErrorMessages.Add(StrModelMuseCanNotAp);
      end;
    end;
  end;
  procedure CreateObsScreenObject(ACell: TDrnTimeItem);
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    CellId: TMfCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Drn_Obs_%d', [Package.PackageName, ObjectCount]));
    AScreenObject.Name := NewName;
    AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(AScreenObject);
    AScreenObject.ElevationCount := ecOne;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.EvaluatedAt := eaBlocks;
    AScreenObject.Visible := False;
    AScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    AScreenObject.CreateMf6Obs;
    AScreenObject.Modflow6Obs.Name := ACell.Boundname;
    AScreenObject.Modflow6Obs.General := [ogDrain];

    CellId := ACell.Cellid;
    if Model.DisvUsed then
    begin
      CellId.Row := 1;
    end;
    ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
    APoint.x := ElementCenter.RotatedLocation.x;
    APoint.y := ElementCenter.RotatedLocation.y;
    AScreenObject.AddPoint(APoint, True);
    AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

  end;
  function CreateScreenObject(ACell: TDrnTimeItem; Period: Integer): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    AuxIFACE: TMf6BoundaryValue;
    BoundName: string;
    ObsList: TObservationList;
    AnObs: TObservation;
    ObsIndex: Integer;
    General: TObGenerals;
  begin
    Inc(ObjectCount);
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Drn_%d_Period_%d', [Package.PackageName, ObjectCount, Period]));
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateDrnBoundary;
    result.ModflowDrnBoundary.FormulaInterpretation := fiDirect;
    if IfaceIndex >= 0 then
    begin
      AuxIFACE := ACell[IfaceIndex];
      Assert(AuxIFACE.ValueType = vtNumeric);
      IFACE := Round(AuxIFACE.NumericValue);
    end
    else
    begin
      IFACE := 0;
    end;
    result.IFACE := TIface(IFACE+2);

    AddItem(result, ACell, Period);

    BoundName := UpperCase(ACell.Boundname);
    if BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      result.CreateMf6Obs;
      General := [];
      for ObsIndex := 0 to ObsList.Count - 1 do
      begin
        AnObs := ObsList[ObsIndex];
        if AnsiSameText(AnObs.ObsType, 'drn') then
        begin
          Include(General, ogDrain);
        end
        else if AnsiSameText(AnObs.ObsType, 'to-mvr') then
        begin
          Include(General, ogMvr);
        end
        else
        begin
          Assert(False);
        end;
      end;
      result.Modflow6Obs.Name := ACell.Boundname;
      result.Modflow6Obs.General := General;
    end;
  end;
begin
  MvrSource.LakeOutlet := nil;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing DRN package');
  end;
  // Get the MVR package.
  if MvrPackage = nil then
  begin
    Mvr := nil;
  end
  else
  begin
    Mvr := MvrPackage.Package as TMvr;
    FoundMvr := False;
    for Index := 0 to Mvr.Packages.Count - 1 do
    begin
      FoundMvr := AnsiSameText(Package.PackageName, Mvr.Packages[Index].pname);
      if FoundMvr then
      begin
        Break;
      end;
    end;
    if not FoundMvr then
    begin
      Mvr := nil;
    end;
  end;

  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.DrnPackage.IsSelected := True;

  Drn := Package.Package as TDrn;
  Options := Drn.Options;

  if Options.AUXMULTNAME <> '' then
  begin
    AuxMultIndex := Options.IndexOfAUXILIARY(Options.AUXMULTNAME);
  end
  else
  begin
    AuxMultIndex := -1;
  end;

  if Options.AUXDEPTHNAME <> '' then
  begin
    AuxDepthIndex := Options.IndexOfAUXILIARY(Options.AUXDEPTHNAME);
  end
  else
  begin
    AuxDepthIndex := -1;
  end;

  OtherCellLists := TObjectList<TMvrDrnTimeItemList>.Create;
  DrnlMvrLinkList := TDrnMvrLinkList.Create;
  CellIds := TCellIdList.Create;
  ConnectionObjectLists := TDrnConnectionObjectLists.Create;
  ConnectionDictionary := TDictionary<string, TDrnConnectionObjectList>.Create;
  ItemList := TList<TDrnItem>.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  CellIdObsDictionary := TCellIdObsDictionary.Create;
  Map := TimeSeriesMap.Create;
  ObsLists := TObsLists.Create;
  KeyStringDictionary := TDictionary<string, TMvrDrnTimeItemList>.Create;
  CellLists := TObjectList<TMvrDrnTimeItemList>.Create;
  try
    OtherCellLists.OwnsObjects := False;
  try
    if Mvr = nil then
    begin
      DrnMvrLink.MvrPeriod := nil;
      for PeriodIndex := 0 to Drn.PeriodCount - 1 do
      begin
        DrnMvrLink.DrnPeriod := Drn.Periods[PeriodIndex];
        DrnlMvrLinkList.Add(DrnMvrLink)
      end;
    end
    else
    begin
      // Make sure that all the stress periods defined in either the MVR or the
      // Drn package are imported.
      SetLength(DrnlMvrLinkArray, Model.ModflowStressPeriods.Count);
      for PeriodIndex := 0 to Length(DrnlMvrLinkArray) - 1 do
      begin
        DrnMvrLink.DrnPeriod := nil;
        DrnMvrLink.MvrPeriod := nil;
      end;

      for PeriodIndex := 0 to Drn.PeriodCount - 1 do
      begin
        DrnlPeriod := Drn.Periods[PeriodIndex];
        if PeriodIndex < Drn.PeriodCount - 1 then
        begin
          NextDrnPeriod := Drn.Periods[PeriodIndex+1];
          EndPeriod := NextDrnPeriod.Period;
        end
        else
        begin
          EndPeriod := Model.ModflowStressPeriods.Count;
        end;
        for Index := DrnlPeriod.Period  to EndPeriod do
        begin
          DrnlMvrLinkArray[Index-1].DrnPeriod  := DrnlPeriod;
        end;
      end;

      for PeriodIndex := 0 to Mvr.PeriodCount - 1 do
      begin
        MvrPeriod := Mvr.Periods[PeriodIndex];
        if PeriodIndex < Mvr.PeriodCount - 1 then
        begin
          NextMvrPeriod := Mvr.Periods[PeriodIndex+1];
          EndPeriod := NextMvrPeriod.Period;
        end
        else
        begin
          EndPeriod := Model.ModflowStressPeriods.Count;
        end;
        for Index := MvrPeriod.Period  to EndPeriod do
        begin
          DrnlMvrLinkArray[Index-1].MvrPeriod  := MvrPeriod;
        end;
      end;

      DrnlMvrLinkList.Add(DrnlMvrLinkArray[0]);
      for Index := 1 to Length(DrnlMvrLinkArray) - 1 do
      begin
        if (DrnlMvrLinkArray[Index].DrnPeriod <> DrnlMvrLinkArray[Index-1].DrnPeriod)
          or (DrnlMvrLinkArray[Index].MvrPeriod <> DrnlMvrLinkArray[Index-1].MvrPeriod) then
        begin
          DrnlMvrLinkList.Add(DrnlMvrLinkArray[Index]);
        end;
      end;
    end;

    IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
    for TimeSeriesIndex := 0 to Drn.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := Drn.TimeSeries[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;

    if Drn.ObservationCount > 0 then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    end;
    for ObsPackageIndex := 0 to Drn.ObservationCount - 1 do
    begin
      ObsFiles := Drn.Observations[ObsPackageIndex].Package as TObs;
      GetObservations(nil, BoundNameObsDictionary,
        CellIdObsDictionary, ObsLists, ObsFiles);
    end;

    if Assigned(OnUpdateStatusBar) then
    begin
      OnUpdateStatusBar(self, 'importing DRN package');
    end;

    LastTime := Model.ModflowStressPeriods.Last.EndTime;

    ACellList := nil;
    ObjectCount := 0;
    for PeriodIndex := 0 to DrnlMvrLinkList.Count - 1 do
    begin
      DrnMvrLink := DrnlMvrLinkList[PeriodIndex];
      APeriod := DrnlMvrLinkList[PeriodIndex].DrnPeriod;
      if APeriod = nil then
      begin
        Continue;
      end;
      StartTime := Model.ModflowStressPeriods[DrnMvrLink.Period-1].StartTime;
      for DrnIndex := 0 to ItemList.Count - 1 do
      begin
        AnItem := ItemList[DrnIndex];
        AnItem.EndTime := StartTime;
      end;
      ItemList.Clear;
      for CellListIndex := 0 to CellLists.Count - 1 do
      begin
        CellLists[CellListIndex].Clear;
      end;

      // Assign all cells in the current period to a cell list.
      for CellIndex := 0 to APeriod.Count - 1 do
      begin
        ACell := APeriod[CellIndex];

        if (ACell.Boundname <> '')
          and BoundNameObsDictionary.ContainsKey(UpperCase(ACell.Boundname)) then
        begin
          KeyString := 'BN:' + UpperCase(ACell.Boundname) + ' ';
        end
        else
        begin
          KeyString := '';
        end;

        if IfaceIndex < 0 then
        begin
          IFACE := 0;
        end
        else
        begin
          AuxIFACE := ACell[IfaceIndex];
          Assert(AuxIFACE.ValueType = vtNumeric);
          IFACE := Round(AuxIFACE.NumericValue);
        end;
        KeyString := KeyString + ACell.Keystring + ' IFACE:' + IntToStr(IFACE);

        MvrUsed := False;
        if DrnMvrLink.MvrPeriod <> nil then
        begin
          if DrnMvrLink.MvrPeriod.HasSource(Package.PackageName, ACell.Id) then
          begin
            KeyString := KeyString + ' MVR ' + InttoStr(ACell.Id);
            MvrUsed := True;
          end;
        end;

        if not KeyStringDictionary.TryGetValue(KeyString, ACellList) then
        begin
          ACellList := TMvrDrnTimeItemList.Create;
          CellLists.Add(ACellList);
          KeyStringDictionary.Add(KeyString, ACellList);
        end;
        ACellList.Add(ACell);
        if MvrUsed then
        begin
          if ACellList.FIds.IndexOf(ACell.Id) < 0 then
          begin
            ACellList.FIds.Add(ACell.Id);
          end;
        end;
      end;

      // After all the cells in the current period have been read,
      // create a TScreenObject for each cell list
      AScreenObject := nil;
      for ObjectIndex := 0 to CellLists.Count - 1 do
      begin
        NewScreenObject := False;
        ACellList := CellLists[ObjectIndex];
        if ACellList.Count > 0 then
        begin
          FirstCell := ACellList[0];
          if (FirstCell.Boundname <> '')
            and BoundNameObsDictionary.ContainsKey(UpperCase(FirstCell.Boundname)) then
          begin
            if IfaceIndex < 0 then
            begin
              IFACE := 0;
            end
            else
            begin
              AuxIFACE := FirstCell[IfaceIndex];
              Assert(AuxIFACE.ValueType = vtNumeric);
              IFACE := Round(AuxIFACE.NumericValue);
            end;
            BoundName := UpperCase(FirstCell.Boundname);
            if not ConnectionDictionary.TryGetValue(BoundName, AConnectionList) then
            begin
              AConnectionList := TDrnConnectionObjectList.Create;
              ConnectionObjectLists.Add(AConnectionList);
              ConnectionDictionary.Add(BoundName, AConnectionList)
            end;
            ACellList.Sort;
            AScreenObject := nil;
            for ConnectionIndex := 0 to AConnectionList.Count - 1 do
            begin
              ConnectionItem := AConnectionList[ConnectionIndex];
              if (IFACE = ConnectionItem.IFACE)
                and ACellList.SameCells(ConnectionItem.List) then
              begin
                AScreenObject := ConnectionItem.ScreenObject;
                Break;
              end;
            end;
            if AScreenObject = nil then
            begin
              AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
              ConnectionItem := TDrnConnection.Create;
              ConnectionItem.ScreenObject := AScreenObject;
              ConnectionItem.IFACE := IFACE;
              ConnectionItem.List := ACellList;
              AConnectionList.Add(ConnectionItem);
              OtherCellLists.Add(ACellList);
              NewScreenObject := True;
            end
            else
            begin
              AddItem(AScreenObject, FirstCell, APeriod.Period);
            end;
          end
          else
          begin
            AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
            NewScreenObject := True;
          end;
        end;

        CellIds.Clear;
        for CellIndex := 0 to ACellList.Count - 1 do
        begin
          ACell := ACellList[CellIndex];
          if ACell.elev.ValueType = vtNumeric then
          begin
            Imported_Drain_Elevations.Values.Add(ACell.elev.NumericValue);// + AuxDepthAdjustment);
          end;
          if AuxDepthIndex >= 0 then
          begin
            Aux := ACell.Aux[AuxDepthIndex];
            if Aux.ValueType = vtNumeric then
            begin
              Imported_Ddrn.Values.Add(Aux.NumericValue);
            end;
          end;

          if ACell.cond.ValueType = vtNumeric then
          begin
            if AuxMultIndex >= 0 then
            begin
              Aux := ACell.Aux[AuxMultIndex];
              if Aux.ValueType = vtNumeric then
              begin
                AuxMultiplier := Aux.NumericValue
              end
              else
              begin
                AuxMultiplier := 1;
                FErrorMessages.Add(StrModelMuseCanNotIm);
              end;
            end
            else
            begin
              AuxMultiplier := 1;
            end;
            Imported_Drain_Conductance.Values.Add(ACell.cond.NumericValue * AuxMultiplier);
          end;

          if NewScreenObject then
          begin
            CellIds.Add(ACell.Cellid);
          end;

          if CellIdObsDictionary.ContainsKey(ACell.Cellid) then
          begin
            CreateObsScreenObject(ACell);
          end;
        end;

        if NewScreenObject then
        begin
          AddPointsToScreenObject(CellIds, AScreenObject);
        end;

        if ACellList.FIds.Count > 0 then
        begin
          MvrSource.ScreenObject := AScreenObject;
          MvrSource.PackageName := Package.PackageName;
          MvrSource.Period := DrnMvrLink.Period;
          MvrSource.IDs := ACellList.FIds.ToArray;
          MvrSource.SourceType := mspcDrn;
          FMvrSources.Add(MvrSource);
        end;
      end;
    end;

  finally
    for CellListIndex := 0 to OtherCellLists.Count - 1 do
    begin
      CellLists.Extract(OtherCellLists[CellListIndex])
    end;
  end;
  finally
    BoundNameObsDictionary.Free;
    CellIdObsDictionary.Free;
    Map.Free;
    ObsLists.Free;
    KeyStringDictionary.Free;
    CellLists.Free;
    ItemList.Free;
    ConnectionObjectLists.Free;
    ConnectionDictionary.Free;
    CellIds.Free;
    DrnlMvrLinkList.Free;
    OtherCellLists.Free;
  end;
end;

type
  TEvtConnection = class(TObject)
    ScreenObject: TScreenObject;
    List: TEvtTimeItemList;
    IFACE: Integer;
    destructor Destroy; override;
  end;

  TEvtConnectionObjectList = TObjectList<TEvtConnection>;
  TEvtConnectionObjectLists = TObjectList<TEvtConnectionObjectList>;

procedure TModflow6Importer.ImportEvt(Package: TPackage;
  TransportModels: TModelList);
var
  Model: TPhastModel;
  Evt: TEvt;
  AModel: TModel;
  APackage: TPackage;
  ModelIndex: Integer;
  TransportModel: TTransportNameFile;
  PackageIndex: Integer;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary;
  Ssm: TSsm;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ObsLists: TObsLists;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  SourceIndex: Integer;
  FoundMatch: Boolean;
  TransportAuxNames: TStringList;
  PeriodIndex: Integer;
  APeriod: TEvtPeriod;
  CellIndex: Integer;
  ACell: TEvtTimeItem;
  IfaceIndex: Integer;
  KeyStringDictionary: TDictionary<string, TEvtTimeItemList>;
  CellLists: TObjectList<TEvtTimeItemList>;
  OtherCellLists: TObjectList<TEvtTimeItemList>;
  ACellList: TEvtTimeItemList;
  Options: TEvtOptions;
  IFace: Integer;
  LastTime: Double;
  Imported_Et: TValueArrayItem;
  Imported_Et_surf: TValueArrayItem;
  Imported_Et_Depth: TValueArrayItem;
  DepthFractions: TValueArrayItemList;
  EdtFractions: TValueArrayItemList;
  ItemList: TList<TEvtItem>;
  SurfDepthList: TList<TEtsSurfDepthItem>;
  StartTime: Double;
  EvtIndex: Integer;
  AnItem: TEvtItem;
  KeyString: string;
  TimeSeries: string;
  ImportedTimeSeries: string;
  ObjectCount: Integer;
  ObjectIndex: Integer;
  FirstCell: TEvtTimeItem;
  BoundName: string;
  ConnectionObjectLists: TEvtConnectionObjectLists;
  ConnectionDictionary: TDictionary<string, TEvtConnectionObjectList>;
  AConnectionList: TEvtConnectionObjectList;
  ConnectionIndex: Integer;
  ConnectionItem: TEvtConnection;
  AScreenObject: TScreenObject;
  AuxIFACE: TMf6BoundaryValue;
  AuxIndex: Integer;
  ChemSpeciesName: string;
  Aux: TMf6BoundaryValue;
  GwtAuxIndex: Integer;
  Values: TValueArrayStorage;
  CellIds: TCellIdList;
  AddCells: Boolean;
  NSEG: Integer;
  NumIntermediatePoint: Integer;
  SurfDepthItem: TEtsSurfDepthItem;
  IntermediateIndex: Integer;
  ImportedPxdp: TValueArrayItem;
  ImportedPetm: TValueArrayItem;
  CellListIndex: Integer;
  procedure AddItem(AScreenObject: TScreenObject; ACell: TEvtTimeItem; Period: Integer);
  var
    EvtItem: TEvtItem;
    ImportedName: string;
    Concentrations: TEvtGwtConcCollection;
    ChemSpeciesName: string;
    ConcItem: TGwtConcStringValueItem;
    GwtAuxIndex: Integer;
    AuxIndex: Integer;
    Aux: TMf6BoundaryValue;
    Imported_Chem: TValueArrayItem;
    EtsSurfDepthItem: TEtsSurfDepthItem;
    EtsItem: TEtsStringValueItem;
    ImportedPxdp: TValueArrayItem;
    ImportedPetm: TValueArrayItem;
    IntermediateIndex: Integer;
  begin
    EvtItem := AScreenObject.ModflowEtsBoundary.Values.Add as TEvtItem;
    ItemList.Add(EvtItem);
    EvtItem.EndTime := LastTime;
    EvtItem.StartTime := StartTime;

    if ACell.Rate.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Et_Period_%d', [Period]);
      Imported_Et := AScreenObject.ImportedValues.Add;
      Imported_Et.Name := ImportedName;
      Imported_Et.Values.DataType := rdtDouble;
      EvtItem.EvapotranspirationRate := rsObjectImportedValuesR + '("' + Imported_Et.Name + '")';
    end
    else
    begin
      Imported_Et := nil;
      TimeSeries := ACell.Rate.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      EvtItem.EvapotranspirationRate := ImportedTimeSeries;
    end;

    EtsSurfDepthItem := AScreenObject.ModflowEtsBoundary.EtsSurfDepthCollection.Add as TEtsSurfDepthItem;
    SurfDepthList.Add(EtsSurfDepthItem);
    EtsSurfDepthItem.EndTime := LastTime;
    EtsSurfDepthItem.StartTime := StartTime;
    if ACell.Surf.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Et_Surf_Period_%d', [Period]);
      Imported_Et_surf := AScreenObject.ImportedValues.Add;
      Imported_Et_surf.Name := ImportedName;
      Imported_Et_surf.Values.DataType := rdtDouble;
      EtsSurfDepthItem.EvapotranspirationSurface := rsObjectImportedValuesR + '("' + Imported_Et_surf.Name + '")';
    end
    else
    begin
      Imported_Et_surf := nil;
      TimeSeries := ACell.Surf.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      EtsSurfDepthItem.EvapotranspirationSurface := ImportedTimeSeries;
    end;

    if ACell.Depth.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Et_Depth_Period_%d', [Period]);
      Imported_Et_Depth := AScreenObject.ImportedValues.Add;
      Imported_Et_Depth.Name := ImportedName;
      Imported_Et_Depth.Values.DataType := rdtDouble;
      EtsSurfDepthItem.EvapotranspirationDepth := rsObjectImportedValuesR + '("' + Imported_Et_Depth.Name + '")';
    end
    else
    begin
      Imported_Et_Depth := nil;
      TimeSeries := ACell.Depth.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      EtsSurfDepthItem.EvapotranspirationDepth := ImportedTimeSeries;
    end;

    for IntermediateIndex := 0 to NumIntermediatePoint - 1 do
    begin
      EtsItem := EtsSurfDepthItem.DepthFractions.Add as TEtsStringValueItem;
      if ACell.Pxdp[IntermediateIndex].ValueType = vtNumeric then
      begin
        ImportedName := Format('Imported_Pxdp_%d_Period_%d', [IntermediateIndex+1, Period]);
        ImportedPxdp := AScreenObject.ImportedValues.Add;
        ImportedPxdp.Name := ImportedName;
        ImportedPxdp.Values.DataType := rdtDouble;
        EtsItem.Value := rsObjectImportedValuesR + '("' + ImportedPxdp.Name + '")';
      end
      else
      begin
        ImportedPxdp := nil;
        TimeSeries := ACell.Pxdp[IntermediateIndex].StringValue;
        if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
        begin
          Assert(False);
        end;
        EtsItem.Value  := ImportedTimeSeries;
      end;
      DepthFractions.Add(ImportedPxdp);

      EtsItem := EtsSurfDepthItem.EtFractions.Add as TEtsStringValueItem;
      if ACell.Petm[IntermediateIndex].ValueType = vtNumeric then
      begin
        ImportedName := Format('Imported_Petm_%d_Period_%d', [IntermediateIndex+1, Period]);
        ImportedPetm := AScreenObject.ImportedValues.Add;
        ImportedPetm.Name := ImportedName;
        ImportedPetm.Values.DataType := rdtDouble;
        EtsItem.Value := rsObjectImportedValuesR + '("' + ImportedPetm.Name + '")';
      end
      else
      begin
        ImportedPetm := nil;
        TimeSeries := ACell.Petm[IntermediateIndex].StringValue;
        if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
        begin
          Assert(False);
        end;
        EtsItem.Value  := ImportedTimeSeries;
      end;
      EdtFractions.Add(ImportedPetm);
    end;

    if TransportAuxNames.Count > 0 then
    begin
      Concentrations := EvtItem.GwtConcentrations;
      Concentrations.Count := TransportAuxNames.Count;
      for AuxIndex := 0 to TransportAuxNames.Count - 1 do
      begin
        ChemSpeciesName := TransportAuxNames[AuxIndex];
        ConcItem := Concentrations[AuxIndex];

        GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
        Assert(GwtAuxIndex >= 0);
        Aux := ACell[GwtAuxIndex];
        if Aux.ValueType = vtNumeric then
        begin
          ImportedName := Format('Imported_%s_Period_%d', [ChemSpeciesName, Period]);
          Imported_Chem := AScreenObject.ImportedValues.Add;
          Imported_Chem.Name := ImportedName;
          Imported_Chem.Values.DataType := rdtDouble;
          ConcItem.Value := rsObjectImportedValuesR + '("' + Imported_Chem.Name + '")';
          TransportAuxNames.Objects[AuxIndex] := Imported_Chem.Values;
        end
        else
        begin
          TransportAuxNames.Objects[AuxIndex] := nil;
          TimeSeries := Aux.StringValue;
          if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
          begin
            Assert(False);
          end;
          ConcItem.Value := ImportedTimeSeries;
        end;
      end;
    end;
  end;
  procedure CreateObsScreenObject(ACell: TEvtTimeItem);
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    CellId: TMfCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Evt_Obs_%d', [Package.PackageName, ObjectCount]));
    AScreenObject.Name := NewName;
    AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(AScreenObject);
    AScreenObject.ElevationCount := ecOne;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.EvaluatedAt := eaBlocks;
    AScreenObject.Visible := False;
    AScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    AScreenObject.CreateMf6Obs;
    AScreenObject.Modflow6Obs.Name := ACell.Boundname;
    AScreenObject.Modflow6Obs.General := [ogEvt];

    CellId := ACell.Cellid;
    if Model.DisvUsed then
    begin
      CellId.Row := 1;
    end;
    ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
    APoint.x := ElementCenter.RotatedLocation.x;
    APoint.y := ElementCenter.RotatedLocation.y;
    AScreenObject.AddPoint(APoint, True);
    AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

  end;
  function CreateScreenObject(ACell: TEvtTimeItem; Period: Integer): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    AuxIFACE: TMf6BoundaryValue;
    BoundName: string;
    ObsList: TObservationList;
    AnObs: TObservation;
    ObsIndex: Integer;
  begin
    Inc(ObjectCount);
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Evt_%d_Period_%d', [Package.PackageName, ObjectCount, Period]));
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateEtsBoundary;
    if IfaceIndex >= 0 then
    begin
      AuxIFACE := ACell[IfaceIndex];
      Assert(AuxIFACE.ValueType = vtNumeric);
      IFACE := Round(AuxIFACE.NumericValue);
    end
    else
    begin
      IFACE := 0;
    end;
    result.IFACE := TIface(IFACE+2);

    AddItem(result, ACell, Period);

    BoundName := UpperCase(ACell.Boundname);
    if BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      result.CreateMf6Obs;
      for ObsIndex := 0 to ObsList.Count - 1 do
      begin
        AnObs := ObsList[ObsIndex];
        Assert(AnsiSameText(AnObs.ObsType, 'Evt'));
      end;
      result.Modflow6Obs.Name := ACell.Boundname;
      result.Modflow6Obs.General := [ogEvt];
    end;
  end;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing EVT package');
  end;
  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.EtsPackage.IsSelected := True;

  Evt := Package.Package as TEvt;
  NSEG := Evt.EvtDimensions.NSEG;
  Model.ModflowPackages.EtsPackage.SegmentCount := NSEG;
  NumIntermediatePoint := NSEG -1;

  Options := Evt.Options;
  if Options.SURF_RATE_SPECIFIED then
  begin
    FErrorMessages.Add(StrModelMuseCanNotSpPetm0)
  end;
  if Options.FIXED_CELL then
  begin
    Model.ModflowPackages.EtsPackage.LayerOption := loSpecified;
  end
  else
  begin
    Model.ModflowPackages.EtsPackage.LayerOption := loTopActive;
  end;

  DepthFractions := TValueArrayItemList.Create;
  EdtFractions := TValueArrayItemList.Create;
  CellIds := TCellIdList.Create;
  ConnectionObjectLists := TEvtConnectionObjectLists.Create;
  ConnectionDictionary := TDictionary<string, TEvtConnectionObjectList>.Create;
  ItemList := TList<TEvtItem>.Create;
  SurfDepthList := TList<TEtsSurfDepthItem>.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  CellIdObsDictionary := TCellIdObsDictionary.Create;
  Map := TimeSeriesMap.Create;
  ObsLists := TObsLists.Create;
  KeyStringDictionary := TDictionary<string, TEvtTimeItemList>.Create;
  CellLists := TObjectList<TEvtTimeItemList>.Create;
  OtherCellLists := TObjectList<TEvtTimeItemList>.Create;
  try
    OtherCellLists.OwnsObjects := False;
    try
      IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
      for TimeSeriesIndex := 0 to Evt.TimeSeriesCount - 1 do
      begin
        TimeSeriesPackage := Evt.TimeSeries[TimeSeriesIndex];
        ImportTimeSeries(TimeSeriesPackage, Map);
      end;
      for TimeSeriesIndex := 0 to Evt.TimeSeriesArrayCount - 1 do
      begin
        TimeSeriesPackage := Evt.TimeSeriesArray[TimeSeriesIndex];
        ImportTimeSeries(TimeSeriesPackage, Map);
      end;

      if Evt.ObservationCount > 0 then
      begin
        Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      end;
      for ObsPackageIndex := 0 to Evt.ObservationCount - 1 do
      begin
        ObsFiles := Evt.Observations[ObsPackageIndex].Package as TObs;
        GetObservations(nil, BoundNameObsDictionary,
          CellIdObsDictionary, ObsLists, ObsFiles);
      end;

      if Assigned(OnUpdateStatusBar) then
      begin
        OnUpdateStatusBar(self, 'importing EVT package');
      end;

      TransportAuxNames := TStringList.Create;
      try
        TransportAuxNames.CaseSensitive := False;
        for ModelIndex := 0 to TransportModels.Count - 1 do
        begin
          AModel := TransportModels[ModelIndex];
          TransportModel := AModel.FName as TTransportNameFile;
          for PackageIndex := 0 to TransportModel.NfPackages.Count  - 1 do
          begin
            APackage := TransportModel.NfPackages[PackageIndex];
            FoundMatch := False;
            if APackage.FileType = 'SSM6' then
            begin
              Ssm := APackage.Package as TSsm;
              for SourceIndex := 0 to Ssm.Sources.Count - 1 do
              begin
                if SameText(Ssm.Sources[SourceIndex].pname, Package.PackageName) then
                begin
                  FoundMatch := True;
                  TransportAuxNames.Add(Ssm.Sources[SourceIndex].auxname);
                  break;
                end;
              end;
  //            Assert(FoundMatch);
              break;
            end;
          end;
        end;

        LastTime := Model.ModflowStressPeriods.Last.EndTime;

        ObjectCount := 0;
        for PeriodIndex := 0 to Evt.PeriodCount - 1 do
        begin
          APeriod := Evt.Periods[PeriodIndex];
          StartTime := Model.ModflowStressPeriods[APeriod.Period-1].StartTime;
          for EvtIndex := 0 to ItemList.Count - 1 do
          begin
            AnItem := ItemList[EvtIndex];
            AnItem.EndTime := StartTime;
          end;
          ItemList.Clear;
          for CellListIndex := 0 to CellLists.Count - 1 do
          begin
            CellLists[CellListIndex].Clear;
          end;

          for EvtIndex := 0 to SurfDepthList.Count - 1 do
          begin
            SurfDepthItem := SurfDepthList[EvtIndex];
            SurfDepthItem.EndTime := StartTime;
          end;
          SurfDepthList.Clear;

          for CellIndex := 0 to APeriod.Count - 1 do
          begin
            ACell := APeriod[CellIndex];

            if (ACell.Boundname <> '')
              and BoundNameObsDictionary.ContainsKey(UpperCase(ACell.Boundname)) then
            begin
              KeyString := 'BN:' + UpperCase(ACell.Boundname) + ' ';
            end
            else
            begin
              KeyString := '';
            end;

            if IfaceIndex < 0 then
            begin
              IFACE := 0;
            end
            else
            begin
              AuxIFACE := ACell[IfaceIndex];
              Assert(AuxIFACE.ValueType = vtNumeric);
              IFACE := Round(AuxIFACE.NumericValue);
            end;
            KeyString := KeyString + ACell.Keystring + ' IFACE:' + IntToStr(IFACE);
            if not KeyStringDictionary.TryGetValue(KeyString, ACellList) then
            begin
              ACellList := TEvtTimeItemList.Create;
              ACellList.OwnsObjects := False;
              CellLists.Add(ACellList);
              KeyStringDictionary.Add(KeyString, ACellList)
            end;
            ACellList.Add(ACell);
          end;

          for ObjectIndex := 0 to CellLists.Count - 1 do
          begin
            DepthFractions.Clear;
            EdtFractions.Clear;
            AddCells := True;
            ACellList := CellLists[ObjectIndex];
            if ACellList.Count > 0 then
            begin
              FirstCell := ACellList[0];
              if (FirstCell.Boundname <> '')
                and BoundNameObsDictionary.ContainsKey(UpperCase(FirstCell.Boundname)) then
              begin
                if IfaceIndex < 0 then
                begin
                  IFACE := 0;
                end
                else
                begin
                  AuxIFACE := FirstCell[IfaceIndex];
                  Assert(AuxIFACE.ValueType = vtNumeric);
                  IFACE := Round(AuxIFACE.NumericValue);
                end;
                BoundName := UpperCase(FirstCell.Boundname);
                if not ConnectionDictionary.TryGetValue(BoundName, AConnectionList) then
                begin
                  AConnectionList := TEvtConnectionObjectList.Create;
                  ConnectionObjectLists.Add(AConnectionList);
                  ConnectionDictionary.Add(BoundName, AConnectionList)
                end;
                ACellList.Sort;
                AScreenObject := nil;
                for ConnectionIndex := 0 to AConnectionList.Count - 1 do
                begin
                  ConnectionItem := AConnectionList[ConnectionIndex];
                  if (IFACE = ConnectionItem.IFACE)
                    and ACellList.SameCells(ConnectionItem.List) then
                  begin
                    AScreenObject := ConnectionItem.ScreenObject;
                    AddCells := False;
                    Break;
                  end;
                end;
                if AScreenObject = nil then
                begin
                  AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
                  ConnectionItem := TEvtConnection.Create;
                  ConnectionItem.ScreenObject := AScreenObject;
                  ConnectionItem.IFACE := IFACE;
                  ConnectionItem.List := ACellList;
                  AConnectionList.Add(ConnectionItem);
                  OtherCellLists.Add(ACellList);
  //                CellLists.OwnsObjects := False;
  //                try
  //                  CellLists[ObjectIndex] := nil;
  //                finally
  //                  CellLists.OwnsObjects := True;
  //                end;
                end
                else
                begin
                  AddItem(AScreenObject, FirstCell, APeriod.Period);
                end;
              end
              else
              begin
                AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
              end;
            end;

            CellIds.Clear;
            for CellIndex := 0 to ACellList.Count - 1 do
            begin
              ACell := ACellList[CellIndex];
              if ACell.Rate.ValueType = vtNumeric then
              begin
                Imported_Et.Values.Add(ACell.Rate.NumericValue);
              end;
              if ACell.Surf.ValueType = vtNumeric then
              begin
                Imported_Et_surf.Values.Add(ACell.Surf.NumericValue);
              end;
              if ACell.Depth.ValueType = vtNumeric then
              begin
                Imported_Et_Depth.Values.Add(ACell.Depth.NumericValue);
              end;
              for IntermediateIndex := 0 to NumIntermediatePoint - 1 do
              begin
                if ACell.Pxdp[IntermediateIndex].ValueType = vtNumeric then
                begin
                  ImportedPxdp := DepthFractions[IntermediateIndex];
                  ImportedPxdp.Values.Add(ACell.Pxdp[IntermediateIndex].NumericValue);
                end;
                if ACell.Petm[IntermediateIndex].ValueType = vtNumeric then
                begin
                  ImportedPetm := EdtFractions[IntermediateIndex];
                  ImportedPetm.Values.Add(ACell.Petm[IntermediateIndex].NumericValue);
                end;
              end;

              for AuxIndex := 0 to TransportAuxNames.Count - 1 do
              begin
                ChemSpeciesName := TransportAuxNames[AuxIndex];
                GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
                Aux := ACell[GwtAuxIndex];
                if Aux.ValueType = vtNumeric then
                begin
                  Values := TransportAuxNames.Objects[AuxIndex] as TValueArrayStorage;
                  Values.Add(Aux.NumericValue);
                end;
              end;

              CellIds.Add(ACell.Cellid);

              if CellIdObsDictionary.ContainsKey(ACell.Cellid) then
              begin
                CreateObsScreenObject(ACell);
              end;
            end;
            if AddCells then
            begin
              AddPointsToScreenObject(CellIds, AScreenObject);
            end;
          end;
        end;

      finally
        TransportAuxNames.Free;
      end;

    finally
      for CellListIndex := 0 to OtherCellLists.Count - 1 do
      begin
        CellLists.Extract(OtherCellLists[CellListIndex])
      end;
    end;
  finally
    BoundNameObsDictionary.Free;
    CellIdObsDictionary.Free;
    Map.Free;
    ObsLists.Free;
    KeyStringDictionary.Free;
    CellLists.Free;
    ItemList.Free;
    SurfDepthList.Free;
    ConnectionObjectLists.Free;
    ConnectionDictionary.Free;
    CellIds.Free;
    DepthFractions.Free;
    EdtFractions.Free;
    OtherCellLists.Free;
  end;
end;

function TModflow6Importer.ImportFlowModel: Boolean;
var
  NameFile: TFlowNameFile;
  Options: TFlowNameFileOptions;
  Packages: TFlowPackages;
  Model: TPhastModel;
  MfOptions: TModflowOptions;
  OC: TModflowOutputControl;
  PackageIndex: Integer;
  APackage: TPackage;
  FlowBudgetFileName: string;
  TransportModels: TModelList;
  ModelIndex: Integer;
  ATransportModel: TModel;
  SimulationIndex: Integer;
  ASimulation: TMf6Simulation;
  SoluteNames: TStringList;
  Ssm: TSsm;
  AModel: TModel;
  TransportModel: TTransportNameFile;
  SourceIndex: Integer;
  AuxName: string;
  ChemSpeciesItem: TMobileChemSpeciesItem;
  MvrPackage: TPackage;
begin
  result := True;
  TransportModels := TModelList.Create;
  try
    if FFlowModel <> nil then
    begin
      Model := frmGoPhast.PhastModel;

      NameFile := FFlowModel.FName as TFlowNameFile;
      FlowBudgetFileName := FFlowModel.FullBudgetFileName;


      for SimulationIndex := 0 to FSimulations.Count - 1 do
      begin
        ASimulation := FSimulations[SimulationIndex];
        for ModelIndex := 0 to ASimulation.Models.Count - 1 do
        begin
          ATransportModel := ASimulation.Models[ModelIndex];
          if (ATransportModel.ModelType = 'GWT6')
            and (ATransportModel.FullBudgetFileName = FlowBudgetFileName) then
          begin
            TransportModels.Add(ATransportModel);
          end;
        end;
      end;

      SoluteNames := TStringList.Create;
      try
        SoluteNames.CaseSensitive := False;
        if TransportModels.Count > 0 then
        begin
          Model.ModflowPackages.GwtProcess.IsSelected := True;

          for ModelIndex := 0 to TransportModels.Count - 1 do
          begin
            AModel := TransportModels[ModelIndex];
            TransportModel := AModel.FName as TTransportNameFile;
            for PackageIndex := 0 to TransportModel.NfPackages.Count  - 1 do
            begin
              APackage := TransportModel.NfPackages[PackageIndex];
              if APackage.FileType = 'SSM6' then
              begin
                Ssm := APackage.Package as TSsm;
                for SourceIndex := 0 to Ssm.Sources.Count - 1 do
                begin
                  AuxName := Ssm.Sources[SourceIndex].auxname;
                  if SoluteNames.IndexOf(AuxName) < 0 then
                  begin
                    SoluteNames.Add(AuxName);
                    ChemSpeciesItem := Model.MobileComponents.Add;
                    ChemSpeciesItem.Name := AuxName;
                    break;
                  end;
                end;
                break;
              end;
            end;
          end;
        end;
      finally
        SoluteNames.Free;
      end;

      Options := NameFile.NfOptions;
      MfOptions := Model.ModflowOptions;
      MfOptions.NewtonMF6 := Options.NEWTON;
      MfOptions.UnderRelaxationMF6 := Options.UNDER_RELAXATION;
      if Options.PRINT_INPUT then
      begin
        OC := Model.ModflowOutputControl;
        OC.PrintInputCellLists := True;
        OC.PrintInputArrays := True;
      end;

      Packages := NameFile.NfPackages;

      for PackageIndex := 0 to Packages.Count - 1 do
      begin
        APackage := Packages[PackageIndex];
        if APackage.FileType = 'DIS6' then
        begin
          ImportDis(APackage);
          break
        end
        else if APackage.FileType = 'DISV6' then
        begin
          ImportDisV(APackage);
          break;
        end
        else if APackage.FileType = 'DISU6' then
        begin
          MessageDlg('ModelMuse can not import DISU models.', mtError, [mbOK], 0);
          result := False;
          Exit
        end
        else
        begin
          Continue;
        end;
      end;

      MvrPackage := nil;
      for PackageIndex := 0 to Packages.Count - 1 do
      begin
        APackage := Packages[PackageIndex];
        if APackage.FileType = 'MVR6' then
        begin
          MvrPackage := APackage;
          Break;
        end
      end;


      for PackageIndex := 0 to Packages.Count - 1 do
      begin
        APackage := Packages[PackageIndex];
        if (APackage.FileType = 'DIS6')
          or (APackage.FileType = 'DISV6')
          or (APackage.FileType = 'DISU6')
          then
        begin
          Continue;
        end;

        if APackage.FileType = 'IC6' then
        begin
          ImportIc(APackage);
        end
        else if APackage.FileType = 'OC6' then
        begin
          ImportOc(APackage)
        end
        else if APackage.FileType = 'OBS6' then
        begin
          ImportGwfObs(APackage)
        end
        else if APackage.FileType = 'NPF6' then
        begin
          ImportNpf(APackage);
        end
        else if APackage.FileType = 'HFB6' then
        begin
          ImportHfb(APackage);
        end
        else if APackage.FileType = 'STO6' then
        begin
          ImportSto(APackage);
        end
        else if APackage.FileType = 'CSUB6' then
        begin
          ImportCSub(APackage);
        end
        else if APackage.FileType = 'BUY6' then
        begin
          ImportBuy(APackage);
        end
        else if APackage.FileType = 'VSC6' then
        begin
          ImportVsc(APackage);
        end
        else if APackage.FileType = 'CHD6' then
        begin
          ImportChd(APackage, TransportModels);
        end
        else if APackage.FileType = 'WEL6' then
        begin
          ImportWel(APackage, TransportModels, MvrPackage);
        end
        else if APackage.FileType = 'DRN6' then
        begin
          ImportDrn(APackage, MvrPackage);
        end
        else if APackage.FileType = 'GHB6' then
        begin
          ImportGhb(APackage, TransportModels, MvrPackage);
        end
        else if APackage.FileType = 'RIV6' then
        begin
          ImportRiv(APackage, TransportModels, MvrPackage);
        end
        else if APackage.FileType = 'RCH6' then
        begin
          ImportRch(APackage, TransportModels);
        end
        else if APackage.FileType = 'EVT6' then
        begin
          ImportEvt(APackage, TransportModels);
        end
        else if APackage.FileType = 'MAW6' then
        begin
          ImportMaw(APackage, TransportModels, MvrPackage);
        end
        else if APackage.FileType = 'SFR6' then
        begin
          ImportSfr(APackage, TransportModels, MvrPackage);
        end
        else if APackage.FileType = 'LAK6' then
        begin
          ImportLak(APackage, TransportModels, MvrPackage);
        end
        else if APackage.FileType = 'UZF6' then
        begin
          ImportUzf(APackage, TransportModels, MvrPackage);
        end
        else if APackage.FileType = 'MVR6' then
        begin
          Continue;
        end
        else if APackage.FileType = 'GNC6' then
        begin
          ImportGnc(APackage);
        end
        else if APackage.FileType = 'GWF6-GWF6' then
        begin
  //        GwfGwfReader := TGwfGwf.Create(APackage.FileType);
  //        GwfGwfReader.Dimensions := FDimensions;
  //        GwfGwfReader.FDimensions2 := FDimensions;
  //        APackage.Package := GwfGwfReader;
  //        APackage.ReadPackage(Unhandled);
        end

      end;

      for PackageIndex := 0 to Packages.Count - 1 do
      begin
        APackage := Packages[PackageIndex];
        if APackage.FileType = 'MVR6' then
        begin
          ImportMvr(APackage);
        end
      end;
    end;
  finally
    TransportModels.Free;
  end;
end;

procedure TModflow6Importer.ImportFlowModelTiming;
var
  PhastModel: TPhastModel;
  MFStressPeriods: TModflowStressPeriods;
  StressPeriods: TTDis;
  MfOptions: TModflowOptions;
  TimeUnits: string;
  ValidUnits: TStringList;
  MfTimeUnit: Integer;
  SPIndex: Integer;
  SPData: TPeriod;
  MfStressPeriod: TModflowStressPeriod;
  StartTime: double;
  AtsIndex: Integer;
  AtsPeriod: TAtsPeriod;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing TDIS package');
  end;
  StressPeriods := FSimulation.Timing.TDis;

  PhastModel := frmGoPhast.PhastModel;
  MfOptions := PhastModel.ModflowOptions;
  TimeUnits := UpperCase(StressPeriods.Options.TimeUnits);
  if StressPeriods.Options.StartDate <> '' then
  begin
    MfOptions.Description.Add('Start Date = ' + StressPeriods.Options.StartDate);
    FErrorMessages.Add('Warning: The start date of the model has been added as a comment to the model description')
  end;

  ValidUnits := TStringList.Create;
  try
    ValidUnits.Add('UNKNOWN');
    ValidUnits.Add('SECONDS');
    ValidUnits.Add('MINUTES');
    ValidUnits.Add('HOURS');
    ValidUnits.Add('DAYS');
    ValidUnits.Add('YEARS');
    MfTimeUnit := ValidUnits.IndexOf(TimeUnits);
    if MfTimeUnit < 0 then
    begin
      MfTimeUnit := 0;
    end;
    MfOptions.TimeUnit := MfTimeUnit;
  finally
    ValidUnits.Free;
  end;


  MFStressPeriods := PhastModel.ModflowStressPeriods;
  MFStressPeriods.Capacity := StressPeriods.Dimensions.NPER;
  StartTime := 0.0;
  for SPIndex := 0 to StressPeriods.PeriodData.Count - 1 do
  begin
    SPData := StressPeriods.PeriodData[SPIndex];
    MfStressPeriod := MFStressPeriods.Add;
    MfStressPeriod.StartTime := StartTime;
    StartTime := StartTime + SPData.PerLen;
    MfStressPeriod.EndTime := StartTime;
    MfStressPeriod.PeriodLength := SPData.PerLen;
    MfStressPeriod.TimeStepMultiplier := SPData.TSMult;

    if SPData.NSTP > 1 then
    begin
      if SPData.TSMULT = 1 then
      begin
        MfStressPeriod.MaxLengthOfFirstTimeStep :=
          SPData.PERLEN / SPData.NSTP;
      end
      else
      begin
        MfStressPeriod.MaxLengthOfFirstTimeStep :=
          SPData.PERLEN * (SPData.TSMULT - 1)
          / (IntPower(SPData.TSMULT, SPData.NSTP) - 1);
      end;
    end
    else
    begin
      MfStressPeriod.MaxLengthOfFirstTimeStep := MfStressPeriod.PeriodLength;
    end;
  end;

  if StressPeriods.Ats <> nil then
  begin
    for AtsIndex := 0 to StressPeriods.Ats.Count - 1 do
    begin
      AtsPeriod := StressPeriods.Ats.AtsPeriod[AtsIndex];
      if AtsPeriod.iperats <= 0 then
      begin
        FErrorMessages.Add('ATS period data for iperats <= 0 is skipped ')
      end
      else if AtsPeriod.iperats > MFStressPeriods.Count then
      begin
        FErrorMessages.Add('ATS period data for iperats > NPER is skipped ')
      end
      else
      begin
        MfStressPeriod := MFStressPeriods[AtsPeriod.iperats-1];
        MfStressPeriod.AtsUsed := True;
        MfStressPeriod.AtsInitialStepSize := AtsPeriod.dt0;
        MfStressPeriod.AtsMinimumStepSize := AtsPeriod.dtmin;
        MfStressPeriod.AtsMaximumStepSize := AtsPeriod.dtmax;
        MfStressPeriod.AtsAdjustmentFactor := AtsPeriod.dtadj;
        MfStressPeriod.AtsFailureFactor := AtsPeriod.dtfailadj;
      end;
    end;
  end;
end;

type
  TGhbConnection = class(TObject)
    ScreenObject: TScreenObject;
    List: TGhbTimeItemList;
    IFACE: Integer;
    destructor Destroy; override;
  end;

  TGhbConnectionObjectList = TObjectList<TGhbConnection>;
  TGhbConnectionObjectLists = TObjectList<TGhbConnectionObjectList>;

  TGhbMvrLink = record
    GhbPeriod: TGhbPeriod;
    MvrPeriod: TMvrPeriod;
    function Period: Integer;
  end;
  TGhbMvrLinkArray = TArray<TGhbMvrLink>;
  TGhbMvrLinkList = TList<TGhbMvrLink>;

  TMvrGhbTimeItemList = Class(TGhbTimeItemList)
    FIds: TGenericIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure Sort;
  end;

procedure TModflow6Importer.ImportGnc(Package: TPackage);
var
  Model: TPhastModel;
  Gnc: TGnc;
  GhostNodes: TGhostNodes;
  ImportedGnc: TGncData;
  Index: Integer;
  ImportedGncItem: TGncDataItem;
  GhostNode: TGhostNode;
  ConnectionIndex: Integer;
  ConnectedCellID: TMfCellId;
  ConnectedCell: TWeightedCellId;
  ExistingGhostNodes: TGhostNodeArray;
  ContainingCell: TWeightedCellId;
  SumWeights: double;
begin
  Model := frmGoPhast.PhastModel;
  if not Model.DisvUsed then
  begin
    FErrorMessages.Add('ModelMuse only support the GNC package for DISV models.');
    Exit;
  end;
  Model.ModflowPackages.GncPackage.IsSelected := True;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing GNC package');
  end;

  Gnc := Package.Package as TGnc;
  if Gnc.Options.EXPLICIT then
  begin
    Model.ModflowPackages.GncPackage.EquationFormulation := efExplicit;
  end;

  SetLength(ExistingGhostNodes, Model.DisvGrid.TwoDGrid.ElementCount);
  GhostNodes := Model.DisvGrid.TwoDGrid.GhostNodes;
  ImportedGnc := Gnc.Data;
  for Index := 0 to ImportedGnc.Count - 1 do
  begin
    ImportedGncItem := ImportedGnc[Index];

    GhostNode := ExistingGhostNodes[ImportedGncItem.cellidn.Column-1];
    if GhostNode = nil then
    begin
      GhostNode := GhostNodes.Add;
      ExistingGhostNodes[ImportedGncItem.cellidn.Column-1] := GhostNode;
    end;
//    GhostNode := GhostNodes[ImportedGncItem.cellidn.Column-1];
    GhostNode.ContainingCell.Cell := ImportedGncItem.cellidn.Column-1;
    GhostNode.LinkedCell.Cell := ImportedGncItem.cellidm.Column-1;

    ContainingCell := GhostNode.CellWeights.GetCellByID(GhostNode.ContainingCell.Cell);
    if ContainingCell = nil then
    begin
      ContainingCell := GhostNode.CellWeights.Add;
      ContainingCell.Cell := GhostNode.ContainingCell.Cell;
      ContainingCell.Weight := 0;
    end;

    for ConnectionIndex := 0 to ImportedGncItem.Count - 1 do
    begin
      ConnectedCellID := ImportedGncItem[ConnectionIndex];
      if ConnectedCellID.Column > 0 then
      begin
        ConnectedCell := GhostNode.CellWeights.GetCellByID(ConnectedCellID.Column-1);
        if ConnectedCell = nil then
        begin
          ConnectedCell := GhostNode.CellWeights.Add;
          ConnectedCell.Cell := ConnectedCellID.Column-1;
          ConnectedCell.Weight := ImportedGncItem.Alpha[ConnectionIndex];
        end;
      end;
    end;

    if ContainingCell.Weight = 0 then
    begin
      SumWeights := 0;
      for ConnectionIndex := 0 to GhostNode.CellWeights.Count - 1 do
      begin
        ConnectedCell := GhostNode.CellWeights[ConnectionIndex];
        SumWeights :=SumWeights + ConnectedCell.Weight
      end;
      ContainingCell.Weight := 1-SumWeights;
    end;

  end;
end;

procedure TModflow6Importer.ImportGhb(Package: TPackage;
  TransportModels: TModelList; MvrPackage: TPackage);
var
  Model: TPhastModel;
  Ghb: TGhb;
  AModel: TModel;
  ModelIndex: Integer;
  TransportModel: TTransportNameFile;
  PackageIndex: Integer;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary;
  Ssm: TSsm;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ObsLists: TObsLists;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  SourceIndex: Integer;
  FoundMatch: Boolean;
  TransportAuxNames: TStringList;
  PeriodIndex: Integer;
  APeriod: TGhbPeriod;
  CellIndex: Integer;
  ACell: TGhbTimeItem;
  IfaceIndex: Integer;
  KeyStringDictionary: TDictionary<string, TMvrGhbTimeItemList>;
  CellLists: TObjectList<TMvrGhbTimeItemList>;
  OtherCellLists: TObjectList<TMvrGhbTimeItemList>;
  ACellList: TMvrGhbTimeItemList;
  Options: TGhbOptions;
  IFace: Integer;
  LastTime: Double;
  Imported_Ghb_Conductance: TValueArrayItem;
  Imported_Ghb_Stage: TValueArrayItem;
  ItemList: TList<TGhbItem>;
  StartTime: Double;
  GhbIndex: Integer;
  AnItem: TGhbItem;
  KeyString: string;
  TimeSeries: string;
  ImportedTimeSeries: string;
  ObjectCount: Integer;
  ObjectIndex: Integer;
  FirstCell: TGhbTimeItem;
  BoundName: string;
  ConnectionObjectLists: TGhbConnectionObjectLists;
  ConnectionDictionary: TDictionary<string, TGhbConnectionObjectList>;
  AConnectionList: TGhbConnectionObjectList;
  ConnectionIndex: Integer;
  ConnectionItem: TGhbConnection;
  AScreenObject: TScreenObject;
  AuxIFACE: TMf6BoundaryValue;
  AuxIndex: Integer;
  ChemSpeciesName: string;
  Aux: TMf6BoundaryValue;
  GwtAuxIndex: Integer;
  Values: TValueArrayStorage;
  CellIds: TCellIdList;
  Mvr: TMvr;
  Index: Integer;
  GhbMvrLink: TGhbMvrLink;
  GhbMvrLinkList: TGhbMvrLinkList;
  MvrPeriod: TMvrPeriod;
  MvrUsed: Boolean;
  NewScreenObject: Boolean;
  MvrSource: TMvrSource;
  APackage: TPackage;
  AuxMultIndex: Integer;
  AuxMultiplier: Extended;
  GhbMvrLinkArray: TGhbMvrLinkArray;
  GhbPeriod: TGhbPeriod;
  NextGhbPeriod: TGhbPeriod;
  EndPeriod: Integer;
  NextMvrPeriod: TMvrPeriod;
  CellListIndex: Integer;
  procedure AddItem(AScreenObject: TScreenObject; ACell: TGhbTimeItem; const Period: Integer);
  var
    GhbItem: TGhbItem;
    ImportedName: string;
    Concentrations: TGhbGwtConcCollection;
    ChemSpeciesName: string;
    ConcItem: TGwtConcStringValueItem;
    GwtAuxIndex: Integer;
    AuxIndex: Integer;
    Aux: TMf6BoundaryValue;
    Imported_Chem: TValueArrayItem;
    AuxMultiplier: Extended;
  begin
    GhbItem := AScreenObject.ModflowGhbBoundary.Values.Add as TGhbItem;
    ItemList.Add(GhbItem);
    GhbItem.EndTime := LastTime;
    GhbItem.StartTime := StartTime;

    if AuxMultIndex >= 0 then
    begin
      Aux := ACell.Aux[AuxMultIndex];
      if Aux.ValueType = vtNumeric then
      begin
        AuxMultiplier := Aux.NumericValue
      end
      else
      begin
        AuxMultiplier := 1;
        FErrorMessages.Add(StrModelMuseCanNotIm);
      end;
    end
    else
    begin
      AuxMultiplier := 1;
    end;

    if ACell.Cond.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Ghb_Conductance_Period_%d', [Period]);
      Imported_Ghb_Conductance := AScreenObject.ImportedValues.Add;
      Imported_Ghb_Conductance.Name := ImportedName;
      Imported_Ghb_Conductance.Values.DataType := rdtDouble;
      GhbItem.Conductance := rsObjectImportedValuesR + '("' + Imported_Ghb_Conductance.Name + '")';
    end
    else
    begin
      Imported_Ghb_Conductance := nil;
      TimeSeries := ACell.Cond.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      GhbItem.Conductance := ImportedTimeSeries;
      if AuxMultiplier <> 1 then
      begin
        FErrorMessages.Add(StrModelMuseCanNotAp);
      end;
    end;

    if ACell.BHead.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Ghb_Stage_Period_%d', [Period]);
      Imported_Ghb_Stage := AScreenObject.ImportedValues.Add;
      Imported_Ghb_Stage.Name := ImportedName;
      Imported_Ghb_Stage.Values.DataType := rdtDouble;
      GhbItem.BoundaryHead := rsObjectImportedValuesR + '("' + Imported_Ghb_Stage.Name + '")';
    end
    else
    begin
      Imported_Ghb_Stage := nil;
      TimeSeries := ACell.BHead.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      GhbItem.BoundaryHead := ImportedTimeSeries;
      if AuxMultiplier <> 1 then
      begin
        FErrorMessages.Add(StrModelMuseCanNotAp);
      end;
    end;

    if TransportAuxNames.Count > 0 then
    begin
      Concentrations := GhbItem.GwtConcentrations;
      Concentrations.Count := TransportAuxNames.Count;
      for AuxIndex := 0 to TransportAuxNames.Count - 1 do
      begin
        ChemSpeciesName := TransportAuxNames[AuxIndex];
        ConcItem := Concentrations[AuxIndex];

        GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
        Assert(GwtAuxIndex >= 0);
        Aux := ACell[GwtAuxIndex];
        if Aux.ValueType = vtNumeric then
        begin
          ImportedName := Format('Imported_%s_Period_%d', [ChemSpeciesName, Period]);
          Imported_Chem := AScreenObject.ImportedValues.Add;
          Imported_Chem.Name := ImportedName;
          Imported_Chem.Values.DataType := rdtDouble;
          ConcItem.Value := rsObjectImportedValuesR + '("' + Imported_Chem.Name + '")';
          TransportAuxNames.Objects[AuxIndex] := Imported_Chem.Values;
        end
        else
        begin
          TransportAuxNames.Objects[AuxIndex] := nil;
          TimeSeries := Aux.StringValue;
          if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
          begin
            Assert(False);
          end;
          ConcItem.Value := ImportedTimeSeries;
        end;
      end;
    end;
  end;
  procedure CreateObsScreenObject(ACell: TGhbTimeItem);
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    CellId: TMfCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Ghb_Obs_%d', [Package.PackageName, ObjectCount]));
    AScreenObject.Name := NewName;
    AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(AScreenObject);
    AScreenObject.ElevationCount := ecOne;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.EvaluatedAt := eaBlocks;
    AScreenObject.Visible := False;
    AScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    AScreenObject.CreateMf6Obs;
    AScreenObject.Modflow6Obs.Name := ACell.Boundname;
    AScreenObject.Modflow6Obs.General := [ogGhb];

    CellId := ACell.Cellid;
    if Model.DisvUsed then
    begin
      CellId.Row := 1;
    end;
    ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
    APoint.x := ElementCenter.RotatedLocation.x;
    APoint.y := ElementCenter.RotatedLocation.y;
    AScreenObject.AddPoint(APoint, True);
    AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

  end;
  function CreateScreenObject(ACell: TGhbTimeItem; Period: Integer): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    AuxIFACE: TMf6BoundaryValue;
    BoundName: string;
    ObsList: TObservationList;
    AnObs: TObservation;
    ObsIndex: Integer;
    General: TObGenerals;
  begin
    Inc(ObjectCount);
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Ghb_%d_Period_%d', [Package.PackageName, ObjectCount, Period]));
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateGhbBoundary;
    result.ModflowGhbBoundary.FormulaInterpretation := fiDirect;
    if IfaceIndex >= 0 then
    begin
      AuxIFACE := ACell[IfaceIndex];
      Assert(AuxIFACE.ValueType = vtNumeric);
      IFACE := Round(AuxIFACE.NumericValue);
    end
    else
    begin
      IFACE := 0;
    end;
    result.IFACE := TIface(IFACE+2);

    AddItem(result, ACell, Period);

    BoundName := UpperCase(ACell.Boundname);
    if BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      result.CreateMf6Obs;
      General := [];
      for ObsIndex := 0 to ObsList.Count - 1 do
      begin
        AnObs := ObsList[ObsIndex];
        if AnsiSameText(AnObs.ObsType, 'ghb') then
        begin
          Include(General, ogGhb);
        end
        else if AnsiSameText(AnObs.ObsType, 'to-mvr') then
        begin
          Include(General, ogMvr);
        end
        else
        begin
          Assert(False);
        end;
      end;
      result.Modflow6Obs.Name := ACell.Boundname;
      result.Modflow6Obs.General := General;
    end;
  end;
begin
  MvrSource.LakeOutlet := nil;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing GHB package');
  end;
  // Get the MVR package.
  Mvr := GetMvr(MvrPackage, Package);

  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.GhbBoundary.IsSelected := True;

  Ghb := Package.Package as TGhb;
  Options := Ghb.Options;

  if Options.AUXMULTNAME <> '' then
  begin
    AuxMultIndex := Options.IndexOfAUXILIARY(Options.AUXMULTNAME);
  end
  else
  begin
    AuxMultIndex := -1;
  end;

  GhbMvrLinkList := TGhbMvrLinkList.Create;
  CellIds := TCellIdList.Create;
  ConnectionObjectLists := TGhbConnectionObjectLists.Create;
  ConnectionDictionary := TDictionary<string, TGhbConnectionObjectList>.Create;
  ItemList := TList<TGhbItem>.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  CellIdObsDictionary := TCellIdObsDictionary.Create;
  Map := TimeSeriesMap.Create;
  ObsLists := TObsLists.Create;
  KeyStringDictionary := TDictionary<string, TMvrGhbTimeItemList>.Create;
  CellLists := TObjectList<TMvrGhbTimeItemList>.Create;
  OtherCellLists := TObjectList<TMvrGhbTimeItemList>.Create;
  try
    OtherCellLists.OwnsObjects := False;
  try
    if Mvr = nil then
    begin
      GhbMvrLink.MvrPeriod := nil;
      for PeriodIndex := 0 to Ghb.PeriodCount - 1 do
      begin
        GhbMvrLink.GhbPeriod := Ghb.Periods[PeriodIndex];
        GhbMvrLinkList.Add(GhbMvrLink)
      end;
    end
    else
    begin
      // Make sure that all the stress periods defined in either the MVR or the
      // Ghb package are imported.
      SetLength(GhbMvrLinkArray, Model.ModflowStressPeriods.Count);
      for PeriodIndex := 0 to Length(GhbMvrLinkArray) - 1 do
      begin
        GhbMvrLinkArray[PeriodIndex].GhbPeriod := nil;
        GhbMvrLinkArray[PeriodIndex].MvrPeriod := nil;
      end;

      for PeriodIndex := 0 to Ghb.PeriodCount - 1 do
      begin
        GhbPeriod := Ghb.Periods[PeriodIndex];
        if PeriodIndex < Ghb.PeriodCount - 1 then
        begin
          NextGhbPeriod := Ghb.Periods[PeriodIndex+1];
          EndPeriod := NextGhbPeriod.Period;
        end
        else
        begin
          EndPeriod := Model.ModflowStressPeriods.Count;
        end;
        for Index := GhbPeriod.Period  to EndPeriod do
        begin
          GhbMvrLinkArray[Index-1].GhbPeriod  := GhbPeriod;
        end;
      end;

      for PeriodIndex := 0 to Mvr.PeriodCount - 1 do
      begin
        MvrPeriod := Mvr.Periods[PeriodIndex];
        if PeriodIndex < Mvr.PeriodCount - 1 then
        begin
          NextMvrPeriod := Mvr.Periods[PeriodIndex+1];
          EndPeriod := NextMvrPeriod.Period;
        end
        else
        begin
          EndPeriod := Model.ModflowStressPeriods.Count;
        end;
        for Index := MvrPeriod.Period  to EndPeriod do
        begin
          GhbMvrLinkArray[Index-1].MvrPeriod  := MvrPeriod;
        end;
      end;

      GhbMvrLinkList.Add(GhbMvrLinkArray[0]);
      for Index := 1 to Length(GhbMvrLinkArray) - 1 do
      begin
        if (GhbMvrLinkArray[Index].GhbPeriod <> GhbMvrLinkArray[Index-1].GhbPeriod)
          or (GhbMvrLinkArray[Index].MvrPeriod <> GhbMvrLinkArray[Index-1].MvrPeriod) then
        begin
          GhbMvrLinkList.Add(GhbMvrLinkArray[Index]);
        end;
      end;
    end;

    IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
    for TimeSeriesIndex := 0 to Ghb.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := Ghb.TimeSeries[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;

    if Ghb.ObservationCount > 0 then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    end;
    for ObsPackageIndex := 0 to Ghb.ObservationCount - 1 do
    begin
      ObsFiles := Ghb.Observations[ObsPackageIndex].Package as TObs;
      GetObservations(nil, BoundNameObsDictionary,
        CellIdObsDictionary, ObsLists, ObsFiles);
    end;
    if Assigned(OnUpdateStatusBar) then
    begin
      OnUpdateStatusBar(self, 'importing GHB package');
    end;

    TransportAuxNames := TStringList.Create;
    try
      TransportAuxNames.CaseSensitive := False;
      for ModelIndex := 0 to TransportModels.Count - 1 do
      begin
        AModel := TransportModels[ModelIndex];
        TransportModel := AModel.FName as TTransportNameFile;
        for PackageIndex := 0 to TransportModel.NfPackages.Count  - 1 do
        begin
          APackage := TransportModel.NfPackages[PackageIndex];
          FoundMatch := False;
          if APackage.FileType = 'SSM6' then
          begin
            Ssm := APackage.Package as TSsm;
            for SourceIndex := 0 to Ssm.Sources.Count - 1 do
            begin
              if SameText(Ssm.Sources[SourceIndex].pname, Package.PackageName) then
              begin
                FoundMatch := True;
                TransportAuxNames.Add(Ssm.Sources[SourceIndex].auxname);
                break;
              end;
            end;
//            Assert(FoundMatch);
            break;
          end;
        end;
      end;

      LastTime := Model.ModflowStressPeriods.Last.EndTime;

      ACellList := nil;
      ObjectCount := 0;
      for PeriodIndex := 0 to GhbMvrLinkList.Count - 1 do
      begin
//        KeyStringDictionary.Clear;
        GhbMvrLink := GhbMvrLinkList[PeriodIndex];
        APeriod := GhbMvrLinkList[PeriodIndex].GhbPeriod;
        if APeriod = nil then
        begin
          Continue;
        end;
        StartTime := Model.ModflowStressPeriods[GhbMvrLink.Period-1].StartTime;
        for GhbIndex := 0 to ItemList.Count - 1 do
        begin
          AnItem := ItemList[GhbIndex];
          AnItem.EndTime := StartTime;
        end;
        ItemList.Clear;

        for CellListIndex := 0 to CellLists.Count - 1 do
        begin
          CellLists[CellListIndex].Clear;
        end;

        for CellListIndex := 0 to CellLists.Count - 1 do
        begin
          CellLists[CellListIndex].Clear
        end;


        // Assign all cells in the current period to a cell list.
        for CellIndex := 0 to APeriod.Count - 1 do
        begin
          ACell := APeriod[CellIndex];

          if (ACell.Boundname <> '')
            and BoundNameObsDictionary.ContainsKey(UpperCase(ACell.Boundname)) then
          begin
            KeyString := 'BN:' + UpperCase(ACell.Boundname) + ' ';
          end
          else
          begin
            KeyString := '';
          end;

          if IfaceIndex < 0 then
          begin
            IFACE := 0;
          end
          else
          begin
            AuxIFACE := ACell[IfaceIndex];
            Assert(AuxIFACE.ValueType = vtNumeric);
            IFACE := Round(AuxIFACE.NumericValue);
          end;
          KeyString := KeyString + ACell.Keystring + ' IFACE:' + IntToStr(IFACE);

          MvrUsed := False;
          if GhbMvrLink.MvrPeriod <> nil then
          begin
            if GhbMvrLink.MvrPeriod.HasSource(Package.PackageName, ACell.Id) then
            begin
              KeyString := KeyString + ' MVR ' + InttoStr(ACell.Id);
              MvrUsed := True;
            end;
          end;

          if not KeyStringDictionary.TryGetValue(KeyString, ACellList) then
          begin
            ACellList := TMvrGhbTimeItemList.Create;
            CellLists.Add(ACellList);
            KeyStringDictionary.Add(KeyString, ACellList);
          end;
          ACellList.Add(ACell);
          if MvrUsed then
          begin
            if ACellList.FIds.IndexOf(ACell.Id) < 0 then
            begin
              ACellList.FIds.Add(ACell.Id);
            end;
          end;
        end;

        // After all the cells in the current period have been read,
        // create a TScreenObject for each cell list
        AScreenObject := nil;
        for ObjectIndex := 0 to CellLists.Count - 1 do
        begin
          NewScreenObject := False;
          ACellList := CellLists[ObjectIndex];
          if ACellList.Count > 0 then
          begin
            FirstCell := ACellList[0];
            if (FirstCell.Boundname <> '')
              and BoundNameObsDictionary.ContainsKey(UpperCase(FirstCell.Boundname)) then
            begin
              if IfaceIndex < 0 then
              begin
                IFACE := 0;
              end
              else
              begin
                AuxIFACE := FirstCell[IfaceIndex];
                Assert(AuxIFACE.ValueType = vtNumeric);
                IFACE := Round(AuxIFACE.NumericValue);
              end;
              BoundName := UpperCase(FirstCell.Boundname);
              if not ConnectionDictionary.TryGetValue(BoundName, AConnectionList) then
              begin
                AConnectionList := TGhbConnectionObjectList.Create;
                ConnectionObjectLists.Add(AConnectionList);
                ConnectionDictionary.Add(BoundName, AConnectionList)
              end;
              ACellList.Sort;
              AScreenObject := nil;
              for ConnectionIndex := 0 to AConnectionList.Count - 1 do
              begin
                ConnectionItem := AConnectionList[ConnectionIndex];
                if (IFACE = ConnectionItem.IFACE)
                  and ACellList.SameCells(ConnectionItem.List) then
                begin
                  AScreenObject := ConnectionItem.ScreenObject;
                  Break;
                end;
              end;
              if AScreenObject = nil then
              begin
                AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
                ConnectionItem := TGhbConnection.Create;
                ConnectionItem.ScreenObject := AScreenObject;
                ConnectionItem.IFACE := IFACE;
                ConnectionItem.List := ACellList;
                AConnectionList.Add(ConnectionItem);
                OtherCellLists.Add(ACellList);
  //              CellLists.OwnsObjects := False;
  //              try
  //                CellLists[ObjectIndex] := nil;
  //              finally
  //                CellLists.OwnsObjects := True;
  //              end;
                NewScreenObject := True;
              end
              else
              begin
                AddItem(AScreenObject, FirstCell, APeriod.Period);
              end;
            end
            else
            begin
              AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
              NewScreenObject := True;
            end;
          end;

          CellIds.Clear;
          for CellIndex := 0 to ACellList.Count - 1 do
          begin
            ACell := ACellList[CellIndex];
            if ACell.Cond.ValueType = vtNumeric then
            begin
              if AuxMultIndex >= 0 then
              begin
                Aux := ACell.Aux[AuxMultIndex];
                if Aux.ValueType = vtNumeric then
                begin
                  AuxMultiplier := Aux.NumericValue
                end
                else
                begin
                  AuxMultiplier := 1;
                  FErrorMessages.Add(StrModelMuseCanNotIm);
                end;
              end
              else
              begin
                AuxMultiplier := 1;
              end;
              Imported_Ghb_Conductance.Values.Add(ACell.Cond.NumericValue * AuxMultiplier);
            end;

            if ACell.BHead.ValueType = vtNumeric then
            begin
              Imported_Ghb_Stage.Values.Add(ACell.BHead.NumericValue);
            end;

            for AuxIndex := 0 to TransportAuxNames.Count - 1 do
            begin
              ChemSpeciesName := TransportAuxNames[AuxIndex];
              GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
              Aux := ACell[GwtAuxIndex];
              if Aux.ValueType = vtNumeric then
              begin
                Values := TransportAuxNames.Objects[AuxIndex] as TValueArrayStorage;
                Values.Add(Aux.NumericValue);
              end;
            end;

            if NewScreenObject then
            begin
              CellIds.Add(ACell.Cellid);
            end;

            if CellIdObsDictionary.ContainsKey(ACell.Cellid) then
            begin
              CreateObsScreenObject(ACell);
            end;
          end;

          if NewScreenObject then
          begin
            AddPointsToScreenObject(CellIds, AScreenObject);
          end;

          if ACellList.FIds.Count > 0 then
          begin
            MvrSource.ScreenObject := AScreenObject;
            MvrSource.PackageName := Package.PackageName;
            MvrSource.Period := GhbMvrLink.Period;
            MvrSource.IDs := ACellList.FIds.ToArray;
            MvrSource.SourceType := mspcGhb;
            FMvrSources.Add(MvrSource);
          end;
        end;
      end;

    finally
      TransportAuxNames.Free;
    end;
  finally
    for CellListIndex := 0 to OtherCellLists.Count - 1 do
    begin
      CellLists.Extract(OtherCellLists[CellListIndex])
    end;
  end;

  finally
    BoundNameObsDictionary.Free;
    CellIdObsDictionary.Free;
    Map.Free;
    ObsLists.Free;
    KeyStringDictionary.Free;
    CellLists.Free;
    ItemList.Free;
    ConnectionObjectLists.Free;
    ConnectionDictionary.Free;
    CellIds.Free;
    GhbMvrLinkList.Free;
    OtherCellLists.Free;
  end;
end;

procedure TModflow6Importer.ImportGwfObs(Package: TPackage);
var
  Obs: TObs;
  Model: TPhastModel;
  Mf6ObservationUtility: TMf6ObservationUtility;
  FileIndex: Integer;
  ObsFile: TObsFile;
  ObsIndex: Integer;
  Observation: TObservation;
  ScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  Modflow6Obs: TModflow6Obs;
  APoint: TPoint2D;
  CellId: TMfCellId;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing OBS package');
  end;
  Obs := Package.Package as TObs;
  Model := frmGoPhast.PhastModel;
  Mf6ObservationUtility := Model.ModflowPackages.Mf6ObservationUtility;
  Mf6ObservationUtility.IsSelected := True;
  if Obs.Options.Digits > 0 then
  begin
    Mf6ObservationUtility.OutputFormat := ofText;
    Mf6ObservationUtility.Digits := Obs.Options.Digits;
  end
  else
  begin
    Mf6ObservationUtility.OutputFormat := ofBinary;
  end;

  for FileIndex := 0 to Obs.FileCount - 1 do
  begin
    ObsFile := Obs[FileIndex];
    for ObsIndex := 0 to ObsFile.Count - 1 do
    begin
      Observation := ObsFile[ObsIndex];

      if AnsiSameText(Observation.ObsType, 'head')
        or AnsiSameText(Observation.ObsType, 'drawdown') then
      begin

        ScreenObject := TScreenObject.CreateWithViewDirection(
          Model, vdTop, UndoCreateScreenObject, False);
        ScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

        Model.AddScreenObject(ScreenObject);
        ScreenObject.ElevationCount := ecOne;
        ScreenObject.SetValuesOfIntersectedCells := True;
        ScreenObject.EvaluatedAt := eaBlocks;
        ScreenObject.Visible := False;
        ScreenObject.Capacity := 1;

        CellId := Observation.CellId1;
        if Model.DisvUsed then
        begin
          Dec(CellId.Column);
          CellId.Row := 0;
        end
        else
        begin
          Dec(CellId.Column);
          Dec(CellId.Row);
        end;
        Assert(Observation.IdType1 = itCell);
        APoint := Model.TwoDElementCenter(CellId.Column, CellId.Row);
        ScreenObject.AddPoint(APoint, True);
        ScreenObject.ElevationFormula := Format('LayerCenter(%d)', [CellId.Layer]);

        Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
        ScreenObject.CreateMf6Obs;
        Modflow6Obs := ScreenObject.Modflow6Obs;
        Modflow6Obs.Name := Observation.ObsName;
        if AnsiSameText(Observation.ObsType, 'head') then
        begin
          Modflow6Obs.General := [ogHead];
        end
        else
        begin
          Modflow6Obs.General := [ogDrawdown];
        end;

      end
      else if AnsiSameText(Observation.ObsType, 'flow-ja-face') then
      begin
        FErrorMessages.Add(Format('ModelMuse could not import the observation "%s" because it is a flow-ja-face observation', [Observation.ObsName] ));
      end
      else
      begin
        FErrorMessages.Add(Format('ModelMuse could not import the observation "%s" because it is not a recognized type', [Observation.ObsName] ));
      end;
    end;
  end;
end;

procedure TModflow6Importer.ImportHfb(Package: TPackage);
const
  KImportedHfbValue = 'ImportedHfbValue';
var
  Hfb: THfb;
  PeriodIndex: Integer;
  Model: TPhastModel;
  LastTime: Double;
  HfbPeriod: THfbStressPeriod;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  NewItem: THfbItem;
  StartTime: Double;
  Item: THfbItem;
  ScreenObjects: Array of TScreenObject;
  LayerIndex: Integer;
  ItemIndex: Integer;
  Barrier: THfbCellPair;
  Layer: Integer;
  Storage: TValueArrayItem;
  StorageValues: TValueArrayStorage;
  Point1: TPoint2D;
  Point2: TPoint2D;
  TwoDGrid: TModflowIrregularGrid2D;
  Cell1: TModflowIrregularCell2D;
  Cell2: TModflowIrregularCell2D;
  BoundarySegment: TSegment2D;
  Column: Integer;
  Row: Integer;
  function CreateScreenObject(LayerIndex: Integer): TScreenObject;
  begin
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    result.Name := Format('Imported_HFB_Layer_%d_Period_%d', [LayerIndex + 1, HfbPeriod.Period]);
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := Format('LayerCenter(%d)', [LayerIndex+1]);
    ScreenObjects[LayerIndex] := result;

    Storage := result.ImportedValues.Add;
    Storage.Name := KImportedHfbValue;
    Storage.Values.DataType := rdtDouble;

    result.CreateHfbBoundary;

    NewItem := result.ModflowHfbBoundary.Values.Add as THfbItem;
    NewItem.StartTime := StartTime;
    NewItem.EndTime := LastTime;
    NewItem.HydraulicConductivity := rsObjectImportedValuesR + '("' + KImportedHfbValue + '")';
    NewItem.Thickness := '1';
  end;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing HFB package');
  end;
  Model := frmGoPhast.PhastModel;
  SetLength(ScreenObjects, Model.LayerCount);
  for LayerIndex := 0 to Length(ScreenObjects) - 1 do
  begin
    ScreenObjects[LayerIndex] := nil;
  end;
  Model.ModflowPackages.HfbPackage.IsSelected := True;
  LastTime := Model.ModflowStressPeriods.Last.EndTime;

  Hfb := Package.Package as THfb;

  for PeriodIndex := 0 to Hfb.Count - 1 do
  begin
    HfbPeriod := Hfb[PeriodIndex];
    StartTime := Model.ModflowStressPeriods[HfbPeriod.Period-1].StartTime;
    for LayerIndex := 0 to Length(ScreenObjects) - 1 do
    begin
      AScreenObject := ScreenObjects[LayerIndex];
      if AScreenObject <> nil then
      begin
        Item := AScreenObject.ModflowHfbBoundary.Values.Last as THfbItem;
        Item.EndTime := StartTime;
        ScreenObjects[LayerIndex] := nil;
      end;
    end;
    if HfbPeriod.Count > 0 then
    begin
      for ItemIndex := 0 to HfbPeriod.Count - 1 do
      begin
        Barrier := HfbPeriod[ItemIndex];
        Layer := Barrier.CellId1.Layer;
        Assert(Layer = Barrier.CellId2.Layer);
        AScreenObject := ScreenObjects[Layer-1];
        if AScreenObject = nil then
        begin
          AScreenObject := CreateScreenObject(Layer-1);
        end;
        StorageValues := AScreenObject.ImportedValues.ValuesByName(KImportedHfbValue);
        StorageValues.Add(Barrier.hydchr);

        if Model.DisvUsed then
        begin
          TwoDGrid := Model.DisvGrid.TwoDGrid;
          Cell1 := TwoDGrid.Cells[Barrier.CellId1.Column-1];
          Cell2 := TwoDGrid.Cells[Barrier.CellId2.Column-1];
          if Cell1.BoundarySegment(Cell2, BoundarySegment) then
          begin
            AScreenObject.AddPoint(BoundarySegment[1], True);
            AScreenObject.AddPoint(BoundarySegment[2], False);
          end
          else
          begin
            FErrorMessages.Add(Format('Error importing HRB because Cells %d and %d are not neighbors', [Barrier.CellId1.Column, Barrier.CellId2.Column]));
          end;
        end
        else
        begin
          Assert((Abs(Barrier.CellId1.Column - Barrier.CellId2.Column) = 1)
            xor (Abs(Barrier.CellId1.Row - Barrier.CellId2.Row) = 1));

          Column := Max(Barrier.CellId1.Column, Barrier.CellId2.Column)-1;
          Row := Max(Barrier.CellId1.Row, Barrier.CellId2.Row)-1;
          Point1 := Model.Grid.TwoDElementCorner(Column, Row);

          if Barrier.CellId1.Column = Barrier.CellId2.Column then
          begin
            Inc(Column);
          end
          else
          begin
            Inc(Row);
          end;

          Point2 := Model.Grid.TwoDElementCorner(Column, Row);
          AScreenObject.AddPoint(Point1, True);
          AScreenObject.AddPoint(Point2, False);
        end;
      end;
    end;
  end;
end;

procedure TModflow6Importer.ImportIc(Package: TPackage);
var
  IC: TIc;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing IC package');
  end;
  IC := Package.Package as TIc;
  Assign3DRealDataSet(rsModflow_Initial_Head, IC.GridData.STRT);
end;

procedure TModflow6Importer.ImportIMS;
var
  SimIndex: Integer;
  ASimulation: TMf6Simulation;
  SolutionGroupIndex: Integer;
  ASolutionGroup: TSolutionGroup;
  SolutionIndex: Integer;
  ASolution: TSolution;
//  APackage: TPackage;
  Ims: TIms;
  ImsPackage: TSmsPackageSelection;
  Options: TImsOptions;
  NonLinear: TImsNonLinear;
  SmsOverrides: TSmsOverrides;
  Linear: TImsLinear;
  ModelIndex: Integer;
  ModelIms: TSmsPackageSelection;
begin
  for SimIndex := 0 to FSimulations.Count - 1 do
  begin
    ASimulation := FSimulations[SimIndex];
    for SolutionGroupIndex := 0 to ASimulation.SolutionGroupCount - 1 do
    begin
      ASolutionGroup := ASimulation.SolutionGroups[SolutionGroupIndex];
      for SolutionIndex := 0 to ASolutionGroup.Count - 1 do
      begin
        ASolution := ASolutionGroup[SolutionIndex];
//        if AnsiSameText(ASolution.SolutionType, 'IMS6') then
        begin
//          APackage := TPackage.Create;
          try
//            APackage.FileType := 'IMS6';
//            APackage.FileName := ASolution.SolutionFileName;
//            APackage.PackageName := '';

            Ims := ASolution.Ims;
//            APackage.Package := Ims;
//            APackage.ReadPackage(ASimulation.OutFile, 0);

            ImsPackage := TSmsPackageSelection.Create(nil);
            try
              {$REGION 'Options'}
              Options := Ims.Options;
              if Options.PRINT_OPTION <> '' then
              begin
                if AnsiSameText(Options.PRINT_OPTION, 'NONE') then
                begin
                  ImsPackage.Print := spPrintNone;
                end
                else if AnsiSameText(Options.PRINT_OPTION, 'SUMMARY') then
                begin
                  ImsPackage.Print := spSummary;
                end
                else if AnsiSameText(Options.PRINT_OPTION, 'ALL') then
                begin
                  ImsPackage.Print := spFull;
                end
                else
                begin
                  Assert(False);
                end;
              end;

              if Options.COMPLEXITY <> '' then
              begin
                if AnsiSameText(Options.COMPLEXITY, 'SIMPLE') then
                begin
                  ImsPackage.Complexity := scoSimple;
                end
                else if AnsiSameText(Options.COMPLEXITY, 'MODERATE') then
                begin
                  ImsPackage.Complexity := scoModerate;
                end
                else if AnsiSameText(Options.COMPLEXITY, 'COMPLEX') then
                begin
                  ImsPackage.Complexity := scoComplex;
                end
                else
                begin
                  Assert(False);
                end;
              end;

              if Options.CSV_OUTER_OUTPUT <> '' then
              begin
                ImsPackage.CsvOutput := sspAll;
              end;

              if Options.CSV_INNER_OUTPUT <> '' then
              begin
                ImsPackage.CsvInnerOutput := sspAll;
              end;

              if Options.NO_PTC <> '' then
              begin
                if AnsiSameText(Options.NO_PTC, 'NO_PTC') then
                begin
                  ImsPackage.UsePTC := upDontUseForAll
                end;

                if AnsiSameText(Options.no_ptc_option, 'FIRST') then
                begin
                  ImsPackage.UsePTC := upDontUseForFirst
                end;
              end;

              if Options.ATS_OUTER_MAXIMUM_FRACTION >= 0 then
              begin
                ImsPackage.AtsOuterMaxFraction :=
                  Options.ATS_OUTER_MAXIMUM_FRACTION
              end;
              {$ENDREGION}

              SmsOverrides := [];
              {$REGION 'NonLinear'}
              NonLinear := Ims.NonLinear;

              if NonLinear.OUTER_DVCLOSE >= 0 then
              begin
                Include(SmsOverrides, soOuterHclose);
                ImsPackage.OuterHclose := NonLinear.OUTER_DVCLOSE;
              end;

              if NonLinear.OUTER_MAXIMUM >= 0 then
              begin
                Include(SmsOverrides, soOuterMaxIt);
                ImsPackage.MaxOuterIterations := NonLinear.OUTER_MAXIMUM;
              end;

              if NonLinear.UNDER_RELAXATION <> '' then
              begin
                Include(SmsOverrides, soUnderRelax);
                if AnsiSameText(NonLinear.UNDER_RELAXATION, 'NONE') then
                begin
                  ImsPackage.UnderRelaxation := surNone;
                end
                else if AnsiSameText(NonLinear.UNDER_RELAXATION, 'SIMPLE') then
                begin
                  ImsPackage.UnderRelaxation := surSimple;
                end
                else if AnsiSameText(NonLinear.UNDER_RELAXATION, 'COOLEY') then
                begin
                  ImsPackage.UnderRelaxation := surCooley;
                end
                else if AnsiSameText(NonLinear.UNDER_RELAXATION, 'DBD') then
                begin
                  ImsPackage.UnderRelaxation := surDbd;
                end
                else
                begin
                  Assert(False);
                end
              end;

              if NonLinear.UNDER_RELAXATION_GAMMA >= 0 then
              begin
                Include(SmsOverrides, soUnderRelaxGamma);
                ImsPackage.UnderRelaxGamma := NonLinear.UNDER_RELAXATION_GAMMA;
              end;

              if NonLinear.UNDER_RELAXATION_THETA >= 0 then
              begin
                Include(SmsOverrides, soUnderRelaxTheta);
                ImsPackage.UnderRelaxTheta := NonLinear.UNDER_RELAXATION_THETA;
              end;

              if NonLinear.UNDER_RELAXATION_KAPPA >= 0 then
              begin
                Include(SmsOverrides, soUnderRelaxKappa);
                ImsPackage.UnderRelaxKappa := NonLinear.UNDER_RELAXATION_KAPPA;
              end;

              if NonLinear.UNDER_RELAXATION_MOMENTUM >= 0 then
              begin
                Include(SmsOverrides, soUnderRelaxMomentum);
                ImsPackage.UnderRelaxMomentum := NonLinear.UNDER_RELAXATION_MOMENTUM;
              end;

              if NonLinear.BACKTRACKING_NUMBER >= 0 then
              begin
                Include(SmsOverrides, soBacktrackingNumber);
                ImsPackage.BacktrackingNumber := NonLinear.BACKTRACKING_NUMBER;
              end;

              if NonLinear.BACKTRACKING_TOLERANCE >= 0 then
              begin
                Include(SmsOverrides, soBacktrackingTolerance);
                ImsPackage.BacktrackingTolerance := NonLinear.BACKTRACKING_TOLERANCE;
              end;

              if NonLinear.BACKTRACKING_REDUCTION_FACTOR >= 0 then
              begin
                Include(SmsOverrides, soBacktrackingReductionFactor);
                ImsPackage.BacktrackingReductionFactor := NonLinear.BACKTRACKING_REDUCTION_FACTOR;
              end;


              if NonLinear.BACKTRACKING_RESIDUAL_LIMIT >= 0 then
              begin
                Include(SmsOverrides, soBacktrackingResidualLimit);
                ImsPackage.BacktrackingResidualLimit := NonLinear.BACKTRACKING_RESIDUAL_LIMIT;
              end;

              {$ENDREGION}

            {$REGION 'Linear'}
              Linear := Ims.Linear;

              if Linear.INNER_MAXIMUM >= 0  then
              begin
                Include(SmsOverrides, soInnerMaxIterations);
                ImsPackage.InnerMaxIterations := Linear.INNER_MAXIMUM;
              end;

              if Linear.INNER_DVCLOSE >= 0  then
              begin
                Include(SmsOverrides, soInnerHclose);
                ImsPackage.InnerHclose := Linear.INNER_DVCLOSE;
              end;

              if Linear.INNER_RCLOSE >= 0  then
              begin
                Include(SmsOverrides, soInnerRclose);
                ImsPackage.InnerRclose := Linear.INNER_RCLOSE;
              end;

              if Linear.rclose_option <> ''  then
              begin
                Include(SmsOverrides, soRcloseOption);
                if AnsiSameText(Linear.rclose_option, 'STRICT') then
                begin
                  ImsPackage.RcloseOption := sroStrict;
                end
                else if AnsiSameText(Linear.rclose_option, 'L2NORM_RCLOSE') then
                begin
                  ImsPackage.RcloseOption := sroL2Norm;
                end
                else if AnsiSameText(Linear.rclose_option, 'RELATIVE_RCLOSE') then
                begin
                  ImsPackage.RcloseOption := sroRelative;
                end
                else
                begin
                  Assert(False);
                end;
              end;

              if Linear.LINEAR_ACCELERATION <> ''  then
              begin
                Include(SmsOverrides, soLinLinearAcceleration);
                if AnsiSameText(Linear.LINEAR_ACCELERATION, 'CG') then
                begin
                  ImsPackage.LinLinearAcceleration := sllaCg;
                end
                else if AnsiSameText(Linear.LINEAR_ACCELERATION, 'BICGSTAB') then
                begin
                  ImsPackage.LinLinearAcceleration := sllaBiCgStab;
                end
                else
                begin
                  Assert(False);
                end;
              end;

              if Linear.RELAXATION_FACTOR >= 0  then
              begin
                Include(SmsOverrides, soRelaxationFactor);
                ImsPackage.RelaxationFactor := Linear.RELAXATION_FACTOR;
              end;

              if Linear.PRECONDITIONER_LEVELS >= 0  then
              begin
                Include(SmsOverrides, soPreconditionerLevel);
                ImsPackage.PreconditionerLevel := Linear.PRECONDITIONER_LEVELS;
              end;

              if Linear.PRECONDITIONER_DROP_TOLERANCE >= 0  then
              begin
                Include(SmsOverrides, soPreconditionerDropTolerance);
                ImsPackage.PreconditionerDropTolerance := Linear.PRECONDITIONER_DROP_TOLERANCE;
              end;

              if Linear.NUMBER_ORTHOGONALIZATIONS >= 0  then
              begin
                Include(SmsOverrides, soNumberOfOrthoganalizations);
                ImsPackage.NumberOfOrthoganalizations := Linear.NUMBER_ORTHOGONALIZATIONS;
              end;

              if Linear.SCALING_METHOD <> ''  then
              begin
                Include(SmsOverrides, soScalingMethod);
                if AnsiSameText(Linear.SCALING_METHOD, 'DIAGONAL') then
                begin
                  ImsPackage.ScalingMethod := ssmDiagonal;
                end
                else if AnsiSameText(Linear.SCALING_METHOD, 'L2NORM') then
                begin
                  ImsPackage.ScalingMethod := ssmL2Norm;
                end
                else if AnsiSameText(Linear.SCALING_METHOD, 'NONE') then
                begin
                  ImsPackage.ScalingMethod := ssmNone;
                end
                else
                begin
                  Assert(False);
                end;
              end;

              if Linear.REORDERING_METHOD <> ''  then
              begin
                Include(SmsOverrides, soReorderingMethod);
                if AnsiSameText(Linear.REORDERING_METHOD, 'RCM') then
                begin
                  ImsPackage.ReorderingMethod := srmReverseCuthillMcKee;
                end
                else if AnsiSameText(Linear.REORDERING_METHOD, 'MD') then
                begin
                  ImsPackage.ReorderingMethod := srmMinimumDegreeOrdering;
                end
                else if AnsiSameText(Linear.REORDERING_METHOD, 'NONE') then
                begin
                  ImsPackage.ReorderingMethod := srmNone;
                end
                else
                begin
                  Assert(False);
                end;
              end;
            {$ENDREGION}

            ImsPackage.SmsOverrides := SmsOverrides;


            for ModelIndex := 0 to ASolution.FSolutionModelNames.Count - 1 do
            begin
              ModelIms := GetIms(ASolution.FSolutionModelNames[ModelIndex]);
              ModelIms.Assign(ImsPackage);
            end;

            finally
              ImsPackage.Free;
            end;

          finally
//            APackage.Free;
          end;
//        end
//        else
//        begin
//          Assert(False);
        end;
      end;
    end;
  end;
end;

type
  TOutletDictionary = TDictionary<Integer,TNumberedItemList>;

  TImportLake = class(TObject)
  private
    FConnections: TLakConnectionItemList;
    FOutlets: TLakOutletItemList;
    FLakPackageItem: TLakPackageItem;
    DataSetsScreenObject: TScreenObject;
    LakeScreenObject: TScreenObject;
    LakeBoundary: TLakeMf6;
    FLakeSettings: TNumberedItemList;
    FOutletSettings: TOutletDictionary;
    FNumberedItemLists: TNumberedItemLists;
    HasOutletSettings: Boolean;
  public
    constructor Create(LakPackageItem: TLakPackageItem);
    destructor Destroy; override;
  end;
  TImportLakes = TObjectList<TImportLake>;

Type
  TLakMvrLink = record
    LakPeriod: TLakPeriod;
    MvrPeriod: TMvrPeriod;
    function Period: Integer;
  end;
  TLakMvrLinkArray = TArray<TLakMvrLink>;
  TLakMvrLinkList = TList<TLakMvrLink>;

procedure TModflow6Importer.ImportLak(Package: TPackage;
  TransportModels: TModelList; MvrPackage: TPackage);
var
  Model: TPhastModel;
  LakPackage: TLakeMf6PackageSelection;
  Lak: TLak;
  Options: TLakOptions;
  Lakes: TImportLakes;
  LakeIndex: Integer;
  LakPackageItem: TLakPackageItem;
  ConnectionIndex: Integer;
  AConnection: TLakConnectionItem;
  ALake: TImportLake;
  DummyConnectionItem: TLakConnectionItem;
  OutletIndex: Integer;
  AnOutlet: TMf6LakOutletItem;
  StartTime: Double;
  EndTime: Double;
  Map: TimeSeriesMap;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  PeriodIndex: Integer;
  APeriod: TLakPeriod;
  Period: Integer;
  ASetting: TNumberedItem;
  SettingIndex: Integer;
  SettingName: string;
  LakeNo: Integer;
  OutletNumber: Integer;
  OutletList: TNumberedItemList;
  TableIndex: Integer;
  TablePackage: TPackage;
  ATable: TMf6LakeTableItem;
  Mvr: TMvr;
  LakMvrLinkArray: TLakMvrLinkArray;
  LakMvrLinkList: TLakMvrLinkList;
  LakMvrLink: TLakMvrLink;
  StressPeriodIndex: Integer;
  MvrPeriod: TMvrPeriod;
  LakPeriod: TLakPeriod;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  NumberObsDictionary: TNumberDictionary;
  BoundNameObsDictionary: TBoundNameDictionary;
  ObsLists: TObsLists;
  ObsNameIndex: Integer;
  procedure ApplyLakeSettings(ALake: TImportLake);
  var
    PriorValueItem: TLakeTimeItem;
    NewTimeItem: TLakeTimeItem;
    ASetting: TNumberedItem;
    SettingIndex: Integer;
    TimeSeriesName: string;
    OutletIndex: Integer;
    MfOutLet: TLakeOutlet;
    ImportOutlet: TMf6LakOutletItem;
    ASettingsList: TNumberedItemList;
    PriorOutletItem: TLakeOutletTimeItem;
    NewOutletItem: TLakeOutletTimeItem;
    function GetFloatFormulaFromSetting(ASetting: TNumberedItem): string;
    begin
      result := '';
      if ASetting.StringValue <> '' then
      begin
        if Map.TryGetValue(UpperCase(ASetting.StringValue), TimeSeriesName) then
        begin
          result := TimeSeriesName;
        end
        else
        begin
          Assert(False);
        end;
      end
      else
      begin
        result := FortranFloatToStr(ASetting.FloatValue);
      end;
    end;
  begin
    if (ALake.FLakeSettings.Count > 0) or (ALake.HasOutletSettings) then
    begin
      PriorValueItem := ALake.LakeBoundary.Values.Last as TLakeTimeItem;
      if Period > 1 then
      begin
        NewTimeItem := ALake.LakeBoundary.Values.Add as TLakeTimeItem;
        NewTimeItem.Assign(PriorValueItem);
        PriorValueItem.EndTime := StartTime;
      end
      else
      begin
        NewTimeItem := PriorValueItem
      end;
      NewTimeItem.StartTime := StartTime;
      for SettingIndex := 0 to ALake.FLakeSettings.Count - 1 do
      begin
        ASetting := ALake.FLakeSettings[SettingIndex];
        SettingName := ASetting.Name;
        if AnsiSameText(SettingName, 'STATUS') then
        begin
          if AnsiSameText(ASetting.StringValue, 'ACTIVE') then
          begin
            NewTimeItem.Status := lsActive;
          end
          else if AnsiSameText(ASetting.StringValue, 'INACTIVE') then
          begin
            NewTimeItem.Status := lsInactive;
          end
          else if AnsiSameText(ASetting.StringValue, 'CONSTANT') then
          begin
            NewTimeItem.Status := lsConstant;
          end
          else
          begin
            Assert(False);
          end;
        end
        else if AnsiSameText(SettingName, 'STAGE') then
        begin
          NewTimeItem.Stage := GetFloatFormulaFromSetting(ASetting);
        end
        else if AnsiSameText(SettingName, 'RAINFALL') then
        begin
          NewTimeItem.Rainfall := GetFloatFormulaFromSetting(ASetting);
        end
        else if AnsiSameText(SettingName, 'EVAPORATION') then
        begin
          NewTimeItem.Evaporation := GetFloatFormulaFromSetting(ASetting);
        end
        else if AnsiSameText(SettingName, 'RUNOFF') then
        begin
          NewTimeItem.Runoff := GetFloatFormulaFromSetting(ASetting);
        end
        else if AnsiSameText(SettingName, 'INFLOW') then
        begin
          NewTimeItem.Inflow := GetFloatFormulaFromSetting(ASetting);
        end
        else if AnsiSameText(SettingName, 'WITHDRAWAL') then
        begin
          NewTimeItem.Withdrawal := GetFloatFormulaFromSetting(ASetting);
        end
        else if AnsiSameText(SettingName, 'AUXILIARY') then
        begin
        end
        else
        begin
        end;
      end;
      Assert(ALake.LakeBoundary.Outlets.Count = ALake.FOutlets.Count);
      for OutletIndex := 0 to ALake.LakeBoundary.Outlets.Count - 1 do
      begin
        MfOutLet := ALake.LakeBoundary.Outlets[OutletIndex].Outlet;
        ImportOutlet := ALake.FOutlets[OutletIndex];
        if ALake.FOutletSettings.TryGetValue(ImportOutlet.outletno, ASettingsList) then
        begin
          PriorOutletItem := MfOutLet.LakeTimes.Last as TLakeOutletTimeItem;
          if Period > 1 then
          begin
            NewOutletItem := MfOutLet.LakeTimes.Add;
            NewOutletItem.Assign(PriorOutletItem);
            PriorOutletItem.EndTime := StartTime;
            NewOutletItem.StartTime := StartTime;
          end
          else
          begin
            NewOutletItem := PriorOutletItem;
          end;
          for SettingIndex := 0 to ASettingsList.Count - 1 do
          begin
            ASetting := ASettingsList[SettingIndex];
            SettingName := ASetting.Name;
            if AnsiSameText(SettingName, 'RATE') then
            begin
              NewOutletItem.Rate := GetFloatFormulaFromSetting(ASetting);
            end
            else if AnsiSameText(SettingName, 'INVERT') then
            begin
              NewOutletItem.Invert := GetFloatFormulaFromSetting(ASetting);
            end
            else if AnsiSameText(SettingName, 'WIDTH') then
            begin
              NewOutletItem.Width := GetFloatFormulaFromSetting(ASetting);
            end
            else if AnsiSameText(SettingName, 'SLOPE') then
            begin
              NewOutletItem.Slope := GetFloatFormulaFromSetting(ASetting);
            end
            else if AnsiSameText(SettingName, 'ROUGH') then
            begin
              NewOutletItem.Roughness := GetFloatFormulaFromSetting(ASetting);
            end
            else
            begin
              Assert(False);
            end;
          end;
        end
        else
        begin
          Assert(False);
        end;
      end;
    end;
  end;
  procedure CreateLakeTable(ALake: TImportLake; TablePackage: TPackage);
  var
    LakeTableReader: TLakeTable;
    LakeTable: TLakeTableMf6;
    RowIndex: Integer;
    NewItem: TLakeTableItemMf6;
    ImportItem: TLakeTableItem;
  begin
    LakeTableReader := TablePackage.Package as TLakeTable;
    LakeTable := ALake.LakeBoundary.LakeTable;
    for RowIndex := 0 to LakeTableReader.Table.Count - 1 do
    begin
      NewItem := LakeTable.Add;
      ImportItem := LakeTableReader.Table[RowIndex];
      NewItem.Stage := FortranFloatToStr(ImportItem.stage);
      NewItem.Volume := FortranFloatToStr(ImportItem.volume);
      NewItem.SurfaceArea := FortranFloatToStr(ImportItem.sarea);
      if ImportItem.barea.Used then
      begin
        NewItem.ExchangeArea := FortranFloatToStr(ImportItem.barea.Value);
      end;
    end;
  end;
  procedure CreateOutlets(ALake: TImportLake);
  var
    OutletIndex: Integer;
    ImportOutlet: TMf6LakOutletItem;
    OtherLake: TImportLake;
    LakeOutlet: TLakeOutlet;
    OutletTimeItem: TLakeOutletTimeItem;
    TimeSeriesName: string;
    ImportedTimeSeries: string;
    OutletList: TNumberedItemList;
    MvrSource: TMvrSource;
    StressPeriodIndex: Integer;
    LakeOutletItem: TLakeOutletItem;
  begin
    for OutletIndex := 0 to ALake.FOutlets.Count - 1 do
    begin
      LakeOutletItem := ALake.LakeBoundary.Outlets.Add;
      LakeOutlet := LakeOutletItem.Outlet;
      ImportOutlet := ALake.FOutlets[OutletIndex];
      OutletList := TNumberedItemList.Create;
      ALake.FNumberedItemLists.Add(OutletList);
      ALake.FOutletSettings.Add(ImportOutlet.outletno, OutletList);
      Assert(ImportOutlet.lakein =  ALake.FLakPackageItem.lakeno);
      if ImportOutlet.lakeout > 0 then
      begin
        OtherLake := Lakes[ImportOutlet.lakeout-1];
        Assert(ImportOutlet.lakeout = OtherLake.FLakPackageItem.lakeno);
        LakeOutlet.OutletObjectName := OtherLake.LakeScreenObject.Name;
      end;

      MvrSource.ScreenObject := ALake.LakeScreenObject;
      MvrSource.LakeOutlet := LakeOutletItem;
      MvrSource.PackageName := Package.PackageName;
      SetLength(MvrSource.IDs, 1);
      MvrSource.SourceType := mspcLak;
      MvrSource.IDs[0] := ImportOutlet.outletno;
      for StressPeriodIndex := 0 to LakMvrLinkList.Count - 1 do
      begin
        MvrSource.Period := LakMvrLinkList[StressPeriodIndex].Period;
        FMvrSources.Add(MvrSource);
      end;


      if AnsiSameText(ImportOutlet.couttype, 'SPECIFIED') then
      begin
        LakeOutlet.OutletType := lotSpecified
      end
      else if AnsiSameText(ImportOutlet.couttype, 'MANNING') then
      begin
        LakeOutlet.OutletType := lotManning;
      end
      else if AnsiSameText(ImportOutlet.couttype, 'WEIR') then
      begin
        LakeOutlet.OutletType := lotWeir;
      end
      else
      begin
        Assert(False);
      end;

      OutletTimeItem := LakeOutlet.LakeTimes.Add;
      OutletTimeItem.StartTime := StartTime;
      OutletTimeItem.EndTime := EndTime;

      if ImportOutlet.invert.ValueType = vtNumeric then
      begin
        OutletTimeItem.Invert := FortranFloatToStr(ImportOutlet.invert.NumericValue)
      end
      else
      begin
        TimeSeriesName := ImportOutlet.invert.StringValue;
        if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeries) then
        begin
          Assert(False);
        end;
        OutletTimeItem.Invert := ImportedTimeSeries;
      end;

      if ImportOutlet.width.ValueType = vtNumeric then
      begin
        OutletTimeItem.Width := FortranFloatToStr(ImportOutlet.width.NumericValue)
      end
      else
      begin
        TimeSeriesName := ImportOutlet.width.StringValue;
        if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeries) then
        begin
          Assert(False);
        end;
        OutletTimeItem.Width := ImportedTimeSeries;
      end;

      if ImportOutlet.rough.ValueType = vtNumeric then
      begin
        OutletTimeItem.Roughness := FortranFloatToStr(ImportOutlet.rough.NumericValue)
      end
      else
      begin
        TimeSeriesName := ImportOutlet.rough.StringValue;
        if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeries) then
        begin
          Assert(False);
        end;
        OutletTimeItem.Roughness := ImportedTimeSeries;
      end;

      if ImportOutlet.slope.ValueType = vtNumeric then
      begin
        OutletTimeItem.Slope := FortranFloatToStr(ImportOutlet.slope.NumericValue)
      end
      else
      begin
        TimeSeriesName := ImportOutlet.slope.StringValue;
        if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeries) then
        begin
          Assert(False);
        end;
        OutletTimeItem.Slope := ImportedTimeSeries;
      end;
    end;
  end;
  procedure GetLakObservations(ALake: TImportLake; Name: string = '');
  var
    BoundName: string;
    Obs: TObservationList;
    LakeObs: TLakObs;
    OutletIndex: Integer;
    outletno: Integer;
    procedure AssignObs(OutletOnly, ObsAllowed: Boolean);
    var
      ObsIndex: Integer;
      AnObs: TObservation;
    begin
      for ObsIndex := 0 to Obs.Count - 1 do
      begin
        AnObs := Obs[ObsIndex];
        if AnsiSameText('stage', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loStage);
          end;
        end
        else if AnsiSameText('ext-inflow', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loExternalInflow);
          end;
        end
        else if AnsiSameText('outlet-inflow', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loSimOutletInflow);
          end;
        end
        else if AnsiSameText('inflow', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loSumInflow);
          end;
        end
        else if AnsiSameText('from-mvr', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loFromMvr);
          end;
        end
        else if AnsiSameText('rainfall', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loRain);
          end;
        end
        else if AnsiSameText('runoff', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loRunoff);
          end;
        end
        else if AnsiSameText('lak', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loFlowRate);
          end;
        end
        else if AnsiSameText('withdrawal', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loWithdrawal);
          end;
        end
        else if AnsiSameText('evaporation', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loEvap);
          end;
        end
        else if AnsiSameText('ext-outflow', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loExternalOutflow);
          end;
        end
        else if AnsiSameText('to-mvr', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loToMvr);
          end;
        end
        else if AnsiSameText('storage', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loStorage);
          end;
        end
        else if AnsiSameText('constant', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loConstantFlow);
          end;
        end
        else if AnsiSameText('outlet', AnObs.ObsType) then
        begin
          if ObsAllowed then
          begin
            Include(LakeObs, loOutlet);
          end;
        end
        else if AnsiSameText('volume', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loVolume);
          end;
        end
        else if AnsiSameText('surface-area', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loSurfaceArea);
          end;
        end
        else if AnsiSameText('wetted-area', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loWettedArea);
          end;
        end
        else if AnsiSameText('conductance', AnObs.ObsType) then
        begin
          if not OutletOnly then
          begin
            Include(LakeObs, loConductance);
          end;
        end
      end;
    end;
  begin
    LakeObs := [];
    BoundName := ALake.FLakPackageItem.boundname;
    if BoundName <> '' then
    begin
      if BoundNameObsDictionary.TryGetValue(UpperCase(BoundName), Obs) then
      begin
        if Obs.Count > 0 then
        begin
          AssignObs(False, True);
        end;
      end;
    end;
    if NumberObsDictionary.TryGetValue(ALake.FLakPackageItem.lakeno, Obs) then
    begin
      if Obs.Count > 0 then
      begin
        AssignObs(False, False);
      end;
    end;
    for OutletIndex := 0 to ALake.FOutlets.Count - 1 do
    begin
      outletno := ALake.FOutlets[OutletIndex].outletno;
      if NumberObsDictionary.TryGetValue(outletno, Obs) then
      begin
        if Obs.Count > 0 then
        begin
          AssignObs(True, True);
        end;
      end;
    end;
    if LakeObs <> [] then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      ALake.LakeScreenObject.CreateMf6Obs;
      ALake.LakeScreenObject.Modflow6Obs.LakObs := LakeObs;
      if ALake.LakeScreenObject.Modflow6Obs.Name = '' then
      begin
        if Name = '' then
        begin
          Inc(ObsNameIndex);
          ALake.LakeScreenObject.Modflow6Obs.Name := 'LAK_Obs_' + IntToStr(ObsNameIndex);
        end
        else
        begin
          ALake.LakeScreenObject.Modflow6Obs.Name := Name;
        end;
      end;
    end;
  end;
  procedure CreateScreenObject(ALake: TImportLake);
  const
    KImportedLakeK = 'LakeK';
    KImportedLakeBelev = 'Lake Bottom Elevation';
    KImportedLakeTelev = 'Lake Top Elevation';
  var
    DataArrayName: string;
    DataArray: TDataArray;
    UndoCreateScreenObject: TCustomUndo;
    AConnection: TLakConnectionItem;
    UniformValues: Boolean;
    FirstValue: Extended;
    CellIndex: Integer;
    ImportedK: TValueArrayItem;
    DataSetIndex: Integer;
    CellIds: TCellIdList;
    ACellId: TMfCellId;
    NewTimeItem: TLakeTimeItem;
    LakeConnectionTypes: TLakeConnectionTypes;
    MvrReceiver: TMvrReceiver;
    StressPeriodIndex: Integer;
    procedure CreateDataSetScreenObject;
    var
      CellIds: TCellIdList;
      CellIndex: Integer;
      ACellId: TMfCellId;
      UndoCreateScreenObject: TCustomUndo;
    begin
      ALake.DataSetsScreenObject := TScreenObject.CreateWithViewDirection(
        Model, vdTop, UndoCreateScreenObject, False);
      ALake.DataSetsScreenObject.Name := Format('ImportedDataLake%d', [ALake.FLakPackageItem.lakeno]);
      ALake.DataSetsScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

      Model.AddScreenObject(ALake.DataSetsScreenObject);
      ALake.DataSetsScreenObject.ElevationCount := ecOne;
      ALake.DataSetsScreenObject.SetValuesOfIntersectedCells := True;
      ALake.DataSetsScreenObject.EvaluatedAt := eaBlocks;
      ALake.DataSetsScreenObject.Visible := False;

      CellIds := TCellIdList.Create;
      try
        for CellIndex := 0 to ALake.FConnections.Count - 1 do
        begin
          ACellId := ALake.FConnections[CellIndex].cellid;
          if Model.DisvUsed then
          begin
            ACellId.Row := 0;
          end;
          CellIds.Add(ACellId);
        end;
        AddPointsToScreenObject(CellIds, ALake.DataSetsScreenObject, True);
      finally
        CellIds.Free;
      end;
      ALake.DataSetsScreenObject.ElevationFormula :=
        rsObjectImportedValuesR + '("' + StrImportedElevations + '")';
    end;
    procedure GetLakeInteriorCells;
    var
      Points: TPoint2DList;
      ConnectionIndex: Integer;
      CellId: TMfCellId;
      ElementCenter: TDualLocation;
      APoint: TPoint2D;
      InputPolyGon: TPolygon2D;
      OutputPolyGon: TPolygon2D;
      PointIndex: Integer;
      Temp: TScreenObject;
      UndoCreateScreenObject: TCustomUndo;
      CellList: TCellAssignmentList;
      CellIndex: Integer;
      ACellLocation: TCellAssignment;
      BoundaryCells: T2DBoolArray;
    begin
      // This method may add extra cells to the lake if the lake outline is concave.
      Points := TPoint2DList.Create;
      try
        SetLength(BoundaryCells, Model.RowCount, Model.ColumnCount);
        for ConnectionIndex := 0 to ALake.FConnections.Count - 1 do
        begin
          CellId := ALake.FConnections[ConnectionIndex].cellid;
          if Model.DisvUsed then
          begin
            CellId.Row := 1;
          end;
          ElementCenter := Model.ElementLocation[CellId.Layer - 1,
            CellId.Row - 1, CellId.Column - 1];
          BoundaryCells[CellId.Row - 1, CellId.Column - 1] := True;
          APoint.x := ElementCenter.RotatedLocation.x;
          APoint.y := ElementCenter.RotatedLocation.y;
          Points.Add(APoint)
        end;
        SetLength(InputPolyGon, Points.Count);
        for PointIndex := 0 to Points.Count - 1 do
        begin
          InputPolyGon[PointIndex] := Points[PointIndex];
        end;
        ConvexHull(InputPolyGon, OutputPolyGon);
        Temp := TScreenObject.CreateWithViewDirection(
          Model, vdTop, UndoCreateScreenObject, False);
        try
          Temp.ElevationCount := ecZero;
          Temp.SetPropertiesOfEnclosedCells := True;
          Temp.Capacity := Length(OutputPolyGon) + 1;
          Temp.AddPoint(OutputPolyGon[Length(OutputPolyGon)-1], True);
          for PointIndex := 0 to Length(OutputPolyGon) - 1 do
          begin
            Temp.AddPoint(OutputPolyGon[PointIndex], False);
          end;

          CellList := TCellAssignmentList.Create;
          try
            Temp.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
            ALake.LakeScreenObject.Capacity := CellList.Count;
            for CellIndex := 0 to CellList.Count - 1 do
            begin
              ACellLocation := CellList[CellIndex];
              if not BoundaryCells[ACellLocation.Row, ACellLocation.Column] then
              begin
                ElementCenter := Model.ElementLocation[ACellLocation.Layer,
                  ACellLocation.Row, ACellLocation.Column];
                APoint.x := ElementCenter.RotatedLocation.x;
                APoint.y := ElementCenter.RotatedLocation.y;
                ALake.LakeScreenObject.AddPoint(APoint, True);
              end;
            end;
          finally
            CellList.Free;
          end;
        finally
          Temp.Free;
        end;
      finally
        Points.Free;
      end;
    end;
  begin
    ObsNameIndex := 0;
    Assert(ALake <> nil);
    ALake.LakeScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    if ALake.FLakPackageItem.boundname = '' then
    begin
      ALake.LakeScreenObject.Name := Format('ImportedLake%d', [ALake.FLakPackageItem.lakeno]);
    end
    else
    begin
      ALake.LakeScreenObject.Name := ALake.FLakPackageItem.boundname;
    end;
    ALake.LakeScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    MvrReceiver.ScreenObject := ALake.LakeScreenObject;
    MvrReceiver.PackageName := Package.PackageName;
    SetLength(MvrReceiver.IDs, 1);
    MvrReceiver.ReceiverType := mrpcLak;
    MvrReceiver.IDs[0] := ALake.FLakPackageItem.lakeno;
    for StressPeriodIndex := 0 to LakMvrLinkList.Count - 1 do
    begin
      MvrReceiver.Period := LakMvrLinkList[StressPeriodIndex].Period;
      FMvrReceivers.Add(MvrReceiver);
    end;

    Model.AddScreenObject(ALake.LakeScreenObject);
    ALake.LakeScreenObject.ElevationCount := ecOne;
    ALake.LakeScreenObject.SetValuesOfIntersectedCells := True;
    ALake.LakeScreenObject.EvaluatedAt := eaBlocks;
    ALake.LakeScreenObject.Visible := False;

    CellIds := TCellIdList.Create;
    try
      for CellIndex := 0 to ALake.FConnections.Count - 1 do
      begin
        AConnection := ALake.FConnections[CellIndex];
        if AnsiSameText(AConnection.claktype, 'VERTICAL')
          or SameText(AConnection.claktype, 'EMBEDDEDH')
          or SameText(AConnection.claktype, 'EMBEDDEDV')
          then
        begin
          ACellId := AConnection.cellid;
          if Model.DisvUsed then
          begin
            ACellId.Row := 0;
          end;
          CellIds.Add(ACellId);
        end;
      end;
      if CellIds.Count > 0 then
      begin
        AddPointsToScreenObject(CellIds, ALake.LakeScreenObject, True);
      end
      else
      begin
        GetLakeInteriorCells;
      end;
    finally
      CellIds.Free;
    end;
//    end;
    ALake.LakeScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';


    ALake.LakeScreenObject.CreateLakMf6Boundary;
    ALake.LakeBoundary := ALake.LakeScreenObject.ModflowLak6;

    LakeConnectionTypes := [];
    for CellIndex := 0 to ALake.FConnections.Count - 1 do
    begin
      AConnection := ALake.FConnections[CellIndex];
      if AnsiSameText(AConnection.claktype, 'EMBEDDEDH') then
      begin
        Include(LakeConnectionTypes, lctHorizontal);
        ALake.LakeBoundary.Embedded := True;
        Assert(ALake.FConnections.Count = 1);
        break;
      end
      else if AnsiSameText(AConnection.claktype, 'EMBEDDEDV') then
      begin
        Include(LakeConnectionTypes, lctVertical);
        ALake.LakeBoundary.Embedded := True;
        Assert(ALake.FConnections.Count = 1);
        Break;
      end
      else if AnsiSameText(AConnection.claktype, 'VERTICAL') then
      begin
        Include(LakeConnectionTypes, lctVertical);
      end
      else if AnsiSameText(AConnection.claktype, 'HORIZONTAL') then
      begin
        Include(LakeConnectionTypes, lctHorizontal);
      end
      else
      begin
        Assert(False);
      end;
      if LakeConnectionTypes = [lctHorizontal, lctVertical] then
      begin
        break;
      end;
    end;
    ALake.LakeBoundary.LakeConnections := LakeConnectionTypes;

    NewTimeItem := ALake.LakeBoundary.Values.Add as TLakeTimeItem;
    NewTimeItem.StartTime := StartTime;
    NewTimeItem.EndTime := EndTime;
    NewTimeItem.Stage := '0';
    NewTimeItem.Rainfall := '0';
    NewTimeItem.Evaporation := '0';
    NewTimeItem.Runoff := '0';
    NewTimeItem.Inflow := '0';
    NewTimeItem.Withdrawal := '0';

    ALake.LakeBoundary.BedThickness := '1';
    ALake.LakeBoundary.StartingStage := FortranFloatToStr(ALake.FLakPackageItem.strt);



    UniformValues := True;
    FirstValue := ALake.FConnections[0].bedleak;
    for CellIndex := 1 to ALake.FConnections.Count - 1 do
    begin
      UniformValues  := FirstValue = ALake.FConnections[CellIndex].bedleak;
      if not UniformValues then
      begin
        break;
      end;
    end;

    if UniformValues then
    begin
      ALake.LakeBoundary.BedK := FortranFloatToStr(FirstValue);
    end
    else
    begin
      if ALake.DataSetsScreenObject = nil then
      begin
        CreateDataSetScreenObject;
      end;

      ImportedK := ALake.DataSetsScreenObject.ImportedValues.Add;
      ImportedK.Name := KImportedLakeK;
      ImportedK.Values.DataType := rdtDouble;

      for CellIndex := 0 to ALake.FConnections.Count - 1 do
      begin
        ImportedK.Values.Add(ALake.FConnections[CellIndex].bedleak);
      end;

      DataArrayName := Format('LakeK_%d', [ALake.FLakPackageItem.lakeno]);
      DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
        DataArrayName, '0', DataArrayName, [dcType], rdtDouble, eaBlocks, dso3d, '');

      DataSetIndex := ALake.DataSetsScreenObject.AddDataSet(DataArray);
      ALake.DataSetsScreenObject.DataSetFormulas[DataSetIndex] :=
        rsObjectImportedValuesR + '("' + KImportedLakeK + '")';

      ALake.LakeBoundary.BedK := DataArrayName;
    end;

    UniformValues := True;
    FirstValue := ALake.FConnections[0].belev;
    for CellIndex := 1 to ALake.FConnections.Count - 1 do
    begin
      UniformValues  := FirstValue = ALake.FConnections[CellIndex].belev;
      if not UniformValues then
      begin
        break;
      end;
    end;

    if UniformValues then
    begin
      ALake.LakeBoundary.BottomElevation := FortranFloatToStr(FirstValue);
    end
    else
    begin
      if ALake.DataSetsScreenObject = nil then
      begin
        CreateDataSetScreenObject;
      end;

      ImportedK := ALake.DataSetsScreenObject.ImportedValues.Add;
      ImportedK.Name := KImportedLakeBelev;
      ImportedK.Values.DataType := rdtDouble;

      for CellIndex := 0 to ALake.FConnections.Count - 1 do
      begin
        ImportedK.Values.Add(ALake.FConnections[CellIndex].belev);
      end;

      DataArrayName := Format('LakeBottomElev_%d', [ALake.FLakPackageItem.lakeno]);
      DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
        DataArrayName, '0', DataArrayName, [dcType], rdtDouble, eaBlocks, dso3d, '');

      DataSetIndex := ALake.DataSetsScreenObject.AddDataSet(DataArray);
      ALake.DataSetsScreenObject.DataSetFormulas[DataSetIndex] :=
        rsObjectImportedValuesR + '("' + KImportedLakeBelev + '")';

      ALake.LakeBoundary.BottomElevation := DataArrayName;
    end;

    UniformValues := True;
    FirstValue := ALake.FConnections[0].telev;
    for CellIndex := 1 to ALake.FConnections.Count - 1 do
    begin
      UniformValues  := FirstValue = ALake.FConnections[CellIndex].telev;
      if not UniformValues then
      begin
        break;
      end;
    end;

    if UniformValues then
    begin
      ALake.LakeBoundary.TopElevation := FortranFloatToStr(FirstValue);
    end
    else
    begin
      if ALake.DataSetsScreenObject = nil then
      begin
        CreateDataSetScreenObject;
      end;

      ImportedK := ALake.DataSetsScreenObject.ImportedValues.Add;
      ImportedK.Name := KImportedLakeTelev;
      ImportedK.Values.DataType := rdtDouble;

      for CellIndex := 0 to ALake.FConnections.Count - 1 do
      begin
        ImportedK.Values.Add(ALake.FConnections[CellIndex].telev);
      end;

      DataArrayName := Format('LakeTopElev_%d', [ALake.FLakPackageItem.lakeno]);
      DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
        DataArrayName, '0', DataArrayName, [dcType], rdtDouble, eaBlocks, dso3d, '');

      DataSetIndex := ALake.DataSetsScreenObject.AddDataSet(DataArray);
      ALake.DataSetsScreenObject.DataSetFormulas[DataSetIndex] :=
        rsObjectImportedValuesR + '("' + KImportedLakeTelev + '")';

      ALake.LakeBoundary.TopElevation := DataArrayName;
    end;


  end;
begin
  // For each lake, define 3D data sets for Lake K, bottom elevation, and top elevation.
  // Assign lake thickness a value of 1.
  // Assign bedleak to Lake K
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing LAK package');
  end;

  Mvr := GetMvr(MvrPackage, Package);

  Model := frmGoPhast.PhastModel;
  LakPackage := Model.ModflowPackages.LakMf6Package;
  LakPackage.IsSelected := True;

  Lak := Package.Package as TLak;
  Options := Lak.Options;
  LakPackage.PrintStage := Options.PRINT_STAGE;
  LakPackage.SaveStage := Options.STAGE;
  LakPackage.SaveBudget := Options.BUDGET;
  LakPackage.SaveBudgetCsv := Options.BUDGETCSV;
  if Options.SURFDEP.Used then
  begin
    LakPackage.SurfDepDepth := Options.SURFDEP.Value;
  end
  else
  begin
    LakPackage.SurfDepDepth := 0;
  end;
  LakPackage.WriteConvergenceData := Options.PACKAGE_CONVERGENCE;
  if Options.MAXIMUM_ITERATIONS.Used then
  begin
    LakPackage.MaxIterations := Options.MAXIMUM_ITERATIONS.Value;
  end;
  if Options.MAXIMUM_STAGE_CHANGE.Used then
  begin
    LakPackage.MaxStageChange := Options.MAXIMUM_STAGE_CHANGE.Value;
  end;

  Model.DataArrayManager.CreateInitialDataSets;

  Map := TimeSeriesMap.Create;
  Lakes := TImportLakes.Create;
  LakMvrLinkList := TLakMvrLinkList.Create;
  NumberObsDictionary := TNumberDictionary.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  ObsLists := TObsLists.Create;
  try

    if Mvr = nil then
    begin
      LakMvrLink.MvrPeriod := nil;
      for StressPeriodIndex := 0 to Lak.PeriodCount - 1 do
      begin
        LakMvrLink.LakPeriod := Lak.Periods[StressPeriodIndex];
        LakMvrLinkList.Add(LakMvrLink);
      end;
    end
    else
    begin
      SetLength(LakMvrLinkArray, Model.ModflowStressPeriods.Count);
      for StressPeriodIndex := 0 to Length(LakMvrLinkArray) - 1 do
      begin
        LakMvrLinkArray[StressPeriodIndex].MvrPeriod := nil;
        LakMvrLinkArray[StressPeriodIndex].LakPeriod := nil;
      end;
      for StressPeriodIndex := 0 to Mvr.PeriodCount - 1 do
      begin
        MvrPeriod := Mvr.Periods[StressPeriodIndex];
        LakMvrLinkArray[MvrPeriod.Period-1].MvrPeriod := MvrPeriod;
      end;
      for StressPeriodIndex := 0 to Lak.PeriodCount - 1 do
      begin
        LakPeriod := Lak.Periods[StressPeriodIndex];
        LakMvrLinkArray[LakPeriod.Period-1].LakPeriod := LakPeriod;
      end;

      for StressPeriodIndex := 1 to Length(LakMvrLinkArray) - 1 do
      begin
        if LakMvrLinkArray[StressPeriodIndex].MvrPeriod = nil then
        begin
          LakMvrLinkArray[StressPeriodIndex].MvrPeriod := LakMvrLinkArray[StressPeriodIndex-1].MvrPeriod;
          LakMvrLinkArray[StressPeriodIndex].LakPeriod := LakMvrLinkArray[StressPeriodIndex-1].LakPeriod;
        end;
      end;

      for StressPeriodIndex := 0 to Length(LakMvrLinkArray) - 1 do
      begin
        if (LakMvrLinkArray[StressPeriodIndex].MvrPeriod = nil)
          and (LakMvrLinkArray[StressPeriodIndex].LakPeriod = nil) then
        begin
          Continue;
        end;

        if StressPeriodIndex > 0 then
        begin
          if (LakMvrLinkArray[StressPeriodIndex].MvrPeriod = LakMvrLinkArray[StressPeriodIndex - 1].MvrPeriod)
            and (LakMvrLinkArray[StressPeriodIndex].LakPeriod = LakMvrLinkArray[StressPeriodIndex - 1].LakPeriod) then
          begin
            Continue
          end;
        end;

        LakMvrLinkList.Add(LakMvrLinkArray[StressPeriodIndex]);
      end;
    end;

    for TimeSeriesIndex := 0 to Lak.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := Lak.TimeSeries[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;

    if Lak.ObservationCount > 0 then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    end;
    for ObsPackageIndex := 0 to Lak.ObservationCount - 1 do
    begin
      ObsFiles := Lak.Observations[ObsPackageIndex].Package as TObs;
      GetObservations(NumberObsDictionary, BoundNameObsDictionary,
        nil, ObsLists, ObsFiles);
    end;

    for LakeIndex := 0 to Lak.PackageData.Count - 1 do
    begin
      LakPackageItem := Lak.PackageData[LakeIndex];
      while Lakes.Count < LakPackageItem.lakeno do
      begin
        Lakes.Add(nil)
      end;
      Assert(Lakes[LakPackageItem.lakeno - 1] = nil);
      Lakes[LakPackageItem.lakeno - 1] := TImportLake.Create(LakPackageItem);
    end;

    DummyConnectionItem.Initialize;
    for ConnectionIndex := 0 to Lak.Connections.Count - 1 do
    begin
      AConnection := Lak.Connections[ConnectionIndex];
      ALake := Lakes[AConnection.lakeno -1];
      while ALake.FConnections.Count < AConnection.iconn do
      begin
        ALake.FConnections.Add(DummyConnectionItem)
      end;
      Assert(ALake.FConnections[AConnection.iconn-1].lakeno = 0);
      ALake.FConnections[AConnection.iconn-1] := AConnection;
    end;

    for OutletIndex := 0 to Lak.LakOutlets.Count - 1 do
    begin
      AnOutlet := Lak.LakOutlets[OutletIndex];
      ALake := Lakes[AnOutlet.lakein-1];
      ALake.FOutlets.Add(AnOutlet);
    end;

    StartTime := Model.ModflowStressPeriods.First.StartTime;
    EndTime := Model.ModflowStressPeriods.Last.EndTime;

    for LakeIndex := 0 to Lakes.Count - 1 do
    begin
      CreateScreenObject(Lakes[LakeIndex]);
    end;
    for LakeIndex := 0 to Lakes.Count - 1 do
    begin
      CreateOutlets(Lakes[LakeIndex]);
    end;

    if Lak.ObservationCount > 0 then
    begin
      for LakeIndex := 0 to Lakes.Count - 1 do
      begin
        GetLakObservations(Lakes[LakeIndex]);
      end;
    end;

    for TableIndex := 0 to Lak.TableCount - 1 do
    begin
      ATable := Lak.Tables[TableIndex];
      ALake := Lakes[ATable.lakeno-1];
      TablePackage := Lak.GetTabFilePackage(ATable.tab6_filename);
      CreateLakeTable(ALake, TablePackage);
    end;

    for PeriodIndex := 0 to Lak.PeriodCount - 1 do
    begin
      APeriod := Lak.Periods[PeriodIndex];
      Period := APeriod.Period;
      StartTime := Model.ModflowStressPeriods[Period-1].StartTime;

      for SettingIndex := 0 to APeriod.Count - 1 do
      begin
        ASetting := APeriod[SettingIndex];
        SettingName := ASetting.Name;
        if AnsiSameText(SettingName, 'STATUS')
          or AnsiSameText(SettingName, 'STAGE')
          or AnsiSameText(SettingName, 'RAINFALL')
          or AnsiSameText(SettingName, 'EVAPORATION')
          or AnsiSameText(SettingName, 'RUNOFF')
          or AnsiSameText(SettingName, 'INFLOW')
          or AnsiSameText(SettingName, 'WITHDRAWAL')
          or AnsiSameText(SettingName, 'AUXILIARY')
          then
        begin
          LakeNo := ASetting.IdNumber;
          ALake := Lakes[LakeNo-1];
          ALake.FLakeSettings.Add(ASetting);
        end
        else if AnsiSameText(SettingName, 'RATE')
          or AnsiSameText(SettingName, 'INVERT')
          or AnsiSameText(SettingName, 'WIDTH')
          or AnsiSameText(SettingName, 'SLOPE')
          or AnsiSameText(SettingName, 'ROUGH')
          then
        begin
          OutletNumber := ASetting.IdNumber;
          AnOutlet := Lak.LakOutlets[OutletNumber-1];
          LakeNo := AnOutlet.lakein;
          ALake := Lakes[LakeNo-1];
          if ALake.FOutletSettings.TryGetValue(OutletNumber, OutletList) then
          begin
            OutletList.Add(ASetting);
            ALake.HasOutletSettings := True;
          end
          else
          begin
            Assert(False);
          end;
        end
        else
        begin
          Assert(False);
        end;
      end;
      for LakeIndex := 0 to Lakes.Count - 1 do
      begin
        ApplyLakeSettings(Lakes[LakeIndex])
      end;
    end;
  finally
    Lakes.Free;
    Map.Free;
    LakMvrLinkList.Free;
    NumberObsDictionary.Free;
    BoundNameObsDictionary.Free;
    ObsLists.Free;
  end;
end;

Type
  TMawMvrLink = record
    MawPeriod: TMawPeriod;
    MvrPeriod: TMvrPeriod;
    function Period: Integer;
  end;
  TMawMvrLinkArray = TArray<TMawMvrLink>;
  TMawMvrLinkList = TList<TMawMvrLink>;

procedure TModflow6Importer.ImportMaw(Package: TPackage;
  TransportModels: TModelList; MvrPackage: TPackage);
var
  Model: TPhastModel;
  Maw: TMaw;
  Options: TMawOptions;
  MawPackage: TMawPackage;
  Wells: array of TScreenObject;
  WellItems: array of TMawItem;
  WellIndex: Integer;
  PackageItem: TMawPackageItem;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  NewName: string;
  ConnectionIndex: Integer;
  CIndex: Integer;
  ConnectionItem: TMawConnectionItem;
  WellScreen: TMawWellScreenItem;
  PeriodIndex: Integer;
  LastTime: Double;
  APeriod: TMawPeriod;
  StartTime: Double;
  ObjectIndex: Integer;
  AnItem: TMawItem;
  SettingIndex: Integer;
  ASetting: TNumberedItem;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  BoundNameObsDictionary: TBoundNameDictionary;
  ObsLists: TObsLists;
  TimeSeriesName: string;
  ImportedTimeSeriesName: string;
  BoundName: string;
  ObsList: TObservationList;
  NumberObsDictionary: TNumberDictionary;
  CellIds: TCellIdList;
  CellId: TMfCellId;
  DisvGrid: TModflowDisvGrid;
  Grid: TModflowGrid;
  DisvCell: TModflowDisVCell;
  CellTop: Double;
  CellBottom: Double;
  Mvr: TMvr;
  MawMvrLinkArray: TMawMvrLinkArray;
  MawMvrLinkList: TMawMvrLinkList;
  StressPeriodIndex: Integer;
  MvrPeriod: TMvrPeriod;
  MawPeriod: TMawPeriod;
  MawMvrLink: TMawMvrLink;
  MvrSource: TMvrSource;
  MvrReceiver: TMvrReceiver;
  ObsNameIndex: Integer;
  procedure AssignObservations(ObsList: TObservationList; Mf6Obs: TModflow6Obs);
  var
    MawObs: TMawObs;
    ObsIndex: Integer;
    AnObs: TObservation;
  begin
    MawObs := Mf6Obs.MawObs;
    for ObsIndex := 0 to ObsList.Count - 1 do
    begin
      AnObs := ObsList[ObsIndex];
      if AnsiSameText(AnObs.ObsType, 'head') then
      begin
        Include(MawObs, moHead)
      end
      else if AnsiSameText(AnObs.ObsType, 'from-mvr') then
      begin
        Include(MawObs, moFromMvr)
      end
      else if AnsiSameText(AnObs.ObsType, 'maw') then
      begin
        if AnObs.IdType2 = itNumber then
        begin
          Include(MawObs, moFlowRateCells)
        end
        else
        begin
          Include(MawObs, moFlowRate)
        end;
      end
      else if AnsiSameText(AnObs.ObsType, 'rate') then
      begin
        Include(MawObs, moPumpRate)
      end
      else if AnsiSameText(AnObs.ObsType, 'rate-to-mvr') then
      begin
        Include(MawObs, moRateToMvr)
      end
      else if AnsiSameText(AnObs.ObsType, 'fw-rate') then
      begin
        Include(MawObs, moFlowingWellFlowRate)
      end
      else if AnsiSameText(AnObs.ObsType, 'fw-to-mvr') then
      begin
        Include(MawObs, moFlowWellToMvr)
      end
      else if AnsiSameText(AnObs.ObsType, 'storage') then
      begin
        Include(MawObs, moStorageFlowRate)
      end
      else if AnsiSameText(AnObs.ObsType, 'constant') then
      begin
        Include(MawObs, moConstantFlowRate)
      end
      else if AnsiSameText(AnObs.ObsType, 'conductance') then
      begin
        if AnObs.IdType2 = itNumber then
        begin
          Include(MawObs, moConductanceCells)
        end
        else
        begin
          Include(MawObs, moConductance)
        end;
      end
      else if AnsiSameText(AnObs.ObsType, 'fw-conductance') then
      begin
        Include(MawObs, moFlowingWellConductance)
      end
      else
      begin
        FErrorMessages.Add(Format('Unrecognized MAW observation type "%s".', [AnObs.ObsType]))
      end;
    end;
    Mf6Obs.MawObs := MawObs;
  end;
begin
  MvrSource.LakeOutlet := nil;
  ObsNameIndex := 0;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing MAW package');
  end;
  Mvr := GetMvr(MvrPackage, Package);


  Model := frmGoPhast.PhastModel;
  MawPackage := Model.ModflowPackages.MawPackage;
  MawPackage.IsSelected := True;
  Model.DataArrayManager.CreateInitialDataSets;

  Maw := Package.Package as TMaw;

  SetLength(MvrSource.IDs, 1);
  SetLength(MvrReceiver.IDs, 1);

  CellIds := TCellIdList.Create;
  Map := TimeSeriesMap.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  NumberObsDictionary := TNumberDictionary.Create;
  ObsLists := TObsLists.Create;
  MawMvrLinkList := TMawMvrLinkList.Create;
  try
    if Mvr = nil then
    begin
      MawMvrLink.MvrPeriod := nil;
      for StressPeriodIndex := 0 to Maw.PeriodCount - 1 do
      begin
        MawMvrLink.MawPeriod := Maw.Periods[StressPeriodIndex];
        MawMvrLinkList.Add(MawMvrLink);
      end;
    end
    else
    begin
      SetLength(MawMvrLinkArray, Model.ModflowStressPeriods.Count);
      for StressPeriodIndex := 0 to Length(MawMvrLinkArray) - 1 do
      begin
        MawMvrLinkArray[StressPeriodIndex].MvrPeriod := nil;
        MawMvrLinkArray[StressPeriodIndex].MawPeriod := nil;
      end;
      for StressPeriodIndex := 0 to Mvr.PeriodCount - 1 do
      begin
        MvrPeriod := Mvr.Periods[StressPeriodIndex];
        MawMvrLinkArray[MvrPeriod.Period-1].MvrPeriod := MvrPeriod;
      end;
      for StressPeriodIndex := 0 to Maw.PeriodCount - 1 do
      begin
        MawPeriod := Maw.Periods[StressPeriodIndex];
        MawMvrLinkArray[MawPeriod.Period-1].MawPeriod := MawPeriod;
      end;

      for StressPeriodIndex := 1 to Length(MawMvrLinkArray) - 1 do
      begin
        if MawMvrLinkArray[StressPeriodIndex].MvrPeriod = nil then
        begin
          MawMvrLinkArray[StressPeriodIndex].MvrPeriod := MawMvrLinkArray[StressPeriodIndex-1].MvrPeriod;
          MawMvrLinkArray[StressPeriodIndex].MawPeriod := MawMvrLinkArray[StressPeriodIndex-1].MawPeriod;
        end;
      end;

      for StressPeriodIndex := 0 to Length(MawMvrLinkArray) - 1 do
      begin
        if (MawMvrLinkArray[StressPeriodIndex].MvrPeriod = nil)
          and (MawMvrLinkArray[StressPeriodIndex].MawPeriod = nil) then
        begin
          Continue;
        end;

        if StressPeriodIndex > 0 then
        begin
          if (MawMvrLinkArray[StressPeriodIndex].MvrPeriod = MawMvrLinkArray[StressPeriodIndex - 1].MvrPeriod)
            and (MawMvrLinkArray[StressPeriodIndex].MawPeriod = MawMvrLinkArray[StressPeriodIndex - 1].MawPeriod) then
          begin
            Continue
          end;
        end;

        MawMvrLinkList.Add(MawMvrLinkArray[StressPeriodIndex]);
      end;
    end;

    for TimeSeriesIndex := 0 to Maw.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := Maw.TimeSeries[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;

    if Maw.ObservationCount > 0 then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    end;
    for ObsPackageIndex := 0 to Maw.ObservationCount - 1 do
    begin
      ObsFiles := Maw.Observations[ObsPackageIndex].Package as TObs;
      GetObservations(NumberObsDictionary, BoundNameObsDictionary,
        nil, ObsLists, ObsFiles);
    end;

    if Assigned(OnUpdateStatusBar) then
    begin
      OnUpdateStatusBar(self, 'importing MAW package');
    end;

    Options := Maw.Options;
    MawPackage.PrintHead := Options.PRINT_HEAD;
    MawPackage.SaveMawHeads := Options.Head;
    MawPackage.SaveMawFlows := Options.BUDGET;
    MawPackage.SaveBudgetCsv := Options.BUDGETCSV;
    MawPackage.IncludeWellStorage := not Options.NO_WELL_STORAGE;
    MawPackage.FlowCorrection := Options.FLOW_CORRECTION;
    if Options.SHUTDOWN_THETA.Used then
    begin
      MawPackage.ShutDownTheta := Options.SHUTDOWN_THETA.Value;
    end;
    if Options.SHUTDOWN_KAPPA.Used then
    begin
      MawPackage.ShutDownKappa := Options.SHUTDOWN_KAPPA.Value;
    end;
    MawPackage.FlowReduceCsv := Options.MAW_FLOW_REDUCE_CSV;

    SetLength(Wells, Maw.PackageData.Count+1);
    SetLength(WellItems, Maw.PackageData.Count+1);
    for WellIndex := 0 to Length(Wells) - 1 do
    begin
      Wells[WellIndex] := nil;
      WellItems[WellIndex] := nil;
    end;

    for WellIndex := 0 to Maw.PackageData.Count - 1 do
    begin
      PackageItem := Maw.PackageData[WellIndex];

      AScreenObject := TScreenObject.CreateWithViewDirection(
        Model, vdTop, UndoCreateScreenObject, False);
      NewName := ValidName(Format('Imported_%s_Maw_%d', [Package.PackageName, WellIndex + 1]));
      AScreenObject.Name := NewName;
      AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

      Model.AddScreenObject(AScreenObject);
      AScreenObject.ElevationCount := ecZero;
      AScreenObject.SetValuesOfIntersectedCells := True;
      AScreenObject.EvaluatedAt := eaBlocks;
      AScreenObject.Visible := False;


      Assert(PackageItem.wellno < Length(Wells));
      Wells[PackageItem.wellno] := AScreenObject;

      AScreenObject.CreateMawBoundary;
      AScreenObject.ModflowMawBoundary.Radius := FortranFloatToStr(PackageItem.radius);
      AScreenObject.ModflowMawBoundary.Bottom := FortranFloatToStr(PackageItem.bottom);
      AScreenObject.ModflowMawBoundary.InitialHead := FortranFloatToStr(PackageItem.strt);
      if PackageItem.condeqn = 'SPECIFIED' then
      begin
        AScreenObject.ModflowMawBoundary.ConductanceMethod := mcmSpecified
      end
      else if (PackageItem.condeqn = 'THIEM') or (PackageItem.condeqn = 'THEIM') then
      begin
        AScreenObject.ModflowMawBoundary.ConductanceMethod := mcmThiem
      end
      else if PackageItem.condeqn = 'SKIN' then
      begin
        AScreenObject.ModflowMawBoundary.ConductanceMethod := mcmSkin
      end
      else if PackageItem.condeqn = 'CUMULATIVE' then
      begin
        AScreenObject.ModflowMawBoundary.ConductanceMethod := mcmCumulative
      end
      else if PackageItem.condeqn = 'MEAN' then
      begin
        AScreenObject.ModflowMawBoundary.ConductanceMethod := mcmMean
      end
      else
      begin
        FErrorMessages.Add(Format('Unrecognized MAW conductance equation "%s".', [PackageItem.condeqn]));
      end;

      BoundName := UpperCase(PackageItem.Boundname);
      if BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
      begin
        Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
        AScreenObject.CreateMf6Obs;
        AScreenObject.Modflow6Obs.Name := BoundName;
        AssignObservations(ObsList, AScreenObject.Modflow6Obs);
      end;
      if NumberObsDictionary.TryGetValue(PackageItem.wellno, ObsList) then
      begin
        Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
        AScreenObject.CreateMf6Obs;
        if AScreenObject.Modflow6Obs.Name = '' then
        begin
          Inc(ObsNameIndex);
          AScreenObject.Modflow6Obs.Name := 'MAW_Obs_' + IntToStr(ObsNameIndex);
        end;
        AssignObservations(ObsList, AScreenObject.Modflow6Obs);
      end;

    end;

    LastTime := Model.ModflowStressPeriods.Last.EndTime;
    for PeriodIndex := 0 to MawMvrLinkList.Count - 1 do
    begin

      MawMvrLink := MawMvrLinkList[PeriodIndex];
      APeriod := MawMvrLink.MawPeriod;
      if APeriod = nil then
      begin
        Continue;
      end;

      StartTime := Model.ModflowStressPeriods[MawMvrLink.Period-1].StartTime;
      for ObjectIndex := 1 to Length(Wells) - 1 do
      begin
        AScreenObject := Wells[ObjectIndex];
        Assert(AScreenObject <> nil);
        Assert(AScreenObject.ModflowMawBoundary <> nil);
        if PeriodIndex > 0 then
        begin
          AnItem := WellItems[ObjectIndex];
          AnItem.EndTime := StartTime;
        end;
        AnItem := AScreenObject.ModflowMawBoundary.Values.Add as TMawItem;
        WellItems[ObjectIndex] := AnItem;
        AnItem.StartTime := StartTime;
        AnItem.EndTime := LastTime;

        if MawMvrLink.MvrPeriod <> nil then
        begin
          MvrSource.ScreenObject := AScreenObject;
          MvrSource.PackageName := Package.PackageName;
          MvrSource.Period := MawMvrLink.Period;
          SetLength(MvrSource.IDs, 1);
          MvrSource.IDs[0] := ObjectIndex;
          MvrSource.SourceType := mspcMaw;
          FMvrSources.Add(MvrSource);

          MvrReceiver.ScreenObject := AScreenObject;
          MvrReceiver.PackageName := Package.PackageName;
          MvrReceiver.Period := MawMvrLink.Period;
          SetLength(MvrReceiver.IDs, 1);
          MvrReceiver.IDs[0] := ObjectIndex;
          MvrReceiver.ReceiverType := mrpcMaw;
          FMvrReceivers.Add(MvrReceiver)
        end;
      end;
      for SettingIndex := 0 to APeriod.Count - 1 do
      begin
        ASetting := APeriod[SettingIndex];
        AnItem := WellItems[ASetting.IdNumber];
        if AnsiSameText(ASetting.Name, 'STATUS') then
        begin
          if AnsiSameText(ASetting.StringValue, 'ACTIVE') then
          begin
            AnItem.MawStatus := mwActive;
          end
          else if AnsiSameText(ASetting.StringValue, 'INACTIVE') then
          begin
            AnItem.MawStatus := mwInactive;
          end
          else if AnsiSameText(ASetting.StringValue, 'CONSTANT') then
          begin
            AnItem.MawStatus := mwConstantHead;
          end
          else
          begin
            Assert(False)
          end;
        end
        else if AnsiSameText(ASetting.Name, 'FLOWING_WELL') then
        begin
          AnItem.FlowingWell := fwFlowing;
          Assert(Length(ASetting.FloatValues) = 3);
          AnItem.FlowingWellElevation := FortranFloatToStr(ASetting.FloatValues[0]);
          AnItem.FlowingWellConductance := FortranFloatToStr(ASetting.FloatValues[1]);
          AnItem.FlowingWellReductionLength := FortranFloatToStr(ASetting.FloatValues[2]);
        end
        else if AnsiSameText(ASetting.Name, 'RATE') then
        begin
          TimeSeriesName := UpperCase(ASetting.StringValue);
          if (TimeSeriesName <> '') and Map.TryGetValue(TimeSeriesName, ImportedTimeSeriesName) then
          begin
            AnItem.Rate := ImportedTimeSeriesName;
          end
          else
          begin
            AnItem.Rate := FortranFloatToStr(ASetting.FloatValue);
          end;
        end
        else if AnsiSameText(ASetting.Name, 'WELL_HEAD') then
        begin
          TimeSeriesName := UpperCase(ASetting.StringValue);
          if (TimeSeriesName <> '') and Map.TryGetValue(TimeSeriesName, ImportedTimeSeriesName) then
          begin
            AnItem.WellHead := ImportedTimeSeriesName;
          end
          else
          begin
            AnItem.WellHead := FortranFloatToStr(ASetting.FloatValue);
          end;
        end
        else if AnsiSameText(ASetting.Name, 'HEAD_LIMIT') then
        begin
          AnItem.HeadLimitChoice := True;
          AnItem.HeadLimit := FortranFloatToStr(ASetting.FloatValue);
        end
        else if AnsiSameText(ASetting.Name, 'SHUT_OFF') then
        begin
          AnItem.RateLimitation := rlShutoff;
          Assert(Length(ASetting.FloatValues) = 2);
          AnItem.MinRate := FortranFloatToStr(ASetting.FloatValues[0]);
          AnItem.MaxRate := FortranFloatToStr(ASetting.FloatValues[1]);
        end
        else if AnsiSameText(ASetting.Name, 'RATE_SCALING') then
        begin
          AnItem.RateLimitation := rlScaling;
          Assert(Length(ASetting.FloatValues) = 2);
          AnItem.PumpElevation := FortranFloatToStr(ASetting.FloatValues[0]);
          AnItem.ScalingLength := FortranFloatToStr(ASetting.FloatValues[1]);
        end
        else if AnsiSameText(ASetting.Name, 'AUXILIARY') then
        begin
          // ignore
        end
        else
        begin
          FErrorMessages.Add(Format('Unrecognized mawsetting "%s".', [ASetting.Name]))
        end;
      end;
    end;

    if Model.DisvUsed then
    begin
      DisvGrid := Model.DisvGrid;
    end
    else
    begin
      Grid := Model.ModflowGrid;
    end;

    ConnectionIndex := 0;
    for WellIndex := 1 to Length(Wells) - 1 do
    begin
      PackageItem := Maw.PackageData[WellIndex-1];
      AScreenObject := Wells[WellIndex];
      AScreenObject.ModflowMawBoundary.WellScreens.Capacity := PackageItem.ngwfnodes;
      for CIndex := 0 to PackageItem.ngwfnodes - 1 do
      begin
        ConnectionItem := Maw.Connections[ConnectionIndex];
        Inc(ConnectionIndex);
        Assert(ConnectionItem.wellno = PackageItem.wellno);
        WellScreen := AScreenObject.ModflowMawBoundary.WellScreens.Add as TMawWellScreenItem;
//        WellScreen.ScreenTop := FortranFloatToStr(ConnectionItem.scrn_top);
//        WellScreen.ScreenBottom := FortranFloatToStr(ConnectionItem.scrn_bot);
        WellScreen.SkinK := FortranFloatToStr(ConnectionItem.hk_skin);
        WellScreen.SkinRadius := FortranFloatToStr(ConnectionItem.radius_skin);

        if AScreenObject.ModflowMawBoundary.ConductanceMethod in
          [mcmSpecified, mcmThiem, mcmSkin, mcmCumulative, mcmTheim] then
        begin
          CellId := ConnectionItem.CellID;
          if Model.DisvUsed then
          begin
            DisvCell := DisvGrid.Cells[CellId.Layer-1, CellId.Column-1];
            CellTop := DisvCell.Top;
            CellBottom := DisvCell.Bottom;
          end
          else
          begin
            CellTop := Grid.CellElevation[CellId.Column-1, CellId.Row-1, CellId.Layer-1];
            CellBottom := Grid.CellElevation[CellId.Column-1, CellId.Row-1, CellId.Layer];
          end;
          if ConnectionItem.scrn_top < CellTop then
          begin
            CellTop := ConnectionItem.scrn_top;
          end;
          if ConnectionItem.scrn_bot > CellBottom then
          begin
            CellBottom := ConnectionItem.scrn_bot
          end;
          WellScreen.ScreenTop := FortranFloatToStr(CellTop);
          WellScreen.ScreenBottom := FortranFloatToStr(CellBottom);
        end
        else
        begin
          WellScreen.ScreenTop := FortranFloatToStr(ConnectionItem.scrn_top);
          WellScreen.ScreenBottom := FortranFloatToStr(ConnectionItem.scrn_bot);
        end;

        if CIndex = 0 then
        begin
          CellIds.Clear;
          CellIds.Add(ConnectionItem.CellID);
          AddPointsToScreenObject(CellIds, AScreenObject, False);
        end;
      end;
    end;
  finally
    Map.Free;
    BoundNameObsDictionary.Free;
    ObsLists.Free;
    NumberObsDictionary.Free;
    CellIds.Free;
    MawMvrLinkList.Free;
  end;

end;

procedure TModflow6Importer.ImportModflow6Model(NameFiles, ErrorMessages: TStringList);
var
  FileIndex: Integer;
  OutFile: string;
  ListFile: TStringList;
  PhastModel: TPhastModel;
  ModelIndex: Integer;
  AModel: TModel;
  FlowModelNames: TStringList;
  FlowModelImported: Boolean;
  frmSelectFlowModel: TfrmSelectFlowModel;
  ExchangeIndex: Integer;
  Exchange: TExchange;
begin
  frmErrorsAndWarnings.Clear;
  FErrorMessages := ErrorMessages;
  for FileIndex := 0 to NameFiles.Count - 1 do
  begin
    if not TFile.Exists(NameFiles[FileIndex]) then
    begin
      Beep;
      MessageDlg(Format(StrTheNameFileSDoe, [NameFiles[FileIndex]]), mtError, [mbOK], 0);
      Exit;
    end;
  end;
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.Clear;
  frmGoPhast.Caption := StrModelName;
  frmGoPhast.sdSaveDialog.FileName := '';
  PhastModel.ModelSelection := msModflow2015;

  FlowModelImported := False;
  for FileIndex := 0 to NameFiles.Count - 1 do
  begin
    FSimulation := TMf6Simulation.Create('Simulation');
    try
      FSimulation.OnUpdataStatusBar := OnUpdateStatusBar;
      FSimulations.Add(FSimulation);
      FSimulation.ReadSimulation(NameFiles[FileIndex]);
//      ErrorMessages.
//      FSimulation.OutFile.BaseStream
    finally
      FSimulation := nil;
    end;
  end;

  for FileIndex := 0 to FSimulations.Count - 1 do
  begin
    FSimulation := FSimulations[FileIndex];
    try
      for ExchangeIndex := 0 to FSimulation.Exchanges.Count - 1 do
      begin
        Exchange := FSimulation.Exchanges[ExchangeIndex];
        if not AnsiSameText(Exchange.ExchangeType, 'GWF6-GWT6') then
        begin
          ErrorMessages.Add('The following error was encountered when reading '
            + NameFiles[FileIndex]);
          ErrorMessages.Add('ModelMuse does not currently support MODFLOW 6 exchanges');
          break;
        end;
      end;

      FlowModelNames := TStringList.Create;
      try
        for ModelIndex := 0 to FSimulation.Models.Count - 1 do
        begin
          AModel := FSimulation.Models[ModelIndex];
          if AModel.ModelType = 'GWF6' then
          begin
            FlowModelNames.Add(AModel.NameFile)
          end;
        end;
        if FlowModelImported and (FlowModelNames.Count > 0) then
        begin
          ErrorMessages.Add('The following error was encountered when reading '
            + NameFiles[FileIndex]);
          ErrorMessages.Add('Another flow model name file was already in another simulation name file');
          ErrorMessages.Add('ModelMuse can only import a single flow model.');
          Continue;
        end;
        FModelNameFile := '';
        if FlowModelNames.Count > 1 then
        begin
          frmSelectFlowModel := TfrmSelectFlowModel.Create(nil);
          try
            frmSelectFlowModel.rgFlowModels.Items := FlowModelNames;
            frmSelectFlowModel.rgFlowModels.ItemIndex := 0;
            if frmSelectFlowModel.ShowModal = mrOK then
            begin
              FModelNameFile := frmSelectFlowModel.rgFlowModels.Items[frmSelectFlowModel.rgFlowModels.ItemIndex];
            end
            else
            begin
              Exit;
            end;
          finally
            frmSelectFlowModel.Free
          end;
        end
        else
        begin
          if FlowModelNames.Count > 0 then
          begin
            FModelNameFile := FlowModelNames[0];
          end
          else
          begin
            FModelNameFile := '';
          end;
        end;
        if FModelNameFile <> '' then
        begin
          FFlowModel := FSimulation.Models.GetModelByNameFile(FModelNameFile);
        end
        else
        begin
          FFlowModel := nil;
        end;
        ImportSimulationOptions;
        ImportFlowModelTiming;
        ImportSolutionGroups;
        if not ImportFlowModel then
        begin
          Exit;
        end;

        ImportIMS;



      finally
        FlowModelNames.Free
      end;

    finally
      FSimulation := nil;
    end;
  end;
  for FileIndex := 0 to FSimulations.Count - 1 do
  begin
    FSimulation := FSimulations[FileIndex];
    FSimulation.OutFile.close
  end;


  for FileIndex := 0 to NameFiles.Count - 1 do
  begin
    OutFile := ChangeFileExt(NameFiles[FileIndex], '.lst');
    if TFile.Exists(OutFile) then
    begin
      ListFile := TStringList.Create;
      try
        ListFile.LoadFromFile(OutFile);
        if ListFile.Count > 0 then
        begin
          ErrorMessages.Add('The following errors were encountered when reading '
            + NameFiles[FileIndex]);
          ErrorMessages.AddStrings(ListFile);
          ErrorMessages.Add('');
        end;
      finally
        ListFile.Free;
      end;
    end
    else
    begin
      ErrorMessages.Add(OutFile + ' does not exist.')
    end;
  end;

  PhastModel.Exaggeration := frmGoPhast.DefaultVE;
  frmGoPhast.RestoreDefault2DView1Click(nil);

  if ErrorMessages.Count > 0 then
  begin
    frmImportWarnings := TfrmImportWarnings.Create(nil);
    try
      frmImportWarnings.memoWarnings.Lines := ErrorMessages;
      Beep;
      frmImportWarnings.ShowModal;
    finally
      frmImportWarnings.Free;
    end;
  end;
end;

procedure TModflow6Importer.ImportMvr(Package: TPackage);
var
  SourceDictionary: TMvrSourceDictionary;
  ReceiverDictionary: TMvrReceiverDictionary;
  Index: Integer;
  ASource: TMvrSource;
  KeyIndex: Integer;
  AReceiver: TMvrReceiver;
  Mvr: TMvr;
  PeriodIndex: Integer;
  MvrPeriod: TMvrPeriod;
  SourceKey: TMvrKey;
  ReceiverKey: TMvrKey;
  MvrIndex: Integer;
  MvrPeriodItem: TMvrPeriodItem;
  Source: TMvrSource;
  Receiver: TMvrReceiver;
  EndTime: double;
  Model: TPhastModel;
  AScreenObject: TScreenObject;
  MvrItem: TMvrItem;
  StartTime: Double;
  IndividualMvrItem: TIndividualMvrItem;
  ModflowMvr: TMvrBoundary;
  ReceiverIndex: Integer;
  ReceiverItem: TReceiverItem;
  AReceiverItem: TReceiverItem;
  TimeIndex: Integer;
  PriorMvrItem: TMvrItem;
  MvrType: TMvrType;
  RIndex: Integer;
  LakeBoundary: TLakeMf6;
  AnOutlet: TLakeOutletItem;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing MVR package');
  end;
  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.MvrPackage.IsSelected := True;
  EndTime := Model.ModflowStressPeriods.Last.EndTime;
  SourceDictionary := TMvrSourceDictionary.Create(TTMvrKeyyComparer.Create);
  ReceiverDictionary := TMvrReceiverDictionary.Create(TTMvrKeyyComparer.Create);
  try
    for Index := 0 to FMvrSources.Count - 1 do
    begin
      ASource := FMvrSources[Index];
      Assert(Length(ASource.IDs) > 0);
      for KeyIndex := 0 to Length(ASource.IDs) - 1 do
      begin
        SourceDictionary.Add(ASource.Key(KeyIndex), ASource);
      end;
    end;
    for Index := 0 to FMvrReceivers.Count - 1 do
    begin
      AReceiver := FMvrReceivers[Index];
      Assert(Length(AReceiver.IDs) > 0);
      for KeyIndex := 0 to Length(AReceiver.IDs) - 1 do
      begin
        ReceiverDictionary.Add(AReceiver.Key(KeyIndex), AReceiver);
      end;
    end;
    Mvr := Package.Package as TMvr;
    for PeriodIndex := 0 to Mvr.PeriodCount - 1 do
    begin
      MvrPeriod := Mvr.Periods[PeriodIndex];
      StartTime := Model.ModflowStressPeriods[MvrPeriod.Period-1].StartTime;
      SourceKey.Period := MvrPeriod.Period;
      ReceiverKey.Period := MvrPeriod.Period;
      for MvrIndex := 0 to MvrPeriod.Count - 1 do
      begin
        MvrPeriodItem := MvrPeriod[MvrIndex];
        SourceKey.ID := MvrPeriodItem.id1;
        SourceKey.PackageName := MvrPeriodItem.pname1;

        ReceiverKey.ID := MvrPeriodItem.id2;
        ReceiverKey.PackageName := MvrPeriodItem.pname2;

        if not SourceDictionary.TryGetValue(SourceKey, Source) then
        begin
          Assert(False);
        end;
        if not ReceiverDictionary.TryGetValue(ReceiverKey, Receiver) then
        begin
          Assert(False);
        end;

        AScreenObject := Source.ScreenObject;
        AScreenObject. CreateModflowMvr;
        ModflowMvr := AScreenObject.ModflowMvr;
        case Source.SourceType of
          mspcWel:
            begin
              ModflowMvr.SourcePackageChoice := spcWel;
            end;
          mspcDrn:
            begin
              ModflowMvr.SourcePackageChoice := spcDrn;
            end;
          mspcRiv:
            begin
              ModflowMvr.SourcePackageChoice := spcRiv;
            end;
          mspcGhb:
            begin
              ModflowMvr.SourcePackageChoice := spcGhb;
            end;
          mspcLak:
            begin
              ModflowMvr.SourcePackageChoice := spcLak;
            end;
          mspcMaw:
            begin
              ModflowMvr.SourcePackageChoice := spcMaw;
            end;
          mspcSfr:
            begin
              ModflowMvr.SourcePackageChoice := spcSfr;
            end;
          mspcUzf:
            begin
              ModflowMvr.SourcePackageChoice := spcUzf;
            end;
          else
            begin
              Assert(false);
            end
        end;
        ReceiverItem := nil;
        for ReceiverIndex := 0 to ModflowMvr.Receivers.Count - 1 do
        begin
          AReceiverItem := ModflowMvr.Receivers[ReceiverIndex];
          if (AReceiverItem.ReceiverObject = Receiver.ScreenObject) then
          begin
            if AReceiver.ReceiverType = mrpcLak then
            begin
              LakeBoundary := Receiver.ScreenObject.ModflowLak6;
              for RIndex := 0 to LakeBoundary.Outlets.Count - 1 do
              begin
                AnOutlet := LakeBoundary.Outlets[RIndex];
                if AnOutlet = Source.LakeOutlet then
                begin
                  ReceiverItem := AReceiverItem;
                  break;
                end;
              end;
            end
            else
            begin
              ReceiverItem := AReceiverItem;
              break;
            end;
          end;
        end;

        if AnsiSameText(MvrPeriodItem.mvrtype, 'FACTOR') then
        begin
          MvrType := mtFactor;
        end
        else if AnsiSameText(MvrPeriodItem.mvrtype, 'EXCESS') then
        begin
          MvrType := mtExcess;
        end
        else if AnsiSameText(MvrPeriodItem.mvrtype, 'THRESHOLD') then
        begin
          MvrType := mtThreshold;
        end
        else if AnsiSameText(MvrPeriodItem.mvrtype, 'UPTO') then
        begin
           MvrType := mtUpTo;
        end
        else
        begin
          MvrType := mtFactor;
          Assert(False);
        end;

        if ReceiverItem = nil then
        begin
          ReceiverItem := ModflowMvr.Receivers.Add;
          case Receiver.ReceiverType of
            mrpcLak:
              begin
                ReceiverItem.ReceiverPackage := rpcLak;
              end;
            mrpcMaw:
              begin
                ReceiverItem.ReceiverPackage := rpcMaw;
              end;
            mrpcSfr:
              begin
                ReceiverItem.ReceiverPackage := rpcSfr;
                ReceiverItem.SfrReceiverChoice := srcFirst
              end;
            mrpcUzf:
              begin
                ReceiverItem.ReceiverPackage := rpcUzf;
              end;
            else
              begin
              end;
          end;
          ReceiverItem.ReceiverObject := Receiver.ScreenObject;
          ReceiverItem.DivisionChoice := dcDoNotDivide;
          for TimeIndex := 0 to ModflowMvr.Values.Count - 1 do
          begin
            PriorMvrItem := ModflowMvr.Values[TimeIndex] as TMvrItem;
            IndividualMvrItem := PriorMvrItem.Items.Add;
            IndividualMvrItem.MvrType := MvrType;
            IndividualMvrItem.Value := '0';
          end;
        end;
        if Source.LakeOutlet <> nil then
        begin
          ReceiverItem.LakeOutlet := Source.LakeOutlet.Index + 1;
        end;

        if ModflowMvr.Values.Count > 0 then
        begin
          PriorMvrItem := ModflowMvr.Values.Last as TMvrItem;
          if (PriorMvrItem.StartTime = StartTime)
            and (PriorMvrItem.EndTime = EndTime) then
          begin
            MvrItem := PriorMvrItem;
          end
          else
          begin
            PriorMvrItem.EndTime := StartTime;
            MvrItem := ModflowMvr.Values.Add as TMvrItem;
            MvrItem.Assign(MvrItem);
          end;
        end
        else
        begin
          MvrItem := ModflowMvr.Values.Add as TMvrItem;
          for ReceiverIndex := 0 to ModflowMvr.Receivers.Count do
          begin
            IndividualMvrItem := MvrItem.Items.Add;
            IndividualMvrItem.MvrType := MvrType;
            IndividualMvrItem.Value := '0';
          end;
        end;
        MvrItem.StartTime := StartTime;
        MvrItem.EndTime := EndTime;

        if ReceiverItem.Index >= MvrItem.Items.Count then
        begin
          MvrItem.Items.Add;
        end;
        IndividualMvrItem := MvrItem.Items[ReceiverItem.Index];
        IndividualMvrItem.MvrType := MvrType;
        IndividualMvrItem.Value := FortranFloatToStr(MvrPeriodItem.value);


//        IndividualMvrItem := MvrItem.Items.Add;

        {
    property mname1: string read Fmname1;
    property pname1: string read Fpname1;
    property id1: Integer read Fid1;
    property mname2: string read Fmname2;
    property pname2: string read Fpname2;
    property id2: Integer read Fid2;
    property mvrtype: string read Fmvrtype;
    property value: Extended read Fvalue;
    function SourceMatch(PackageName: string; ID: Integer): Boolean;
    function ReceiverMatch(PackageName: string; ID: Integer): Boolean;
        }
      end;
    end;
  finally
    ReceiverDictionary.Free;
    SourceDictionary.Free;
  end;
end;

function TModflow6Importer.GetMvr(MvrPackage, Package: TPackage): TMvr;
var
  FoundMvr: Boolean;
  Index: Integer;
begin
  if MvrPackage = nil then
  begin
    result := nil;
  end
  else
  begin
    result := MvrPackage.Package as TMvr;
    FoundMvr := False;
    for Index := 0 to result.Packages.Count - 1 do
    begin
      FoundMvr := AnsiSameText(Package.PackageName, result.Packages[Index].pname);
      if FoundMvr then
      begin
        Break;
      end;
    end;
    if not FoundMvr then
    begin
      result := nil;
    end;
  end;
end;

procedure TModflow6Importer.AddPointsToScreenObject(CellIds: TCellIdList;
  AScreenObject: TScreenObject; ThreeD: Boolean = True);
var
  ElementCenter: TDualLocation;
  APoint: TPoint2D;
  CellIndex: Integer;
  Model: TPhastModel;
  CellId: TMfCellId;
  DisvUsed: Boolean;
  Limits: TGridLimit;
begin
  Model := frmGoPhast.PhastModel;
  DisvUsed := Model.DisvUsed;
  for CellIndex := 0 to CellIds.Count - 1 do
  begin
    CellId := CellIds[CellIndex];
    if (CellId.Layer = 0) and  (CellId.Row = 0) and (CellId.column = 0) then
    begin
      if not FMinPointAssigned then
      begin
        Limits := Model.DiscretizationLimits(vdTop);
        APoint.x := Limits.MinX -1;
        APoint.y := Limits.MaxY;
        FMinPoint := APoint;
        FMinPointAssigned := True;
      end
      else
      begin
        APoint := FMinPoint;
      end;
      AScreenObject.AddPoint(APoint, True);
      if ThreeD then
      begin
        if CellIds.Count > 1 then
        begin
          AScreenObject.ImportedSectionElevations.Add(0.0);
        end
        else
        begin
          AScreenObject.ElevationFormula := FortranFloatToStr(0.0);
        end;
      end;
    end
    else
    begin
      if DisvUsed then
      begin
        CellId.Row := 1;
      end;
      ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
      APoint.x := ElementCenter.RotatedLocation.x;
      APoint.y := ElementCenter.RotatedLocation.y;
      AScreenObject.AddPoint(APoint, True);
      if ThreeD then
      begin
        if CellIds.Count > 1 then
        begin
          AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);
        end
        else
        begin
          AScreenObject.ElevationFormula := FortranFloatToStr(ElementCenter.RotatedLocation.z);
        end;
      end;
    end;
  end;
end;

procedure TModflow6Importer.GetObservations(NumberObsDictionary: TNumberDictionary;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary; ObsLists: TObsLists; ObsFiles: TObs);
var
  ObsFileIndex: Integer;
  ObsFile: TObsFile;
  ObsIndex: Integer;
  Observation: TObservation;
  ObsList: TObservationList;
begin
  for ObsFileIndex := 0 to ObsFiles.FileCount - 1 do
  begin
    ObsFile := ObsFiles[ObsFileIndex];
    for ObsIndex := 0 to ObsFile.Count - 1 do
    begin
      Observation := ObsFile[ObsIndex];
      case Observation.IdType1 of
        itCell:
          begin
            Assert(CellIdObsDictionary <> nil);
            if not CellIdObsDictionary.TryGetValue(Observation.CellId1, ObsList) then
            begin
              ObsList := TObservationList.Create;
              ObsLists.Add(ObsList);
              CellIdObsDictionary.Add(Observation.CellId1, ObsList);
            end;
            ObsList.Add(Observation);
          end;
        itNumber:
          begin
            Assert(NumberObsDictionary <> nil);
            if not NumberObsDictionary.TryGetValue(Observation.Num1, ObsList) then
            begin
              ObsList := TObservationList.Create;
              ObsLists.Add(ObsList);
              NumberObsDictionary.Add(Observation.Num1, ObsList);
            end;
            ObsList.Add(Observation);
          end;
        itFloat:
          begin
            Assert(False);
          end;
        itName:
          begin
            Assert(BoundNameObsDictionary <> nil);
            if not BoundNameObsDictionary.TryGetValue(UpperCase(Observation.Name1), ObsList) then
            begin
              ObsList := TObservationList.Create;
              ObsLists.Add(ObsList);
              BoundNameObsDictionary.Add(UpperCase(Observation.Name1), ObsList);
            end;
            ObsList.Add(Observation);
          end;
        itAbsent:
          begin
            Assert(False);
          end;
      end;
    end;
  end;
end;

procedure TModflow6Importer.ImportNpf(Package: TPackage);
var
  Npf: TNpf;
  Model: TPhastModel;
  NpfPackage: TNpfPackage;
  Options: TNpfOptions;
  GridData: TNpfGridData;
  DataArray: TDataArray;
  TvkIndex: Integer;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing NPF package');
  end;
  Model := frmGoPhast.PhastModel;
  Npf := Package.Package as TNpf;

  NpfPackage := Model.ModflowPackages.NpfPackage;
  Options := Npf.Options;
  if Options.ALTERNATIVE_CELL_AVERAGING <> '' then
  begin
    if Options.ALTERNATIVE_CELL_AVERAGING = 'LOGARITHMIC' then
    begin
      NpfPackage.CellAveraging := caLogarithmic;
    end
    else if Options.ALTERNATIVE_CELL_AVERAGING = 'AMT-LMK' then
    begin
      NpfPackage.CellAveraging := caArithLog;
    end
    else if Options.ALTERNATIVE_CELL_AVERAGING = 'AMT-HMK' then
    begin
      NpfPackage.CellAveraging := caArithHarm;
    end
    else
    begin
      FErrorMessages.Add(Format('Unrecognized ALTERNATIVE_CELL_AVERAGING option %s in NPF package',
        [Options.ALTERNATIVE_CELL_AVERAGING]))
    end;
  end;
  NpfPackage.UseSaturatedThickness := Options.THICKSTRT;
  NpfPackage.TimeVaryingVerticalConductance := Options.VARIABLECV;
  NpfPackage.Dewatered := Options.DEWATERED;
  NpfPackage.Perched := Options.PERCHED;
  Model.ModflowWettingOptions.WettingActive := Options.REWET.Used;
  if Options.REWET.Used then
  begin
    Model.ModflowWettingOptions.WettingFactor := Options.REWET.WETFCT;
    Model.ModflowWettingOptions.WettingIterations := Options.REWET.IWETIT;
    Model.ModflowWettingOptions.WettingEquation := Options.REWET.IHDWET;
  end;
  NpfPackage.UseXT3D := Options.XT3D;
  NpfPackage.Xt3dOnRightHandSide := Options.RHS;
  NpfPackage.SaveSpecificDischarge := Options.SAVE_SPECIFIC_DISCHARGE;
  NpfPackage.SaveSaturation := Options.SAVE_SATURATION;
  NpfPackage.UseHorizontalAnisotropy := Options.K22OVERK;
  NpfPackage.UseVerticalAnisotropy := Options.K33OVERK;

  GridData := Npf.GridData;
  Assign3DIntegerDataSet(KCellType, GridData.ICELLTYPE);

  Assign3DRealDataSet(rsKx, GridData.K);

  if GridData.K22 <> nil then
  begin
    if NpfPackage.UseHorizontalAnisotropy then
    begin
      Assign3DRealDataSet(KKyOverKx, GridData.K22);
    end
    else
    begin
      Assign3DRealDataSet(rsKy, GridData.K22);
    end;
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKy);
    DataArray.Formula := rsKx;
  end;

  if GridData.K33 <> nil then
  begin
    if NpfPackage.UseVerticalAnisotropy then
    begin
      Assign3DRealDataSet(KKzOverKx, GridData.K33);
    end
    else
    begin
      Assign3DRealDataSet(rsKz, GridData.K33);
    end;
  end
  else
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(rsKz);
    DataArray.Formula := rsKx;
  end;

  if GridData.ANGLE1 <> nil then
  begin
    Assign3DRealDataSet(KXT3DAngle1, GridData.ANGLE1);
  end;

  if GridData.ANGLE2 <> nil then
  begin
    Assign3DRealDataSet(KXT3DAngle2, GridData.ANGLE2);
  end;

  if GridData.ANGLE3 <> nil then
  begin
    Assign3DRealDataSet(KXT3DAngle3, GridData.ANGLE3);
  end;

  if GridData.WETDRY <> nil then
  begin
    Assign3DRealDataSet(rsWetDry, GridData.WETDRY);
  end;

  if Npf.Count > 0 then
  begin
    Model.ModflowPackages.TvkPackage.IsSelected := True;
    for TvkIndex := 0 to Npf.Count - 1 do
    begin
      ImportTvk(Npf[TvkIndex])
    end;
  end;
end;

procedure TModflow6Importer.ImportOc(Package: TPackage);
var
  OC: TOc;
  Model: TPhastModel;
  OutputControl: TModflowOutputControl;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing OC package');
  end;
  OC := Package.Package as TOc;
  Model := frmGoPhast.PhastModel;
  OutputControl := Model.ModflowOutputControl;

  if OC.Options.BudgetFile then
  begin
    OutputControl.SaveCellFlows := csfBinary
  end;
  if OC.Options.BudgetCsvFile then
  begin
    OutputControl.SaveBudgetCSV := True
  end;
  if OC.Options.HeadFile then
  begin
    OutputControl.HeadOC.SaveInExternalFile := True
  end;
  if OC.Options.ConcentrationFile then
  begin
    OutputControl.ConcentrationOC.SaveInExternalFile := True
  end;
end;

type
  TRchConnection = class(TObject)
    ScreenObject: TScreenObject;
    List: TRchTimeItemList;
    IFACE: Integer;
    destructor Destroy; override;
  end;

  TRchConnectionObjectList = TObjectList<TRchConnection>;
  TRchConnectionObjectLists = TObjectList<TRchConnectionObjectList>;

procedure TModflow6Importer.ImportRch(Package: TPackage;
  TransportModels: TModelList);
var
  Model: TPhastModel;
  Rch: TRch;
  AModel: TModel;
  APackage: TPackage;
  ModelIndex: Integer;
  TransportModel: TTransportNameFile;
  PackageIndex: Integer;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary;
  Ssm: TSsm;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ObsLists: TObsLists;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  SourceIndex: Integer;
  FoundMatch: Boolean;
  TransportAuxNames: TStringList;
  PeriodIndex: Integer;
  APeriod: TRchPeriod;
  CellIndex: Integer;
  ACell: TRchTimeItem;
  IfaceIndex: Integer;
  KeyStringDictionary: TDictionary<string, TRchTimeItemList>;
  CellLists: TObjectList<TRchTimeItemList>;
  OtherCellLists: TObjectList<TRchTimeItemList>;
  ACellList: TRchTimeItemList;
  Options: TRchOptions;
  IFace: Integer;
  LastTime: Double;
  Imported_Recharge: TValueArrayItem;
  ItemList: TList<TRchItem>;
  StartTime: Double;
  RchIndex: Integer;
  AnItem: TRchItem;
  KeyString: string;
  TimeSeries: string;
  ImportedTimeSeries: string;
  ObjectCount: Integer;
  ObjectIndex: Integer;
  FirstCell: TRchTimeItem;
  BoundName: string;
  ConnectionObjectLists: TRchConnectionObjectLists;
  ConnectionDictionary: TDictionary<string, TRchConnectionObjectList>;
  AConnectionList: TRchConnectionObjectList;
  ConnectionIndex: Integer;
  ConnectionItem: TRchConnection;
  AScreenObject: TScreenObject;
  AuxIFACE: TMf6BoundaryValue;
  AuxIndex: Integer;
  ChemSpeciesName: string;
  Aux: TMf6BoundaryValue;
  GwtAuxIndex: Integer;
  Values: TValueArrayStorage;
  CellIds: TCellIdList;
  AddCells: Boolean;
  CellListIndex: Integer;
  procedure AddItem(AScreenObject: TScreenObject; ACell: TRchTimeItem; Period: Integer);
  var
    RchItem: TRchItem;
    ImportedName: string;
    Concentrations: TRchGwtConcCollection;
    ChemSpeciesName: string;
    ConcItem: TGwtConcStringValueItem;
    GwtAuxIndex: Integer;
    AuxIndex: Integer;
    Aux: TMf6BoundaryValue;
    Imported_Chem: TValueArrayItem;
  begin
    RchItem := AScreenObject.ModflowRchBoundary.Values.Add as TRchItem;
    ItemList.Add(RchItem);
    RchItem.EndTime := LastTime;
    RchItem.StartTime := StartTime;

    if ACell.Recharge.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Recharge_Period_%d', [Period]);
      Imported_Recharge := AScreenObject.ImportedValues.Add;
      Imported_Recharge.Name := ImportedName;
      Imported_Recharge.Values.DataType := rdtDouble;
      RchItem.RechargeRate := rsObjectImportedValuesR + '("' + Imported_Recharge.Name + '")';
    end
    else
    begin
      Imported_Recharge := nil;
      TimeSeries := ACell.Recharge.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      RchItem.RechargeRate := ImportedTimeSeries;
    end;

    if TransportAuxNames.Count > 0 then
    begin
      Concentrations := RchItem.GwtConcentrations;
      Concentrations.Count := TransportAuxNames.Count;
      for AuxIndex := 0 to TransportAuxNames.Count - 1 do
      begin
        ChemSpeciesName := TransportAuxNames[AuxIndex];
        ConcItem := Concentrations[AuxIndex];

        GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
        Assert(GwtAuxIndex >= 0);
        Aux := ACell[GwtAuxIndex];
        if Aux.ValueType = vtNumeric then
        begin
          ImportedName := Format('Imported_%s_Period_%d', [ChemSpeciesName, Period]);
          Imported_Chem := AScreenObject.ImportedValues.Add;
          Imported_Chem.Name := ImportedName;
          Imported_Chem.Values.DataType := rdtDouble;
          ConcItem.Value := rsObjectImportedValuesR + '("' + Imported_Chem.Name + '")';
          TransportAuxNames.Objects[AuxIndex] := Imported_Chem.Values;
        end
        else
        begin
          TransportAuxNames.Objects[AuxIndex] := nil;
          TimeSeries := Aux.StringValue;
          if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
          begin
            Assert(False);
          end;
          ConcItem.Value := ImportedTimeSeries;
        end;
      end;
    end;
  end;
  procedure CreateObsScreenObject(ACell: TRchTimeItem);
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    CellId: TMfCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Rch_Obs_%d', [Package.PackageName, ObjectCount]));
    AScreenObject.Name := NewName;
    AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(AScreenObject);
    AScreenObject.ElevationCount := ecOne;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.EvaluatedAt := eaBlocks;
    AScreenObject.Visible := False;
    AScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    AScreenObject.CreateMf6Obs;
    AScreenObject.Modflow6Obs.Name := ACell.Boundname;
    AScreenObject.Modflow6Obs.General := [ogRch];

    CellId := ACell.Cellid;
    if Model.DisvUsed then
    begin
      CellId.Row := 1;
    end;
    ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
    APoint.x := ElementCenter.RotatedLocation.x;
    APoint.y := ElementCenter.RotatedLocation.y;
    AScreenObject.AddPoint(APoint, True);
    AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

  end;
  function CreateScreenObject(ACell: TRchTimeItem; Period: Integer): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    AuxIFACE: TMf6BoundaryValue;
    BoundName: string;
    ObsList: TObservationList;
    AnObs: TObservation;
    ObsIndex: Integer;
  begin
    Inc(ObjectCount);
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Rch_%d_Period_%d', [Package.PackageName, ObjectCount, Period]));
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateRchBoundary;
    if IfaceIndex >= 0 then
    begin
      AuxIFACE := ACell[IfaceIndex];
      Assert(AuxIFACE.ValueType = vtNumeric);
      IFACE := Round(AuxIFACE.NumericValue);
    end
    else
    begin
      IFACE := 0;
    end;
    result.IFACE := TIface(IFACE+2);

    AddItem(result, ACell, Period);

    BoundName := UpperCase(ACell.Boundname);
    if BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      result.CreateMf6Obs;
      for ObsIndex := 0 to ObsList.Count - 1 do
      begin
        AnObs := ObsList[ObsIndex];
        Assert(AnsiSameText(AnObs.ObsType, 'rch'));
      end;
      result.Modflow6Obs.Name := ACell.Boundname;
      result.Modflow6Obs.General := [ogRch];
    end;
  end;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing RCH package');
  end;
  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.RchPackage.IsSelected := True;
  Model.ModflowPackages.RchPackage.AssignmentMethod := umAdd;

  Rch := Package.Package as TRch;
  Options := Rch.Options;
  if Options.FIXED_CELL then
  begin
    Model.ModflowPackages.RchPackage.LayerOption := loSpecified;
  end
  else
  begin
    Model.ModflowPackages.RchPackage.LayerOption := loTopActive;
  end;

  CellIds := TCellIdList.Create;
  ConnectionObjectLists := TRchConnectionObjectLists.Create;
  ConnectionDictionary := TDictionary<string, TRchConnectionObjectList>.Create;
  ItemList := TList<TRchItem>.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  CellIdObsDictionary := TCellIdObsDictionary.Create;
  Map := TimeSeriesMap.Create;
  ObsLists := TObsLists.Create;
  KeyStringDictionary := TDictionary<string, TRchTimeItemList>.Create;
  CellLists := TObjectList<TRchTimeItemList>.Create;
  OtherCellLists := TObjectList<TRchTimeItemList>.Create;
  try
    OtherCellLists.OwnsObjects := False;
    try
      IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
      for TimeSeriesIndex := 0 to Rch.TimeSeriesCount - 1 do
      begin
        TimeSeriesPackage := Rch.TimeSeries[TimeSeriesIndex];
        ImportTimeSeries(TimeSeriesPackage, Map);
      end;
      for TimeSeriesIndex := 0 to Rch.TimeSeriesArrayCount - 1 do
      begin
        TimeSeriesPackage := Rch.TimeSeriesArray[TimeSeriesIndex];
        ImportTimeSeries(TimeSeriesPackage, Map);
      end;

      if Rch.ObservationCount > 0 then
      begin
        Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      end;
      for ObsPackageIndex := 0 to Rch.ObservationCount - 1 do
      begin
        ObsFiles := Rch.Observations[ObsPackageIndex].Package as TObs;
        GetObservations(nil, BoundNameObsDictionary,
          CellIdObsDictionary, ObsLists, ObsFiles);
      end;

      if Assigned(OnUpdateStatusBar) then
      begin
        OnUpdateStatusBar(self, 'importing RCH package');
      end;

      TransportAuxNames := TStringList.Create;
      try
        TransportAuxNames.CaseSensitive := False;
        for ModelIndex := 0 to TransportModels.Count - 1 do
        begin
          AModel := TransportModels[ModelIndex];
          TransportModel := AModel.FName as TTransportNameFile;
          for PackageIndex := 0 to TransportModel.NfPackages.Count  - 1 do
          begin
            APackage := TransportModel.NfPackages[PackageIndex];
            FoundMatch := False;
            if APackage.FileType = 'SSM6' then
            begin
              Ssm := APackage.Package as TSsm;
              for SourceIndex := 0 to Ssm.Sources.Count - 1 do
              begin
                if SameText(Ssm.Sources[SourceIndex].pname, Package.PackageName) then
                begin
                  FoundMatch := True;
                  TransportAuxNames.Add(Ssm.Sources[SourceIndex].auxname);
                  break;
                end;
              end;
  //            Assert(FoundMatch);
              break;
            end;
          end;
        end;

        LastTime := Model.ModflowStressPeriods.Last.EndTime;

        ObjectCount := 0;
        for PeriodIndex := 0 to Rch.PeriodCount - 1 do
        begin
          APeriod := Rch.Periods[PeriodIndex];
          StartTime := Model.ModflowStressPeriods[APeriod.Period-1].StartTime;
          for RchIndex := 0 to ItemList.Count - 1 do
          begin
            AnItem := ItemList[RchIndex];
            AnItem.EndTime := StartTime;
          end;
          ItemList.Clear;
          for CellListIndex := 0 to CellLists.Count - 1 do
          begin
            CellLists[CellListIndex].Clear;
          end;

          for CellIndex := 0 to APeriod.Count - 1 do
          begin
            ACell := APeriod[CellIndex];

            if (ACell.Boundname <> '')
              and BoundNameObsDictionary.ContainsKey(UpperCase(ACell.Boundname)) then
            begin
              KeyString := 'BN:' + UpperCase(ACell.Boundname) + ' ';
            end
            else
            begin
              KeyString := '';
            end;

            if IfaceIndex < 0 then
            begin
              IFACE := 0;
            end
            else
            begin
              AuxIFACE := ACell[IfaceIndex];
              Assert(AuxIFACE.ValueType = vtNumeric);
              IFACE := Round(AuxIFACE.NumericValue);
            end;
            KeyString := KeyString + ACell.Keystring + ' IFACE:' + IntToStr(IFACE);
            if not KeyStringDictionary.TryGetValue(KeyString, ACellList) then
            begin
              ACellList := TRchTimeItemList.Create;
              ACellList.OwnsObjects := False;
              CellLists.Add(ACellList);
              KeyStringDictionary.Add(KeyString, ACellList);
            end;
            ACellList.Add(ACell);
          end;
          for ObjectIndex := 0 to CellLists.Count - 1 do
          begin
            AddCells := True;
            ACellList := CellLists[ObjectIndex];
            if ACellList.Count > 0 then
            begin
              FirstCell := ACellList[0];
              if (FirstCell.Boundname <> '')
                and BoundNameObsDictionary.ContainsKey(UpperCase(FirstCell.Boundname)) then
              begin
                if IfaceIndex < 0 then
                begin
                  IFACE := 0;
                end
                else
                begin
                  AuxIFACE := FirstCell[IfaceIndex];
                  Assert(AuxIFACE.ValueType = vtNumeric);
                  IFACE := Round(AuxIFACE.NumericValue);
                end;
                BoundName := UpperCase(FirstCell.Boundname);
                if not ConnectionDictionary.TryGetValue(BoundName, AConnectionList) then
                begin
                  AConnectionList := TRchConnectionObjectList.Create;
                  ConnectionObjectLists.Add(AConnectionList);
                  ConnectionDictionary.Add(BoundName, AConnectionList)
                end;
                ACellList.Sort;
                AScreenObject := nil;
                for ConnectionIndex := 0 to AConnectionList.Count - 1 do
                begin
                  ConnectionItem := AConnectionList[ConnectionIndex];
                  if (IFACE = ConnectionItem.IFACE)
                    and ACellList.SameCells(ConnectionItem.List) then
                  begin
                    AScreenObject := ConnectionItem.ScreenObject;
                    AddCells := False;
                    Break;
                  end;
                end;
                if AScreenObject = nil then
                begin
                  AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
                  ConnectionItem := TRchConnection.Create;
                  ConnectionItem.ScreenObject := AScreenObject;
                  ConnectionItem.IFACE := IFACE;
                  ConnectionItem.List := ACellList;
                  AConnectionList.Add(ConnectionItem);
                  OtherCellLists.Add(ACellList);
  //                CellLists.OwnsObjects := False;
  //                try
  //                  CellLists[ObjectIndex] := nil;
  //                finally
  //                  CellLists.OwnsObjects := True;
  //                end;
                end
                else
                begin
                  AddItem(AScreenObject, FirstCell, APeriod.Period);
                end;
              end
              else
              begin
                AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
              end;
            end;

            CellIds.Clear;
            for CellIndex := 0 to ACellList.Count - 1 do
            begin
              ACell := ACellList[CellIndex];
              if ACell.Recharge.ValueType = vtNumeric then
              begin
                Imported_Recharge.Values.Add(ACell.Recharge.NumericValue);
              end;

              for AuxIndex := 0 to TransportAuxNames.Count - 1 do
              begin
                ChemSpeciesName := TransportAuxNames[AuxIndex];
                GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
                Aux := ACell[GwtAuxIndex];
                if Aux.ValueType = vtNumeric then
                begin
                  Values := TransportAuxNames.Objects[AuxIndex] as TValueArrayStorage;
                  Values.Add(Aux.NumericValue);
                end;
              end;

              CellIds.Add(ACell.Cellid);

              if CellIdObsDictionary.ContainsKey(ACell.Cellid) then
              begin
                CreateObsScreenObject(ACell);
              end;
            end;
            if AddCells then
            begin
              AddPointsToScreenObject(CellIds, AScreenObject);
            end;
          end;
        end;

      finally
        TransportAuxNames.Free;
      end;

    finally
      for CellListIndex := 0 to OtherCellLists.Count - 1 do
      begin
        CellLists.Extract(OtherCellLists[CellListIndex])
      end;
    end;
  finally
    BoundNameObsDictionary.Free;
    CellIdObsDictionary.Free;
    Map.Free;
    ObsLists.Free;
    KeyStringDictionary.Free;
    CellLists.Free;
    ItemList.Free;
    ConnectionObjectLists.Free;
    ConnectionDictionary.Free;
    CellIds.Free;
    OtherCellLists.Free;
  end;
end;

type
  TRivConnection = class(TObject)
    ScreenObject: TScreenObject;
    List: TRivTimeItemList;
    IFACE: Integer;
    destructor Destroy; override;
  end;

  TRivConnectionObjectList = TObjectList<TRivConnection>;
  TRivConnectionObjectLists = TObjectList<TRivConnectionObjectList>;

  TRivMvrLink = record
    RivPeriod: TRivPeriod;
    MvrPeriod: TMvrPeriod;
    function Period: Integer;
  end;
  TRivMvrLinkArray = TArray<TRivMvrLink>;
  TRivMvrLinkList = TList<TRivMvrLink>;

  TMvrRivTimeItemList = Class(TRivTimeItemList)
    FIds: TGenericIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure Sort;
  end;

procedure TModflow6Importer.ImportRiv(Package: TPackage;
  TransportModels: TModelList; MvrPackage: TPackage);
var
  Model: TPhastModel;
  Riv: TRiv;
  AModel: TModel;
  ModelIndex: Integer;
  TransportModel: TTransportNameFile;
  PackageIndex: Integer;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary;
  Ssm: TSsm;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ObsLists: TObsLists;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  SourceIndex: Integer;
  FoundMatch: Boolean;
  TransportAuxNames: TStringList;
  PeriodIndex: Integer;
  APeriod: TRivPeriod;
  CellIndex: Integer;
  ACell: TRivTimeItem;
  IfaceIndex: Integer;
  KeyStringDictionary: TDictionary<string, TMvrRivTimeItemList>;
  CellLists: TObjectList<TMvrRivTimeItemList>;
  OtherCellLists: TObjectList<TMvrRivTimeItemList>;
  ACellList: TMvrRivTimeItemList;
  Options: TRivOptions;
  IFace: Integer;
  LastTime: Double;
  Imported_River_Conductance: TValueArrayItem;
  Imported_River_Stage: TValueArrayItem;
  Imported_River_RBot: TValueArrayItem;
  ItemList: TList<TRivItem>;
  StartTime: Double;
  RivIndex: Integer;
  AnItem: TRivItem;
  KeyString: string;
  TimeSeries: string;
  ImportedTimeSeries: string;
  ObjectCount: Integer;
  ObjectIndex: Integer;
  FirstCell: TRivTimeItem;
  BoundName: string;
  ConnectionObjectLists: TRivConnectionObjectLists;
  ConnectionDictionary: TDictionary<string, TRivConnectionObjectList>;
  AConnectionList: TRivConnectionObjectList;
  ConnectionIndex: Integer;
  ConnectionItem: TRivConnection;
  AScreenObject: TScreenObject;
  AuxIFACE: TMf6BoundaryValue;
  AuxIndex: Integer;
  ChemSpeciesName: string;
  Aux: TMf6BoundaryValue;
  GwtAuxIndex: Integer;
  Values: TValueArrayStorage;
  CellIds: TCellIdList;
  Mvr: TMvr;
  Index: Integer;
  RivMvrLink: TRivMvrLink;
  RivMvrLinkList: TRivMvrLinkList;
  MvrPeriod: TMvrPeriod;
  MvrUsed: Boolean;
  NewScreenObject: Boolean;
  MvrSource: TMvrSource;
  APackage: TPackage;
  AuxMultIndex: Integer;
  AuxMultiplier: Extended;
  RivMvrLinkArray: TRivMvrLinkArray;
  RivlPeriod: TRivPeriod;
  NextRivPeriod: TRivPeriod;
  EndPeriod: Integer;
  NextMvrPeriod: TMvrPeriod;
  CellListIndex: Integer;
  procedure AddItem(AScreenObject: TScreenObject; ACell: TRivTimeItem; Period: Integer);
  var
    RivItem: TRivItem;
    ImportedName: string;
    Concentrations: TRivGwtConcCollection;
    ChemSpeciesName: string;
    ConcItem: TGwtConcStringValueItem;
    GwtAuxIndex: Integer;
    AuxIndex: Integer;
    Aux: TMf6BoundaryValue;
    Imported_Chem: TValueArrayItem;
    AuxMultiplier: Extended;
  begin
    RivItem := AScreenObject.ModflowRivBoundary.Values.Add as TRivItem;
    ItemList.Add(RivItem);
    RivItem.EndTime := LastTime;
    RivItem.StartTime := StartTime;

    if AuxMultIndex >= 0 then
    begin
      Aux := ACell.Aux[AuxMultIndex];
      if Aux.ValueType = vtNumeric then
      begin
        AuxMultiplier := Aux.NumericValue
      end
      else
      begin
        AuxMultiplier := 1;
        FErrorMessages.Add(StrModelMuseCanNotIm);
      end;
    end
    else
    begin
      AuxMultiplier := 1;
    end;

    if ACell.Cond.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_River_Conductance_Period_%d', [Period]);
      Imported_River_Conductance := AScreenObject.ImportedValues.Add;
      Imported_River_Conductance.Name := ImportedName;
      Imported_River_Conductance.Values.DataType := rdtDouble;
      RivItem.Conductance := rsObjectImportedValuesR + '("' + Imported_River_Conductance.Name + '")';
    end
    else
    begin
      Imported_River_Conductance := nil;
      TimeSeries := ACell.Cond.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      RivItem.Conductance := ImportedTimeSeries;
      if AuxMultiplier <> 1 then
      begin
        FErrorMessages.Add(StrModelMuseCanNotAp);
      end;
    end;

    if ACell.Stage.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_River_Stage_Period_%d', [Period]);
      Imported_River_Stage := AScreenObject.ImportedValues.Add;
      Imported_River_Stage.Name := ImportedName;
      Imported_River_Stage.Values.DataType := rdtDouble;
      RivItem.RiverStage := rsObjectImportedValuesR + '("' + Imported_River_Stage.Name + '")';
    end
    else
    begin
      Imported_River_Stage := nil;
      TimeSeries := ACell.Stage.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      RivItem.RiverStage := ImportedTimeSeries;
      if AuxMultiplier <> 1 then
      begin
        FErrorMessages.Add(StrModelMuseCanNotAp);
      end;
    end;

    if ACell.RBot.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_River_RBot_Period_%d', [Period]);
      Imported_River_RBot := AScreenObject.ImportedValues.Add;
      Imported_River_RBot.Name := ImportedName;
      Imported_River_RBot.Values.DataType := rdtDouble;
      RivItem.RiverBottom := rsObjectImportedValuesR + '("' + Imported_River_RBot.Name + '")';
    end
    else
    begin
      Imported_River_RBot := nil;
      TimeSeries := ACell.RBot.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      RivItem.RiverBottom := ImportedTimeSeries;
      if AuxMultiplier <> 1 then
      begin
        FErrorMessages.Add(StrModelMuseCanNotAp);
      end;
    end;


    if TransportAuxNames.Count > 0 then
    begin
      Concentrations := RivItem.GwtConcentrations;
      Concentrations.Count := TransportAuxNames.Count;
      for AuxIndex := 0 to TransportAuxNames.Count - 1 do
      begin
        ChemSpeciesName := TransportAuxNames[AuxIndex];
        ConcItem := Concentrations[AuxIndex];

        GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
        Assert(GwtAuxIndex >= 0);
        Aux := ACell[GwtAuxIndex];
        if Aux.ValueType = vtNumeric then
        begin
          ImportedName := Format('Imported_%s_Period_%d', [ChemSpeciesName, Period]);
          Imported_Chem := AScreenObject.ImportedValues.Add;
          Imported_Chem.Name := ImportedName;
          Imported_Chem.Values.DataType := rdtDouble;
          ConcItem.Value := rsObjectImportedValuesR + '("' + Imported_Chem.Name + '")';
          TransportAuxNames.Objects[AuxIndex] := Imported_Chem.Values;
        end
        else
        begin
          TransportAuxNames.Objects[AuxIndex] := nil;
          TimeSeries := Aux.StringValue;
          if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
          begin
            Assert(False);
          end;
          ConcItem.Value := ImportedTimeSeries;
        end;
      end;
    end;
  end;
  procedure CreateObsScreenObject(ACell: TRivTimeItem);
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    CellId: TMfCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Riv_Obs_%d', [Package.PackageName, ObjectCount]));
    AScreenObject.Name := NewName;
    AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(AScreenObject);
    AScreenObject.ElevationCount := ecOne;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.EvaluatedAt := eaBlocks;
    AScreenObject.Visible := False;
    AScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    AScreenObject.CreateMf6Obs;
    AScreenObject.Modflow6Obs.Name := ACell.Boundname;
    AScreenObject.Modflow6Obs.General := [ogRiv];

    CellId := ACell.Cellid;
    if Model.DisvUsed then
    begin
      CellId.Row := 1;
    end;
    ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
    APoint.x := ElementCenter.RotatedLocation.x;
    APoint.y := ElementCenter.RotatedLocation.y;
    AScreenObject.AddPoint(APoint, True);
    AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

  end;
  function CreateScreenObject(ACell: TRivTimeItem; Period: Integer): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    AuxIFACE: TMf6BoundaryValue;
    BoundName: string;
    ObsList: TObservationList;
    AnObs: TObservation;
    ObsIndex: Integer;
    General: TObGenerals;
  begin
    Inc(ObjectCount);
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Riv_%d_Period_%d', [Package.PackageName, ObjectCount, Period]));
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateRivBoundary;
    result.ModflowRivBoundary.FormulaInterpretation := fiDirect;
    if IfaceIndex >= 0 then
    begin
      AuxIFACE := ACell[IfaceIndex];
      Assert(AuxIFACE.ValueType = vtNumeric);
      IFACE := Round(AuxIFACE.NumericValue);
    end
    else
    begin
      IFACE := 0;
    end;
    result.IFACE := TIface(IFACE+2);

    AddItem(result, ACell, Period);

    BoundName := UpperCase(ACell.Boundname);
    if BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      result.CreateMf6Obs;
      General := [];
      for ObsIndex := 0 to ObsList.Count - 1 do
      begin
        AnObs := ObsList[ObsIndex];
        if AnsiSameText(AnObs.ObsType, 'riv') then
        begin
          Include(General, ogRiv);
        end
        else if AnsiSameText(AnObs.ObsType, 'to-mvr') then
        begin
          Include(General, ogMvr);
        end
        else
        begin
          Assert(False);
        end;
      end;
      result.Modflow6Obs.Name := ACell.Boundname;
      result.Modflow6Obs.General := General;
    end;
  end;
begin
  MvrSource.LakeOutlet := nil;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing RIV package');
  end;
  // Get the MVR package.
  Mvr := GetMvr(MvrPackage, Package);

  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.RivPackage.IsSelected := True;

  Riv := Package.Package as TRiv;
  Options := Riv.Options;

  if Options.AUXMULTNAME <> '' then
  begin
    AuxMultIndex := Options.IndexOfAUXILIARY(Options.AUXMULTNAME);
  end
  else
  begin
    AuxMultIndex := -1;
  end;

  RivMvrLinkList := TRivMvrLinkList.Create;
  CellIds := TCellIdList.Create;
  ConnectionObjectLists := TRivConnectionObjectLists.Create;
  ConnectionDictionary := TDictionary<string, TRivConnectionObjectList>.Create;
  ItemList := TList<TRivItem>.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  CellIdObsDictionary := TCellIdObsDictionary.Create;
  Map := TimeSeriesMap.Create;
  ObsLists := TObsLists.Create;
  KeyStringDictionary := TDictionary<string, TMvrRivTimeItemList>.Create;
  CellLists := TObjectList<TMvrRivTimeItemList>.Create;
  OtherCellLists := TObjectList<TMvrRivTimeItemList>.Create;
  try
    OtherCellLists.OwnsObjects := False;
    try
      if Mvr = nil then
      begin
        RivMvrLink.MvrPeriod := nil;
        for PeriodIndex := 0 to Riv.PeriodCount - 1 do
        begin
          RivMvrLink.RivPeriod := Riv.Periods[PeriodIndex];
          RivMvrLinkList.Add(RivMvrLink)
        end;
      end
      else
      begin
        // Make sure that all the stress periods defined in either the MVR or the
        // Riv package are imported.
        SetLength(RivMvrLinkArray, Model.ModflowStressPeriods.Count);
        for PeriodIndex := 0 to Length(RivMvrLinkArray) - 1 do
        begin
          RivMvrLinkArray[PeriodIndex].RivPeriod := nil;
          RivMvrLinkArray[PeriodIndex].MvrPeriod := nil;
        end;

        for PeriodIndex := 0 to Riv.PeriodCount - 1 do
        begin
          RivlPeriod := Riv.Periods[PeriodIndex];
          if PeriodIndex < Riv.PeriodCount - 1 then
          begin
            NextRivPeriod := Riv.Periods[PeriodIndex+1];
            EndPeriod := NextRivPeriod.Period;
          end
          else
          begin
            EndPeriod := Model.ModflowStressPeriods.Count;
          end;
          for Index := RivlPeriod.Period  to EndPeriod do
          begin
            RivMvrLinkArray[Index-1].RivPeriod  := RivlPeriod;
          end;
        end;

        for PeriodIndex := 0 to Mvr.PeriodCount - 1 do
        begin
          MvrPeriod := Mvr.Periods[PeriodIndex];
          if PeriodIndex < Mvr.PeriodCount - 1 then
          begin
            NextMvrPeriod := Mvr.Periods[PeriodIndex+1];
            EndPeriod := NextMvrPeriod.Period;
          end
          else
          begin
            EndPeriod := Model.ModflowStressPeriods.Count;
          end;
          for Index := MvrPeriod.Period  to EndPeriod do
          begin
            RivMvrLinkArray[Index-1].MvrPeriod  := MvrPeriod;
          end;
        end;

        RivMvrLinkList.Add(RivMvrLinkArray[0]);
        for Index := 1 to Length(RivMvrLinkArray) - 1 do
        begin
          if (RivMvrLinkArray[Index].RivPeriod <> RivMvrLinkArray[Index-1].RivPeriod)
            or (RivMvrLinkArray[Index].MvrPeriod <> RivMvrLinkArray[Index-1].MvrPeriod) then
          begin
            RivMvrLinkList.Add(RivMvrLinkArray[Index]);
          end;
        end;
      end;

      IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
      for TimeSeriesIndex := 0 to Riv.TimeSeriesCount - 1 do
      begin
        TimeSeriesPackage := Riv.TimeSeries[TimeSeriesIndex];
        ImportTimeSeries(TimeSeriesPackage, Map);
      end;

      if Riv.ObservationCount > 0 then
      begin
        Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      end;
      for ObsPackageIndex := 0 to Riv.ObservationCount - 1 do
      begin
        ObsFiles := Riv.Observations[ObsPackageIndex].Package as TObs;
        GetObservations(nil, BoundNameObsDictionary,
          CellIdObsDictionary, ObsLists, ObsFiles);
      end;

      if Assigned(OnUpdateStatusBar) then
      begin
        OnUpdateStatusBar(self, 'importing RIV package');
      end;

      TransportAuxNames := TStringList.Create;
      try
        TransportAuxNames.CaseSensitive := False;
        for ModelIndex := 0 to TransportModels.Count - 1 do
        begin
          AModel := TransportModels[ModelIndex];
          TransportModel := AModel.FName as TTransportNameFile;
          for PackageIndex := 0 to TransportModel.NfPackages.Count  - 1 do
          begin
            APackage := TransportModel.NfPackages[PackageIndex];
            FoundMatch := False;
            if APackage.FileType = 'SSM6' then
            begin
              Ssm := APackage.Package as TSsm;
              for SourceIndex := 0 to Ssm.Sources.Count - 1 do
              begin
                if SameText(Ssm.Sources[SourceIndex].pname, Package.PackageName) then
                begin
                  FoundMatch := True;
                  TransportAuxNames.Add(Ssm.Sources[SourceIndex].auxname);
                  break;
                end;
              end;
  //            Assert(FoundMatch);
              break;
            end;
          end;
        end;

        LastTime := Model.ModflowStressPeriods.Last.EndTime;

        ACellList := nil;
        ObjectCount := 0;
        for PeriodIndex := 0 to RivMvrLinkList.Count - 1 do
        begin
          RivMvrLink := RivMvrLinkList[PeriodIndex];
          APeriod := RivMvrLinkList[PeriodIndex].RivPeriod;
          if APeriod = nil then
          begin
            Continue;
          end;
          StartTime := Model.ModflowStressPeriods[RivMvrLink.Period-1].StartTime;
          for RivIndex := 0 to ItemList.Count - 1 do
          begin
            AnItem := ItemList[RivIndex];
            AnItem.EndTime := StartTime;
          end;
          ItemList.Clear;
          for CellListIndex := 0 to CellLists.Count - 1 do
          begin
            CellLists[CellListIndex].Clear;
          end;

          // Assign all cells in the current period to a cell list.
          for CellIndex := 0 to APeriod.Count - 1 do
          begin
            ACell := APeriod[CellIndex];

            if (ACell.Boundname <> '')
              and BoundNameObsDictionary.ContainsKey(UpperCase(ACell.Boundname)) then
            begin
              KeyString := 'BN:' + UpperCase(ACell.Boundname) + ' ';
            end
            else
            begin
              KeyString := '';
            end;

            if IfaceIndex < 0 then
            begin
              IFACE := 0;
            end
            else
            begin
              AuxIFACE := ACell[IfaceIndex];
              Assert(AuxIFACE.ValueType = vtNumeric);
              IFACE := Round(AuxIFACE.NumericValue);
            end;
            KeyString := KeyString + ACell.Keystring + ' IFACE:' + IntToStr(IFACE);

            MvrUsed := False;
            if RivMvrLink.MvrPeriod <> nil then
            begin
              if RivMvrLink.MvrPeriod.HasSource(Package.PackageName, ACell.Id) then
              begin
                KeyString := KeyString + ' MVR ' + InttoStr(ACell.Id);
                MvrUsed := True;
              end;
            end;

            if not KeyStringDictionary.TryGetValue(KeyString, ACellList) then
            begin
              ACellList := TMvrRivTimeItemList.Create;
              CellLists.Add(ACellList);
              KeyStringDictionary.Add(KeyString, ACellList);
            end;
            ACellList.Add(ACell);
            if MvrUsed then
            begin
              if ACellList.FIds.IndexOf(ACell.Id) < 0 then
              begin
                ACellList.FIds.Add(ACell.Id);
              end;
            end;
          end;

          // After all the cells in the current period have been read,
          // create a TScreenObject for each cell list
          AScreenObject := nil;
          for ObjectIndex := 0 to CellLists.Count - 1 do
          begin
            NewScreenObject := False;
            ACellList := CellLists[ObjectIndex];
            if ACellList.Count > 0 then
            begin
              FirstCell := ACellList[0];
              if (FirstCell.Boundname <> '')
                and BoundNameObsDictionary.ContainsKey(UpperCase(FirstCell.Boundname)) then
              begin
                if IfaceIndex < 0 then
                begin
                  IFACE := 0;
                end
                else
                begin
                  AuxIFACE := FirstCell[IfaceIndex];
                  Assert(AuxIFACE.ValueType = vtNumeric);
                  IFACE := Round(AuxIFACE.NumericValue);
                end;
                BoundName := UpperCase(FirstCell.Boundname);
                if not ConnectionDictionary.TryGetValue(BoundName, AConnectionList) then
                begin
                  AConnectionList := TRivConnectionObjectList.Create;
                  ConnectionObjectLists.Add(AConnectionList);
                  ConnectionDictionary.Add(BoundName, AConnectionList)
                end;
                ACellList.Sort;
                AScreenObject := nil;
                for ConnectionIndex := 0 to AConnectionList.Count - 1 do
                begin
                  ConnectionItem := AConnectionList[ConnectionIndex];
                  if (IFACE = ConnectionItem.IFACE)
                    and ACellList.SameCells(ConnectionItem.List) then
                  begin
                    AScreenObject := ConnectionItem.ScreenObject;
                    Break;
                  end;
                end;
                if AScreenObject = nil then
                begin
                  AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
                  ConnectionItem := TRivConnection.Create;
                  ConnectionItem.ScreenObject := AScreenObject;
                  ConnectionItem.IFACE := IFACE;
                  ConnectionItem.List := ACellList;
                  AConnectionList.Add(ConnectionItem);
                  OtherCellLists.Add(ACellList);
  //                CellLists.OwnsObjects := False;
  //                try
  //                  CellLists[ObjectIndex] := nil;
  //                finally
  //                  CellLists.OwnsObjects := True;
  //                end;
                  NewScreenObject := True;
                end
                else
                begin
                  AddItem(AScreenObject, FirstCell, APeriod.Period);
                end;
              end
              else
              begin
                AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
                NewScreenObject := True;
              end;
            end;

            CellIds.Clear;
            for CellIndex := 0 to ACellList.Count - 1 do
            begin
              ACell := ACellList[CellIndex];
              if ACell.Cond.ValueType = vtNumeric then
              begin
                if AuxMultIndex >= 0 then
                begin
                  Aux := ACell.Aux[AuxMultIndex];
                  if Aux.ValueType = vtNumeric then
                  begin
                    AuxMultiplier := Aux.NumericValue
                  end
                  else
                  begin
                    AuxMultiplier := 1;
                    FErrorMessages.Add(StrModelMuseCanNotIm);
                  end;
                end
                else
                begin
                  AuxMultiplier := 1;
                end;
                Imported_River_Conductance.Values.Add(ACell.Cond.NumericValue * AuxMultiplier);
              end;

              if ACell.Stage.ValueType = vtNumeric then
              begin
                Imported_River_Stage.Values.Add(ACell.Stage.NumericValue);
              end;

              if ACell.RBot.ValueType = vtNumeric then
              begin
                Imported_River_RBot.Values.Add(ACell.RBot.NumericValue);
              end;

              for AuxIndex := 0 to TransportAuxNames.Count - 1 do
              begin
                ChemSpeciesName := TransportAuxNames[AuxIndex];
                GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
                Aux := ACell[GwtAuxIndex];
                if Aux.ValueType = vtNumeric then
                begin
                  Values := TransportAuxNames.Objects[AuxIndex] as TValueArrayStorage;
                  Values.Add(Aux.NumericValue);
                end;
              end;

              if NewScreenObject then
              begin
                CellIds.Add(ACell.Cellid);
              end;

              if CellIdObsDictionary.ContainsKey(ACell.Cellid) then
              begin
                CreateObsScreenObject(ACell);
              end;
            end;

            if NewScreenObject then
            begin
              AddPointsToScreenObject(CellIds, AScreenObject);
            end;

            if ACellList.FIds.Count > 0 then
            begin
              MvrSource.ScreenObject := AScreenObject;
              MvrSource.PackageName := Package.PackageName;
              MvrSource.Period := RivMvrLink.Period;
              MvrSource.IDs := ACellList.FIds.ToArray;
              MvrSource.SourceType := mspcRiv;
              FMvrSources.Add(MvrSource);
            end;
          end;
        end;

      finally
        TransportAuxNames.Free;
      end;

    finally
      for CellListIndex := 0 to OtherCellLists.Count - 1 do
      begin
        CellLists.Extract(OtherCellLists[CellListIndex])
      end;
    end;
  finally
      BoundNameObsDictionary.Free;
      CellIdObsDictionary.Free;
      Map.Free;
      ObsLists.Free;
      KeyStringDictionary.Free;
      CellLists.Free;
      ItemList.Free;
      ConnectionObjectLists.Free;
      ConnectionDictionary.Free;
      CellIds.Free;
      RivMvrLinkList.Free;
      OtherCellLists.Free;
  end;
end;

procedure TModflow6Importer.Assign3DBooleanDataSet(DsName: string;
  Data: TIArray3D);
var
  Formula: string;
  Model: TPhastModel;
  LayerIndex: Integer;
  FirstValue: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  Uniform: Boolean;
  DataArrayName: string;
  DataArray: TDataArray;
  Interpolator: TNearestPoint2DInterpolator;
  ScreenObject: TScreenObject;
begin
  Formula := 'CaseB(Layer';
  Model := frmGoPhast.PhastModel;
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstValue := Data[LayerIndex - 1, 0, 0] <> 0;
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstValue = (Data[LayerIndex - 1, RowIndex, ColIndex] <> 0);
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Format('Imported_%s_%d', [DsName, LayerIndex]);
    Formula := Formula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
      DataArrayName, 'False', DataArrayName, [dcType], rdtBoolean, eaBlocks, dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s', [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      if FirstValue then
      begin
        DataArray.Formula := 'True';
      end
      else
      begin
        DataArray.Formula := 'False';
      end;
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignBooleanValuesToCellCenters(DataArray, ScreenObject, Data[LayerIndex - 1]);
    end;
  end;
  Formula := Formula + ')';
  if Model.LayerCount = 1 then
  begin
    DataArrayName := Format('Imported_%s_%d', [DsName, 1]);
    Formula := DataArrayName;
  end
  else
  begin
    Formula := Format('IfB(Layer > %d, False, %s)', [Model.LayerCount, Formula]);
  end;
  DataArray := Model.DataArrayManager.GetDataSetByName(DsName);
  Assert(DataArray <> nil);
  DataArray.Formula := Formula;
end;

procedure TModflow6Importer.Assign3DIntegerDataSet(DsName: string;
  Data: TIArray3D);
var
  Formula: string;
  Model: TPhastModel;
  LayerIndex: Integer;
  FirstValue: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Uniform: Boolean;
  DataArrayName: string;
  DataArray: TDataArray;
  Interpolator: TNearestPoint2DInterpolator;
  ScreenObject: TScreenObject;
begin
  Formula := 'CaseI(Layer';
  Model := frmGoPhast.PhastModel;
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstValue := Data[LayerIndex - 1, 0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstValue = Data[LayerIndex - 1, RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Format('Imported_%s_%d', [DsName, LayerIndex]);
    Formula := Formula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
      DataArrayName, '0', DataArrayName, [dcType], rdtInteger, eaBlocks, dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s', [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      DataArray.Formula := IntToStr(FirstValue);
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignIntegerValuesToCellCenters(DataArray, ScreenObject, Data[LayerIndex - 1]);
    end;
  end;
  Formula := Formula + ')';
  if Model.LayerCount = 1 then
  begin
    DataArrayName := Format('Imported_%s_%d', [DsName, 1]);
    Formula := DataArrayName;
  end
  else
  begin
    Formula := Format('IfI(Layer > %d, 0, %s)', [Model.LayerCount, Formula]);
  end;
  DataArray := Model.DataArrayManager.GetDataSetByName(DsName);
  Assert(DataArray <> nil);
  DataArray.Formula := Formula;
end;

procedure TModflow6Importer.Assign3DRealDataSet(DsName: string; Data: TDArray3D);
var
  Formula: string;
  Model: TPhastModel;
  LayerIndex: Integer;
  FirstValue: Double;
  RowIndex: Integer;
  ColIndex: Integer;
  Uniform: Boolean;
  DataArrayName: string;
  DataArray: TDataArray;
  Interpolator: TNearestPoint2DInterpolator;
  ScreenObject: TScreenObject;
begin
  Formula := 'CaseR(Layer';
  Model := frmGoPhast.PhastModel;
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstValue := Data[LayerIndex - 1, 0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstValue = Data[LayerIndex - 1, RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Format('Imported_%s_%d', [DsName, LayerIndex]);
    Formula := Formula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
      DataArrayName, '0', DataArrayName, [dcType], rdtDouble, eaBlocks, dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s', [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      DataArray.Formula := FortranFloatToStr(FirstValue);
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignRealValuesToCellCenters(DataArray, ScreenObject, Data[LayerIndex - 1]);
    end;
  end;
  Formula := Formula + ')';
  if Model.LayerCount = 1 then
  begin
    DataArrayName := Format('Imported_%s_%d', [DsName, 1]);
    Formula := DataArrayName;
  end
  else
  begin
    Formula := Format('IfR(Layer > %d, 1, %s)', [Model.LayerCount, Formula]);
  end;
  DataArray := Model.DataArrayManager.GetDataSetByName(DsName);
  Assert(DataArray <> nil);
  DataArray.Formula := Formula;
end;

procedure TModflow6Importer.AssignTOP(TOP: TDArray2D);
var
  Model: TPhastModel; 
  ColIndex: Integer; 
  RowIndex: Integer;
  Uniform: Boolean;
  FirstValue: Double;
  DataArrayName: string;
  DataArray: TDataArray;
  ScreenObject: TScreenObject;
begin
  Model := frmGoPhast.PhastModel;
  Uniform := True;
  FirstValue := TOP[0, 0];
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
      Uniform := FirstValue = TOP[RowIndex, ColIndex];
      if not Uniform then
      begin
        break;
      end;
    end;
  end;
  DataArrayName := Model.LayerStructure[0].DataArrayName;
  DataArray := Model.DataArrayManager.GetDataSetByName(DataArrayName);
  if Uniform then
  begin
    DataArray.Formula := FortranFloatToStr(FirstValue);
  end
  else
  begin
    ScreenObject := AllTopCellsScreenObject;
    AssignRealValuesToCellCenters(DataArray, ScreenObject, TOP);
  end;
end;

procedure TModflow6Importer.AssignBooleanValuesToCellCenters(
  DataArray: TDataArray; ScreenObject: TScreenObject; ImportedData: TIArray2D);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesB
    + '("' + DataArray.Name + '")';
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtBoolean;
  ImportedValues.Values.Count := Model.RowCount * Model.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to Model.RowCount - 1 do
  begin
    for ColIndex := 0 to Model.ColumnCount - 1 do
    begin
      ImportedValues.Values.BooleanValues[PointIndex] :=
        ImportedData[RowIndex, ColIndex] <> 0;
      Inc(PointIndex);
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.Values.CacheData;
end;

procedure TModflow6Importer.AssignBOTM(BOTM: TDArray3D);
var
  Model: TPhastModel; 
  LayerIndex: Integer;
  Uniform: Boolean; 
  FirstValue: Double; 
  DataArrayName: string; 
  DataArray: TDataArray; 
  ScreenObject: TScreenObject;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Model := frmGoPhast.PhastModel;
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstValue := BOTM[LayerIndex - 1, 0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstValue = BOTM[LayerIndex - 1, RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Model.LayerStructure[LayerIndex].DataArrayName;
    DataArray := Model.DataArrayManager.GetDataSetByName(DataArrayName);
    if Uniform then
    begin
      DataArray.Formula := FortranFloatToStr(FirstValue);
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignRealValuesToCellCenters(DataArray, ScreenObject, BOTM[LayerIndex - 1]);
    end;
  end;
end;

procedure TModflow6Importer.AssignIDomain(IDOMAIN: TIArray3D; NumberOfLayers: Integer);
var
  Uniform: Boolean; 
  DataArrayName: string; 
  DataArray: TDataArray; 
  ScreenObject: TScreenObject;
  Model: TPhastModel; 
  ColIndex: Integer; 
  RowIndex: Integer; 
  LayerIndex: Integer;
  IDomainFormula: string;
  FirstIntValue: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  ActiveFormula: string;
  Active: TBArray2D;
  FirstBoolValue: Boolean;
begin
  Model := frmGoPhast.PhastModel;
  if IDOMAIN = nil then
  begin
    DataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
    DataArray.Formula := '1';
    DataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    DataArray.Formula := 'True';
    Exit;
  end;
  IDomainFormula := 'CaseI(Layer';
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Uniform := True;
    FirstIntValue := IDOMAIN[LayerIndex - 1, 0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstIntValue = IDOMAIN[LayerIndex - 1, RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
      if not Uniform then
      begin
        break;
      end;
    end;
    DataArrayName := Format('Imported_IDOMAIN_%d', [LayerIndex]);
    IDomainFormula := IDomainFormula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
      DataArrayName, '0', DataArrayName, [dcType], rdtInteger, eaBlocks,
      dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s',
      [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      DataArray.Formula := IntToStr(FirstIntValue);
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignIntegerValuesToCellCenters(DataArray, ScreenObject, IDOMAIN[LayerIndex - 1]);
    end;
  end;
  IDomainFormula := IDomainFormula + ')';
  if NumberOfLayers = 1 then
  begin
    DataArrayName := Format('Imported_IDOMAIN_%d', [1]);
    IDomainFormula := DataArrayName;
  end
  else
  begin
    IDomainFormula := Format('IfI(Layer > %d, 1, %s)', [NumberOfLayers, IDomainFormula]);
  end;
  DataArray := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  DataArray.Formula := IDomainFormula;

  ActiveFormula := 'CaseB(Layer';
  SetLength(Active, Model.RowCount, Model.ColumnCount);
  for LayerIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Active[RowIndex, ColIndex] := IDOMAIN[LayerIndex - 1, RowIndex, ColIndex] <> 0;
      end;
    end;
    Uniform := True;
    FirstBoolValue := Active[0, 0];
    for RowIndex := 0 to Model.RowCount - 1 do
    begin
      for ColIndex := 0 to Model.ColumnCount - 1 do
      begin
        Uniform := FirstBoolValue = Active[RowIndex, ColIndex];
        if not Uniform then
        begin
          break;
        end;
      end;
    end;
    DataArrayName := Format('Imported_Active_%d', [LayerIndex]);
    ActiveFormula := ActiveFormula + ',' + DataArrayName;
    DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray, 
      DataArrayName, 'True', DataArrayName, [dcType], rdtBoolean, eaBlocks, 
      dsoTop, '');
    DataArray.Comment := Format('Imported from %s on %s', 
      [FModelNameFile, DateTimeToStr(Now)]);
    DataArray.UpdateDimensions(Model.LayerCount, Model.RowCount, Model.ColumnCount);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
    if Uniform then
    begin
      if FirstBoolValue then
      begin
        DataArray.Formula := 'True';
      end
      else
      begin
        DataArray.Formula := 'False';
      end;
    end
    else
    begin
      ScreenObject := AllTopCellsScreenObject;
      AssignBooleanValuesToCellCenters(DataArray, ScreenObject, Active);
    end;
  end;
  ActiveFormula := ActiveFormula + ')';
  if NumberOfLayers = 1 then
  begin
    DataArrayName := Format('Imported_Active_%d', [1]);
    ActiveFormula := DataArrayName;
  end
  else
  begin
    ActiveFormula := Format('IfB(Layer > %d, True, %s)', [NumberOfLayers, ActiveFormula]);
  end;
  DataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  DataArray.Formula := ActiveFormula;
end;

procedure TModflow6Importer.UpdateLayerStructure(NumberOfLayers: Integer);
var
  Model: TPhastModel;
  TopLayer: TLayerGroup;
  LayerIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  Model := frmGoPhast.PhastModel;
  Model.LayerStructure.BeginUpdate;
  try
    TopLayer := Model.LayerStructure.Add;
    TopLayer.AquiferName := kModelTop;
    for LayerIndex := 1 to NumberOfLayers do
    begin
      LayerGroup := Model.LayerStructure.Add;
      LayerGroup.AquiferName := Format('Layer %d', [LayerIndex]);
    end;
    Model.ModflowGrid.LayerCount := NumberOfLayers;
    Model.DisvGrid.LayerCount := NumberOfLayers;
  finally
    Model.LayerStructure.EndUpdate;
  end;
end;

type
  TSfrReachInfo = class(TObject)
  private
    PackageData: TSfrPackageItem;
    CrossSectionFile: string;
    Connections: TSfrConnectionItem;
    Diversions: TSfrDiversionItemList;
    BoundNameObs: TObservationList;
    IdObs: TObservationList;
    IsDiversion: Boolean;
    Added: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function UpstreamReachCount: Integer;
    function DownstreamReachCount: Integer;
    function Compatible(OtherInfo: TSfrReachInfo): Boolean;
  end;

  TSfrReachInfoList = class(TList<TSfrReachInfo>)
    Terminated: Boolean;
  end;
  TSfrReachInfoLists = TObjectList<TSfrReachInfoList>;
  TSfrReachInfoObjectList = TObjectList<TSfrReachInfo>;

  TSfrMvrLink = record
    SfrPeriod: TSfrPeriod;
    MvrPeriod: TMvrPeriod;
    function Period: Integer;
  end;
  TSfrMvrLinkArray = TArray<TSfrMvrLink>;
  TSfrMvrLinkList = TList<TSfrMvrLink>;

  function TModflow6Importer.RealValuesToFormula(Values: TOneDRealArray; Name: string;
    ScreenObject: TScreenObject = nil): string;
  var
    FirstValue: Double;
    Uniform: Boolean;
    Index: Integer;
    ImportedData: TValueArrayItem;
  begin
    Assert(Length(Values) > 0);
    if Length(Values) = 1 then
    begin
      result := FortranFloatToStr(Values[0]);
    end
    else
    begin
      Uniform := True;
      FirstValue := Values[0];
      for Index := 1 to Length(Values) - 1 do
      begin
        Uniform := Values[Index] = FirstValue;
        if not Uniform then
        begin
          break;
        end;
      end;
      if Uniform then
      begin
        result := FortranFloatToStr(Values[0]);
      end
      else
      begin
        if ScreenObject <> nil then
        begin
          ImportedData := ScreenObject.ImportedValues.Add;
          ImportedData.Name := Name;
          ImportedData.Values.DataType := rdtDouble;
          ImportedData.Values.Count := Length(Values);
          for Index := 0 to Length(Values) - 1 do
          begin
            ImportedData.Values.RealValues[Index] := Values[Index];
          end;
        end;
        Result := rsObjectImportedValuesR + '("' + Name + '")';
      end;
    end;
  end;
  function TModflow6Importer.BoundaryValuesToFormula(
    Values: TMf6BoundaryValueArray; Name: string;
    Map: TimeSeriesMap; ScreenObject: TScreenObject = nil): string;
  var
    Index: Integer;
    RealValues: TOneDRealArray;
    UseRealFormula: Boolean;
    UseTimeSeries: Boolean;
    ImportedTimeSeries: String;
  begin
  // If the values are all numeric, this function provides a formula for that.
  // If the values all represent the same TimeSeries,
  //    this function returns the name of the TimeSeries.
  // If the values represent a mixture of real values and TimeSeries names,
  //    or a mixture of different TimeSeries names,
  //    this function returns an empty string indicating that the points can
  //    not all belong to the same object.
    result := '';
    SetLength(RealValues, Length(Values));
    UseRealFormula := True;
    for Index := 0 to Length(Values) - 1 do
    begin
      if Values[Index].ValueType = vtNumeric then
      begin
        RealValues[Index] := Values[Index].NumericValue
      end
      else
      begin
        UseRealFormula := False;
        break;
      end;
    end;
    if UseRealFormula then
    begin
      result := RealValuesToFormula(RealValues, Name, ScreenObject);
    end
    else
    begin
      UseTimeSeries := True;
      for Index := 0 to Length(Values) - 1 do
      begin
        if (Values[Index].ValueType <> vtString)
          or (Values[Index].StringValue <> Values[0].StringValue) then
        begin
          UseTimeSeries := False;
          break;
        end;
      end;
      if UseTimeSeries then
      begin
        if not Map.TryGetValue(UpperCase(Values[0].StringValue), ImportedTimeSeries) then
        begin
          Assert(False);
        end;
        result := ImportedTimeSeries;
      end
    end;
  end;

procedure TModflow6Importer.ImportSfr(Package: TPackage;
  TransportModels: TModelList; MvrPackage: TPackage);
var
  Model: TPhastModel;
  SfrPackage: TSfrModflow6PackageSelection;
  Sfr: TSfr;
  CellIds: TCellIdList;
  Map: TimeSeriesMap;
  BoundNameObsDictionary: TBoundNameDictionary;
  NumberObsDictionary: TNumberDictionary;
  ObsLists: TObsLists;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  Options: TSfrOptions;
  SfrReachInfoList: TSfrReachInfoObjectList;
  PackageData: TSfrPackageData;
  SfrReachInfo: TSfrReachInfo;
  CrossSections: TSfrCrossSections;
  Index: Integer;
  CrossSectionItem: TCrossSectionItem;
  Connections: TSfrConnections;
  ConnectInfo: TSfrConnectionItem;
  Diversions: TSfrDiversions;
  ADiversion: TSfrDiversionItem;
  AReachList: TSfrReachInfoList;
  ReachListDictionary: TDictionary<Integer, TSfrReachInfoList>;
  SfrReachInfoLists: TSfrReachInfoLists;
  FirstItem: TSfrReachInfo;
  ReachesAdded: Boolean;
  UpstreamReach: Integer;
  UpstreamIndex: Integer;
  ObjectIndex: Integer;
  ReachIndex: Integer;
  ScreenObjectDictionary: TDictionary<Integer, TScreenObject>;
  AScreenObject: TScreenObject;
  ObjectCount: Integer;
  CellIndex: Integer;
  BoundaryValues: TMf6BoundaryValueArray;
  StringValues: TOneDStringArray;
  DefaultFormula: string;
  PeriodIndex: Integer;
  APeriod: TSfrPeriod;
  SettingIndex: Integer;
  ASetting: TNumberedItem;
  ASettingList: TNumberedItemLists;
  PeriodSettings: TObjectList<TNumberedItemLists>;
  AReachSettingsList: TNumberedItemList;
  SplitReachLists: TObjectList<TSfrReachInfoList>;
  AuxIndex: Integer;
  NeedToSplit: Boolean;
  ManningBoundaryValues: TMf6BoundaryValueArray;
  UpstreamFractionBoundaryValues: TMf6BoundaryValueArray;
  StageBoundaryValues: TMf6BoundaryValueArray;
  InflowBoundaryValues: TMf6BoundaryValueArray;
  RainfallBoundaryValues: TMf6BoundaryValueArray;
  EvaporationBoundaryValues: TMf6BoundaryValueArray;
  RunoffBoundaryValues: TMf6BoundaryValueArray;
  LastReach: TSfrReachInfo;
  DownstreamIndex: Integer;
  ReachNo: Integer;
  DownStreamSegment: TScreenObject;
  IntItem: TIntegerItem;
  DiversionIndex: Integer;
  DiversionSegment: TScreenObject;
  SfrDiversions: TDiversionCollection;
  DiversionItem: TSDiversionItem;
  DiversionFormula: string;
  SfrItem: TSfrMf6Item;
  IFaceIndex: Integer;
  AuxName: string;
  AuxArrays: TMf6BoundaryValueArrays;
  AReach: TSfrReachInfo;
  Mvr: TMvr;
  SfrMvrLinkArray: TSfrMvrLinkArray;
  SfrMvrLinkList: TSfrMvrLinkList;
  SfrMvrLink: TSfrMvrLink;
  StressPeriodIndex: Integer;
  MvrPeriod: TMvrPeriod;
  SfrPeriod: TSfrPeriod;
  SfrSources: TIntegerList;
  SfrReceivers: TIntegerList;
  ObsNameIndex: Integer;
  procedure CreateReachList(SfrReachInfo: TSfrReachInfo);
  begin
    AReachList := TSfrReachInfoList.Create;
    SfrReachInfoLists.Add(AReachList);
    AReachList.Add(SfrReachInfo);
    SfrReachInfo.Added := True;
    ReachListDictionary.Add(SfrReachInfo.PackageData.rno, AReachList);
    if (SfrReachInfo.DownstreamReachCount <> 1)
      or (SfrReachInfo.Diversions.Count > 0)
      or (SfrReachInfo.IdObs <> nil) then
    begin
      AReachList.Terminated := True;
    end;
  end;
  procedure UpdateReachSettings(AReachList: TSfrReachInfoList;
    var BoundaryValues: TMf6BoundaryValueArray; const Key: string; AuxName: string = '');
  var
    ReachIndex: Integer;
    AReachSettingsList: TNumberedItemList;
    SettingIndex: Integer;
    ASetting: TNumberedItem;
    AMf6BoundaryValue: TMf6BoundaryValue;
  begin
    for ReachIndex := 0 to AReachList.Count - 1 do
    begin
      AReachSettingsList := ASettingList[AReachList[ReachIndex].PackageData.rno-1];
      for SettingIndex := 0 to AReachSettingsList.Count - 1 do
      begin
        ASetting := AReachSettingsList[SettingIndex];
        if AnsiSameText(ASetting.Name, Key) then
        begin
          if (AuxName = '') or AnsiSameText(ASetting.AuxName, AuxName)  then
          begin
            AMf6BoundaryValue.StringValue := ASetting.StringValue;
            AMf6BoundaryValue.NumericValue := ASetting.FloatValue;
            if AMf6BoundaryValue.StringValue <> '' then
            begin
              AMf6BoundaryValue.ValueType := vtString;
            end
            else
            begin
              AMf6BoundaryValue.ValueType := vtNumeric;
            end;
            BoundaryValues[ReachIndex]  := AMf6BoundaryValue;
            break;
          end;
        end;
      end;
    end;
  end;
  function CreateScreenObject(AReachList: TSfrReachInfoList): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    CellIds: TCellIdList;
    CellIndex: Integer;
    SfrBoundary: TSfrMf6Boundary;
    Values: TOneDRealArray;
    PeriodIndex: Integer;
    SfrItem: TSfrMf6Item;
    SettingIndex: Integer;
    ASetting: TNumberedItem;
    ACrossSectionPackage: TPackage;
    CrossSection: TCrossSection;
    CSItem: TimeVaryingSfr6CrossSectionItem;
    ACrossSection: TSfr6CrossSection;
    CrossSectionFileName: string;
    StartTime: Double;
    FirstReach: TSfrReachInfo;
    BoundName: string;
    ObsList: TObservationList;
    SfrObs: TSfrObs;
    AuxIFACE: TMf6BoundaryValue;
    IFACE: Integer;
    FirstReachNo: Integer;
    LastReachNo: Integer;
    MvrSource: TMvrSource;
    MvrReceiver: TMvrReceiver;
    StressPeriodIndex: Integer;
    procedure ReadCrossSection(ACrossSection: TSfr6CrossSection; CrossSection: TCrossSection);
    var
      RowIndex: Integer;
      XsecPoint: TSfr6CrossSectionPoint;
      TableRow: TCrossSectionTableItem;
    begin
      ACrossSection.UseManningFraction := (CrossSection.Dimensions.NCOL = 3);

      for RowIndex := 0 to CrossSection.Table.Count - 1 do
      begin
        TableRow := CrossSection.Table[RowIndex];
        XsecPoint := ACrossSection.Add;

        XsecPoint.XFraction := TableRow.xfraction;
        XsecPoint.Height := TableRow.height;
        if ACrossSection.UseManningFraction then
        begin
          if TableRow.manfraction.Used then
          begin
            XsecPoint.ManningsFraction := TableRow.manfraction.Value;
          end
          else
          begin
            XsecPoint.ManningsFraction := 0;
          end;
        end;
      end
    end;
    procedure IncludeObservations(ObsList: TObservationList);
    var
      ObsIndex: Integer;
      AnObs: TObservation;
    begin
      for ObsIndex := 0 to ObsList.Count - 1 do
      begin
        AnObs := ObsList[ObsIndex];
        if AnsiSameText(AnObs.ObsType, 'stage') then
        begin
          Include(SfrObs, soStage);
        end
        else if AnsiSameText(AnObs.ObsType, 'ext-inflow') then
        begin
          Include(SfrObs, soExtInflow);
        end
        else if AnsiSameText(AnObs.ObsType, 'inflow') then
        begin
          Include(SfrObs, soInflow);
        end
        else if AnsiSameText(AnObs.ObsType, 'from-mvr') then
        begin
          Include(SfrObs, soFromMvr);
        end
        else if AnsiSameText(AnObs.ObsType, 'rainfall') then
        begin
          Include(SfrObs, soRainfall);
        end
        else if AnsiSameText(AnObs.ObsType, 'runoff') then
        begin
          Include(SfrObs, soRunoff);
        end
        else if AnsiSameText(AnObs.ObsType, 'sfr') then
        begin
          Include(SfrObs, soSfr);
        end
        else if AnsiSameText(AnObs.ObsType, 'evaporation') then
        begin
          Include(SfrObs, soEvaporation);
        end
        else if AnsiSameText(AnObs.ObsType, 'outflow') then
        begin
          Include(SfrObs, soOutflow);
        end
        else if AnsiSameText(AnObs.ObsType, 'ext-outflow') then
        begin
          Include(SfrObs, soExternalOutflow);
        end
        else if AnsiSameText(AnObs.ObsType, 'to-mvr') then
        begin
          Include(SfrObs, soToMvr);
        end
        else if AnsiSameText(AnObs.ObsType, 'upstream-flow') then
        begin
          Include(SfrObs, soUpstreamFlow);
        end
        else if AnsiSameText(AnObs.ObsType, 'downstream-flow') then
        begin
          Include(SfrObs, soDownstreamFlow);
        end
        else if AnsiSameText(AnObs.ObsType, 'depth') then
        begin
          Include(SfrObs, soDepth);
        end
        else if AnsiSameText(AnObs.ObsType, 'wet-perimeter') then
        begin
          Include(SfrObs, soWetPerimeter);
        end
        else if AnsiSameText(AnObs.ObsType, 'wet-area') then
        begin
          Include(SfrObs, soWetArea);
        end
        else if AnsiSameText(AnObs.ObsType, 'wet-width') then
        begin
          Include(SfrObs, soWetWidth);
        end
        else
        begin
          Assert(False);
        end;
//
//          TSftOb = (stoConcentration, stoStorage, stoConstant, stoFromMvr, stoToMvr,
//            stoSFT, stoRainfall, stoEvaporation, stoRunoff, stoExtInflow, stoExtOutflow);
//          TSftObs = set of TSftOb;
      end;
    end;
  begin
    MvrSource.LakeOutlet := nil;
    Inc(ObjectCount);

    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Sfr_%d',
      [Package.PackageName, ObjectCount]));
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    CellIds:= TCellIdList.Create;
    try
      for CellIndex := 0 to AReachList.Count - 1 do
      begin
        CellIds.Add(AReachList[CellIndex].PackageData.cellid);
      end;
      AddPointsToScreenObject(CellIds, result);
    finally
      CellIds.Free;
    end;

    FirstReachNo := AReachList.First.PackageData.rno;
    LastReachNo := AReachList.Last.PackageData.rno;
    if SfrSources.IndexOf(LastReachNo) >= 0 then
    begin
      MvrSource.ScreenObject := result;
      MvrSource.PackageName := Package.PackageName;
      SetLength(MvrSource.IDs, 1);
      MvrSource.IDs[0] := LastReachNo;
      MvrSource.SourceType := mspcSfr;
      for StressPeriodIndex := 0 to SfrMvrLinkList.Count - 1 do
      begin
        MvrSource.Period := SfrMvrLinkList[StressPeriodIndex].Period;
        FMvrSources.Add(MvrSource);
      end;
    end;
    if SfrReceivers.IndexOf(FirstReachNo) >= 0 then
    begin
      MvrReceiver.ScreenObject := result;
      MvrReceiver.PackageName := Package.PackageName;
      SetLength(MvrReceiver.IDs, 1);
      MvrReceiver.IDs[0] := FirstReachNo;
      MvrReceiver.ReceiverType := mrpcSfr;
      for StressPeriodIndex := 0 to SfrMvrLinkList.Count - 1 do
      begin
        MvrReceiver.Period := SfrMvrLinkList[StressPeriodIndex].Period;
        FMvrReceivers.Add(MvrReceiver);
      end;
    end;

    result.CreateSfr6Boundary;
    SfrBoundary := result.ModflowSfr6Boundary;
    SfrBoundary.SegmentNumber := ObjectCount;
    SetLength(Values, AReachList.Count);

    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      Values[CellIndex] := AReachList[CellIndex].PackageData.rlen;
    end;
    SfrBoundary.ReachLength := RealValuesToFormula(Values, 'ReachLength', result);

    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      Values[CellIndex] := AReachList[CellIndex].PackageData.rwid;
    end;
    SfrBoundary.ReachWidth := RealValuesToFormula(Values, 'ReachWidth', result);

    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      Values[CellIndex] := AReachList[CellIndex].PackageData.rgrd;
    end;
    SfrBoundary.Gradient := RealValuesToFormula(Values, 'ReachGradient', result);

    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      Values[CellIndex] := AReachList[CellIndex].PackageData.rtp;
    end;
    SfrBoundary.StreambedTop := RealValuesToFormula(Values, 'ReachTop', result);

    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      Values[CellIndex] := AReachList[CellIndex].PackageData.rbth;
    end;
    SfrBoundary.StreambedThickness := RealValuesToFormula(Values, 'StreambedThickness', result);

    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      Values[CellIndex] := AReachList[CellIndex].PackageData.rhk;
    end;
    SfrBoundary.HydraulicConductivity := RealValuesToFormula(Values, 'ReachK', result);

    CrossSectionFileName := '';
    if AReachList[0].CrossSectionFile <> '' then
    begin
      CrossSectionFileName := AReachList[0].CrossSectionFile;
      ACrossSectionPackage := Sfr.GetCrossSectionPackage(AReachList[0].CrossSectionFile);
      CrossSection := ACrossSectionPackage.Package as TCrossSection;

      CSItem := SfrBoundary.CrossSections.Add as TimeVaryingSfr6CrossSectionItem;
      CSItem.StartTime := frmGoPhast.PhastModel.ModflowStressPeriods.First.StartTime;
      CSItem.EndTime := frmGoPhast.PhastModel.ModflowStressPeriods.Last.EndTime;
      ACrossSection := CSItem.CrossSection;

      ReadCrossSection(ACrossSection, CrossSection);
    end;

    SetLength(ManningBoundaryValues, AReachList.Count);
    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      ManningBoundaryValues[CellIndex] := AReachList[CellIndex].PackageData.man;
    end;

    SetLength(UpstreamFractionBoundaryValues, AReachList.Count);
    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      UpstreamFractionBoundaryValues[CellIndex] := AReachList[CellIndex].PackageData.ustrf;
    end;

    SetLength(StageBoundaryValues, AReachList.Count);
    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      StageBoundaryValues[CellIndex].ValueType := vtNumeric;
      StageBoundaryValues[CellIndex].NumericValue := AReachList[CellIndex].PackageData.rtp;
    end;

    SetLength(InflowBoundaryValues, AReachList.Count);
    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      InflowBoundaryValues[CellIndex].ValueType := vtNumeric;
      InflowBoundaryValues[CellIndex].NumericValue := 0;
    end;

    SetLength(RainfallBoundaryValues, AReachList.Count);
    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      RainfallBoundaryValues[CellIndex].ValueType := vtNumeric;
      RainfallBoundaryValues[CellIndex].NumericValue := 0;
    end;

    SetLength(EvaporationBoundaryValues, AReachList.Count);
    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      EvaporationBoundaryValues[CellIndex].ValueType := vtNumeric;
      EvaporationBoundaryValues[CellIndex].NumericValue := 0;
    end;

    SetLength(RunoffBoundaryValues, AReachList.Count);
    for CellIndex := 0 to AReachList.Count - 1 do
    begin
      RunoffBoundaryValues[CellIndex].ValueType := vtNumeric;
      RunoffBoundaryValues[CellIndex].NumericValue := 0;
    end;

    for PeriodIndex := 0 to SfrMvrLinkList.Count - 1 do
    begin
      SfrMvrLink := SfrMvrLinkList[PeriodIndex];
      APeriod := SfrMvrLink.SfrPeriod;
      ASettingList := PeriodSettings[PeriodIndex];

      SfrItem := SfrBoundary.Values.Add as TSfrMf6Item;

      StartTime := Model.ModflowStressPeriods[SfrMvrLink.Period-1].StartTime;
      SfrItem.StartTime := StartTime;
      if PeriodIndex < SfrMvrLinkList.Count - 1 then
      begin
        SfrItem.EndTime := Model.ModflowStressPeriods[SfrMvrLink.Period].StartTime;
      end
      else
      begin
        SfrItem.EndTime := Model.ModflowStressPeriods.Last.EndTime;
      end;

      SfrItem.StreamStatus := ssActive;
      AReachSettingsList := ASettingList[AReachList[0].PackageData.rno-1];
      for SettingIndex := 0 to AReachSettingsList.Count - 1 do
      begin
        ASetting := AReachSettingsList[SettingIndex];
        if AnsiSameText(ASetting.Name, 'STATUS') then
        begin
          if AnsiSameText(ASetting.StringValue, 'INACTIVE') then
          begin
            SfrItem.StreamStatus := ssInactive;
          end
          else if AnsiSameText(ASetting.StringValue, 'SIMPLE') then
          begin
            SfrItem.StreamStatus := ssSimple;
          end
          else
          begin
            Assert(AnsiSameText(ASetting.StringValue, 'ACTIVE'));
          end;
          Break;
        end;
      end;

      for SettingIndex := 0 to AReachSettingsList.Count - 1 do
      begin
        ASetting := AReachSettingsList[SettingIndex];
        if AnsiSameText(ASetting.Name, 'CROSS_SECTION') then
        begin
          if not AnsiSameText(ASetting.StringValue, CrossSectionFileName) then
          begin
            CrossSectionFileName := ASetting.StringValue;
            CrossSection := Sfr.GetCrossSectionPackage(CrossSectionFileName).Package as TCrossSection;
            if SfrBoundary.CrossSections.Count > 0 then
            begin
              CsItem := SfrBoundary.CrossSections.Last as TimeVaryingSfr6CrossSectionItem;
              CsItem.EndTime := StartTime;
            end;
            CsItem := SfrBoundary.CrossSections.Add as TimeVaryingSfr6CrossSectionItem;
            CsItem.StartTime := StartTime;
            CsItem.EndTime := Model.ModflowStressPeriods.Last.EndTime;

            ReadCrossSection(CsItem.CrossSection, CrossSection);
          end;
        end;
      end;

      UpdateReachSettings(AReachList, ManningBoundaryValues, 'MANNING');
      SfrItem.Roughness := BoundaryValuesToFormula(ManningBoundaryValues,
        Format('Roughness_%d', [SfrMvrLink.Period]), Map, result);

      UpdateReachSettings(AReachList, UpstreamFractionBoundaryValues, 'UPSTREAM_FRACTION');
      SfrItem.UpstreamFraction := BoundaryValuesToFormula(UpstreamFractionBoundaryValues,
        Format('UpstreamFraction_%d', [SfrMvrLink.Period]), Map, result);

      UpdateReachSettings(AReachList, StageBoundaryValues, 'STAGE');
      SfrItem.Stage := BoundaryValuesToFormula(StageBoundaryValues,
        Format('Stage_%d', [SfrMvrLink.Period]), Map, result);

      UpdateReachSettings(AReachList, InflowBoundaryValues, 'INFLOW');
      SfrItem.Inflow := BoundaryValuesToFormula(InflowBoundaryValues,
        Format('Inflow_%d', [SfrMvrLink.Period]), Map, result);

      UpdateReachSettings(AReachList, RainfallBoundaryValues, 'RAINFALL');
      SfrItem.Rainfall := BoundaryValuesToFormula(RainfallBoundaryValues,
        Format('Inflow_%d', [SfrMvrLink.Period]), Map, result);

      UpdateReachSettings(AReachList, EvaporationBoundaryValues, 'EVAPORATION');
      SfrItem.Evaporation := BoundaryValuesToFormula(EvaporationBoundaryValues,
        Format('Evaporation_%d', [SfrMvrLink.Period]), Map, result);

      UpdateReachSettings(AReachList, RunoffBoundaryValues, 'RUNOFF');
      SfrItem.Runoff := BoundaryValuesToFormula(RunoffBoundaryValues,
        Format('Runoff_%d', [SfrMvrLink.Period]), Map, result);
    end;

    if SfrBoundary.CrossSections.Count > 0 then
    begin
      if SfrBoundary.CrossSections.Count = 1 then
      begin
        SfrBoundary.CrossSectionUsage := csuSingle;
      end
      else
      begin
        SfrBoundary.CrossSectionUsage := csuMultiple;
      end;
    end;

    FirstReach := AReachList.First;
    BoundName := FirstReach.PackageData.Boundname;
    SfrObs := [];
    if BoundName <> '' then
    begin
      if BoundNameObsDictionary.TryGetValue(UpperCase(Boundname), ObsList) then
      begin
        Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
        result.CreateMf6Obs;
        IncludeObservations(ObsList);
        result.Modflow6Obs.Name := ValidName(Boundname);
      end;
    end;
    if NumberObsDictionary.TryGetValue(FirstReach.PackageData.rno, ObsList) then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      result.CreateMf6Obs;
      IncludeObservations(ObsList);
      if result.Modflow6Obs.Name = '' then
      begin
        Inc(ObsNameIndex);
        result.Modflow6Obs.Name := 'SFR_' + IntToStr(ObsNameIndex);
      end;
    end;
    if SfrObs <> [] then
    begin
      result.Modflow6Obs.SfrObs := SfrObs;
    end;

    if IfaceIndex >= 0 then
    begin
      AuxIFACE := FirstReach.PackageData.Aux[IfaceIndex];
      Assert(AuxIFACE.ValueType = vtNumeric);
      IFACE := Round(AuxIFACE.NumericValue);
    end
    else
    begin
      IFACE := 0;
    end;
    result.IFACE := TIface(IFACE+2);
  end;
  procedure SplitReachListWithBoundaryValues(var AReachList: TSfrReachInfoList;
    BoundaryValues: TMf6BoundaryValueArray);
  var
    NewReachList: TSfrReachInfoList;
    Index: Integer;
    SplitIndex: Integer;
    TempList: TSfrReachInfoLists;
  begin
    TempList := TSfrReachInfoLists.Create;
    try
      NewReachList := TSfrReachInfoList.Create;
      TempList.Add(NewReachList);
      NewReachList.Add(AReachList[0]);
      for Index := 1 to AReachList.Count - 1 do
      begin
        if BoundaryValues[Index].ValueType = BoundaryValues[Index-1].ValueType then
        begin
          if (BoundaryValues[Index].ValueType = vtString) then
          begin
            if (BoundaryValues[Index].StringValue = BoundaryValues[Index-1].StringValue) then
            begin
              NewReachList.Add(AReachList[Index]);
            end
            else
            begin
              NewReachList := TSfrReachInfoList.Create;
              TempList.Add(NewReachList);
              NewReachList.Add(AReachList[Index]);
            end;
          end
          else
          begin
            NewReachList.Add(AReachList[Index]);
          end;
        end
        else
        begin
          NewReachList := TSfrReachInfoList.Create;
          TempList.Add(NewReachList);
          NewReachList.Add(AReachList[Index]);
        end;
      end;

      {
      SfrReachInfoLists[ObjectIndex] := SplitReachLists[0];
      for SplitIndex := 1 to SplitReachLists.Count - 1 do
      begin
        SfrReachInfoLists.Add(SplitReachLists[SplitIndex]);
      end;
      }
    finally
      TempList.Free;
    end;

    AReachList := SfrReachInfoLists[ObjectIndex];
  end;
  procedure SplitReachListWithStrings(var AReachList: TSfrReachInfoList;
    StringValues: TOneDStringArray);
  var
    NewReachList: TSfrReachInfoList;
    Index: Integer;
    SplitIndex: Integer;
    TempList: TSfrReachInfoLists;
  begin
    TempList := TSfrReachInfoLists.Create;
    try
      NewReachList := TSfrReachInfoList.Create;
      TempList.Add(NewReachList);
      NewReachList.Add(AReachList[0]);
      for Index := 1 to AReachList.Count - 1 do
      begin
        if AnsiSameText(StringValues[Index], StringValues[Index-1]) then
        begin
          NewReachList.Add(AReachList[Index]);
        end
        else
        begin
          NewReachList := TSfrReachInfoList.Create;
          TempList.Add(NewReachList);
          NewReachList.Add(AReachList[Index]);
        end;
      end;

      {
      SfrReachInfoLists[ObjectIndex] := SplitReachLists[0];
      for SplitIndex := 1 to SplitReachLists.Count - 1 do
      begin
        SfrReachInfoLists.Add(SplitReachLists[SplitIndex]);
      end;
      }
    finally
      TempList.Free;
    end;

    AReachList := SfrReachInfoLists[ObjectIndex];
  end;
  procedure IdentifySourcesAndReceivers(MvrPeriod: TMvrPeriod);
  var
    ItemIndex: Integer;
    PackageName: string;
    MvrItem: TMvrPeriodItem;
  begin
    PackageName := Package.PackageName;
    for ItemIndex := 0 to MvrPeriod.Count - 1 do
    begin
      MvrItem := MvrPeriod[ItemIndex];
      if AnsiSameText(MvrItem.pname1, PackageName) then
      begin
        SfrSources.AddUnique(MvrItem.id1)
      end;
      if AnsiSameText(MvrItem.pname2, PackageName) then
      begin
        SfrReceivers.AddUnique(MvrItem.id2)
      end;
    end;
  end;
begin
  SplitReachLists := nil;
  ObsNameIndex := 0;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing SFR package');
  end;

  Model := frmGoPhast.PhastModel;
  SfrPackage := Model.ModflowPackages.SfrModflow6Package;
  SfrPackage.IsSelected := True;

  Sfr := Package.Package as TSfr;
  Options := Sfr.Options;
  IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
  SetLength(AuxArrays, Options.Count);

  if Options.MAXIMUM_ITERATIONS.Used then
  begin
    SfrPackage.MaxIteration := Options.MAXIMUM_ITERATIONS.Value;
  end;

  if Options.MAXIMUM_PICARD_ITERATIONS.Used then
  begin
    SfrPackage.MaxPicardIteration := Options.MAXIMUM_PICARD_ITERATIONS.Value;
  end;

  if Options.MAXIMUM_DEPTH_CHANGE.Used then
  begin
    SfrPackage.MaxDepthChange := Options.MAXIMUM_DEPTH_CHANGE.Value;
  end;

  SfrPackage.SaveStageFile := Options.STAGE;
  SfrPackage.SaveBudgetFile := Options.BUDGET;
  SfrPackage.PrintStage := Options.PRINT_STAGE;
  SfrPackage.PrintFlows := Options.PRINT_FLOWS;

  if Options.BUDGETCSV then
  begin
    SfrPackage.SaveGwtBudgetCsv := True;
  end;

  SfrPackage.WriteConvergenceData := Options.PACKAGE_CONVERGENCE;

  Model.DataArrayManager.CreateInitialDataSets;

  Mvr := GetMvr(MvrPackage, Package);

  SfrMvrLinkList := TSfrMvrLinkList.Create;
  CellIds := TCellIdList.Create;
  Map := TimeSeriesMap.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  NumberObsDictionary := TNumberDictionary.Create;
  ObsLists := TObsLists.Create;
  SfrSources := TIntegerList.Create;
  SfrReceivers := TIntegerList.Create;
  try
    if Mvr = nil then
    begin
      SfrMvrLink.MvrPeriod := nil;
      for StressPeriodIndex := 0 to Sfr.PeriodCount - 1 do
      begin
        SfrMvrLink.SfrPeriod := Sfr.Periods[StressPeriodIndex];
        SfrMvrLinkList.Add(SfrMvrLink);
      end;
    end
    else
    begin
      SetLength(SfrMvrLinkArray, Model.ModflowStressPeriods.Count);
      for StressPeriodIndex := 0 to Length(SfrMvrLinkArray) - 1 do
      begin
        SfrMvrLinkArray[StressPeriodIndex].MvrPeriod := nil;
        SfrMvrLinkArray[StressPeriodIndex].SfrPeriod := nil;
      end;
      for StressPeriodIndex := 0 to Mvr.PeriodCount - 1 do
      begin
        MvrPeriod := Mvr.Periods[StressPeriodIndex];
        SfrMvrLinkArray[MvrPeriod.Period-1].MvrPeriod := MvrPeriod;
        IdentifySourcesAndReceivers(MvrPeriod);
      end;
      for StressPeriodIndex := 0 to Sfr.PeriodCount - 1 do
      begin
        SfrPeriod := Sfr.Periods[StressPeriodIndex];
        SfrMvrLinkArray[SfrPeriod.Period-1].SfrPeriod := SfrPeriod;
      end;

      for StressPeriodIndex := 1 to Length(SfrMvrLinkArray) - 1 do
      begin
        if SfrMvrLinkArray[StressPeriodIndex].MvrPeriod = nil then
        begin
          SfrMvrLinkArray[StressPeriodIndex].MvrPeriod := SfrMvrLinkArray[StressPeriodIndex-1].MvrPeriod;
          SfrMvrLinkArray[StressPeriodIndex].SfrPeriod := SfrMvrLinkArray[StressPeriodIndex-1].SfrPeriod;
        end;
      end;

      for StressPeriodIndex := 0 to Length(SfrMvrLinkArray) - 1 do
      begin
        if (SfrMvrLinkArray[StressPeriodIndex].MvrPeriod = nil)
          and (SfrMvrLinkArray[StressPeriodIndex].SfrPeriod = nil) then
        begin
          Continue;
        end;

        if StressPeriodIndex > 0 then
        begin
          if (SfrMvrLinkArray[StressPeriodIndex].MvrPeriod = SfrMvrLinkArray[StressPeriodIndex - 1].MvrPeriod)
            and (SfrMvrLinkArray[StressPeriodIndex].SfrPeriod = SfrMvrLinkArray[StressPeriodIndex - 1].SfrPeriod) then
          begin
            Continue
          end;
        end;

        SfrMvrLinkList.Add(SfrMvrLinkArray[StressPeriodIndex]);
      end;

    end;

    for TimeSeriesIndex := 0 to Sfr.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := Sfr.TimeSeries[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;

    if Sfr.ObservationCount > 0 then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    end;
    for ObsPackageIndex := 0 to Sfr.ObservationCount - 1 do
    begin
      ObsFiles := Sfr.Observations[ObsPackageIndex].Package as TObs;
      GetObservations(NumberObsDictionary, BoundNameObsDictionary,
        nil, ObsLists, ObsFiles);
    end;

    if Assigned(OnUpdateStatusBar) then
    begin
      OnUpdateStatusBar(self, 'importing SFR package');
    end;

    SfrReachInfoList := TSfrReachInfoObjectList.Create;
    try
      // read data for each reach.

      PackageData := Sfr.PackageData;
      SfrReachInfoList.Capacity := PackageData.Count;
      for Index := 0 to PackageData.Count - 1 do
      begin
        SfrReachInfo := TSfrReachInfo.Create;
        SfrReachInfoList.Add(SfrReachInfo);
        SfrReachInfo.PackageData := PackageData[Index];

        if SfrReachInfo.PackageData.Boundname <> '' then
        begin
          if not BoundNameObsDictionary.TryGetValue(
            UpperCase(SfrReachInfo.PackageData.Boundname),
            SfrReachInfo.BoundNameObs) then
          begin
            SfrReachInfo.BoundNameObs := nil
          end;
        end;

        if not NumberObsDictionary.TryGetValue(SfrReachInfo.PackageData.rno,
          SfrReachInfo.IdObs) then
        begin
          SfrReachInfo.IdObs := nil;
        end;
      end;

      CrossSections := Sfr.CrossSections;
      for Index := 0 to CrossSections.Count - 1 do
      begin
        CrossSectionItem := CrossSections[Index];
        SfrReachInfo := SfrReachInfoList[CrossSectionItem.rno-1];
        Assert(SfrReachInfo.PackageData.rno = CrossSectionItem.rno);
        SfrReachInfo.CrossSectionFile := CrossSectionItem.tab6_filename;
      end;

      Connections := Sfr.Connections;
      for Index := 0 to Connections.Count - 1 do
      begin
        ConnectInfo := Connections[Index];
        SfrReachInfo := SfrReachInfoList[ConnectInfo.rno-1];
        Assert(SfrReachInfo.PackageData.rno = ConnectInfo.rno);
        SfrReachInfo.Connections := ConnectInfo;
      end;

      Diversions := Sfr.Diversions;
      for Index := 0 to Diversions.Count - 1 do
      begin
        ADiversion := Diversions[Index];
        SfrReachInfo := SfrReachInfoList[ADiversion.rno-1];
        Assert(SfrReachInfo.PackageData.rno = ADiversion.rno);
        SfrReachInfo.Diversions.Add(ADiversion);

        SfrReachInfo := SfrReachInfoList[ADiversion.iconr-1];
        SfrReachInfo.IsDiversion := True;
      end;

      SfrReachInfoLists := TSfrReachInfoLists.Create;
      ReachListDictionary := TDictionary<Integer, TSfrReachInfoList>.Create;
      try
        for Index := 0 to SfrReachInfoList.Count - 1 do
        begin
          SfrReachInfo := SfrReachInfoList[Index];
          if  (SfrReachInfo.UpstreamReachCount <> 1)
            or SfrReachInfo.IsDiversion
            or (SfrReceivers.IndexOf(SfrReachInfo.PackageData.rno) >= 0)
            then
          begin
            CreateReachList(SfrReachInfo);
          end;
        end;

        ReachesAdded := True;
        while ReachesAdded do
        begin
          ReachesAdded := False;
          for Index := 0 to SfrReachInfoList.Count - 1 do
          begin
            SfrReachInfo := SfrReachInfoList[Index];
            if SfrReachInfo.Added then
            begin
              Continue;
            end;
            UpstreamReach := 0;
            for UpstreamIndex := 0 to Length(SfrReachInfo.Connections.ic) - 1 do
            begin
              if SfrReachInfo.Connections.ic[UpstreamIndex] > 0 then
              begin
                UpstreamReach := SfrReachInfo.Connections.ic[UpstreamIndex];
                break;
              end;
            end;
            Assert(UpstreamReach > 0);
            if ReachListDictionary.TryGetValue(UpstreamReach, AReachList) then
            begin
              if AReachList.Terminated then
              begin
                CreateReachList(SfrReachInfo);
                ReachesAdded := True;
              end
              else
              begin
                FirstItem := AReachList[0];
                if FirstItem.Compatible(SfrReachInfo) then
                begin
                  AReachList.Add(SfrReachInfo);
                  SfrReachInfo.Added := True;
                  ReachListDictionary.Remove(UpstreamReach);
                  ReachListDictionary.Add(SfrReachInfo.PackageData.rno, AReachList);
                  ReachesAdded := True;

                  if (SfrReachInfo.DownstreamReachCount <> 1)
                    or (SfrReachInfo.Diversions.Count > 0)
                    or (SfrReachInfo.IdObs <> nil)
                    or (SfrSources.IndexOf(SfrReachInfo.PackageData.rno) >= 0)
                    then
                  begin
                    AReachList.Terminated := True;
                  end;
                end
                else
                begin
                  AReachList.Terminated := True;
                  CreateReachList(SfrReachInfo);
                  ReachesAdded := True;
                end;
              end;
            end;
          end;
        end;

        for Index := 0 to SfrReachInfoList.Count - 1 do
        begin
          SfrReachInfo := SfrReachInfoList[Index];
          Assert(SfrReachInfo.Added);
        end;

        PeriodSettings := TObjectList<TNumberedItemLists>.Create;
        try
          for PeriodIndex := 0 to SfrMvrLinkList.Count - 1 do
          begin
            ASettingList := TNumberedItemLists.Create;
            PeriodSettings.Add(ASettingList);

            for ReachIndex := 0 to PackageData.Count - 1 do
            begin
              AReachSettingsList := TNumberedItemList.Create;
              ASettingList.Add(AReachSettingsList);
            end;

            APeriod := SfrMvrLinkList[PeriodIndex].SfrPeriod;
            if APeriod <> nil then
            begin
              for SettingIndex := 0 to APeriod.Count - 1 do
              begin
                ASetting := APeriod[SettingIndex];
                AReachSettingsList := ASettingList[ASetting.IdNumber-1];
                AReachSettingsList.Add(ASetting);
              end;
            end;
          end;

          ObjectCount := 0;
          ReachListDictionary.Clear;
          ScreenObjectDictionary := TDictionary<Integer, TScreenObject>.Create;
          try
            ObjectIndex := 0;
            While ObjectIndex < SfrReachInfoLists.Count do
            begin
              AReachList := SfrReachInfoLists[ObjectIndex];

              SetLength(ManningBoundaryValues, AReachList.Count);
              for CellIndex := 0 to AReachList.Count - 1 do
              begin
                ManningBoundaryValues[CellIndex] := AReachList[CellIndex].PackageData.man;
              end;
              DefaultFormula := BoundaryValuesToFormula(ManningBoundaryValues, 'dummyvariable', Map);
              if DefaultFormula = '' then
              begin
                SplitReachListWithBoundaryValues(AReachList, ManningBoundaryValues);
              end;

              SetLength(UpstreamFractionBoundaryValues, AReachList.Count);
              for CellIndex := 0 to AReachList.Count - 1 do
              begin
                UpstreamFractionBoundaryValues[CellIndex] := AReachList[CellIndex].PackageData.ustrf;
              end;
              DefaultFormula := BoundaryValuesToFormula(UpstreamFractionBoundaryValues, 'dummyvariable', Map);
              if DefaultFormula = '' then
              begin
                SplitReachListWithBoundaryValues(AReachList, UpstreamFractionBoundaryValues);
              end;

              for AuxIndex := 0 to Options.Count - 1 do
              begin
                SetLength(BoundaryValues, AReachList.Count);
                for CellIndex := 0 to AReachList.Count - 1 do
                begin
                  BoundaryValues[CellIndex] := AReachList[CellIndex].PackageData.Aux[AuxIndex];
                end;
                DefaultFormula := BoundaryValuesToFormula(BoundaryValues, 'dummyvariable', Map);
                if DefaultFormula = '' then
                begin
                  SplitReachListWithBoundaryValues(AReachList, BoundaryValues);
                end;
              end;

              for PeriodIndex := 0 to SfrMvrLinkList.Count - 1 do
              begin
                ASettingList := PeriodSettings[PeriodIndex];
                SetLength(StringValues, AReachList.Count);
                for ReachIndex := 0 to AReachList.Count - 1 do
                begin
                  StringValues[ReachIndex] := 'ACTIVE';
                end;
                NeedToSplit := False;
                for ReachIndex := 0 to AReachList.Count - 1 do
                begin
                  AReachSettingsList := ASettingList[AReachList[ReachIndex].PackageData.rno-1];
                  for SettingIndex := 0 to AReachSettingsList.Count - 1 do
                  begin
                    ASetting := AReachSettingsList[SettingIndex];
                    if AnsiSameText(ASetting.Name, 'STATUS') then
                    begin
                      StringValues[ReachIndex] := ASetting.StringValue;
                    end;
                  end;
                  if (ReachIndex > 0) and not AnsiSameText(StringValues[ReachIndex],
                    StringValues[ReachIndex-1]) then
                  begin
                    NeedToSplit := True;
                  end;
                end;
                if NeedToSplit then
                begin
                  SplitReachListWithStrings(AReachList, StringValues);
                  SetLength(StringValues, AReachList.Count);
                end;

                for ReachIndex := 0 to AReachList.Count - 1 do
                begin
                  StringValues[ReachIndex] := '';
                end;
                NeedToSplit := False;
                for ReachIndex := 0 to AReachList.Count - 1 do
                begin
                  AReachSettingsList := ASettingList[AReachList[ReachIndex].PackageData.rno-1];
                  for SettingIndex := 0 to AReachSettingsList.Count - 1 do
                  begin
                    ASetting := AReachSettingsList[SettingIndex];
                    if AnsiSameText(ASetting.Name, 'CROSS_SECTION') then
                    begin
                      StringValues[ReachIndex] := ASetting.StringValue;
                    end;
                  end;
                  if (ReachIndex > 0) and not AnsiSameText(StringValues[ReachIndex],
                    StringValues[ReachIndex-1]) then
                  begin
                    NeedToSplit := True;
                  end;
                end;
                if NeedToSplit then
                begin
                  SplitReachListWithStrings(AReachList, StringValues);
                  SetLength(BoundaryValues, AReachList.Count);
                  SetLength(StringValues, AReachList.Count);
                end;

                SetLength(ManningBoundaryValues, AReachList.Count);
                UpdateReachSettings(AReachList, ManningBoundaryValues, 'MANNING');
                DefaultFormula := BoundaryValuesToFormula(ManningBoundaryValues, 'dummyvariable', Map);
                if DefaultFormula = '' then
                begin
                  SplitReachListWithBoundaryValues(AReachList, ManningBoundaryValues);
                end;

                SetLength(UpstreamFractionBoundaryValues, AReachList.Count);
                UpdateReachSettings(AReachList, UpstreamFractionBoundaryValues, 'UPSTREAM_FRACTION');
                DefaultFormula := BoundaryValuesToFormula(UpstreamFractionBoundaryValues, 'dummyvariable', Map);
                if DefaultFormula = '' then
                begin
                  SplitReachListWithBoundaryValues(AReachList, UpstreamFractionBoundaryValues);
                end;

                SetLength(StageBoundaryValues, AReachList.Count);
                if PeriodIndex = 0 then
                begin
                  for ReachIndex := 0 to AReachList.Count - 1 do
                  begin
                    StageBoundaryValues[ReachIndex].ValueType := vtNumeric;
                    StageBoundaryValues[ReachIndex].NumericValue :=
                      AReachList[ReachIndex].PackageData.rtp;
                  end;
                end;
                UpdateReachSettings(AReachList, StageBoundaryValues, 'STAGE');
                DefaultFormula := BoundaryValuesToFormula(StageBoundaryValues, 'dummyvariable', Map);
                if DefaultFormula = '' then
                begin
                  SplitReachListWithBoundaryValues(AReachList, StageBoundaryValues);
                end;

                SetLength(InflowBoundaryValues, AReachList.Count);
                if PeriodIndex = 0 then
                begin
                  for ReachIndex := 0 to AReachList.Count - 1 do
                  begin
                    InflowBoundaryValues[ReachIndex].ValueType := vtNumeric;
                    InflowBoundaryValues[ReachIndex].NumericValue := 0;
                  end;
                end;
                UpdateReachSettings(AReachList, InflowBoundaryValues, 'INFLOW');
                DefaultFormula := BoundaryValuesToFormula(InflowBoundaryValues, 'dummyvariable', Map);
                if DefaultFormula = '' then
                begin
                  SplitReachListWithBoundaryValues(AReachList, InflowBoundaryValues);
                end;

                SetLength(RainfallBoundaryValues, AReachList.Count);
                if PeriodIndex = 0 then
                begin
                  for ReachIndex := 0 to AReachList.Count - 1 do
                  begin
                    RainfallBoundaryValues[ReachIndex].ValueType := vtNumeric;
                    RainfallBoundaryValues[ReachIndex].NumericValue := 0;
                  end;
                end;
                UpdateReachSettings(AReachList, RainfallBoundaryValues, 'RAINFALL');
                DefaultFormula := BoundaryValuesToFormula(RainfallBoundaryValues, 'dummyvariable', Map);
                if DefaultFormula = '' then
                begin
                  SplitReachListWithBoundaryValues(AReachList, RainfallBoundaryValues);
                end;

                SetLength(EvaporationBoundaryValues, AReachList.Count);
                if PeriodIndex = 0 then
                begin
                  for ReachIndex := 0 to AReachList.Count - 1 do
                  begin
                    EvaporationBoundaryValues[ReachIndex].ValueType := vtNumeric;
                    EvaporationBoundaryValues[ReachIndex].NumericValue := 0;
                  end;
                end;
                UpdateReachSettings(AReachList, EvaporationBoundaryValues, 'EVAPORATION');
                DefaultFormula := BoundaryValuesToFormula(EvaporationBoundaryValues, 'dummyvariable', Map);
                if DefaultFormula = '' then
                begin
                  SplitReachListWithBoundaryValues(AReachList, EvaporationBoundaryValues);
                end;

                SetLength(RunoffBoundaryValues, AReachList.Count);
                if PeriodIndex = 0 then
                begin
                  for ReachIndex := 0 to AReachList.Count - 1 do
                  begin
                    RunoffBoundaryValues[ReachIndex].ValueType := vtNumeric;
                    RunoffBoundaryValues[ReachIndex].NumericValue := 0;
                  end;
                end;
                UpdateReachSettings(AReachList, RunoffBoundaryValues, 'RUNOFF');
                DefaultFormula := BoundaryValuesToFormula(RunoffBoundaryValues, 'dummyvariable', Map);
                if DefaultFormula = '' then
                begin
                  SplitReachListWithBoundaryValues(AReachList, RunoffBoundaryValues);
                end;

                for AuxIndex := 0 to Options.Count - 1 do
                begin
                  AuxName := Options.AUXILIARY[AuxIndex];
                  SetLength(AuxArrays[AuxIndex], AReachList.Count);
                  if PeriodIndex = 0 then
                  begin
                    for ReachIndex := 0 to AReachList.Count - 1 do
                    begin
                      AReach := AReachList[ReachIndex];
                      AuxArrays[AuxIndex][ReachIndex] := AReach.PackageData.Aux[AuxIndex];
                    end;
                  end;
                  UpdateReachSettings(AReachList, AuxArrays[AuxIndex], 'AUXILIARY', AuxName);
                  DefaultFormula := BoundaryValuesToFormula(AuxArrays[AuxIndex], 'dummyvariable', Map);
                  if DefaultFormula = '' then
                  begin
                    SplitReachListWithBoundaryValues(AReachList, AuxArrays[AuxIndex]);
                  end;
                end;

              end;

              AScreenObject := CreateScreenObject(AReachList);
              for ReachIndex := 0 to AReachList.Count - 1 do
              begin
                SfrReachInfo := AReachList[ReachIndex];
                ReachListDictionary.Add(SfrReachInfo.PackageData.rno, AReachList);
                ScreenObjectDictionary.Add(SfrReachInfo.PackageData.rno, AScreenObject);
              end;


              Inc(ObjectIndex);
            end;

            for ObjectIndex := 0 to SfrReachInfoLists.Count - 1 do
            begin
              AReachList := SfrReachInfoLists[ObjectIndex];
              LastReach := AReachList.Last;
              if LastReach.DownstreamReachCount > 0 then
              begin
                AScreenObject := nil;
                if not ScreenObjectDictionary.TryGetValue(
                  LastReach.PackageData.rno, AScreenObject) then
                begin
                  Assert(False);
                end;
                for DownstreamIndex := 0 to Length(LastReach.Connections.ic) - 1 do
                begin
                  ReachNo := LastReach.Connections.ic[DownstreamIndex];
                  if ReachNo < 0 then
                  begin
                    DownStreamSegment := nil;
                    if not ScreenObjectDictionary.TryGetValue(-ReachNo, DownStreamSegment) then
                    begin
                      Assert(False);
                    end;
                    IntItem :=AScreenObject.ModflowSfr6Boundary.DownstreamSegments.Add;
                    IntItem.Value := DownStreamSegment.ModflowSfr6Boundary.SegmentNumber;
                  end;
                end;


                if LastReach.Diversions.Count > 0 then
                begin
                  for DiversionIndex := 0 to LastReach.Diversions.Count - 1 do
                  begin
                    ADiversion := LastReach.Diversions[DiversionIndex];
                    DiversionSegment := nil;
                    if not ScreenObjectDictionary.TryGetValue(ADiversion.iconr, DiversionSegment) then
                    begin
                      Assert(False);
                    end;
                    SfrDiversions := AScreenObject.ModflowSfr6Boundary.Diversions;
                    DiversionItem := SfrDiversions.Add;
                    DiversionItem.DownstreamSegment :=
                      DiversionSegment.ModflowSfr6Boundary.SegmentNumber;

                    for DownstreamIndex := 0 to AScreenObject.ModflowSfr6Boundary.DownstreamSegments.Count - 1 do
                    begin
                      IntItem := AScreenObject.ModflowSfr6Boundary.DownstreamSegments[DownstreamIndex];
                      if IntItem.Value = DiversionItem.DownstreamSegment then
                      begin
                        IntItem.Free;
                        Break;
                      end;
                    end;

                    if AnsiSameText(ADiversion.cprior, 'FRACTION') then
                    begin
                      DiversionItem.Priority := cpFraction;
                    end
                    else if AnsiSameText(ADiversion.cprior, 'EXCESS') then
                    begin
                      DiversionItem.Priority := cpExcess;
                    end
                    else if AnsiSameText(ADiversion.cprior, 'THRESHOLD') then
                    begin
                      DiversionItem.Priority := cpThreshold;
                    end
                    else if AnsiSameText(ADiversion.cprior, 'UPTO') then
                    begin
                      DiversionItem.Priority := cpUpTo;
                    end
                    else
                    begin
                      Assert(False);
                    end;

                    for PeriodIndex := 0 to SfrMvrLinkList.Count - 1 do
                    begin
                      ASettingList := PeriodSettings[PeriodIndex];
                      AReachSettingsList := ASettingList[LastReach.PackageData.rno-1];
                      if PeriodIndex = 0 then
                      begin
                        DiversionFormula := '0';
                      end
                      else
                      begin
                        SfrItem := AScreenObject.ModflowSfr6Boundary.
                          Values[PeriodIndex-1] as TSfrMf6Item;
                        DiversionFormula := SfrItem.DiversionFormulas[DiversionItem.Index];
                      end;
                      for SettingIndex := 0 to AReachSettingsList.Count - 1 do
                      begin
                        ASetting := AReachSettingsList[SettingIndex];
                        if AnsiSameText(ASetting.Name, 'DIVERSION')
                          and (ASetting.IntValue-1 = DiversionIndex) then
                        begin
                          if ASetting.StringValue <> '' then
                          begin
                            DiversionFormula := ASetting.StringValue;
                          end
                          else
                          begin
                            DiversionFormula := FortranFloatToStr(ASetting.FloatValue);
                          end;
                          break;
                        end;
                      end;
                      SfrItem := AScreenObject.ModflowSfr6Boundary.
                        Values[PeriodIndex] as TSfrMf6Item;
                      if SfrItem.DiversionCount <= DiversionItem.Index then
                      begin
                        SfrItem.DiversionCount := DiversionItem.Index+1
                      end;
                      SfrItem.DiversionFormulas[DiversionItem.Index] := DiversionFormula;
                    end;
                  end;
                end;
              end;
            end;
          finally
            ScreenObjectDictionary.Free;
          end;
        finally
          PeriodSettings.Free;
        end;


      finally
        ReachListDictionary.Free;
        SfrReachInfoLists.Free;
      end;
      // Put all reaches into lists.
      // Each list will be used to create a TScreenObject.

    finally
      SfrReachInfoList.Free;
    end;

    // Find singlely connected reaches.
    //  => 0 or 1 upstream reaches,
    //     0 or 1 downstream reaches.
    //     if diversions > 0 end of connected reaches.
    //     cross sections the same.
    //     if Observations, then ID1 is the same
    //     if diversions > 0, end of connected reaches.
    //     if number of downstream reaches = 0, end of connected reaches.

    // NEW CLASSES
    // descendent of TSfrPackageItemList with key properties:
    //    cross section
    //    observations the same
    //    terminated.
    // TObjectList<TSfrPackageItemList>
    // TDictionary<Integer, TObjectList>

    // STRATEGY
    // 1. sort reaches in order from upstream to downstream
    //    Put all reaches with no upstream reaches at the beginning of the list.
    //    Then add downstream reaches from those until there are no more.
    //    For reaches with more than one upstream reach, don't add until all
    //    their upstream reaches have been added.
    //
    // 2. for each TSfrPackageItem, get a TSfrPackageItemList for it.
    //   If the upstream reach for the TSfrPackageItem is 0,
    //     make a new list,
    //     add the item to the list,
    //     assign cross section to the list,
    //     and add observations to the list.
    //     Terminate list if needed.
    //   If the upstream reach is not zero, find upstream list.
    //     if not found Error
    //     if found
    //       is upstream terminated
    //         Yes: start new list
    //       else is upsteam reach a match
    //         Yes: add to list, terminate list if needed.
    //       else
    //         start new list


  finally
    Map.Free;
    BoundNameObsDictionary.Free;
    ObsLists.Free;
    NumberObsDictionary.Free;
    CellIds.Free;
    SfrMvrLinkList.Free;
    SfrSources.Free;
    SfrReceivers.Free;

  end;

end;

procedure TModflow6Importer.ImportSimulationOptions;
var
  Model: TPhastModel;
  SmsPkg: TSmsPackageSelection;
  OC: TModflowOutputControl;
begin
  Model := frmGoPhast.PhastModel;
  SmsPkg := Model.ModflowPackages.SmsPackage;
  SmsPkg.ContinueModel := FSimulation.Options.ContinueOption;
  if FSimulation.Options.NoCheckOption then
  begin
    SmsPkg.CheckInput := ciDontCheck
  end;
  case FSimulation.Options.MemPrint of
    Mf6.SimulationNameFileReaderUnit.mpNone:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpNone;
      end;
    Mf6.SimulationNameFileReaderUnit.mpSummary:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpSummary;
      end;
    Mf6.SimulationNameFileReaderUnit.mpAll:
      begin
        SmsPkg.MemoryPrint := ModflowPackageSelectionUnit.mpAll;
      end;
  end;
  SmsPkg.MaxErrors := FSimulation.Options.MaxErrors;
  if FSimulation.Options.PrintInputOption then
  begin
    OC := Model.ModflowOutputControl;
    OC.PrintInputCellLists := True;
  end;
end;

procedure TModflow6Importer.ImportSolutionGroups;
var
  GroupIndex: Integer;
  Group: TSolutionGroup;
  SolutionIndex: Integer;
  Solution: TSolution;
  ModelIndex: Integer;
  Model: TPhastModel;
begin
  // for now, this just imports Mxiter.
  if FFlowModel = nil then
  begin
    Exit;
  end;
  Model := frmGoPhast.PhastModel;
  for GroupIndex := 0 to FSimulation.SolutionGroupCount - 1 do
  begin
    Group := FSimulation.SolutionGroups[GroupIndex];
    for SolutionIndex := 0 to Group.Count - 1 do
    begin
      Solution := Group.Solutions[SolutionIndex];
      for ModelIndex := 0 to Solution.FSolutionModelNames.Count - 1 do
      begin
        if AnsiSameText(Solution.FSolutionModelNames[ModelIndex], FFlowModel.ModelName) then
        begin
          Model.ModflowPackages.SmsPackage.SolutionGroupMaxIteration
            := Group.Mxiter
        end;
      end;
    end;
  end;
end;

procedure TModflow6Importer.ImportSto(Package: TPackage);
var
  Sto: TSto;
  Model: TPhastModel;
  StoPackage: TStoPackage;
  Options: TStoOptions;
  GridData: TStoGridData;
  TvsIndex: Integer;
  DataSetName: string;
  StressPeriods: TModflowStressPeriods;
  SPIndex: Integer;
  StoPeriod: TStoStressPeriod;
  PriorStoPeriod: TStoStressPeriod;
  StressPeriod: TModflowStressPeriod;
  InnerIndex: Integer;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing STO package');
  end;
  Model := frmGoPhast.PhastModel;
  Sto := Package.Package as TSto;

  StoPackage := Model.ModflowPackages.StoPackage;
  StoPackage.IsSelected := True;
  Options := Sto.Options;
  if Options.STORAGECOEFFICIENT then
  begin
    StoPackage.StorageChoice := scStorageCoefficient;
  end;

  Model.DataArrayManager.CreateInitialDataSets;

  GridData := Sto.GridData;
  Assign3DBooleanDataSet(KConvertible, GridData.ICONVERT);

  case StoPackage.StorageChoice of
    scSpecificStorage:
      begin
        DataSetName := rsSpecific_Storage;
      end;
    scStorageCoefficient:
      begin
        DataSetName := StrConfinedStorageCoe;
      end;
  end;
  Assign3DRealDataSet(DataSetName, GridData.SS);

  Assign3DRealDataSet(rsSpecificYield, GridData.SY);

  PriorStoPeriod := nil;
  StressPeriods := Model.ModflowStressPeriods;
  for SPIndex := 0 to Sto.Count - 1 do
  begin
    StoPeriod := Sto[SPIndex];
    StressPeriod := StressPeriods[StoPeriod.Period -1];
    if StoPeriod.Transient then
    begin
      StressPeriod.StressPeriodType := sptTransient;
    end
    else
    begin
      StressPeriod.StressPeriodType := sptSteadyState;
    end;
    if PriorStoPeriod <> nil then
    begin
      for InnerIndex := PriorStoPeriod.Period to StoPeriod.Period - 2 do
      begin
        StressPeriod := StressPeriods[InnerIndex];
        if PriorStoPeriod.Transient then
        begin
          StressPeriod.StressPeriodType := sptTransient;
        end
        else
        begin
          StressPeriod.StressPeriodType := sptSteadyState;
        end;
      end;
    end;

    PriorStoPeriod := StoPeriod
  end;
  if PriorStoPeriod <> nil then
  begin
    for InnerIndex := PriorStoPeriod.Period to StressPeriods.Count - 1 do
    begin
      StressPeriod := StressPeriods[InnerIndex];
      if PriorStoPeriod.Transient then
      begin
        StressPeriod.StressPeriodType := sptTransient;
      end
      else
      begin
        StressPeriod.StressPeriodType := sptSteadyState;
      end;
    end;
  end;

  if Sto.TvsCount > 0 then
  begin
    Model.ModflowPackages.TvsPackage.IsSelected := True;
    for TvsIndex := 0 to Sto.TvsCount - 1 do
    begin
      ImportTvs(Sto.TvsPackages[TvsIndex])
    end;
  end;
end;

procedure TModflow6Importer.ImportTimeSeries(Package: TPackage; Map: TimeSeriesMap);
var
  TsReader: TTimeSeries;
  Model: TPhastModel;
  Mf6TimesSeries: TTimesSeriesCollections;
  Attributes: TTsAttributes;
  GroupName: string;
  NewGroup: TTimesSeriesCollection;
  Index: Integer;
  TSName: string;
  NewName: string;
  TimeSeries: TMf6TimeSeries;
  Method: TTsMethod;
  ImportedTs: TTsTimeSeries;
  TimeIndex: Integer;
  ImportedValues: TDoubleList;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing TIME SERIES file');
  end;
  Model := frmGoPhast.PhastModel;
  Mf6TimesSeries := Model.Mf6TimesSeries;

  GroupName := ExtractFileName(Package.FileName);
  NewGroup := Mf6TimesSeries.Add.TimesSeriesCollection;
  NewGroup.GroupName := AnsiString(GroupName);

  TsReader := Package.Package as TTimeSeries;
  Attributes := TsReader.Attributes;
  ImportedTs := TsReader.TimeSeries;

  NewGroup.Times.Capacity := ImportedTs.TimeCount;
  for TimeIndex := 0 to ImportedTs.TimeCount - 1 do
  begin
    NewGroup.Times.Add.Value := ImportedTs.Times[TimeIndex];
  end;

  for Index := 0 to Attributes.NameCount - 1 do
  begin
    TSName := Attributes.Names[Index];

    if Mf6TimesSeries.GetTimeSeriesByName(UpperCase(TSName)) = nil then
    begin
      NewName := TSName;
    end
    else
    begin
      Inc(TSIndex);
      NewName := 'ImportedTS_' + TSName + '_' + IntToStr(TSIndex);
      while Mf6TimesSeries.GetTimeSeriesByName(TSName) <> nil do
      begin
        Inc(TSIndex);
        NewName := 'ImportedTS_' + TSName + '_' + IntToStr(TSIndex);
      end;
    end;

    Map.Add(UpperCase(TSName), NewName);

    TimeSeries := NewGroup.Add.TimeSeries;
    TimeSeries.SeriesName := AnsiString(NewName);

    Method := Attributes.Methods[Index];
    Assert(Method <> tsUndefined);
    case Method of
      tmStepWise:
        begin
          TimeSeries.InterpolationMethod := mimStepwise;
        end;
      tmLinear:
        begin
          TimeSeries.InterpolationMethod := mimLinear;
        end;
      tmLinearEnd:
        begin
          TimeSeries.InterpolationMethod := mimLinearEnd;
        end;
      else
        begin
          assert(False)
        end;
    end;
    if Attributes.SfacCount > Index then
    begin
      TimeSeries.ScaleFactor := Attributes.SFacs[Index];
    end
    else
    begin
      TimeSeries.ScaleFactor := 1;
    end;
    ImportedValues := ImportedTs.TimeSeriesValues[Index];
    TimeSeries.Capacity := ImportedValues.Count;
    for TimeIndex := 0 to ImportedValues.Count - 1 do
    begin
      TimeSeries.Add.Value := ImportedValues[TimeIndex];
    end;
  end;
end;

procedure TModflow6Importer.ImportTvk(Package: TPackage);
var
  Tvk: TTvk;
  APeriod: TTvkPeriodData;
  Model: TPhastModel;
  LastTime: Double;
  StartTime: Double;
  PeriodIndex: Integer;
  BoundIndex: Integer;
  TvkBound: TTimeVariableCell;
  KScreenObject: TScreenObject;
  Item: TTvkItem;
  CellId: TMfCellId;
  KDictionary: TDictionary<string, TScreenObject>;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  APoint: TPoint2D;
  TimeSeriesName: string;
  KStorage: TValueArrayItem;
  ImportedKName: string;
  K22ScreenObject: TScreenObject;
  K33ScreenObject: TScreenObject;
  K22Storage: TValueArrayItem;
  K22Dictionary: TDictionary<string, TScreenObject>;
  K33Dictionary: TDictionary<string, TScreenObject>;
  K33Storage: TValueArrayItem;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ImportedTimeSeriesName: string;
  ElementCenter: TDualLocation;
  function CreateScreenObject(RootName: String): TScreenObject;
  var
    NewItem: TTvkItem;
  begin
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    result.Name := 'ImportedTVK_' + RootName + '_Period_' + IntToStr(APeriod.Period);
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateTvkBoundary;

    NewItem := result.ModflowTvkBoundary.Values.Add as TTvkItem;
    NewItem.StartTime := StartTime;
    NewItem.EndTime := LastTime;
  end;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing TVK package');
  end;
  Model := frmGoPhast.PhastModel;
  LastTime := Model.ModflowStressPeriods.Last.EndTime;

  KDictionary := TDictionary<string, TScreenObject>.Create;
  K22Dictionary := TDictionary<string, TScreenObject>.Create;
  K33Dictionary := TDictionary<string, TScreenObject>.Create;
  Map := TimeSeriesMap.Create;
  try
    Tvk := Package.Package as TTvk;

    for TimeSeriesIndex := 0 to Tvk.TimeSeriesPackageCount - 1 do
    begin
      TimeSeriesPackage := Tvk.TimeSeriesPackages[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;
    if Assigned(OnUpdateStatusBar) then
    begin
      OnUpdateStatusBar(self, 'importing TVK package');
    end;

    KScreenObject := nil;
    K22ScreenObject := nil;
    K33ScreenObject := nil;
    for PeriodIndex := 0 to Tvk.Count - 1 do
    begin
      KStorage := nil;
      K22Storage := nil;
      K33Storage := nil;

      APeriod := Tvk[PeriodIndex];
      StartTime := Model.ModflowStressPeriods[APeriod.Period-1].StartTime;
      if KScreenObject <> nil then
      begin
        Item := KScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
        Item.EndTime := StartTime;
      end;
      KScreenObject := nil;

      if K22ScreenObject <> nil then
      begin
        Item := K22ScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
        Item.EndTime := StartTime;
      end;
      K22ScreenObject := nil;

      if K33ScreenObject <> nil then
      begin
        Item := K33ScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
        Item.EndTime := StartTime;
      end;
      K33ScreenObject := nil;

      for AScreenObject in KDictionary.Values do
      begin
        Item := AScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
        Item.EndTime := StartTime;
      end;
      KDictionary.Clear;

      for AScreenObject in K22Dictionary.Values do
      begin
        Item := AScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
        Item.EndTime := StartTime;
      end;
      K22Dictionary.Clear;

      for AScreenObject in K33Dictionary.Values do
      begin
        Item := AScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
        Item.EndTime := StartTime;
      end;
      K33Dictionary.Clear;

      for BoundIndex := 0 to APeriod.Count - 1 do
      begin
        TvkBound := APeriod[BoundIndex];
        if TvkBound.VariableName = 'K' then
        begin
          if TvkBound.ValueType = vtNumeric then
          begin
            ImportedKName := 'ImportedTvk_K_' + IntToStr(APeriod.Period);
            if KScreenObject = nil then
            begin

              KScreenObject := CreateScreenObject('K');

              KStorage := KScreenObject.ImportedValues.Add;
              KStorage.Name := ImportedKName;
              KStorage.Values.DataType := rdtDouble;

              Item := KScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
              Item.K := rsObjectImportedValuesR + '("' + ImportedKName + '")';
            end;
            AScreenObject := KScreenObject;
            KStorage.Values.Add(TvkBound.NumericValue);
          end
          else
          begin
            Assert(TvkBound.ValueType = vtString);
            TimeSeriesName := TvkBound.StringValue;
            if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeriesName) then
            begin
              Assert(False);
            end;
            TimeSeriesName := ImportedTimeSeriesName;
            if not KDictionary.TryGetValue(TimeSeriesName, AScreenObject) then
            begin
              AScreenObject := CreateScreenObject('K_' + TimeSeriesName);

              KDictionary.Add(TimeSeriesName, AScreenObject);
              Item := AScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
              Item.K := TimeSeriesName;
            end;
          end;
        end
        else if TvkBound.VariableName = 'K22' then
        begin
          if TvkBound.ValueType = vtNumeric then
          begin
            ImportedKName := 'ImportedTvk_K22_' + IntToStr(APeriod.Period);
            if K22ScreenObject = nil then
            begin

              K22ScreenObject := CreateScreenObject('K22');

              K22Storage := K22ScreenObject.ImportedValues.Add;
              K22Storage.Name := ImportedKName;
              K22Storage.Values.DataType := rdtDouble;

              Item := K22ScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
              Item.K22 := rsObjectImportedValuesR + '("' + ImportedKName + '")';
            end;
            AScreenObject := K22ScreenObject;
            K22Storage.Values.Add(TvkBound.NumericValue);
          end
          else
          begin
            Assert(TvkBound.ValueType = vtString);
            TimeSeriesName := TvkBound.StringValue;
            if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeriesName) then
            begin
              Assert(False);
            end;
            if not K22Dictionary.TryGetValue(TimeSeriesName, AScreenObject) then
            begin
              AScreenObject := CreateScreenObject('K22_' + TimeSeriesName);

              K22Dictionary.Add(TimeSeriesName, AScreenObject);
              Item := AScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
              Item.K22 := TimeSeriesName;
            end;
          end;
        end
        else if TvkBound.VariableName = 'K33' then
        begin
          if TvkBound.ValueType = vtNumeric then
          begin
            ImportedKName := 'ImportedTvk_K33_' + IntToStr(APeriod.Period);
            if K33ScreenObject = nil then
            begin

              K33ScreenObject := CreateScreenObject('K33');

              K33Storage := K33ScreenObject.ImportedValues.Add;
              K33Storage.Name := ImportedKName;
              K33Storage.Values.DataType := rdtDouble;

              Item := K33ScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
              Item.K33 := rsObjectImportedValuesR + '("' + ImportedKName + '")';
            end;
            AScreenObject := K33ScreenObject;
            K33Storage.Values.Add(TvkBound.NumericValue);
          end
          else
          begin
            Assert(TvkBound.ValueType = vtString);
            TimeSeriesName := TvkBound.StringValue;
            if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeriesName) then
            begin
              Assert(False);
            end;
            if not K33Dictionary.TryGetValue(TimeSeriesName, AScreenObject) then
            begin
              AScreenObject := CreateScreenObject('K33_' + TimeSeriesName);

              K33Dictionary.Add(TimeSeriesName, AScreenObject);
              Item := AScreenObject.ModflowTvkBoundary.Values.Last as TTvkItem;
              Item.K33 := TimeSeriesName;
            end;
          end;
        end
        else
        begin
          Assert(False);
        end;
        CellId := TvkBound.CellId;
        if Model.DisvUsed then
        begin
          Dec(CellId.Column);
          CellId.Row := 0;
        end
        else
        begin
          Dec(CellId.Column);
          Dec(CellId.Row);
        end;
        ElementCenter := Model.ElementLocation[CellId.Layer-1, CellId.Row, CellId.Column];
        APoint.x := ElementCenter.RotatedLocation.x;
        APoint.y := ElementCenter.RotatedLocation.y;
        AScreenObject.AddPoint(APoint, True);
        AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);
      end;
    end;
  finally
    KDictionary.Free;
    K22Dictionary.Free;
    K33Dictionary.Free;
    Map.Free;
  end;

end;

procedure TModflow6Importer.ImportTvs(Package: TPackage);
var
  Tvs: TTvs;
  APeriod: TTvsPeriodData;
  Model: TPhastModel;
  LastTime: Double;
  StartTime: Double;
  PeriodIndex: Integer;
  BoundIndex: Integer;
  TvsBound: TTimeVariableCell;
  SsScreenObject: TScreenObject;
  Item: TTvsItem;
  CellId: TMfCellId;
  SsDictionary: TDictionary<string, TScreenObject>;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  APoint: TPoint2D;
  TimeSeriesName: string;
  SsStorage: TValueArrayItem;
  ImportedName: string;
  SyScreenObject: TScreenObject;
  SyStorage: TValueArrayItem;
  SyDictionary: TDictionary<string, TScreenObject>;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ImportedTimeSeriesName: string;
  ElementCenter: TDualLocation;
  function CreateScreenObject(RootName: String): TScreenObject;
  var
    NewItem: TTvsItem;
  begin
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    result.Name := 'ImportedTVS_' + RootName + '_Period_' + IntToStr(APeriod.Period);
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateTvsBoundary;

    NewItem := result.ModflowTvsBoundary.Values.Add as TTvsItem;
    NewItem.StartTime := StartTime;
    NewItem.EndTime := LastTime;
  end;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing TVS package');
  end;
  Model := frmGoPhast.PhastModel;
  LastTime := Model.ModflowStressPeriods.Last.EndTime;

  SsDictionary := TDictionary<string, TScreenObject>.Create;
  SyDictionary := TDictionary<string, TScreenObject>.Create;
  Map := TimeSeriesMap.Create;
  try
    Tvs := Package.Package as TTvs;

    Model.ModflowPackages.TvsPackage.Enable_Storage_Change_Integration :=
      not Tvs.Options.DISABLE_STORAGE_CHANGE_INTEGRATION;

    for TimeSeriesIndex := 0 to Tvs.TimeSeriesPackageCount - 1 do
    begin
      TimeSeriesPackage := Tvs.TimeSeriesPackages[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;
    if Assigned(OnUpdateStatusBar) then
    begin
      OnUpdateStatusBar(self, 'importing TVS package');
    end;

    SsScreenObject := nil;
    SyScreenObject := nil;
    for PeriodIndex := 0 to Tvs.Count - 1 do
    begin
      SsStorage := nil;
      SyStorage := nil;
//      K33Storage := nil;

      APeriod := Tvs[PeriodIndex];
      StartTime := Model.ModflowStressPeriods[APeriod.Period-1].StartTime;
      if SsScreenObject <> nil then
      begin
        Item := SsScreenObject.ModflowTvsBoundary.Values.Last as TTvsItem;
        Item.EndTime := StartTime;
      end;
      SsScreenObject := nil;

      if SyScreenObject <> nil then
      begin
        Item := SyScreenObject.ModflowTvsBoundary.Values.Last as TTvsItem;
        Item.EndTime := StartTime;
      end;
      SyScreenObject := nil;

      for AScreenObject in SsDictionary.Values do
      begin
        Item := AScreenObject.ModflowTvsBoundary.Values.Last as TTvsItem;
        Item.EndTime := StartTime;
      end;
      SsDictionary.Clear;

      for AScreenObject in SyDictionary.Values do
      begin
        Item := AScreenObject.ModflowTvsBoundary.Values.Last as TTvsItem;
        Item.EndTime := StartTime;
      end;
      SyDictionary.Clear;

      for BoundIndex := 0 to APeriod.Count - 1 do
      begin
        TvsBound := APeriod[BoundIndex];
        if TvsBound.VariableName = 'SS' then
        begin
          if TvsBound.ValueType = vtNumeric then
          begin
            ImportedName := 'ImportedTvs_SS_' + IntToStr(APeriod.Period);
            if SsScreenObject = nil then
            begin

              SsScreenObject := CreateScreenObject('Ss');

              SsStorage := SsScreenObject.ImportedValues.Add;
              SsStorage.Name := ImportedName;
              SsStorage.Values.DataType := rdtDouble;

              Item := SsScreenObject.ModflowTvsBoundary.Values.Last as TTvsItem;
              Item.Ss := rsObjectImportedValuesR + '("' + ImportedName + '")';
            end;
            AScreenObject := SsScreenObject;
            SsStorage.Values.Add(TvsBound.NumericValue);
          end
          else
          begin
            Assert(TvsBound.ValueType = vtString);
            TimeSeriesName := TvsBound.StringValue;
            if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeriesName) then
            begin
              Assert(False);
            end;
            TimeSeriesName := ImportedTimeSeriesName;
            if not SsDictionary.TryGetValue(TimeSeriesName, AScreenObject) then
            begin
              AScreenObject := CreateScreenObject('Ss_' + TimeSeriesName);

              SsDictionary.Add(TimeSeriesName, AScreenObject);
              Item := AScreenObject.ModflowTvsBoundary.Values.Last as TTvsItem;
              Item.Ss := TimeSeriesName;
            end;
          end;
        end
        else if TvsBound.VariableName = 'SY' then
        begin
          if TvsBound.ValueType = vtNumeric then
          begin
            ImportedName := 'ImportedTvs_Sy_' + IntToStr(APeriod.Period);
            if SyScreenObject = nil then
            begin

              SyScreenObject := CreateScreenObject('Sy');

              SyStorage := SyScreenObject.ImportedValues.Add;
              SyStorage.Name := ImportedName;
              SyStorage.Values.DataType := rdtDouble;

              Item := SyScreenObject.ModflowTvsBoundary.Values.Last as TTvsItem;
              Item.Sy := rsObjectImportedValuesR + '("' + ImportedName + '")';
            end;
            AScreenObject := SyScreenObject;
            SyStorage.Values.Add(TvsBound.NumericValue);
          end
          else
          begin
            Assert(TvsBound.ValueType = vtString);
            TimeSeriesName := TvsBound.StringValue;
            if not Map.TryGetValue(UpperCase(TimeSeriesName), ImportedTimeSeriesName) then
            begin
              Assert(False);
            end;
            if not SyDictionary.TryGetValue(TimeSeriesName, AScreenObject) then
            begin
              AScreenObject := CreateScreenObject('Sy_' + TimeSeriesName);

              SyDictionary.Add(TimeSeriesName, AScreenObject);
              Item := AScreenObject.ModflowTvsBoundary.Values.Last as TTvsItem;
              Item.Sy := TimeSeriesName;
            end;
          end;
        end
        else
        begin
          Assert(False);
        end;
        CellId := TvsBound.CellId;
        if Model.DisvUsed then
        begin
          Dec(CellId.Column);
          CellId.Row := 0;
        end
        else
        begin
          Dec(CellId.Column);
          Dec(CellId.Row);
        end;
        ElementCenter := Model.ElementLocation[CellId.Layer-1, CellId.Row, CellId.Column];
        APoint.x := ElementCenter.RotatedLocation.x;
        APoint.y := ElementCenter.RotatedLocation.y;
        AScreenObject.AddPoint(APoint, True);
        AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);
      end;
    end;
  finally
    SsDictionary.Free;
    SyDictionary.Free;
    Map.Free;
  end;

end;

type
  TImportUzfPeriodItem = record
    PeriodData: TUzfPeriodItem;
    Period: Integer;
    function Compatible(Item: TImportUzfPeriodItem): Boolean;
  end;
  TImportUzfPeriodItemList = TList<TImportUzfPeriodItem>;

  TUzfData = Class(TObject)
    PackageData: TUzfPackageItem;
    PeriodData: TImportUzfPeriodItemList;
    MvrSource: Boolean;
    MvrReceiver: Boolean;
    BoundNameObs: TObservationList;
    NumberObs: TObservationList;
    constructor Create;
    destructor Destroy; override;
    function Compatible(UzfData: TUzfData): Boolean;
  end;

  TUzfDataList = TList<TUzfData>;
  TUzfDataObjectList = TObjectList<TUzfData>;
  TUzfDataObjectLists = TObjectList<TUzfDataList>;

  TUzfMvrLink = record
    UzfPeriod: TUzfPeriod;
    MvrPeriod: TMvrPeriod;
    function Period: Integer;
  end;
  TUzfMvrLinkArray = TArray<TUzfMvrLink>;
  TUzfMvrLinkList = TList<TUzfMvrLink>;

procedure TModflow6Importer.ImportUzf(Package: TPackage;
  TransportModels: TModelList; MvrPackage: TPackage);
var
  Model: TPhastModel;
  UzfPackage: TUzfMf6PackageSelection;
  Uzf: TUzf;
  Options: TUzfOptions;
  Dimensions: TUzfDimensions;
  Mvr: TMvr;
  UzfSources: TIntegerList;
  UzfReceivers: TIntegerList;
  UzfMvrLink: TUzfMvrLink;
  UzfMvrLinkArray: TUzfMvrLinkArray;
  UzfMvrLinkList:  TUzfMvrLinkList;
  StressPeriodIndex: Integer;
  MvrPeriod: TMvrPeriod;
  UzfPeriod: TUzfPeriod;
  UzfData: TUzfDataObjectList;
  MergedUzfData: TUzfDataObjectLists;
  PackageItem: TUzfPackageItem;
  CellIndex: Integer;
  UzfDataItem: TUzfData;
  PeriodIndex: Integer;
  UzfPeriodItem: TUzfPeriodItem;
  ImportUzfPeriodItem: TImportUzfPeriodItem;
  Period: Integer;
  Map: TimeSeriesMap;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  NumberObsDictionary: TNumberDictionary;
  BoundNameObsDictionary: TBoundNameDictionary;
  ObsLists: TObsLists;
  Obs: TObservationList;
  UzfIndex: Integer;
  MergeIndex: Integer;
  MergedList: TUzfDataList;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  CellIds: TCellIdList;
  CellId: TMfCellId;
  ModflowUzfMf6Boundary: TUzfMf6Boundary;
  SurfDepthItem: TValueArrayItem;
  vksItem: TValueArrayItem;
  thtrItem: TValueArrayItem;
  thtsItem: TValueArrayItem;
  thtiItem: TValueArrayItem;
  epsItem: TValueArrayItem;
  StartTime: Double;
  EndTime: Double;
  UzfMf6Item: TUzfMf6Item;
  ImportedUzfPeriodItem: TImportUzfPeriodItem;
  PData: TUzfPeriodItem;
  BoundaryValueArray: TMf6BoundaryValueArray;
  ADataArray: TDataArray;
  FormulaPosition: Integer;
  MvrSource: TMvrSource;
  MvrReceiver: TMvrReceiver;
  ObsNameIndex: Integer;
  procedure IdentifySourcesAndReceivers(MvrPeriod: TMvrPeriod);
  var
    ItemIndex: Integer;
    PackageName: string;
    MvrItem: TMvrPeriodItem;
  begin
    PackageName := Package.PackageName;
    for ItemIndex := 0 to MvrPeriod.Count - 1 do
    begin
      MvrItem := MvrPeriod[ItemIndex];
      if AnsiSameText(MvrItem.pname1, PackageName) then
      begin
        UzfSources.AddUnique(MvrItem.id1)
      end;
      if AnsiSameText(MvrItem.pname2, PackageName) then
      begin
        UzfReceivers.AddUnique(MvrItem.id2)
      end;
    end;
  end;
  function BoundaryValueToFormula(Value: TMf6BoundaryValue): string;
  begin
    if Value.ValueType = vtNumeric then
    begin
      result := FortranFloatToStr(Value.NumericValue);
    end
    else
    begin
      result := Value.StringValue;
    end;
  end;
  procedure AssignObservations(Obs: TObservationList; AScreenObject: TScreenObject;
    Name: string = '');
  var
    ObsIndex: Integer;
    AnObs: TObservation;
    UzfObs: TUzfObs;
  begin
    if Obs <> nil then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      AScreenObject.CreateMf6Obs;
      if AScreenObject.Modflow6Obs.Name = '' then
      begin
        if Name = '' then
        begin
          Inc(ObsNameIndex);
          AScreenObject.Modflow6Obs.Name := 'UzfObs_' + IntToStr(ObsNameIndex);
        end
        else
        begin
          AScreenObject.Modflow6Obs.Name := Name;
        end;
      end;
      UzfObs := AScreenObject.Modflow6Obs.UzfObs;
      for ObsIndex := 0 to Obs.Count - 1 do
      begin
        AnObs := Obs[ObsIndex];
        if AnsiSameText(AnObs.ObsType, 'uzf-gwrch') then
        begin
          Include(UzfObs, uoGW_Recharge);
        end
        else if AnsiSameText(AnObs.ObsType, 'uzf-gwd') then
        begin
          Include(UzfObs, uoGW_Discharge);
        end
        else if AnsiSameText(AnObs.ObsType, 'uzf-gwd-to-mvr') then
        begin
          Include(UzfObs, uoDischargeToMvr);
        end
        else if AnsiSameText(AnObs.ObsType, 'uzf-gwet') then
        begin
          Include(UzfObs, uoSatZoneEvapotranspiration);
        end
        else if AnsiSameText(AnObs.ObsType, 'infiltration') then
        begin
          Include(UzfObs, uoInfiltration);
        end
        else if AnsiSameText(AnObs.ObsType, 'from-mvr') then
        begin
          Include(UzfObs, uoMvrInflow);
        end
        else if AnsiSameText(AnObs.ObsType, 'rej-inf') then
        begin
          Include(UzfObs, uoRejectInfiltration);
        end
        else if AnsiSameText(AnObs.ObsType, 'rej-inf-to-mvr') then
        begin
          Include(UzfObs, uoRejectInfiltrationToMvr);
        end
        else if AnsiSameText(AnObs.ObsType, 'uzet') then
        begin
          Include(UzfObs, uoUnsatZoneEvapotranspiration);
        end
        else if AnsiSameText(AnObs.ObsType, 'storage') then
        begin
          Include(UzfObs, uoStorage);
        end
        else if AnsiSameText(AnObs.ObsType, 'net-infiltration') then
        begin
          Include(UzfObs, uoNetInfiltration);
        end
        else if AnsiSameText(AnObs.ObsType, 'water-content') then
        begin
          Include(UzfObs, uoWaterContent);
        end
        else
        begin
          Assert(False);
        end;
      end;
      AScreenObject.Modflow6Obs.UzfObs := UzfObs;

//  TUzfOb = (uoGW_Recharge, uoGW_Discharge, uoDischargeToMvr,
//    uoSatZoneEvapotranspiration, uoInfiltration, uoMvrInflow,
//    uoRejectInfiltration, uoRejectInfiltrationToMvr,
//    uoUnsatZoneEvapotranspiration, uoStorage, uoNetInfiltration, uoWaterContent);
//  TUzfObs = set of TUzfOb;

    end;
  end;
begin
  MvrSource.LakeOutlet := nil;
  ObsNameIndex := 0;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing UZF package');
  end;
  Model := frmGoPhast.PhastModel;
  UzfPackage := Model.ModflowPackages.UzfMf6Package;
  UzfPackage.IsSelected := True;

  Uzf := Package.Package as TUzf;
  Options := Uzf.Options;

  UzfPackage.SaveWaterContent := Options.WATER_CONTENT;
  UzfPackage.SaveBudgetFile := Options.BUDGET;
  UzfPackage.SaveBudgetCsvFile := Options.BUDGETCSV;
  UzfPackage.WriteConvergenceData := Options.PACKAGE_CONVERGENCE;
  UzfPackage.SimulateGroundwaterSeepage := Options.SIMULATE_GWSEEP;

  if Options.SIMULATE_ET then
  begin
    UzfPackage.GroundwaterET := ugecSimulateUnsatOnly;
  end;
  if Options.LINEAR_GWET then
  begin
    UzfPackage.GroundwaterET := ugecLinear;
  end;
  if Options.SQUARE_GWET then
  begin
    UzfPackage.GroundwaterET := ugecSquare;
  end;
  if Options.UNSAT_ETWC then
  begin
    UzfPackage.UnsatET := uuecWaterContent;
    if UzfPackage.GroundwaterET = ugecNoEt then
    begin
      UzfPackage.GroundwaterET := ugecSimulateUnsatOnly;
    end;
  end;
  if Options.UNSAT_ETAE then
  begin
    UzfPackage.UnsatET := uuecCapillaryPressure;
    if UzfPackage.GroundwaterET = ugecNoEt then
    begin
      UzfPackage.GroundwaterET := ugecSimulateUnsatOnly;
    end;
  end;

  Dimensions := Uzf.UzfDimensions;

  UzfPackage.NumberOfTrailingWaves := Dimensions.NTRAILWAVES;
  UzfPackage.NumberOfWaveSets := Dimensions.NWAVESETS;

  Model.DataArrayManager.CreateInitialDataSets;

  Mvr := GetMvr(MvrPackage, Package);

  UzfMvrLinkList := TUzfMvrLinkList.Create;
  UzfSources := TIntegerList.Create;
  UzfReceivers := TIntegerList.Create;
  UzfData := TUzfDataObjectList.Create;
  MergedUzfData := TUzfDataObjectLists.Create;
  Map := TimeSeriesMap.Create;
  NumberObsDictionary := TNumberDictionary.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  ObsLists := TObsLists.Create;
  CellIds := TCellIdList.Create;
  try
    for TimeSeriesIndex := 0 to Uzf.TimeSeriesPackageCount - 1 do
    begin
      TimeSeriesPackage := Uzf.TimeSeriesPackages[TimeSeriesIndex];
      ImportTimeSeries(TimeSeriesPackage, Map);
    end;

    if Uzf.ObservationCount > 0 then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    end;
    for ObsPackageIndex := 0 to Uzf.ObservationCount - 1 do
    begin
      ObsFiles := Uzf.Observations[ObsPackageIndex].Package as TObs;
      GetObservations(NumberObsDictionary, BoundNameObsDictionary,
        nil, ObsLists, ObsFiles);
    end;

    if Mvr = nil then
    begin
      UzfMvrLink.MvrPeriod := nil;
      for StressPeriodIndex := 0 to Uzf.PeriodCount - 1 do
      begin
        UzfMvrLink.UzfPeriod := Uzf.Periods[StressPeriodIndex];
        UzfMvrLinkList.Add(UzfMvrLink);
      end;
    end
    else
    begin
      SetLength(UzfMvrLinkArray, Model.ModflowStressPeriods.Count);
      for StressPeriodIndex := 0 to Length(UzfMvrLinkArray) - 1 do
      begin
        UzfMvrLinkArray[StressPeriodIndex].MvrPeriod := nil;
        UzfMvrLinkArray[StressPeriodIndex].UzfPeriod := nil;
      end;
      for StressPeriodIndex := 0 to Mvr.PeriodCount - 1 do
      begin
        MvrPeriod := Mvr.Periods[StressPeriodIndex];
        UzfMvrLinkArray[MvrPeriod.Period-1].MvrPeriod := MvrPeriod;
        IdentifySourcesAndReceivers(MvrPeriod);
      end;
      for StressPeriodIndex := 0 to Uzf.PeriodCount - 1 do
      begin
        UzfPeriod := Uzf.Periods[StressPeriodIndex];
        UzfMvrLinkArray[UzfPeriod.Period-1].UzfPeriod := UzfPeriod;
      end;

      for StressPeriodIndex := 1 to Length(UzfMvrLinkArray) - 1 do
      begin
        if UzfMvrLinkArray[StressPeriodIndex].MvrPeriod = nil then
        begin
          UzfMvrLinkArray[StressPeriodIndex].MvrPeriod := UzfMvrLinkArray[StressPeriodIndex-1].MvrPeriod;
          UzfMvrLinkArray[StressPeriodIndex].UzfPeriod := UzfMvrLinkArray[StressPeriodIndex-1].UzfPeriod;
        end;
      end;

      for StressPeriodIndex := 0 to Length(UzfMvrLinkArray) - 1 do
      begin
        if (UzfMvrLinkArray[StressPeriodIndex].MvrPeriod = nil)
          and (UzfMvrLinkArray[StressPeriodIndex].UzfPeriod = nil) then
        begin
          Continue;
        end;

        if StressPeriodIndex > 0 then
        begin
          if (UzfMvrLinkArray[StressPeriodIndex].MvrPeriod = UzfMvrLinkArray[StressPeriodIndex - 1].MvrPeriod)
            and (UzfMvrLinkArray[StressPeriodIndex].UzfPeriod = UzfMvrLinkArray[StressPeriodIndex - 1].UzfPeriod) then
          begin
            Continue
          end;
        end;

        UzfMvrLinkList.Add(UzfMvrLinkArray[StressPeriodIndex]);
      end;
    end;

    for CellIndex := 0 to Uzf.PackageData.Count - 1 do
    begin
      PackageItem := Uzf.PackageData[CellIndex];
      UzfDataItem := TUzfData.Create;
      UzfDataItem.MvrSource := UzfSources.IndexOf(PackageItem.iuzno) >= 0;
      UzfDataItem.MvrReceiver := UzfReceivers.IndexOf(PackageItem.iuzno) >= 0;
      UzfData.Add(UzfDataItem);
      UzfDataItem.PackageData := PackageItem;
      if PackageItem.boundname <> '' then
      begin
        if BoundNameObsDictionary.TryGetValue(UpperCase(PackageItem.boundname), Obs) then
        begin
          UzfDataItem.BoundNameObs := Obs;
        end;
      end;
      if NumberObsDictionary.TryGetValue(PackageItem.iuzno, Obs) then
      begin
        UzfDataItem.NumberObs := Obs;
      end;
    end;

    for PeriodIndex := 0 to UzfMvrLinkList.Count - 1 do
    begin
      Period := UzfMvrLinkList[PeriodIndex].Period;
      UzfPeriod := UzfMvrLinkList[PeriodIndex].UzfPeriod;
      for CellIndex := 0 to UzfPeriod.Count - 1 do
      begin
        UzfPeriodItem := UzfPeriod[CellIndex];
        UzfDataItem := UzfData[UzfPeriodItem.iuzno-1];
        ImportUzfPeriodItem.PeriodData := UzfPeriodItem;
        ImportUzfPeriodItem.Period := Period;
        UzfDataItem.PeriodData.Add(ImportUzfPeriodItem);
      end;
    end;

    for UzfIndex := 0 to UzfData.Count - 1 do
    begin
      UzfDataItem := UzfData[UzfIndex];
      MergedList := nil;
      for MergeIndex := 0 to MergedUzfData.Count - 1 do
      begin
        MergedList := MergedUzfData[MergeIndex];
        if MergedList.First.Compatible(UzfDataItem) then
        begin
          break;
        end
        else
        begin
          MergedList := nil;
        end;
      end;
      if MergedList = nil then
      begin
        MergedList := TUzfDataList.Create;
        MergedUzfData.Add(MergedList);
      end;
      MergedList.Add(UzfDataItem);

    end;

    for MergeIndex := 0 to MergedUzfData.Count - 1 do
    begin
      MergedList := MergedUzfData[MergeIndex];
      AScreenObject := TScreenObject.CreateWithViewDirection(
        Model, vdTop, UndoCreateScreenObject, False);
      AScreenObject.Name := 'ImportedUZF_' + IntToStr(MergeIndex+1);
      AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

      Model.AddScreenObject(AScreenObject);
      AScreenObject.ElevationCount := ecOne;
      AScreenObject.SetValuesOfIntersectedCells := True;
      AScreenObject.EvaluatedAt := eaBlocks;
      AScreenObject.Visible := False;

      CellIds.Clear;
      for CellIndex := 0 to MergedList.Count - 1 do
      begin
        UzfDataItem := MergedList[CellIndex];
        CellId := UzfDataItem.PackageData.cellid;
        if Model.DisvUsed then
        begin
          CellId.Row := 1;
        end;
        CellIds.Add(CellId);
      end;

      AddPointsToScreenObject(CellIds, AScreenObject, True);

      AScreenObject.CreateUzfMf6Boundary;
      ModflowUzfMf6Boundary := AScreenObject.ModflowUzfMf6Boundary;

      StartTime := Model.ModflowStressPeriods.First.StartTime;
      EndTime := Model.ModflowStressPeriods.Last.EndTime;

      UzfMf6Item := ModflowUzfMf6Boundary.Values.Add as TUzfMf6Item;
      UzfMf6Item.StartTime := StartTime;
      UzfMf6Item.EndTime := EndTime;
      UzfMf6Item.Infiltration := '0';
      UzfMf6Item.PotentialET := '0';
      UzfMf6Item.ExtinctionDepth := '0';
      UzfMf6Item.ExtinctionWaterContent := '0';
      UzfMf6Item.AirEntryPotential := '0';
      UzfMf6Item.RootPotential := '0';
      UzfMf6Item.RootActivity := '0';

      if MergedList.Count = 1 then
      begin
        UzfDataItem := MergedList[0];
        ModflowUzfMf6Boundary.SurfaceDepressionDepth :=
          FortranFloatToStr(UzfDataItem.PackageData.surfdep);
        ModflowUzfMf6Boundary.VerticalSaturatedK :=
          FortranFloatToStr(UzfDataItem.PackageData.vks);
        ModflowUzfMf6Boundary.ResidualWaterContent :=
          FortranFloatToStr(UzfDataItem.PackageData.thtr);
        ModflowUzfMf6Boundary.SaturatedWaterContent :=
          FortranFloatToStr(UzfDataItem.PackageData.thts);
        ModflowUzfMf6Boundary.InitialWaterContent :=
          FortranFloatToStr(UzfDataItem.PackageData.thti);
        ModflowUzfMf6Boundary.BrooksCoreyEpsilon :=
          FortranFloatToStr(UzfDataItem.PackageData.eps);

        for PeriodIndex := 0 to UzfDataItem.PeriodData.Count - 1 do
        begin
          ImportedUzfPeriodItem := UzfDataItem.PeriodData[PeriodIndex];
          StartTime :=
            Model.ModflowStressPeriods[ImportedUzfPeriodItem.Period-1].StartTime;
          PData := ImportedUzfPeriodItem.PeriodData;
          if ImportedUzfPeriodItem.Period > 1 then
          begin
            UzfMf6Item.EndTime := StartTime;
            UzfMf6Item := ModflowUzfMf6Boundary.Values.Add as TUzfMf6Item;
            UzfMf6Item.StartTime := StartTime;
            UzfMf6Item.EndTime := EndTime;
          end;
          UzfMf6Item.Infiltration := BoundaryValueToFormula(PData.finf);
          UzfMf6Item.ExtinctionDepth := BoundaryValueToFormula(PData.extdp);
          UzfMf6Item.ExtinctionWaterContent := BoundaryValueToFormula(PData.extwc);
          UzfMf6Item.AirEntryPotential := BoundaryValueToFormula(PData.ha);
          UzfMf6Item.RootPotential := BoundaryValueToFormula(PData.hroot);
          UzfMf6Item.RootActivity := BoundaryValueToFormula(PData.rootact);
        end;
      end
      else
      begin
        AScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

        SurfDepthItem := AScreenObject.ImportedValues.Add;
        SurfDepthItem.Name := 'Imported_surfdep';
        SurfDepthItem.Values.DataType := rdtDouble;
        SurfDepthItem.Values.Count := MergedList.Count;

        for CellIndex := 0 to MergedList.Count - 1 do
        begin
          UzfDataItem := MergedList[CellIndex];
          SurfDepthItem.Values.RealValues[CellIndex] := UzfDataItem.PackageData.surfdep;
        end;
        ModflowUzfMf6Boundary.SurfaceDepressionDepth :=
          rsObjectImportedValuesR + '("' + SurfDepthItem.Name + '")';

        vksItem := AScreenObject.ImportedValues.Add;
        vksItem.Name := 'Imported_vks';
        vksItem.Values.DataType := rdtDouble;
        vksItem.Values.Count := MergedList.Count;

        for CellIndex := 0 to MergedList.Count - 1 do
        begin
          UzfDataItem := MergedList[CellIndex];
          vksItem.Values.RealValues[CellIndex] := UzfDataItem.PackageData.vks;
        end;
        ModflowUzfMf6Boundary.VerticalSaturatedK :=
          rsObjectImportedValuesR + '("' + vksItem.Name + '")';

        thtrItem := AScreenObject.ImportedValues.Add;
        thtrItem.Name := 'Imported_thtr';
        thtrItem.Values.DataType := rdtDouble;
        thtrItem.Values.Count := MergedList.Count;

        for CellIndex := 0 to MergedList.Count - 1 do
        begin
          UzfDataItem := MergedList[CellIndex];
          thtrItem.Values.RealValues[CellIndex] := UzfDataItem.PackageData.thtr;
        end;
        ModflowUzfMf6Boundary.ResidualWaterContent :=
          rsObjectImportedValuesR + '("' + thtrItem.Name + '")';

        thtsItem := AScreenObject.ImportedValues.Add;
        thtsItem.Name := 'Imported_thts';
        thtsItem.Values.DataType := rdtDouble;
        thtsItem.Values.Count := MergedList.Count;

        for CellIndex := 0 to MergedList.Count - 1 do
        begin
          UzfDataItem := MergedList[CellIndex];
          thtsItem.Values.RealValues[CellIndex] := UzfDataItem.PackageData.thts;
        end;
        ModflowUzfMf6Boundary.SaturatedWaterContent :=
          rsObjectImportedValuesR + '("' + thtsItem.Name + '")';

        thtiItem := AScreenObject.ImportedValues.Add;
        thtiItem.Name := 'Imported_thti';
        thtiItem.Values.DataType := rdtDouble;
        thtiItem.Values.Count := MergedList.Count;

        for CellIndex := 0 to MergedList.Count - 1 do
        begin
          UzfDataItem := MergedList[CellIndex];
          thtiItem.Values.RealValues[CellIndex] := UzfDataItem.PackageData.thti;
        end;
        ModflowUzfMf6Boundary.InitialWaterContent :=
          rsObjectImportedValuesR + '("' + thtiItem.Name + '")';

        epsItem := AScreenObject.ImportedValues.Add;
        epsItem.Name := 'Imported_eps';
        epsItem.Values.DataType := rdtDouble;
        epsItem.Values.Count := MergedList.Count;

        for CellIndex := 0 to MergedList.Count - 1 do
        begin
          UzfDataItem := MergedList[CellIndex];
          epsItem.Values.RealValues[CellIndex] := UzfDataItem.PackageData.eps;
        end;
        ModflowUzfMf6Boundary.BrooksCoreyEpsilon :=
          rsObjectImportedValuesR + '("' + epsItem.Name + '")';


        UzfDataItem := MergedList.First;
        for PeriodIndex := 0 to UzfDataItem.PeriodData.Count - 1 do
        begin

          UzfDataItem := MergedList.First;
          ImportedUzfPeriodItem := UzfDataItem.PeriodData[PeriodIndex];
          StartTime :=
            Model.ModflowStressPeriods[ImportedUzfPeriodItem.Period-1].StartTime;
          if ImportedUzfPeriodItem.Period > 1 then
          begin
            UzfMf6Item.EndTime := StartTime;
            UzfMf6Item := ModflowUzfMf6Boundary.Values.Add as TUzfMf6Item;
            UzfMf6Item.StartTime := StartTime;
            UzfMf6Item.EndTime := EndTime;
          end;

          SetLength(BoundaryValueArray, MergedList.Count);

          for CellIndex := 0 to MergedList.Count - 1 do
          begin
            UzfDataItem := MergedList[CellIndex];
            ImportedUzfPeriodItem := UzfDataItem.PeriodData[PeriodIndex];
            PData := ImportedUzfPeriodItem.PeriodData;
            BoundaryValueArray[CellIndex] := PData.finf;
          end;
          UzfMf6Item.Infiltration := BoundaryValuesToFormula(BoundaryValueArray,
            Format('Imported_finf_SP%d', [ImportedUzfPeriodItem.Period]),
            Map, AScreenObject);

          for CellIndex := 0 to MergedList.Count - 1 do
          begin
            UzfDataItem := MergedList[CellIndex];
            ImportedUzfPeriodItem := UzfDataItem.PeriodData[PeriodIndex];
            PData := ImportedUzfPeriodItem.PeriodData;
            BoundaryValueArray[CellIndex] := PData.extdp;
          end;
          UzfMf6Item.ExtinctionDepth := BoundaryValuesToFormula(BoundaryValueArray,
            Format('Imported_extdp_SP%d', [ImportedUzfPeriodItem.Period]),
            Map, AScreenObject);

          for CellIndex := 0 to MergedList.Count - 1 do
          begin
            UzfDataItem := MergedList[CellIndex];
            ImportedUzfPeriodItem := UzfDataItem.PeriodData[PeriodIndex];
            PData := ImportedUzfPeriodItem.PeriodData;
            BoundaryValueArray[CellIndex] := PData.extwc;
          end;
          UzfMf6Item.ExtinctionWaterContent := BoundaryValuesToFormula(BoundaryValueArray,
            Format('Imported_extwc_SP%d', [ImportedUzfPeriodItem.Period]),
            Map, AScreenObject);

          for CellIndex := 0 to MergedList.Count - 1 do
          begin
            UzfDataItem := MergedList[CellIndex];
            ImportedUzfPeriodItem := UzfDataItem.PeriodData[PeriodIndex];
            PData := ImportedUzfPeriodItem.PeriodData;
            BoundaryValueArray[CellIndex] := PData.ha;
          end;
          UzfMf6Item.AirEntryPotential := BoundaryValuesToFormula(BoundaryValueArray,
            Format('Imported_ha_SP%d', [ImportedUzfPeriodItem.Period]),
            Map, AScreenObject);

          for CellIndex := 0 to MergedList.Count - 1 do
          begin
            UzfDataItem := MergedList[CellIndex];
            ImportedUzfPeriodItem := UzfDataItem.PeriodData[PeriodIndex];
            PData := ImportedUzfPeriodItem.PeriodData;
            BoundaryValueArray[CellIndex] := PData.hroot;
          end;
          UzfMf6Item.RootPotential := BoundaryValuesToFormula(BoundaryValueArray,
            Format('Imported_hroot_SP%d', [ImportedUzfPeriodItem.Period]),
            Map, AScreenObject);

          for CellIndex := 0 to MergedList.Count - 1 do
          begin
            UzfDataItem := MergedList[CellIndex];
            ImportedUzfPeriodItem := UzfDataItem.PeriodData[PeriodIndex];
            PData := ImportedUzfPeriodItem.PeriodData;
            BoundaryValueArray[CellIndex] := PData.rootact;
          end;
          UzfMf6Item.RootActivity := BoundaryValuesToFormula(BoundaryValueArray,
            Format('Imported_rootact_SP%d', [ImportedUzfPeriodItem.Period]),
            Map, AScreenObject);
        end;

      end;

      ADataArray := Model.DataArrayManager.GetDataSetByName(
        StrUzfMf6BrooksCoreyEpsilon);
      FormulaPosition := AScreenObject.AddDataSet(ADataArray);
      AScreenObject.DataSetFormulas[FormulaPosition] :=
        ModflowUzfMf6Boundary.BrooksCoreyEpsilon;

      ADataArray := Model.DataArrayManager.GetDataSetByName(
        StrUzfMf6InitialUnsaturatedWaterContent);
      FormulaPosition := AScreenObject.AddDataSet(ADataArray);
      AScreenObject.DataSetFormulas[FormulaPosition] :=
        ModflowUzfMf6Boundary.InitialWaterContent;

      ADataArray := Model.DataArrayManager.GetDataSetByName(
        StrUzfMf6ReisidualWaterContent);
      FormulaPosition := AScreenObject.AddDataSet(ADataArray);
      AScreenObject.DataSetFormulas[FormulaPosition] :=
        ModflowUzfMf6Boundary.ResidualWaterContent;

      ADataArray := Model.DataArrayManager.GetDataSetByName(
        StrUzfMf6SaturatedWaterContent);
      FormulaPosition := AScreenObject.AddDataSet(ADataArray);
      AScreenObject.DataSetFormulas[FormulaPosition] :=
        ModflowUzfMf6Boundary.SaturatedWaterContent;

      ADataArray := Model.DataArrayManager.GetDataSetByName(
        StrUzfMf6SurfaceDepressionDepth);
      FormulaPosition := AScreenObject.AddDataSet(ADataArray);
      AScreenObject.DataSetFormulas[FormulaPosition] :=
        ModflowUzfMf6Boundary.SurfaceDepressionDepth;

      ADataArray := Model.DataArrayManager.GetDataSetByName(
        StrUzfMf6VerticalSaturatedK);
      FormulaPosition := AScreenObject.AddDataSet(ADataArray);
      AScreenObject.DataSetFormulas[FormulaPosition] :=
        ModflowUzfMf6Boundary.VerticalSaturatedK;

      UzfDataItem := MergedList.First;
      if UzfDataItem.MvrSource then
      begin
        MvrSource.ScreenObject := AScreenObject;
        MvrSource.PackageName := Package.PackageName;
        SetLength(MvrSource.IDs, 1);
        MvrSource.SourceType := mspcUzf;
        MvrSource.IDs[0] := UzfDataItem.PackageData.iuzno;
        for StressPeriodIndex := 0 to UzfMvrLinkList.Count - 1 do
        begin
          MvrSource.Period := UzfMvrLinkList[StressPeriodIndex].Period;
          FMvrSources.Add(MvrSource);
        end;
      end;

      if UzfDataItem.MvrReceiver then
      begin
        MvrReceiver.ScreenObject := AScreenObject;
        MvrReceiver.PackageName := Package.PackageName;
        SetLength(MvrReceiver.IDs, 1);
        MvrReceiver.ReceiverType := mrpcLak;
        MvrReceiver.IDs[0] := UzfDataItem.PackageData.iuzno;
        for StressPeriodIndex := 0 to UzfMvrLinkList.Count - 1 do
        begin
          MvrReceiver.Period := UzfMvrLinkList[StressPeriodIndex].Period;
          FMvrReceivers.Add(MvrReceiver);
        end;
      end;
      AssignObservations(UzfDataItem.BoundNameObs, AScreenObject, UzfDataItem.PackageData.boundname);
      AssignObservations(UzfDataItem.NumberObs, AScreenObject);
    end;
  finally
    UzfReceivers.Free;
    UzfSources.Free;
    UzfMvrLinkList.Free;
    UzfData.Free;
    MergedUzfData.Free;
    Map.Free;
    NumberObsDictionary.Free;
    BoundNameObsDictionary.Free;
    ObsLists.Free;
    CellIds.Free;
  end;
end;

procedure TModflow6Importer.ImportVsc(Package: TPackage);
var
  Vsc: TVsc;
  Model: TPhastModel;
  ViscosityPackage: TViscosityPackage;
  Options: TVscOptions;
  PackageData: TVscPackageData;
  index: Integer;
  Item: TVscItem;
  ChemComponents: TMobileChemSpeciesCollection;
  ChemItem: TMobileChemSpeciesItem;
begin
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing VSC package');
  end;
  Model := frmGoPhast.PhastModel;
  ViscosityPackage := Model.ModflowPackages.ViscosityPackage;
  ViscosityPackage.IsSelected := True;
  ViscosityPackage.ViscositySpecified := False;

  Vsc := Package.Package as TVsc;
  Options := Vsc.Options;

  if Options.VISCREF.Used then
  begin
    ViscosityPackage.RefViscosity := Options.VISCREF.Value;
  end;
  if Options.TEMPERATURE_SPECIES_NAME.Used then
  begin
    ViscosityPackage.ThermalSpecies := Options.TEMPERATURE_SPECIES_NAME.Value;
  end;
  if Options.THERMAL_FORMULATION.Used then
  begin
    if Options.THERMAL_FORMULATION.Value = 'LINEAR' then
    begin
      ViscosityPackage.ThermalFormulation := tfLinear;
    end
    else if Options.THERMAL_FORMULATION.Value = 'NONLINEAR' then
    begin
      ViscosityPackage.ThermalFormulation := tfNonLinear;
    end
    else
    begin
      Assert(False)
    end;
  end;
  if Options.THERMAL_A2.Used then
  begin
    ViscosityPackage.ThermalA2 := Options.THERMAL_A2.Value;
  end;
  if Options.THERMAL_A3.Used then
  begin
    ViscosityPackage.ThermalA3 := Options.THERMAL_A3.Value;
  end;
  if Options.THERMAL_A4.Used then
  begin
    ViscosityPackage.ThermalA4 := Options.THERMAL_A4.Value;
  end;

  ChemComponents := Model.MobileComponents;
  PackageData := Vsc.PackageData;
  for index := 0 to PackageData.Count - 1 do
  begin
    Item := PackageData[index];
    ChemItem := ChemComponents.GetItemByName(Item.auxspeciesname);
    if ChemItem = nil then
    begin
      ChemItem := ChemComponents.Add;
      ChemItem.Name := Item.auxspeciesname;
    end;
    if SameText(ChemItem.Name, 'Viscosity') then
    begin
      ViscosityPackage.ViscositySpecified := True;
    end;
    ChemItem.RefViscosity := Item.cviscref;
    ChemItem.ViscositySlope := Item.dviscdc;
  end;
end;

type
  TWelConnection = class(TObject)
    ScreenObject: TScreenObject;
    List: TWelTimeItemList;
    IFACE: Integer;
    destructor Destroy; override;
  end;

  TWelConnectionObjectList = TObjectList<TWelConnection>;
  TWelConnectionObjectLists = TObjectList<TWelConnectionObjectList>;

  TWellMvrLink = record
    WelPeriod: TWelPeriod;
    MvrPeriod: TMvrPeriod;
    function Period: Integer;
  end;
  TWellMvrLinkArray = TArray<TWellMvrLink>;
  TWellMvrLinkList = TList<TWellMvrLink>;

  TMvrWelTimeItemList = Class(TWelTimeItemList)
    FIds: TGenericIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure Sort;
  end;

procedure TModflow6Importer.ImportWel(Package: TPackage;
  TransportModels: TModelList; MvrPackage: TPackage);
var
  Model: TPhastModel;
  Wel: TWel;
  AModel: TModel;
  ModelIndex: Integer;
  TransportModel: TTransportNameFile;
  PackageIndex: Integer;
  BoundNameObsDictionary: TBoundNameDictionary;
  CellIdObsDictionary: TCellIdObsDictionary;
  Ssm: TSsm;
  TimeSeriesIndex: Integer;
  TimeSeriesPackage: TPackage;
  Map: TimeSeriesMap;
  ObsLists: TObsLists;
  ObsPackageIndex: Integer;
  ObsFiles: TObs;
  SourceIndex: Integer;
  FoundMatch: Boolean;
  TransportAuxNames: TStringList;
  PeriodIndex: Integer;
  APeriod: TWelPeriod;
  CellIndex: Integer;
  ACell: TWelTimeItem;
  IfaceIndex: Integer;
  KeyStringDictionary: TDictionary<string, TMvrWelTimeItemList>;
  CellLists: TObjectList<TMvrWelTimeItemList>;
  ACellList: TMvrWelTimeItemList;
  Options: TWelOptions;
  IFace: Integer;
  LastTime: Double;
  Imported_Heads: TValueArrayItem;
  ItemList: TList<TWellItem>;
  StartTime: Double;
  WelIndex: Integer;
  AnItem: TWellItem;
  KeyString: string;
  TimeSeries: string;
  ImportedTimeSeries: string;
  ObjectCount: Integer;
  ObjectIndex: Integer;
  FirstCell: TWelTimeItem;
  BoundName: string;
  ConnectionObjectLists: TWelConnectionObjectLists;
  ConnectionDictionary: TDictionary<string, TWelConnectionObjectList>;
  AConnectionList: TWelConnectionObjectList;
  ConnectionIndex: Integer;
  ConnectionItem: TWelConnection;
  AScreenObject: TScreenObject;
  AuxIFACE: TMf6BoundaryValue;
  AuxIndex: Integer;
  ChemSpeciesName: string;
  Aux: TMf6BoundaryValue;
  GwtAuxIndex: Integer;
  Values: TValueArrayStorage;
  CellIds: TCellIdList;
  Mvr: TMvr;
  Index: Integer;
  WellMvrLink: TWellMvrLink;
  WellMvrLinkList: TWellMvrLinkList;
  MvrPeriod: TMvrPeriod;
  MvrUsed: Boolean;
  NewScreenObject: Boolean;
  MvrSource: TMvrSource;
  APackage: TPackage;
  AuxMultIndex: Integer;
  AuxMultiplier: Extended;
  WellMvrLinkArray: TWellMvrLinkArray;
  WellPeriod: TWelPeriod;
  NextWelPeriod: TWelPeriod;
  EndPeriod: Integer;
  NextMvrPeriod: TMvrPeriod;
  OtherCellLists: TObjectList<TMvrWelTimeItemList>;
  CellListIndex: Integer;
  procedure AddItem(AScreenObject: TScreenObject; ACell: TWelTimeItem; Period: Integer);
  var
    WelItem: TWellItem;
    ImportedName: string;
    Concentrations: TWelGwtConcCollection;
    ChemSpeciesName: string;
    ConcItem: TGwtConcStringValueItem;
    GwtAuxIndex: Integer;
    AuxIndex: Integer;
    Aux: TMf6BoundaryValue;
    Imported_Chem: TValueArrayItem;
    AuxMultiplier: Extended;
  begin
    WelItem := AScreenObject.ModflowWellBoundary.Values.Add as TWellItem;
    ItemList.Add(WelItem);
    WelItem.EndTime := LastTime;
    WelItem.StartTime := StartTime;

    if AuxMultIndex >= 0 then
    begin
      Aux := ACell.Aux[AuxMultIndex];
      if Aux.ValueType = vtNumeric then
      begin
        AuxMultiplier := Aux.NumericValue
      end
      else
      begin
        AuxMultiplier := 1;
        FErrorMessages.Add(StrModelMuseCanNotIm);
      end;
    end
    else
    begin
      AuxMultiplier := 1;
    end;

    if ACell.Q.ValueType = vtNumeric then
    begin
      ImportedName := Format('Imported_Heads_Period_%d', [Period]);
      Imported_Heads := AScreenObject.ImportedValues.Add;
      Imported_Heads.Name := ImportedName;
      Imported_Heads.Values.DataType := rdtDouble;
      WelItem.PumpingRate := rsObjectImportedValuesR + '("' + Imported_Heads.Name + '")';
    end
    else
    begin
      Imported_Heads := nil;
      TimeSeries := ACell.Q.StringValue;
      if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      WelItem.PumpingRate := ImportedTimeSeries;
      if AuxMultiplier <> 1 then
      begin
        FErrorMessages.Add(StrModelMuseCanNotAp);
      end;
    end;

    if TransportAuxNames.Count > 0 then
    begin
      Concentrations := WelItem.GwtConcentrations;
      Concentrations.Count := TransportAuxNames.Count;
      for AuxIndex := 0 to TransportAuxNames.Count - 1 do
      begin
        ChemSpeciesName := TransportAuxNames[AuxIndex];
        ConcItem := Concentrations[AuxIndex];

        GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
        Assert(GwtAuxIndex >= 0);
        Aux := ACell[GwtAuxIndex];
        if Aux.ValueType = vtNumeric then
        begin
          ImportedName := Format('Imported_%s_Period_%d', [ChemSpeciesName, Period]);
          Imported_Chem := AScreenObject.ImportedValues.Add;
          Imported_Chem.Name := ImportedName;
          Imported_Chem.Values.DataType := rdtDouble;
          ConcItem.Value := rsObjectImportedValuesR + '("' + Imported_Chem.Name + '")';
          TransportAuxNames.Objects[AuxIndex] := Imported_Chem.Values;
        end
        else
        begin
          TransportAuxNames.Objects[AuxIndex] := nil;
          TimeSeries := Aux.StringValue;
          if not Map.TryGetValue(UpperCase(TimeSeries), ImportedTimeSeries) then
          begin
            Assert(False);
          end;
          ConcItem.Value := ImportedTimeSeries;
        end;
      end;
    end;
  end;
  procedure CreateObsScreenObject(ACell: TWelTimeItem);
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    CellId: TMfCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Wel_Obs_%d', [Package.PackageName, ObjectCount]));
    AScreenObject.Name := NewName;
    AScreenObject.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(AScreenObject);
    AScreenObject.ElevationCount := ecOne;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.EvaluatedAt := eaBlocks;
    AScreenObject.Visible := False;
    AScreenObject.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
    AScreenObject.CreateMf6Obs;
    AScreenObject.Modflow6Obs.Name := ACell.Boundname;
    AScreenObject.Modflow6Obs.General := [ogWell];

    CellId := ACell.Cellid;
    if Model.DisvUsed then
    begin
      CellId.Row := 1;
    end;
    ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
    APoint.x := ElementCenter.RotatedLocation.x;
    APoint.y := ElementCenter.RotatedLocation.y;
    AScreenObject.AddPoint(APoint, True);
    AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);

  end;
  function CreateScreenObject(ACell: TWelTimeItem; Period: Integer): TScreenObject;
  var
    UndoCreateScreenObject: TCustomUndo;
    NewName: string;
    AuxIFACE: TMf6BoundaryValue;
    BoundName: string;
    ObsList: TObservationList;
    AnObs: TObservation;
    ObsIndex: Integer;
    General: TObGenerals;
  begin
    Inc(ObjectCount);
    result := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := ValidName(Format('Imported_%s_Wel_%d_Period_%d', [Package.PackageName, ObjectCount, Period]));
    result.Name := NewName;
    result.Comment := 'Imported from ' + FModelNameFile +' on ' + DateTimeToStr(Now);

    Model.AddScreenObject(result);
    result.ElevationCount := ecOne;
    result.SetValuesOfIntersectedCells := True;
    result.EvaluatedAt := eaBlocks;
    result.Visible := False;
    result.ElevationFormula := rsObjectImportedValuesR + '("' + StrImportedElevations + '")';

    result.CreateWelBoundary;
    if IfaceIndex >= 0 then
    begin
      AuxIFACE := ACell[IfaceIndex];
      Assert(AuxIFACE.ValueType = vtNumeric);
      IFACE := Round(AuxIFACE.NumericValue);
    end
    else
    begin
      IFACE := 0;
    end;
    result.IFACE := TIface(IFACE+2);

    result.ModflowWellBoundary.FormulaInterpretation := fiDirect;
    AddItem(result, ACell, Period);

    BoundName := UpperCase(ACell.Boundname);
    if BoundNameObsDictionary.TryGetValue(BoundName, ObsList) then
    begin
      Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      result.CreateMf6Obs;
      General := [];
      for ObsIndex := 0 to ObsList.Count - 1 do
      begin
        AnObs := ObsList[ObsIndex];
        if AnsiSameText(AnObs.ObsType, 'wel') then
        begin
          Include(General, ogWell);
        end
        else if AnsiSameText(AnObs.ObsType, 'to-mvr') then
        begin
          Include(General, ogMvr);
        end
        else if AnsiSameText(AnObs.ObsType, 'wel-reduction') then
        begin
          Include(General, ogWellReduction);
        end
        else
        begin
          Assert(False);
        end;
      end;
      result.Modflow6Obs.Name := ACell.Boundname;
      result.Modflow6Obs.General := General;
    end;
  end;
begin
  MvrSource.LakeOutlet := nil;
  if Assigned(OnUpdateStatusBar) then
  begin
    OnUpdateStatusBar(self, 'importing WEL package');
  end;
  // Get the MVR package.
  Mvr := GetMvr(MvrPackage, Package);

  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.WelPackage.IsSelected := True;

  Wel := Package.Package as TWel;
  Options := Wel.Options;

  if Options.AUXMULTNAME <> '' then
  begin
    AuxMultIndex := Options.IndexOfAUXILIARY(Options.AUXMULTNAME);
  end
  else
  begin
    AuxMultIndex := -1;
  end;

  OtherCellLists := TObjectList<TMvrWelTimeItemList>.Create;
  WellMvrLinkList := TWellMvrLinkList.Create;
  CellIds := TCellIdList.Create;
  ConnectionObjectLists := TWelConnectionObjectLists.Create;
  ConnectionDictionary := TDictionary<string, TWelConnectionObjectList>.Create;
  ItemList := TList<TWellItem>.Create;
  BoundNameObsDictionary := TBoundNameDictionary.Create;
  CellIdObsDictionary := TCellIdObsDictionary.Create;
  Map := TimeSeriesMap.Create;
  ObsLists := TObsLists.Create;
  KeyStringDictionary := TDictionary<string, TMvrWelTimeItemList>.Create;
  CellLists := TObjectList<TMvrWelTimeItemList>.Create;
  try
    OtherCellLists.OwnsObjects := False;
    try
      if Mvr = nil then
      begin
        WellMvrLink.MvrPeriod := nil;
        for PeriodIndex := 0 to Wel.PeriodCount - 1 do
        begin
          WellMvrLink.WelPeriod := Wel.Periods[PeriodIndex];
          WellMvrLinkList.Add(WellMvrLink)
        end;
      end
      else
      begin
        // Make sure that all the stress periods defined in either the MVR or the
        // WEL package are imported.
        SetLength(WellMvrLinkArray, Model.ModflowStressPeriods.Count);
        for PeriodIndex := 0 to Length(WellMvrLinkArray) - 1 do
        begin
          WellMvrLinkArray[PeriodIndex].WelPeriod := nil;
          WellMvrLinkArray[PeriodIndex].MvrPeriod := nil;
        end;

        for PeriodIndex := 0 to Wel.PeriodCount - 1 do
        begin
          WellPeriod := Wel.Periods[PeriodIndex];
          if PeriodIndex < Wel.PeriodCount - 1 then
          begin
            NextWelPeriod := Wel.Periods[PeriodIndex+1];
            EndPeriod := NextWelPeriod.Period;
          end
          else
          begin
            EndPeriod := Model.ModflowStressPeriods.Count;
          end;
          for Index := WellPeriod.Period  to EndPeriod do
          begin
            WellMvrLinkArray[Index-1].WelPeriod  := WellPeriod;
          end;
        end;

        for PeriodIndex := 0 to Mvr.PeriodCount - 1 do
        begin
          MvrPeriod := Mvr.Periods[PeriodIndex];
          if PeriodIndex < Mvr.PeriodCount - 1 then
          begin
            NextMvrPeriod := Mvr.Periods[PeriodIndex+1];
            EndPeriod := NextMvrPeriod.Period;
          end
          else
          begin
            EndPeriod := Model.ModflowStressPeriods.Count;
          end;
          for Index := MvrPeriod.Period  to EndPeriod do
          begin
            WellMvrLinkArray[Index-1].MvrPeriod  := MvrPeriod;
          end;
        end;

        WellMvrLinkList.Add(WellMvrLinkArray[0]);
        for Index := 1 to Length(WellMvrLinkArray) - 1 do
        begin
          if (WellMvrLinkArray[Index].WelPeriod <> WellMvrLinkArray[Index-1].WelPeriod)
            or (WellMvrLinkArray[Index].MvrPeriod <> WellMvrLinkArray[Index-1].MvrPeriod) then
          begin
            WellMvrLinkList.Add(WellMvrLinkArray[Index]);
          end;
        end;
      end;

      IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
      for TimeSeriesIndex := 0 to Wel.TimeSeriesCount - 1 do
      begin
        TimeSeriesPackage := Wel.TimeSeries[TimeSeriesIndex];
        ImportTimeSeries(TimeSeriesPackage, Map);
      end;

      if Wel.ObservationCount > 0 then
      begin
        Model.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
      end;
      for ObsPackageIndex := 0 to Wel.ObservationCount - 1 do
      begin
        ObsFiles := Wel.Observations[ObsPackageIndex].Package as TObs;
        GetObservations(nil, BoundNameObsDictionary,
          CellIdObsDictionary, ObsLists, ObsFiles);
      end;

      if Assigned(OnUpdateStatusBar) then
      begin
        OnUpdateStatusBar(self, 'importing WEL package');
      end;

      TransportAuxNames := TStringList.Create;
      try
        TransportAuxNames.CaseSensitive := False;
        for ModelIndex := 0 to TransportModels.Count - 1 do
        begin
          AModel := TransportModels[ModelIndex];
          TransportModel := AModel.FName as TTransportNameFile;
          for PackageIndex := 0 to TransportModel.NfPackages.Count  - 1 do
          begin
            APackage := TransportModel.NfPackages[PackageIndex];
            FoundMatch := False;
            if APackage.FileType = 'SSM6' then
            begin
              Ssm := APackage.Package as TSsm;
              for SourceIndex := 0 to Ssm.Sources.Count - 1 do
              begin
                if SameText(Ssm.Sources[SourceIndex].pname, Package.PackageName) then
                begin
                  FoundMatch := True;
                  TransportAuxNames.Add(Ssm.Sources[SourceIndex].auxname);
                  break;
                end;
              end;
  //            Assert(FoundMatch);
              break;
            end;
          end;
        end;

        LastTime := Model.ModflowStressPeriods.Last.EndTime;

        ACellList := nil;
        ObjectCount := 0;
        for PeriodIndex := 0 to WellMvrLinkList.Count - 1 do
        begin
          WellMvrLink := WellMvrLinkList[PeriodIndex];
          APeriod := WellMvrLinkList[PeriodIndex].WelPeriod;
          if APeriod = nil then
          begin
            Continue;
          end;
          StartTime := Model.ModflowStressPeriods[WellMvrLink.Period-1].StartTime;
          for WelIndex := 0 to ItemList.Count - 1 do
          begin
            AnItem := ItemList[WelIndex];
            AnItem.EndTime := StartTime;
          end;
          ItemList.Clear;
          for CellListIndex := 0 to CellLists.Count - 1 do
          begin
            CellLists[CellListIndex].Clear;
          end;

          // Assign all cells in the current period to a cell list.
          for CellIndex := 0 to APeriod.Count - 1 do
          begin
            ACell := APeriod[CellIndex];

            if (ACell.Boundname <> '')
              and BoundNameObsDictionary.ContainsKey(UpperCase(ACell.Boundname)) then
            begin
              KeyString := 'BN:' + UpperCase(ACell.Boundname) + ' ';
            end
            else
            begin
              KeyString := '';
            end;

            if IfaceIndex < 0 then
            begin
              IFACE := 0;
            end
            else
            begin
              AuxIFACE := ACell[IfaceIndex];
              Assert(AuxIFACE.ValueType = vtNumeric);
              IFACE := Round(AuxIFACE.NumericValue);
            end;
            KeyString := KeyString + ACell.Keystring + ' IFACE:' + IntToStr(IFACE);

            MvrUsed := False;
            if WellMvrLink.MvrPeriod <> nil then
            begin
              if WellMvrLink.MvrPeriod.HasSource(Package.PackageName, ACell.Id) then
              begin
                KeyString := KeyString + ' MVR ' + InttoStr(ACell.Id);
                MvrUsed := True;
              end;
            end;

            if not KeyStringDictionary.TryGetValue(KeyString, ACellList) then
            begin
              ACellList := TMvrWelTimeItemList.Create;
              CellLists.Add(ACellList);
              KeyStringDictionary.Add(KeyString, ACellList);
            end;
            ACellList.Add(ACell);
            if MvrUsed then
            begin
              if ACellList.FIds.IndexOf(ACell.Id) < 0 then
              begin
                ACellList.FIds.Add(ACell.Id);
              end;
            end;
          end;

          // After all the cells in the current period have been read,
          // create a TScreenObject for each cell list
          AScreenObject := nil;
          for ObjectIndex := 0 to CellLists.Count - 1 do
          begin
            NewScreenObject := False;
            ACellList := CellLists[ObjectIndex];
            if ACellList.Count > 0 then
            begin
              FirstCell := ACellList[0];
              if (FirstCell.Boundname <> '')
                and BoundNameObsDictionary.ContainsKey(UpperCase(FirstCell.Boundname)) then
              begin
                if IfaceIndex < 0 then
                begin
                  IFACE := 0;
                end
                else
                begin
                  AuxIFACE := FirstCell[IfaceIndex];
                  Assert(AuxIFACE.ValueType = vtNumeric);
                  IFACE := Round(AuxIFACE.NumericValue);
                end;
                BoundName := UpperCase(FirstCell.Boundname);
                if not ConnectionDictionary.TryGetValue(BoundName, AConnectionList) then
                begin
                  AConnectionList := TWelConnectionObjectList.Create;
                  ConnectionObjectLists.Add(AConnectionList);
                  ConnectionDictionary.Add(BoundName, AConnectionList)
                end;
                ACellList.Sort;
                AScreenObject := nil;
                for ConnectionIndex := 0 to AConnectionList.Count - 1 do
                begin
                  ConnectionItem := AConnectionList[ConnectionIndex];
                  if (IFACE = ConnectionItem.IFACE)
                    and ACellList.SameCells(ConnectionItem.List) then
                  begin
                    AScreenObject := ConnectionItem.ScreenObject;
                    Break;
                  end;
                end;
                if AScreenObject = nil then
                begin
                  AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
                  ConnectionItem := TWelConnection.Create;
                  ConnectionItem.ScreenObject := AScreenObject;
                  ConnectionItem.IFACE := IFACE;
                  ConnectionItem.List := ACellList;
                  AConnectionList.Add(ConnectionItem);
                  OtherCellLists.Add(ACellList);
  //                CellLists.OwnsObjects := False;
  //                try
  //                  CellLists[ObjectIndex] := nil;
  //                finally
  //                  CellLists.OwnsObjects := True;
  //                end;
                  NewScreenObject := True;
                end
                else
                begin
                  AddItem(AScreenObject, FirstCell, APeriod.Period);
                end;
              end
              else
              begin
                AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
                NewScreenObject := True;
              end;
            end;

            CellIds.Clear;
            for CellIndex := 0 to ACellList.Count - 1 do
            begin
              ACell := ACellList[CellIndex];
              if ACell.Q.ValueType = vtNumeric then
              begin
                if AuxMultIndex >= 0 then
                begin
                  Aux := ACell.Aux[AuxMultIndex];
                  if Aux.ValueType = vtNumeric then
                  begin
                    AuxMultiplier := Aux.NumericValue
                  end
                  else
                  begin
                    AuxMultiplier := 1;
                    FErrorMessages.Add(StrModelMuseCanNotIm);
                  end;
                end
                else
                begin
                  AuxMultiplier := 1;
                end;
                Imported_Heads.Values.Add(ACell.Q.NumericValue * AuxMultiplier);
              end;

              for AuxIndex := 0 to TransportAuxNames.Count - 1 do
              begin
                ChemSpeciesName := TransportAuxNames[AuxIndex];
                GwtAuxIndex := Options.IndexOfAUXILIARY(ChemSpeciesName);
                Aux := ACell[GwtAuxIndex];
                if Aux.ValueType = vtNumeric then
                begin
                  Values := TransportAuxNames.Objects[AuxIndex] as TValueArrayStorage;
                  Values.Add(Aux.NumericValue);
                end;
              end;

              if NewScreenObject then
              begin
                CellIds.Add(ACell.Cellid);
              end;

              if CellIdObsDictionary.ContainsKey(ACell.Cellid) then
              begin
                CreateObsScreenObject(ACell);
              end;
            end;

            if NewScreenObject then
            begin
              AddPointsToScreenObject(CellIds, AScreenObject);
            end;

            if ACellList.FIds.Count > 0 then
            begin
              MvrSource.ScreenObject := AScreenObject;
              MvrSource.PackageName := Package.PackageName;
              MvrSource.Period := WellMvrLink.Period;
              MvrSource.IDs := ACellList.FIds.ToArray;
              MvrSource.SourceType := mspcWel;
              FMvrSources.Add(MvrSource);
            end;
          end;
        end;

      finally
        TransportAuxNames.Free;
      end;

    finally
      for CellListIndex := 0 to OtherCellLists.Count - 1 do
      begin
        CellLists.Extract(OtherCellLists[CellListIndex])
      end;
    end;
  finally
    BoundNameObsDictionary.Free;
    CellIdObsDictionary.Free;
    Map.Free;
    ObsLists.Free;
    KeyStringDictionary.Free;
    CellLists.Free;
    ItemList.Free;
    ConnectionObjectLists.Free;
    ConnectionDictionary.Free;
    CellIds.Free;
    WellMvrLinkList.Free;
    OtherCellLists.Free;
  end;
end;

procedure TModflow6Importer.SetOnUpdataStatusBar(
  const Value: TOnUpdataStatusBar);
begin
  FOnUpdataStatusBar := Value;
end;

{ TConnection }

destructor TChdConnection.Destroy;
begin
  List.Free;
  inherited;
end;

{ TWelConnection }

destructor TWelConnection.Destroy;
begin
  List.Free;
  inherited;
end;

{ TWellMvrLink }

function TWellMvrLink.Period: Integer;
begin
  if MvrPeriod = nil then
  begin
    result := WelPeriod.Period;
  end
  else if WelPeriod = nil then
  begin
    result := MvrPeriod.Period;
  end
  else
  begin
    Result := Max(WelPeriod.Period, MvrPeriod.Period);
  end;
end;

{ TMvrWelTimeItemList }

constructor TMvrWelTimeItemList.Create;
begin
  inherited;
  OwnsObjects := False;
  FIds := TGenericIntegerList.Create;
end;

destructor TMvrWelTimeItemList.Destroy;
begin
  FIds.Free;
  inherited;
end;

procedure TMvrWelTimeItemList.Sort;
var
  Index: Integer;
begin
  inherited Sort;
  if FIds.Count > 0 then
  begin
    FIds.Clear;
    FIds.Capacity := Count;
    for Index := 0 to Count - 1 do
    begin
      FIds.Add(Items[Index].Id);
    end;
  end;
end;

{ TMvrSourceList }

procedure TMvrSourceList.Sort;
begin
  inherited Sort(
    TComparer<TMvrSource>.Construct(
      function(const Left, Right: TMvrSource): Integer
      begin
        Result := Left.Period - Right.Period;
        if Result = 0 then
        begin
          Result := Ord(Left.SourceType) - Ord(Right.SourceType);
          if Result = 0 then
          begin
            Result := AnsiCompareText(Left.PackageName, Right.PackageName);
          end;
        end;
      end
    ));
end;

{ TMvrReceiverList }

procedure TMvrReceiverList.Sort;
begin
  inherited Sort(
    TComparer<TMvrReceiver>.Construct(
      function(const Left, Right: TMvrReceiver): Integer
      begin
        Result := Left.Period - Right.Period;
        if Result = 0 then
        begin
          Result := Ord(Left.ReceiverType) - Ord(Right.ReceiverType);
          if Result = 0 then
          begin
            Result := AnsiCompareText(Left.PackageName, Right.PackageName);
          end;
        end;
      end
    ));
end;

{ TDrnConnection }

destructor TDrnConnection.Destroy;
begin
  List.Free;
  inherited;
end;

{ TMvrDrnTimeItemList }

constructor TMvrDrnTimeItemList.Create;
begin
  inherited;
  OwnsObjects := False;
  FIds := TGenericIntegerList.Create;
end;

destructor TMvrDrnTimeItemList.Destroy;
begin
  FIds.Free;
  inherited;
end;

procedure TMvrDrnTimeItemList.Sort;
var
  Index: Integer;
begin
  inherited Sort;
  if FIds.Count > 0 then
  begin
    FIds.Clear;
    FIds.Capacity := Count;
    for Index := 0 to Count - 1 do
    begin
      FIds.Add(Items[Index].Id);
    end;
  end;
end;

{ TDrnlMvrLink }

function TDrnMvrLink.Period: Integer;
begin
  if MvrPeriod = nil then
  begin
    result := DrnPeriod.Period;
  end
  else if DrnPeriod = nil then
  begin
    result := MvrPeriod.Period;
  end
  else
  begin
    Result := Max(DrnPeriod.Period, MvrPeriod.Period);
  end;
end;

{ TRivConnection }

destructor TRivConnection.Destroy;
begin
  List.Free;
  inherited;
end;

{ TRivlMvrLink }

function TRivMvrLink.Period: Integer;
begin
  if MvrPeriod = nil then
  begin
    result := RivPeriod.Period;
  end
  else if RivPeriod = nil then
  begin
    result := MvrPeriod.Period;
  end
  else
  begin
    Result := Max(RivPeriod.Period, MvrPeriod.Period);
  end;
end;

{ TMvrRivTimeItemList }

constructor TMvrRivTimeItemList.Create;
begin
  inherited;
  OwnsObjects := False;
  FIds := TGenericIntegerList.Create;
end;

destructor TMvrRivTimeItemList.Destroy;
begin
  FIds.Free;
  inherited;
end;

procedure TMvrRivTimeItemList.Sort;
var
  Index: Integer;
begin
  inherited Sort;
  if FIds.Count > 0 then
  begin
    FIds.Clear;
    FIds.Capacity := Count;
    for Index := 0 to Count - 1 do
    begin
      FIds.Add(Items[Index].Id);
    end;
  end;
end;

{ TGhbConnection }

destructor TGhbConnection.Destroy;
begin
  List.Free;
  inherited;
end;

{ TGhblMvrLink }

function TGhbMvrLink.Period: Integer;
begin
  if MvrPeriod = nil then
  begin
    result := GhbPeriod.Period;
  end
  else if GhbPeriod = nil then
  begin
    result := MvrPeriod.Period;
  end
  else
  begin
    Result := Max(GhbPeriod.Period, MvrPeriod.Period);
  end;
end;

{ TMvrGhbTimeItemList }

constructor TMvrGhbTimeItemList.Create;
begin
  inherited;
  OwnsObjects := False;
  FIds := TGenericIntegerList.Create;
end;

destructor TMvrGhbTimeItemList.Destroy;
begin
  FIds.Free;
  inherited;
end;

procedure TMvrGhbTimeItemList.Sort;
var
  Index: Integer;
begin
  inherited Sort;
  if FIds.Count > 0 then
  begin
    FIds.Clear;
    FIds.Capacity := Count;
    for Index := 0 to Count - 1 do
    begin
      FIds.Add(Items[Index].Id);
    end;
  end;

end;

{ TRchConnection }

destructor TRchConnection.Destroy;
begin
  List.Free;
  inherited;
end;

{ TEvtConnection }

destructor TEvtConnection.Destroy;
begin
  List.Free;
  inherited;
end;

{ TSfrReachInfo }

function TSfrReachInfo.Compatible(OtherInfo: TSfrReachInfo): Boolean;
begin
  result := (CrossSectionFile = OtherInfo.CrossSectionFile)
    and (BoundNameObs = OtherInfo.BoundNameObs)
    and (IdObs = nil)
    and (OtherInfo.IdObs = nil)
end;

constructor TSfrReachInfo.Create;
begin
  inherited;
  Diversions := TSfrDiversionItemList.Create;
end;

destructor TSfrReachInfo.Destroy;
begin
  Diversions.Free;
  inherited;
end;

function TSfrReachInfo.DownstreamReachCount: Integer;
var
  Index: Integer;
begin
  result := 0;
  for Index := 0 to Length(Connections.ic) - 1 do
  begin
    if Connections.ic[Index] < 0 then
    begin
      Inc(result);
    end;
  end;
end;

function TSfrReachInfo.UpstreamReachCount: Integer;
var
  Index: Integer;
begin
  result := 0;
  for Index := 0 to Length(Connections.ic) - 1 do
  begin
    if Connections.ic[Index] > 0 then
    begin
      Inc(result);
    end;
  end;
end;

{ TImportLake }

constructor TImportLake.Create(LakPackageItem: TLakPackageItem);
begin
  inherited Create;
  FLakPackageItem := LakPackageItem;
  FConnections := TLakConnectionItemList.Create;
  FOutlets := TLakOutletItemList.Create;
  DataSetsScreenObject := nil;
  FLakeSettings := TNumberedItemList.Create;
  FOutletSettings := TOutletDictionary.Create;
  FNumberedItemLists := TNumberedItemLists.Create;
  HasOutletSettings := False;
end;

destructor TImportLake.Destroy;
begin
  FNumberedItemLists.Free;
  FLakeSettings.Free;
  FOutletSettings.Free;
  FOutlets.Free;
  FConnections.Free;
  inherited;
end;

{ TMvrLinks }

//constructor TMvrLinks.Create;
//begin
//  inherited;
//  WellDonors := TMvrDictionarys.Create;
//  DrainDonors := TMvrDictionarys.Create;
//  GhbDonors := TMvrDictionarys.Create;
//  RivDonors := TMvrDictionarys.Create;
//  MawDonors := TMvrDictionarys.Create;
//  LakDonors := TMvrDictionarys.Create;
//  SfrDonors := TMvrDictionarys.Create;
//  UzfDonors := TMvrDictionarys.Create;
//  MawReceivers := TMvrDictionarys.Create;
//  LakReceivers := TMvrDictionarys.Create;
//  SfrReceivers := TMvrDictionarys.Create;
//  UzfReceivers := TMvrDictionarys.Create;
//
//end;

//destructor TMvrLinks.Destroy;
//begin
//  WellDonors.Free;
//  DrainDonors.Free;
//  GhbDonors.Free;
//  RivDonors.Free;
//  MawDonors.Free;
//  LakDonors.Free;
//  SfrDonors.Free;
//  UzfDonors.Free;
//  MawReceivers.Free;
//  LakReceivers.Free;
//  SfrReceivers.Free;
//  UzfReceivers.Free;
//
//  inherited;
//end;

{ TMawMvrLink }

function TMawMvrLink.Period: Integer;
begin
  if MvrPeriod = nil then
  begin
    result := MawPeriod.Period;
  end
  else if MawPeriod = nil then
  begin
    result := MvrPeriod.Period;
  end
  else
  begin
    Result := Max(MawPeriod.Period, MvrPeriod.Period);
  end;
end;

{ TSfrMvrLink }

function TSfrMvrLink.Period: Integer;
begin
  if MvrPeriod = nil then
  begin
    result := SfrPeriod.Period;
  end
  else if SfrPeriod = nil then
  begin
    result := MvrPeriod.Period;
  end
  else
  begin
    Result := Max(SfrPeriod.Period, MvrPeriod.Period);
  end;
end;

{ TLakMvrLink }

function TLakMvrLink.Period: Integer;
begin
  if MvrPeriod = nil then
  begin
    result := LakPeriod.Period;
  end
  else if LakPeriod = nil then
  begin
    result := MvrPeriod.Period;
  end
  else
  begin
    Result := Max(LakPeriod.Period, MvrPeriod.Period);
  end;
end;

{ TUzfMvrLink }

function TUzfMvrLink.Period: Integer;
begin
  if MvrPeriod = nil then
  begin
    result := UzfPeriod.Period;
  end
  else if UzfPeriod = nil then
  begin
    result := MvrPeriod.Period;
  end
  else
  begin
    Result := Max(UzfPeriod.Period, MvrPeriod.Period);
  end;
end;

{ TUzfData }

function TUzfData.Compatible(UzfData: TUzfData): Boolean;
var
  Index: Integer;
begin
  result := not MvrSource and not MvrReceiver
    and not UzfData.MvrSource and not UzfData.MvrReceiver
    and (PeriodData.Count = UzfData.PeriodData.Count)
    and (PackageData.boundname = UzfData.PackageData.boundname)
    and (NumberObs = nil) and (UzfData.NumberObs = nil);
  if result then
  begin
    for Index := 0 to PeriodData.Count - 1 do
    begin
      result := PeriodData[Index].Compatible(UzfData.PeriodData[Index]);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

constructor TUzfData.Create;
begin
  PeriodData := TImportUzfPeriodItemList.Create;
end;

destructor TUzfData.Destroy;
begin
  PeriodData.Free;
  inherited;
end;

{ TImportUzfPeriodItem }

function TImportUzfPeriodItem.Compatible(Item: TImportUzfPeriodItem): Boolean;
  function CompatibleBoundValues(Source, Other: TMf6BoundaryValue): Boolean;
  begin
    result := (Source.ValueType = Other.ValueType);
    if result and (Source.ValueType = vtString) then
    begin
      result := AnsiSameText(Source.StringValue, Other.StringValue);
    end;
  end;
var
  AuxIndex: Integer;
begin
  result := (Period = Item.Period)
    and CompatibleBoundValues(PeriodData.finf, Item.PeriodData.finf)
    and CompatibleBoundValues(PeriodData.pet, Item.PeriodData.pet)
    and CompatibleBoundValues(PeriodData.extdp, Item.PeriodData.extdp)
    and CompatibleBoundValues(PeriodData.extwc, Item.PeriodData.extwc)
    and CompatibleBoundValues(PeriodData.ha, Item.PeriodData.ha)
    and CompatibleBoundValues(PeriodData.hroot, Item.PeriodData.hroot)
    and CompatibleBoundValues(PeriodData.rootact, Item.PeriodData.rootact)
    and (PeriodData.Count = Item.PeriodData.Count);
  if result then
  begin
    for AuxIndex := 0 to PeriodData.Count - 1 do
    begin
      result := CompatibleBoundValues(PeriodData.Aux[AuxIndex],
        Item.PeriodData.Aux[AuxIndex]);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

{ TTMvrKeyyComparer }

function TTMvrKeyyComparer.Equals(const Left, Right: TMvrKey): Boolean;
begin
  Result := (Left.ID = Right.ID)
    and (Left.Period = Right.Period)
    and (Left.PackageName = Right.PackageName)
end;

function TTMvrKeyyComparer.GetHashCode(const Value: TMvrKey): Integer;
begin
  Result := THashBobJenkins.GetHashValue(Value.PackageName);
  Result := THashBobJenkins.GetHashValue(Value.ID, SizeOf(Value.ID), Result);
  Result := THashBobJenkins.GetHashValue(Value.Period, SizeOf(Value.Period), result);
end;

{ TMvrSource }

function TMvrSource.Key(Index: Integer): TMvrKey;
begin
  result.ID := IDs[Index];
  result.PackageName := PackageName;
  result.Period := Period;
end;

{ TMvrReceiver }

function TMvrReceiver.Key(Index: Integer): TMvrKey;
begin
  result.ID := IDs[Index];
  result.PackageName := PackageName;
  result.Period := Period;
end;

end.
