unit Modflow6ModelImporter;

interface

uses
  System.Classes, System.IOUtils, Vcl.Dialogs, System.SysUtils, System.UITypes,
  Mf6.SimulationNameFileReaderUnit, System.Math, Mf6.CustomMf6PersistentUnit,
  ScreenObjectUnit, DataSetUnit, System.Generics.Collections,
  System.Generics.Defaults, Mf6.ObsFileReaderUnit;

  // The first name in NameFiles must be the name of the groundwater flow
  // simulation name file (mfsim.nam). Any additional names must be associated
  // transport simulation name files (mfsim.nam)

type
  TMvrSourcePackageChoice = (mspcWel, mspcDrn, mspcRiv, mspcGhb, mspcLak, mspcMaw,
    mspcSfr, mspcUzf);

  TMvrReceiverPackageChoice = (mrpcLak, mrpcMaw, mrpcSfr, mrpcUzf);


  TimeSeriesMap = TDictionary<string, string>;
  TBoundNameDictionary = TDictionary<string, TObservationList>;
  TCellIdObsDictionary = TDictionary<TCellId, TObservationList>;
  TNumberDictionary = TDictionary<Integer, TObservationList>;
  TObsLists = TObjectList<TObservationList>;

  TMvrSource = record
    ScreenObject: TScreenObject;
    PackageName: string;
    Period: Integer;
    IDs: TArray<Integer>;
    SourceType: TMvrSourcePackageChoice;
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
  end;

  TMvrReceiverList = class(TList<TMvrReceiver>)
    procedure Sort;
  end;

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
    procedure ImportFlowModelTiming;
    procedure ImportSimulationOptions;
    procedure ImportSolutionGroups;
    function ImportFlowModel: Boolean;
    procedure ImportDis(Package: TPackage);
    procedure ImportDisV(Package: TPackage);
    procedure UpdateLayerStructure(NumberOfLayers: Integer);
    procedure CreateAllTopCellsScreenObject;
    function GetAllTopCellsScreenObject: TScreenObject;
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
    procedure AddPointsToScreenObject(CellIds: TCellIdList; AScreenObject: TScreenObject);
    procedure ImportWel(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportDrn(Package: TPackage; MvrPackage: TPackage);
    procedure ImportRiv(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportGhb(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
    procedure ImportRch(Package: TPackage; TransportModels: TModelList);
    procedure ImportEvt(Package: TPackage; TransportModels: TModelList);
    procedure ImportMaw(Package: TPackage; TransportModels: TModelList; MvrPackage: TPackage);
  public
    Constructor Create;
    destructor Destroy; override;
    procedure ImportModflow6Model(NameFiles, ErrorMessages: TStringList);
  end;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, GoPhastTypes, frmSelectFlowModelUnit,
  Mf6.TDisFileReaderUnit, ModflowTimeUnit, ModflowOptionsUnit,
  Mf6.AtsFileReaderUnit, ModflowPackageSelectionUnit, ModflowOutputControlUnit,
  Mf6.NameFileReaderUnit, Mf6.DisFileReaderUnit, LayerStructureUnit,
  UndoItems, FastGEO, AbstractGridUnit, ValueArrayStorageUnit,
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
  Mf6.WelFileReaderUnit, Mf6.MvrFileReaderUnit, ModflowWellUnit,
  Mf6.DrnFileReaderUnit, ModflowDrnUnit, Mf6.RivFileReaderUnit, ModflowRivUnit,
  Mf6.GhbFileReaderUnit, ModflowGhbUnit, Mf6.RchFileReaderUnit, ModflowRchUnit,
  ModflowEtsUnit, Mf6.EvtFileReaderUnit, ModflowEvtUnit, Mf6.MawFileReaderUnit,
  ModflowMawUnit;

resourcestring
  StrTheNameFileSDoe = 'The name file %s does not exist.';
  StrModelMuseCanNotIm = 'ModelMuse can not import AUXMULTNAME specified as ' +
  'a time series.';
  StrModelMuseCanNotAp = 'ModelMuse can not apply AUXMULTNAME values to data' +
  ' specified as a time series.';
  StrModelMuseCanNotApDepth = 'ModelMuse can not apply AUXDEPTHNAME values to dat' +
  'a specified as a time series.';
  StrModelMuseCanNotSpPetm0 = 'ModelMuse can not specify a separate value fo' +
  'r petm0 in the EVt package.';

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
  procedure AddItem(AScreenObject: TScreenObject; ACell: TChdTimeItem);
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
      ImportedName := 'Imported_Heads';
      Imported_Heads := AScreenObject.ImportedValues.Add;
      Imported_Heads.Name := ImportedName;
      Imported_Heads.Values.DataType := rdtDouble;
      ChdItem.StartHead := rsObjectImportedValuesR + '("' + Imported_Heads.Name + '")';
    end
    else
    begin
      Imported_Heads := nil;
      TimeSeries := ACell.Head.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
          if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
    CellId: TCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := Format('ImportedCHD_Obs_%d', [ObjectCount]);
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
    NewName := Format('ImportedCHD_%d_Period_%d', [ObjectCount, Period]);
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

    AddItem(result, ACell);

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
  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.ChdBoundary.IsSelected := True;

  Chd := Package.Package as TChd;
  Options := Chd.Options;

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
              CellLists.Extract(ACellList);
            end
            else
            begin
              AddItem(AScreenObject, FirstCell);
            end;
          end
          else
          begin
            AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
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
  CellId: TCellId;
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
      NewName := 'ImportedCSUB_' + BoundName;
    end
    else
    begin
      if Period > 0 then
      begin
        NewName := 'ImportedCSUB_Period_' + IntToStr(Period);
      end
      else
      begin
        Inc(ObjectCount);
        NewName := 'ImportedCSUB_Obs'  + IntToStr(ObjectCount);
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
  procedure IncludeObservations(ObsList: TObservationList; AScreenObject: TScreenObject);
  var
    Modflow6Obs: TModflow6Obs;
    CSubObsSet: TSubObsSet;
    ObsIndex: Integer;
    CSubDelayCells: TIntegerCollection;
  begin
    AScreenObject.CreateMf6Obs;
    Modflow6Obs := AScreenObject.Modflow6Obs;
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
            IncludeObservations(ObsList, AScreenObject);
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
                    if not Map.TryGetValue(TimeSeries, ImportedTimeSeriesName) then
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
  AuxDepthAdjustment: Extended;
  procedure AddItem(AScreenObject: TScreenObject; ACell: TDrnTimeItem);
  var
    DrnItem: TDrnItem;
    ImportedName: string;
    Aux: TMf6BoundaryValue;
    AuxMultiplier: Extended;
    AuxDepthAdjustment: Extended;
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
        AuxDepthAdjustment := Aux.NumericValue
      end
      else
      begin
        AuxDepthAdjustment := 1;
        FErrorMessages.Add(StrModelMuseCanNotIm);
      end;
    end
    else
    begin
      AuxDepthAdjustment := 1;
    end;

    if ACell.elev.ValueType = vtNumeric then
    begin
      ImportedName := 'Imported_Drain_Elevations';
      Imported_Drain_Elevations := AScreenObject.ImportedValues.Add;
      Imported_Drain_Elevations.Name := ImportedName;
      Imported_Drain_Elevations.Values.DataType := rdtDouble;
      DrnItem.Elevation := rsObjectImportedValuesR + '("' + Imported_Drain_Elevations.Name + '")';
    end
    else
    begin
      Imported_Drain_Elevations := nil;
      TimeSeries := ACell.elev.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
      begin
        Assert(False);
      end;
      DrnItem.Elevation := ImportedTimeSeries;
      if AuxDepthAdjustment <> 1 then
      begin
        FErrorMessages.Add(StrModelMuseCanNotApDepth);
      end;
    end;

    if ACell.cond.ValueType = vtNumeric then
    begin
      ImportedName := 'Imported_Drain_Conductance';
      Imported_Drain_Conductance := AScreenObject.ImportedValues.Add;
      Imported_Drain_Conductance.Name := ImportedName;
      Imported_Drain_Conductance.Values.DataType := rdtDouble;
      DrnItem.Conductance := rsObjectImportedValuesR + '("' + Imported_Drain_Conductance.Name + '")';
    end
    else
    begin
      Imported_Drain_Conductance := nil;
      TimeSeries := ACell.cond.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
    CellId: TCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := Format('ImportedDrn_Obs_%d', [ObjectCount]);
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
    NewName := Format('ImportedDrn_%d_Period_%d', [ObjectCount, Period]);
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

    AddItem(result, ACell);

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
  // Get the MVR package.
  if MvrPackage <> nil then
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
            KeyString := KeyString + ' MVR';
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
          ACellList.FIds.Add(ACell.Id)
        end;
      end;

      // After all the cells in the current period have been read,
      // create a TScreenObject for each cell list
      for ObjectIndex := 0 to CellLists.Count - 1 do
      begin
        NewScreenObject := False;
        ACellList := CellLists[ObjectIndex];
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
            CellLists.Extract(ACellList);
            NewScreenObject := True;
          end
          else
          begin
            AddItem(AScreenObject, FirstCell);
          end;
        end
        else
        begin
          AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
          NewScreenObject := True;
        end;

        CellIds.Clear;
        for CellIndex := 0 to ACellList.Count - 1 do
        begin
          ACell := ACellList[CellIndex];
          if ACell.elev.ValueType = vtNumeric then
          begin
            if AuxDepthIndex >= 0 then
            begin
              Aux := ACell.Aux[AuxDepthIndex];
              if Aux.ValueType = vtNumeric then
              begin
                AuxDepthAdjustment := Aux.NumericValue
              end
              else
              begin
                AuxDepthAdjustment := 0;
                FErrorMessages.Add(StrModelMuseCanNotIm);
              end;
            end
            else
            begin
              AuxDepthAdjustment := 0;
            end;
            Imported_Drain_Elevations.Values.Add(ACell.elev.NumericValue + AuxDepthAdjustment);
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
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
        if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
        if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
          if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
    CellId: TCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := Format('ImportedEvt_Obs_%d', [ObjectCount]);
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
    NewName := Format('ImportedEvt_%d_Period_%d', [ObjectCount, Period]);
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
  try
    IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
    for TimeSeriesIndex := 0 to Evt.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := Evt.TimeSeries[TimeSeriesIndex];
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
              CellLists.Extract(ACellList);
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
  //        SfrReader := TSfr.Create(APackage.FileType);
  //        SfrReader.Dimensions := FDimensions;
  //        APackage.Package := SfrReader;
  //        APackage.ReadPackage(Unhandled);
        end
        else if APackage.FileType = 'LAK6' then
        begin
  //        LakReader := TLak.Create(APackage.FileType);
  //        LakReader.Dimensions := FDimensions;
  //        APackage.Package := LakReader;
  //        APackage.ReadPackage(Unhandled);
        end
        else if APackage.FileType = 'UZF6' then
        begin
  //        UzfReader := TUzf.Create(APackage.FileType);
  //        UzfReader.Dimensions := FDimensions;
  //        APackage.Package := UzfReader;
  //        APackage.ReadPackage(Unhandled);
        end
        else if APackage.FileType = 'MVR6' then
        begin
  //        MovReader := TMvr.Create(APackage.FileType);
  //        APackage.Package := MovReader;
  //        APackage.ReadPackage(Unhandled);
        end
        else if APackage.FileType = 'GNC6' then
        begin
  //        GncReader := TGnc.Create(APackage.FileType);
  //        GncReader.Dimensions := FDimensions;
  //        APackage.Package := GncReader;
  //        APackage.ReadPackage(Unhandled);
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
  FoundMvr: Boolean;
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
  procedure AddItem(AScreenObject: TScreenObject; ACell: TGhbTimeItem);
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
      ImportedName := 'Imported_Ghb_Conductance';
      Imported_Ghb_Conductance := AScreenObject.ImportedValues.Add;
      Imported_Ghb_Conductance.Name := ImportedName;
      Imported_Ghb_Conductance.Values.DataType := rdtDouble;
      GhbItem.Conductance := rsObjectImportedValuesR + '("' + Imported_Ghb_Conductance.Name + '")';
    end
    else
    begin
      Imported_Ghb_Conductance := nil;
      TimeSeries := ACell.Cond.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
      ImportedName := 'Imported_Ghb_Stage';
      Imported_Ghb_Stage := AScreenObject.ImportedValues.Add;
      Imported_Ghb_Stage.Name := ImportedName;
      Imported_Ghb_Stage.Values.DataType := rdtDouble;
      GhbItem.BoundaryHead := rsObjectImportedValuesR + '("' + Imported_Ghb_Stage.Name + '")';
    end
    else
    begin
      Imported_Ghb_Stage := nil;
      TimeSeries := ACell.BHead.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
          if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
    CellId: TCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := Format('ImportedGhb_Obs_%d', [ObjectCount]);
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
    NewName := Format('ImportedGhb_%d_Period_%d', [ObjectCount, Period]);
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

    AddItem(result, ACell);

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
        GhbMvrLink.GhbPeriod := nil;
        GhbMvrLink.MvrPeriod := nil;
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
              KeyString := KeyString + ' MVR';
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
            ACellList.FIds.Add(ACell.Id)
          end;
        end;

        // After all the cells in the current period have been read,
        // create a TScreenObject for each cell list
        for ObjectIndex := 0 to CellLists.Count - 1 do
        begin
          NewScreenObject := False;
          ACellList := CellLists[ObjectIndex];
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
              CellLists.Extract(ACellList);
              NewScreenObject := True;
            end
            else
            begin
              AddItem(AScreenObject, FirstCell);
            end;
          end
          else
          begin
            AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
            NewScreenObject := True;
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
  CellId: TCellId;
begin
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
  IC := Package.Package as TIc;
  Assign3DRealDataSet(rsModflow_Initial_Head, IC.GridData.STRT);
end;

procedure TModflow6Importer.ImportMaw(Package: TPackage;
  TransportModels: TModelList; MvrPackage: TPackage);
var
  Model: TPhastModel;
  Maw: TMaw;
  Options: TMawOptions;
  MawPackage: TMawPackage;
  Wells: array of TScreenObject;
  WellIndex: Integer;
  PackageItem: TMawPackageItem;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  NewName: string;
begin
  Model := frmGoPhast.PhastModel;
  MawPackage := Model.ModflowPackages.MawPackage;
  MawPackage.IsSelected := True;

  Maw := Package.Package as TMaw;
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
  for WellIndex := 0 to Maw.PackageData.Count - 1 do
  begin
    PackageItem := Maw.PackageData[WellIndex];

    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := Format('ImportedMaw_%d', [WellIndex + 1]);
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
      FSimulations.Add(FSimulation);
      FSimulation.ReadSimulation(NameFiles[FileIndex]);
      OutFile := ChangeFileExt(NameFiles[FileIndex], '.lst');
      if TFile.Exists(OutFile) then
      begin
        ListFile := TStringList.Create;
        try
          ListFile.LoadFromFile(OutFile);
          if ListFile.Count > 0 then
          begin
            ErrorMessages.Add('The following errors were encountered when reading ' + NameFiles[FileIndex]);
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
          ErrorMessages.Add('The following error was encountered when reading ' + NameFiles[FileIndex]);
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
          ErrorMessages.Add('The following error was encountered when reading ' + NameFiles[FileIndex]);
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

      finally
        FlowModelNames.Free
      end;

    finally
      FSimulation := nil;
    end;
  end;
  PhastModel.Exaggeration := frmGoPhast.DefaultVE;
  frmGoPhast.RestoreDefault2DView1Click(nil);
end;

procedure TModflow6Importer.AddPointsToScreenObject(CellIds: TCellIdList; AScreenObject: TScreenObject);
var
  ElementCenter: TDualLocation;
  APoint: TPoint2D;
  CellIndex: Integer;
  Model: TPhastModel;
  CellId: TCellId;
  DisvUsed: Boolean;
begin
  Model := frmGoPhast.PhastModel;
  DisvUsed := Model.DisvUsed;
  for CellIndex := 0 to CellIds.Count - 1 do
  begin
    CellId := CellIds[CellIndex];
    if DisvUsed then
    begin
      CellId.Row := 1;
    end;
    ElementCenter := Model.ElementLocation[CellId.Layer - 1, CellId.Row - 1, CellId.Column - 1];
    APoint.x := ElementCenter.RotatedLocation.x;
    APoint.y := ElementCenter.RotatedLocation.y;
    AScreenObject.AddPoint(APoint, True);
    AScreenObject.ImportedSectionElevations.Add(ElementCenter.RotatedLocation.z);
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
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
          if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
    CellId: TCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := Format('ImportedRch_Obs_%d', [ObjectCount]);
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
    NewName := Format('ImportedRch_%d_Period_%d', [ObjectCount, Period]);
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
  Model := frmGoPhast.PhastModel;
  Model.ModflowPackages.RchPackage.IsSelected := True;

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
  try
    IFaceIndex := Options.IndexOfAUXILIARY('IFACE');
    for TimeSeriesIndex := 0 to Rch.TimeSeriesCount - 1 do
    begin
      TimeSeriesPackage := Rch.TimeSeries[TimeSeriesIndex];
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
              CellLists.Extract(ACellList);
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
  FoundMvr: Boolean;
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
  procedure AddItem(AScreenObject: TScreenObject; ACell: TRivTimeItem);
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
      ImportedName := 'Imported_River_Conductance';
      Imported_River_Conductance := AScreenObject.ImportedValues.Add;
      Imported_River_Conductance.Name := ImportedName;
      Imported_River_Conductance.Values.DataType := rdtDouble;
      RivItem.Conductance := rsObjectImportedValuesR + '("' + Imported_River_Conductance.Name + '")';
    end
    else
    begin
      Imported_River_Conductance := nil;
      TimeSeries := ACell.Cond.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
      ImportedName := 'Imported_River_Stage';
      Imported_River_Stage := AScreenObject.ImportedValues.Add;
      Imported_River_Stage.Name := ImportedName;
      Imported_River_Stage.Values.DataType := rdtDouble;
      RivItem.RiverStage := rsObjectImportedValuesR + '("' + Imported_River_Stage.Name + '")';
    end
    else
    begin
      Imported_River_Stage := nil;
      TimeSeries := ACell.Stage.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
      ImportedName := 'Imported_River_RBot';
      Imported_River_RBot := AScreenObject.ImportedValues.Add;
      Imported_River_RBot.Name := ImportedName;
      Imported_River_RBot.Values.DataType := rdtDouble;
      RivItem.RiverBottom := rsObjectImportedValuesR + '("' + Imported_River_RBot.Name + '")';
    end
    else
    begin
      Imported_River_RBot := nil;
      TimeSeries := ACell.RBot.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
          if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
    CellId: TCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := Format('ImportedRiv_Obs_%d', [ObjectCount]);
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
    NewName := Format('ImportedRiv_%d_Period_%d', [ObjectCount, Period]);
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

    AddItem(result, ACell);

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
        RivMvrLink.RivPeriod := nil;
        RivMvrLink.MvrPeriod := nil;
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
              KeyString := KeyString + ' MVR';
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
            ACellList.FIds.Add(ACell.Id)
          end;
        end;

        // After all the cells in the current period have been read,
        // create a TScreenObject for each cell list
        for ObjectIndex := 0 to CellLists.Count - 1 do
        begin
          NewScreenObject := False;
          ACellList := CellLists[ObjectIndex];
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
              CellLists.Extract(ACellList);
              NewScreenObject := True;
            end
            else
            begin
              AddItem(AScreenObject, FirstCell);
            end;
          end
          else
          begin
            AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
            NewScreenObject := True;
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
      DataArrayName, '0', DataArrayName, [dcType], rdtBoolean, eaBlocks, dsoTop, '');
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

    if Mf6TimesSeries.GetTimeSeriesByName(TSName) = nil then
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

    Map.Add(TSName, NewName);

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
  CellId: TCellId;
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
            if not Map.TryGetValue(TimeSeriesName, ImportedTimeSeriesName) then
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
            if not Map.TryGetValue(TimeSeriesName, ImportedTimeSeriesName) then
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
            if not Map.TryGetValue(TimeSeriesName, ImportedTimeSeriesName) then
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
  CellId: TCellId;
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
            if not Map.TryGetValue(TimeSeriesName, ImportedTimeSeriesName) then
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
            if not Map.TryGetValue(TimeSeriesName, ImportedTimeSeriesName) then
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
  FoundMvr: Boolean;
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
  procedure AddItem(AScreenObject: TScreenObject; ACell: TWelTimeItem);
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
      ImportedName := 'Imported_Heads';
      Imported_Heads := AScreenObject.ImportedValues.Add;
      Imported_Heads.Name := ImportedName;
      Imported_Heads.Values.DataType := rdtDouble;
      WelItem.PumpingRate := rsObjectImportedValuesR + '("' + Imported_Heads.Name + '")';
    end
    else
    begin
      Imported_Heads := nil;
      TimeSeries := ACell.Q.StringValue;
      if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
          if not Map.TryGetValue(TimeSeries, ImportedTimeSeries) then
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
    CellId: TCellId;
    ElementCenter: TDualLocation;
    APoint: TPoint2D;
    AScreenObject: TScreenObject;
  begin
    Inc(ObjectCount);
    AScreenObject := TScreenObject.CreateWithViewDirection(
      Model, vdTop, UndoCreateScreenObject, False);
    NewName := Format('ImportedWel_Obs_%d', [ObjectCount]);
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
    NewName := Format('ImportedWel_%d_Period_%d', [ObjectCount, Period]);
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
    AddItem(result, ACell);

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
        WellMvrLink.WelPeriod := nil;
        WellMvrLink.MvrPeriod := nil;
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
              KeyString := KeyString + ' MVR';
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
            ACellList.FIds.Add(ACell.Id)
          end;
        end;

        // After all the cells in the current period have been read,
        // create a TScreenObject for each cell list
        for ObjectIndex := 0 to CellLists.Count - 1 do
        begin
          NewScreenObject := False;
          ACellList := CellLists[ObjectIndex];
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
              CellLists.Extract(ACellList);
              NewScreenObject := True;
            end
            else
            begin
              AddItem(AScreenObject, FirstCell);
            end;
          end
          else
          begin
            AScreenObject := CreateScreenObject(FirstCell, APeriod.Period);
            NewScreenObject := True;
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
  end;
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

end.
