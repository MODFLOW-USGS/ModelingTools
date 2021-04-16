unit ModflowSUB_Writer;

interface

uses
  Classes, CustomModflowWriterUnit, ModflowPackageSelectionUnit, UnitList,
  IntListUnit, PhastModelUnit, SysUtils, ModflowSubsidenceDefUnit,
  ModflowSwiWriterUnit, InterpolatedObsCellUnit, System.Generics.Collections,
  DataSetUnit;

type
  TMaterialZone = record
    VerticalK: double;
    ElasticSpecificStorage: double;
    InelasticSpecificStorage: double;
    class operator Equal(Var1: TMaterialZone; Var2: TMaterialZone): boolean;
    class operator NotEqual(Var1: TMaterialZone; Var2: TMaterialZone): boolean;
    function ID: Integer;
  end;

  TMaterialZoneIdItem = class(TIDItem)
  private
    FMaterialZoneValues: TMaterialZone;
    FZoneNumber: integer;
    FNextSameID: TMaterialZoneIdItem;
    procedure SetMaterialZoneValues(const Value: TMaterialZone);
    procedure AssignID;
  public
    Constructor Create;
    Destructor Destroy; override;
    property MaterialZoneValues: TMaterialZone read FMaterialZoneValues
      write SetMaterialZoneValues;
    property ZoneNumber: integer read FZoneNumber write FZoneNumber;
    property NextSameID: TMaterialZoneIdItem read FNextSameID write FNextSameID;
  end;

  TMaterialZoneList = class(TObject)
  private
    FMaterialZones: TList;
    FIdList: TIDList;
    function GetItem(Index: integer): TMaterialZoneIdItem;
  public
    Constructor Create;
    Destructor Destroy; override;
    function AddRecord(MaterialZone: TMaterialZone): TMaterialZoneIdItem;
    function Count: integer;
    property Items[Index: integer]: TMaterialZoneIdItem read GetItem; default;
  end;

  TModflowSUB_Writer = class(TCustomSubWriter)
  private
    // model layer assignments for each system of no-delay interbeds
    FLN: TIntegerList;
    // model layer assignments for each system of delay interbeds
    FLDN: TIntegerList;
    // specifying the factor n-equiv - Delay beds
    FRNB_List: TList;
    // preconsolidation head - No-Delay beds
    FHC_List: TList;
    // elastic skeletal storage coefficient - No-Delay beds
    FSfe_List: TList;
    // inelastic skeletal storage coefficient - No-Delay beds
    FSfv_List: TList;
    // starting compaction - No-Delay beds
    FCom_List: TList;
    // starting elastic compaction - No-Delay beds
    FComE_List: TList;
    // starting inelastic compaction - No-Delay beds
    FComV_List: TList;
    // material zones - Delay beds
    FDP_List: TMaterialZoneList;
    FDelayVK_List: TList;
    FDelayElasticSpecificStorage_List: TList;
    FDelayInElasticSpecificStorage_List: TList;
    // starting head - Delay beds
    FDstart_List: TList;
    // starting preconsolidation head - Delay beds
    FDHC_List: TList;
    // starting compaction - Delay beds
    FDCOM_List: TList;

    // starting elastic compaction - Delay beds
    FDCOM_E_List: TList;
    // starting elastic compaction - Delay beds
    FDCOM_V_List: TList;

    // equivalent thickness - Delay beds
    FDZ_List: TList;
    // material zone numbers - Delay beds
    FNZ_List: TList;
    FSubPackage: TSubPackageSelection;
//    FNameOfFile: string;
    FObsList: TSubObsItemList;
    FUsedObsTypes: TStringList;
    FCombinedSubFileName: string;
    FMultipleSubFileNames: TStringList;
    FInterpolatedObs: TBaseInterpolatedObsObjectList;
    FNoDelayNames: TStringList;
    FDelayNames: TStringList;
    FSubObsCollectionList: TSubObsCollectionList;
    FPestDataArrays: TList<TDataArray>;
    procedure RetrieveArrays;
    procedure EvaluateMaterialZones;
    procedure EvaluatePestObs;
    procedure Evaluate;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSets5to8;
    procedure WriteDataSet9;
    procedure WriteDataSets10to14;
    procedure WriteDataSet15;
    procedure WriteDataSet16;
    procedure WriteObsScript;
    procedure WritePestScripts;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
  end;


implementation

uses
  Contnrs, LayerStructureUnit,
  GoPhastTypes, RbwParser, ModflowUnitNumbers, frmProgressUnit,
  frmErrorsAndWarningsUnit, Forms, JclMath, ScreenObjectUnit, System.Math,
  ModflowTimeUnit, System.Generics.Defaults, ModflowCellUnit, PestObsUnit,
  AbstractGridUnit, FastGEO, BasisFunctionUnit, PestParamRoots;

resourcestring
  StrSubsidenceNotSuppo = 'Subsidence not supported with MODFLOW-LGR';
  StrRestartFileNamesI = 'Restart File names identical for SUB package';
  StrTheRestartFileSav = 'The restart file saved by the Subsidence package' +
  ' has the same name as the restart file read by Subsidence package to ' +
  'define the starting head and starting preconsolidation head.  You need ' +
  'to change the name of the file read by the Subsidence package in the ' +
  '"Model|Packages and Programs" dialog box.';
  StrModelMuseDoesNotC = 'ModelMuse does not currently support the use of th' +
  'e Subsidence package in MODFLOW-LGR.';
  StrWritingSUBPackage = 'Writing SUB Package input.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSet3 = '  Writing Data Set 3.';
//  StrWritingDataSet4 = '  Writing Data Set 4.';
//  StrWritingDataSets5to8 = '  Writing Data Sets 5 to 8.';
//  StrWritingDataSet9 = '  Writing Data Set 9.';
  StrWritingDataSets10to14 = '  Writing Data Sets 10 to 14.';
  StrNoSubsidenceLayers = 'No subsidence layers defined.';
  StrNeitherDelayBedsN = 'Neither delay beds nor no-delay beds in the Subsid' +
  'ence package have been defined for any layer in the model. They must be d' +
  'efined in the "Model|MODFLOW Layers" dialog box.';
  StrInvalidSubsidenceO = 'Invalid Subsidence observation time.';
  StrTheObservationTime = 'The observation time of the subsidence observatio' +
  'n "%0:s" defined in %1:s is invalid';
  StrInvalidSUBObservat = 'Invalid SUB observation location';
  StrTheSubsidenceObser = 'The subsidence observations defined in %s are inv' +
  'alid because it does not intersect any cells.';
//  StrWritingDataSet15 = '  Writing Data Set 15.';
//  StrWritingDataSet16 = '  Writing Data Set 16.';

function TMaterialZone.ID: Integer;
var
  ByteArray: array of Byte;
begin
  SetLength(ByteArray, SizeOf(TMaterialZone));
  Move(self, ByteArray[0], SizeOf(TMaterialZone));
  result := Integer(CRC32(ByteArray, SizeOf(TMaterialZone)));
end;

class operator TMaterialZone.NotEqual(Var1, Var2: TMaterialZone): boolean;
begin
  result := (Var1.VerticalK <> Var2.VerticalK)
    or (Var1.ElasticSpecificStorage <> Var2.ElasticSpecificStorage)
    or (Var1.InelasticSpecificStorage <> Var2.InelasticSpecificStorage)
end;

{ TMaterialZone }

class operator TMaterialZone.Equal(Var1, Var2: TMaterialZone): boolean;
begin
  result := (Var1.VerticalK = Var2.VerticalK)
    and (Var1.ElasticSpecificStorage = Var2.ElasticSpecificStorage)
    and (Var1.InelasticSpecificStorage = Var2.InelasticSpecificStorage)
end;

{ TModflowSUB_Writer }

constructor TModflowSUB_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
var
  Index: Integer;
begin
  inherited;
  FLN := TIntegerList.Create;
  FLDN := TIntegerList.Create;
  FRNB_List := TList.Create;
  FHC_List := TList.Create;
  FSfe_List := TList.Create;
  FSfv_List := TList.Create;
  FCom_List := TList.Create;
  FComE_List := TList.Create;
  FComV_List := TList.Create;
  FDP_List := TMaterialZoneList.Create;
  FDelayVK_List := TList.Create;
  FDelayElasticSpecificStorage_List := TList.Create;
  FDelayInElasticSpecificStorage_List := TList.Create;
  FDstart_List := TList.Create;
  FDHC_List := TList.Create;
  FDCOM_List := TList.Create;
  FDCOM_E_List := TList.Create;
  FDCOM_V_List := TList.Create;
  FDZ_List := TList.Create;
  FNZ_List := TObjectList.Create;
  FObsList:= TSubObsItemList.Create;
  FUsedObsTypes := TStringList.Create;
  FUsedObsTypes.Sorted := True;
  FUsedObsTypes.Duplicates := dupIgnore;
  FUsedObsTypes.CaseSensitive := False;
  FMultipleSubFileNames := TStringList.Create;
  for Index := 0 to SubsidenceUnits.Count - 1 do
  begin
    FMultipleSubFileNames.Add('');
  end;
  FInterpolatedObs:= TBaseInterpolatedObsObjectList.Create;
  FNoDelayNames := TStringList.Create;
  FDelayNames := TStringList.Create;
  FSubObsCollectionList := TSubObsCollectionList.Create;
  FPestDataArrays := TList<TDataArray>.Create;
  
end;

destructor TModflowSUB_Writer.Destroy;
begin
  FPestDataArrays.Free;
  FSubObsCollectionList.Free;
  FDelayNames.Free;
  FNoDelayNames.Free;
  FInterpolatedObs.Free;
  FMultipleSubFileNames.Free;
  FUsedObsTypes.Free;
  FObsList.Free;
  FNZ_List.Free;
  FDZ_List.Free;
  FDCOM_V_List.Free;
  FDCOM_E_List.Free;
  FDCOM_List.Free;
  FDHC_List.Free;
  FDstart_List.Free;
  FDelayInElasticSpecificStorage_List.Free;
  FDelayElasticSpecificStorage_List.Free;
  FDelayVK_List.Free;
  FDP_List.Free;
  FComV_List.Free;
  FComE_List.Free;
  FCom_List.Free;
  FSfv_List.Free;
  FSfe_List.Free;
  FHC_List.Free;
  FRNB_List.Free;
  FLDN.Free;
  FLN.Free;
  inherited;
end;

procedure TModflowSUB_Writer.Evaluate;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoSubsidenceLayers);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSubsidenceO);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidSUBObservat);

  RetrieveArrays;
  EvaluateMaterialZones;

  if Model.PestUsed then
  begin
    EvaluatePestObs;
  end;
end;

procedure TModflowSUB_Writer.EvaluateMaterialZones;
var
  DataArrayIndex: Integer;
  VK_Array: TDataArray;
  ElasticSSArray: TDataArray;
  InElasticSSArray: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  MaterialZoneRecord: TMaterialZone;
  MaterialZoneObject: TMaterialZoneIdItem;
  MaterialZoneArray: TDataArray;
begin
  FNZ_List.Capacity := FDelayVK_List.Count;
  for DataArrayIndex := 0 to FDelayVK_List.Count - 1 do
  begin
    VK_Array := FDelayVK_List[DataArrayIndex];
    ElasticSSArray := FDelayElasticSpecificStorage_List[DataArrayIndex];
    InElasticSSArray := FDelayInElasticSpecificStorage_List[DataArrayIndex];
    VK_Array.Initialize;
    ElasticSSArray.Initialize;
    InElasticSSArray.Initialize;
    MaterialZoneArray := TDataArray.Create(Model);
    FNZ_List.Add(MaterialZoneArray);
    MaterialZoneArray.Orientation := dsoTop;
    MaterialZoneArray.DataType := rdtInteger;
    MaterialZoneArray.EvaluatedAt := eaBlocks;
    MaterialZoneArray.UpdateDimensions(1, VK_Array.RowCount, VK_Array.ColumnCount, True);
    for RowIndex := 0 to VK_Array.RowCount - 1 do
    begin
      for ColIndex := 0 to VK_Array.ColumnCount - 1 do
      begin
        MaterialZoneRecord.VerticalK := VK_Array.RealData[0,RowIndex,ColIndex];
        MaterialZoneRecord.ElasticSpecificStorage := ElasticSSArray.RealData[0,RowIndex,ColIndex];
        MaterialZoneRecord.InelasticSpecificStorage := InElasticSSArray.RealData[0,RowIndex,ColIndex];
        MaterialZoneObject := FDP_List.AddRecord(MaterialZoneRecord);
        MaterialZoneArray.IntegerData[0,RowIndex,ColIndex] := MaterialZoneObject.ZoneNumber;
      end;
    end;
    MaterialZoneArray.UpToDate := True;
  end;
end;

function CompareSubObservations(Item1, Item2: Pointer): Integer;
var
  Sub1: TSubObsItem;
  Sub2: TSubObsItem;
begin
  Sub1 := Item1;
  Sub2 := Item2;
  Result := Sign(Sub2.Time - Sub1.Time);
end;

procedure TModflowSUB_Writer.EvaluatePestObs;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  SubObservations: TSubObservations;
  ObsIndex: Integer;
//  CurrentPrintItem: TSubPrintItem;
  Obs: TSubObsItem;
  PrintChoiceIndex: Integer;
  PrintChoice: TSubPrintItem;
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  CurrentStressPeriod: TModflowStressPeriod;
  CellList: TCellAssignmentList;
  ACell: TCellLocation;
  Grid: TCustomModelGrid;
  ObservationPoint: TPoint2D;
  Center: double;
  ObservationRowOffset: double;
  ObservationColumnOffset: double;
  ColDirection: Integer;
  RowDirection: Integer;
  CenterCell: TInterpolatedObsCell;
  Cell1: TInterpolatedObsCell;
  Cell3: TInterpolatedObsCell;
  Cell2: TInterpolatedObsCell;
  Direction: T2DDirection;
  NewRow: Integer;
  ActiveDataArray: TDataArray;
  NewColumn: Integer;
  InterpObs: TBaseInterpolatedObs;
  Element: TElement;
  CellIndex: Integer;
  SubCell: TInterpolatedObsCell;
  ALocation: TPoint2D;
  Fractions: TOneDRealArray;
  FoundPrintItem: Boolean;
  procedure AssignSaveOption(PrintItem: TSubPrintItem);
  begin
    case Obs.ObsTypeIndex of
      0: // rsSUBSIDENCE
        begin
          PrintItem.SaveSubsidence := True;
        end;
      1: // rsLAYERCOMPACT
        begin
          PrintItem.SaveCompactionByModelLayer := True;
        end;
      2: // rsNDSYSCOMPACT
        begin
          PrintItem.SaveCompactionByInterbedSystem := True;
        end;
      3: // rsDSYSCOMPACTI
        begin
          PrintItem.SaveCompactionByInterbedSystem := True;
        end;
      4: // rsZDISPLACEMEN
        begin
          PrintItem.SaveVerticalDisplacement := True;
        end;
      5: // rsNDCRITICALHE
        begin
          PrintItem.SaveCriticalHeadNoDelay := True;
        end;
      6: // rsDCRITICALHEA
        begin
          PrintItem.SaveCriticalHeadDelay := True;
        end;
    end;
  end;
begin
  Grid := Model.Grid;
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  ActiveDataArray.Initialize;
  for ObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    AScreenObject := Model.ScreenObjects[ObjectIndex];
    if AScreenObject.Deleted
      or not AScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    SubObservations := AScreenObject.ModflowSubObservations;
    if SubObservations <> nil then
    begin
      if SubObservations.Comparisons.Count > 0 then
      begin
        FSubObsCollectionList.Add(SubObservations);
      end;

      InterpObs := nil;
      CellList := TCellAssignmentList.Create;
      try
        AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alFirstVertex, Model);
        if CellList.Count > 0 then
        begin
          ACell := CellList[0].Cell;
          ObservationPoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(
            AScreenObject.Points[0]);

          Center := Grid.RowCenter(ACell.Row);
          ObservationRowOffset := -(ObservationPoint.y - Center);

          Center := Grid.ColumnCenter(ACell.Column);
          ObservationColumnOffset := (ObservationPoint.x - Center);

          ColDirection := Sign(ObservationColumnOffset);
          RowDirection := Sign(ObservationRowOffset);

          CenterCell := TInterpolatedObsCell.Create;
          CenterCell.Layer := ACell.Layer;
          CenterCell.Row := ACell.Row;
          CenterCell.Col := ACell.Column;

          Cell1 := nil;
          Cell2 := nil;
          Cell3 := nil;
          Direction := dirX;

          NewRow := 0;
          if RowDirection <> 0 then
          begin
            NewRow := ACell.Row + RowDirection;
            if (NewRow >= 0) and (NewRow < Grid.RowCount)
              and ActiveDataArray.BooleanData[ACell.Layer, NewRow, ACell.Column]  then
            begin
              Cell1 := TInterpolatedObsCell.Create;
              Cell1.Layer := ACell.Layer;
              Cell1.Row := NewRow;
              Cell1.Col := ACell.Column;
              Direction := dirY;
            end;
          end;

          NewColumn := 0;
          if ColDirection <> 0 then
          begin
            NewColumn := ACell.Column + ColDirection;
            if (NewColumn >= 0) and (NewColumn < Grid.ColumnCount)
              and ActiveDataArray.BooleanData[ACell.Layer, ACell.Row, NewColumn]  then
            begin
              Cell3 := TInterpolatedObsCell.Create;
              Cell3.Layer := ACell.Layer;
              Cell3.Row := ACell.Row;
              Cell3.Col := NewColumn;
              Direction := dirX;
            end;
          end;

          if (RowDirection <> 0) and (ColDirection <> 0)
            and ((Cell1 <> nil) or (Cell3 <> nil))
            and ActiveDataArray.BooleanData[ACell.Layer, NewRow, NewColumn]  then
          begin
            Cell2 := TInterpolatedObsCell.Create;
            Cell2.Layer := ACell.Layer;
            Cell2.Row := NewRow;
            Cell2.Col := NewColumn;
          end;

          InterpObs := TBaseInterpolatedObs.Create;
          FInterpolatedObs.Add(InterpObs);

          if (RowDirection = 0) or (ColDirection = 0) then
          begin
            InterpObs.Cells.Add(CenterCell);
            if (Cell1 <> nil) then
            begin
              InterpObs.Cells.Add(Cell1);
            end;
            if (Cell3 <> nil) then
            begin
              InterpObs.Cells.Add(Cell3);
            end;
          end
          else
          begin
            if RowDirection = ColDirection then
            begin
              if (Cell3 <> nil) then
              begin
                InterpObs.Cells.Add(Cell3);
              end;
              InterpObs.Cells.Add(CenterCell);
              if (Cell1 <> nil) then
              begin
                InterpObs.Cells.Add(Cell1);
              end;
              if (Cell2 <> nil) then
              begin
                InterpObs.Cells.Add(Cell2);
              end;
            end
            else
            begin
              InterpObs.Cells.Add(CenterCell);
              if (Cell3 <> nil) then
              begin
                InterpObs.Cells.Add(Cell3);
              end;
              if (Cell2 <> nil) then
              begin
                InterpObs.Cells.Add(Cell2);
              end;
              if (Cell1 <> nil) then
              begin
                InterpObs.Cells.Add(Cell1);
              end;
            end;
          end;

          if InterpObs.Cells.Count = 1 then
          begin
            InterpObs.Cells[0].Fraction := 1;
          end
          else
          begin
            SetLength(Element, InterpObs.Cells.Count);
            for CellIndex := 0 to InterpObs.Cells.Count - 1 do
            begin
              SubCell := InterpObs.Cells[CellIndex];
              Element[CellIndex] := Grid.UnrotatedTwoDElementCenter
                (SubCell.Col, SubCell.Row);
            end;
            ALocation := Grid.
              RotateFromRealWorldCoordinatesToGridCoordinates(AScreenObject.Points[0]);
            GetBasisFunctionFractions(Element, ALocation, Fractions, Direction);
            for CellIndex := 0 to InterpObs.Cells.Count - 1 do
            begin
              SubCell := InterpObs.Cells[CellIndex];
              SubCell.Fraction := Fractions[CellIndex];
            end;
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrInvalidSUBObservat,
            Format(StrTheSubsidenceObser, [AScreenObject.Name]), AScreenObject);
        end;
      finally
        CellList.Free;
      end;
      for ObsIndex := 0 to SubObservations.Count - 1 do
      begin
        Obs := SubObservations[ObsIndex];
        Obs.Cells := InterpObs;
        FObsList.Add(Obs);
      end;
    end;
  end;

  FObsList.Sort(TComparer<TSubObsItem>.Construct(
    function (const Left, Right: TSubObsItem): Integer
    begin
      Result := Sign(Left.Time - Right.Time);
    end
    ));

  for ObsIndex := 0 to FObsList.Count - 1 do
  begin
    FoundPrintItem := False;
    Obs := FObsList[ObsIndex];
    FUsedObsTypes.Add(Obs.ObsType);

    for PrintChoiceIndex := 0 to FSubPackage.PrintChoices.Count -1 do
    begin
      PrintChoice := FSubPackage.PrintChoices[PrintChoiceIndex];
      if (Obs.Time >= PrintChoice.StartTime)
        and (Obs.Time <= PrintChoice.EndTime)  then
      begin
        AssignSaveOption(PrintChoice);
        FoundPrintItem := True;
      end;
    end;

    if not FoundPrintItem then
    begin
      CurrentStressPeriod := nil;
      for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
      begin
        StressPeriod := Model.ModflowFullStressPeriods[StressPeriodIndex];
        if (Obs.Time >= StressPeriod.StartTime)
          and (Obs.Time <= StressPeriod.EndTime) then
        begin
          CurrentStressPeriod := StressPeriod;
          Break;
        end;
      end;

      if CurrentStressPeriod <> nil then
      begin
        PrintChoice := FSubPackage.PrintChoices.Add as TSubPrintItem;
        PrintChoice.StartTime := CurrentStressPeriod.StartTime;
        PrintChoice.EndTime := CurrentStressPeriod.EndTime;
        AssignSaveOption(PrintChoice)
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model, StrInvalidSubsidenceO,
          Format(StrTheObservationTime, [Obs.Name,
          (Obs.ScreenObject as TScreenObject).Name]), Obs.ScreenObject);
      end;
    end;
  end;
end;

class function TModflowSUB_Writer.Extension: string;
begin
  result := '.sub';
end;

function TModflowSUB_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SubPackage;
end;

procedure TModflowSUB_Writer.RetrieveArrays;
var
  GroupIndex: Integer;
  Layers: TLayerStructure;
  Group: TLayerGroup;
  SubsidenceIndex: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem;
  PreconsolidationHeadDataArray: TDataArray;
  ElasticSkeletalStorageCoefficientDataArray: TDataArray;
  InelasticSkeletalStorageCoefficientDataArray: TDataArray;
  InitialCompactionDataArray: TDataArray;
  MFLayer_Group: Integer;
  LayerIndex: Integer;
  DelayItem: TSubDelayBedLayerItem;
  EquivNumberDataArray: TDataArray;
  VerticalHydraulicConductivityDataArray: TDataArray;
  ElasticSpecificStorageDataArray: TDataArray;
  InelasticSpecificStorageDataArray: TDataArray;
  InterbedStartingHeadDataArray: TDataArray;
  InterbedPreconsolidationHeadDataArray: TDataArray;
  InterbedStartingCompactionDataArray: TDataArray;
  InterbedEquivalentThicknessDataArray: TDataArray;
  InterbedStartingElasticCompactionDataArray: TDataArray;
  InterbedStartingInelasticCompactionDataArray: TDataArray;
  InitialCompactionElasticDataArray: TDataArray;
  InitialCompactionInelasticDataArray: TDataArray;
begin
  Layers := Model.LayerStructure;
  MFLayer_Group := 0;
  for GroupIndex := 1 to Layers.Count - 1 do
  begin
    Group := Layers.LayerGroups[GroupIndex];
    if Group.RunTimeSimulated then
    begin
      for SubsidenceIndex := 0 to Group.SubNoDelayBedLayers.Count - 1 do
      begin
        NoDelayItem := Group.SubNoDelayBedLayers[SubsidenceIndex];
        FNoDelayNames.AddObject(NoDelayItem.Name, NoDelayItem);

        PreconsolidationHeadDataArray := Model.DataArrayManager.GetDataSetByName(
          NoDelayItem.PreconsolidationHeadDataArrayName);
        Assert(PreconsolidationHeadDataArray <> nil);

        ElasticSkeletalStorageCoefficientDataArray := Model.DataArrayManager.GetDataSetByName(
          NoDelayItem.ElasticSkeletalStorageCoefficientDataArrayName);
        Assert(ElasticSkeletalStorageCoefficientDataArray <> nil);

        InelasticSkeletalStorageCoefficientDataArray := Model.DataArrayManager.GetDataSetByName(
          NoDelayItem.InelasticSkeletalStorageCoefficientDataArrayName);
        Assert(InelasticSkeletalStorageCoefficientDataArray <> nil);

        if Model.ModelSelection = msModflowFmp then
        begin
          InitialCompactionDataArray := nil;
          InitialCompactionElasticDataArray := Model.DataArrayManager.GetDataSetByName(
            NoDelayItem.InitialElasticCompactionDataArrayName);
          Assert(InitialCompactionElasticDataArray <> nil);
          InitialCompactionInelasticDataArray := Model.DataArrayManager.GetDataSetByName(
            NoDelayItem.InitialInelasticCompactionDataArrayName);
          Assert(InitialCompactionInelasticDataArray <> nil);
        end
        else
        begin
          InitialCompactionElasticDataArray := nil;
          InitialCompactionInelasticDataArray := nil;
          InitialCompactionDataArray := Model.DataArrayManager.GetDataSetByName(
            NoDelayItem.InitialCompactionDataArrayName);
          Assert(InitialCompactionDataArray <> nil);
        end;

        if Group.LayerCount = 1 then
        begin
          FLN.Add(MFLayer_Group+1);
          FHC_List.Add(PreconsolidationHeadDataArray);
          FSfe_List.Add(ElasticSkeletalStorageCoefficientDataArray);
          FSfv_List.Add(InelasticSkeletalStorageCoefficientDataArray);
          FCom_List.Add(InitialCompactionDataArray);
          FComE_List.Add(InitialCompactionElasticDataArray);
          FComV_List.Add(InitialCompactionInelasticDataArray);
        end
        else
        begin
          if NoDelayItem.UseInAllLayers then
          begin
            for LayerIndex := 1 to Group.LayerCount do
            begin
              FLN.Add(MFLayer_Group+LayerIndex);
              FHC_List.Add(PreconsolidationHeadDataArray);
              FSfe_List.Add(ElasticSkeletalStorageCoefficientDataArray);
              FSfv_List.Add(InelasticSkeletalStorageCoefficientDataArray);
              FCom_List.Add(InitialCompactionDataArray);
              FComE_List.Add(InitialCompactionElasticDataArray);
              FComV_List.Add(InitialCompactionInelasticDataArray);
            end;
          end
          else
          begin
            for LayerIndex := 1 to Group.LayerCount do
            begin
              if NoDelayItem.UsedLayers.GetItemByLayerNumber(LayerIndex) <> nil then
              begin
                FLN.Add(MFLayer_Group+LayerIndex);
                FHC_List.Add(PreconsolidationHeadDataArray);
                FSfe_List.Add(ElasticSkeletalStorageCoefficientDataArray);
                FSfv_List.Add(InelasticSkeletalStorageCoefficientDataArray);
                FCom_List.Add(InitialCompactionDataArray);
                FComE_List.Add(InitialCompactionElasticDataArray);
                FComV_List.Add(InitialCompactionInelasticDataArray);
              end;
            end;
          end;
        end;
      end;

      for SubsidenceIndex := 0 to Group.SubDelayBedLayers.Count - 1 do
      begin
        DelayItem := Group.SubDelayBedLayers[SubsidenceIndex];
        FDelayNames.AddObject(DelayItem.Name, DelayItem);

        EquivNumberDataArray := Model.DataArrayManager.GetDataSetByName(
          DelayItem.EquivNumberDataArrayName);
        Assert(EquivNumberDataArray <> nil);

        VerticalHydraulicConductivityDataArray := Model.DataArrayManager.GetDataSetByName(
          DelayItem.VerticalHydraulicConductivityDataArrayName);
        Assert(VerticalHydraulicConductivityDataArray <> nil);

        ElasticSpecificStorageDataArray := Model.DataArrayManager.GetDataSetByName(
          DelayItem.ElasticSpecificStorageDataArrayName);
        Assert(ElasticSpecificStorageDataArray <> nil);

        InelasticSpecificStorageDataArray := Model.DataArrayManager.GetDataSetByName(
          DelayItem.InelasticSpecificStorageDataArrayName);
        Assert(InelasticSpecificStorageDataArray <> nil);

        InterbedStartingHeadDataArray := Model.DataArrayManager.GetDataSetByName(
          DelayItem.InterbedStartingHeadDataArrayName);
        if FSubPackage.ReadDelayRestartFileName = '' then
        begin
          Assert(InterbedStartingHeadDataArray <> nil);
        end;

        InterbedPreconsolidationHeadDataArray := Model.DataArrayManager.GetDataSetByName(
          DelayItem.InterbedPreconsolidationHeadDataArrayName);
        if FSubPackage.ReadDelayRestartFileName = '' then
        begin
          Assert(InterbedPreconsolidationHeadDataArray <> nil);
        end;

        if Model.ModelSelection = msModflowFmp then
        begin
          InterbedStartingCompactionDataArray := nil;
          InterbedStartingElasticCompactionDataArray := Model.DataArrayManager.GetDataSetByName(
            DelayItem.InterbedStartingElasticCompactionDataArrayName);
          Assert(InterbedStartingElasticCompactionDataArray <> nil);
          InterbedStartingInelasticCompactionDataArray := Model.DataArrayManager.GetDataSetByName(
            DelayItem.InterbedStartingInelasticCompactionDataArrayName);
          Assert(InterbedStartingInelasticCompactionDataArray <> nil);
        end
        else
        begin
          InterbedStartingElasticCompactionDataArray := nil;
          InterbedStartingInelasticCompactionDataArray := nil;
          InterbedStartingCompactionDataArray := Model.DataArrayManager.GetDataSetByName(
            DelayItem.InterbedStartingCompactionDataArrayName);
          Assert(InterbedStartingCompactionDataArray <> nil);
        end;

        InterbedEquivalentThicknessDataArray := Model.DataArrayManager.GetDataSetByName(
          DelayItem.InterbedEquivalentThicknessDataArrayName);
        Assert(InterbedEquivalentThicknessDataArray <> nil);

        if Group.LayerCount = 1 then
        begin
          FLDN.Add(MFLayer_Group+1);
          FRNB_List.Add(EquivNumberDataArray);
          FDelayVK_List.Add(VerticalHydraulicConductivityDataArray);
          FDelayElasticSpecificStorage_List.Add(ElasticSpecificStorageDataArray);
          FDelayInElasticSpecificStorage_List.Add(InelasticSpecificStorageDataArray);
          FDstart_List.Add(InterbedStartingHeadDataArray);
          FDHC_List.Add(InterbedPreconsolidationHeadDataArray);
          FDCOM_List.Add(InterbedStartingCompactionDataArray);
          FDCOM_E_List.Add(InterbedStartingElasticCompactionDataArray);
          FDCOM_V_List.Add(InterbedStartingInelasticCompactionDataArray);
          FDZ_List.Add(InterbedEquivalentThicknessDataArray);
        end
        else
        begin
          if DelayItem.UseInAllLayers then
          begin
            for LayerIndex := 1 to Group.LayerCount do
            begin
              FLDN.Add(MFLayer_Group+LayerIndex);
              FRNB_List.Add(EquivNumberDataArray);
              FDelayVK_List.Add(VerticalHydraulicConductivityDataArray);
              FDelayElasticSpecificStorage_List.Add(ElasticSpecificStorageDataArray);
              FDelayInElasticSpecificStorage_List.Add(InelasticSpecificStorageDataArray);
              FDstart_List.Add(InterbedStartingHeadDataArray);
              FDHC_List.Add(InterbedPreconsolidationHeadDataArray);
              FDCOM_List.Add(InterbedStartingCompactionDataArray);
              FDCOM_E_List.Add(InterbedStartingElasticCompactionDataArray);
              FDCOM_V_List.Add(InterbedStartingInelasticCompactionDataArray);
              FDZ_List.Add(InterbedEquivalentThicknessDataArray);
            end;
          end
          else
          begin
            for LayerIndex := 1 to Group.LayerCount do
            begin
              if DelayItem.UsedLayers.GetItemByLayerNumber(LayerIndex) <> nil then
              begin
                FLDN.Add(MFLayer_Group+LayerIndex);
                FRNB_List.Add(EquivNumberDataArray);
                FDelayVK_List.Add(VerticalHydraulicConductivityDataArray);
                FDelayElasticSpecificStorage_List.Add(ElasticSpecificStorageDataArray);
                FDelayInElasticSpecificStorage_List.Add(InelasticSpecificStorageDataArray);
                FDstart_List.Add(InterbedStartingHeadDataArray);
                FDHC_List.Add(InterbedPreconsolidationHeadDataArray);
                FDCOM_List.Add(InterbedStartingCompactionDataArray);
                FDCOM_E_List.Add(InterbedStartingElasticCompactionDataArray);
                FDCOM_V_List.Add(InterbedStartingInelasticCompactionDataArray);
                FDZ_List.Add(InterbedEquivalentThicknessDataArray);
              end;
            end;
          end;
        end;
      end;
      Inc(MFLayer_Group,Group.LayerCount);
    end;
  end;
  Assert(FLN.Count = FHC_List.Count);
  Assert(FLN.Count = FSfe_List.Count);
  Assert(FLN.Count = FSfv_List.Count);
  Assert(FLN.Count = FCom_List.Count);
  Assert(FLN.Count = FComE_List.Count);
  Assert(FLN.Count = FComV_List.Count);

  Assert(FLDN.Count = FRNB_List.Count);
  Assert(FLDN.Count = FDelayVK_List.Count);
  Assert(FLDN.Count = FDelayElasticSpecificStorage_List.Count);
  Assert(FLDN.Count = FDelayInElasticSpecificStorage_List.Count);
  Assert(FLDN.Count = FDstart_List.Count);
  Assert(FLDN.Count = FDCOM_List.Count);
  Assert(FLDN.Count = FDCOM_E_List.Count);
  Assert(FLDN.Count = FDCOM_V_List.Count);
  Assert(FLDN.Count = FDZ_List.Count);
end;

procedure TModflowSUB_Writer.WriteDataSet1;
var
  ISUBCB: Integer;
  ISUBOC: Integer;
  NNDB: Integer;
  NDB: Integer;
  NMZ: integer;
  NN: integer;
  AC1: Double;
  AC2: Double;
  ITMIN: Integer;
  IDSAVE: Integer;
  SaveRestartFileName: string;
  IDREST: Integer;
  SUBLNK: integer;
  ReadRestartFileName: string;
  BaseName: string;
begin
  GetFlowUnitNumber(ISUBCB);
  ISUBOC := FSubPackage.PrintChoices.Count;
  NNDB := Model.LayerStructure.NoDelayCount;
  NDB := Model.LayerStructure.DelayCount;
  if (NNDB = 0) and (NDB = 0) then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoSubsidenceLayers,
      StrNeitherDelayBedsN);
  end;
  NMZ := FDP_List.Count;
  NN := FSubPackage.NumberOfNodes;
  AC1 := FSubPackage.AccelerationParameter1;
  AC2 := FSubPackage.AccelerationParameter2;
  ITMIN := FSubPackage.MinIterations;
  SaveRestartFileName := '';
  BaseName := ChangeFileExt(FNameOfFile, '');
  if FSubPackage.SaveDelayRestart then
  begin
    IDSAVE := Model.UnitNumbers.UnitNumber(StrSUBSaveRestart);

    SaveRestartFileName := ExtractFileName(ChangeFileExt(BaseName, '.rst'));
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDATABINARY, IDSAVE,
        SaveRestartFileName, foOutput, Model);
    end;
  end
  else
  begin
    IDSAVE := 0;
  end;

  if FSubPackage.ReadDelayRestartFileName = '' then
  begin
    IDREST := 0;
  end
  else
  begin
    IDREST := Model.UnitNumbers.UnitNumber(StrSUBReadRestart);
    ReadRestartFileName := ExtractRelativePath(BaseName,
      FSubPackage.ReadDelayRestartFileName);
    if SaveRestartFileName = ReadRestartFileName then
    begin
      frmErrorsAndWarnings.AddError(Model, StrRestartFileNamesI,
        StrTheRestartFileSav);
    end;
    if not WritingTemplate then
    begin
      WriteToNameFile(StrDATABINARY, IDREST,
        ReadRestartFileName, foInputAlreadyExists, Model, True);
    end;
  end;

  SUBLNK := Ord(FSubPackage.LinkSubsidence);

  WriteInteger(ISUBCB);
  WriteInteger(ISUBOC);
  WriteInteger(NNDB);
  WriteInteger(NDB);
  WriteInteger(NMZ);
  WriteInteger(NN);
  WriteFloat(AC1);
  WriteFloat(AC2);
  WriteInteger(ITMIN);
  WriteInteger(IDSAVE);
  WriteInteger(IDREST);

  if Model.ModelSelection = msModflowFmp then
  begin
    WriteInteger(SUBLNK);
  end;

  WriteString(' # ISUBCB ISUBOC NNDB NDB NMZ NN AC1 AC2 ITMIN IDSAVE IDREST');
  if Model.ModelSelection = msModflowFmp then
  begin
    WriteString(' SUBLNK');
  end;

  NewLine;
  
end;

procedure TModflowSUB_Writer.WriteDataSet15;
var
  Ifm1: Integer;
  Iun1: Integer;
  AFileName: string;
  Ifm2: Integer;
  Iun2: Integer;
  Ifm3: Integer;
  Iun3: Integer;
  Ifm4: Integer;
  Iun4: Integer;
  Ifm5: Integer;
  Iun5: Integer;
  Ifm6: Integer;
  Iun6: Integer;
  Index: Integer;
  PrintChoice: TSubPrintItem;
  Save1: Boolean;
  Save2: Boolean;
  Save3: Boolean;
  Save4: Boolean;
  Save5: Boolean;
  Save6: Boolean;
  SubFileName: string;
  Save7: Boolean;
  Save8: Boolean;
  Save9: Boolean;
  Save10: Boolean;
  Ifm7: Integer;
  Iun7: Integer;
  Ifm8: Integer;
  Iun8: Integer;
  Ifm9: Integer;
  Iun9: Integer;
  Ifm10: Integer;
  Iun10: Integer;
  BaseName: string;
  function GetCombinedUnitNumber: integer;
  begin
    result := Model.UnitNumbers.UnitNumber(StrSubSUB_Out);
    if SubFileName = '' then
    begin
      SubFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubOut));
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATABINARY, result,
                SubFileName, foOutput, Model, True);
      end;
      FCombinedSubFileName := SubFileName;
    end;
  end;
begin
  BaseName := ChangeFileExt(FNameOfFile, '');
  if FSubPackage.PrintChoices.Count > 0 then
  begin
    Save1 := False;
    Save2 := False;
    Save3 := False;
    Save4 := False;
    Save5 := False;
    Save6 := False;
    Save7 := False;
    Save8 := False;
    Save9 := False;
    Save10 := False;
    for Index := 0 to FSubPackage.PrintChoices.Count - 1 do
    begin
      PrintChoice := FSubPackage.PrintChoices[Index];
      Save1 := Save1 or PrintChoice.SaveSubsidence;
      Save2 := Save2 or PrintChoice.SaveCompactionByModelLayer;
      Save3 := Save3 or PrintChoice.SaveCompactionByInterbedSystem;
      Save4 := Save4 or PrintChoice.SaveVerticalDisplacement;
      Save5 := Save5 or PrintChoice.SaveCriticalHeadNoDelay;
      Save6 := Save6 or PrintChoice.SaveCriticalHeadDelay;
      Save7 := Save7 or PrintChoice.SaveElasticCompactionByModelLayer;
      Save8 := Save8 or PrintChoice.SaveInelasticCompactionByModelLayer;
      Save9 := Save9 or PrintChoice.SaveElasticCompactionByInterbedSystem;
      Save10 := Save10 or PrintChoice.SaveInelasticCompactionByInterbedSystem;
      if Save1 and Save2 and Save3 and Save4 and Save5 and Save6
        and Save7 and Save8 and Save9 and Save10 then
      begin
        break;
      end;
    end;
    Ifm1 := FSubPackage.PrintFormats.SubsidenceFormat+1;
    SubFileName := '';
    Iun1 := 0;
    if Save1 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun1 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun1 := Model.UnitNumbers.UnitNumber(StrSubSUB_Out);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubSubOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun1,
                            AFileName, foOutput, Model, True);
            end;
            FMultipleSubFileNames[0] := AFileName;
          end
        else Assert(False);
      end;
    end;

    Ifm2 := FSubPackage.PrintFormats.CompactionByModelLayerFormat+1;
    Iun2 := 0;
    if Save2 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun2 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun2 := Model.UnitNumbers.UnitNumber(StrSubCOM_ML_Out);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubComMlOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun2,
                            AFileName, foOutput, Model, True);
            end;
            FMultipleSubFileNames[1] := AFileName;
          end
        else Assert(False);
      end;
    end;

    Ifm3 := FSubPackage.PrintFormats.CompactionByInterbedSystemFormat+1;
    Iun3 := 0;
    if Save3 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun3 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun3 := Model.UnitNumbers.UnitNumber(StrSubCOM_IS_Out);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubComIsOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun3,
                            AFileName, foOutput, Model, True);
            end;
            FMultipleSubFileNames[2] := AFileName;
            FMultipleSubFileNames[3] := AFileName;
          end
        else Assert(False);
      end;
    end;

    Ifm4 := FSubPackage.PrintFormats.VerticalDisplacementFormat+1;
    Iun4 := 0;
    if Save4 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun4 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun4 := Model.UnitNumbers.UnitNumber(StrSub_VD_Out);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubVdOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun4,
                            AFileName, foOutput, Model, True);
            end;
            FMultipleSubFileNames[4] := AFileName;
          end
        else Assert(False);
      end;
    end;

    Ifm5 := FSubPackage.PrintFormats.NoDelayPreconsolidationHeadFormat+1;
    Iun5 := 0;
    if Save5 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun5 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun5 := Model.UnitNumbers.UnitNumber(StrSub_NDPCH_Out);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubNdCritHeadOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun5,
                            AFileName, foOutput, Model, True);
            end;
            FMultipleSubFileNames[5] := AFileName;
          end
        else Assert(False);
      end;
    end;

    Ifm6 := FSubPackage.PrintFormats.DelayPreconsolidationHeadFormat+1;
    Iun6 := 0;
    if Save6 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun6 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun6 := Model.UnitNumbers.UnitNumber(StrSub_DPCH_Out);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubDCritHeadOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun6,
                            AFileName, foOutput, Model, True);
            end;
            FMultipleSubFileNames[6] := AFileName;
          end
        else Assert(False);
      end;
    end;

    Ifm7 := FSubPackage.PrintFormats.ElasticCompactionByModelLayerFormat+1;
    Iun7 := 0;
    if Save7 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun7 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun7 := Model.UnitNumbers.UnitNumber(StrSubElasCompML);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubElasCompMLOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun7,
                            AFileName, foOutput, Model, True);
            end;
          end
        else Assert(False);
      end;
    end;

    Ifm8 := FSubPackage.PrintFormats.InelasticCompactionByModelLayerFormat+1;
    Iun8 := 0;
    if Save7 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun8 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun8 := Model.UnitNumbers.UnitNumber(StrSubInelasCompML);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubInelasCompMLOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun8,
                            AFileName, foOutput, Model, True);
            end;
          end
        else Assert(False);
      end;
    end;

    Ifm9 := FSubPackage.PrintFormats.ElasticCompactionByInterbedSystemFormat+1;
    Iun9 := 0;
    if Save7 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun9 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun9 := Model.UnitNumbers.UnitNumber(StrSubElasCompIB);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubElasCompIBOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun9,
                            AFileName, foOutput, Model, True);
            end;
          end
        else Assert(False);
      end;
    end;


    Ifm10 := FSubPackage.PrintFormats.InelasticCompactionByInterbedSystemFormat+1;
    Iun10 := 0;
    if Save7 then
    begin
      case FSubPackage.BinaryOutputChoice of
        sbocSingleFile:
          begin
            Iun10 := GetCombinedUnitNumber;
          end;
        sbocMultipleFiles:
          begin
            Iun10 := Model.UnitNumbers.UnitNumber(StrSubInelasCompIB);
            AFileName := ExtractFileName(ChangeFileExt(BaseName, StrSubInlasCompIBOut));
            if not WritingTemplate then
            begin
              WriteToNameFile(StrDATABINARY, Iun10,
                            AFileName, foOutput, Model, True);
            end;
          end
        else Assert(False);
      end;
    end;

    WriteInteger(Ifm1);
    WriteInteger(Iun1);
    WriteInteger(Ifm2);
    WriteInteger(Iun2);
    WriteInteger(Ifm3);
    WriteInteger(Iun3);
    WriteInteger(Ifm4);
    WriteInteger(Iun4);
    WriteInteger(Ifm5);
    WriteInteger(Iun5);
    WriteInteger(Ifm6);
    WriteInteger(Iun6);
    if Model.ModelSelection = msModflowFMP then
    begin
      WriteInteger(Ifm7);
      WriteInteger(Iun7);
      WriteInteger(Ifm8);
      WriteInteger(Iun8);
      WriteInteger(Ifm9);
      WriteInteger(Iun9);
      WriteInteger(Ifm10);
      WriteInteger(Iun10);
    end;

    WriteString(' # Ifm1 Iun1 Ifm2 Iun2 Ifm3 Iun3 Ifm4 Iun4 Ifm5 Iun5 Ifm6 Iun6');
    if Model.ModelSelection = msModflowFMP then
    begin
      WriteString(' Ifm7 Iun7 Ifm8 Iun8 Ifm9 Iun9 Ifm10 Iun10');
    end;
    NewLine;
  end;
end;

procedure TModflowSUB_Writer.WriteDataSet16;
var
  PrintChoice: TSubPrintItem;
  ISP1, ISP2, ITS1, ITS2: integer;
  PrintChoiceIndex: Integer;
  Ifl1: Integer;
  Ifl2: Integer;
  Ifl3: Integer;
  Ifl4: Integer;
  Ifl5: Integer;
  Ifl6: Integer;
  Ifl7: Integer;
  Ifl8: Integer;
  Ifl9: Integer;
  Ifl10: Integer;
  Ifl11: Integer;
  Ifl12: Integer;
  Ifl13: Integer;
  Ifl14: Integer;
  Ifl15: Integer;
  Ifl16: Integer;
  Ifl17: Integer;
  Ifl18: Integer;
  Ifl19: Integer;
  Ifl20: Integer;
  Ifl21: Integer;
begin
  FSubPackage.PrintChoices.ReportErrors;
  for PrintChoiceIndex := 0 to FSubPackage.PrintChoices.Count -1 do
  begin
    PrintChoice := FSubPackage.PrintChoices[PrintChoiceIndex];
    if PrintChoice.StartTime <= PrintChoice.EndTime then
    begin
      GetStartAndEndTimeSteps(ITS2, ISP2, ITS1, ISP1, PrintChoice);
      Ifl1 := Ord(PrintChoice.PrintSubsidence);
      Ifl2  := Ord(PrintChoice.SaveSubsidence);
      Ifl3 := Ord(PrintChoice.PrintCompactionByModelLayer);
      Ifl4 := Ord(PrintChoice.SaveCompactionByModelLayer);
      Ifl5 := Ord(PrintChoice.PrintCompactionByInterbedSystem);
      Ifl6 := Ord(PrintChoice.SaveCompactionByInterbedSystem);
      Ifl7 := Ord(PrintChoice.PrintVerticalDisplacement);
      Ifl8 := Ord(PrintChoice.SaveVerticalDisplacement);
      Ifl9 := Ord(PrintChoice.PrintCriticalHeadNoDelay);
      Ifl10 := Ord(PrintChoice.SaveCriticalHeadNoDelay);
      Ifl11 := Ord(PrintChoice.PrintCriticalHeadDelay);
      Ifl12 := Ord(PrintChoice.SaveCriticalHeadDelay);
      Ifl13 := Ord(PrintChoice.PrintDelayBudgets);

      Ifl14 := Ord(PrintChoice.PrintElasticCompactionByModelLayer);
      Ifl15 := Ord(PrintChoice.SaveElasticCompactionByModelLayer);
      Ifl16 := Ord(PrintChoice.PrintInelasticCompactionByModelLayer);
      Ifl17 := Ord(PrintChoice.SaveInelasticCompactionByModelLayer);
      Ifl18 := Ord(PrintChoice.PrintElasticCompactionByInterbedSystem);
      Ifl19 := Ord(PrintChoice.SaveElasticCompactionByInterbedSystem);
      Ifl20 := Ord(PrintChoice.PrintInelasticCompactionByInterbedSystem);
      Ifl21 := Ord(PrintChoice.SaveInelasticCompactionByInterbedSystem);

      WriteInteger(ISP1);
      WriteInteger(ISP2);
      WriteInteger(ITS1);
      WriteInteger(ITS2);
      WriteInteger(Ifl1);
      WriteInteger(Ifl2);
      WriteInteger(Ifl3);
      WriteInteger(Ifl4);
      WriteInteger(Ifl5);
      WriteInteger(Ifl6);
      WriteInteger(Ifl7);
      WriteInteger(Ifl8);
      WriteInteger(Ifl9);
      WriteInteger(Ifl10);
      WriteInteger(Ifl11);
      WriteInteger(Ifl12);
      WriteInteger(Ifl13);

      if Model.ModelSelection = msModflowFmp then
      begin
        WriteInteger(Ifl14);
        WriteInteger(Ifl15);
        WriteInteger(Ifl16);
        WriteInteger(Ifl17);
        WriteInteger(Ifl18);
        WriteInteger(Ifl19);
        WriteInteger(Ifl20);
        WriteInteger(Ifl21);
      end;

      WriteString(' # ISP1 ISP2 ITS1 ITS2 Ifl1 Ifl2 Ifl3 Ifl4 Ifl5 Ifl6 Ifl7 Ifl8 Ifl9 Ifl10 Ifl11 Ifl12 Ifl13');
      if Model.ModelSelection = msModflowFmp then
      begin
        WriteString(' Ifl14 Ifl15 Ifl16 Ifl17 Ifl18 Ifl19 Ifl20 Ifl21');
      end;
      NewLine;
    end;
  end;

end;

procedure TModflowSUB_Writer.WriteDataSet2;
var
  Index: Integer;
begin
  if FLN.Count = 0 then
  begin
    Exit;
  end;
  for Index := 0 to FLN.Count - 1 do
  begin
    WriteInteger(FLN[Index]);
  end;
  WriteString(' # LN');
  NewLine;
end;

procedure TModflowSUB_Writer.WriteDataSet3;
var
  Index: Integer;
begin
  if FLDN.Count = 0 then
  begin
    Exit;
  end;
  for Index := 0 to FLDN.Count - 1 do
  begin
    WriteInteger(FLDN[Index]);
  end;
  WriteString(' # LDN');
  NewLine;
end;

procedure TModflowSUB_Writer.WriteDataSet4;
var
  Index: Integer;
  DataArray: TDataArray;
begin
  for Index := 0 to FRNB_List.Count - 1 do
  begin
    DataArray := FRNB_List[Index];
    WriteArray(DataArray, 0, 'RNB', StrNoValueAssigned, 'RNB');
    Model.DataArrayManager.AddDataSetToCache(DataArray);
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
  end;
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSUB_Writer.WriteDataSet9;
var
  MaterialZone: TMaterialZoneIdItem;
  Index: Integer;
begin
  if FDP_List.Count = 0 then
  begin
    Exit;
  end;
  for Index := 0 to FDP_List.Count - 1 do
  begin
    MaterialZone := FDP_List[Index];
    WriteFloat(MaterialZone.FMaterialZoneValues.VerticalK);
    WriteFloat(MaterialZone.FMaterialZoneValues.ElasticSpecificStorage);
    WriteFloat(MaterialZone.FMaterialZoneValues.InelasticSpecificStorage);
    WriteString(' # DP');
    NewLine;
  end;
end;

procedure TModflowSUB_Writer.WriteDataSets10to14;
var
  Index: Integer;
  DataArray: TDataArray;
begin
  for Index := 0 to FDstart_List.Count - 1 do
  begin
    if FSubPackage.ReadDelayRestartFileName = '' then
    begin
      DataArray := FDstart_List[Index];
      WriteArray(DataArray, 0, 'Dstart', StrNoValueAssigned, 'Dstart');
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
      Model.DataArrayManager.AddDataSetToCache(DataArray);

      DataArray := FDHC_List[Index];
      WriteArray(DataArray, 0, 'DHC', StrNoValueAssigned, 'DHC');
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
      Model.DataArrayManager.AddDataSetToCache(DataArray);
    end;

    DataArray := FDCOM_List[Index];
    if DataArray <> nil then
    begin
      WriteArray(DataArray, 0, 'DCOM', StrNoValueAssigned, 'DCOM');
      Model.DataArrayManager.AddDataSetToCache(DataArray);
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
    end;

    DataArray := FDCOM_E_List[Index];
    if DataArray <> nil then
    begin
      WriteArray(DataArray, 0, 'DCOME', StrNoValueAssigned, 'DCOME');
      Model.DataArrayManager.AddDataSetToCache(DataArray);
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
    end;

    DataArray := FDCOM_V_List[Index];
    if DataArray <> nil then
    begin
      WriteArray(DataArray, 0, 'DCOMV', StrNoValueAssigned, 'DCOMV');
      Model.DataArrayManager.AddDataSetToCache(DataArray);
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
    end;

    DataArray := FDZ_List[Index];
    WriteArray(DataArray, 0, 'DZ', StrNoValueAssigned, 'DZ');
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
    Model.DataArrayManager.AddDataSetToCache(DataArray);

    DataArray := FNZ_List[Index];
    WriteArray(DataArray, 0, 'NZ', StrNoValueAssigned, 'NZ');
    // This one isn't cached because it is temporary
  end;
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSUB_Writer.WriteDataSets5to8;
var
  Index: Integer;
  DataArray: TDataArray;
begin
  for Index := 0 to FHC_List.Count - 1 do
  begin
    DataArray := FHC_List[Index];
    WriteArray(DataArray, 0, 'HC', StrNoValueAssigned, 'HC');
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
    Model.DataArrayManager.AddDataSetToCache(DataArray);

    DataArray := FSfe_List[Index];
    WriteArray(DataArray, 0, 'Sfe', StrNoValueAssigned, 'Sfe');
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
    Model.DataArrayManager.AddDataSetToCache(DataArray);

    DataArray := FSfv_List[Index];
    WriteArray(DataArray, 0, 'Sfv', StrNoValueAssigned, 'Sfv');
    if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
    begin
      FPestDataArrays.Add(DataArray);
    end;
    Model.DataArrayManager.AddDataSetToCache(DataArray);

    DataArray := FCom_List[Index];
    if DataArray <> nil then
    begin
      WriteArray(DataArray, 0, 'Com', StrNoValueAssigned, 'Com');
      Model.DataArrayManager.AddDataSetToCache(DataArray);
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
    end;
    DataArray := FComE_List[Index];
    if DataArray <> nil then
    begin
      WriteArray(DataArray, 0, 'ComE', StrNoValueAssigned, 'ComE');
      Model.DataArrayManager.AddDataSetToCache(DataArray);
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
    end;
    DataArray := FComV_List[Index];
    if DataArray <> nil then
    begin
      WriteArray(DataArray, 0, 'ComV', StrNoValueAssigned, 'ComV');
      Model.DataArrayManager.AddDataSetToCache(DataArray);
      if DataArray.PestParametersUsed and (FPestDataArrays.IndexOf(DataArray) < 0) then
      begin
        FPestDataArrays.Add(DataArray);
      end;
    end;
  end;
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TModflowSUB_Writer.WriteObsScript;
var
  ScriptFileName: string;
  ObsIndex: Integer;
  Obs: TSubObsItem;
  StartTime: Double;
  ObsTypeIndex: Integer;
  ActiveDataArray: TDataArray;
  InterbedSystemIndex: Integer;
  ObjectIndex: Integer;
  SubObservations: TSubObservations;
  CompIndex: Integer;
  CompItem: TObsCompareItem;
  FoundFirstObs: Boolean;
  function GetObName(ObjectIndex: Integer; Obs: TCustomObservationItem): string;
  begin
    Result := PrefixedObsName('Sub', ObjectIndex, Obs);
  end;
  procedure WriteObs(Obs: TSubObsItem);
  var
    CellIndex: Integer;
    ACell: TInterpolatedObsCell;
  begin
    if Obs.Cells = nil then
    begin
      Exit;
    end;
  
    WriteString('  OBSERVATION ');
    WriteString(GetObName(ObsIndex, Obs));
    WriteString(' "');
    WriteString(Obs.ObsType);
    WriteString('" ');
    WriteFloat(Obs.Time - StartTime);
    WriteFloat(Obs.ObservedValue);
    WriteFloat(Obs.Weight);
    WriteString(' PRINT');
    NewLine;

    InterbedSystemIndex := -1;
    if AnsiSameText(Obs.ObsType, rsNDSYSCOMPACT) then
    begin
      InterbedSystemIndex := FNoDelayNames.IndexOf(Obs.InterbedSystem) + 1;
    end
    else if AnsiSameText(Obs.ObsType, rsDSYSCOMPACTI) then
    begin
      InterbedSystemIndex := FDelayNames.IndexOf(Obs.InterbedSystem) + 1;
    end;

    for CellIndex := 0 to Obs.Cells.Cells.Count - 1 do
    begin
      ACell := Obs.Cells.Cells[CellIndex];
      WriteString('  CELL ');
      if AnsiSameText(Obs.ObsType, rsNDSYSCOMPACT)
        or AnsiSameText(Obs.ObsType, rsDSYSCOMPACTI) then
      begin
        WriteInteger(InterbedSystemIndex);
      end
      else
      begin
        WriteInteger(ACell.Layer+1);
      end;
      WriteInteger(ACell.Row+1);
      WriteInteger(ACell.Col+1);
      WriteFloat(ACell.Fraction);
      NewLine;
    end;
  end;
begin
  if Model.PestUsed then
  begin
    if FObsList.Count = 0 then
    begin
      Exit;
    end;
    ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    ActiveDataArray.Initialize;

    StartTime := Model.ModflowFullStressPeriods.First.StartTime;
    ScriptFileName := ChangeFileExt(FNameOfFile, '.SubObsScript');
    OpenFile(ScriptFileName);
    try
      WriteString('BEGIN OBSERVATIONS');
      NewLine;
      if FSubPackage.BinaryOutputChoice = sbocSingleFile then
      begin
        WriteString('  FILENAME "');
        WriteString(FCombinedSubFileName);
        WriteString('"');
        NewLine;

        for ObsIndex := 0 to FObsList.Count - 1 do
        begin
          Obs := FObsList[ObsIndex];
          WriteObs(Obs);
        end;
      end
      else
      begin
        for ObsTypeIndex := 0 to SubsidenceTypes.Count - 1 do
        begin
          if FMultipleSubFileNames[ObsTypeIndex] <> '' then
          begin
            FoundFirstObs := False;

            for ObsIndex := 0 to FObsList.Count - 1 do
            begin
              Obs := FObsList[ObsIndex];
              if AnsiSameText(SubsidenceTypes[ObsTypeIndex], Obs.ObsType) then
              begin
                if not FoundFirstObs then
                begin
                  WriteString('  FILENAME "');
                  WriteString(FMultipleSubFileNames[ObsTypeIndex]);
                  WriteString('"');
                  NewLine;
                  FoundFirstObs := True;
                end;
                WriteObs(Obs);
              end;
            end;
          end;
        end;
      end;
      WriteString('END OBSERVATIONS');

      // DERIVED_OBSERVATIONS block
      if FSubObsCollectionList.Count > 0 then
      begin
        NewLine;
        NewLine;
        WriteString('BEGIN DERIVED_OBSERVATIONS');
        NewLine;

        for ObjectIndex := 0 to FSubObsCollectionList.Count - 1 do
        begin
          SubObservations := FSubObsCollectionList[ObjectIndex];

          WriteString('  # ');
          WriteString('Observation comparisons defined in ');
          WriteString((SubObservations.ScreenObject as TScreenObject).Name);
          NewLine;

          for CompIndex := 0 to SubObservations.Comparisons.Count - 1 do
          begin
            WriteString('  DIFFERENCE ');
            CompItem := SubObservations.Comparisons[CompIndex];
            WriteString(GetObName(ObjectIndex, CompItem));
            WriteString(' ');
            Obs := SubObservations[CompItem.Index1];
            WriteString(Obs.ExportedName);
            WriteString(' ');
            Obs := SubObservations[CompItem.Index2];
            WriteString(Obs.ExportedName);
            WriteFloat(CompItem.ObservedValue);
            WriteFloat(CompItem.Weight);
            WriteString(' PRINT');
            NewLine;
          end;
        end;

        WriteString('END DERIVED_OBSERVATIONS');
      end;
    finally
      CloseFile;
    end;
  end;
end;

procedure TModflowSUB_Writer.WritePestScripts;
var
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
begin
  for DataArrayIndex := 0 to FPestDataArrays.Count - 1 do
  begin
    ADataArray := FPestDataArrays[DataArrayIndex];
    WritePestZones(ADataArray, FInputFileName, Format(StrSUBd, [DataArrayIndex+1]));
  end;
end;

procedure TModflowSUB_Writer.WriteFile(const AFileName: string);
begin
  FSubPackage := Package as TSubPackageSelection;
  if not FSubPackage.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrSUB) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrSubsidenceNotSuppo);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrRestartFileNamesI);
    if Model is TChildModel then
    begin
      frmErrorsAndWarnings.AddError(Model, StrSubsidenceNotSuppo,
        StrModelMuseDoesNotC);
    end;

    FNameOfFile := FileName(AFileName);
    FInputFileName := FNameOfFile;
    WriteToNameFile(StrSUB, Model.UnitNumbers.UnitNumber(StrSUB),
      FNameOfFile, foInput, Model);
    Evaluate;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    OpenFile(FNameOfFile);
    try
      frmProgressMM.AddMessage(StrWritingSUBPackage);

      WriteDataSet0;

      frmProgressMM.AddMessage(StrWritingDataSet1);
      WriteDataSet1;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet2);
      WriteDataSet2;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet3);
      WriteDataSet3;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet4);
      WriteDataSet4;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSets5to8);
      WriteDataSets5to8;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet9);
      WriteDataSet9;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSets10to14);
      WriteDataSets10to14;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet15);
      WriteDataSet15;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingDataSet16);
      WriteDataSet16;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WritePestScripts;
    finally
      CloseFile;
    end;
    
    WriteObsScript;

  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

{ TMaterialZoneIdItem }

procedure TMaterialZoneIdItem.AssignID;
begin
  ID := MaterialZoneValues.ID;
end;

constructor TMaterialZoneIdItem.Create;
begin
  FNextSameID := nil;
end;

destructor TMaterialZoneIdItem.Destroy;
begin
  NextSameID.Free;
  inherited;
end;

procedure TMaterialZoneIdItem.SetMaterialZoneValues(const Value: TMaterialZone);
begin
  FMaterialZoneValues := Value;
  AssignID;
end;

{ TMaterialZoneList }

function TMaterialZoneList.AddRecord(
  MaterialZone: TMaterialZone): TMaterialZoneIdItem;
var
  LastResult: TMaterialZoneIdItem;
begin
  result := FIdList.ByID[MaterialZone.ID] as TMaterialZoneIdItem;
  if result = nil then
  begin
    result := TMaterialZoneIdItem.Create;
    result.MaterialZoneValues := MaterialZone;
    result.ZoneNumber := FMaterialZones.Add(result)+1;
    FIdList.Add(result);
  end
  else
  begin
    LastResult := nil;
    while Assigned(result) and (result.MaterialZoneValues <> MaterialZone) do
    begin
      LastResult := result;
      result := result.NextSameID;
    end;
    if result = nil then
    begin
      result := TMaterialZoneIdItem.Create;
      result.MaterialZoneValues := MaterialZone;
      result.ZoneNumber := FMaterialZones.Add(result)+1;
      LastResult.NextSameID := result;
    end;
  end;
end;

function TMaterialZoneList.Count: integer;
begin
  result := FMaterialZones.Count;
end;

constructor TMaterialZoneList.Create;
begin
  FMaterialZones:= TList.Create;
  FIdList := TIDList.Create;
end;

destructor TMaterialZoneList.Destroy;
begin
  FIdList.Free;
  FMaterialZones.Free;
  inherited;
end;

function TMaterialZoneList.GetItem(Index: integer): TMaterialZoneIdItem;
begin
  result := FMaterialZones[Index];
end;

end.
