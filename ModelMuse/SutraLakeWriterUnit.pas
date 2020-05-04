unit SutraLakeWriterUnit;

interface

uses
  CustomModflowWriterUnit, System.Generics.Collections, PhastModelUnit,
  SutraOptionsUnit, System.SysUtils, SutraOutputControlUnit, System.Classes,
  SutraBoundaryWriterUnit;

type
  TLakeNodeRecord = record
    InitialStage: double;
    InitialU: double;
    RechargeFraction: double;
    DischargeFraction: double;
    InitialStageAnnotation: string;
    InitialUAnnotation: string;
    RechargeFractionAnnotation: string;
    DischargeFractionAnnotation: string;
  end;

  TLakeNode = class(TObject)
    NodeNumber: Integer;
    LakeProperties: TLakeNodeRecord;
    FCol: integer;
  end;

  TLakeNodes = TObjectList<TLakeNode>;

  TSutraLakeWriter = class(TCustomFileWriter)
  private
    FLakeNodes: TLakeNodes;
    FLakeOptions: TSutraLakeOptions;
    FHasLakes: Boolean;
    FSutraOutputControl: TSutraOutputControl;
    FBcsFileNames: TLakeInteractionStringLists;
    procedure Evaluate;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteLakeAreaDataSet1a;
    procedure WriteLakeAreaDataSet1b;
    procedure WriteLakeInteractionDataSet1;
    procedure WriteLakeInteractionDataSet2;
    procedure WriteLakeInteractionDataSet3;
    procedure WriteLakeInteractionDataSet4;
    procedure WriteLakeInteractionDataSet5;
    procedure WriteLakeInteractionDataSet6A;
    procedure WriteLakeInteractionDataSet6B;
    procedure WriteLakeObsNodes;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    property HasLakes: Boolean read FHasLakes;
    procedure WriteFile(const AFileName: string; BcsFileNames: TLakeInteractionStringLists);
  end;

implementation

uses
  ScreenObjectUnit, SutraBoundariesUnit, GoPhastTypes, DataSetUnit, RbwParser,
  frmFormulaErrorsUnit, SutraMeshUnit, SutraFileWriterUnit, SutraBoundaryUnit,
  frmErrorsAndWarningsUnit, MeshRenumberingTypes;

resourcestring
  StrInitialLakeStage = 'Initial Lake Stage';
  StrInitialLakeConcent = 'Initial Lake Concentration or Temperature';
  StrInvalidLakeNode = 'Invalid Lake Node';
  StrTheLakeAtNode0 = 'The lake at node %0:d is invalid because it is at the' +
  ' edge of the mesh. The node is  defined by %1:s.';

{ TSutraLakeWriter }

constructor TSutraLakeWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FLakeNodes := TLakeNodes.Create;
end;

destructor TSutraLakeWriter.Destroy;
begin
  FLakeNodes.Free;
  inherited;
end;

procedure TSutraLakeWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ALake: TSutraLake;
  InitialStageFormula: string;
  InitalStageDataSet: TRealSparseDataSet;
  InitialU: TRealSparseDataSet;
  InitialUFormula: string;
  FractionRechargeDivertedFormula: string;
  FractionRechargeDiverted: TRealSparseDataSet;
  FractionDischargeDivertedFormula: string;
  FractionDischargeDiverted: TRealSparseDataSet;
  ColIndex: Integer;
  NodeNumber: Integer;
  LakeNodes: array of TLakeNode;
  LakeNodeRecord: TLakeNodeRecord;
  ALakeNode: TLakeNode;
  Node3D: TSutraNode3D;
  ElementIndex: Integer;
  Element2D: TSutraElement2D;
  NodeIndex: Integer;
  Node2D: TSutraNode2D;
  NeighborNode: TSutraNode3D;
  ProblemNode: Boolean;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidLakeNode);
  SetLength(LakeNodes, Model.SutraMesh.Nodes.Count);
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    ALake := ScreenObject.SutraBoundaries.Lake;
    if ALake.IsUsed then
    begin
      InitialStageFormula := ALake.InitialStage;
      InitialUFormula := ALake.InitialConcentrationOrTemperature;
      FractionRechargeDivertedFormula := ALake.FractionRechargeDiverted;
      FractionDischargeDivertedFormula := ALake.FractionDischargeDiverted;

      InitalStageDataSet := TRealSparseDataSet.Create(Model);
      InitialU := TRealSparseDataSet.Create(Model);
      FractionRechargeDiverted := TRealSparseDataSet.Create(Model);
      FractionDischargeDiverted := TRealSparseDataSet.Create(Model);
      try
        InitalStageDataSet.DataType := rdtDouble;
        InitalStageDataSet.Name := ValidName('Initial_Stage');
        InitalStageDataSet.UseLgrEdgeCells := lctUse;
        InitalStageDataSet.EvaluatedAt := eaNodes;
        InitalStageDataSet.Orientation := dsoTop;
        Model.UpdateDataArrayDimensions(InitalStageDataSet);

        InitialU.DataType := rdtDouble;
        InitialU.Name := ValidName('Initial_U');
        InitialU.UseLgrEdgeCells := lctUse;
        InitialU.EvaluatedAt := eaNodes;
        InitialU.Orientation := dsoTop;
        Model.UpdateDataArrayDimensions(InitialU);

        FractionRechargeDiverted.DataType := rdtDouble;
        FractionRechargeDiverted.Name := ValidName('Fraction_Recharge_Diverted');
        FractionRechargeDiverted.UseLgrEdgeCells := lctUse;
        FractionRechargeDiverted.EvaluatedAt := eaNodes;
        FractionRechargeDiverted.Orientation := dsoTop;
        Model.UpdateDataArrayDimensions(FractionRechargeDiverted);

        FractionDischargeDiverted.DataType := rdtDouble;
        FractionDischargeDiverted.Name := ValidName('Fraction_Discharge_Diverted');
        FractionDischargeDiverted.UseLgrEdgeCells := lctUse;
        FractionDischargeDiverted.EvaluatedAt := eaNodes;
        FractionDischargeDiverted.Orientation := dsoTop;
        Model.UpdateDataArrayDimensions(FractionDischargeDiverted);

        try
          ScreenObject.AssignValuesToSutraDataSet(Model.SutraMesh, InitalStageDataSet,
            InitialStageFormula, Model, 'True');
        except on E: ErbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(ScreenObject.Name, StrInitialLakeStage,
              InitialStageFormula, E.Message);
            InitialStageFormula := '0';
            ScreenObject.AssignValuesToSutraDataSet(Model.SutraMesh, InitalStageDataSet,
              InitialStageFormula, Model, 'True');
          end;
        end;

        try
          ScreenObject.AssignValuesToSutraDataSet(Model.SutraMesh, InitialU,
            InitialUFormula, Model, 'True');
        except on E: ErbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(ScreenObject.Name, StrInitialLakeConcent,
              InitialUFormula, E.Message);
            InitialUFormula := '0';
            ScreenObject.AssignValuesToSutraDataSet(Model.SutraMesh, InitialU,
              InitialUFormula, Model, 'True');
          end;
        end;

        try
          ScreenObject.AssignValuesToSutraDataSet(Model.SutraMesh, FractionRechargeDiverted,
            FractionRechargeDivertedFormula, Model, 'True');
        except on E: ErbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(ScreenObject.Name, StrInitialLakeConcent,
              FractionRechargeDivertedFormula, E.Message);
            FractionRechargeDivertedFormula := '0';
            ScreenObject.AssignValuesToSutraDataSet(Model.SutraMesh, FractionRechargeDiverted,
              FractionRechargeDivertedFormula, Model, 'True');
          end;
        end;

        try
          ScreenObject.AssignValuesToSutraDataSet(Model.SutraMesh, FractionDischargeDiverted,
            FractionDischargeDivertedFormula, Model, 'True');
        except on E: ErbwParserError do
          begin
            frmFormulaErrors.AddFormulaError(ScreenObject.Name, StrInitialLakeConcent,
              FractionDischargeDivertedFormula, E.Message);
            FractionDischargeDivertedFormula := '0';
            ScreenObject.AssignValuesToSutraDataSet(Model.SutraMesh, FractionDischargeDiverted,
              FractionDischargeDivertedFormula, Model, 'True');
          end;
        end;

        Assert(InitalStageDataSet.RowCount = 1);
        Assert(InitalStageDataSet.LayerCount = 1);
        for ColIndex := 0 to InitalStageDataSet.ColumnCount - 1 do
        begin
          if InitalStageDataSet.IsValue[0,0,ColIndex] then
          begin
            Assert(InitialU.IsValue[0,0,ColIndex]);
            Assert(FractionRechargeDiverted.IsValue[0,0,ColIndex]);
            Assert(FractionDischargeDiverted.IsValue[0,0,ColIndex]);

            Node3D :=Model.SutraMesh.NodeArray[0,ColIndex];
            NodeNumber := Node3D.Number;

            LakeNodeRecord.InitialStage := InitalStageDataSet.RealData[0,0,ColIndex];
            LakeNodeRecord.InitialU := InitialU.RealData[0,0,ColIndex];
            LakeNodeRecord.RechargeFraction := FractionRechargeDiverted.RealData[0,0,ColIndex];
            LakeNodeRecord.DischargeFraction := FractionDischargeDiverted.RealData[0,0,ColIndex];

            LakeNodeRecord.InitialStageAnnotation := InitalStageDataSet.Annotation[0,0,ColIndex];
            LakeNodeRecord.InitialUAnnotation := InitialU.Annotation[0,0,ColIndex];
            LakeNodeRecord.RechargeFractionAnnotation := FractionRechargeDiverted.Annotation[0,0,ColIndex];
            LakeNodeRecord.DischargeFractionAnnotation := FractionDischargeDiverted.Annotation[0,0,ColIndex];

            if LakeNodes[NodeNumber] = nil then
            begin
              ALakeNode := TLakeNode.Create;
              ALakeNode.NodeNumber := NodeNumber;
              ALakeNode.FCol := ColIndex;
              FLakeNodes.Add(ALakeNode);
              LakeNodes[NodeNumber] := ALakeNode;

              if Node3D.Node2D.NodeType = ntEdge then
              begin
                ProblemNode := True;
                for ElementIndex := 0 to Node3D.Node2D.ElementCount - 1 do
                begin
                  Element2D := Node3D.Node2D.Elements[ElementIndex];
                  for NodeIndex := 0 to Element2D.NodeCount - 1 do
                  begin
                    Node2D := Element2D.Nodes[NodeIndex].Node;
                    if Node3D.Node2D <> Node2D then
                    begin
                      NeighborNode :=Model.SutraMesh.NodeArray[0,Node2D.Number];
                      if NeighborNode.Z < Node3D.Z then
                      begin
                        ProblemNode := False;
                        Break;
                      end;
                    end;
                  end;
                  if not ProblemNode then
                  begin
                    Break;
                  end;
                end;
                if ProblemNode then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrInvalidLakeNode,
                    Format(StrTheLakeAtNode0, [NodeNumber, ScreenObject.Name]),
                    ScreenObject);
                end;
              end;
            end
            else
            begin
              ALakeNode := LakeNodes[NodeNumber];
            end;
            ALakeNode.LakeProperties := LakeNodeRecord;
          end;
        end;

        Model.DataArrayManager.CacheDataArrays;
        InitalStageDataSet.UpToDate := True;
        InitalStageDataSet.CacheData;

      finally
        InitalStageDataSet.Free;
        InitialU.Free;
        FractionRechargeDiverted.Free;
        FractionDischargeDiverted.Free;
      end;
    end;
  end;
end;

class function TSutraLakeWriter.Extension: string;
begin
  result := '.lkin';
end;

procedure TSutraLakeWriter.WriteDataSet1;
var
//  ITLMAX: Integer;
  NLAKPR: Integer;
begin
//  ITLMAX := FLakeOptions.MaxLakeIterations;
  NLAKPR := FSutraOutputControl.LakePrintFrequency;
//  WriteInteger(ITLMAX);
  WriteInteger(NLAKPR);
  NewLine;
end;

procedure TSutraLakeWriter.WriteDataSet2;
var
  NLSPEC: Integer;
  FRROD: Double;
  FDROD: Double;
//  VLIM: Double;
  RNOLK: Double;
begin
  NLSPEC := FLakeNodes.Count;
  FRROD := FLakeOptions.RechargeFraction;
  FDROD := FLakeOptions.DischargeFraction;
//  VLIM := FLakeOptions.MinLakeVolume;
  RNOLK := FLakeOptions.SubmergedOutput;
  WriteInteger(NLSPEC);
  WriteFloat(FRROD);
  WriteFloat(FDROD);
//  WriteFloat(VLIM);
  WriteFloat(RNOLK);
  NewLine;
end;

procedure TSutraLakeWriter.WriteDataSet3;
const
  CTYPE = 'NODE';
var
  NodeIndex: Integer;
  ALake: TLakeNode;
  ILON: Integer;
  STGI: Double;
  UWI: Double;
  FRRO: Double;
  FDRO: Double;
begin
  for NodeIndex := 0 to FLakeNodes.Count - 1 do
  begin
    ALake := FLakeNodes[NodeIndex];
    ILON := ALake.NodeNumber + 1;
    STGI := ALake.LakeProperties.InitialStage;
    UWI := ALake.LakeProperties.InitialU;
    FRRO := ALake.LakeProperties.RechargeFraction;
    FDRO := ALake.LakeProperties.DischargeFraction;
    WriteString(CTYPE);
    WriteInteger(ILON);
    WriteFloat(STGI);
    WriteFloat(UWI);
    WriteFloat(FRRO);
    WriteFloat(FDRO);
    NewLine;
  end;
end;

procedure TSutraLakeWriter.WriteFile(const AFileName: string; BcsFileNames: TLakeInteractionStringLists);
var
  NameOfFile: string;
  LakeStageFile: string;
  LakeRestartFile: string;
  LakeBudgetFile: string;
  LakeNodeFile: string;
  LakeHierarchyFile: string;
begin
  FHasLakes := False;
  FBcsFileNames := BcsFileNames;
  if Model.ModelSelection = msSutra22 then
  begin
    Exit;
  end;
  if Model.SutraMesh.MeshType <> mt3D then
  begin
    Exit;
  end;

  FLakeOptions := Model.SutraOptions.LakeOptions;
  FSutraOutputControl := Model.SutraOutputControl;

  if not FLakeOptions.UseLakes then
  begin
    Exit;
  end;

  Evaluate;
  FHasLakes := True;

  NameOfFile := FileName(AFileName);
  OpenFile(NameOfFile);
  try
    WriteDataSet1;
    WriteDataSet2;
    WriteDataSet3;
  finally
    CloseFile;
  end;
  SutraFileWriter.AddFile(sftLkin, NameOfFile);

  if not FLakeOptions.AllNodesLakes then
  begin
    NameOfFile := ChangeFileExt(AFileName, '.lkar');
    OpenFile(NameOfFile);
    try
      WriteLakeAreaDataSet1a;
      WriteLakeAreaDataSet1b;
    finally
      CloseFile;
    end;
    SutraFileWriter.AddFile(sftLkar, NameOfFile);
  end;

  NameOfFile := ChangeFileExt(AFileName, '.lkbc');
  OpenFile(NameOfFile);
  try
    WriteLakeInteractionDataSet1;
    WriteLakeInteractionDataSet2;
    WriteLakeInteractionDataSet3;
    WriteLakeInteractionDataSet4;
    WriteLakeInteractionDataSet5;
    WriteLakeInteractionDataSet6A;
    WriteLakeInteractionDataSet6B;
  finally
    CloseFile;
  end;
  SutraFileWriter.AddFile(sftLkbc, NameOfFile);


  LakeStageFile := ChangeFileExt(AFileName, '.lkst');
  SutraFileWriter.AddFile(sftLkst, LakeStageFile);

  LakeRestartFile := ChangeFileExt(AFileName, '.lkrs');
  SutraFileWriter.AddFile(sftLKrs, LakeRestartFile);

  LakeBudgetFile := ChangeFileExt(AFileName, '.lkbu');
  SutraFileWriter.AddFile(sftLkbu, LakeBudgetFile);

  LakeNodeFile := ChangeFileExt(AFileName, '.lkh');
  SutraFileWriter.AddFile(sftLkh, LakeNodeFile);

  LakeHierarchyFile := ChangeFileExt(AFileName, '.lkn');
  SutraFileWriter.AddFile(sftLkn, LakeHierarchyFile);
end;

procedure TSutraLakeWriter.WriteLakeAreaDataSet1a;
var
  NNLK: integer;
begin
  NNLK := FLakeNodes.Count;
  WriteString('LAKE');
  WriteInteger(NNLK);
  if FLakeOptions.SpecifyLakeBottom then
  begin
    WriteString(' ''SPECIFIED''');
  end
  else
  begin
    WriteString(' ''DEFAULT''');
  end;
  NewLine;
end;

procedure TSutraLakeWriter.WriteLakeAreaDataSet1b;
var
  NodeIndex: Integer;
  IL: Integer;
  Lake_Bottom: TDataArray;
  ANode: TLakeNode;
  ELVLB: Double;
begin
  if FLakeOptions.SpecifyLakeBottom then
  begin
    Lake_Bottom := Model.DataArrayManager.GetDataSetByName(KLake_Bottom);
  end
  else
  begin
    Lake_Bottom := nil;
  end;
  for NodeIndex := 0 to FLakeNodes.Count - 1 do
  begin
    ANode := FLakeNodes[NodeIndex];
    IL := ANode.NodeNumber + 1;
    WriteInteger(IL);
    if FLakeOptions.SpecifyLakeBottom then
    begin
      ELVLB := Lake_Bottom.RealData[0, 0, ANode.FCol];
      WriteFloat(ELVLB);
    end;
    NewLine;
  end;
  WriteInteger(0);
  NewLine;
end;

procedure TSutraLakeWriter.WriteLakeInteractionDataSet1;
var
  NBCIF: Integer;
  NBCIS: Integer;
  NBCIP: Integer;
  NBCIU: Integer;
  NBCIPG: Integer;
  NBCIUG: Integer;
  procedure UpdateCount(const Position: Integer; var Count: Integer);
  var
    AStringList: TLakeInteractionStringList;
    Index: Integer;
  begin
    if FBcsFileNames[Position] <> nil then
    begin
      AStringList := FBcsFileNames[Position];
      for Index := 0 to AStringList.Count - 1 do
      begin
        if AStringList[Index]  <> '' then
        begin
          Inc(Count);
        end;
      end;
    end;
  end;
begin
  NBCIF := 1;
  UpdateCount(0, NBCIF);

  NBCIS := 1;
  UpdateCount(1, NBCIS);

  NBCIP := 1;
  UpdateCount(2, NBCIP);

  NBCIU := 1;
  UpdateCount(3, NBCIU);

  NBCIPG := 1;
  UpdateCount(4, NBCIPG);

  NBCIUG := 1;
  UpdateCount(5, NBCIUG);

  WriteInteger(NBCIF);
  WriteInteger(NBCIS);
  WriteInteger(NBCIP);
  WriteInteger(NBCIU);
  WriteInteger(NBCIPG);
  WriteInteger(NBCIUG);
  NewLine;
end;

procedure TSutraLakeWriter.WriteLakeInteractionDataSet2;
var
  ILKF: Integer;
  AStringList: TLakeInteractionStringList;
  LakeInteraction: TLakeBoundaryInteraction;
begin
  WriteString('# INTERACTIONS WITH FLUID SOURCES');
  NewLine;
  WriteString('''DEFAULT''');
  ILKF := Ord(FLakeOptions.FluidSourceSinkLakePresent)-1;
  WriteInteger(ILKF);
  NewLine;
  if FBcsFileNames[0] <> nil then
  begin
    AStringList := FBcsFileNames[0];
    for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
    begin
      if AStringList[Ord(LakeInteraction)] <> '' then
      begin
        WriteString('''');
        WriteString(ExtractFileName(AStringList[Ord(LakeInteraction)]));
        WriteString('''');
        if LakeInteraction = lbiUseDefaults then
        begin
          ILKF := Ord(FLakeOptions.FluidSourceSinkLakePresent)-1;
        end
        else
        begin
          ILKF := Ord(LakeInteraction) -1;
        end;
        WriteInteger(ILKF);
        NewLine;
      end;
    end;
  end;
end;

procedure TSutraLakeWriter.WriteLakeInteractionDataSet3;
var
  AStringList: TLakeInteractionStringList;
  LakeInteraction: TLakeBoundaryInteraction;
  ILKS: Integer;
begin
  WriteString('# INTERACTIONS WITH SOLUTE/ENERGY SOURCES');
  NewLine;
  WriteString('''DEFAULT''');
  ILKS := Ord(FLakeOptions.USourceSinkLakePresent)-1;
  WriteInteger(ILKS);
  NewLine;
  if FBcsFileNames[1] <> nil then
  begin
    AStringList := FBcsFileNames[1];
    for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
    begin
      if AStringList[Ord(LakeInteraction)] <> '' then
      begin
        WriteString('''');
        WriteString(ExtractFileName(AStringList[Ord(LakeInteraction)]));
        WriteString('''');
        if LakeInteraction = lbiUseDefaults then
        begin
          ILKS := Ord(FLakeOptions.FluidSourceSinkLakePresent)-1;
        end
        else
        begin
          ILKS := Ord(LakeInteraction) -1;
        end;
        WriteInteger(ILKS);
        NewLine;
      end;
    end;
  end;
end;

procedure TSutraLakeWriter.WriteLakeInteractionDataSet4;
var
  AStringList: TLakeInteractionStringList;
  LakeInteraction: TLakeBoundaryInteraction;
  ILKP: Integer;
begin
  WriteString('# INTERACTIONS WITH SPECIFIED PRESSURES');
  NewLine;
  WriteString('''DEFAULT''');
  ILKP := Ord(FLakeOptions.SpecifiedPressureLakePresent)-1;
  WriteInteger(ILKP);
  NewLine;
  if FBcsFileNames[2] <> nil then
  begin
    AStringList := FBcsFileNames[2];
    for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
    begin
      if AStringList[Ord(LakeInteraction)] <> '' then
      begin
        WriteString('''');
        WriteString(ExtractFileName(AStringList[Ord(LakeInteraction)]));
        WriteString('''');
        if LakeInteraction = lbiUseDefaults then
        begin
          ILKP := Ord(FLakeOptions.FluidSourceSinkLakePresent)-1;
        end
        else
        begin
          ILKP := Ord(LakeInteraction) -1;
        end;
        WriteInteger(ILKP);
        NewLine;
      end;
    end;
  end;
end;

procedure TSutraLakeWriter.WriteLakeInteractionDataSet5;
var
  AStringList: TLakeInteractionStringList;
  LakeInteraction: TLakeBoundaryInteraction;
  ILKU: Integer;
begin
  WriteString('# INTERACTIONS WITH CONC/TEMP');
  NewLine;
  WriteString('''DEFAULT''');
  ILKU := Ord(FLakeOptions.SpecifiedULakePresent)-1;
  WriteInteger(ILKU);
  NewLine;
  if FBcsFileNames[3] <> nil then
  begin
    AStringList := FBcsFileNames[3];
    for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
    begin
      if AStringList[Ord(LakeInteraction)] <> '' then
      begin
        WriteString('''');
        WriteString(ExtractFileName(AStringList[Ord(LakeInteraction)]));
        WriteString('''');
        if LakeInteraction = lbiUseDefaults then
        begin
          ILKU := Ord(FLakeOptions.FluidSourceSinkLakePresent)-1;
        end
        else
        begin
          ILKU := Ord(LakeInteraction) -1;
        end;
        WriteInteger(ILKU);
        NewLine;
      end;
    end;
  end;
end;

procedure TSutraLakeWriter.WriteLakeInteractionDataSet6A;
var
  AStringList: TLakeInteractionStringList;
  LakeInteraction: TLakeBoundaryInteraction;
  ILKPG: Integer;
  NameIndex: Integer;
  GenLakeInteractionType: TGeneralizedFlowInteractionType;
begin
  WriteString('# INTERACTIONS WITH GENERALIZED FLOW CONDITIONS');
  NewLine;
  WriteString('''DEFAULT''');
  ILKPG := Ord(FLakeOptions.GeneralizedFlowLakePresent)-1;
  WriteInteger(ILKPG);
  case FLakeOptions.GeneralizedFlowInteractionType of
    gfitFluidSource: WriteString(' ''F''');
    gfitSpecifiedPressure: WriteString(' ''P''');
    else Assert(False);
  end;
  NewLine;
  if FBcsFileNames[4] <> nil then
  begin
    AStringList := FBcsFileNames[4];
    NameIndex := 0;
    for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
    begin
      for GenLakeInteractionType := Low(TGeneralizedFlowInteractionType)
        to High(TGeneralizedFlowInteractionType) do
      begin
        if AStringList[NameIndex] <> '' then
        begin
          WriteString('''');
          WriteString(ExtractFileName(AStringList[NameIndex]));
          WriteString('''');
          if LakeInteraction = lbiUseDefaults then
          begin
            ILKPG := Ord(FLakeOptions.FluidSourceSinkLakePresent)-1;
          end
          else
          begin
            ILKPG := Ord(LakeInteraction) -1;
          end;
          WriteInteger(ILKPG);
          case GenLakeInteractionType of
            gfitFluidSource: WriteString(' ''F''');
            gfitSpecifiedPressure: WriteString(' ''P''');
            gfitUseDefaults:
              begin
                case FLakeOptions.GeneralizedFlowInteractionType of
                  gfitFluidSource: WriteString(' ''F''');
                  gfitSpecifiedPressure: WriteString(' ''P''');
                  else Assert(False);
                end;
              end
            else Assert(False);
          end;
          NewLine;
        end;
        Inc(NameIndex);
      end;
    end;
  end;
end;

procedure TSutraLakeWriter.WriteLakeInteractionDataSet6B;
var
  AStringList: TLakeInteractionStringList;
  NameIndex: Integer;
  LakeInteraction: TLakeBoundaryInteraction;
  GenLakeTransInteractionType: TGeneralizedTransportInteractionType;
  ILKUG: Integer;
//  GenLakeInteractionType: TGeneralizedFlowInteractionType;
begin
  WriteString('# INTERACTIONS WITH GENERALIZED TRANSPORT CONDITIONS');
  NewLine;
  WriteString('''DEFAULT''');
  ILKUG := Ord(FLakeOptions.GeneralizedTransportLakePresent)-1;
  WriteInteger(ILKUG);
  case FLakeOptions.GeneralizedTransportInteractionType of
    gtitSoluteSource: WriteString(' ''S''');
    gtitSpecifiedConcentration: WriteString(' ''U''');
    else Assert(False);
  end;
  NewLine;

  if FBcsFileNames[5] <> nil then
  begin
    AStringList := FBcsFileNames[5];
    NameIndex := 0;
    for LakeInteraction := Low(TLakeBoundaryInteraction) to High(TLakeBoundaryInteraction) do
    begin
      for GenLakeTransInteractionType := Low(TGeneralizedTransportInteractionType)
        to High(TGeneralizedTransportInteractionType) do
      begin
        if AStringList[NameIndex] <> '' then
        begin
          WriteString('''');
          WriteString(ExtractFileName(AStringList[NameIndex]));
          WriteString('''');
          if LakeInteraction = lbiUseDefaults then
          begin
            ILKUG := Ord(FLakeOptions.FluidSourceSinkLakePresent)-1;
          end
          else
          begin
            ILKUG := Ord(LakeInteraction) -1;
          end;
//          ILKUG := Ord(LakeInteraction) -1;
          WriteInteger(ILKUG);
          case GenLakeTransInteractionType of
            gtitSoluteSource: WriteString(' ''S''');
            gtitSpecifiedConcentration: WriteString(' ''U''');
            gtitUseDefaults:
              begin
                case FLakeOptions.GeneralizedTransportInteractionType of
                  gtitSoluteSource: WriteString(' ''S''');
                  gtitSpecifiedConcentration: WriteString(' ''U''');
                  else Assert(False);
                end;
              end
            else Assert(False);
          end;
          NewLine;
        end;
        Inc(NameIndex);
      end;
    end;
  end;
end;

procedure TSutraLakeWriter.WriteLakeObsNodes;
begin

end;

end.
