unit ModflowCfpWriterUnit;

interface

uses System.UITypes,
  Winapi.Windows, CustomModflowWriterUnit, ModflowPackageSelectionUnit,
  Generics.Collections, PhastModelUnit, ScreenObjectUnit, DataSetUnit,
  GoPhastTypes, SysUtils, Classes, ModflowBoundaryDisplayUnit, Vcl.Dialogs,
  ModflowCfpFixedUnit;

type
  TCfpPipe = class;

  TCfpNode = class(TObject)
  private
    FCfpBoundaryType: TCfpBoundaryType;
    FNumber: Integer;
    FIsFixed: Boolean;
    FFixedHead: double;
    FPipes: TList<TCfpPipe>;
    FExchange: Double;
    FLayer: integer;
    FRow: Integer;
    FColumn: Integer;
    FElevation: double;
    FRechargeFraction: array of double;
    FRechargeFractionUsed: array of boolean;
    FRechargeFractionAnnotation: array of string;
    // @name is used to indicate that data about this node should be written
    // to an output file. See Conduit Output Control File.
    FRecordData: Boolean;
    FScreenObject: TScreenObject;
    constructor Create;
  public
    destructor Destroy; override;
  end;

  TCfpPipe = class(TObject)
  private
    FNode1: TCfpNode;
    FNode2: TCfpNode;
    FNumber: Integer;
    FDiameter: double;
    FTortuosity: double;
    FRoughnessHeight: Double;
    FLowerR: Double;
    FHigherR: Double;
    // @name is used to indicate that data about this pipe should be written
    // to an output file. See Conduit Output Control File.
    FRecordData: Boolean;
    function SameNodes(Node1, Node2: TCfpNode): boolean;
    function OtherNode(ANode: TCfpNode): TCfpNode;
  end;

  TModflowCfpWriter = class(TCustomPackageWriter)
  private
    FPipes: TObjectList<TCfpPipe>;
    FNodes: TObjectList<TCfpNode>;
    FNodeGrid: array of array of array of TCfpNode;
    FLayerCount: Integer;
    FRowCount: Integer;
    FColumnCount: Integer;
    FConduitFlowProcess: TConduitFlowProcess;
    // After @link(EvaluateConduitRecharge) is called,
    // @name contains a series of @link(TValueCellList)s;
    // one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for one stress period.
    // @name is a TObjectList.
    FValues: TList;
    FCrchUsed: Boolean;
    FUseCOC: Boolean;
    FShouldWriteCRCH: Boolean;
    FShouldWriteCOC: Boolean;
    NameOfFile: string;
    procedure Evaluate;
    procedure EvaluateConduitRecharge;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSets2and3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSet8;
    procedure WriteDataSets9to11;
    procedure WriteDataSet12;
    procedure WriteDataSet13;
    procedure WriteDataSet14;
    procedure WriteDataSet15;
    procedure WriteDataSet16;
    procedure WriteDataSet17;
    procedure WriteDataSet18;
    procedure WriteDataSet19;
    procedure WriteDataSet20;
    procedure WriteDataSet21;
    procedure WriteDataSet22;
    procedure WriteDataSets23to24;
    procedure WriteDataSet25;
    procedure WriteDataSet26;
    procedure WriteDataSet27;
    procedure WriteDataSet28;
    procedure WriteDataSet29;
    procedure WriteDataSets30to31;
    procedure WriteDataSet32;
    procedure WriteDataSet33;
    procedure WriteDataSet34;
    procedure WriteDataSet35;
    procedure WriteDataSet36;
    procedure WriteDataSet37;
    procedure WriteDataSets38to39;
    procedure WriteCrchFile(NameOfFile: string);
    procedure WriteCocFile(NameOfFile: string);
    procedure ClearTimeLists(AModel: TBaseModel);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    Constructor Create(Model: TCustomModel;
      EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses
  ModflowGridUnit, ModflowCfpPipeUnit, Forms, frmProgressUnit,
  frmErrorsAndWarningsUnit, Math,
  LayerStructureUnit, ModflowCfpRechargeUnit, Contnrs, ModflowCellUnit,
  ModflowUnitNumbers, AbstractGridUnit, DataSetNamesUnit, CellLocationUnit;

resourcestring
  StrTooManyConduitsAt = 'Too many conduits at a node. The following cells h' +
  'ave more than the maximum allowed 6 conduits. (Layer, Row, Column)';
  Str0d1d2d = '%0:d, %1:d, %2:d';
  StrTheFollowingObject = 'The following objects define both conduits and fi' +
  'xed heads in conduits. Thus, every node in the conduit will be a fixed he' +
  'ad node. If that is not what you want, use a separate object to define th' +
  'e CFP fixed head nodes.';
  StrConduitRechargeNot = 'Conduit Recharge not defined.';
  StrConduitRechargeWas = 'Conduit Recharge was selected but no conduit rec' +
  'harge boundaries were defined.';
  StrCFPDiameterIsNot = 'CFP Diameter is not assigned in the following object' +
  's.';
  StrCFPTortuosityIsNo = 'CFP Tortuosity is not assigned in the following obj' +
  'ects.';
  StrCFPRoughnessHeight = 'CFP Roughness Height is not assigned in the follow' +
  'ing objects.';
  StrCFPLowerCriticalR = 'CFP Lower Critical Reynolds number is not assigned' +
  ' in the following objects.';
  StrCFPUpperCriticalR = 'CFP Upper Critical Reynolds number is not assigned' +
  ' in the following objects.';
  StrCFPPipeElevationI = 'CFP pipe elevation is not assigned at the following' +
  ' cells.';
  StrCFPNodeElevationHigh = 'CFP node elevation is too high.';
  StrNodeNumber0d = 'Node number: %0:d; (Layer, Row, Column) = (%1:d, %2:d, ' +
  '%3:d)';
  StrCFPNodeElevationLow = 'CFP node elevation is too low.';
  StrTheConduitFlowPro = 'The Conduit Flow Process is not supported by MT3DM' +
  'S.';
  StrMT3DMSVersion53D = 'MT3DMS version 5.3 and MT3D-USGS do not suppport the Conduit Fl' +
  'ow Process.';
  StrCFPSpecifiedHeads = 'CFP Specified heads less than -1 are not allowed' +
  '.';
  StrTheCFPSpecifiedHe = 'The CFP specified head defined by %0:s is %1:g. (Layer, Row, Column) = (%2:d, %3:d, %4:d)';

{ TModflowCfpWriter }

procedure TModflowCfpWriter.ClearTimeLists(AModel: TBaseModel);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TCfpRchFractionBoundary;
//  Boundary: TModflowBoundary;
begin
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowCfpRchFraction;
    if Boundary <> nil then
    begin
      Boundary.ClearTimeLists(AModel);
    end;
  end;
end;

constructor TModflowCfpWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
var
  Grid: TModflowGrid;
begin
  inherited;
  FConduitFlowProcess := Model.ModflowPackages.ConduitFlowProcess;
  FPipes := TObjectList<TCfpPipe>.Create;
  FNodes := TObjectList<TCfpNode>.Create;
  Grid := Model.ModflowGrid;
  FLayerCount := Grid.LayerCount;
  FRowCount := Grid.RowCount;
  FColumnCount := Grid.ColumnCount;
  SetLength(FNodeGrid, FLayerCount, FRowCount, FColumnCount);
  FValues := TObjectList.Create;
end;

destructor TModflowCfpWriter.Destroy;
begin
  FValues.Free;
  FPipes.Free;
  FNodes.Free;
  inherited;
end;

type TRealSparseDataSetCrack = class(TRealSparseDataSet);

procedure TModflowCfpWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  PipeBoundary: TCfpPipeBoundary;
  Diameter: TDataArray;
  PipeDiameter: double;
  Node1: TCfpNode;
  Node2: TCfpNode;
  NodeCreated: Boolean;
  PipeIndex: Integer;
  APipe: TCfpPipe;
  Pipe: TCfpPipe;
  Tortuosity: TDataArray;
  RoughnessHeight: TDataArray;
  LowerCriticalR: TDataArray;
  UpperCriticalR: TDataArray;
  PipeConducOrPerm: TDataArray;
  PipeElevation: TDataArray;
  TortuosityValue: double;
  RoughnessHeightValue: double;
  LowerCriticalRValue: double;
  UpperCriticalRValue: double;
  NodeIndex: Integer;
  ANode: TCfpNode;
  FixedHeadsArray: TDataArray;
  FixedHeads: TCfpFixedBoundary;
  CellList: TCellAssignmentList;
  OtherData: TObject;
  CellIndex: Integer;
  ACell1: TCellAssignment;
  ACell2: TCellAssignment;
  LocalGrid: TCustomModelGrid;
  BoundaryTypeArray: TDataArray;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTooManyConduitsAt);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPDiameterIsNot);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPTortuosityIsNo);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPRoughnessHeight);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPLowerCriticalR);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPUpperCriticalR);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPNodeElevationHigh);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPNodeElevationLow);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPPipeElevationI);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrCFPSpecifiedHeads);

  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingObject);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheConduitFlowPro);

    if Model.ModflowPackages.Mt3dBasic.IsSelected then
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrTheConduitFlowPro,
        StrMT3DMSVersion53D);
    end;
  FUseCOC := False;
  if FConduitFlowProcess.PipesUsed then
  begin
    OtherData := nil;
    CellList := TCellAssignmentList.Create;
    try
      Diameter := Model.DataArrayManager.GetDataSetByName(KPipeDiameter);
      Tortuosity := Model.DataArrayManager.GetDataSetByName(KTortuosity);
      RoughnessHeight := Model.DataArrayManager.GetDataSetByName(KRoughnessHeight);
      LowerCriticalR := Model.DataArrayManager.GetDataSetByName(KLowerCriticalR);
      UpperCriticalR := Model.DataArrayManager.GetDataSetByName(KUpperCriticalR);
      PipeConducOrPerm := Model.DataArrayManager.GetDataSetByName(KPipeConductanceOrPer);
      PipeElevation := Model.DataArrayManager.GetDataSetByName(KCfpNodeElevation);
      Assert(Diameter <> nil);
      Assert(Tortuosity <> nil);
      Assert(RoughnessHeight <> nil);
      Assert(LowerCriticalR <> nil);
      Assert(UpperCriticalR <> nil);
      Assert(PipeConducOrPerm <> nil);
      Assert(PipeElevation <> nil);
      TRealSparseDataSetCrack(PipeConducOrPerm).Clear;
      TRealSparseDataSetCrack(PipeElevation).Clear;
      for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
      begin
        AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
        if AScreenObject.Deleted
          or not (AScreenObject.Count > 1)
          or (AScreenObject.ElevationCount <> ecOne)
          or not AScreenObject.UsedModels.UsesModel(Model) then
        begin
          Continue;
        end;
        PipeBoundary := AScreenObject.ModflowCfpPipes;
        if (PipeBoundary <> nil) and PipeBoundary.Used then
        begin
          FixedHeads := AScreenObject.ModflowCfpFixedHeads;
          if (FixedHeads <> nil) and FixedHeads.Used then
          begin
            frmErrorsAndWarnings.AddWarning(Model,
              StrTheFollowingObject, AScreenObject.Name, AScreenObject);
          end;
          TRealSparseDataSetCrack(Diameter).Clear;
          TRealSparseDataSetCrack(Tortuosity).Clear;
          TRealSparseDataSetCrack(RoughnessHeight).Clear;
          TRealSparseDataSetCrack(LowerCriticalR).Clear;
          TRealSparseDataSetCrack(UpperCriticalR).Clear;
          // don't clear PipeConducOrPerm or PipeElevation because
          // they are a cell properties not a pipe properties.
          AScreenObject.AssignValuesToDataSet(Diameter, Model, lctIgnore);
          AScreenObject.AssignValuesToDataSet(Tortuosity, Model, lctIgnore);
          AScreenObject.AssignValuesToDataSet(RoughnessHeight, Model, lctIgnore);
          AScreenObject.AssignValuesToDataSet(LowerCriticalR, Model, lctIgnore);
          AScreenObject.AssignValuesToDataSet(UpperCriticalR, Model, lctIgnore);
          AScreenObject.AssignValuesToDataSet(PipeConducOrPerm, Model, lctIgnore);
          if FConduitFlowProcess.CfpElevationChoice = cecIndividual then
          begin
            AScreenObject.AssignValuesToDataSet(PipeElevation, Model, lctIgnore);
          end;

          CellList.Clear;
//          Segments := AScreenObject.Segments[Model];
          AScreenObject.GetCellsToAssign({Model.ModflowGrid,} '0', OtherData, Diameter, CellList, alAll, Model);

          if CellList.Count > 1 then
          begin
            for CellIndex := 1 to CellList.Count - 1 do
            begin
              ACell1 := CellList[CellIndex-1];
              ACell2 := CellList[CellIndex];
              if ((ACell1.Column <> ACell2.Column)
                or (ACell1.Row <> ACell2.Row)
                or (ACell1.Layer <> ACell2.Layer))
                and ((Abs(ACell1.Column - ACell2.Column) <= 1)
                and (Abs(ACell1.Row - ACell2.Row) <= 1)
                and (Abs(ACell1.Layer - ACell2.Layer) <= 1))
                then
              begin
                if not Diameter.IsValue[ACell1.Layer, ACell1.Row, ACell1.Column]
                  or not Diameter.IsValue[ACell2.Layer, ACell2.Row, ACell2.Column] then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrCFPDiameterIsNot,
                    AScreenObject.Name, AScreenObject);
                  Break;
                end;
                PipeDiameter := (Diameter.RealData[ACell1.Layer, ACell1.Row, ACell1.Column]
                  + Diameter.RealData[ACell2.Layer, ACell2.Row, ACell2.Column])/2;

                if not Tortuosity.IsValue[ACell1.Layer, ACell1.Row, ACell1.Column]
                  or not Tortuosity.IsValue[ACell2.Layer, ACell2.Row, ACell2.Column] then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrCFPTortuosityIsNo,
                    AScreenObject.Name, AScreenObject);
                  Break;
                end;
                TortuosityValue := (Tortuosity.RealData[ACell1.Layer, ACell1.Row, ACell1.Column]
                  + Tortuosity.RealData[ACell2.Layer, ACell2.Row, ACell2.Column])/2;

                if not RoughnessHeight.IsValue[ACell1.Layer, ACell1.Row, ACell1.Column]
                  or not RoughnessHeight.IsValue[ACell2.Layer, ACell2.Row, ACell2.Column] then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrCFPRoughnessHeight,
                    AScreenObject.Name, AScreenObject);
                  Break;
                end;
                RoughnessHeightValue := (RoughnessHeight.RealData[ACell1.Layer, ACell1.Row, ACell1.Column]
                  + RoughnessHeight.RealData[ACell2.Layer, ACell2.Row, ACell2.Column])/2;

                if not LowerCriticalR.IsValue[ACell1.Layer, ACell1.Row, ACell1.Column]
                  or not LowerCriticalR.IsValue[ACell2.Layer, ACell2.Row, ACell2.Column] then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrCFPLowerCriticalR,
                    AScreenObject.Name, AScreenObject);
                  Break;
                end;
                LowerCriticalRValue := (LowerCriticalR.RealData[ACell1.Layer, ACell1.Row, ACell1.Column]
                  + LowerCriticalR.RealData[ACell2.Layer, ACell2.Row, ACell2.Column])/2;

                if not UpperCriticalR.IsValue[ACell1.Layer, ACell1.Row, ACell1.Column]
                  or not UpperCriticalR.IsValue[ACell2.Layer, ACell2.Row, ACell2.Column] then
                begin
                  frmErrorsAndWarnings.AddError(Model, StrCFPUpperCriticalR,
                    AScreenObject.Name, AScreenObject);
                  Break;
                end;
                UpperCriticalRValue := (UpperCriticalR.RealData[ACell1.Layer, ACell1.Row, ACell1.Column]
                  + UpperCriticalR.RealData[ACell2.Layer, ACell2.Row, ACell2.Column])/2;

                Node1 := FNodeGrid[ACell1.Layer, ACell1.Row, ACell1.Column];
                Node2 := FNodeGrid[ACell2.Layer, ACell2.Row, ACell2.Column];
                NodeCreated := (Node1 = nil) or (Node2 = nil);
                if Node1 = nil then
                begin
                  Node1 := TCfpNode.Create;
                  FNodes.Add(Node1);
                  Node1.FNumber := FNodes.Count;
                  FNodeGrid[ACell1.Layer, ACell1.Row, ACell1.Column] := Node1;
                  Node1.FLayer := ACell1.Layer;
                  Node1.FRow := ACell1.Row;
                  Node1.FColumn := ACell1.Column;
                  Node1.FScreenObject := AScreenObject;
                end;
                Node1.FRecordData :=
                  PipeBoundary.RecordNodeValues or Node1.FRecordData;
                if Node2 = nil then
                begin
                  Node2 := TCfpNode.Create;
                  FNodes.Add(Node2);
                  Node2.FNumber := FNodes.Count;
                  FNodeGrid[ACell2.Layer, ACell2.Row, ACell2.Column] := Node2;
                  Node2.FLayer := ACell2.Layer;
                  Node2.FRow := ACell2.Row;
                  Node2.FColumn := ACell2.Column;
                  Node2.FScreenObject := AScreenObject;
                end;
                Node2.FRecordData :=
                  PipeBoundary.RecordNodeValues or Node2.FRecordData;
                Pipe := nil;
                if not NodeCreated then
                begin
                  for PipeIndex := 0 to Node1.FPipes.Count - 1 do
                  begin
                    APipe := Node1.FPipes[PipeIndex];
                    if APipe.SameNodes(Node1, Node2) then
                    begin
                      Pipe := APipe;
                      break;
                    end;
                  end;
                end;
                if Pipe = nil then
                begin
                  Pipe := TCfpPipe.Create;
                  FPipes.Add(Pipe);
                  Pipe.FNumber := FPipes.Count;
                  Pipe.FNode1 := Node1;
                  Pipe.FNode2 := Node2;
                  Node1.FPipes.Add(Pipe);
                  Node2.FPipes.Add(Pipe);
                end;
                Pipe.FDiameter := PipeDiameter;
                Pipe.FTortuosity := TortuosityValue;
                Pipe.FRoughnessHeight := RoughnessHeightValue;
                Pipe.FLowerR := LowerCriticalRValue;
                Pipe.FHigherR := UpperCriticalRValue;
                Pipe.FRecordData := PipeBoundary.RecordPipeValues;
              end;
            end;
          end;
        end;
      end;

      LocalGrid := Model.Grid;
      FixedHeadsArray := Model.DataArrayManager.GetDataSetByName(KCfpFixedHeads);
      Assert(FixedHeadsArray <> nil);
      FixedHeadsArray.Initialize;
      BoundaryTypeArray := nil;
      if Model.ModelSelection  = msModflowOwhm2 then
      begin
        BoundaryTypeArray := Model.DataArrayManager.GetDataSetByName(KCfpBoundaryType);
        Assert(BoundaryTypeArray <> nil);
        BoundaryTypeArray.Initialize;
      end;
      for NodeIndex := 0 to FNodes.Count - 1 do
      begin
        ANode := FNodes[NodeIndex];
        Assert(PipeConducOrPerm.IsValue[
          ANode.FLayer, ANode.FRow, ANode.FColumn]);
        ANode.FExchange := PipeConducOrPerm.RealData[
          ANode.FLayer, ANode.FRow, ANode.FColumn];

        if FConduitFlowProcess.CfpElevationChoice = cecIndividual then
        begin
          if not PipeElevation.IsValue[ANode.FLayer, ANode.FRow, ANode.FColumn] then
          begin
            frmErrorsAndWarnings.AddError(Model, StrCFPPipeElevationI,
              Format(StrLayerRowCol,
              [ANode.FLayer+1, ANode.FRow+1, ANode.FColumn+1]));
            Break;
          end;
          ANode.FElevation := PipeElevation.RealData[
            ANode.FLayer, ANode.FRow, ANode.FColumn];
          if ANode.FElevation >
            LocalGrid.CellElevation[ANode.FColumn, ANode.FRow, ANode.FLayer] then
          begin
            frmErrorsAndWarnings.AddError(Model, StrCFPNodeElevationHigh,
              Format(StrNodeNumber0d,
              [ANode.FNumber, ANode.FLayer+1, ANode.FRow+1, ANode.FColumn+1]));
          end;
          if ANode.FElevation <
            LocalGrid.CellElevation[ANode.FColumn, ANode.FRow, ANode.FLayer+1] then
          begin
            frmErrorsAndWarnings.AddError(Model, StrCFPNodeElevationLow,
              Format(StrNodeNumber0d,
              [ANode.FNumber, ANode.FLayer+1, ANode.FRow+1, ANode.FColumn+1]));
          end;
        end;

        ANode.FIsFixed := FixedHeadsArray.IsValue[
          ANode.FLayer, ANode.FRow, ANode.FColumn];
        if ANode.FIsFixed then
        begin
          ANode.FFixedHead := FixedHeadsArray.RealData[
            ANode.FLayer, ANode.FRow, ANode.FColumn];
          if BoundaryTypeArray <> nil then
          begin
            ANode.FCfpBoundaryType :=
              TCfpBoundaryType(BoundaryTypeArray.IntegerData[
              ANode.FLayer, ANode.FRow, ANode.FColumn]);
          end
          else
          begin
            ANode.FCfpBoundaryType := cbtFixedHead;
          end;
        end;
      end;
    finally
      CellList.Free;
    end;
  end;
  FCrchUsed := FConduitFlowProcess.PipesUsed
    and FConduitFlowProcess.ConduitRechargeUsed
    and Model.ModflowPackages.RchPackage.IsSelected;
  if FCrchUsed then
  begin
    EvaluateConduitRecharge
  end;
end;

procedure TModflowCfpWriter.EvaluateConduitRecharge;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  NoAssignmentErrorRoot: string;
  NoDefinedErrorRoot: string;
  Boundary: TCfpRchFractionBoundary;
  NodeIndex: Integer;
  ANode: TCfpNode;
  StressPeriodCount: Integer;
  StressPeriodIndex: Integer;
  CellList: TValueCellList;
  CellIndex: Integer;
  ACell: TCfpRchFraction_Cell;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    ClearTimeLists(Model);

    StressPeriodCount := Model.ModflowFullStressPeriods.Count;
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      ANode := FNodes[NodeIndex];
      SetLength(ANode.FRechargeFractionUsed, StressPeriodCount);
      SetLength(ANode.FRechargeFraction, StressPeriodCount);
      SetLength(ANode.FRechargeFractionAnnotation, StressPeriodCount);
      for StressPeriodIndex := 0 to StressPeriodCount - 1 do
      begin
        ANode.FRechargeFractionUsed[StressPeriodIndex] := False;
      end;
    end;

    RemoveNoDefinedError(NoDefinedErrorRoot);
    NoAssignmentErrorRoot := Format(StrNoBoundaryConditio, [Package.PackageIdentifier]);
    frmProgressMM.AddMessage(Format(StrEvaluatingSData, [Package.PackageIdentifier]));
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowCfpRchFraction;
      if Boundary <> nil then
      begin
        if not ScreenObject.SetValuesOfEnclosedCells
          and not ScreenObject.SetValuesOfIntersectedCells then
        begin
          frmErrorsAndWarnings.AddError(Model, NoAssignmentErrorRoot,
            ScreenObject.Name, ScreenObject);
        end;
        frmProgressMM.AddMessage(Format(StrEvaluatingS,
          [ScreenObject.Name]));
        Boundary.GetCellValues(FValues, nil, Model, self);
      end;
    end;
    if (FValues.Count = 0) then
    begin
      frmErrorsAndWarnings.AddError(Model, StrConduitRechargeNot,
        StrConduitRechargeWas);
    end;
    for StressPeriodIndex := 0 to FValues.Count - 1 do
    begin
      CellList := FValues[StressPeriodIndex];
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        ACell := CellList[CellIndex] as TCfpRchFraction_Cell;
        ANode := FNodeGrid[ACell.Layer, ACell.Row, ACell.Column];
        if ANode <> nil then
        begin
          ANode.FRechargeFractionUsed[StressPeriodIndex] := True;
          ANode.FRechargeFraction[StressPeriodIndex] := ACell.CfpRechargeFraction;
          ANode.FRechargeFractionAnnotation[StressPeriodIndex] := ACell.CfpRechargeFractionAnnotation;
        end;
      end;
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowCfpWriter.Extension: string;
begin
  result := '.cfp';
end;

function TModflowCfpWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.ConduitFlowProcess;
end;

procedure TModflowCfpWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  RechFractionsTimes: TModflowBoundaryDisplayTimeList;
  TimeIndex: Integer;
  StressPeriodCount: integer;
  RechFractionsArray: TModflowBoundaryDisplayDataArray;
  NodeIndex: Integer;
  ANode: TCfpNode;
begin
  if not (Model as TPhastModel).CfpRechargeIsSelected(nil) then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  try
    Evaluate;

    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    RechFractionsTimes := TimeLists[0];

    StressPeriodCount := Model.ModflowFullStressPeriods.Count;
    for TimeIndex := 0 to StressPeriodCount - 1 do
    begin
      RechFractionsArray := RechFractionsTimes[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      for NodeIndex := 0 to FNodes.Count - 1 do
      begin
        ANode := FNodes[NodeIndex];
        if ANode.FRechargeFractionUsed[TimeIndex] then
        begin
          RechFractionsArray.RealData[ANode.FLayer, ANode.FRow, ANode.FColumn] :=
            ANode.FRechargeFraction[TimeIndex];
          RechFractionsArray.Annotation[ANode.FLayer, ANode.FRow, ANode.FColumn] :=
            ANode.FRechargeFractionAnnotation[TimeIndex];
        end;
      end;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

end;

procedure TModflowCfpWriter.WriteDataSet0;
begin
  // CFP requires that 1 and only 1 line be present in data set 0.
  WriteCommentLine(PackageID_Comment(Package));
end;

procedure TModflowCfpWriter.WriteDataSet1;
var
  Mode: Integer;
  OutDir: string;
  OutputFileName: string;
begin
  Mode := 0;
  if FConduitFlowProcess.PipesUsed then
  begin
    Mode := Mode + 1;
  end;
  if FConduitFlowProcess.ConduitLayersUsed then
  begin
    Mode := Mode + 2;

    OutDir := ExtractFileDir(NameOfFile);
    OutDir := IncludeTrailingPathDelimiter(OutDir);
    OutputFileName := OutDir + 'turblam.txt';
    Model.AddModelOutputFile(OutputFileName);
  end;
  WriteInteger(Mode);
  WriteString(' # Data Set 1: MODE');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet12;
var
  NNODES: Integer;
  ELEVATION: Double;
  NodeIndex: Integer;
  ANode: TCfpNode;
  NO_N: Integer;
begin
  case FConduitFlowProcess.CfpElevationChoice of
    cecIndividual:
      begin
        for NodeIndex := 0 to FNodes.Count - 1 do
        begin
          ANode := FNodes[NodeIndex];
          NO_N := ANode.FNumber;
          ELEVATION := ANode.FElevation;
          WriteInteger(NO_N);
          WriteFloat(ELEVATION);
          NewLine;
        end;
      end;
    cecGroup:
      begin
        NNODES := FNodes.Count;
        ELEVATION := FConduitFlowProcess.ElevationOffset;
        WriteInteger(NNODES);
        WriteFloat(ELEVATION);
        NewLine;
      end;
    else
      Assert(False);
  end;
end;

procedure TModflowCfpWriter.WriteDataSet13;
begin
  case FConduitFlowProcess.CfpExchange of
    ceNodeConductance:
      begin
        WriteString('# SA_EXCHANGE = 0; pipe conductance for each node');
      end;
    ceWallPermeability:
      begin
        WriteString('# SA_EXCHANGE = 1; conduit wall permeability for each node');
      end
    else
      Assert(False);
  end;
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet14;
var
  SA_EXCHANGE: Integer;
begin
  SA_EXCHANGE := Ord(FConduitFlowProcess.CfpExchange);
  WriteInteger(SA_EXCHANGE);
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet15;
begin
  WriteString('# criterion for convergence (EPSILON)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet16;
var
  Epsilon: Double;
begin
  Epsilon := FConduitFlowProcess.Epsilon;
  WriteFloat(Epsilon);
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet17;
begin
  WriteString('# maximum number for loop iterations (NITER)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet18;
var
  NITER: Integer;
begin
  NITER := FConduitFlowProcess.MaxIterations;
  WriteInteger(NITER);
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet19;
begin
  WriteString('# Relaxation parameter (RELAX)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet20;
var
  RELAX: Double;
begin
  RELAX := FConduitFlowProcess.Relax;
  WriteFloat(RELAX);
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet21;
begin
  case FConduitFlowProcess.CfpPrintIterations of
    cpiNoPrint:
      begin
        WriteString('# Print flag (P_NR); 0 = iteration results not printed');
      end;
    cpiPrint:
      begin
        WriteString('# Print flag (P_NR); 1 = iteration results printed');
      end;
    else
      Assert(False);
  end;
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet22;
var
  P_NR: Integer;
begin
  P_NR := Ord(FConduitFlowProcess.CfpPrintIterations);
  WriteInteger(P_NR);
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet25;
var
  PipeIndex: Integer;
  APipe: TCfpPipe;
  NO_P: Integer;
  DIAMETER: Double;
  TORTUOSITY: Double;
  RHEIGHT: Double;
  LCRITREY_P: Double;
  TCRITREY_P: Double;
begin
  for PipeIndex := 0 to FPipes.Count - 1 do
  begin
    APipe := FPipes[PipeIndex];
    NO_P := APipe.FNumber;
    DIAMETER := APipe.FDiameter;
    TORTUOSITY := APipe.FTortuosity;
    RHEIGHT := APipe.FRoughnessHeight;
    LCRITREY_P := APipe.FLowerR;
    TCRITREY_P := APipe.FHigherR;
    WriteInteger(NO_P);
    WriteFloat(DIAMETER);
    WriteFloat(TORTUOSITY);
    WriteFloat(RHEIGHT);
    WriteFloat(LCRITREY_P);
    WriteFloat(TCRITREY_P);
    NewLine;
  end;
end;

procedure TModflowCfpWriter.WriteDataSet26;
begin
  WriteString('# NodeNumber (NO_N), N_HEAD = -1 = not fixed; N_HEAD has positive value = piezometric heads for nodes with fixed head (Data Set 27)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet27;
var
  NodeIndex: Integer;
  ANode: TCfpNode;
  NO_N: Integer;
  N_HEAD: Double;
begin
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    ANode := FNodes[NodeIndex];
    NO_N := ANode.FNumber;
    WriteInteger(NO_N);
    if ANode.FIsFixed then
    begin
      N_HEAD := ANode.FFixedHead;
      if N_HEAD <= -1 then
      begin
        frmErrorsAndWarnings.AddError(Model, StrCFPSpecifiedHeads,
          Format(StrTheCFPSpecifiedHe, [ANode.FScreenObject.Name, N_HEAD,
            ANode.FLayer+1, ANode.FRow+1, ANode.FColumn+1]), ANode.FScreenObject);
      end;
      WriteFloat(N_HEAD);
    end
    else
    begin
      WriteInteger(-1);
    end;
    NewLine;
  end;
end;

procedure TModflowCfpWriter.WriteDataSet28;
begin
  case FConduitFlowProcess.CfpExchange of
    ceNodeConductance:
      begin
        WriteString('#Node number (NO_N), Conduit conductance (K_EXCHANGE)');
      end;
    ceWallPermeability:
      begin
        WriteString('#Node number (NO_N), Conduit wall permeability (K_EXCHANGE)');
      end;
    else
      Assert(False);
  end;
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet29;
var
  NodeIndex: integer;
  ANode: TCfpNode;
  NO_N: Integer;
  K_EXCHANGE: Double;
begin
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    ANode := FNodes[NodeIndex];
    NO_N := ANode.FNumber;
    K_EXCHANGE := ANode.FExchange;
    WriteInteger(NO_N);
    WriteFloat(K_EXCHANGE);
    NewLine;
  end;
end;

procedure TModflowCfpWriter.WriteDataSet32;
var
  LayerGroupIndex: Integer;
  ALayerGroup: TLayerGroup;
  NCL: Integer;
  LayerIndex: Integer;
  AConduitLayer: TConduitLayerItem;
begin
  NCL := 0;
  // Start at 1 because first layer group defines the top of the model.
  for LayerGroupIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    ALayerGroup := Model.LayerStructure[LayerGroupIndex];
    if ALayerGroup.RunTimeSimulated then
    begin
      for LayerIndex := 0 to Min(ALayerGroup.LayerCount, ALayerGroup.ConduitLayers.Count) - 1 do
      begin
        AConduitLayer := ALayerGroup.ConduitLayers[LayerIndex];
        if AConduitLayer.IsConduitLayer then
        begin
          Inc(NCL);
        end;
      end;
    end;
  end;
  WriteInteger(NCL);
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet33;
begin
  WriteString('# Conduit layer numbers (CL)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet34;
var
  CL: Integer;
  LayerGroupIndex: Integer;
  ALayerGroup: TLayerGroup;
  LayerIndex: Integer;
  AConduitLayer: TConduitLayerItem;
begin
  CL := 0;
  for LayerGroupIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    ALayerGroup := Model.LayerStructure[LayerGroupIndex];
    if ALayerGroup.RunTimeSimulated then
    begin
      for LayerIndex := 0 to Max(ALayerGroup.LayerCount, ALayerGroup.ConduitLayers.Count) - 1 do
      begin
        Inc(CL);
        if LayerIndex < ALayerGroup.ConduitLayers.Count then
        begin
          AConduitLayer := ALayerGroup.ConduitLayers[LayerIndex];
          if AConduitLayer.IsConduitLayer then
          begin
            WriteInteger(CL);
          end;
        end;
      end;
    end;
  end;
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet35;
begin
{$IFDEF OWHMV2}
  if Model.ModelSelection = msModflowOwhm2 then
  begin
    WriteString('# undocumented flag: IRADFLAG');
    NewLine;
    WriteInteger(0);
    NewLine;
  end
  ELSE
  BEGIN
    WriteString('# Water temperature, in degrees Celsius (LTEMP)');
    NewLine;
  end;
{$ELSE}
  WriteString('# Water temperature, in degrees Celsius (LTEMP)');
  NewLine;
{$ENDIF}
end;

procedure TModflowCfpWriter.WriteDataSet36;
var
  LTEMP: Double;
begin
{$IFDEF OWHMV2}
  if Model.ModelSelection <> msModflowOwhm2 then
  begin
    LTEMP := FConduitFlowProcess.LayerTemperature;
    WriteFloat(LTEMP);
    NewLine;
  end;
{$else}
  LTEMP := FConduitFlowProcess.LayerTemperature;
  WriteFloat(LTEMP);
  NewLine;
{$ENDIF}
end;

procedure TModflowCfpWriter.WriteDataSet37;
begin
{$IFDEF OWHMV2}
  if Model.ModelSelection <> msModflowOwhm2 then
  begin
    WriteString('# mean void diameter (VOID), '
      + 'lower critical Reynolds number (turbulent to laminar) (LCRITREY_L), '
      + 'Upper critical Reynolds number (laminar to turbulent) (TCRITREY_L)');
    NewLine;
  end;
{$ELSE}
    WriteString('# mean void diameter (VOID), '
      + 'lower critical Reynolds number (turbulent to laminar) (LCRITREY_L), '
      + 'Upper critical Reynolds number (laminar to turbulent) (TCRITREY_L)');
    NewLine;
{$ENDIF}
end;

procedure TModflowCfpWriter.WriteDataSet4;
var
  NNODES: Integer;
  NPIPES: Integer;
  NLAYERS: Integer;
begin
  NNODES := FNodes.Count;
  NPIPES := FPipes.Count;
  NLAYERS := Model.ModflowLayerCount;
  WriteInteger(NNODES);
  WriteInteger(NPIPES);
  WriteInteger(NLAYERS);
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet5;
begin
  WriteString('# temperature or water in conduits (TEMPERATURE)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet6;
var
  TEMPERATURE: Double;
begin
  TEMPERATURE := FConduitFlowProcess.ConduitTemperature;
  WriteFloat(TEMPERATURE);
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet7;
begin
  WriteString('# Node number (NO_N), '
    + 'Column, Row, and Layer numbers (MC, MR, ML), '
    + 'Neighbor nodes (NB1, NB2, NB3, NB4, NB5, NB6), '
    + 'Pipe numbers (PB1, PB2, PB3, PB4, PB5, PB6)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSet8;
var
  NodeIndex: Integer;
  ANode: TCfpNode;
  PipeIndex: Integer;
  APipe: TCfpPipe;
  OtherNode: TCfpNode;
begin
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    ANode := FNodes[NodeIndex];
    WriteInteger(ANode.FNumber);
    WriteInteger(ANode.FColumn+1);
    WriteInteger(ANode.FRow+1);
    WriteInteger(Model.DataSetLayerToModflowLayer(ANode.FLayer));
    if ANode.FPipes.Count > 6 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrTooManyConduitsAt,
        Format(Str0d1d2d, [ANode.FLayer+1, ANode.FRow+1, ANode.FColumn+1]));
    end;
    for PipeIndex := 0 to Min(6, ANode.FPipes.Count) - 1 do
    begin
      APipe := ANode.FPipes[PipeIndex];
      OtherNode := APipe.OtherNode(ANode);
      WriteInteger(OtherNode.FNumber);
    end;
    for PipeIndex := ANode.FPipes.Count+1 to 6 do
    begin
      WriteInteger(0);
    end;
    for PipeIndex := 0 to Min(6, ANode.FPipes.Count) - 1 do
    begin
      APipe := ANode.FPipes[PipeIndex];
      WriteInteger(APipe.FNumber);
    end;
    for PipeIndex := ANode.FPipes.Count+1 to 6 do
    begin
      WriteInteger(0);
    end;
    NewLine;
  end;
end;

procedure TModflowCfpWriter.WriteDataSets23to24;
begin
  WriteString('# Data for conduit parameters');
  NewLine;
  WriteString('# Pipe number (NO_P), diameter (DIAMETER), '
    + 'tortuosity (TORTUOSITY), roughness height (RHEIGHT), '
    + 'lower critical Reynolds number (turbulent to laminar) (LCRITREY_P), '
    + 'upper critical Reynolds number (laminar to turbulent) (TCRITREY_P)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSets2and3;
begin
  WriteString('# data for mode 1 (or 3) conduit pipe system');
  NewLine;
  WriteString('# number of nodes (NNODES), number of conduits (NPIPES), number of layers (NLAYERS)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSets30to31;
begin
  WriteString('# Data for mode 2 (or 3) conduit layer system');
  NewLine;
  WriteString('# Number of conduit layers (NCL)');
  NewLine;
end;

procedure TModflowCfpWriter.WriteDataSets38to39;
var
  ModelLayer: Integer;
  CL: Integer;
  LayerGroupIndex: Integer;
  ALayerGroup: TLayerGroup;
  LayerIndex: Integer;
  AConduitLayer: TConduitLayerItem;
  VOID: Double;
  LCRITREY_L: Double;
  TCRITREY_L: Double;
  LayerID: string;
  LTEMP: Double;
begin
{$IFDEF OWHMV2}
  if Model.ModelSelection = msModflowOwhm2 then
  begin
    WriteString('TURBULENT FLOW PARAMETER ARRAYS (undocumented)');
    NewLine;
  end;
{$ENDIF}
  ModelLayer := 0;
  CL := 0;
  LTEMP := FConduitFlowProcess.LayerTemperature;
  for LayerGroupIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    ALayerGroup := Model.LayerStructure[LayerGroupIndex];
    if ALayerGroup.RunTimeSimulated then
    begin
      for LayerIndex := 0 to Max(ALayerGroup.LayerCount, ALayerGroup.ConduitLayers.Count) - 1 do
      begin
        Inc(ModelLayer);
        if LayerIndex < ALayerGroup.ConduitLayers.Count then
        begin
          AConduitLayer := ALayerGroup.ConduitLayers[LayerIndex];
          if AConduitLayer.IsConduitLayer then
          begin
            VOID := AConduitLayer.Void;
            LCRITREY_L := AConduitLayer.LowerCriticalReynoldsNumber;
            TCRITREY_L := AConduitLayer.HigherCriticalReynoldsNumber;

          {$IFDEF OWHMV2}
            if Model.ModelSelection = msModflowOwhm2 then
            begin
              Inc(CL);
              LayerID := Format('# conduit layer %0:d, (Model layer %1:d) ', [CL, ModelLayer]);
              // Data Set 38
              WriteString(LayerID + 'TWATER2');
              NewLine;
              // Data Set 39
              WriteString('CONSTANT');
              WriteFloat(LTEMP);
              NewLine;

              // Data Set 38
              WriteString(LayerID + 'VOID2');
              NewLine;
              // Data Set 39
              WriteString('CONSTANT');
              WriteFloat(VOID);
              NewLine;

              // Data Set 38
              WriteString(LayerID + 'LCRITREY2');
              NewLine;
              // Data Set 39
              WriteString('CONSTANT');
              WriteFloat(LCRITREY_L);
              NewLine;

              // Data Set 38
              WriteString(LayerID + 'TCRITREY2');
              NewLine;
              // Data Set 39
              WriteString('CONSTANT');
              WriteFloat(TCRITREY_L);
              NewLine;

              // Data Set 38
              WriteString(LayerID + 'FEEXP');
              NewLine;
              // Data Set 39
              WriteString('CONSTANT');
              WriteFloat(1);
              NewLine;

            end
            else
            begin
              Inc(CL);
              WriteString(Format('# conduit layer %0:d, (Model layer %1:d)', [CL, ModelLayer]));
              NewLine;
              // Data Set 39
//              VOID := AConduitLayer.Void;
//              LCRITREY_L := AConduitLayer.LowerCriticalReynoldsNumber;
//              TCRITREY_L := AConduitLayer.HigherCriticalReynoldsNumber;
              WriteFloat(VOID);
              WriteFloat(LCRITREY_L);
              WriteFloat(TCRITREY_L);
              NewLine;
            end;
          {$else}
            // Data Set 38
            Inc(CL);
            WriteString(Format('# conduit layer %0:d, (Model layer %1:d)', [CL, ModelLayer]));
            NewLine;
            // Data Set 39
//            VOID := AConduitLayer.Void;
//            LCRITREY_L := AConduitLayer.LowerCriticalReynoldsNumber;
//            TCRITREY_L := AConduitLayer.HigherCriticalReynoldsNumber;
            WriteFloat(VOID);
            WriteFloat(LCRITREY_L);
            WriteFloat(TCRITREY_L);
            NewLine;
          {$ENDIF}

          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowCfpWriter.WriteDataSets9to11;
begin
  WriteString('# Node elevations (GEOHEIGHT)');
  NewLine;
  case FConduitFlowProcess.CfpElevationChoice of
    cecIndividual:
      begin
        WriteString('# Option 1 selected');
        NewLine;
        WriteString('# Node number (NO_N), Elevation with respect to datum (ELEVATION)');
        NewLine;
      end;
    cecGroup:
      begin
        WriteString('# Option 2 selected');
        NewLine;
        WriteString('# Number of nodes (NNODES), Elevation offset from cell center (ELEVATION)');
        NewLine;
      end;
    else
      Assert(False);
  end;

end;

procedure TModflowCfpWriter.WriteFile(const AFileName: string);
var
  ShouldWriteFile: boolean;
begin
  if not Package.IsSelected or not (Model.ModelSelection in [msModflowCfp
    {$IFDEF OWHMV2}
    , msModflowOwhm2
    {$ENDIF}
    ]) then
  begin
    Exit
  end;
  ShouldWriteFile := not Model.PackageGeneratedExternally(StrCFP);
  FShouldWriteCRCH := not Model.PackageGeneratedExternally(StrCRCH);
  FShouldWriteCOC := not Model.PackageGeneratedExternally(StrCOC);

  if not ShouldWriteFile and not FShouldWriteCRCH and not FShouldWriteCOC then
  begin
    Exit;
  end;

  if not FConduitFlowProcess.PipesUsed and not ShouldWriteFile then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);
  if ShouldWriteFile and not WritingTemplate then
  begin
    WriteToNameFile(StrCFP, Model.UnitNumbers.UnitNumber(StrCFP),
      NameOfFile, foInput, Model);
  end;

//  if ShouldWriteFile then
//  begin
    Evaluate;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
//  end;

  if ShouldWriteFile then
  begin
    FInputFileName := NameOfFile;
    OpenFile(NameOfFile);
    try
      WriteDataSet0;
      WriteDataSet1;
      if FConduitFlowProcess.PipesUsed then
      begin
        WriteDataSets2and3;
        WriteDataSet4;
        WriteDataSet5;
        WriteDataSet6;
        WriteDataSet7;
        WriteDataSet8;
        WriteDataSets9to11;
        WriteDataSet12;
        WriteDataSet13;
        WriteDataSet14;
        WriteDataSet15;
        WriteDataSet16;
        WriteDataSet17;
        WriteDataSet18;
        WriteDataSet19;
        WriteDataSet20;
        WriteDataSet21;
        WriteDataSet22;
        WriteDataSets23to24;
        WriteDataSet25;
        WriteDataSet26;
        WriteDataSet27;
        WriteDataSet28;
        WriteDataSet29;
      end;
      if FConduitFlowProcess.ConduitLayersUsed then
      begin
        WriteDataSets30to31;
        WriteDataSet32;
        WriteDataSet33;
        WriteDataSet34;
        WriteDataSet35;
        WriteDataSet36;
        WriteDataSet37;
        WriteDataSets38to39;
      end;
    finally
      CloseFile;
    end;
  end;
  WriteCrchFile(NameOfFile);
  WriteCocFile(NameOfFile);
end;

procedure TModflowCfpWriter.WriteCocFile(NameOfFile: string);
var
  NodeIndex: Integer;
  CocUsed: Boolean;
  PipeIndex: Integer;
  NNODES: Integer;
  NPIPES: Integer;
  ANode: TCfpNode;
  APipe: TCfpPipe;
  OutputFileName: string;
  OutDir: string;
begin
  CocUsed := FConduitFlowProcess.PipesUsed
    and (FConduitFlowProcess.OutputInterval > 0)
    and FShouldWriteCOC;
  if CocUsed then
  begin
    NNODES := 0;
    for NodeIndex := 0 to FNodes.Count - 1 do
    begin
      if FNodes[NodeIndex].FRecordData then
      begin
        Inc(NNODES);
      end;
    end;
    NPIPES := 0;
    for PipeIndex := 0 to FPipes.Count - 1 do
    begin
      if FPipes[PipeIndex].FRecordData then
      begin
        Inc(NPIPES);
      end;
    end;
    if (NNODES > 0) or (NPIPES > 0) then
    begin
      NameOfFile := ChangeFileExt(NameOfFile, '.coc');
      OutDir := ExtractFileDir(NameOfFile);
      OutDir := IncludeTrailingPathDelimiter(OutDir);

      if not WritingTemplate then
      begin
        WriteToNameFile(StrCOC, Model.UnitNumbers.UnitNumber(StrCOC),
                NameOfFile, foInput, Model);
      end;

      OpenFile(NameOfFile);
      try
        // Data Set 0
        WriteCommentLine(File_Comment('COC'));
        // Data Set 1
        WriteCommentLine('Number of nodes for output (NNODES)');
        // Data set 2
        WriteInteger(NNODES);
        NewLine;
        // Data set 3
        WriteCommentLine('Node numbers, one per line (NODE_NUMBERS)');

        // Data set 4
        for NodeIndex := 0 to FNodes.Count - 1 do
        begin
          ANode := FNodes[NodeIndex];
          if ANode.FRecordData then
          begin
            WriteInteger(ANode.FNumber);
            NewLine;
            OutputFileName := Format('%sNODE%.4d.OUT', [OutDir, ANode.FNumber]);
            Model.AddModelOutputFile(OutputFileName);
          end;
        end;

        // Data set 5
        WriteCommentLine('Node output each n time steps (N_NTS)');
        // Data set 6
        WriteInteger(FConduitFlowProcess.OutputInterval);
        NewLine;


        // Data Set 7
        WriteCommentLine('Number of conduits for output (NPIPES)');
        // Data Set 8
        WriteInteger(NPIPES);
        NewLine;
        // Data set 9
        WriteCommentLine('Conduit numbers, one per line (PIPE_NUMBERS)');

        // Data set 10
        for PipeIndex := 0 to FPipes.Count - 1 do
        begin
          APipe := FPipes[PipeIndex];
          if APipe.FRecordData then
          begin
            WriteInteger(APipe.FNumber);
            NewLine;
            OutputFileName := Format('%sTUBE%.4d.OUT', [OutDir, APipe.FNumber]);
            Model.AddModelOutputFile(OutputFileName);
          end;
        end;

        // Data set 11
        WriteCommentLine('Conduit output each n time steps (T_NTS)');
        // Data set 12
        WriteInteger(FConduitFlowProcess.OutputInterval);
        NewLine;

      finally
        CloseFile;
      end;
    end;
  end;
end;

procedure TModflowCfpWriter.WriteCrchFile(NameOfFile: string);
var
  ANode: TCfpNode;
  NodeIndex: Integer;
  IFLAG_CRCH: Integer;
  StressPeriodIndex: Integer;
  StressPeriodCount: Integer;
begin
  if FCrchUsed and FShouldWriteCRCH then
  begin
    NameOfFile := ChangeFileExt(NameOfFile, '.crch');

    if not WritingTemplate then
    begin
      WriteToNameFile(StrCRCH, Model.UnitNumbers.UnitNumber(StrCRCH),
            NameOfFile, foInput, Model);
    end;

    OpenFile(NameOfFile);
    try
      StressPeriodCount := Model.ModflowFullStressPeriods.Count;
      for StressPeriodIndex := 0 to StressPeriodCount - 1 do
      begin
        IFLAG_CRCH := FNodes.Count;
        // CRCH data set 1
        WriteString('# Conduit Recharge data Stress Period ');
        WriteInteger(StressPeriodIndex + 1);
        NewLine;
        // Data Set 2;
        WriteInteger(IFLAG_CRCH);
        NewLine;
        // Data Set 3;
        for NodeIndex := 0 to FNodes.Count - 1 do
        begin
          ANode := FNodes[NodeIndex];
          WriteInteger(ANode.FNumber);
          if ANode.FRechargeFractionUsed[StressPeriodIndex] then
          begin
            WriteFloat(ANode.FRechargeFraction[StressPeriodIndex]);
          end
          else
          begin
            WriteFloat(0);
          end;
          NewLine;
        end;
      end;
    finally
      CloseFile;
    end;
  end;
end;

{ TCfpNode }

constructor TCfpNode.Create;
begin
  FPipes := TList<TCfpPipe>.Create;
end;

destructor TCfpNode.Destroy;
begin
  FPipes.Free;
  inherited;
end;

{ TCfpPipe }

function TCfpPipe.OtherNode(ANode: TCfpNode): TCfpNode;
begin
  if ANode = FNode1 then
  begin
    result := FNode2;
  end
  else if ANode = FNode2 then
  begin
    result := FNode1;
  end
  else
  begin
    Result := nil;
    Assert(False);
  end;
end;

function TCfpPipe.SameNodes(Node1, Node2: TCfpNode): boolean;
begin
  result := ((Node1 = FNode1) and (Node2 = FNode2))
    or ((Node1 = FNode2) and (Node2 = FNode1));
end;

end.
