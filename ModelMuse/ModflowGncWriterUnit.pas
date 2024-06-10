unit ModflowGncWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowGncUnit, System.Generics.Collections,
  PhastModelUnit, ModflowIrregularMeshUnit, ModflowPackageSelectionUnit,
  Vcl.Forms, System.Classes;

type

  TWeightList = TObjectList<TWeightedCellId>;

  TFinalGnc = class(TObject)
  private
    FGhostNode: TGhostNode;
    FLayer: Integer;
    FWeightList: TWeightList;
  public
    constructor Create(AGhostNode: TGhostNode; Layer: Integer);
    destructor Destroy; override;
  end;

  TFinalGncList = TObjectList<TFinalGnc>;

  TModflowGncWriter = class(TCustomPackageWriter)
  private
    FFileName: string;
    FFGncList: TFinalGncList;
    FDisvGrid: TModflowDisvGrid;
    numalphaj: Integer;
    procedure Evaluate;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WriteGncData;
  protected
    class function Extension: string; override;
    // @name identifies the package that is being exported.
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  GoPhastTypes, DataSetUnit, frmProgressUnit,
  frmErrorsAndWarningsUnit, DataSetNamesUnit;

resourcestring
  StrWritingGNCPackage = 'Writing GNC Package input.';
  StrEvaluatingGhostnod = 'Evaluating ghost-node corrections';
  StrWritingGNCDimensio = 'Writing GNC dimensions';
  StrWritingGNCData = 'Writing GNC data';
  StrTheXT3DOptionCann = 'The XT3D option cannot be used with the GNC Packag' +
  'e.';
  StrMODFLOWDoesNotAll = 'MODFLOW does not allow the XT3D option and the GNC' +
  ' Package to be used in the same model. You must deactivate at least one o' +
  'f them for MODFLOW to run.';

{ TModflowGncWriter }

constructor TModflowGncWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FFGncList := TFinalGncList.Create;
end;

destructor TModflowGncWriter.Destroy;
begin
  FFGncList.Free;
  inherited;
end;

procedure TModflowGncWriter.Evaluate;
var
  FGhostNodes: TGhostNodes;
  LayerIndex: Integer;
  GhostNodeIndex: Integer;
  AGhostNode: TGhostNode;
//  ActiveDataArray: TDataArray;
  ConnectedIndex: Integer;
  AWeight: TWeightedCellId;
  AGhostNodeCorrection: TFinalGnc;
  NewWeight: TWeightedCellId;
  KxDataArray: TDataArray;
  ALayer: TModflowIrregularLayer;
  ACell: TModflowDisVCell;
  SumWeights: double;
  IDomainDataSet: TDataArray;
begin
  FDisvGrid := Model.DisvGrid;
  FGhostNodes := FDisvGrid.TwodGrid.GhostNodes;
  IDomainDataSet := Model.DataArrayManager.GetDataSetByName(K_IDOMAIN);
  KxDataArray := Model.DataArrayManager.GetDataSetByName(rsKx);
  for LayerIndex := 0 to FDisvGrid.Layers.Count - 1 do
  begin
    ALayer := FDisvGrid.Layers[LayerIndex].Layer;
    for GhostNodeIndex := 0 to FGhostNodes.Count - 1 do
    begin
      AGhostNode := FGhostNodes[GhostNodeIndex];
      if (IDomainDataSet.IntegerData[LayerIndex,0,AGhostNode.ContainingCell.Cell] > 0)
        and (IDomainDataSet.IntegerData[LayerIndex,0,AGhostNode.LinkedCell.Cell] > 0) then
      begin
        AGhostNodeCorrection := TFinalGnc.Create(AGhostNode, LayerIndex);
        SumWeights := 0;
        for ConnectedIndex := 0 to AGhostNode.CellWeights.Count - 1 do
        begin
          AWeight := AGhostNode.CellWeights[ConnectedIndex];
          if IDomainDataSet.IntegerData[LayerIndex,0,AWeight.Cell] > 0 then
          begin
            NewWeight := TWeightedCellId.Create(nil);
            NewWeight.Assign(AWeight);
            ACell := ALayer[AWeight.Cell];
            NewWeight.Weight := NewWeight.Weight
              * KxDataArray.RealData[LayerIndex,0,AWeight.Cell]
              * ACell.Thickness;
            if NewWeight.Weight > 0 then
            begin
              AGhostNodeCorrection.FWeightList.Add(NewWeight);
              SumWeights := SumWeights + NewWeight.Weight;
            end;
          end;
        end;
        if AGhostNodeCorrection.FWeightList.Count <= 1 then
        begin
          AGhostNodeCorrection.Free;
        end
        else
        begin
          FFGncList.Add(AGhostNodeCorrection);
          for ConnectedIndex := 0 to AGhostNodeCorrection.FWeightList.Count - 1 do
          begin
            NewWeight := AGhostNodeCorrection.FWeightList[ConnectedIndex];
            NewWeight.Weight := NewWeight.Weight / SumWeights;
          end;
        end;
      end;
    end;
  end;
end;

class function TModflowGncWriter.Extension: string;
begin
  Result := '.gnc';
end;

function TModflowGncWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GncPackage;
end;

procedure TModflowGncWriter.WriteDimensions;
var
  numgnc: Integer;
  GhostNodeIndex: Integer;
  AGhostNode: TFinalGnc;
begin
  numgnc := FFGncList.Count;

  numalphaj := 0;
  for GhostNodeIndex := 0 to FFGncList.Count - 1 do
  begin
    AGhostNode := FFGncList[GhostNodeIndex];
    if AGhostNode.FWeightList.Count > numalphaj then
    begin
      numalphaj := AGhostNode.FWeightList.Count;
    end;
  end;
  // AGhostNode.FWeightList includes a weight for the cell that contains
  // the ghost node but that cell isn't part of the weight input in the
  // GNCDATA block.
  Dec(numalphaj);

  WriteBeginDimensions;
  try
    WriteString('  NUMGNC');
    WriteInteger(numgnc);
    NewLine;
    WriteString('  NUMALPHAJ');
    WriteInteger(numalphaj);
    NewLine;
  finally
    WriteEndDimensions;
  end;
end;

procedure TModflowGncWriter.WriteFile(const AFileName: string);
const
  Abbreviation = 'GNC6';
begin
  if not Model.DisvUsed then
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(Abbreviation) then
  begin
    Exit;
  end;
  if not Package.IsSelected then
  begin
    Exit
  end;


  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheXT3DOptionCann);
  if Model.ModflowPackages.NpfPackage.UseXT3D then
  begin
    frmErrorsAndWarnings.AddError(Model, StrTheXT3DOptionCann,
      StrMODFLOWDoesNotAll)
  end;

  frmProgressMM.AddMessage(StrEvaluatingGhostnod);
  Evaluate;
  if FFGncList.Count = 0 then
  begin 
    Exit;
  end;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FFileName := FileName(AFileName);
  FNameOfFile := FFileName;
  WriteToNameFile(Abbreviation, -1, FFileName, foInput, Model, False, 'GNC');

  FInputFileName := FFileName;
  OpenFile(FFileName);
  try
    frmProgressMM.AddMessage(StrWritingGNCPackage);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteOptions;

    frmProgressMM.AddMessage(StrWritingGNCDimensio);
    WriteDimensions;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingGNCData);
    WriteGncData;

  finally
    CloseFile;
  end;

end;

procedure TModflowGncWriter.WriteGncData;
var
  GhostNodeIndex: Integer;
  AGhostNode: TFinalGnc;
  WeightIndex: Integer;
  AWeightCell: TWeightedCellId;
  Count: Integer;
begin
  WriteString('BEGIN GNCDATA');
  NewLine;
  try
    for GhostNodeIndex := 0 to FFGncList.Count - 1 do
    begin
      AGhostNode := FFGncList[GhostNodeIndex];
      WriteString(' ');
      WriteInteger(AGhostNode.FLayer + 1);
      WriteInteger(AGhostNode.FGhostNode.ContainingCell.Cell + 1);
      WriteString('   ');
      WriteInteger(AGhostNode.FLayer + 1);
      WriteInteger(AGhostNode.FGhostNode.LinkedCell.Cell + 1);
      Count := 0;
      for WeightIndex := 0 to AGhostNode.FWeightList.Count - 1 do
      begin
        AWeightCell := AGhostNode.FWeightList[WeightIndex];
        if AGhostNode.FGhostNode.ContainingCell.Cell = AWeightCell.Cell then
        begin
          Continue;
        end;
        WriteString('   ');
        WriteInteger(AGhostNode.FLayer + 1);
        WriteInteger(AWeightCell.Cell + 1);
        Inc(Count)
      end;

      for WeightIndex := Count to numalphaj -1 do
      begin
        WriteString('   ');
        WriteInteger(0);
        WriteInteger(0);
      end;

      WriteString('   ');
      for WeightIndex := 0 to AGhostNode.FWeightList.Count - 1 do
      begin
        AWeightCell := AGhostNode.FWeightList[WeightIndex];
        if AGhostNode.FGhostNode.ContainingCell.Cell = AWeightCell.Cell then
        begin
          Continue;
        end;
        WriteFloat(AWeightCell.Weight);
      end;

      for WeightIndex := Count to numalphaj -1 do
      begin
        WriteFloat(0);
      end;

      NewLine;
    end;
  finally
    WriteString('END GNCDATA');
    NewLine;
  end;
end;

procedure TModflowGncWriter.WriteOptions;
var
  GncPackage: TGncPackage;
begin
  GncPackage := Model.ModflowPackages.GncPackage;
  WriteBeginOptions;
  try
    PrintListInputOption;
    PrintFlowsOption;
    if GncPackage.EquationFormulation = efExplicit then
    begin
      WriteString('  EXPLICIT');
      NewLine;
    end;
  finally
    WriteEndOptions;
  end;
end;

{ TFinalGnc }

constructor TFinalGnc.Create(AGhostNode: TGhostNode; Layer: Integer);
begin
  FWeightList := TWeightList.Create;
  FGhostNode := AGhostNode;
  FLayer := Layer;
end;

destructor TFinalGnc.Destroy;
begin
  FWeightList.Free;
  inherited;
end;

end.
