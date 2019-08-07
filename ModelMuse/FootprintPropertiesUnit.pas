unit FootprintPropertiesUnit;

interface

uses
  System.Classes, GoPhastTypes, SubscriptionUnit, DataSetUnit,
  System.Generics.Collections, System.Generics.Defaults;

type
  TCellItem = class
    Col: Integer;
    Row: Integer;
    Distance: double;
  end;

  TCellItemObjectList = TObjectList<TCellItem>;

  TCellItemComparer = class(TComparer<TCellItem>)
    function Compare(const Left, Right: TCellItem): Integer; override;
  end;


  TFootprintProperties = class(TObserver)
  private
    FStoredClosureCriterion: TRealStorage;
    FMaxIterations: integer;
    FModel: TBaseModel;
//    FFootPrintWellError: Boolean;
    FPerformInitialRedistribution: boolean;
    FRedistributionCriterion: integer;
    FSaveResultsText: boolean;
    FSaveResultsBinary: boolean;
    FOpenInTextEditor: boolean;
    FStoredMinimumDepthRateIndex: TRealStorage;
    function GetClosureCriterion: Double;
    procedure SetClosureCriterion(const Value: double);
    procedure SetMaxIterations(const Value: integer);
    procedure SetStoredClosureCriterion(const Value: TRealStorage);
    procedure DoOnChange(sender: TObject);
    procedure DoOnChangeMinimumDepthRateIndex(sender: TObject);
    procedure SetPerformInitialRedistribution(const Value: boolean);
    procedure SetRedistributionCriterion(const Value: integer);
    procedure SetSaveResultsBinary(const Value: boolean);
    procedure SetSaveResultsText(const Value: boolean);
    procedure SetOpenInTextEditor(const Value: boolean);
    procedure SetStoredMinimumDepthRateIndex(const Value: TRealStorage);
    function GetMinimumDepthRateIndex: Double;
    procedure SetMinimumDepthRateIndex(const Value: Double);
  public
    Constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ClosureCriterion: Double read GetClosureCriterion
      write SetClosureCriterion;
    property MinimumDepthRateIndex: Double read GetMinimumDepthRateIndex
      write SetMinimumDepthRateIndex;
    procedure AssignFootprintBoundarydWithdrawal(Sender: TObject);
    procedure Initialize;
  published
    property MaxIterations: integer read FMaxIterations write SetMaxIterations
      default 10000;
    property StoredClosureCriterion: TRealStorage read FStoredClosureCriterion
      write SetStoredClosureCriterion;
    property PerformInitialRedistribution: boolean
      read FPerformInitialRedistribution write SetPerformInitialRedistribution
      default True;
    property RedistributionCriterion: integer read FRedistributionCriterion
      write SetRedistributionCriterion default 1;
    property SaveResultsBinary: boolean read FSaveResultsBinary
      write SetSaveResultsBinary default True;
    property SaveResultsText: boolean read FSaveResultsText
       write SetSaveResultsText;
    property OpenInTextEditor: boolean read FOpenInTextEditor
      write SetOpenInTextEditor default True;
    property StoredMinimumDepthRateIndex: TRealStorage
      read FStoredMinimumDepthRateIndex write SetStoredMinimumDepthRateIndex;
  end;

implementation

uses
  PhastModelUnit, ModflowBoundaryDisplayUnit,
  frmErrorsAndWarningsUnit, frmGoPhastUnit, ScreenObjectUnit, FootprintBoundary,
  RbwParser, AbstractGridUnit, GIS_Functions, System.SysUtils, System.Math,
  QuadTreeClass, frmCustomGoPhastUnit, frmFootprintPropertiesUnit,
  System.UITypes, Vcl.Dialogs;

resourcestring
  StrRedistributedWithdrawals = 'Redistributed withdrawals';
  StrNoDistributedWithdrawal = 'No Distributed withdrawals';
  StrTooMuchWithdrawal = 'Total withdrawals too large';
  StrTheSumOfTheWithdrawal = 'The sum of the withdrawals of all the objects (%0:g) exceeds ' +
  'the withdrawal capacity (%1:g).';
  StrClosureCriterionNo = 'Closure Criterion not met.';
  StrTheClosureCriterio = 'The closure criterion for the distributed withdrawals' +
  ' was not met. Try using a higher closure criterion or more iterations. The '
  + 'final excess fraction was %g.';
  StrNoWithdrawal = 'No withdrawals';
  StrInvalidWithdrawalR = 'Invalid withdrawal rate';
  StrInSTheWithdrawa = 'In %s, the withdrawal rate is less than zero.';
  StrInSTheWithdrawaWarning = 'In %s, the withdrawal rate is zero.';
  StrAssignedByObject = 'Assigned by object %0:s with formula %1:s.';
  StrWithdrawalSpecified = 'Withdrawal specified in inactive cell';
  StrRow0dColumn1 = 'Row: %0:d; Column %1:d';

{ TFootprintProperties }

procedure TFootprintProperties.Assign(Source: TPersistent);
var
  SourceProperties: TFootprintProperties;
begin
  if Source is TFootprintProperties then
  begin
    SourceProperties := TFootprintProperties(Source);
    MaxIterations := SourceProperties.MaxIterations;
    ClosureCriterion := SourceProperties.ClosureCriterion;
    MinimumDepthRateIndex := SourceProperties.MinimumDepthRateIndex;
    PerformInitialRedistribution := SourceProperties.PerformInitialRedistribution;
    RedistributionCriterion := SourceProperties.RedistributionCriterion;
    SaveResultsBinary := SourceProperties.SaveResultsBinary;
    SaveResultsText := SourceProperties.SaveResultsText;
    OpenInTextEditor := SourceProperties.OpenInTextEditor;
  end
  else
  begin
    inherited;
  end;
end;

procedure TFootprintProperties.AssignFootprintBoundarydWithdrawal(
  Sender: TObject);
var
  BoundaryWithdrawal: TModflowBoundaryDisplayDataArray;
  CellList: TCellAssignmentList;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  FootprintWell: TFootprintWell;
  ACell: TCellAssignment;
  Compiler: TRbwParser;
  AFormula: string;
  Expression: TExpression;
  Annotation: string;
  CellIndex: Integer;
  UsedVariables: TStringList;
  LocalGrid: TCustomModelGrid;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerIndex: Integer;
  AValue: double;
  LocalModel: TPhastModel;
  CanDraw: Boolean;
//  UsedFunction: string;
//  UsedExpression: TExpression;
begin
  CanDraw := frmGoPhast.CanDraw;
//  FFootPrintWellError := False;
  LocalModel := frmGoPhast.PhastModel;
  frmErrorsAndWarnings.RemoveErrorGroup(LocalModel, StrInvalidWithdrawalR);
  frmErrorsAndWarnings.RemoveWarningGroup(LocalModel, StrInvalidWithdrawalR);

  BoundaryWithdrawal := Sender as TModflowBoundaryDisplayDataArray;
  BoundaryWithdrawal.AddMethod := vamAdd;
  CellList := TCellAssignmentList.Create;
  UsedVariables := TStringList.Create;
  try
    frmGoPhast.CanDraw := False;
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      FootprintWell := AScreenObject.FootprintWell;
      if not AScreenObject.Deleted
        and (FootprintWell <> nil) and FootprintWell.Used
        and (AScreenObject.Count = AScreenObject.SectionCount) then
      begin
        UsedVariables.Clear;
        CellList.Clear;
        AScreenObject.GetCellsToAssign({LocalModel.Grid,}
          FootprintWell.Withdrawal, nil,
          BoundaryWithdrawal, CellList, alAll, LocalModel);

        AScreenObject.Delegate.InitializeExpression(Compiler,
          AFormula, Expression, BoundaryWithdrawal, nil, LocalModel);

        Annotation := Format(StrAssignedByObject,
          [AScreenObject.Name, AFormula]);
        AScreenObject.InitializeVariables(UsedVariables, BoundaryWithdrawal, Expression, Compiler);

        UpdateCurrentScreenObject(AScreenObject);
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          UpdateGlobalLocations(ACell.Column, ACell.Row, ACell.Layer,
            eaBlocks, LocalModel);
          UpdateCurrentSegment(ACell.Segment);
          UpdateCurrentSection(ACell.Section);
          AScreenObject.UpdateVariables(UsedVariables,
            BoundaryWithdrawal, ACell.Layer, ACell.Row,
            ACell.Column, Compiler);
          Expression.Evaluate;

          AValue := Expression.DoubleResult;
          if AValue > 0 then
          begin
            BoundaryWithdrawal.AddDataValue(Annotation, AValue,
              ACell.Column, ACell.Row, ACell.Layer);
          end
          else
          begin
//            FFootPrintWellError := True;
            if AValue < 0 then
            begin
              frmErrorsAndWarnings.AddError(LocalModel, StrInvalidWithdrawalR,
                Format(StrInSTheWithdrawa, [AScreenObject.Name]), AScreenObject);
            end
            else
            begin
              frmErrorsAndWarnings.AddWarning(LocalModel, StrInvalidWithdrawalR,
                Format(StrInSTheWithdrawaWarning, [AScreenObject.Name]), AScreenObject);
            end;
          end;
        end;
      end;
    end;
    LocalGrid := LocalModel.Grid;
    Annotation := StrNoWithdrawal;
    if LocalGrid <> nil then
    begin
      for LayerIndex := 0 to LocalGrid.LayerCount - 1 do
      begin
        for RowIndex := 0 to LocalGrid.RowCount - 1 do
        begin
          for ColIndex := 0 to LocalGrid.ColumnCount - 1 do
          begin
            if not BoundaryWithdrawal.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              BoundaryWithdrawal.AddDataValue(Annotation,
                0, ColIndex, RowIndex, LayerIndex);
            end;
          end;
        end;
      end;
    end;
    BoundaryWithdrawal.LabelAsSum;
    BoundaryWithdrawal.UpToDate := True;
  finally
    CellList.Free;
    UsedVariables.Free;
    frmGoPhast.CanDraw := CanDraw;
    frmGoPhast.ReDrawAllViews(nil);
  end
end;

procedure TFootprintProperties.Initialize;
begin
  FMaxIterations := 10000;
  FStoredClosureCriterion.Value := 0.01;
  FStoredMinimumDepthRateIndex.Value := 1E-6;
  FPerformInitialRedistribution := True;
  FRedistributionCriterion := 1;
  FStoredClosureCriterion.OnChange := DoOnChange;
  FStoredMinimumDepthRateIndex.OnChange := DoOnChangeMinimumDepthRateIndex;
  FSaveResultsBinary := True;
  FSaveResultsText := False;
  FOpenInTextEditor := True;
  UpToDate := True;
end;

constructor TFootprintProperties.Create(Owner: TComponent);
begin
  inherited;
  FStoredClosureCriterion := TRealStorage.Create;
  FStoredMinimumDepthRateIndex := TRealStorage.Create;
  Initialize;
  if Owner <> nil then
  begin
    FModel := Owner as TCustomModel;
  end;
end;

destructor TFootprintProperties.Destroy;
begin
  FStoredMinimumDepthRateIndex.Free;
  FStoredClosureCriterion.Free;
  inherited;
end;

function TFootprintProperties.GetClosureCriterion: Double;
begin
  Result := FStoredClosureCriterion.Value;
end;

function TFootprintProperties.GetMinimumDepthRateIndex: Double;
begin
  result := StoredMinimumDepthRateIndex.Value;
end;

procedure TFootprintProperties.DoOnChange(sender: TObject);
begin
  UpToDate := False;
  UpToDate := True;
  if FModel <> nil then
  begin
    FModel.Invalidate(Self);
  end;
end;

procedure TFootprintProperties.DoOnChangeMinimumDepthRateIndex(sender: TObject);
var
  DepthRateIndexDataArray: TDataArray;
begin
  DoOnChange(self);
  if FModel <> nil then
  begin
    DepthRateIndexDataArray := (FModel as TCustomModel).DataArrayManager.
      GetDataSetByName(KDepthRateIndex);
    if (DepthRateIndexDataArray <> nil)
      and (DepthRateIndexDataArray.Min <> MinimumDepthRateIndex) then
    begin
      DepthRateIndexDataArray.Min := MinimumDepthRateIndex;
      DepthRateIndexDataArray.Invalidate;
    end;

  end;
end;

procedure TFootprintProperties.SetClosureCriterion(const Value: double);
begin
  FStoredClosureCriterion.Value := Value;
end;

procedure TFootprintProperties.SetMaxIterations(const Value: integer);
begin
  if FMaxIterations <> Value then
  begin
    FMaxIterations := Value;
    DoOnChange(Self);
  end;
end;

procedure TFootprintProperties.SetMinimumDepthRateIndex(const Value: Double);
begin
  StoredMinimumDepthRateIndex.Value := Value;
end;

procedure TFootprintProperties.SetOpenInTextEditor(const Value: boolean);
begin
  FOpenInTextEditor := Value;
end;

procedure TFootprintProperties.SetPerformInitialRedistribution(
  const Value: boolean);
begin
  if FPerformInitialRedistribution <> Value then
  begin
    FPerformInitialRedistribution := Value;
    DoOnChange(Self);
  end;
end;

procedure TFootprintProperties.SetRedistributionCriterion(const Value: integer);
begin
  if FRedistributionCriterion <> Value then
  begin
    FRedistributionCriterion := Value;
    DoOnChange(Self);
  end;
end;

procedure TFootprintProperties.SetSaveResultsBinary(const Value: boolean);
begin
  FSaveResultsBinary := Value;
end;

procedure TFootprintProperties.SetSaveResultsText(const Value: boolean);
begin
  FSaveResultsText := Value;
end;

procedure TFootprintProperties.SetStoredClosureCriterion(
  const Value: TRealStorage);
begin
  FStoredClosureCriterion.Assign(Value);
end;

procedure TFootprintProperties.SetStoredMinimumDepthRateIndex(
  const Value: TRealStorage);
begin
  FStoredMinimumDepthRateIndex.Assign(Value);
end;

{ TCellItemComparer }

function TCellItemComparer.Compare(const Left, Right: TCellItem): Integer;
begin
  result := Sign(Left.Distance - Right.Distance);

end;

end.
