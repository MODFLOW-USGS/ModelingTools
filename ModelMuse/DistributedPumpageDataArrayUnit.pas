unit DistributedPumpageDataArrayUnit;

interface

uses
  DataSetUnit, System.Classes;

type
  TDistributedPumpageDataArray = class(TDataArray)
  private
    FUpdatingProgress: Boolean;
  public
    procedure Initialize; override;
  end;

implementation

uses
  PhastModelUnit, ModflowBoundaryDisplayUnit, ScreenObjectUnit,
  FootprintBoundary, FootprintGridUnit, GoPhastTypes, RbwParser,
  frmProgressUnit, System.SysUtils, GIS_Functions, frmErrorsAndWarningsUnit;

resourcestring
  StrNoDistributedPumpi = 'No Distributed Pumping';
  StrTooMuchPumpage = 'Too much pumpage';
  StrTheSumOfThePumpa = 'The sum of the pumpages of all the objects exceeds ' +
  'the pumpage capacity.';
  StrRedistributedPumpag = 'Redistributed Pumpage';

{ TDistributedPumpageDataArray }

procedure TDistributedPumpageDataArray.Initialize;
const
  IterationLimit = 10000;
var
  LocalModel: TCustomModel;
  DataArrayManager: TDataArrayManager;
  HideProgressForm: Boolean;
  ActiveDataArray: TDataArray;
  DepthRateDataArray: TDataArray;
  PumpageCapacity: double;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryPumpage: TModflowBoundaryDisplayDataArray;
  FootPrintGrid: TFootprintGrid;
  FreeStack: Boolean;
  LayerLimit: integer;
  RowLimit: integer;
  ColLimit: integer;
  InitialValueAnnotation: string;
  NeighborCount: array of array of Integer;
  CellCapacity: TTwoDRealArray;
  NewValues: TTwoDRealArray;
  PriorValues: TTwoDRealArray;
  IterationIndex: Integer;
  TotalPumpage: double;
  RedistbAnnotation: string;
  Excess: Double;
  procedure GetLimits;
  begin
    LayerLimit := LayerCount - 1;
    RowLimit := RowCount - 1;
    ColLimit := ColumnCount - 1;
  end;
begin
  if FUpdatingProgress then
  begin
    Exit;
  end;

//  OutputDebugString('SAMPLING ON');
  if UpToDate and not DimensionsChanged then
  begin
    CheckRestoreData;
    Exit;
  end;

  LocalModel := FModel as TCustomModel;
  DataArrayManager := LocalModel.DataArrayManager;
  HideProgressForm := False;
  if (Name <> '') and (frmProgressMM <> nil) then
  begin
    FUpdatingProgress := True;
    try
      HideProgressForm := not frmProgressMM.Visible;
      if HideProgressForm then
      begin
        frmProgressMM.Caption := '';
        frmProgressMM.Show;
      end;
      frmProgressMM.AddMessage(Format(StrEvaluatingDat, [Name]), False);
    finally
      FUpdatingProgress := False;
    end;
  end;

  FDataCached := False;

  ActiveDataArray := DataArrayManager.GetDataSetByName(rsActive);
  DepthRateDataArray := DataArrayManager.GetDataSetByName(KDepthRateIndex);
  ActiveDataArray.TalksTo(self);
  DepthRateDataArray.TalksTo(self);

  ActiveDataArray.Initialize;
  DepthRateDataArray.Initialize;

  FootPrintGrid := LocalModel.FootPrintGrid;

  GetLimits;
  SetLength(CellCapacity, RowLimit+1, ColLimit+1);

  PumpageCapacity := 0;
  for RowIndex := 0 to ActiveDataArray.RowCount - 1 do
  begin
    for ColIndex := 0 to ActiveDataArray.ColumnCount - 1 do
    begin
      if ActiveDataArray.BooleanData[0,RowIndex,ColIndex] then
      begin
        CellCapacity[RowIndex,ColIndex] :=
          DepthRateDataArray.RealData[0,RowIndex,ColIndex]*
          FootPrintGrid.ColumnWidth[ColIndex]*
          FootPrintGrid.RowWidth[RowIndex];
        PumpageCapacity := PumpageCapacity +
          CellCapacity[RowIndex,ColIndex];
      end
      else
      begin
        CellCapacity[RowIndex,ColIndex] := 0;
      end;
    end;
  end;

  FreeStack := (Stack = nil);
  try
    if FreeStack then
    begin
      Stack := TStringList.Create;
    end;
    if Stack.IndexOf(Name) >= 0 then
    begin
      UpToDate := True;
      raise ECircularReference.Create(Format(StrCircularReferenceI2, [Name, Stack.Text]));
    end;
    Stack.Add(Name);

    GlobalEvaluatedAt := EvaluatedAt;

    if DimensionsChanged then
    begin
      SetDimensions(False);
    end
    else
    begin
      RestoreArraySize;
    end;

    if (LayerLimit >= 0) and (RowLimit >= 0) and (ColLimit >= 0) then
    begin
      UpdateCurrentScreenObject(nil);
    end;

    BoundaryPumpage := DataArrayManager.
      GetDataSetByName(KFootprint_Well_Pumpage)
      as TModflowBoundaryDisplayDataArray;

    BoundaryPumpage.TalksTo(self);

    BoundaryPumpage.Initialize;

    RedistbAnnotation := StrRedistributedPumpag;

    TotalPumpage := 0;
    InitialValueAnnotation := StrNoDistributedPumpi;
    for RowIndex := 0 to RowLimit do
    begin
      for ColIndex := 0 to ColLimit do
      begin
        if BoundaryPumpage.IsValue[0,RowIndex,ColIndex]
          and ActiveDataArray.BooleanData[0,RowIndex,ColIndex] then
        begin
          RealData[0,RowIndex,ColIndex] :=
            BoundaryPumpage.RealData[0,RowIndex,ColIndex];
          Annotation[0,RowIndex,ColIndex] := RedistbAnnotation;
          TotalPumpage := TotalPumpage +
            BoundaryPumpage.RealData[0,RowIndex,ColIndex];
        end
        else
        begin
          RealData[0,RowIndex,ColIndex] := 0;
          Annotation[0,RowIndex,ColIndex] := InitialValueAnnotation;
        end;
      end;
    end;

    frmErrorsAndWarnings.RemoveErrorGroup(LocalModel, StrTooMuchPumpage);
    if TotalPumpage > PumpageCapacity then
    begin
      frmErrorsAndWarnings.AddError(LocalModel, StrTooMuchPumpage,
        StrTheSumOfThePumpa);
      UpToDate := True;
      Exit;
    end;

    SetLength(NeighborCount, RowLimit+1, ColLimit+1);
    for RowIndex := 0 to RowLimit do
    begin
      for ColIndex := 0 to ColLimit do
      begin
        NeighborCount[RowIndex,ColIndex] := 0;
        if ActiveDataArray.BooleanData[0,RowIndex,ColIndex] then
        begin
          if (RowIndex > 0)
            and ActiveDataArray.BooleanData[0,RowIndex-1,ColIndex] then
          begin
            NeighborCount[RowIndex,ColIndex] :=
              NeighborCount[RowIndex,ColIndex] + 1;
          end;
          if (ColIndex > 0)
            and ActiveDataArray.BooleanData[0,RowIndex,ColIndex-1] then
          begin
            NeighborCount[RowIndex,ColIndex] :=
              NeighborCount[RowIndex,ColIndex] + 1;
          end;
          if (RowIndex < RowLimit)
            and ActiveDataArray.BooleanData[0,RowIndex+1,ColIndex] then
          begin
            NeighborCount[RowIndex,ColIndex] :=
              NeighborCount[RowIndex,ColIndex] + 1;
          end;
          if (ColIndex < ColLimit)
            and ActiveDataArray.BooleanData[0,RowIndex,ColIndex+1] then
          begin
            NeighborCount[RowIndex,ColIndex] :=
              NeighborCount[RowIndex,ColIndex] + 1;
          end;
        end;
      end;
    end;

    SetLength(PriorValues, RowLimit+1, ColLimit+1);
    for RowIndex := 0 to RowLimit do
    begin
      for ColIndex := 0 to ColLimit do
      begin
        if BoundaryPumpage.IsValue[0,RowIndex,ColIndex]
          and ActiveDataArray.BooleanData[0,RowIndex,ColIndex] then
        begin
          PriorValues[RowIndex,ColIndex] :=
            BoundaryPumpage.RealData[0,RowIndex,ColIndex];
        end
        else
        begin
          PriorValues[RowIndex,ColIndex] := 0;
        end;
      end;
    end;
    NewValues := PriorValues;
    SetLength(NewValues, RowLimit+1, ColLimit+1);
    for IterationIndex := 0 to IterationLimit - 1 do
    begin
      for RowIndex := 0 to RowLimit do
      begin
        for ColIndex := 0 to ColLimit do
        begin
          if (NeighborCount[RowIndex,ColIndex] > 0) then
          begin
            Excess := PriorValues[RowIndex,ColIndex]-
              CellCapacity[RowIndex,ColIndex];
            if Excess > 0 then
            begin
              Excess := Excess/(NeighborCount[RowIndex,ColIndex]+1);
              NewValues[RowIndex,ColIndex] := NewValues[RowIndex,ColIndex]
                - Excess * NeighborCount[RowIndex,ColIndex];
              if (RowIndex > 0)
                and ActiveDataArray.BooleanData[0,RowIndex-1,ColIndex] then
              begin
                NewValues[RowIndex-1,ColIndex] :=
                  NewValues[RowIndex-1,ColIndex]+Excess;
              end;
              if (ColIndex > 0)
                and ActiveDataArray.BooleanData[0,RowIndex,ColIndex-1] then
              begin
                NewValues[RowIndex,ColIndex-1] :=
                  NewValues[RowIndex,ColIndex-1]+Excess;
              end;
              if (RowIndex < RowLimit)
                and ActiveDataArray.BooleanData[0,RowIndex+1,ColIndex] then
              begin
                NewValues[RowIndex+1,ColIndex] :=
                  NewValues[RowIndex+1,ColIndex]+Excess;
              end;
              if (ColIndex < ColLimit)
                and ActiveDataArray.BooleanData[0,RowIndex,ColIndex+1] then
              begin
                NewValues[RowIndex,ColIndex+1] :=
                  NewValues[RowIndex,ColIndex+1]+Excess;
              end;
            end;
          end;
        end;
      end;
      PriorValues := NewValues;
      SetLength(NewValues, RowLimit+1, ColLimit+1);
    end;
    for RowIndex := 0 to RowLimit do
    begin
      for ColIndex := 0 to ColLimit do
      begin
        RealData[0,RowIndex,ColIndex] :=
          NewValues[RowIndex,ColIndex];
      end;
    end;

  finally
    if FreeStack then
    begin
      FreeAndNil(Stack);
      DataArrayManager.DontCache(self);
      DataArrayManager.CacheDataArrays;
//      OutputDebugString('SAMPLING OFF');
    end;
    if HideProgressForm then
    begin
      frmProgressMM.Hide;
    end;
  end;
  UpToDate := True;

end;

initialization
  RegisterClass(TDistributedPumpageDataArray);

end.
