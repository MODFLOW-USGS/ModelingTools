unit Mt3dLktWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit,
  ScreenObjectUnit, GoPhastTypes, System.Generics.Collections, Vcl.Forms,
  System.Classes;

type
  TLakeConcBoundary  = record
    LakeNumber: Integer;
    BoundaryType: Integer;
    Concentrations: array of double;
  end;

  TLakeConcBoundList = TList<TLakeConcBoundary>;

  TLakeStressPeriods = TObjectList<TLakeConcBoundList>;

  TMt3dLktWriter = class(TCustomModflowWriter)
  private
//    FNameOfFile: string;
    // @name contains Lake objects for MODFLOW-2005 or MODFLOW 6. Those may or
    // may not have lake transport objects.
    FLakeObjects: TScreenObjectList;
    FInitialConcentrations: TTwoDRealArray;
    FStressPeriods: TLakeStressPeriods;
    procedure Evaluate;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteStressPeriods;
    procedure WriteDataSet4(StressPeriodIndex: Integer);
    procedure GetLakeBoundaries;
    procedure GetInitialConcentrations;
    procedure EvaluateStressPeriods;
    function LakeNumber(AScreenObject: TScreenObject): Integer;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel;
      EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;


implementation

uses
  ModflowUnitNumbers, ModflowLakUnit, ModflowLakMf6Unit,
  Mt3dLktUnit, ModflowPackageSelectionUnit, GIS_Functions, RbwParser,
  ModflowLAK_Writer, frmProgressUnit, System.SysUtils;

resourcestring
  StrWritingMT3DUSGSLK = 'Writing MT3D-USGS LKT Package input.';

{ TMt3dLktWriter }

constructor TMt3dLktWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FArrayWritingFormat := awfMt3dms;
  FLakeObjects := TScreenObjectList.Create;
  FStressPeriods := TLakeStressPeriods.Create;
end;

destructor TMt3dLktWriter.Destroy;
begin
  FStressPeriods.Free;
  FLakeObjects.Free;
  inherited;
end;

procedure TMt3dLktWriter.Evaluate;
var
  LAK_Writer: TModflowLAK_Writer;
begin
  if Model.ModelSelection <> msModflow2015 then
  begin
    LAK_Writer := TModflowLAK_Writer.Create(Model, FEvaluationType);
    try
      LAK_Writer.Evaluate;
    finally
      LAK_Writer.Free;
    end;
  end;

  GetLakeBoundaries;
  GetInitialConcentrations;
  EvaluateStressPeriods;
end;

procedure TMt3dLktWriter.EvaluateStressPeriods;
var
  StressPeriodIndex: Integer;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  LktConcBoundary: TMt3dLktConcBoundary;
  TimeIndex: Integer;
  Item: TMt3dLktConcItem;
  NCOMP: Integer;
  ComponentIndex: Integer;
  Formula: String;
  Compiler: TRbwParser;
  Expression: TExpression;
  LakeBoundary: TLakeConcBoundary;
  LakeNum: Integer;
  StartStressPeriod: Integer;
  EndStressPeriod: Integer;
begin
  NCOMP := Model.NumberOfMt3dChemComponents;
  Compiler := Model.rpThreeDFormulaCompiler;
  FStressPeriods.Clear;
  FStressPeriods.Capacity := Model.ModflowFullStressPeriods.Count;
  SetLength(LakeBoundary.Concentrations, NCOMP);
  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
  begin
    FStressPeriods.Add(TLakeConcBoundList.Create);
  end;
  for ScreenObjectIndex := 0 to FLakeObjects.Count - 1 do
  begin
    AScreenObject := FLakeObjects[ScreenObjectIndex];
    if Model.ModelSelection <> msModflow2015 then
    begin
      LakeNum := LakeNumber(AScreenObject);
    end
    else
    begin
      LakeNum := ScreenObjectIndex+1;
    end;
    LakeBoundary.LakeNumber := LakeNum;

    LktConcBoundary := AScreenObject.Mt3dLktConcBoundary;
    if (LktConcBoundary <> nil) and LktConcBoundary.Used then
    begin
      LakeBoundary.BoundaryType := 1;
      for TimeIndex := 0 to LktConcBoundary.Values.Count - 1 do
      begin
        Item := LktConcBoundary.Values[TimeIndex] as TMt3dLktConcItem;
        SetLength(LakeBoundary.Concentrations, NCOMP);
        for ComponentIndex := 0 to NCOMP - 1 do
        begin
          Formula := Item.Mt3dmsConcRate[ComponentIndex];
          Compiler.Compile(Formula);
          UpdateCurrentScreenObject(AScreenObject);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
          LakeBoundary.Concentrations[ComponentIndex] := Expression.DoubleResult;
        end;

        StartStressPeriod := Model.ModflowFullStressPeriods.FindStressPeriod(Item.StartTime);
        EndStressPeriod := Model.ModflowFullStressPeriods.FindEndStressPeriod(Item.EndTime);

        for StressPeriodIndex := StartStressPeriod to EndStressPeriod do
        begin
          FStressPeriods[StressPeriodIndex].Add(LakeBoundary);
        end;
      end;


      LakeBoundary.BoundaryType := 2;
      for TimeIndex := 0 to LktConcBoundary.RunoffConcentration.Count - 1 do
      begin
        Item := LktConcBoundary.RunoffConcentration[TimeIndex] as TMt3dLktConcItem;
        SetLength(LakeBoundary.Concentrations, NCOMP);
        for ComponentIndex := 0 to NCOMP - 1 do
        begin
          Formula := Item.Mt3dmsConcRate[ComponentIndex];
          Compiler.Compile(Formula);
          UpdateCurrentScreenObject(AScreenObject);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
          LakeBoundary.Concentrations[ComponentIndex] := Expression.DoubleResult;
        end;

        StartStressPeriod := Model.ModflowFullStressPeriods.FindStressPeriod(Item.StartTime);
        EndStressPeriod := Model.ModflowFullStressPeriods.FindEndStressPeriod(Item.EndTime);

        for StressPeriodIndex := StartStressPeriod to EndStressPeriod do
        begin
          FStressPeriods[StressPeriodIndex].Add(LakeBoundary);
      end;
        end;
    end;
  end;
end;

class function TMt3dLktWriter.Extension: string;
begin
  result := '.lkt';
end;

procedure TMt3dLktWriter.WriteDataSet1;
var
  NLKINIT: Integer;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  LktConcBoundary: TMt3dLktConcBoundary;
  MXLKBC: Integer;
  Mt3dLkt: TMt3dLktPackage;
  ICBCLK: Integer;
  IETLAK: Integer;
  OutName: string;
  BaseName: string;
begin
  NLKINIT := FLakeObjects.Count;
  MXLKBC := 0;
  for ScreenObjectIndex := 0 to FLakeObjects.Count - 1 do
  begin
    AScreenObject := FLakeObjects[ScreenObjectIndex];
    LktConcBoundary := AScreenObject.Mt3dLktConcBoundary;
    if (LktConcBoundary <> nil) and LktConcBoundary.Used then
    begin
      if LktConcBoundary.Values.Count > 0 then
      begin
        Inc(MXLKBC);
      end;
      if LktConcBoundary.RunoffConcentration.Count > 0 then
      begin
        Inc(MXLKBC);
      end;
    end;
  end;
  Mt3dLkt := Model.ModflowPackages.Mt3dLkt;

  BaseName := ChangeFileExt(FNameOfFile, '');
  if Mt3dLkt.PrintLakeTransportBudget then
  begin
    ICBCLK := nt3dLktOut;
    OutName := ChangeFileExt(BaseName, '.lktobs');

    WriteToMt3dMsNameFile(StrDATA, ICBCLK,
      OutName, foOutput, Model);
  end
  else
  begin
    ICBCLK := 0;
  end;


  if Mt3dLkt.EvaporateMass then
  begin
    IETLAK := 1;
  end
  else
  begin
    IETLAK := 0;
  end;
  
  WriteInteger(NLKINIT);
  WriteInteger(MXLKBC);
  WriteInteger(ICBCLK);
  WriteInteger(IETLAK);
  WriteString(' # NLKINIT, MXLKBC, ICBCLK, IETLAK');
  NewLine;
end;

procedure TMt3dLktWriter.WriteDataSet2;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for RowIndex := 0 to Length(FInitialConcentrations) - 1 do
  begin
    WriteU2DRELHeader('CINITLAK', matStructured, '');
    for ColIndex := 0 to Length(FInitialConcentrations[0]) - 1 do
    begin
      WriteFloat(FInitialConcentrations[RowIndex,ColIndex]);
      NewLine;
    end;
  end;
end;

procedure TMt3dLktWriter.WriteStressPeriods;
var
  StressPeriodIndex: Integer;
  StressPeriodData: TLakeConcBoundList;
  NTMP: Integer;

begin
  for StressPeriodIndex := 0 to FStressPeriods.Count -1 do
  begin
    StressPeriodData := FStressPeriods[StressPeriodIndex];
    NTMP := StressPeriodData.Count;
    WriteI10Integer(NTMP, 'NTMP in LKT');
    WriteString(' # NTMP for stress period ');
    WriteInteger(StressPeriodIndex+1);
    NewLine;

    WriteDataSet4(StressPeriodIndex);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

  end;
end;

procedure TMt3dLktWriter.WriteDataSet4(StressPeriodIndex: Integer);
var
  StressPeriodData: TLakeConcBoundList;
  LakeIndex: Integer;
  LakeConc: TLakeConcBoundary;
  ComponentIndex: Integer;
begin
  StressPeriodData := FStressPeriods[StressPeriodIndex];
  for LakeIndex := 0 to StressPeriodData.Count -1 do
  begin
    LakeConc := StressPeriodData[LakeIndex];
    WriteInteger(LakeConc.LakeNumber);
    WriteInteger(LakeConc.BoundaryType);
    for ComponentIndex := 0 to Length(LakeConc.Concentrations)-1 do
    begin
      WriteFloat(LakeConc.Concentrations[ComponentIndex]);
    end;
    WriteString(' # ILKBC, ILKBCTYP, (CBCLK(n), n=1, NCOMP)');
    NewLine;
  end;
end;



procedure TMt3dLktWriter.WriteFile(const AFileName: string);
begin
  if Model.ModelSelection <> msModflowNWT then
  begin
    Exit;
  end;

  if not Model.ModflowPackages.Mt3dLkt.IsSelected then
  begin
    Exit;
  end;
  // remove errors and warnings
//  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrFileForTheInitial);

  FNameOfFile := FileName(AFileName);
  // PackageGeneratedExternally needs to be updated for MT3DMS
  if Model.PackageGeneratedExternally(StrLKT) then
  begin
    Exit;
  end;

  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  // write to MT3DMS or MT3D-USGS name file.
  WriteToMt3dMsNameFile(StrLKT, mt3dLKT,
    FNameOfFile, foInput, Model);

  FInputFileName := FNameOfFile;
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingMT3DUSGSLK);
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

    frmProgressMM.AddMessage(StrWritingDataSets3and4);
    WriteStressPeriods;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

procedure TMt3dLktWriter.GetInitialConcentrations;
var
  NCOMP: Integer;
  Compiler: TRbwParser;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  LktConcBoundary: TMt3dLktConcBoundary;
  ComponentIndex: Integer;
  Item: TLktInitConcItem;
  Formula: string;
  Expression: TExpression;
  LakeNum: Integer;
begin
  NCOMP := Model.NumberOfMt3dChemComponents;
  SetLength(FInitialConcentrations, NCOMP, FLakeObjects.Count);
  Compiler := Model.rpThreeDFormulaCompiler;
  for ScreenObjectIndex := 0 to FLakeObjects.Count - 1 do
  begin
    AScreenObject := FLakeObjects[ScreenObjectIndex];
    if Model.ModelSelection <> msModflow2015 then
    begin
      LakeNum := LakeNumber(AScreenObject) -1;
    end
    else
    begin
      LakeNum := ScreenObjectIndex;
    end;
    LktConcBoundary := AScreenObject.Mt3dLktConcBoundary;
    if (LktConcBoundary <> nil) and (LktConcBoundary.InitialConcentrations.Count > 0) then
    begin
      for ComponentIndex := 0 to NCOMP - 1 do
      begin
        Item := LktConcBoundary.InitialConcentrations[ComponentIndex] as TLktInitConcItem;
        Formula := Item.InitConc;
        Compiler.Compile(Formula);
        UpdateCurrentScreenObject(AScreenObject);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
        FInitialConcentrations[ComponentIndex, LakeNum] := Expression.DoubleResult;
      end;
    end
    else
    begin
      for ComponentIndex := 0 to NCOMP - 1 do
      begin
        FInitialConcentrations[ComponentIndex, LakeNum] := 0;
      end;
    end;
  end;
end;

procedure TMt3dLktWriter.GetLakeBoundaries;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  LakBoundary: TLakBoundary;
  Lak6Boundary: TLakeMf6;
  AddLake: Boolean;
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
    AddLake := False;
    if Model.ModelSelection <> msModflow2015 then
    begin
      LakBoundary := ScreenObject.ModflowLakBoundary;
      if (LakBoundary <> nil) and LakBoundary.Used then
      begin
        AddLake := True;
      end;
    end
    else
    begin
      Lak6Boundary := ScreenObject.ModflowLak6;
      if (Lak6Boundary <> nil) and Lak6Boundary.Used then
      begin
        AddLake := True;
      end;
    end;
    if AddLake then
    begin
      FLakeObjects.Add(ScreenObject);
    end;
  end;
end;

function TMt3dLktWriter.LakeNumber(AScreenObject: TScreenObject): Integer;
var
  LakBoundary: TLakBoundary;
begin
  Result := -1;
  if Model.ModelSelection <> msModflow2015 then
  begin
    LakBoundary := AScreenObject.ModflowLakBoundary;
    if (LakBoundary <> nil) and LakBoundary.Used then
    begin
      result := LakBoundary.TrueLakeID;
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

end.
