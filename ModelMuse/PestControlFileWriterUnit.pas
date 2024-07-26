unit PestControlFileWriterUnit;

interface

uses
  CustomModflowWriterUnit, System.SysUtils, PestObsUnit, GoPhastTypes,
  PhastModelUnit, ObsInterfaceUnit, OrderedCollectionUnit, System.Classes,
  RealListUnit;

type
  TPestControlFileWriter = class(TCustomFileWriter)
  private
//    FNameOfFile: string;
    FUsedObservations: TObservationInterfaceList;
    FUsePval: Boolean;
    F_AbsParMax: TRealList;
    FPriorInfomationEquations: TStringList;
    procedure WriteFirstLine;
    procedure WriteSectionHeader(const SectionID: String);
    procedure WriteControlSection(SetNOPTMAX: Boolean = False);
    // @name is not currently supported. Chapter 7
    procedure WriteSensitivityReuse;
    procedure WriteSingularValueDecomposition;
    procedure WriteLsqr;
    // @name is not currently supported. Section 6.3
    procedure WriteAutomaticUserIntervention;
    // @name is not currently written by ModelMuse.
    // It is written by SVDAPREP.
    // Section 10
    procedure WriteSVD_Assist;
    procedure WriteParameterGroups;
    procedure WriteParameters;
    procedure WriteObservationGroups;
    procedure WriteObservations;
    // @name is not supported because it is only relevant to MODFLOW-2000.
    procedure WriteDerivatives;
    procedure WriteCommandLine;
    procedure WriteModelInputOutput;
    procedure WritePriorInformation;
    procedure WritePredictiveAnalysis;
    procedure WriteRegularisation;
    // Section 13
    procedure WritePareto;
    // NPAR
    function NumberOfParameters: Integer;
    // NOBS
    function NumberOfObservations: integer;
    // NPARGP
    function NumberOfParameterGroups: Integer;
    // NPRIOR
    function NumberOfPriorInformation: Integer;
    // NOBSGP
    function NumberOfObservationGroups: Integer;
    function NumberOfTemplateFiles: Integer;
    procedure GetUsedTypes(var UsedTypes: TParameterTypes);
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    function WriteFile(const AFileName: string; SetNOPTMAX: Boolean = False): string;
  end;

implementation

uses
  PestPropertiesUnit, ModflowParameterUnit,
  PestParamGroupsUnit, PestObsGroupUnit, frmGoPhastUnit,
  PestObsExtractorInputWriterUnit, frmErrorsAndWarningsUnit,


  DataSetUnit, PilotPointDataUnit, System.Math,
  QuadTreeClass, PointCollectionUnit, System.Generics.Collections, FastGEO,
  System.Generics.Defaults, System.IOUtils, PilotPointCovarinceFileWriterUnit;

resourcestring
  StrNoParametersHaveB = 'No parameters have been defined';
  StrNoPestParameters = 'No parameters have been defined for use with PEST.';
  StrNoParameterGroups = 'No parameter groups defined';
  StrNoPESTParameterGr = 'No PEST parameter groups have been defined. Define them in "Model|Manage Parameters".';
  StrNoObservationGroup = 'No observation groups defined';
  StrNoPESTObservation = 'No PEST observation groups have been defined. "Define them in "Model|Pest Properties".';
  StrParameterGroupName = 'Parameter group name not assigned';
  StrTheParameterSH = 'The parameter "%s" has not been assigned to a paramet' +
  'er group ';
  StrObservationGroupNo = 'Observation group not assigned';
//  StrNoObservationGroupAssigned = 'No observation group has been assigned to %s.';
  StrParameterUndefined = 'Parameter undefined';
  StrPilotPointsWereDe = 'Pilot points were defined for %s but no parameter ' +
  'by that name that uses pilot points has been defined.';
  StrObservationGroupNa = 'Observation Group name too long';
  StrTheNamesOfSHas = 'The names of %s has been truncated because "regul" mu' +
  'st be inserted at the beginning of the observation group name to indicate' +
  ' it is to be used for regularization.';
  StrObservationGroupUn = 'Observation group undefined';
  StrNoObservationGroupAssigned = 'No observation group is defined for "%s"';
  StrInvalidParameterVa = 'Invalid parameter value';
  StrForParameter0s = 'For parameter "%0:s", the parameter value is %1:s tha' +
  'n the %2:s bound for the parameter.';
  StrLess = 'less';
  StrGreater = 'greater';
  StrLower = 'lower';
  StrUpper = 'upper';
  StrInvalidPESTVariabl = 'Invalid PEST variable value';
  Str0sInThePESTPre = '%0:s in the PEST Predictive Analysis section must be ' +
  'greater than 0 but has a value of %1:g.';
  Str0sInThePESTPre2 = '%0:s in the PEST Predictive Analysis section must be' +
  ' greater than %1:s 0 but they have values of %2:g and %3:g.';
  StrPredictionObservati = 'Prediction observation specified incorrectly';
  StrIfThePredictionAn = 'If the prediction analysis mode is used in PEST, t' +
  'here must be exactly one observation assigned to the "predict" observatio' +
  'n group. In this model there were %d such observation assigned to the "pr' +
  'edict" group.';
  StrTooManyParameters = 'Too many parameters subject to absolute change lim' +
  'its.';
  StrPESTAllowsAMaximu = 'PEST allows a maximum of 10 parameters to be subje' +
  'ct to absolute change limits (ABSPARMAX). You specified absolute change l' +
  'imits in %d parameters. The remainder will be skipped.';
  StrInvalidPESTDelimit = 'Invalid PEST delimiter';
  StrUseOfAsADeli = 'Use of "@" as a delimiter will cause errors with SUTRA ' +
  'models. Fix this is the PEST Properties dialog box.';
  StrObservationGroupTa = 'Observation group targets must be greater than ze' +
  'ro.';
  StrTheGroupTargetFor = 'The group target for the %s observation group is l' +
  'ess than or equal to zero.';
  StrEquationsForVert = '# Equations for vertical continuity between layers ' +
  '%0:d and %1:d for parameter %2:s in data set %3:s.';
  StrEquationsForHori = '# Equations for horizontal continuity in layers %0:' +
  'd for parameter %1:s.';
  StrEquationForIniti = '# Equation for initial value for parameter %0:s.';
  StrTheSearchDistance = 'The search distance for within-layer continuity pr' +
  'ior information equations is zero.';
  StrToDefinePriorinfo = 'To define prior-information equations for within-l' +
  'ayer continuity, the search distance musts be large enough that every pil' +
  'ot point has at least one neighbor that is within the search distance.';

{ TPestControlFileWriter }


constructor TPestControlFileWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FUsedObservations := TObservationInterfaceList.Create;
  F_AbsParMax:= TRealList.Create;
  F_AbsParMax.Sorted := True;
  FPriorInfomationEquations := TStringList.Create;
end;

destructor TPestControlFileWriter.Destroy;
begin
  FPriorInfomationEquations.Free;
  F_AbsParMax.Free;
  FUsedObservations.Free;
  inherited;
end;

class function TPestControlFileWriter.Extension: string;
begin
  result := '.pst';
end;

function TPestControlFileWriter.NumberOfObservationGroups: Integer;
begin
  result := Model.PestProperties.ObservationGroups.Count
    + Model.PestProperties.PriorInfoObservationGroups.Count;
  if (result = 0) and (Model.PestProperties.PestStatus = psActive) then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoObservationGroup,
      StrNoPESTObservation);
  end;
end;

function TPestControlFileWriter.NumberOfObservations: integer;
var
  TempList: TObservationInterfaceList;
  ObsIndex: Integer;
  AnObs: TCustomObservationItem;
  IObs: IObservationItem;
  ObservationGroups: TPestObservationGroups;
  ObsGroup: TPestObservationGroup;
begin
  ObservationGroups := frmGoPhast.PhastModel.PestProperties.ObservationGroups;
  TempList := TObservationInterfaceList.Create;
  try
    frmGoPhast.PhastModel.FillObsInterfaceItemList(TempList, True);
    FUsedObservations.Capacity := TempList.Count;
    for ObsIndex := 0 to TempList.Count - 1 do
    begin
      IObs := TempList[ObsIndex];
      if IObs is TCustomObservationItem then
      begin
        AnObs := TCustomObservationItem(IObs);
        if AnObs.Print then
        begin
          FUsedObservations.Add(IObs);
          ObsGroup := ObservationGroups.GetObsGroupByName(IObs.ObservationGroup);
          if ObsGroup = nil then
          begin
            ObservationGroups.Add.ObsGroupName := IObs.ObservationGroup;
          end;
        end;
      end
      else
      begin
        FUsedObservations.Add(IObs);
        ObsGroup := ObservationGroups.GetObsGroupByName(IObs.ObservationGroup);
        if ObsGroup = nil then
        begin
          ObservationGroups.Add.ObsGroupName := IObs.ObservationGroup;
        end;
      end;
    end;
    result := FUsedObservations.Count;
    if result = 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrNoObservationsDefi,
        StrNoObservationsHave);
    end;
  finally
    TempList.Free;
  end;
end;

function TPestControlFileWriter.NumberOfParameterGroups: Integer;
begin
  result := Model.ParamGroups.Count;
  if result = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoParameterGroups,
      StrNoPESTParameterGr);
  end;
end;

function TPestControlFileWriter.NumberOfParameters: Integer;
var
  ParamIndex: Integer;
  AParam: TModflowParameter;
  UsedTypes: TParameterTypes;
  index: Integer;
  ASteadyParam: TModflowSteadyParameter;
begin
  result := 0;

  UsedTypes := [];
  if Model.ModelSelection = msModflow2015 then
  begin
    UsedTypes := Mf15ParamType;
  end
  else if Model.ModelSelection in Modflow2005Selection then
  begin
    UsedTypes := Mf2005ParamType
  end
  else if Model.ModelSelection in SutraSelection then
  begin
    UsedTypes := SutraParamType
  end
  else
  begin
    Assert(False);
  end;

  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
  begin
    ASteadyParam := Model.ModflowSteadyParameters[ParamIndex];
    if (ASteadyParam.ParameterType in UsedTypes)
      and (ASteadyParam.UsedDirectly or (ASteadyParam.ParameterType <> ptPEST))
    { and not ASteadyParam.UsePilotPoints} then
    begin
      Inc(result);
    end;
  end;

  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    AParam := Model.ModflowTransientParameters[ParamIndex];
    if AParam.ParameterType in UsedTypes then
    begin
      Inc(result);
    end;
  end;

  for ParamIndex := 0 to Model.HufParameters.Count - 1 do
  begin
    AParam := Model.HufParameters[ParamIndex];
    if AParam.ParameterType in UsedTypes then
    begin
      Inc(result);
    end;
  end;

  for index := 0 to Model.PilotPointData.Count - 1 do
  begin
    result := result + Model.PilotPointData[index].Count;
  end;

  if result = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrNoParametersHaveB,
      StrNoPestParameters)
  end;
end;

function TPestControlFileWriter.NumberOfPriorInformation: Integer;
const
  EquationRoot = 'eq_';
var
  UsedTypes: TParameterTypes;
  PilotPointParameters: TStringList;
  ParamIndex: Integer;
  AParam: TModflowParameter;
  index: Integer;
  PilotPointItem: TStoredPilotParamDataItem;
  ParameterIndex: Integer;
  PilotParamName: string;
  EquationCount: Integer;
  ObservationGroupNames: TStringList;
  ObsGroupIndex: Integer;
  ObservationGroups: TPestObservationGroups;
  ObsGroup: TPestObservationGroup;
  LocationQuadTree: TRbwQuadTree;
  LinkQuadQuadTree: TRbwQuadTree;
  DisLimits: TGridLimit;
  ASteadyParam: TModflowSteadyParameter;
  SearchDistance: Double;
  MaxPilotPointsInRange: Integer;
  PilotPointItemSortList: TList<TStoredPilotParamDataItem>;
  ItemIndex: Integer;
  PilotPointItem1: TStoredPilotParamDataItem;
  PilotPointItem2: TStoredPilotParamDataItem;
  CommentCount: Integer;
  OldDecimalSeparator: Char;
  function NewGroupName: string;
  var
    Index: Integer;
  begin
    Index := ObservationGroups.Count +1;
    result := 'Grp' + IntToStr(Index);
    While ObservationGroupNames.IndexOf(result) >= 0 do
    begin
      Inc(Index);
      result := 'Grp' + IntToStr(Index);
    end;
  end;
  procedure WriteVerticalContinuityEquation(
    PPItem1, PPItem2: TStoredPilotParamDataItem; AParam: TModflowSteadyParameter);
  var
    PPIndex: Integer;
    PilotPoint: TPointItem;
    OtherPP: TPointItem;
    EquationName: string;
    ObsGroupName: string;
    ParameterName1: string;
    ParameterName2: string;
    Equation: string;
  begin
    FPriorInfomationEquations.Add(Format(StrEquationsForVert,
      [PPItem1.Layer + 1, PPItem2.Layer +1, AParam.ParameterName, PPItem1.DataArrayName]));
    Inc(CommentCount);
    ObsGroupName := '';
    LocationQuadTree.Clear;
    for PPIndex := 0 to PPItem1.Points.Count - 1 do
    begin
      PilotPoint := PPItem1.Points[PPIndex];
      LocationQuadTree.AddPoint(PilotPoint.X, PilotPoint.Y, PilotPoint);
    end;
    for PPIndex := 0 to PPItem2.Points.Count - 1 do
    begin
      PilotPoint := PPItem2.Points[PPIndex];
      OtherPP := LocationQuadTree.NearestPointsFirstData(PilotPoint.X, PilotPoint.Y);
      if (PilotPoint.X <> OtherPP.X) or (PilotPoint.Y <> OtherPP.Y) then
      begin
        Continue;
      end;

      ParameterName1 := PPItem1.ParameterName(OtherPP.Index+1);
      ParameterName2 := PPItem2.ParameterName(PilotPoint.Index+1);
//      Value1 := PPItem1.Values[OtherPP.Index].Value;
//      Value2 := PPItem2.Values[PilotPoint.Index].Value;

      Inc(EquationCount);
      EquationName := EquationRoot + IntToStr(EquationCount);

      if ObsGroupName = '' then
      begin
        if AParam.VertSpatialContinuityGroupName = '' then
        begin
          AParam.VertSpatialContinuityGroupName := NewGroupName;
        end;
        ObsGroupIndex := ObservationGroupNames.IndexOf(
          AParam.VertSpatialContinuityGroupName);
        if ObsGroupIndex < 0 then
        begin
          ObsGroup := ObservationGroups.Add;
          ObsGroup.ObsGroupName := AParam.VertSpatialContinuityGroupName;
          ObsGroup.IsRegularizationGroup := True;
          ObservationGroupNames.AddObject(ObsGroup.ObsGroupName, ObsGroup);
        end
        else
        begin
          ObsGroup := ObservationGroupNames.Objects[ObsGroupIndex]
            as TPestObservationGroup;
        end;
        if ObsGroup.IsRegularizationGroup then
        begin
          ObsGroupName := strRegul + Copy(ObsGroup.ObsGroupName, 1,
            AllowableGroupNameLength - Length(strRegul));
        end
        else
        begin
          ObsGroupName := ObsGroup.ObsGroupName;
        end;
      end;

      if AParam.Transform = ptLog then
      begin
        Equation := Format('1.0 * log(%0:s) - 1.0 * log(%1:s) = 0.0', [ParameterName1, ParameterName2]);
      end
      else
      begin
        Equation := Format('1.0 * %0:s - 1.0 * %1:s = 0.0', [ParameterName1, ParameterName2]);
      end;
      FPriorInfomationEquations.Add(Format(' %0:s          %1:s       %2:g     %3:s',
        [EquationName, Equation,
        AParam.VertSpatialContinuityPriorInfoWeight, ObsGroupName]));
    end;
  end;
  procedure WriteHorizontalContinuityEquations(Param: TModflowSteadyParameter;
    PPItem: TStoredPilotParamDataItem);
  var
    PPIndex: Integer;
    PilotPoint: TPointItem;
    Points: TQuadPointInRegionArray;
    NeighborIndex: Integer;
    Neighbors: TList<TPointItem>;
    TestDistance: double;
    NeighborPoint: TPointItem;
    PointDistance: double;
    X: double;
    Y: double;
    XFloat: double;
    YFloat: Double;
    Data: TPointerArray;
    EquationName: string;
    ObsGroupName: string;
    Equation: string;
    ParameterName1: string;
    ParameterName2: string;
//    X: TPoint3D;
  begin
    if SearchDistance <= 0 then
    begin
      frmErrorsAndWarnings.AddError(Model, StrTheSearchDistance,
        StrToDefinePriorinfo)
    end;
    FPriorInfomationEquations.Add(Format(StrEquationsForHori,
      [PPItem.Layer + 1, Param.ParameterName]));
    Inc(CommentCount);
    LinkQuadQuadTree.Clear;
    LocationQuadTree.Clear;
    Neighbors := TList<TPointItem>.Create;
    try
      for PPIndex := 0 to PPItem.Points.Count - 1 do
      begin
        PilotPoint := PPItem.Points[PPIndex];
        LocationQuadTree.AddPoint(PilotPoint.X, PilotPoint.Y, PilotPoint);
      end;
      for PPIndex := 0 to PPItem.Points.Count - 1 do
      begin
        ParameterName1 := PilotPointItem.ParameterName(PPIndex+1);

        PilotPoint := PPItem.Points[PPIndex];
        LocationQuadTree.FindPointsInCircle(PilotPoint.X, PilotPoint.Y,
          SearchDistance, Points, True);
        Assert(Points[0].Data[0] = PilotPoint);
        Neighbors.Clear;
        if Length(Points) > 1 then
        begin
          NeighborPoint := Points[1].Data[0];
          TestDistance := Distance(PilotPoint.Point2D, NeighborPoint.Point2D);
          for NeighborIndex := 1 to Length(Points) - 1 do
          begin
            NeighborPoint := Points[NeighborIndex].Data[0];
            PointDistance := Distance(PilotPoint.Point2D, NeighborPoint.Point2D);
            if (Neighbors.Count >= MaxPilotPointsInRange) and (PointDistance > TestDistance) then
            begin
              break;
            end;
            TestDistance := PointDistance;
            Neighbors.Add(NeighborPoint);
          end;
        end;
        for NeighborIndex := 0 to Neighbors.Count - 1 do
        begin
          NeighborPoint := Neighbors[NeighborIndex];
          X := System.Math.Min(PilotPoint.Index, NeighborPoint.Index);
          Y := System.Math.Max(PilotPoint.Index, NeighborPoint.Index);
          if LinkQuadQuadTree.Count = 0 then
          begin
            LinkQuadQuadTree.AddPoint(X, Y, nil);
          end
          else
          begin
            XFloat := X;
            YFloat := Y;
            LinkQuadQuadTree.FindClosestPointsData(XFloat, YFloat, Data);
            if (XFloat = X) and (YFloat = Y) then
            begin
              Continue
            end;
            LinkQuadQuadTree.AddPoint(X, Y, nil);
          end;
          Inc(EquationCount);
          EquationName := EquationRoot + IntToStr(EquationCount);
          if Param.HorizontalSpatialContinuityGroupName = '' then
          begin
            Param.HorizontalSpatialContinuityGroupName := NewGroupName;
          end;
          ObsGroupIndex := ObservationGroupNames.IndexOf(
            Param.HorizontalSpatialContinuityGroupName);
          if ObsGroupIndex < 0 then
          begin
            ObsGroup := ObservationGroups.Add;
            ObsGroup.ObsGroupName := Param.HorizontalSpatialContinuityGroupName;
            ObsGroup.IsRegularizationGroup := True;
            ObservationGroupNames.AddObject(ObsGroup.ObsGroupName, ObsGroup);
          end
          else
          begin
            ObsGroup := ObservationGroupNames.Objects[ObsGroupIndex]
              as TPestObservationGroup;
          end;
          if ObsGroup.IsRegularizationGroup then
          begin
            ObsGroupName := strRegul + Copy(ObsGroup.ObsGroupName, 1,
              AllowableGroupNameLength - Length(strRegul));
          end
          else
          begin
            ObsGroupName := ObsGroup.ObsGroupName;
          end;
          ParameterName2 := PilotPointItem.ParameterName(NeighborPoint.Index+1);
          if Param.Transform = ptLog then
          begin
            Equation := Format('1.0 * log(%0:s) - 1.0 * log(%1:s) = 0.0', [ParameterName1, ParameterName2]);
          end
          else
          begin
            Equation := Format('1.0 * %0:s - 1.0 * %1:s = 0.0', [ParameterName1, ParameterName2]);
          end;
          FPriorInfomationEquations.Add(Format(' %0:s          %1:s       %2:g     %3:s',
            [EquationName, Equation, Param.HorizontalSpatialContinuityPriorInfoWeight, ObsGroupName]));
        end;
      end;
    finally
      Neighbors.Free;
    end;
  end;
  procedure WriteInitialValueEquation(Param: TModflowParameter; InitialValue: double;
    ParameterName: string; PilotPointItem: TStoredPilotParamDataItem);
  var
    EquationName: string;
    ObsGroupName: string;
    Equation: string;
    IsPilotPoint: Boolean;
    GroupName: string;
  begin
    if not Model.PestProperties.UseInitialValuePriorInfo then
    begin
      Exit;
    end;
    if ParameterName = '' then
    begin
      ParameterName := Param.ParameterName;
    end;
    FPriorInfomationEquations.Add(Format(StrEquationForIniti,
      [ParameterName]));
    Inc(CommentCount);
    IsPilotPoint := PilotPointItem <> nil;
    Inc(EquationCount);
    EquationName := EquationRoot + IntToStr(EquationCount);
    if IsPilotPoint then
    begin
      GroupName := Param.PilotPointObsGrpCollection.
        GetGroupNameByLayerAndFamily(PilotPointItem.Layer, PilotPointItem.ParamFamily);
      if GroupName = '' then
      begin
        GroupName := NewGroupName;
      end;
      ObsGroupIndex := ObservationGroupNames.IndexOf(GroupName);
    end
    else
    begin
      if Param.RegularizationGroup = '' then
      begin
        Param.RegularizationGroup := NewGroupName;
      end;
      ObsGroupIndex := ObservationGroupNames.IndexOf(Param.RegularizationGroup);
    end;
    if ObsGroupIndex < 0 then
    begin
      ObsGroup := ObservationGroups.Add;
      if IsPilotPoint then
      begin
        ObsGroup.ObsGroupName := GroupName;
        ObsGroup.RelativCorrelationFileName :=
          ChangeFileExt(PilotPointItem.FileName, StrCov);
        Param.PilotPointObsGrpCollection.
          SetGroupNameByLayerAndFamily(PilotPointItem.Layer, PilotPointItem.ParamFamily, GroupName);
      end
      else
      begin
        ObsGroup.ObsGroupName := Param.RegularizationGroup;
      end;
      ObsGroup.IsRegularizationGroup := True;
      ObservationGroupNames.AddObject(ObsGroup.ObsGroupName, ObsGroup);
    end
    else
    begin
      ObsGroup := ObservationGroupNames.Objects[ObsGroupIndex] as TPestObservationGroup;
    end;
    if ObsGroup.IsRegularizationGroup then
    begin
      ObsGroupName := strRegul + Copy(ObsGroup.ObsGroupName, 1,
        AllowableGroupNameLength - Length(strRegul))
    end
    else
    begin
      ObsGroupName := ObsGroup.ObsGroupName;
    end;
    if Param.Transform = ptLog then
    begin
      Equation := Format('1.0 * log(%0:s) = %1:g', [ParameterName, Log10(InitialValue)]);
    end
    else
    begin
      Equation := Format('1.0 * %0:s = %1:g', [ParameterName, InitialValue]);
    end;
    FPriorInfomationEquations.Add(Format(' %0:s          %1:s       %2:g     %3:s',
      [EquationName, Equation, Param.InitialValuePriorInfoWeight, ObsGroupName]));
  end;
begin
  CommentCount := 0;
  FPriorInfomationEquations.Clear;
  if not Model.PestProperties.UseInitialValuePriorInfo
    and not Model.PestProperties.UseHorizontalSpatialContinuityPriorInfo
    and not Model.PestProperties.UseVertSpatialContinuityPriorInfo then
  begin
    result := 0;
    Exit;
  end;
  GetUsedTypes(UsedTypes);

  EquationCount := 0;
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  LinkQuadQuadTree := TRbwQuadTree.Create(nil);
  LocationQuadTree := TRbwQuadTree.Create(nil);
  ObservationGroupNames := TStringList.Create;
  try
    DisLimits := Model.DiscretizationLimits(vdTop);
    LocationQuadTree.XMax := DisLimits.MaxX;
    LocationQuadTree.XMin := DisLimits.MinX;
    LocationQuadTree.YMax := DisLimits.MaxY;
    LocationQuadTree.YMin := DisLimits.MinY;
    LocationQuadTree.MaxPoints := Model.PestProperties.MaxPilotPointsInRange;

    LinkQuadQuadTree.XMax := Model.PilotPointCount;
    LinkQuadQuadTree.XMin := 0;
    LinkQuadQuadTree.YMax := LinkQuadQuadTree.XMax;
    LinkQuadQuadTree.YMin := 0;
    LinkQuadQuadTree.MaxPoints := Model.PestProperties.MaxPilotPointsInRange;

    ObservationGroupNames.Sorted := True;
    ObservationGroupNames.Duplicates := dupIgnore;
    ObservationGroupNames.CaseSensitive := False;
    ObservationGroups := Model.PestProperties.PriorInfoObservationGroups;

    SearchDistance := Model.PestProperties.SeachDistance;
    MaxPilotPointsInRange := Model.PestProperties.MaxPilotPointsInRange;
    for ObsGroupIndex := 0 to ObservationGroups.Count - 1 do
    begin
      ObsGroup := ObservationGroups[ObsGroupIndex];
      ObservationGroupNames.AddObject(ObsGroup.ObsGroupName, ObsGroup);
    end;

    PilotPointParameters := TStringList.Create;
    try
      FUsePval := False;
      for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
      begin
        ASteadyParam := Model.ModflowSteadyParameters[ParamIndex];
        if ASteadyParam.ParameterType in UsedTypes then
        begin
          if (ASteadyParam.Transform in [ptNoTransform, ptLog]) then
          begin
            if ASteadyParam.UseInitialValuePriorInfo
              and (ASteadyParam.UsedDirectly or (ASteadyParam.ParameterType <> ptPEST)) then
            begin
              WriteInitialValueEquation(ASteadyParam, ASteadyParam.Value, '', nil);
            end;

            if ASteadyParam.UsePilotPoints then
            begin
              PilotPointParameters.AddObject(ASteadyParam.ParameterName, ASteadyParam);
            end;
          end;
        end;
      end;
      PilotPointParameters.CaseSensitive := False;
      PilotPointParameters.Sorted := True;

      for index := 0 to Model.PilotPointData.Count - 1 do
      begin
        PilotPointItem := Model.PilotPointData[index];
        ParamIndex := PilotPointParameters.IndexOf(PilotPointItem.BaseParamName);
        if ParamIndex >= 0 then
        begin
          ASteadyParam := PilotPointParameters.Objects[ParamIndex] as TModflowSteadyParameter;
          if ASteadyParam.UseInitialValuePriorInfo then
          begin
            for ParameterIndex := 1 to PilotPointItem.Count do
            begin
              PilotParamName := PilotPointItem.ParameterName(ParameterIndex);
              WriteInitialValueEquation(ASteadyParam,
                PilotPointItem.Values[ParameterIndex-1].Value, PilotParamName,
                PilotPointItem);
            end;
          end;

          if Model.PestProperties.UseHorizontalSpatialContinuityPriorInfo
            and ASteadyParam.UseHorizontalSpatialContinuityPriorInfo then
          begin
            WriteHorizontalContinuityEquations(ASteadyParam, PilotPointItem)
            // Write Spatial Continuity equations
          end;
        end
        else
        begin
          frmErrorsAndWarnings.AddError(Model, StrParameterUndefined,
            Format(StrPilotPointsWereDe, [PilotPointItem.BaseParamName]));
        end;
      end;

      if Model.PestProperties.UseVertSpatialContinuityPriorInfo
        and (Model.LayerCount > 1) then
      begin
        PilotPointItemSortList := TList<TStoredPilotParamDataItem>.Create;
        try
          for index := 0 to Model.PilotPointData.Count - 1 do
          begin
            PilotPointItem := Model.PilotPointData[index];
            PilotPointItemSortList.Add(PilotPointItem);
          end;
          PilotPointItemSortList.Sort(
            TComparer<TStoredPilotParamDataItem>.Construct(
              function(const Left, Right: TStoredPilotParamDataItem): Integer
              begin
                Result := CompareStr( Left.BaseParamName, Right.BaseParamName);
                if Result = 0 then
                begin
                  Result := CompareStr( Left.DataArrayName, Right.DataArrayName);
                  if result = 0 then
                  begin
                    result := Left.Layer - Right.Layer;
                  end;
                end;
              end
            ));
            for ItemIndex := 0 to PilotPointItemSortList.Count - 2 do
            begin
              PilotPointItem1 := PilotPointItemSortList[ItemIndex];
              PilotPointItem2 := PilotPointItemSortList[ItemIndex+1];
              if (PilotPointItem1.Points.Count = 0) or (PilotPointItem2.Points.Count = 0) then
              begin
                Continue;
              end;
              if
                (PilotPointItem1.BaseParamName = PilotPointItem2.BaseParamName)
                and (PilotPointItem1.DataArrayName = PilotPointItem2.DataArrayName)
                and (Abs(PilotPointItem1.Layer - PilotPointItem2.Layer) = 1)
                then
              begin
                ParamIndex := PilotPointParameters.IndexOf(
                  PilotPointItem1.BaseParamName);
                if ParamIndex >= 0 then
                begin
                  ASteadyParam := PilotPointParameters.Objects[ParamIndex]
                    as TModflowSteadyParameter;
                  if ASteadyParam.UseVertSpatialContinuityPriorInfo then
                  begin
                    WriteVerticalContinuityEquation(PilotPointItem1,
                      PilotPointItem2, ASteadyParam);
                  end;
                end
                else
                begin
                  frmErrorsAndWarnings.AddError(Model, StrParameterUndefined,
                    Format(StrPilotPointsWereDe, [PilotPointItem.BaseParamName]));
                end;
              end;
            end;
          finally
          PilotPointItemSortList.Free;
        end;
      end;
    finally
      PilotPointParameters.Free;
    end;

    if Model.PestProperties.UseInitialValuePriorInfo then
    begin
      for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
      begin
        AParam := Model.ModflowTransientParameters[ParamIndex];
        if AParam.ParameterType in UsedTypes then
        begin
          if AParam.UseInitialValuePriorInfo
            and (AParam.Transform in [ptNoTransform, ptLog]) then
          begin
            WriteInitialValueEquation(AParam, AParam.Value, '', nil);
          end;
        end;
      end;

      for ParamIndex := 0 to Model.HufParameters.Count - 1 do
      begin
        AParam := Model.HufParameters[ParamIndex];
        if AParam.ParameterType in UsedTypes then
        begin
          if AParam.UseInitialValuePriorInfo
            and (AParam.Transform in [ptNoTransform, ptLog]) then
          begin
            WriteInitialValueEquation(AParam, AParam.Value, '', nil);
          end;
        end;
      end;
    end;

  finally
    ObservationGroupNames.Free;
    LocationQuadTree.Free;
    LinkQuadQuadTree.Free;
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
  // prior information will be added by the running ADDREG1 or a program in the
  // Groundwater Utility suite,
  result := FPriorInfomationEquations.Count - CommentCount;

end;

function TPestControlFileWriter.NumberOfTemplateFiles: Integer;
var
  DSIndex: Integer;
  ADataArray: TDataArray;
  UsedTypes: TParameterTypes;
  ParamIndex: Integer;
  AParam: TModflowSteadyParameter;
  AParam2: TModflowParameter;
begin
  result := 0;
  GetUsedTypes(UsedTypes);
  // template for PVAL file
  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
  begin
    AParam := Model.ModflowSteadyParameters[ParamIndex];
    if (AParam.ParameterType in UsedTypes)
      and (AParam.UsedDirectly or (AParam.ParameterType <> ptPEST)) then
    begin
      result := 1;
      break;
    end;
  end;

  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    AParam2 := Model.ModflowTransientParameters[ParamIndex];
    if AParam2.ParameterType in UsedTypes then
    begin
      result := 1;
      break;
    end;
  end;

  for ParamIndex := 0 to Model.HufParameters.Count - 1 do
  begin
    AParam2 := Model.HufParameters[ParamIndex];
    if AParam2.ParameterType in UsedTypes then
    begin
      result := 1;
      break;
    end;
  end;


  // PVAL file;
  if Model.ModelSelection in SutraSelection then
  begin
    result := result + Model.SutraPestScripts.Count;
  end;
//  else
  begin
    for DSIndex := 0 to Model.DataArrayManager.DataSetCount - 1 do
    begin
      ADataArray := Model.DataArrayManager[DSIndex];
      if ADataArray.PestParametersUsed and ADataArray.TemplateNeeded then
      begin
        Inc(result);
      end;
    end;
  end;
  result := result + Model.PilotPointData.Count
end;

procedure TPestControlFileWriter.WriteAutomaticUserIntervention;
begin
// The Automatic User Intervention is not currently supported.
end;

procedure TPestControlFileWriter.WriteCommandLine;
begin
  WriteSectionHeader('model command line');
  WriteString(StrRunModelBat);
  NewLine;
  NewLine;
end;

procedure TPestControlFileWriter.WriteControlSection(SetNOPTMAX: Boolean = False);
var
  PestControlData: TPestControlData;
  NINSFLE: Integer;
//  ParIndex: Integer;
  UsedTypes: TParameterTypes;
  ParamIndex: Integer;
  AParam: TModflowParameter;
begin
  PestControlData := Model.PestProperties.PestControlData;
  // First line 4.2.2.
  WriteSectionHeader('control data');

  {$REGION 'second line 4.2.3'}
  // second line 4.2.3
  case PestControlData.PestRestart of
    prNoRestart:
      begin
        WriteString('norestart ');
      end;
    prRestart:
      begin
        WriteString('restart ');
      end;
    else
      Assert(False);
  end;
  case PestControlData.PestMode of
    pmEstimation:
      begin
        WriteString('estimation ');
      end;
    pmPrediction:
      begin
        WriteString('prediction ');
      end;
    pmRegularisation:
      begin
        WriteString('regularisation ');
      end;
    pmPareto:
      begin
        WriteString('pareto ');
      end;
    else
      Assert(False);
  end;
  WriteString('# RSTFLE PESTMODE');
  NewLine;
  {$ENDREGION}

  {$REGION 'third line 4.2.4'}
  // third line 4.2.4
  // NPAR
  WriteInteger(NumberOfParameters);
  // NOBS
  WriteInteger(NumberOfObservations);
  // NPARGP
  WriteInteger(NumberOfParameterGroups);
  // NPRIOR
  WriteInteger(NumberOfPriorInformation);
  // NOBSGP
  WriteInteger(NumberOfObservationGroups);
  // MAXCOMPRDIM
  WriteInteger(PestControlData.MaxCompressionDimension);
  if PestControlData.MaxCompressionDimension > 1 then
  begin
    // DERZEROLIM
    WriteFloat(PestControlData.ZeroLimit);
  end;

  WriteString(' # NPAR NOBS NPARGP, NPRIOR NOBSGP, MAXCOMPRDIM');
  if PestControlData.MaxCompressionDimension > 1 then
  begin
    WriteString(', DERZEROLIM');
  end;
  NewLine;
  {$ENDREGION}

  {$REGION 'fourth line 4.2.5'}
  // NTPLFLE NINSFLE PRECIS DPOINT [NUMCOM JACFILE MESSFILE] [OBSREREF]
  // fourth line 4.2.5
  // NTPLFLE
  WriteInteger(NumberOfTemplateFiles);
  // NINSFLE
  NINSFLE := 0;
  case Model.MOdelSelection of
    msUndefined, msPhast, msFootPrint:
      Assert(False);
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp,
      msModflowCfp, msModflowOwhm2:
      begin
        NINSFLE := 1;
      end;
    msSutra22, msSutra30, msSutra40:
      begin
        NINSFLE := 1;
      end;
    msModflow2015:
      begin
        NINSFLE := 1;
      end;
    else
      Assert(False);
  end;
  NINSFLE := NINSFLE + Model.InputObsInstructionFiles.Count;
  // PEST will always read all the simulated values from one file.
  WriteInteger(NINSFLE);
  // PRECIS
  // All data will be writen in double precision
  WriteString(' double');
  // DPOINT
  // The decimal point is never needed because free format is used exclusively.
  WriteString(' nopoint');

  // NUMCOM, JCFILE, and MESSFILE will be omited in all cases because
  // the supported versions of MODFLOW and SUTRA don't write derivatives.
  // MODFLOW-2000 can write derivatives but it isn't supported.

  // OBSREREF
  // observation rereferencing is not supported in ModelMuse.
  WriteString(' noobsreref');

  WriteString(' # NTPLFLE, NINSFLE, PRECIS, DPOINT, OBSREREF');
  NewLine;
  {$ENDREGION}

  {$REGION 'fifth line 4.2.6'}
  // Fifth line 4.2.6
  // RLAMBDA1
  WriteFloat(PestControlData.InitalLambda);
  // RLAMFAC
  WriteFloat(PestControlData.LambdaAdjustmentFactor);
  // PHIRATSUF
  WriteFloat(PestControlData.PhiRatioSufficient);
  // PHIREDLAM
  WriteFloat(PestControlData.PhiReductionLambda);
  // NUMLAM
  WriteInteger(PestControlData.NumberOfLambdas);
  // JACUPDATE
  WriteInteger(PestControlData.JacobianUpdate);
  // LAMFORGIVE
  case PestControlData.LambdaForgive of
    lfForgive:
      begin
        if (PestControlData.PestMode = pmPrediction) then
        begin
          WriteString(' nolamforgive');
        end
        else
        begin
          WriteString(' lamforgive');
        end;
      end;
    lfNoForgive:
      begin
        WriteString(' nolamforgive');
      end;
    else
      Assert(False);
  end;
  // DERFORGIVE
  case PestControlData.DerivedForgive of
    dfForgive:
      begin
        WriteString(' derforgive');
      end;
    dNoForgive:
      begin
        WriteString(' noderforgive');
      end;
    else
      Assert(False);
  end;

  WriteString(' # RLAMBDA1, RLAMFAC, PHIRATSUF, PHIREDLAM, NUMLAM, JACUPDATE, LAMFORGIVE, DERFORGIVE');
  NewLine;
  {$ENDREGION}

  {$REGION 'sixth line 4.2.7'}
  // sixth line 4.2.7
  //RELPARMAX
  WriteFloat(PestControlData.RelativeMaxParamChange);
  //FACPARMAX
  WriteFloat(PestControlData.FactorMaxParamChange);
  // ABSPARMAX(N)
  GetUsedTypes(UsedTypes);

  for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
  begin
    AParam := Model.ModflowSteadyParameters[ParamIndex];
    if (AParam.ParameterType in UsedTypes) and
      (AParam.ChangeLimitation = pclAbsolute) then
    begin
      F_AbsParMax.AddUnique(AParam.AbsoluteN);
    end;
  end;

  for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
  begin
    AParam := Model.ModflowTransientParameters[ParamIndex];
    if (AParam.ParameterType in UsedTypes) and
      (AParam.ChangeLimitation = pclAbsolute) then
    begin
      F_AbsParMax.AddUnique(AParam.AbsoluteN);
    end;
  end;

  for ParamIndex := 0 to Model.HufParameters.Count - 1 do
  begin
    AParam := Model.HufParameters[ParamIndex];
    if (AParam.ParameterType in UsedTypes) and
      (AParam.ChangeLimitation = pclAbsolute) then
    begin
      F_AbsParMax.AddUnique(AParam.AbsoluteN);
    end;
  end;

  for ParamIndex := 0 to Min(10, F_AbsParMax.Count) - 1 do
  begin
    WriteString(Format(' absparmax(%0:d)=%1:g',
      [ParamIndex + 1,F_AbsParMax[ParamIndex]]));
  end;

  if F_AbsParMax.Count > 10 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrTooManyParameters,
      Format(StrPESTAllowsAMaximu, [F_AbsParMax.Count]));
  end;

  //FACORIG
  WriteFloat(PestControlData.FactorOriginal);
  // IBOUNDSTICK
  WriteInteger(Ord(PestControlData.BoundStick));
  // UPVECBEND
  WriteInteger(Ord(PestControlData.UpgradeParamVectorBending));
  // ABSPARMAX is not currently supported by ModelMuse.

  WriteString(' # RELPARMAX, FACPARMAX, FACORIG, IBOUNDSTICK, UPVECBEND');
  NewLine;
  {$ENDREGION}

  {$REGION 'seventh line 4.2.8'}
  // seventh line 4.2.8
  // PHIREDSWH
  WriteFloat(PestControlData.SwitchCriterion);
  // NOPTSWITCH
  WriteInteger(PestControlData.OptSwitchCount);
  // SPLITSWH
  WriteFloat(PestControlData.SplitSlopeCriterion);

  // The Automatic user intervention section is not supported
  // so PEST will only use default values.
  // DOAUI
  case PestControlData.AutomaticUserIntervation of
    auiInactive:
      begin
        WriteString(' noaui');
      end;
    auiActiveLeastSensitiveFirst:
      begin
        WriteString(' aui');
      end;
    auiMostSensitiveFirst:
      begin
        WriteString(' auid');
      end;
    else
      Assert(False);
  end;

  // DOSENREUSE
  case PestControlData.SensitivityReuse of
    srNoReuse:
      begin
        WriteString(' nosenreuse');
      end;
    srReuse:
      begin
        WriteString(' senreuse');
      end;
    else
      Assert(False);
  end;
  // BOUNDSCALE
  if (PestControlData.Boundscaling = bsBoundsScaling)
    and (PestControlData.PestMode <> pmPrediction) then
  begin
    WriteString(' boundscale');
  end
  else
  begin
    WriteString(' noboundscale');
  end;

  WriteString(' # PHIREDSWH, NOPTSWITCH, SPLITSWH, DOAUI, DOSENREUSE, BOUNDSCALE');
  NewLine;
  {$ENDREGION}

  {$REGION 'eighth line 4.2.9'}
  // eighth line 4.2.9
  // NOPTMAX
  if SetNOPTMAX then
  begin
    WriteInteger(-2);
  end
  else
  begin
    WriteInteger(PestControlData.MaxIterations);
  end;

  // PHIREDSTP
  WriteFloat(PestControlData.SlowConvergenceCriterion);

  // NPHISTP
  WriteInteger(PestControlData.SlowConvergenceCountCriterion);

  // NPHINORED
  WriteInteger(PestControlData.ConvergenceCountCriterion);

  // RELPARSTP
  WriteFloat(PestControlData.ParameterChangeConvergenceCriterion);

  // NRELPAR
  WriteInteger(PestControlData.ParameterChangeConvergenceCount);

  // PHISTOPTHRESH
  if PestControlData.PestMode in [pmPrediction, pmPareto] then
  begin
    WriteFloat(0);
  end
  else
  begin
    WriteFloat(PestControlData.ObjectiveCriterion);
  end;

  // LASTRUN
  if PestControlData.PestMode = pmPareto then
  begin
    WriteInteger(0);
  end
  else
  begin
    WriteInteger(Ord(PestControlData.MakeFinalRun));
  end;

  // PHIABANDON
  WriteFloat(PestControlData.PhiAbandon);

  WriteString(' # NOPTMAX, PHIREDSTP, NPHISTP, NPHINORED, RELPARSTP, NRELPAR, PHISTOPTHRESH, LASTRUN, PHIABANDON');
  NewLine;
  {$ENDREGION}

  {$REGION 'nineth line 4.2.10'}
  // ICOV
  WriteInteger(Ord(PestControlData.WriteCovariance));
  // ICOR
  WriteInteger(Ord(PestControlData.WriteCorrelations));
  // IEIG
  WriteInteger(Ord(PestControlData.WriteEigenVectors));
  // IRES
  WriteInteger(Ord(PestControlData.SaveResolution));
  // JCOSAVE
  case PestControlData.SaveJacobian of
    sjDontSave:
      begin
        WriteString(' nojcosave');
      end;
    sjSave:
      begin
        WriteString(' jcosave');
      end;
    else
      Assert(False);
  end;
  // JCOSAVEITN
  case PestControlData.SaveJacobianIteration of
    sjiDontSave:
      begin
        WriteString(' nojcosaveitn');
      end;
    sjiSave:
      begin
        WriteString(' jcosaveitn');
      end;
    else
      Assert(False);
  end;
  // VERBOSEREC
  case PestControlData.VerboseRecord of
    vrNonVerbose:
      begin
        WriteString(' noverboserec');
      end;
    vrVerbose:
      begin
        WriteString(' verboserec');
      end;
    else
      Assert(False);
  end;
  // RESSAVEITN
  case PestControlData.SaveInterimResiduals of
    sirDontSave:
      begin
        WriteString(' noreisaveitn');
      end;
    sirSave:
      begin
        WriteString(' reisaveitn');
      end;
    else
      Assert(False);
  end;
  // PARSAVEITN
  case PestControlData.SaveParamIteration of
    spiDontSave:
      begin
        WriteString(' noparsaveitn');
      end;
    spiSave:
      begin
        WriteString(' parsaveitn');
      end;
    else
      Assert(False);
  end;
  // PARSAVERUN
  case PestControlData.SaveParamRun of
    sprDontSave:
      begin
        WriteString(' noparsaverun');
      end;
    sprSave:
      begin
        WriteString(' parsaverun');
      end;
    else
      Assert(False);
  end;

  WriteString(' # ICOV, ICOR, IEIG, IRES, JCOSAVE, JCOSAVEITN, VERBOSEREC, RESSAVEITN, PARSAVEITN, PARSAVERUN');
  NewLine;

  {$ENDREGION}
  NewLine;
end;

procedure TPestControlFileWriter.WriteDerivatives;
begin
// The Derivatives is not currently supported.
end;

function TPestControlFileWriter.WriteFile(const AFileName: string; SetNOPTMAX: Boolean = False): string;
var
  PestProperties: TPestProperties;
  ArraysDir: string;
  OldDecimalSeparator: Char;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoParametersHaveB);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoObservationGroup);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrNoParameterGroups);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrParameterGroupName);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrObservationGroupNo);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrParameterUndefined);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrObservationGroupNa);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrObservationGroupUn);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidParameterVa);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrPredictionObservati);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTooManyParameters);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidPESTDelimit);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrObservationGroupTa);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheSearchDistance);


  if not Model.PestUsed then
  begin
    Exit;
  end;

  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    ArraysDir := IncludeTrailingPathDelimiter(ExtractFileDir(AFileName)) + 'arrays';
    if not TDirectory.Exists(ArraysDir) then
    begin
      TDirectory.CreateDirectory(ArraysDir);
    end;

    if Model.ModelSelection in SutraSelection then
    begin
      PestProperties := Model.PestProperties;
      if (PestProperties.TemplateCharacter = '@')
        or (PestProperties.ExtendedTemplateCharacter = '@')
        or (PestProperties.ArrayTemplateCharacter = '@') then
      begin
        frmErrorsAndWarnings.AddError(Model, StrInvalidPESTDelimit,
          StrUseOfAsADeli)
      end;
    end;

    FNameOfFile := FileName(AFileName);
    FInputFileName := FNameOfFile;
    result := FNameOfFile;
    OpenFile(FNameOfFile);
    try
      WriteFirstLine;
      WriteControlSection(SetNOPTMAX);
      // The Sensitivity Reuse Section is not currently supported.
      WriteSensitivityReuse;
      WriteSingularValueDecomposition;
      WriteLsqr;
      // The Automatic User Intervention Section is not currently supported.
      WriteAutomaticUserIntervention;
      // Writing the SVD Assist Section is not currently supported.
      WriteSVD_Assist;
      WriteParameterGroups;
      WriteParameters;
      WriteObservationGroups;
      WriteObservations;
      WriteDerivatives;
      WriteCommandLine;
      WriteModelInputOutput;
      WritePriorInformation;
      WritePredictiveAnalysis;
      WriteRegularisation;
      WritePareto;
    finally
      CloseFile;
    end;
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

procedure TPestControlFileWriter.GetUsedTypes(var UsedTypes: TParameterTypes);
begin
  UsedTypes := [];
  if Model.ModelSelection = msModflow2015 then
  begin
    UsedTypes := Mf15ParamType;
  end
  else if Model.ModelSelection in Modflow2005Selection then
  begin
    UsedTypes := Mf2005ParamType;
  end
  else if Model.ModelSelection in SutraSelection then
  begin
    UsedTypes := SutraParamType;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TPestControlFileWriter.WriteFirstLine;
begin
  WriteString('pcf');
  NewLine;
end;

procedure TPestControlFileWriter.WriteLsqr;
var
  LsqrProperties: TLsqrProperties;
begin
  LsqrProperties := Model.PestProperties.LsqrProperties;
  WriteSectionHeader('lsqr');

  WriteInteger(Ord(LsqrProperties.Mode));
  WriteString(' # LSQRMODE');
  NewLine;

  WriteFloat(LsqrProperties.MatrixTolerance);
  WriteFloat(LsqrProperties.RightHandSideTolerance);
  WriteFloat(LsqrProperties.ConditionNumberLimit);
  if LsqrProperties.MaxIteration <> 0 then
  begin
    WriteInteger(LsqrProperties.MaxIteration);
  end
  else
  begin
    WriteInteger(NumberOfParameters * 4);
  end;
  WriteString(' # LSQR_ATOL LSQR_BTOL LSQR_CONLIM LSQR_ITNLIM');
  NewLine;

  WriteInteger(Ord(LsqrProperties.LsqrWrite));
  WriteString(' # LSQRWRITE');
  NewLine;
  NewLine;
end;

procedure TPestControlFileWriter.WriteModelInputOutput;
var
  TEMPFLE: string;
  INFLE: string;
  INSFLE: string;
  OUTFLE: string;
  DSIndex: Integer;
  ADataArray: TDataArray;
  LineIndex: Integer;
  PPIndex: Integer;
  PilotPointItem: TStoredPilotParamDataItem;
  InsFileIndex: Integer;
begin
  // template files
  WriteSectionHeader('model input/output');
  if FUsePval then
  begin
    TEMPFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPtf));
    INFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPvalExt));
    WriteString(TEMPFLE);
    WriteString(' ' + INFLE);
    NewLine;
  end;

  if Model.ModelSelection in SutraSelection then
  begin
    for LineIndex := 0 to Model.SutraPestScripts.Count - 1 do
    begin
      INFLE := ExtractFileName(Model.SutraPestScripts[LineIndex]);
      TEMPFLE :=  INFLE + '.tpl';
      WriteString(TEMPFLE);
      WriteString(' ' + INFLE);
      NewLine;
    end;
  end;
//  else
  begin
    for DSIndex := 0 to Model.DataArrayManager.DataSetCount - 1 do
    begin
      ADataArray := Model.DataArrayManager[DSIndex];
      if ADataArray.PestParametersUsed and ADataArray.TemplateNeeded then
      begin
        INFLE := ExtractFileName(ChangeFileExt(FNameOfFile,
          '.' + ADataArray.Name + '.script' ));
        TEMPFLE := INFLE + '.tpl';
        WriteString(TEMPFLE);
        WriteString(' ' + INFLE);
        NewLine;
      end;
    end;
  end;
  for PPIndex := 0 to Model.PilotPointData.Count - 1 do
  begin
    PilotPointItem := Model.PilotPointData[PPIndex];
    INFLE := ExtractFileName(PilotPointItem.FileName);
    TEMPFLE := INFLE + '.tpl';
    WriteString(TEMPFLE);
    WriteString(' ' + INFLE);
    NewLine;
  end;

  // instruction files.
  if Model.ModelSelection in Modflow2005Selection then
  begin
    INSFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPestIns));
    OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrMf2005Values));
    WriteString(INSFLE);
    WriteString(' ' + OUTFLE);
    Model.FilesToDelete.Add(OUTFLE);
    NewLine;
  end
  else if Model.ModelSelection = msModflow2015 then
  begin
    INSFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPestIns));
    OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrMf6Values));
    WriteString(INSFLE);
    WriteString(' ' + OUTFLE);
    Model.FilesToDelete.Add(OUTFLE);
    NewLine;
  end
  else if Model.ModelSelection in SutraSelection then
  begin
    INSFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrPestIns));
    OUTFLE := ExtractFileName(ChangeFileExt(FNameOfFile, StrSutraValues));
    WriteString(INSFLE);
    WriteString(' ' + OUTFLE);
    Model.FilesToDelete.Add(OUTFLE);
    NewLine;
  end
  else
  begin
    Assert(False);
  end;

  for InsFileIndex := 0 to Model.InputObsInstructionFiles.Count - 1 do
  begin
    INSFLE := ExtractFileName(Model.InputObsInstructionFiles[InsFileIndex]);
    OUTFLE := ChangeFileExt(INSFLE, '.arrayvalues');
    WriteString(INSFLE);
    WriteString(' ' + OUTFLE);
    Model.FilesToDelete.Add(OUTFLE);
    NewLine;
  end;

  NewLine;
end;

procedure TPestControlFileWriter.WriteObservationGroups;
var
//  ObservationGroups: TPestObservationGroups;
  Mode: TPestMode;
  Procedure WriteObsGroups(ObsGroups: TPestObservationGroups);
  var
    ObsGrpIndex: Integer;
    ObsGroup: TPestObservationGroup;
    CorrelationFileName: string;
  begin
    for ObsGrpIndex := 0 to ObsGroups.Count - 1 do
    begin
      ObsGroup := ObsGroups[ObsGrpIndex];
      WriteString(ObsGroup.ExportedGroupName);
      if ObsGroup.IsRegularizationGroup then
      begin
        if Length(ObsGroup.ObsGroupName) > 7 then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrObservationGroupNa,
            Format(StrTheNamesOfSHas, [ObsGroup.ObsGroupName]))
        end;
      end;
      if Mode = pmRegularisation then
      begin
        if ObsGroup.UseGroupTarget then
        begin
          WriteFloat(ObsGroup.GroupTarget);
          if ObsGroup.GroupTarget <= 0 then
          begin
            frmErrorsAndWarnings.AddError(Model, StrObservationGroupTa,
              Format(StrTheGroupTargetFor, [ObsGroup.ObsGroupName]))
          end;
        end;
      end;
      if ObsGroup.AbsoluteCorrelationFileName <> '' then
      begin
        CorrelationFileName := ' ' + ExtractRelativePath(FNameOfFile, ObsGroup.AbsoluteCorrelationFileName);
        WriteString(CorrelationFileName);
      end;
      NewLine;
    end;
  end;
begin
  WriteSectionHeader('observation groups');
  Mode := Model.PestProperties.PestControlData.PestMode;
  WriteObsGroups(Model.PestProperties.ObservationGroups);
  WriteObsGroups(Model.PestProperties.PriorInfoObservationGroups);
  NewLine;
end;

procedure TPestControlFileWriter.WriteObservations;
var
  ObsIndex: Integer;
  AnObs: IObservationItem;
  ObsItem: TCustomObservationItem;
  ObsGroup: TPestObservationGroup;
  PestControlData: TPestControlData;
  CheckPredictGroup: Boolean;
  PredictGroupCount: Integer;
begin
  PestControlData := Model.PestProperties.PestControlData;
  CheckPredictGroup := PestControlData.PestMode = pmPrediction;
  PredictGroupCount := 0;
  WriteSectionHeader('observation data');
  for ObsIndex := 0 to FUsedObservations.Count - 1 do
  begin
    AnObs := FUsedObservations[ObsIndex];
    if AnObs is TCustomObservationItem then
    begin
      ObsItem := TCustomObservationItem(AnObs);
      if ObsItem.ExportedName <> '' then
      begin
        WriteString(ObsItem.ExportedName);
      end
      else
      begin
        WriteString(AnObs.Name);
      end;
    end
    else
    begin
      WriteString(AnObs.Name);
    end;
    WriteFloat(AnObs.ObservedValue);
    WriteFloat(AnObs.Weight);
    ObsGroup := Model.PestProperties.ObservationGroups.GetObsGroupByName(AnObs.ObservationGroup);
    if ObsGroup = nil then
    begin
      ObsGroup := Model.PestProperties.PriorInfoObservationGroups.GetObsGroupByName(AnObs.ObservationGroup);
    end;

    if ObsGroup = nil then
    begin
      WriteString(' ' + AnObs.ObservationGroup);
      if (AnObs.ObservationGroup = '') and (Model.PestStatus = psActive) then
      begin
        frmErrorsAndWarnings.AddError(Model, StrObservationGroupUn,
          Format(StrNoObservationGroupAssigned, [AnObs.Name]));
      end;
      if SameText(AnObs.ObservationGroup, 'predict') then
      begin
        Inc(PredictGroupCount);
      end;
    end
    else
    begin
      WriteString(' ' + ObsGroup.ExportedGroupName);
      if SameText(ObsGroup.ExportedGroupName, 'predict') then
      begin
        Inc(PredictGroupCount);
      end;
      if (ObsGroup.ExportedGroupName = '') and (Model.PestStatus = psActive) then
      begin
        frmErrorsAndWarnings.AddError(Model, StrObservationGroupUn,
          Format(StrNoObservationGroupAssigned, [AnObs.Name]));
      end;
    end;
//    if AnObs.ObservationGroup = '' then
//    begin
//      frmErrorsAndWarnings.AddError(Model, StrObservationGroupNo,
//        Format(StrNoObservationGroupAssigned, [AnObs.Name]));
//    end;
    NewLine;
  end;
  NewLine;
  if CheckPredictGroup and (PredictGroupCount <> 1) then
  begin
    frmErrorsAndWarnings.AddError(Model, StrPredictionObservati,
      Format(StrIfThePredictionAn, [PredictGroupCount]))
  end;
end;

procedure TPestControlFileWriter.WriteParameterGroups;
var
  GroupIndex: Integer;
  AGroup: TPestParamGroup;
  PestControlData: TPestControlData;
begin
  PestControlData := Model.PestProperties.PestControlData;
  WriteSectionHeader('parameter groups');
  for GroupIndex := 0 to Model.ParamGroups.Count - 1 do
  begin
    AGroup := Model.ParamGroups[GroupIndex];

   // PARGPNME
    WriteString(AGroup.ParamGroupName);

    // INCTYP
    case AGroup.IncrementType of
      icRelative:
        begin
          WriteString(' relative');
        end;
      icAbsolute:
        begin
          WriteString(' absolute');
        end;
      icRelativeToMax:
        begin
          WriteString(' rel_to_max');
        end;
      else Assert(False);
    end;

    // DERINC
    WriteFloat(AGroup.ParamIncrement);

    // DERINCLB
    WriteFloat(AGroup.MinParamIncrement);

    // FORCEN
    case AGroup.ForceCentral of
      fcAlways2:
        begin
          WriteString(' always_2');
        end;
      fcAlways3:
        begin
          WriteString(' always_3');
        end;
      fcAlways5:
        begin
          WriteString(' always_5');
        end;
      fcSwitch:
        begin
          if PestControlData.PestMode = pmPareto then
          begin
            WriteString(' always_3');
          end
          else
          begin
            WriteString(' switch');
          end;
        end;
      fcSwitch5:
        begin
          if PestControlData.PestMode = pmPareto then
          begin
            WriteString(' always_5');
          end
          else
          begin
            WriteString(' switch_5');
          end;
        end;
      else Assert(False);
    end;

    // DERINCMUL
    WriteFloat(AGroup.ParamIncrementMultiplier);

    // DERMTHD
    case AGroup.ForceCentral of
      fcAlways2, fcAlways3, fcSwitch:
        begin
          case AGroup.DM3 of
            dm3Parabolic:
              begin
                WriteString(' parabolic');
              end;
            dm3BestFit:
              begin
                WriteString(' best_fit');
              end;
            dm3OutsidePoints:
              begin
                WriteString(' outside_pts');
              end;
            else Assert(False);
          end;
        end;
      fcAlways5, fcSwitch5:
        begin
          case AGroup.DM5 of
            dm5MinimumVariance:
              begin
                WriteString(' minvar');
              end;
            dm5MaxPrecision:
              begin
                WriteString(' maxprec');
              end;
            else Assert(False);
          end;
        end;
      else
        Assert(False);
    end;

    if AGroup.UseSplitSlopeAnalysis then
    begin
      // SPLITTHRESH
      WriteFloat(AGroup.SplitThreshold);

      // SPLITRELDIFF
      WriteFloat(AGroup.RelSlopeDif);

      // SPLITACTION
      case AGroup.SplitAction of
        saSmaller:
          begin
            WriteString(' smaller');
          end;
        saZero:
          begin
            WriteString(' zero');
          end;
        saPrevious:
          begin
            WriteString(' previous');
          end;
        else Assert(False);
      end;
    end;
    NewLine;
  end;
  NewLine;
end;

procedure TPestControlFileWriter.WriteParameters;
var
  UsedTypes: TParameterTypes;
  ParamIndex: Integer;
  AParam: TModflowParameter;
  PilotPointParameters: TStringList;
  index: Integer;
  PilotPointItem: TStoredPilotParamDataItem;
  ParameterIndex: Integer;
  PilotParamName: string;
  procedure WriteParameter(AParam: TModflowParameter; ParameterName: string = '';
    Value: double = 0; IsPilotPoint: Boolean = False);
  var
    PARNME: string;
    N: Integer;
    PARVAL1: Double;
    PilotPointParam: Boolean;
    PARLBND: Double;
    PARUBND: Double;
    BoundFactor: double;
  begin
    if (PilotParamName <> '') and (ParameterIndex = 1) then
    begin
      WriteString('# Parameters in the ');
      WriteString(PilotPointItem.ParamFamily);
      WriteString(' family are pilot point parameters related to ');
      WriteString(AParam.ParameterName);
      WriteString(' in layer ');
      WriteInteger(PilotPointItem.Layer+1);
      WriteString(' in data set ');
      WriteString(PilotPointItem.DataArrayName);
      NewLine;
    end;

    PilotPointParam := False;
    //PARNME
    if ParameterName <> '' then
    begin
      PARNME := ParameterName;
      PilotPointParam := True;
    end
    else
    begin
      PARNME := AParam.ParameterName;
    end;
    WriteString(PARNME);

    //PARTRANS
    case AParam.Transform of
      ptNoTransform:
        begin
          WriteString(' none');
        end;
      ptLog:
        begin
          WriteString(' log');
        end;
      ptFixed:
        begin
          WriteString(' fixed');
        end;
      ptTied:
        begin
          WriteString(' tied');
        end;
      else Assert(False);
    end;

    //PARCHGLIM
    case AParam.ChangeLimitation of
      pclRelative:
        begin
          WriteString(' relative');
        end;
      pclFactor:
        begin
          WriteString(' factor');
        end;
      pclAbsolute:
        begin
          N := F_AbsParMax.IndexOf(AParam.AbsoluteN) + 1;
          Assert(N >= 1);
          WriteString(Format(' absolute(%d)', [N]));
        end;
      else
        Assert(False);
    end;

    //PARVAL1

    BoundFactor := 1.;
    if PilotPointParam then
    begin
      PARVAL1 := Value;
      if AParam.Value = 0 then
      begin
        BoundFactor := 1.;
      end
      else
      begin
        BoundFactor := PARVAL1/AParam.Value;
      end;
    end
    else
    begin
      PARVAL1 := AParam.Value;
    end;
    WriteFloat(PARVAL1);

    //PARLBND
    if not PilotPointParam then
    begin
      PARLBND := AParam.LowerBound
    end
    else
    begin
      PARLBND := AParam.LowerBound * BoundFactor;
    end;
    WriteFloat(PARLBND);
    if PARVAL1 < PARLBND then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidParameterVa,
        Format(StrForParameter0s, [PARNME, StrLess, StrLower]))
    end;

    //PARUBND
    if not PilotPointParam then
    begin
      PARUBND := AParam.UpperBound;
    end
    else
    begin
      PARUBND := AParam.UpperBound * BoundFactor;
    end;

    WriteFloat(PARUBND);
    if PARVAL1 > PARUBND then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInvalidParameterVa,
        Format(StrForParameter0s, [PARNME, StrGreater, StrUpper]))
    end;

    //PARGP
//    if IsPilotPoint then
//    begin
//      if AParam.PilotPointParameterGroup = '' then
//      begin
//        WriteString(' none');
//        frmErrorsAndWarnings.AddError(Model, StrParameterGroupName,
//          Format(StrTheParameterSH, [AParam.ParameterName]));
//      end
//      else
//      begin
//        WriteString(' ' + AParam.PilotPointParameterGroup);
//      end;
//    end
//    else
//    begin
      if AParam.ParameterGroup = '' then
      begin
        WriteString(' none');
        frmErrorsAndWarnings.AddError(Model, StrParameterGroupName,
          Format(StrTheParameterSH, [AParam.ParameterName]));
      end
      else
      begin
        WriteString(' ' + AParam.ParameterGroup);
      end;
//    end;

    //SCALE
    WriteFloat(AParam.Scale);

    //OFFSET
    WriteFloat(AParam.Offset);

    //DERCOM
    // write only in NUMCOM is written in line 4 of the control data section
//    WriteInteger(1);

    NewLine;
  end;

  procedure WriteTiedParameter(AParam: TModflowParameter);
  begin
    if AParam.Transform = ptTied then
    begin
      // PARNME
      WriteString(AParam.ParameterName);

      // PARTIED
      WriteString(' ' + AParam.TiedParameterName);
      NewLine;
    end;
  end;
begin
  GetUsedTypes(UsedTypes);

  WriteSectionHeader('parameter data');

  PilotPointParameters := TStringList.Create;
  try
    FUsePval := False;
    for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      AParam := Model.ModflowSteadyParameters[ParamIndex];
      if AParam.ParameterType in UsedTypes then
      begin
        if (AParam is TModflowSteadyParameter)
          and TModflowSteadyParameter(AParam).UsePilotPoints then
        begin
          PilotPointParameters.AddObject(AParam.ParameterName, AParam);
        end;

        if (AParam as TModflowSteadyParameter).UsedDirectly
          or (AParam.ParameterType <> ptPEST) then
        begin
          WriteParameter(AParam);
          FUsePval := True;
        end;
      end;
    end;
    PilotPointParameters.CaseSensitive := False;
    PilotPointParameters.Sorted := True;

    for index := 0 to Model.PilotPointData.Count - 1 do
    begin
      PilotPointItem := Model.PilotPointData[index];
      ParamIndex := PilotPointParameters.IndexOf(PilotPointItem.BaseParamName);
      if ParamIndex >= 0 then
      begin
        AParam := PilotPointParameters.Objects[ParamIndex] as TModflowParameter;
        for ParameterIndex := 1 to PilotPointItem.Count do
        begin
          PilotParamName := PilotPointItem.ParameterName(ParameterIndex);
          WriteParameter(AParam, PilotParamName,
            PilotPointItem.Values[ParameterIndex-1].Value);
        end;
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model, StrParameterUndefined,
          Format(StrPilotPointsWereDe, [PilotPointItem.BaseParamName]));
      end;
    end;

    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
    begin
      AParam := Model.ModflowTransientParameters[ParamIndex];
      if AParam.ParameterType in UsedTypes then
      begin
        WriteParameter(AParam);
        FUsePval := True;
      end;
    end;

    for ParamIndex := 0 to Model.HufParameters.Count - 1 do
    begin
      AParam := Model.HufParameters[ParamIndex];
      if AParam.ParameterType in UsedTypes then
      begin
        WriteParameter(AParam);
        FUsePval := True;
      end;
    end;

    // tied parameters.
    for ParamIndex := 0 to Model.ModflowSteadyParameters.Count - 1 do
    begin
      AParam := Model.ModflowSteadyParameters[ParamIndex];
      WriteTiedParameter(AParam);
    end;
    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
    begin
      AParam := Model.ModflowTransientParameters[ParamIndex];
      WriteTiedParameter(AParam);
    end;
    for ParamIndex := 0 to Model.HufParameters.Count - 1 do
    begin
      AParam := Model.HufParameters[ParamIndex];
      WriteTiedParameter(AParam);
    end;

    for index := 0 to Model.PilotPointData.Count - 1 do
    begin
      PilotPointItem := Model.PilotPointData[index];
      ParamIndex := PilotPointParameters.IndexOf(PilotPointItem.BaseParamName);
      if ParamIndex >= 0 then
      begin
        AParam := PilotPointParameters.Objects[ParamIndex] as TModflowParameter;
        if AParam.Transform = ptTied then
        begin
          for ParameterIndex := 1 to PilotPointItem.Count do
          begin
            PilotParamName := PilotPointItem.ParameterName(ParameterIndex);
            // PARNME
            WriteString(PilotParamName);

            // PARTIED
            WriteString(' ' + AParam.TiedParameterName);
            NewLine;
          end;
        end;

      end;
    end;



  finally
    PilotPointParameters.Free;
  end;


  NewLine;
end;

procedure TPestControlFileWriter.WritePareto;
var
  ParetoProperties: TParetoProperties;
  ObsIndex: Integer;
begin
  if Model.PestProperties.PestControlData.PestMode <> pmPareto then
  begin
    Exit;
  end;
  WriteSectionHeader('pareto');
  ParetoProperties := Model.PestProperties.ParetoProperties;

  WriteString(ParetoProperties.ParetoGroupName);
  NewLine;

  WriteFloat(ParetoProperties.InitialParetoWeight);
  WriteFloat(ParetoProperties.FinalParetoWeight);
  WriteInteger(ParetoProperties.ParetoIncrements);
  NewLine;

  WriteInteger(ParetoProperties.InitialIterationCount);
  WriteInteger(ParetoProperties.IntermediateIterationCount);
  WriteInteger(ParetoProperties.FinalIterationCount);
  NewLine;

  WriteInteger(Ord(ParetoProperties.AltTerminationOption));
  NewLine;

  if ParetoProperties.AltTerminationOption = atoUse then
  begin
    WriteString(ParetoProperties.ObservationName);
    case ParetoProperties.AltDirection of
      adAbove:
        begin
          WriteString(' above');
        end;
      adBelow:
        begin
          WriteString(' below');
        end;
      else
        Assert(False);
    end;
    WriteFloat(ParetoProperties.AltThreshold);
    WriteInteger(ParetoProperties.AltIterations);
    NewLine;
  end;

  WriteInteger(ParetoProperties.ObservationsToReport.Count);
  NewLine;

  for ObsIndex := 0 to ParetoProperties.ObservationsToReport.Count - 1 do
  begin
    WriteString(' ' + ParetoProperties.ObservationsToReport[ObsIndex]);
  end;
  NewLine;
end;

procedure TPestControlFileWriter.WritePredictiveAnalysis;
var
  PredictionProperties: TPredictionProperties;
begin
  if Model.PestProperties.PestControlData.PestMode <> pmPrediction then
  begin
    Exit;
  end;
  WriteSectionHeader('predictive analysis');
  PredictionProperties := Model.PestProperties.PredictionProperties;

  if PredictionProperties.MinOrMax = mmMinimize then
  begin
    WriteInteger(-1);
  end
  else
  begin
    WriteInteger(1);
  end;
  WriteInteger(Ord(PredictionProperties.PredictiveNoise));
  NewLine;

  if PredictionProperties.TargetPhi <= 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidPESTVariabl,
      Format(Str0sInThePESTPre, ['PD0', PredictionProperties.TargetPhi]))
  end;
  if PredictionProperties.AcceptedPhi <= PredictionProperties.TargetPhi then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidPESTVariabl,
      Format(Str0sInThePESTPre2, ['PD1', 'PD0',
      PredictionProperties.AcceptedPhi, PredictionProperties.TargetPhi]))
  end;
  if PredictionProperties.TestLambdaPhi <= PredictionProperties.AcceptedPhi then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidPESTVariabl,
      Format(Str0sInThePESTPre2, ['PD2', 'PD1',
      PredictionProperties.TestLambdaPhi, PredictionProperties.AcceptedPhi]))
  end;
  WriteFloat(PredictionProperties.TargetPhi);
  WriteFloat(PredictionProperties.AcceptedPhi);
  WriteFloat(PredictionProperties.TestLambdaPhi);
  NewLine;

  WriteFloat(PredictionProperties.AbsoluteLamdaCriterion);
  WriteFloat(PredictionProperties.RelativeLamdaCriterion);
  if PredictionProperties.InitialLineSearchFactor <= 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidPESTVariabl,
      Format(Str0sInThePESTPre, ['INITSCHFAC',
      PredictionProperties.InitialLineSearchFactor]))
  end;
  WriteFloat(PredictionProperties.InitialLineSearchFactor);
  if PredictionProperties.InitialLineSearchFactor <= 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrInvalidPESTVariabl,
      Format(Str0sInThePESTPre, ['MULSCHFAC',
      PredictionProperties.UpdateLineSearchFactor]))
  end;
  WriteFloat(PredictionProperties.UpdateLineSearchFactor);
  WriteInteger(PredictionProperties.LineSearchRuns);
  NewLine;

  WriteFloat(PredictionProperties.AbsolutePredictionSwitch);
  WriteFloat(PredictionProperties.RelativePredictionSwitch);
  NewLine;

  WriteInteger(PredictionProperties.MaxNoPredictionImprovmentRuns);
  WriteFloat(PredictionProperties.AbsoluteImprovementCriterion);
  WriteFloat(PredictionProperties.RelativeImprovementCriterion);
  WriteInteger(PredictionProperties.NumberOfPredictionsToCompare);
  NewLine;
end;

procedure TPestControlFileWriter.WritePriorInformation;
var
  PriorIndex: Integer;
begin
  if FPriorInfomationEquations.Count > 0 then
  begin
    WriteSectionHeader('prior information');
    for PriorIndex := 0 to FPriorInfomationEquations.Count - 1 do
    begin
      WriteString(FPriorInfomationEquations[PriorIndex]);
      NewLine;
    end;
    NewLine;
  end;
end;

procedure TPestControlFileWriter.WriteRegularisation;
var
  Regularization: TPestRegularization;
begin
  if Model.PestProperties.PestControlData.PestMode <> pmRegularisation then
  begin
    Exit;
  end;

  Regularization := Model.PestProperties.Regularization;
  WriteSectionHeader('regularisation');
  WriteFloat(Regularization.PhiMLim);
  WriteFloat(Regularization.PhiMAccept);
  WriteFloat(Regularization.FracPhiM);
  if Regularization.MemSave = msNoMemSave then
  begin
    WriteString(' nomemsave');
  end
  else
  begin
    WriteString(' memsave');
  end;
  // GENREG doesn't like comments in the regularizaiton section here.
//  WriteString(' # PHIMLIM PHIMACCEPT FRACPHIM MEMSAVE');
  NewLine;

  WriteFloat(Regularization.WFInit);
  WriteFloat(Regularization.WeightFactorMinimum);
  WriteFloat(Regularization.WeightFactorMaximum);
  if Regularization.LinearRegression = lfNoLinReg then
  begin
    WriteString(' nonlinreg');
  end
  else
  begin
    WriteString(' linreg');
  end;
  if Regularization.RegContinue = rcNoContinue then
  begin
    WriteString(' nocontinue');
  end
  else
  begin
    WriteString(' continue');
  end;
  WriteString(' # WFINIT WFMIN WFMAX LINREG REGCONTINUE');
  NewLine;
  
  WriteFloat(Regularization.WFFac);
  WriteFloat(Regularization.WeightFactorTolerance);
  WriteInteger(Regularization.RegularizationOption);
  if Regularization.RegularizationOption in [4,5] then
  begin
    WriteInteger(Regularization.OptimizationInterval);
    WriteFloat(Regularization.RegWeightRatio);
  end;
  if Regularization.RegularizationOption  = 5 then
  begin
    WriteFloat(Regularization.RegularizationSingularValueThreshhold);
  end;
  WriteString(' # WFFAC WFTOL IREGADJ');
  if Regularization.RegularizationOption in [4,5] then
  begin
    WriteString(' NOPTREGADJ REGWEIGHTRAT');
  end;
  if Regularization.RegularizationOption  = 5 then
  begin
    WriteString(' REGSINGTHRESH');
  end;
  NewLine;
end;

procedure TPestControlFileWriter.WriteSectionHeader(const SectionID: String);
begin
  WriteString('* ');
  WriteString(SectionID);
  NewLine;
end;

procedure TPestControlFileWriter.WriteSensitivityReuse;
begin
// The sensitivity reuse section is not currently supported.
end;

procedure TPestControlFileWriter.WriteSingularValueDecomposition;
var
  SvdProperties: TSingularValueDecompositionProperties;
  PestControlData: TPestControlData;
begin
  SvdProperties := Model.PestProperties.SvdProperties;
  WriteSectionHeader('singular value decomposition');

  PestControlData := Model.PestProperties.PestControlData;
  if PestControlData.PestMode = pmPrediction then
  begin
    WriteInteger(0);
  end
  else
  begin
    WriteInteger(Ord(SvdProperties.Mode));
  end;

  WriteString(' # SVDMODE');
  NewLine;

  WriteInteger(SvdProperties.MaxSingularValues);
  WriteFloat(SvdProperties.EigenThreshold);
  WriteString(' # MAXSING, EIGTHRESH');
  NewLine;

  WriteInteger(Ord(SvdProperties.EigenWrite));
  WriteString(' # EIGWRITE');
  NewLine;
  NewLine;
end;

procedure TPestControlFileWriter.WriteSVD_Assist;
begin
// Writing the SVD Assist section is not currently supported.
end;

end.
