unit GlobalData;

interface

  uses
    SyncObjs, SysUtils, Graphics,
    //Classes,
    DataGrid, PriorInfoUnit, DependentsUnit,
    GlobalBasicData, GlobalTypesUnit, ModelMateClassesUnit, ParallelControlUnit,
    ParallelRunnerUnit, JupiterUnit, RbwDataGrid4;

  var
    // Types defined for ModelMate
    PDefault: TProject;    // Default values of ModelMate project variables
    PCurrent: TProject;    // Current values of ModelMate project variables
    PLastSaved: TProject;  // ModelMate project variables as last saved

    ParameterSetupDefault: TParameterSetup;
    ParameterSetupCurrent: TParameterSetup;
    ParameterSetupLastSaved: TParameterSetup;

    ParamSetCurrent: TParamSet;

    ParamGpsDefault: TParamSet;
    ParamGpsCurrent: TParamSet;
    ParamGpsLastSaved: TParamSet;

    ParamSetTemp: TParamSet;
    ParamGpsTemp: TParamSet;

    ParallelControlGlobal: TParallelControl;
    ParallelRunnersGlobal: TParallelRunners;
    RunnerFilesGlobal: TRunnerFiles;

    DisabledGray: TColor = clGrayText;

    { Events }
    EShowCompleted: TEvent;

  procedure InitializeGlobalMemoryObjects;
  procedure FreeGlobalData;
  function GetObsHint(const aGrid: TRbwDataGrid4; const aCol: Integer): string;
  function GetPredHint(const aGrid: TRbwDataGrid4; const aCol: Integer): string;
  function GetParHint(const aGrid: TRbwDataGrid4; const aCol: Integer): string;
  function GetParHintOld(const aGrid: TDataGrid; const aCol: Integer): string;
  function GetPriHint(const aGrid: TRbwDataGrid4; const aCol: Integer): string;
  procedure InitializeGlobalData;

implementation

//###################################################################

procedure InitializeGlobalMemoryObjects;
begin
  ParameterSetupCurrent.Assign(ParameterSetupDefault);
  ParamSetCurrent.Clear;
  ParamGpsCurrent.Assign(ParamGpsDefault);
end;

//###################################################################

procedure FreeGlobalData;
begin
  FreeAndNil(ParamSetCurrent);
  FreeAndNil(ParamGpsDefault);
  FreeAndNil(ParamGpsLastSaved);
  FreeAndNil(ParamGpsCurrent);
  FreeAndNil(ParamSetTemp);
  FreeAndNil(ParamGpsTemp);

  SetLength(AttributeTypes, 0);
  EShowCompleted.Free;
  ParallelControlGlobal.Free;
  ParallelRunnersGlobal.Free;
  RunnerFilesGlobal.Free;
end;

//###################################################################

function GetObsHint(const aGrid: TRbwDataGrid4; const aCol: Integer): string;
var
  Str, TestStr, Caption: string;
  I: Integer;
begin
  // Need to determine attribute type, given Caption
  Caption := aGrid.Cells[aCol,0];
  result := '';
  for I := 0 to ObservationSetupCurrent.NumAtt - 1 do
    begin
      TestStr := ObservationSetupCurrent.ObsAttributes[I].Caption;
      if TestStr = Caption then
        begin
          Str := ObservationSetupCurrent.ObsAttributes[I].Caption + ': ' +
                 ObservationSetupCurrent.ObsAttributes[I].Hint(dcObs);
          result := Str;
        end;
    end;
end;

function GetPredHint(const aGrid: TRbwDataGrid4; const aCol: Integer): string;
var
  Str, TestStr, Caption: string;
  I: Integer;
begin
  // Need to determine attribute type, given Caption.
  Caption := aGrid.Cells[aCol,0];
  result := '';
  for I := 0 to PredictionSetupCurrent.NumAtt - 1 do
    begin
      TestStr := PredictionSetupCurrent.PredAttributes[I].Caption;
      if TestStr = Caption then
        begin
          Str := PredictionSetupCurrent.PredAttributes[I].Caption + ': ' +
                 PredictionSetupCurrent.PredAttributes[I].Hint(dcPred);
          result := Str;
        end;
    end;
end;

//###################################################################

function GetParHint(const aGrid: TRbwDataGrid4; const aCol: Integer): string;
var
  Str, TestStr, Caption: string;
  I: Integer;
begin
  // Need to determine attribute type, given Caption
  Caption := aGrid.Cells[aCol,0];
  result := '';
  if ParameterSetupCurrent <> nil then
    begin
      for I := 0 to ParameterSetupCurrent.NumAtt - 1 do
        begin
          TestStr := ParameterSetupCurrent.ParAttributes[I].Caption;
          if TestStr = Caption then
            begin
              Str := ParameterSetupCurrent.ParAttributes[I].Caption + ': ' +
                     ParameterSetupCurrent.ParAttributes[I].Hint;
              result := Str;
              Break;
            end;
        end;
    end;
end;

//###################################################################

function GetParHintOld(const aGrid: TDataGrid; const aCol: Integer): string;
var
  Str, TestStr, Caption: string;
  I: Integer;
begin
  // Need to determine attribute type, given Caption
  Caption := aGrid.Cells[aCol,0];
  result := '';
  for I := 0 to ParameterSetupCurrent.NumAtt - 1 do
  begin
    TestStr := ParameterSetupCurrent.ParAttributes[I].Caption;
    if TestStr = Caption then
    begin
      Str := ParameterSetupCurrent.ParAttributes[I].Caption + ': ' +
             ParameterSetupCurrent.ParAttributes[I].Hint;
      result := Str;
      Break;
    end;
  end;
end;

//###################################################################

  function GetPriHint(const aGrid: TRbwDataGrid4; const aCol: Integer): string;
var
  Str, TestStr, Caption: string;
  I: Integer;
begin
  // Need to determine attribute type, given Caption.
  Caption := aGrid.Cells[aCol,0];
  result := '';
  for I := 0 to PriorSetupCurrent.NumAtt - 1 do
  begin
    TestStr := PriorSetupCurrent.PriAttributes[I].Caption;
    if TestStr = Caption then
    begin
      Str := PriorSetupCurrent.PriAttributes[I].Caption + ': ' +
             PriorSetupCurrent.PriAttributes[I].Hint;
      result := Str;
    end;
  end;
end;

//###################################################################

procedure InitializeGlobalData;
begin
  EShowCompleted := TEvent.Create(nil, False, False,
                    'SHOW_COMPLETED', True);
  ParallelControlGlobal := TParallelControl.Create;
  ParallelRunnersGlobal := TParallelRunners.Create;
  RunnerFilesGlobal := TRunnerFiles.Create;
  ParamSetTemp := TParamSet.Create;
  ParamGpsTemp := TParamSet.Create;
end;

//###################################################################

initialization
  InitializeGlobalData;

finalization
  FreeGlobalData;

end.


