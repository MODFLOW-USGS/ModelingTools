unit ModelMateUtilities;

interface

  uses Classes, Dialogs, SysUtils, Windows,
       DependentsUnit,
       GlobalBasicData, GlobalTypesUnit, JupiterUnit, ModelMateClassesUnit,
       GlobalData, PriorInfoUnit;

  procedure AssignCurrentDataObjects(Prj: TProject);
  procedure AssignCurrentObsObjects(Prj: TProject);
  procedure AssignCurrentParamObjects(Prj: TProject);
  procedure AssignCurrentPredObjects(Prj: TProject);
  procedure AssignCurrentPriorObjects(Prj: TProject);
  function FileBaseName(const FileName: TFileName): string;
  function PosCap(Cap: string): integer;
  function UniqueGroupName(const GroupName: string; const GpUse: TGrpUse): boolean;
  procedure UpdateCurrentProject;

implementation

procedure AssignCurrentDataObjects(Prj: TProject);
begin
  AssignCurrentParamObjects(Prj);
  AssignCurrentObsObjects(Prj);
  AssignCurrentPredObjects(Prj);
  AssignCurrentPriorObjects(Prj);
end;

procedure AssignCurrentObsObjects(Prj: TProject);
begin
  ObservationSetupCurrent.Assign(Prj.ObservationSetup);
  ObsGpsCurrent.Assign(Prj.ObsGpSet);
  ObsSetCurrent.Assign(Prj.ObsSet);
end;

procedure AssignCurrentParamObjects(Prj: TProject);
begin
  ParamSetCurrent.Assign(Prj.ParamSet);
  ParamGpsCurrent.Assign(Prj.ParGpSet);
  ParameterSetupCurrent.Assign(Prj.ParameterSetup);
end;

procedure AssignCurrentPredObjects(Prj: TProject);
begin
  PredictionSetupCurrent.Assign(Prj.PredictionSetup);
  PredGpsCurrent.Assign(Prj.PredGpSet);
  PredSetCurrent.Assign(Prj.PredSet);
end;

procedure AssignCurrentPriorObjects(Prj: TProject);
begin
  PriorSetupCurrent.Assign(Prj.PriorSetup);
  PriGpsCurrent.Assign(Prj.PriGpSet);
  PriSetCurrent.Assign(Prj.PriSet);
end;

function FileBaseName(const FileName: TFileName): string;
{ Return the base name part of a file name.  Note that TFileName is
  defined as AnsiString }
var
  BaseName: string;
begin
  BaseName := ChangeFileExt(FileName, '');
  BaseName := ExtractFileName(BaseName);
  result := BaseName;
end;

function PosCap(Cap: string): integer;
// Find parameter attribute index that has Cap as caption
var
  ParTemp: TParam;
  I: integer;
begin
  ParTemp := TParam.CreateAndName(nil, 'AnyName', 'AnyGroup');
  try
    result := -8;
    for I := 0 to NumParAttributes - 1 do
      begin
        if ParTemp.AllAtts[I].Caption = Cap then
          result := I;
      end;
  finally
    ParTemp.Free;
  end;
end;

function UniqueGroupName(const GroupName: string; const GpUse: TGrpUse): boolean;
var
  I: integer;
begin
  result := True;
  // Ensure that group name is unique among group GpUse.
  case GpUse of
    guParGroup:
      begin
        for I := 0 to ParamGpsCurrent.Count - 1 do
          begin
            if AnsiSameText(GroupName,ConvertString(ParamGpsCurrent.Items[I].Name)) then
              result := False;
          end;
      end;
    guObsGroup:
      begin
        for I := 0  to ObsGpsCurrent.Count - 1 do
          begin
            if AnsiSameText(GroupName,ConvertString(ObsGpsCurrent.Items[I].Name)) then
              result := False;
          end;
      end;
    guPredGroup:
      begin
        for I := 0  to PredGpsCurrent.Count - 1 do
          begin
            if AnsiSameText(GroupName,ConvertString(PredGpsCurrent.Items[I].Name)) then
              result := False;
          end;
      end;
    guPriGroup:
      begin
        for I := 0  to PriGpsCurrent.Count - 1 do
          begin
            if AnsiSameText(GroupName,ConvertString(PriGpsCurrent.Items[I].Name)) then
              result := False;
          end;
      end;
    guUnknown: ;
  end
end;

procedure UpdateCurrentProject;
begin
  PCurrent.ParamSet.Assign(ParamSetCurrent);
  PCurrent.ParGpSet.Assign(ParamGpsCurrent);
  PCurrent.ParameterSetup.Assign(ParameterSetupCurrent);
  PCurrent.ObservationSetup.Assign(ObservationSetupCurrent);
  PCurrent.ObsGpSet.Assign(ObsGpsCurrent);
  PCurrent.ObsSet.Assign(ObsSetCurrent);
  PCurrent.PredictionSetup.Assign(PredictionSetupCurrent);
  PCurrent.PredGpSet.Assign(PredGpsCurrent);
  PCurrent.PredSet.Assign(PredSetCurrent);
  PCurrent.PriorSetup.Assign(PriorSetupCurrent);
  PCurrent.PriGpSet.Assign(PriGpsCurrent);
  PCurrent.PriSet.Assign(PriSetCurrent);
  PCurrent.SetupParGroupUsage;
  PCurrent.SetupObsGroupUsage;
  PCurrent.SetupPredGroupUsage;
  PCurrent.SetupPriGroupUsage;
end;

end.




