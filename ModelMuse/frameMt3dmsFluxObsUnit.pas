unit frameMt3dmsFluxObsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CustomFrameFluxObsUnit, StdCtrls, Grids, RbwDataGrid4,
  Mt3dmsFluxObservationsUnit;

type
  TframeMt3dmsFluxObs = class(TCustomframeFluxObs)
  private
    function GetRowCount(Observations: TMt3dFluxGroupList): integer;
    { Private declarations }
  public
    function GetData(ListOfScreenObjects: TList;
      Observations: TMt3dFluxGroupList): integer;
    procedure SetData(ListOfScreenObjects: TList;
      Observations: TMt3dFluxGroupList; ScreenObjectsUsed: TCheckBoxState);
    { Public declarations }
  end;

var
  frameMt3dmsFluxObs: TframeMt3dmsFluxObs;

implementation

uses
  Math, ScreenObjectUnit, FluxObservationUnit;

{$R *.dfm}

{ TframeMt3dmsFluxObs }

function TframeMt3dmsFluxObs.GetData(ListOfScreenObjects: TList;
  Observations: TMt3dFluxGroupList): integer;
var
  GroupIndex: Integer;
  ObsGroup: TMt3dmsFluxObservationGroups;
  ObsIndex: Integer;
  Obs: TMt3dmsFluxObservationGroup;
  RowIndex: Integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ScreenObjectPosition: Integer;
  ScreenObjectUsed: Boolean;
  ObsFactor: TObservationFactor;
  FirstObsForScreenObject: Boolean;
begin
  rdgObservationGroups.RowCount := GetRowCount(Observations);
  rdgObservationGroups.FixedCols := 1;
  rdgObservationGroups.FixedRows := 1;
  RowIndex := 1;
  rdgObservationGroups.BeginUpdate;
  try
    for GroupIndex := 0 to Observations.Count - 1 do
    begin
      ObsGroup := Observations[GroupIndex];
      for ObsIndex := 0 to ObsGroup.Count - 1 do
      begin
        Obs := ObsGroup[ObsIndex];
        rdgObservationGroups.Cells[0,RowIndex] := IntToStr(RowIndex);
        rdgObservationGroups.Cells[1,RowIndex] := Obs.ObservationName;
        rdgObservationGroups.Objects[0,RowIndex] := Obs;

        for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
          ScreenObjectPosition :=
            Obs.ObservationFactors.IndexOfScreenObject(ScreenObject);
          ScreenObjectUsed := ScreenObjectPosition >= 0;
          if ScreenObjectIndex = 0 then
          begin
            rdgObservationGroups.Checked[1,RowIndex] := ScreenObjectUsed;
            if ScreenObjectUsed then
            begin
              ObsFactor := Obs.ObservationFactors[ScreenObjectPosition];
              rdgObservationGroups.Cells[2,RowIndex] := ObsFactor.Factor;
            end
            else
            begin
              rdgObservationGroups.Cells[2,RowIndex] := '';
            end;
          end
          else
          begin
            if rdgObservationGroups.State[1,RowIndex] <> cbGrayed then
            begin
              if rdgObservationGroups.Checked[1,RowIndex]
                <> ScreenObjectUsed then
              begin
                if ScreenObjectUsed then
                begin
                  ObsFactor :=
                    Obs.ObservationFactors[ScreenObjectPosition];
                  rdgObservationGroups.Cells[2,RowIndex] :=
                    ObsFactor.Factor;
                end;
                rdgObservationGroups.State[1,RowIndex] := cbGrayed
              end;
            end
            else if ScreenObjectUsed then
            begin
              ObsFactor := Obs.ObservationFactors[ScreenObjectPosition];
              if rdgObservationGroups.Cells[2,RowIndex]
                <> ObsFactor.Factor then
              begin
                rdgObservationGroups.Cells[2,RowIndex] := '';
              end;
            end;
          end;
        end;
        Inc(RowIndex);
      end;
    end;
  finally
    rdgObservationGroups.EndUpdate;
  end;

  result := 0;
  for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
  begin
    ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
    FirstObsForScreenObject := True;
    for GroupIndex := 0 to Observations.Count - 1 do
    begin
      ObsGroup := Observations[GroupIndex];
      for ObsIndex := 0 to ObsGroup.Count - 1 do
      begin
        Obs := ObsGroup[ObsIndex];
        if Obs.ObservationFactors.
          IndexOfScreenObject(ScreenObject) >= 0 then
        begin
          if FirstObsForScreenObject then
          begin
            FirstObsForScreenObject := False;
            Inc(result);
          end;
        end;
      end;
    end;
  end;
end;

procedure TframeMt3dmsFluxObs.SetData(ListOfScreenObjects: TList;
  Observations: TMt3dFluxGroupList; ScreenObjectsUsed: TCheckBoxState);
var
  RowIndex: Integer;
  GroupIndex: Integer;
  ObsGroup: TMt3dmsFluxObservationGroups;
  ObsIndex: Integer;
  Observation: TMt3dmsFluxObservationGroup;
  ObsState: TCheckBoxState;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ObjectPosition: Integer;
  ObsFactor: TObservationFactor;
  ObsAllowed: Boolean;
begin
  Assert(rdgObservationGroups.RowCount = GetRowCount(Observations));
  RowIndex := 1;
  for GroupIndex := 0 to Observations.Count - 1 do
  begin
    ObsGroup:= Observations[GroupIndex];
    for ObsIndex := 0 to ObsGroup.Count - 1 do
    begin
      Observation := ObsGroup[ObsIndex];

      Assert(Observation.ObservationName =
        rdgObservationGroups.Cells[1,RowIndex]);

      ObsState := rdgObservationGroups.CheckState[1, RowIndex];
      for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
      begin
        ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
        ObjectPosition := Observation.ObservationFactors.
          IndexOfScreenObject(ScreenObject);

        ObsAllowed := False;
        case ObsGroup.FluxObservationType of
          mfotHead: ObsAllowed := (ScreenObject.ModflowChdBoundary <> nil)
            and ScreenObject.ModflowChdBoundary.Used;
          mfotWell: ObsAllowed := (ScreenObject.ModflowWellBoundary <> nil)
            and ScreenObject.ModflowWellBoundary.Used;
          mfotDrain: ObsAllowed := (ScreenObject.ModflowDrnBoundary <> nil)
            and ScreenObject.ModflowDrnBoundary.Used;
          mfotRiver: ObsAllowed := (ScreenObject.ModflowRivBoundary <> nil)
            and ScreenObject.ModflowRivBoundary.Used;
          mfotGHB: ObsAllowed := (ScreenObject.ModflowGhbBoundary <> nil)
            and ScreenObject.ModflowGhbBoundary.Used;
          mfotRecharge: ObsAllowed := (ScreenObject.ModflowRchBoundary <> nil)
            and ScreenObject.ModflowRchBoundary.Used;
          mfotEVT: ObsAllowed := (ScreenObject.ModflowEvtBoundary <> nil)
            and ScreenObject.ModflowEvtBoundary.Used;
          mfotMassLoading: ObsAllowed := (ScreenObject.Mt3dmsConcBoundary <> nil)
            and ScreenObject.Mt3dmsConcBoundary.Used;
          mfotSTR: ObsAllowed := (ScreenObject.ModflowStrBoundary <> nil)
            and ScreenObject.ModflowStrBoundary.Used;
          mfotReservoir: ObsAllowed := (ScreenObject.ModflowResBoundary <> nil)
            and ScreenObject.ModflowResBoundary.Used;
          mfotFHB_Head:  ObsAllowed := (ScreenObject.ModflowFhbHeadBoundary <> nil)
            and ScreenObject.ModflowFhbHeadBoundary.Used;
          mfotFHB_Flow:  ObsAllowed := (ScreenObject.ModflowFhbFlowBoundary <> nil)
            and ScreenObject.ModflowFhbFlowBoundary.Used;
          mfotIBS: ;
          mfotTransLeakage: ; // The Transient Leakage (TLK) package is not in MODFLOW-2000 or MODFLOW-2005.
          mfotLake: ObsAllowed := (ScreenObject.ModflowLakBoundary <> nil)
            and ScreenObject.ModflowLakBoundary.Used;
          mfotMNW1: ;
          mfotDRT: ObsAllowed := (ScreenObject.ModflowDrtBoundary <> nil)
            and ScreenObject.ModflowDrtBoundary.Used;
          mfotETS: ObsAllowed := (ScreenObject.ModflowEtsBoundary <> nil)
            and ScreenObject.ModflowEtsBoundary.Used;
          else Assert(False);
        end;

        if (ScreenObjectsUsed = cbUnchecked)
          or (ObsState = cbUnchecked)
          or not ObsAllowed then
        begin
          // remove
          if ObjectPosition >= 0 then
          begin
            Observation.ObservationFactors.Delete(ObjectPosition);
            ObjectPosition := -1;
          end;
        end
        else if (ScreenObjectsUsed = cbChecked)
          and (ObsState = cbChecked) then
        begin
          // add
          if ObjectPosition < 0 then
          begin
            ObjectPosition := Observation.AddObject(ScreenObject)
          end;
        end;
        if (ObjectPosition >= 0)
          and (rdgObservationGroups.Cells[2, RowIndex] <> '') then
        begin
          ObsFactor := Observation.ObservationFactors[ObjectPosition];
          ObsFactor.Factor := rdgObservationGroups.Cells[2, RowIndex];
        end;
      end;
      Inc(RowIndex);
    end;
  end;
end;

function TframeMt3dmsFluxObs.GetRowCount(Observations: TMt3dFluxGroupList): integer;
var
  GroupIndex: Integer;
  ObsGroup: TMt3dmsFluxObservationGroups;
begin
  result := 1;
  for GroupIndex := 0 to Observations.Count - 1 do
  begin
    ObsGroup := Observations[GroupIndex];
    result := result + ObsGroup.Count;
  end;
  result := Max(2, result);
end;

end.
