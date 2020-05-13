unit frameFluxObsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CustomFrameFluxObsUnit, StdCtrls, Grids, RbwDataGrid4,
  FluxObservationUnit, SutraPestObsUnit;

type
  TObsCol = (ocLabel, ocName, ocFormula);

  TframeFluxObs = class(TCustomframeFluxObs)
  private
    procedure GetObservationFactors(ObsevationIndex: Integer;
      ObservationFactors: TObservationFactors; ListOfScreenObjects: TList);
    procedure SetObservationFactors(ObsevationIndex: Integer; ObservationFactors: TObservationFactors; ListOfScreenObjects: TList; ScreenObjectsUsed: TCheckBoxState);
    { Private declarations }
  public
    // @name returns the number of @link(TScreenObject)s in ListOfScreenObjects
    // that are included in one or more @link(TFluxObservationGroup) in
    // Observations.
    //
    // @name fills @link(rdgObservationGroups) with the data related to
    // the observations.
    function GetData(ListOfScreenObjects: TList;
      Observations: TFluxObservationGroups): integer; overload;
    function GetData(ListOfScreenObjects: TList;
      Observations: TCustomSutraFluxObservationGroups): integer; overload;
    // @name modifies Observations based on the values in
    // @link(rdgObservationGroups) and the @link(TScreenObject)s
    // in ListOfScreenObjects.
    // Initially, Observations should be identical to the
    // ones in @link(GetData).
    procedure SetData(ListOfScreenObjects: TList;
      Observations: TFluxObservationGroups;
      ScreenObjectsUsed: TCheckBoxState); overload;
    procedure SetData(ListOfScreenObjects: TList;
      Observations: TCustomSutraFluxObservationGroups;
      ScreenObjectsUsed: TCheckBoxState); overload;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Math, ScreenObjectUnit;

{ TframeFluxObs }

function TframeFluxObs.GetData(ListOfScreenObjects: TList;
  Observations: TFluxObservationGroups): integer;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ObsevationIndex: Integer;
  Observation: TFluxObservationGroup;
  FirstObsForScreenObject: boolean;
begin
  rdgObservationGroups.RowCount := Max(Observations.Count + 1,2);
  rdgObservationGroups.FixedCols := 1;
  rdgObservationGroups.FixedRows := 1;
  rdgObservationGroups.BeginUpdate;
  try
    for ObsevationIndex := 0 to Observations.Count - 1 do
    begin
      Observation := Observations[ObsevationIndex];
      rdgObservationGroups.Cells[Ord(ocLabel),ObsevationIndex+1] :=
        IntToStr(ObsevationIndex+1);
      rdgObservationGroups.Cells[Ord(ocName),ObsevationIndex+1] :=
        Observation.ObservationName;
      GetObservationFactors(ObsevationIndex, Observation.ObservationFactors,
        ListOfScreenObjects);
//      for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
//      begin
//        ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
//        ScreenObjectPosition :=
//          Observation.ObservationFactors.IndexOfScreenObject(ScreenObject);
//        ScreenObjectUsed := ScreenObjectPosition >= 0;
//        if ScreenObjectIndex = 0 then
//        begin
//          rdgObservationGroups.Checked[Ord(ocName),ObsevationIndex+1] := ScreenObjectUsed;
//          if ScreenObjectUsed then
//          begin
//            ObsFactor := Observation.ObservationFactors[ScreenObjectPosition];
//            rdgObservationGroups.Cells[Ord(ocFormula),ObsevationIndex+1] := ObsFactor.Factor;
//          end
//          else
//          begin
//            rdgObservationGroups.Cells[Ord(ocFormula),ObsevationIndex+1] := '';
//          end;
//        end
//        else
//        begin
//          if rdgObservationGroups.State[Ord(ocName),ObsevationIndex+1] <> cbGrayed then
//          begin
//            if rdgObservationGroups.Checked[Ord(ocName),ObsevationIndex+1]
//              <> ScreenObjectUsed then
//            begin
//              if ScreenObjectUsed then
//              begin
//                ObsFactor :=
//                  Observation.ObservationFactors[ScreenObjectPosition];
//                rdgObservationGroups.Cells[Ord(ocFormula),ObsevationIndex+1] :=
//                  ObsFactor.Factor;
//              end;
//              rdgObservationGroups.State[Ord(ocName),ObsevationIndex+1] := cbGrayed
//            end;
//          end
//          else if ScreenObjectUsed then
//          begin
//            ObsFactor := Observation.ObservationFactors[ScreenObjectPosition];
//            if rdgObservationGroups.Cells[Ord(ocFormula),ObsevationIndex+1]
//              <> ObsFactor.Factor then
//            begin
//              rdgObservationGroups.Cells[Ord(ocFormula),ObsevationIndex+1] := '';
//            end;
//          end;
//        end;
//      end;
    end;
  finally
    rdgObservationGroups.EndUpdate;
  end;

  result := 0;
  for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
  begin
    ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
    FirstObsForScreenObject := True;
    for ObsevationIndex := 0 to Observations.Count - 1 do
    begin
      Observation := Observations[ObsevationIndex];
      if Observation.ObservationFactors.
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

procedure TframeFluxObs.SetData(ListOfScreenObjects: TList;
  Observations: TFluxObservationGroups; ScreenObjectsUsed: TCheckBoxState);
var
  ObsevationIndex: Integer;
  Observation: TFluxObservationGroup;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ObjectPosition: Integer;
  ObsFactor: TObservationFactor;
  ObsState: TCheckBoxState;
begin
  if Observations.Count > 0 then
  begin
    Assert(rdgObservationGroups.RowCount = Observations.Count+1);
  end;
  for ObsevationIndex := 0 to Observations.Count - 1 do
  begin
    Observation:= Observations[ObsevationIndex];
    Assert(Observation.ObservationName =
      rdgObservationGroups.Cells[Ord(ocName),ObsevationIndex+1]);

    SetObservationFactors(ObsevationIndex, Observation.ObservationFactors,
      ListOfScreenObjects, ScreenObjectsUsed);
//    ObsState := rdgObservationGroups.CheckState[Ord(ocName), ObsevationIndex+1];
//    for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
//    begin
//      ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
//      ObjectPosition := Observation.ObservationFactors.
//        IndexOfScreenObject(ScreenObject);
//      if (ScreenObjectsUsed = cbUnchecked)
//        or (ObsState = cbUnchecked) then
//      begin
//        // remove
//        if ObjectPosition >= 0 then
//        begin
//          Observation.ObservationFactors.Delete(ObjectPosition);
//          ObjectPosition := -1;
//        end;
//      end
//      else if (ScreenObjectsUsed = cbChecked)
//        and (ObsState = cbChecked) then
//      begin
//        // add
//        if ObjectPosition < 0 then
//        begin
//          ObjectPosition := Observation.AddObject(ScreenObject)
//        end;
//      end;
//      if (ObjectPosition >= 0)
//        and (rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex+1] <> '') then
//      begin
//        ObsFactor := Observation.ObservationFactors[ObjectPosition];
//        ObsFactor.Factor := rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex+1];
//      end;
//    end;
  end;
end;

function TframeFluxObs.GetData(ListOfScreenObjects: TList;
  Observations: TCustomSutraFluxObservationGroups): integer;
var
  ObsevationIndex: Integer;
  Observation: TCustomSutraFluxObservations;
  ScreenObjectIndex: Integer;
  ScreenObject: Pointer;
  FirstObsForScreenObject: Boolean;
//  ObservationFactors: TObservationFactors;
begin
  rdgObservationGroups.RowCount := Max(Observations.Count + 1,2);
  rdgObservationGroups.FixedCols := 1;
  rdgObservationGroups.FixedRows := 1;
  rdgObservationGroups.BeginUpdate;
  try
    for ObsevationIndex := 0 to Observations.Count - 1 do
    begin
      Observation := (Observations.Items[ObsevationIndex] as TCustomSutraFluxObservationGroup).ObservationGroup;
      rdgObservationGroups.Cells[Ord(ocLabel),ObsevationIndex+1] :=
        IntToStr(ObsevationIndex+1);
      rdgObservationGroups.Cells[Ord(ocName),ObsevationIndex+1] :=
        Observation.ObservationName;
//      ObservationFactors := Observation.ObservationFactors;
      GetObservationFactors(ObsevationIndex, Observation.ObservationFactors,
        ListOfScreenObjects);
    end;
  finally
    rdgObservationGroups.EndUpdate;
  end;

  result := 0;
  for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
  begin
    ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
    FirstObsForScreenObject := True;
    for ObsevationIndex := 0 to Observations.Count - 1 do
    begin
      Observation := (Observations.Items[ObsevationIndex]
        as TCustomSutraFluxObservationGroup).ObservationGroup;
      if Observation.ObservationFactors.
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

procedure TframeFluxObs.SetData(ListOfScreenObjects: TList;
  Observations: TCustomSutraFluxObservationGroups;
  ScreenObjectsUsed: TCheckBoxState);
var
  ObsevationIndex: Integer;
  Observation: TCustomSutraFluxObservations;
//  ObservationFactors: TObservationFactors;
begin
  if Observations.Count > 0 then
  begin
    Assert(rdgObservationGroups.RowCount = Observations.Count+1);
  end;
  for ObsevationIndex := 0 to Observations.Count - 1 do
  begin
    Observation:= (Observations.Items[ObsevationIndex]
      as TCustomSutraFluxObservationGroup).ObservationGroup;
    Assert(Observation.ObservationName =
      rdgObservationGroups.Cells[Ord(ocName),ObsevationIndex+1]);

//    ObservationFactors := Observation.ObservationFactors;
    SetObservationFactors(ObsevationIndex, Observation.ObservationFactors,
      ListOfScreenObjects, ScreenObjectsUsed);
  end;
end;

procedure TframeFluxObs.SetObservationFactors(ObsevationIndex: Integer;
  ObservationFactors: TObservationFactors; ListOfScreenObjects: TList;
  ScreenObjectsUsed: TCheckBoxState);
var
  ObsState: TCheckBoxState;
  ScreenObjectIndex: Integer;
  ScreenObject: Pointer;
  ObjectPosition: Integer;
  ObsFactor: TObservationFactor;
begin
  ObsState := rdgObservationGroups.CheckState[Ord(ocName), ObsevationIndex + 1];
  for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
  begin
    ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
    ObjectPosition := ObservationFactors.IndexOfScreenObject(ScreenObject);
    if (ScreenObjectsUsed = cbUnchecked) or (ObsState = cbUnchecked) then
    begin
      // remove
      if ObjectPosition >= 0 then
      begin
        ObservationFactors.Delete(ObjectPosition);
        ObjectPosition := -1;
      end;
    end
    else if (ScreenObjectsUsed = cbChecked) and (ObsState = cbChecked) then
    begin
      // add
      if ObjectPosition < 0 then
      begin
        ObjectPosition := ObservationFactors.AddObject(ScreenObject);
      end;
    end;
    if (ObjectPosition >= 0) and (rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex + 1] <> '') then
    begin
      ObsFactor := ObservationFactors[ObjectPosition];
      ObsFactor.Factor := rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex + 1];
    end;
  end;
end;

procedure TframeFluxObs.GetObservationFactors(ObsevationIndex: Integer;
  ObservationFactors: TObservationFactors; ListOfScreenObjects: TList);
var
  ScreenObjectPosition: Integer;
  ScreenObjectUsed: Boolean;
  ObsFactor: TObservationFactor;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
begin
  for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
  begin
    ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
    ScreenObjectPosition := ObservationFactors.IndexOfScreenObject(ScreenObject);
    ScreenObjectUsed := ScreenObjectPosition >= 0;
    if ScreenObjectIndex = 0 then
    begin
      rdgObservationGroups.Checked[Ord(ocName), ObsevationIndex + 1] :=
        ScreenObjectUsed;
      if ScreenObjectUsed then
      begin
        ObsFactor := ObservationFactors[ScreenObjectPosition];
        rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex + 1] :=
          ObsFactor.Factor;
      end
      else
      begin
        rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex + 1] := '';
      end;
    end
    else
    begin
      if rdgObservationGroups.State[Ord(ocName), ObsevationIndex + 1]
        <> cbGrayed then
      begin
        if rdgObservationGroups.Checked[Ord(ocName), ObsevationIndex + 1]
          <> ScreenObjectUsed then
        begin
          if ScreenObjectUsed then
          begin
            ObsFactor := ObservationFactors[ScreenObjectPosition];
            rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex + 1] :=
              ObsFactor.Factor;
          end;
          rdgObservationGroups.State[Ord(ocName), ObsevationIndex + 1]
            := cbGrayed;
        end;
      end
      else if ScreenObjectUsed then
      begin
        ObsFactor := ObservationFactors[ScreenObjectPosition];
        if rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex + 1]
          <> ObsFactor.Factor then
        begin
          rdgObservationGroups.Cells[Ord(ocFormula), ObsevationIndex + 1] := '';
        end;
      end;
    end;
  end;
end;

end.
