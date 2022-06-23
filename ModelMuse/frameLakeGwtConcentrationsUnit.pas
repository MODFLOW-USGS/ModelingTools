unit frameLakeGwtConcentrationsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtConcentrationsUnit,
  Vcl.Grids, RbwDataGrid4, UndoItemsScreenObjects, ScreenObjectUnit,
  Vcl.ExtCtrls, Vcl.StdCtrls, SsButtonEd, System.Generics.Collections;

type
  TLakeConcColumns = (lccStart, lccEnd, lccStatus, lccSpecifiedConcentration,
    lccRainfall, lccEvaporation, lccRunoff, lccInflow);


  TframeLakeGwtConcentrations = class(TframeCustomGwtConcentrations)
  private
    FScreenObject: TScreenObject;
    { Private declarations }
  protected
    procedure InitializeControls;
    function GetPestModifiers: TStringList; override;
  public
    procedure GetData(const List: TScreenObjectEditCollection;
      SpeciesIndex: Integer);
    procedure SetData(List: TScreenObjectEditCollection; SpeciesIndex: Integer);
    { Public declarations }

  end;

  TLakeGwtObjectList = TObjectList<TframeLakeGwtConcentrations>;

var
  frameLakeGwtConcentrations: TframeLakeGwtConcentrations;

implementation

uses
  ModflowLakMf6Unit, GoPhastTypes;



{$R *.dfm}

{ TframeLakeGwtConcentrations }

procedure TframeLakeGwtConcentrations.GetData(
  const List: TScreenObjectEditCollection; SpeciesIndex: Integer);
var
  FoundFirst: Boolean;
  FirstLake: TLakeMf6;
  LakeIndex: Integer;
  ALake: TLakeMf6;
  TimeIndex: Integer;
  ALakeItem: TLakeTimeItem;
begin
  InitializeControls;
  FDataAssigned := False;
  FoundFirst := False;
  FirstLake := nil;
  for LakeIndex := 0 to List.Count - 1 do
  begin
    FScreenObject := List[LakeIndex].ScreenObject;
    ALake := FScreenObject.ModflowLak6;
    if ALake <> nil then
    begin
      if not FoundFirst then
      begin
        FirstLake := ALake;

        btnedInitialConcentration.Text := ALake.StartingConcentrations[SpeciesIndex].Value;

        rdgConcentrations.RowCount := ALake.Values.Count + PestRowOffset + 1;

        PestModifier[Ord(lccSpecifiedConcentration)] := ALake.PestSpecifiedConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(lccRainfall)] := ALake.PestRainfallConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(lccEvaporation)] := ALake.PestEvaporationConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(lccRunoff)] := ALake.PestRunoffConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(lccInflow)] := ALake.PestInflowConcentrations[SpeciesIndex].Value;

        PestMethod[Ord(lccSpecifiedConcentration)] := ALake.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(lccRainfall)] := ALake.PestRainfallConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(lccEvaporation)] := ALake.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(lccRunoff)] := ALake.PestRunoffConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(lccInflow)] := ALake.PestInflowConcentrationMethods[SpeciesIndex].PestParamMethod;

        for TimeIndex := 0 to ALake.Values.Count - 1 do
        begin
          ALakeItem := ALake.Values[TimeIndex] as TLakeTimeItem;

          rdgConcentrations.RealValue[Ord(lccStart), TimeIndex+1+PestRowOffset] := ALakeItem.StartTime;
          rdgConcentrations.RealValue[Ord(lccEnd), TimeIndex+1+PestRowOffset] := ALakeItem.EndTime;
          rdgConcentrations.ItemIndex[Ord(lccStatus), TimeIndex+1+PestRowOffset] := Ord(ALakeItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus);
          rdgConcentrations.Cells[Ord(lccSpecifiedConcentration), TimeIndex+1+PestRowOffset] := ALakeItem.SpecifiedConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(lccRainfall), TimeIndex+1+PestRowOffset] := ALakeItem.RainfallConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(lccEvaporation), TimeIndex+1+PestRowOffset] := ALakeItem.EvapConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(lccRunoff), TimeIndex+1+PestRowOffset] := ALakeItem.RunoffConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(lccInflow), TimeIndex+1+PestRowOffset] := ALakeItem.InflowConcentrations[SpeciesIndex].Value;
        end;
      end
      else
      begin
        if rdgConcentrations.RowCount <> ALake.Values.Count + PestRowOffset + 1 then
        begin
          InitializeControls;
          Exit;
        end;

        if btnedInitialConcentration.Text <> ALake.StartingConcentrations[SpeciesIndex].Value then
        begin
          btnedInitialConcentration.Text := '';
        end;

        if PestModifier[Ord(lccSpecifiedConcentration)] <> ALake.PestSpecifiedConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(lccSpecifiedConcentration)] := False;
        end;

        if PestModifier[Ord(lccRainfall)] <> ALake.PestRainfallConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(lccRainfall)] := False;
        end;

        if PestModifier[Ord(lccEvaporation)] <> ALake.PestEvaporationConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(lccEvaporation)] := False;
        end;

        if PestModifier[Ord(lccRunoff)] <> ALake.PestRunoffConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(lccRunoff)] := False;
        end;

        if PestModifier[Ord(lccInflow)] <> ALake.PestInflowConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(lccInflow)] := False;
        end;

        if PestMethod[Ord(lccSpecifiedConcentration)] <> ALake.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(lccSpecifiedConcentration)] := False;
        end;

        if PestMethod[Ord(lccRainfall)] <> ALake.PestRainfallConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(lccRainfall)] := False;
        end;

        if PestMethod[Ord(lccEvaporation)] <> ALake.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(lccEvaporation)] := False;
        end;

        if PestMethod[Ord(lccRunoff)] <> ALake.PestRunoffConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(lccRunoff)] := False;
        end;

        if PestMethod[Ord(lccInflow)] <> ALake.PestInflowConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(lccInflow)] := False;
        end;

        for TimeIndex := 0 to ALake.Values.Count - 1 do
        begin
          ALakeItem := ALake.Values[TimeIndex] as TLakeTimeItem;

          if rdgConcentrations.Cells[Ord(lccStart), TimeIndex+1+PestRowOffset] <> FloatToStr(ALakeItem.StartTime) then
          begin
            InitializeControls;
            Exit;
          end;
          if rdgConcentrations.Cells[Ord(lccEnd), TimeIndex+1+PestRowOffset] <> FloatToStr(ALakeItem.EndTime) then
          begin
            InitializeControls;
            Exit;
          end;
          if rdgConcentrations.ItemIndex[Ord(lccEnd), TimeIndex+1+PestRowOffset] <> Ord(ALakeItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus) then
          begin
            rdgConcentrations.ItemIndex[Ord(lccEnd), TimeIndex+1+PestRowOffset] := -1;
            Exit;
          end;
          if rdgConcentrations.Cells[Ord(lccSpecifiedConcentration), TimeIndex+1+PestRowOffset] <> ALakeItem.SpecifiedConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(lccSpecifiedConcentration), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(lccRainfall), TimeIndex+1+PestRowOffset] <> ALakeItem.RainfallConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(lccRainfall), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(lccEvaporation), TimeIndex+1+PestRowOffset] <> ALakeItem.EvapConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(lccEvaporation), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(lccRunoff), TimeIndex+1+PestRowOffset] <> ALakeItem.RunoffConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(lccRunoff), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(lccInflow), TimeIndex+1+PestRowOffset] <> ALakeItem.InflowConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(lccInflow), TimeIndex+1+PestRowOffset] := '';
          end;
        end;
      end;
    end;
  end;
  FDataAssigned := True;
end;

function TframeLakeGwtConcentrations.GetPestModifiers: TStringList;
begin
  result := FPestParameters;
end;

procedure TframeLakeGwtConcentrations.InitializeControls;
begin
  rdgConcentrations.BeginUpdate;
  try
    inherited;
    rdgConcentrations.Cells[Ord(lccSpecifiedConcentration), 0] := 'Specified Concentration';
    rdgConcentrations.Cells[Ord(lccRainfall), 0] := 'Rainfall Concentration';
    rdgConcentrations.Cells[Ord(lccEvaporation), 0] := 'Evaporation Concentration';
    rdgConcentrations.Cells[Ord(lccRunoff), 0] := 'Runoff Concentration';
    rdgConcentrations.Cells[Ord(lccInflow), 0] := 'Inflow Concentration';
  finally
    rdgConcentrations.EndUpdate;
  end;
end;

procedure TframeLakeGwtConcentrations.SetData(List: TScreenObjectEditCollection;
  SpeciesIndex: Integer);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  ALake: TLakeMf6;
  TimeIndex: Integer;
  ALakeItem: TLakeTimeItem;
  RowIndex: Integer;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    ALake := ScreenObject.ModflowLak6;
    if ALake <> nil then
    begin
      if btnedInitialConcentration.Text <> '' then
      begin
        ALake.StartingConcentrations[SpeciesIndex].Value := btnedInitialConcentration.Text;
      end
      else if not FDataAssigned then
      begin
        ALake.StartingConcentrations[SpeciesIndex].Value := '0';
      end;

      if PestModifierAssigned[Ord(lccSpecifiedConcentration)] then
      begin
        ALake.PestSpecifiedConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(lccSpecifiedConcentration)];
      end;
      if PestModifierAssigned[Ord(lccRainfall)] then
      begin
        ALake.PestRainfallConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(lccRainfall)];
      end;
      if PestModifierAssigned[Ord(lccEvaporation)] then
      begin
        ALake.PestEvaporationConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(lccEvaporation)];
      end;
      if PestModifierAssigned[Ord(lccRunoff)] then
      begin
        ALake.PestRunoffConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(lccRunoff)];
      end;
      if PestModifierAssigned[Ord(lccInflow)] then
      begin
        ALake.PestInflowConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(lccInflow)];
      end;

      if FDataAssigned and PestMethodAssigned[Ord(lccSpecifiedConcentration)] then
      begin
        ALake.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(lccSpecifiedConcentration)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(lccRainfall)] then
      begin
        ALake.PestRainfallConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(lccRainfall)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(lccEvaporation)] then
      begin
        ALake.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(lccEvaporation)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(lccRunoff)] then
      begin
        ALake.PestRunoffConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(lccRunoff)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(lccInflow)] then
      begin
        ALake.PestInflowConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(lccInflow)];
      end;
    end;

    if FDataAssigned then
    begin
      for TimeIndex := 0 to ALake.Values.Count - 1 do
      begin
        ALakeItem := ALake.Values[TimeIndex] as TLakeTimeItem;
        RowIndex := TimeIndex+1+PestRowOffset;

        if rdgConcentrations.ItemIndex[Ord(lccStatus), RowIndex] >= 0 then
        begin
          ALakeItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus :=
            TGwtBoundaryStatus(rdgConcentrations.ItemIndex[Ord(lccStatus), RowIndex]);
        end;
        if rdgConcentrations.Cells[Ord(lccSpecifiedConcentration), RowIndex] <> '' then
        begin
          ALakeItem.SpecifiedConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(lccSpecifiedConcentration), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(lccRainfall), RowIndex] <> '' then
        begin
          ALakeItem.RainfallConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(lccRainfall), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(lccEvaporation), RowIndex] <> '' then
        begin
          ALakeItem.EvapConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(lccEvaporation), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(lccRunoff), RowIndex] <> '' then
        begin
          ALakeItem.RunoffConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(lccRunoff), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(lccInflow), RowIndex] <> '' then
        begin
          ALakeItem.InflowConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(lccInflow), RowIndex];
        end;
      end;
    end;
  end;
end;

end.
