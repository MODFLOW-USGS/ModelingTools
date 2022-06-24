unit frameSfrGwtConcentrationsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtConcentrationsUnit,
  RbwParser, Vcl.StdCtrls, SsButtonEd, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4,
  UndoItemsScreenObjects, ScreenObjectUnit, System.Generics.Collections;

type
  TSfrConcColumns = (sccStart, sccEnd, sccStatus, sccSpecifiedConcentration,
    sccRainfall, sccEvaporation, sccRunoff, sccInflow);

  TframeSfrGwtConcentrations = class(TframeCustomGwtConcentrations)
    procedure rdgConcentrationsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FScreenObject: TScreenObject;
    { Private declarations }
  protected
    procedure InitializeControls;
  public
    procedure GetData(const List: TScreenObjectEditCollection;
      SpeciesIndex: Integer);
    procedure SetData(List: TScreenObjectEditCollection; SpeciesIndex: Integer);
    { Public declarations }
  end;

  TSfrGwtObjectList = TObjectList<TframeSfrGwtConcentrations>;

var
  frameSfrGwtConcentrations: TframeSfrGwtConcentrations;

implementation

uses
  ModflowSfr6Unit, GoPhastTypes;

{$R *.dfm}

resourcestring
  StrSpecifiedConcentrat = 'Specified Concentration';
  StrRainfallConcentrati = 'Rainfall Concentration';
  StrEvaporationConcentr = 'Evaporation Concentration';
  StrRunoffConcentration = 'Runoff Concentration';
  StrInflowConcentration = 'Inflow Concentration';

{ TframeSfrGwtConcentrations }

procedure TframeSfrGwtConcentrations.GetData(
  const List: TScreenObjectEditCollection; SpeciesIndex: Integer);
var
  FoundFirst: Boolean;
  StreamIndex: Integer;
  AStream: TSfrMf6Boundary;
  TimeIndex: Integer;
  AStreamItem: TSfrMf6Item;
begin
  InitializeControls;
  FDataAssigned := False;
  FoundFirst := False;
  for StreamIndex := 0 to List.Count - 1 do
  begin
    FScreenObject := List[StreamIndex].ScreenObject;
    AStream := FScreenObject.ModflowSfr6Boundary;
    if (AStream <> nil) and AStream.Used then
    begin
      if not FoundFirst then
      begin
        FoundFirst := True;
        btnedInitialConcentration.Text := AStream.StartingConcentrations[SpeciesIndex].Value;

        rdgConcentrations.RowCount := AStream.Values.Count + PestRowOffset + 1;

        PestModifier[Ord(sccSpecifiedConcentration)] := AStream.PestSpecifiedConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(sccRainfall)] := AStream.PestRainfallConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(sccEvaporation)] := AStream.PestEvaporationConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(sccRunoff)] := AStream.PestRunoffConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(sccInflow)] := AStream.PestInflowConcentrations[SpeciesIndex].Value;

        PestMethod[Ord(sccSpecifiedConcentration)] := AStream.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(sccRainfall)] := AStream.PestRainfallConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(sccEvaporation)] := AStream.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(sccRunoff)] := AStream.PestRunoffConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(sccInflow)] := AStream.PestInflowConcentrationMethods[SpeciesIndex].PestParamMethod;

        for TimeIndex := 0 to AStream.Values.Count - 1 do
        begin
          AStreamItem := AStream.Values[TimeIndex] as TSfrMf6Item;

          rdgConcentrations.RealValue[Ord(sccStart), TimeIndex+1+PestRowOffset] := AStreamItem.StartTime;
          rdgConcentrations.RealValue[Ord(sccEnd), TimeIndex+1+PestRowOffset] := AStreamItem.EndTime;
          rdgConcentrations.ItemIndex[Ord(sccStatus), TimeIndex+1+PestRowOffset] := Ord(AStreamItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus);
          rdgConcentrations.Cells[Ord(sccSpecifiedConcentration), TimeIndex+1+PestRowOffset] := AStreamItem.SpecifiedConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(sccRainfall), TimeIndex+1+PestRowOffset] := AStreamItem.RainfallConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(sccEvaporation), TimeIndex+1+PestRowOffset] := AStreamItem.EvapConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(sccRunoff), TimeIndex+1+PestRowOffset] := AStreamItem.RunoffConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(sccInflow), TimeIndex+1+PestRowOffset] := AStreamItem.InflowConcentrations[SpeciesIndex].Value;
        end;
      end
      else
      begin
        if rdgConcentrations.RowCount <> AStream.Values.Count + PestRowOffset + 1 then
        begin
          InitializeControls;
          Exit;
        end;

        if btnedInitialConcentration.Text <> AStream.StartingConcentrations[SpeciesIndex].Value then
        begin
          btnedInitialConcentration.Text := '';
        end;

        if PestModifier[Ord(sccSpecifiedConcentration)] <> AStream.PestSpecifiedConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(sccSpecifiedConcentration)] := False;
        end;

        if PestModifier[Ord(sccRainfall)] <> AStream.PestRainfallConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(sccRainfall)] := False;
        end;

        if PestModifier[Ord(sccEvaporation)] <> AStream.PestEvaporationConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(sccEvaporation)] := False;
        end;

        if PestModifier[Ord(sccRunoff)] <> AStream.PestRunoffConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(sccRunoff)] := False;
        end;

        if PestModifier[Ord(sccInflow)] <> AStream.PestInflowConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(sccInflow)] := False;
        end;

        if PestMethod[Ord(sccSpecifiedConcentration)] <> AStream.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(sccSpecifiedConcentration)] := False;
        end;

        if PestMethod[Ord(sccRainfall)] <> AStream.PestRainfallConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(sccRainfall)] := False;
        end;

        if PestMethod[Ord(sccEvaporation)] <> AStream.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(sccEvaporation)] := False;
        end;

        if PestMethod[Ord(sccRunoff)] <> AStream.PestRunoffConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(sccRunoff)] := False;
        end;

        if PestMethod[Ord(sccInflow)] <> AStream.PestInflowConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(sccInflow)] := False;
        end;

        for TimeIndex := 0 to AStream.Values.Count - 1 do
        begin
          AStreamItem := AStream.Values[TimeIndex] as TSfrMf6Item;

          if rdgConcentrations.Cells[Ord(sccStart), TimeIndex+1+PestRowOffset] <> FloatToStr(AStreamItem.StartTime) then
          begin
            InitializeControls;
            Exit;
          end;
          if rdgConcentrations.Cells[Ord(sccEnd), TimeIndex+1+PestRowOffset] <> FloatToStr(AStreamItem.EndTime) then
          begin
            InitializeControls;
            Exit;
          end;
          if rdgConcentrations.ItemIndex[Ord(sccStatus), TimeIndex+1+PestRowOffset] <> Ord(AStreamItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus) then
          begin
            rdgConcentrations.ItemIndex[Ord(sccStatus), TimeIndex+1+PestRowOffset] := -1;
            Exit;
          end;
          if rdgConcentrations.Cells[Ord(sccSpecifiedConcentration), TimeIndex+1+PestRowOffset] <> AStreamItem.SpecifiedConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(sccSpecifiedConcentration), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(sccRainfall), TimeIndex+1+PestRowOffset] <> AStreamItem.RainfallConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(sccRainfall), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(sccEvaporation), TimeIndex+1+PestRowOffset] <> AStreamItem.EvapConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(sccEvaporation), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(sccRunoff), TimeIndex+1+PestRowOffset] <> AStreamItem.RunoffConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(sccRunoff), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(sccInflow), TimeIndex+1+PestRowOffset] <> AStreamItem.InflowConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(sccInflow), TimeIndex+1+PestRowOffset] := '';
          end;
        end;
      end;
    end;
  end;
  FDataAssigned := True;
end;

procedure TframeSfrGwtConcentrations.InitializeControls;
begin
  rdgConcentrations.BeginUpdate;
  try
    inherited;
    rdgConcentrations.Cells[Ord(sccSpecifiedConcentration), 0] := StrSpecifiedConcentrat;
    rdgConcentrations.Cells[Ord(sccRainfall), 0] := StrRainfallConcentrati;
    rdgConcentrations.Cells[Ord(sccEvaporation), 0] := StrEvaporationConcentr;
    rdgConcentrations.Cells[Ord(sccRunoff), 0] := StrRunoffConcentration;
    rdgConcentrations.Cells[Ord(sccInflow), 0] := StrInflowConcentration;
  finally
    rdgConcentrations.EndUpdate;
  end;
end;

procedure TframeSfrGwtConcentrations.rdgConcentrationsSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  AStatus: TGwtBoundaryStatus;
begin
  inherited;
  if (ARow >= rdgConcentrations.FixedRows + PestRowOffset)
    and (ACol > Ord(sccStatus)) then
  begin
    if rdgConcentrations.ItemIndex[Ord(sccStatus), ARow] >= 0 then
    begin
      AStatus :=
        TGwtBoundaryStatus(rdgConcentrations.ItemIndex[Ord(sccStatus), ARow]);
      if ACol = Ord(sccSpecifiedConcentration) then
      begin
        CanSelect := AStatus = gbsConstant;
      end
      else
      begin
        CanSelect := AStatus = gbsActive;
      end;
    end;
  end;
end;

procedure TframeSfrGwtConcentrations.SetData(List: TScreenObjectEditCollection;
  SpeciesIndex: Integer);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  AStream: TSfrMf6Boundary;
  TimeIndex: Integer;
  AStreamItem: TSfrMf6Item;
  RowIndex: Integer;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    AStream := ScreenObject.ModflowSfr6Boundary;
    if (AStream <> nil) and AStream.Used then
    begin
      if btnedInitialConcentration.Text <> '' then
      begin
        AStream.StartingConcentrations[SpeciesIndex].Value := btnedInitialConcentration.Text;
      end
      else if AStream.StartingConcentrations[SpeciesIndex].Value = '' then
      begin
        AStream.StartingConcentrations[SpeciesIndex].Value := '0';
      end;

      if PestModifierAssigned[Ord(sccSpecifiedConcentration)] then
      begin
        AStream.PestSpecifiedConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(sccSpecifiedConcentration)];
      end;
      if PestModifierAssigned[Ord(sccRainfall)] then
      begin
        AStream.PestRainfallConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(sccRainfall)];
      end;
      if PestModifierAssigned[Ord(sccEvaporation)] then
      begin
        AStream.PestEvaporationConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(sccEvaporation)];
      end;
      if PestModifierAssigned[Ord(sccRunoff)] then
      begin
        AStream.PestRunoffConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(sccRunoff)];
      end;
      if PestModifierAssigned[Ord(sccInflow)] then
      begin
        AStream.PestInflowConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(sccInflow)];
      end;

      if FDataAssigned and PestMethodAssigned[Ord(sccSpecifiedConcentration)] then
      begin
        AStream.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(sccSpecifiedConcentration)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(sccRainfall)] then
      begin
        AStream.PestRainfallConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(sccRainfall)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(sccEvaporation)] then
      begin
        AStream.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(sccEvaporation)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(sccRunoff)] then
      begin
        AStream.PestRunoffConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(sccRunoff)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(sccInflow)] then
      begin
        AStream.PestInflowConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(sccInflow)];
      end;
    end;

    if FDataAssigned then
    begin
      for TimeIndex := 0 to AStream.Values.Count - 1 do
      begin
        AStreamItem := AStream.Values[TimeIndex] as TSfrMf6Item;
        RowIndex := TimeIndex+1+PestRowOffset;

        if rdgConcentrations.ItemIndex[Ord(sccStatus), RowIndex] >= 0 then
        begin
          AStreamItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus :=
            TGwtBoundaryStatus(rdgConcentrations.ItemIndex[Ord(sccStatus), RowIndex]);
        end;
        if rdgConcentrations.Cells[Ord(sccSpecifiedConcentration), RowIndex] <> '' then
        begin
          AStreamItem.SpecifiedConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(sccSpecifiedConcentration), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(sccRainfall), RowIndex] <> '' then
        begin
          AStreamItem.RainfallConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(sccRainfall), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(sccEvaporation), RowIndex] <> '' then
        begin
          AStreamItem.EvapConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(sccEvaporation), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(sccRunoff), RowIndex] <> '' then
        begin
          AStreamItem.RunoffConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(sccRunoff), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(sccInflow), RowIndex] <> '' then
        begin
          AStreamItem.InflowConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(sccInflow), RowIndex];
        end;
      end;
    end;
  end;
end;

end.
