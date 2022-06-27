unit frameUzfGwtConcentrationsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtConcentrationsUnit,
  RbwParser, Vcl.StdCtrls, SsButtonEd, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4,
  UndoItemsScreenObjects, ScreenObjectUnit, System.Generics.Collections;

type
  TUzfConcColumns = (uccStart, uccEnd, uccStatus, uccSpecifiedConcentration,
    uccInfiltration, uccEvapotranspiration);

  TframeUzfGwtConcentrations = class(TframeCustomGwtConcentrations)
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

  TUzfGwtObjectList = TObjectList<TframeUzfGwtConcentrations>;

var
  frameUzfGwtConcentrations: TframeUzfGwtConcentrations;

implementation

uses
  ModflowUzfMf6Unit, GoPhastTypes;

{$R *.dfm}

resourcestring
  StrSpecifiedConcentrat = 'Specified Concentration';
  StrInfiltrationConcentrati = 'Infiltration Concentration';
  StrEvapotranspirationConcentr = 'Evapotranspiration Concentration';


procedure TframeUzfGwtConcentrations.GetData(
  const List: TScreenObjectEditCollection; SpeciesIndex: Integer);
var
  FoundFirst: Boolean;
  StreamIndex: Integer;
  UzfMf6Boundary: TUzfMf6Boundary;
  TimeIndex: Integer;
  AUzfItem: TUzfMf6Item;
begin
  InitializeControls;
  FDataAssigned := False;
  FoundFirst := False;
  for StreamIndex := 0 to List.Count - 1 do
  begin
    FScreenObject := List[StreamIndex].ScreenObject;
    UzfMf6Boundary := FScreenObject.ModflowUzfMf6Boundary;
    if (UzfMf6Boundary <> nil) and UzfMf6Boundary.Used then
    begin
      if not FoundFirst then
      begin
        FoundFirst := True;
        btnedInitialConcentration.Text := UzfMf6Boundary.StartingConcentrations[SpeciesIndex].Value;

        rdgConcentrations.RowCount := UzfMf6Boundary.Values.Count + PestRowOffset + 1;

        PestModifier[Ord(uccSpecifiedConcentration)] := UzfMf6Boundary.PestSpecifiedConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(uccInfiltration)] := UzfMf6Boundary.PestInfiltrationConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(uccEvapotranspiration)] := UzfMf6Boundary.PestEvaporationConcentrations[SpeciesIndex].Value;

        PestMethod[Ord(uccSpecifiedConcentration)] := UzfMf6Boundary.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(uccInfiltration)] := UzfMf6Boundary.PestInfiltrationConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(uccEvapotranspiration)] := UzfMf6Boundary.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod;

        for TimeIndex := 0 to UzfMf6Boundary.Values.Count - 1 do
        begin
          AUzfItem := UzfMf6Boundary.Values[TimeIndex] as TUzfMf6Item;

          rdgConcentrations.RealValue[Ord(uccStart), TimeIndex+1+PestRowOffset] := AUzfItem.StartTime;
          rdgConcentrations.RealValue[Ord(uccEnd), TimeIndex+1+PestRowOffset] := AUzfItem.EndTime;
          rdgConcentrations.ItemIndex[Ord(uccStatus), TimeIndex+1+PestRowOffset] := Ord(AUzfItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus);
          rdgConcentrations.Cells[Ord(uccSpecifiedConcentration), TimeIndex+1+PestRowOffset] := AUzfItem.SpecifiedConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(uccInfiltration), TimeIndex+1+PestRowOffset] := AUzfItem.InfiltrationConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(uccEvapotranspiration), TimeIndex+1+PestRowOffset] := AUzfItem.EvapConcentrations[SpeciesIndex].Value;
        end;
      end
      else
      begin
        if rdgConcentrations.RowCount <> UzfMf6Boundary.Values.Count + PestRowOffset + 1 then
        begin
          InitializeControls;
          Exit;
        end;

        if btnedInitialConcentration.Text <> UzfMf6Boundary.StartingConcentrations[SpeciesIndex].Value then
        begin
          btnedInitialConcentration.Text := '';
        end;

        if PestModifier[Ord(uccSpecifiedConcentration)] <> UzfMf6Boundary.PestSpecifiedConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(uccSpecifiedConcentration)] := False;
        end;

        if PestModifier[Ord(uccInfiltration)] <> UzfMf6Boundary.PestInfiltrationConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(uccInfiltration)] := False;
        end;

        if PestModifier[Ord(uccEvapotranspiration)] <> UzfMf6Boundary.PestEvaporationConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(uccEvapotranspiration)] := False;
        end;

        if PestMethod[Ord(uccSpecifiedConcentration)] <> UzfMf6Boundary.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(uccSpecifiedConcentration)] := False;
        end;

        if PestMethod[Ord(uccInfiltration)] <> UzfMf6Boundary.PestInfiltrationConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(uccInfiltration)] := False;
        end;

        if PestMethod[Ord(uccEvapotranspiration)] <> UzfMf6Boundary.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(uccEvapotranspiration)] := False;
        end;

        for TimeIndex := 0 to UzfMf6Boundary.Values.Count - 1 do
        begin
          AUzfItem := UzfMf6Boundary.Values[TimeIndex] as TUzfMf6Item;

          if rdgConcentrations.Cells[Ord(uccStart), TimeIndex+1+PestRowOffset] <> FloatToStr(AUzfItem.StartTime) then
          begin
            InitializeControls;
            Exit;
          end;
          if rdgConcentrations.Cells[Ord(uccEnd), TimeIndex+1+PestRowOffset] <> FloatToStr(AUzfItem.EndTime) then
          begin
            InitializeControls;
            Exit;
          end;
          if rdgConcentrations.ItemIndex[Ord(uccStatus), TimeIndex+1+PestRowOffset] <> Ord(AUzfItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus) then
          begin
            rdgConcentrations.ItemIndex[Ord(uccStatus), TimeIndex+1+PestRowOffset] := -1;
            Exit;
          end;
          if rdgConcentrations.Cells[Ord(uccSpecifiedConcentration), TimeIndex+1+PestRowOffset] <> AUzfItem.SpecifiedConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(uccSpecifiedConcentration), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(uccInfiltration), TimeIndex+1+PestRowOffset] <> AUzfItem.InfiltrationConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(uccInfiltration), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(uccEvapotranspiration), TimeIndex+1+PestRowOffset] <> AUzfItem.EvapConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(uccEvapotranspiration), TimeIndex+1+PestRowOffset] := '';
          end;
        end;
      end;
    end;
  end;
  FDataAssigned := True;
end;

procedure TframeUzfGwtConcentrations.InitializeControls;
begin
  rdgConcentrations.BeginUpdate;
  try
    inherited;
    rdgConcentrations.Cells[Ord(uccSpecifiedConcentration), 0] := StrSpecifiedConcentrat;
    rdgConcentrations.Cells[Ord(uccInfiltration), 0] := StrInfiltrationConcentrati;
    rdgConcentrations.Cells[Ord(uccEvapotranspiration), 0] := StrEvapotranspirationConcentr;
  finally
    rdgConcentrations.EndUpdate;
  end;
end;

procedure TframeUzfGwtConcentrations.rdgConcentrationsSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  AStatus: TGwtBoundaryStatus;
begin
  inherited;
  if (ARow >= rdgConcentrations.FixedRows + PestRowOffset)
    and (ACol > Ord(uccStatus)) then
  begin
    if rdgConcentrations.ItemIndex[Ord(uccStatus), ARow] >= 0 then
    begin
      AStatus :=
        TGwtBoundaryStatus(rdgConcentrations.ItemIndex[Ord(uccStatus), ARow]);
      if ACol = Ord(uccSpecifiedConcentration) then
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

procedure TframeUzfGwtConcentrations.SetData(List: TScreenObjectEditCollection;
  SpeciesIndex: Integer);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  AUzfBoundary: TUzfMf6Boundary;
  TimeIndex: Integer;
  AUzfItem: TUzfMf6Item;
  RowIndex: Integer;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    AUzfBoundary := ScreenObject.ModflowUzfMf6Boundary;
    if (AUzfBoundary <> nil) and AUzfBoundary.Used then
    begin
      if btnedInitialConcentration.Text <> '' then
      begin
        AUzfBoundary.StartingConcentrations[SpeciesIndex].Value := btnedInitialConcentration.Text;
      end
      else if AUzfBoundary.StartingConcentrations[SpeciesIndex].Value = '' then
      begin
        AUzfBoundary.StartingConcentrations[SpeciesIndex].Value := '0';
      end;

      if PestModifierAssigned[Ord(uccSpecifiedConcentration)] then
      begin
        AUzfBoundary.PestSpecifiedConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(uccSpecifiedConcentration)];
      end;
      if PestModifierAssigned[Ord(uccInfiltration)] then
      begin
        AUzfBoundary.PestInfiltrationConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(uccInfiltration)];
      end;
      if PestModifierAssigned[Ord(uccEvapotranspiration)] then
      begin
        AUzfBoundary.PestEvaporationConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(uccEvapotranspiration)];
      end;

      if FDataAssigned and PestMethodAssigned[Ord(uccSpecifiedConcentration)] then
      begin
        AUzfBoundary.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(uccSpecifiedConcentration)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(uccInfiltration)] then
      begin
        AUzfBoundary.PestInfiltrationConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(uccInfiltration)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(uccEvapotranspiration)] then
      begin
        AUzfBoundary.PestEvaporationConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(uccEvapotranspiration)];
      end;
    end;

    if FDataAssigned then
    begin
      for TimeIndex := 0 to AUzfBoundary.Values.Count - 1 do
      begin
        AUzfItem := AUzfBoundary.Values[TimeIndex] as TUzfMf6Item;
        RowIndex := TimeIndex+1+PestRowOffset;

        if rdgConcentrations.ItemIndex[Ord(uccStatus), RowIndex] >= 0 then
        begin
          AUzfItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus :=
            TGwtBoundaryStatus(rdgConcentrations.ItemIndex[Ord(uccStatus), RowIndex]);
        end;
        if rdgConcentrations.Cells[Ord(uccSpecifiedConcentration), RowIndex] <> '' then
        begin
          AUzfItem.SpecifiedConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(uccSpecifiedConcentration), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(uccInfiltration), RowIndex] <> '' then
        begin
          AUzfItem.InfiltrationConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(uccInfiltration), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(uccEvapotranspiration), RowIndex] <> '' then
        begin
          AUzfItem.EvapConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(uccEvapotranspiration), RowIndex];
        end;
      end;
    end;
  end;
end;

end.
