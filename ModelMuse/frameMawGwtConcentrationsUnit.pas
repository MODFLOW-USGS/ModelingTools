unit frameMawGwtConcentrationsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameCustomGwtConcentrationsUnit,
  RbwParser, Vcl.StdCtrls, SsButtonEd, Vcl.ExtCtrls, Vcl.Grids, RbwDataGrid4,
  ScreenObjectUnit, UndoItemsScreenObjects, System.Generics.Collections;

type
  TMawConcColumns = (mccStart, mccEnd, mccStatus, mccSpecifiedConcentration,
    mccInjection);

  TframeMawGwtConcentrations = class(TframeCustomGwtConcentrations)
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

  TMawGwtObjectList = TObjectList<TframeMawGwtConcentrations>;

var
  frameMawGwtConcentrations: TframeMawGwtConcentrations;

implementation

uses
  ModflowMawUnit, GoPhastTypes;

{$R *.dfm}

{ TframeMawGwtConcentrations }

procedure TframeMawGwtConcentrations.GetData(
  const List: TScreenObjectEditCollection; SpeciesIndex: Integer);
var
  FoundFirst: Boolean;
  MawIndex: Integer;
  MultAqWell: TMawBoundary;
  TimeIndex: Integer;
  AMawItem: TMawItem;
begin
  InitializeControls;
  FDataAssigned := False;
  FoundFirst := False;
  for MawIndex := 0 to List.Count - 1 do
  begin
    FScreenObject := List[MawIndex].ScreenObject;
    MultAqWell := FScreenObject.ModflowMawBoundary;
    if (MultAqWell <> nil) and MultAqWell.Used then
    begin
      if not FoundFirst then
      begin
        FoundFirst := True;
        btnedInitialConcentration.Text := MultAqWell.StartingConcentrations[SpeciesIndex].Value;

        rdgConcentrations.RowCount := MultAqWell.Values.Count + PestRowOffset + 1;

        PestModifier[Ord(mccSpecifiedConcentration)] := MultAqWell.PestSpecifiedConcentrations[SpeciesIndex].Value;
        PestModifier[Ord(mccInjection)] := MultAqWell.PestInjectionConcentrations[SpeciesIndex].Value;

        PestMethod[Ord(mccSpecifiedConcentration)] := MultAqWell.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod;
        PestMethod[Ord(mccInjection)] := MultAqWell.PestInjectionConcentrationMethods[SpeciesIndex].PestParamMethod;

        for TimeIndex := 0 to MultAqWell.Values.Count - 1 do
        begin
          AMawItem := MultAqWell.Values[TimeIndex] as TMawItem;

          rdgConcentrations.RealValue[Ord(mccStart), TimeIndex+1+PestRowOffset] := AMawItem.StartTime;
          rdgConcentrations.RealValue[Ord(mccEnd), TimeIndex+1+PestRowOffset] := AMawItem.EndTime;
          rdgConcentrations.ItemIndex[Ord(mccStatus), TimeIndex+1+PestRowOffset] := Ord(AMawItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus);
          rdgConcentrations.Cells[Ord(mccSpecifiedConcentration), TimeIndex+1+PestRowOffset] := AMawItem.SpecifiedConcentrations[SpeciesIndex].Value;
          rdgConcentrations.Cells[Ord(mccInjection), TimeIndex+1+PestRowOffset] := AMawItem.InjectionConcentrations[SpeciesIndex].Value;
        end;
      end
      else
      begin
        if rdgConcentrations.RowCount <> MultAqWell.Values.Count + PestRowOffset + 1 then
        begin
          InitializeControls;
          Exit;
        end;

        if btnedInitialConcentration.Text <> MultAqWell.StartingConcentrations[SpeciesIndex].Value then
        begin
          btnedInitialConcentration.Text := '';
        end;

        if PestModifier[Ord(mccSpecifiedConcentration)] <> MultAqWell.PestSpecifiedConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(mccSpecifiedConcentration)] := False;
        end;

        if PestModifier[Ord(mccInjection)] <> MultAqWell.PestInjectionConcentrations[SpeciesIndex].Value then
        begin
          PestModifierAssigned[Ord(mccInjection)] := False;
        end;

        if PestMethod[Ord(mccSpecifiedConcentration)] <> MultAqWell.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(mccSpecifiedConcentration)] := False;
        end;

        if PestMethod[Ord(mccInjection)] <> MultAqWell.PestInjectionConcentrationMethods[SpeciesIndex].PestParamMethod then
        begin
          PestMethodAssigned[Ord(mccInjection)] := False;
        end;

        for TimeIndex := 0 to MultAqWell.Values.Count - 1 do
        begin
          AMawItem := MultAqWell.Values[TimeIndex] as TMawItem;

          if rdgConcentrations.Cells[Ord(mccStart), TimeIndex+1+PestRowOffset] <> FloatToStr(AMawItem.StartTime) then
          begin
            InitializeControls;
            Exit;
          end;
          if rdgConcentrations.Cells[Ord(mccEnd), TimeIndex+1+PestRowOffset] <> FloatToStr(AMawItem.EndTime) then
          begin
            InitializeControls;
            Exit;
          end;
          if rdgConcentrations.ItemIndex[Ord(mccStatus), TimeIndex+1+PestRowOffset] <> Ord(AMawItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus) then
          begin
            rdgConcentrations.ItemIndex[Ord(mccStatus), TimeIndex+1+PestRowOffset] := -1;
            Exit;
          end;
          if rdgConcentrations.Cells[Ord(mccSpecifiedConcentration), TimeIndex+1+PestRowOffset] <> AMawItem.SpecifiedConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(mccSpecifiedConcentration), TimeIndex+1+PestRowOffset] := '';
          end;
          if rdgConcentrations.Cells[Ord(mccInjection), TimeIndex+1+PestRowOffset] <> AMawItem.InjectionConcentrations[SpeciesIndex].Value then
          begin
            rdgConcentrations.Cells[Ord(mccInjection), TimeIndex+1+PestRowOffset] := '';
          end;
        end;
      end;
    end;
  end;
  FDataAssigned := True;
end;

procedure TframeMawGwtConcentrations.InitializeControls;
begin
  rdgConcentrations.BeginUpdate;
  try
    inherited;
    rdgConcentrations.Cells[Ord(mccSpecifiedConcentration), 0] := 'Specified concentration';
    rdgConcentrations.Cells[Ord(mccInjection), 0] := 'Injection concentration';
  finally
    rdgConcentrations.EndUpdate;
  end;
end;

procedure TframeMawGwtConcentrations.SetData(List: TScreenObjectEditCollection;
  SpeciesIndex: Integer);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  MultAqWell: TMawBoundary;
  TimeIndex: Integer;
  AMawItem: TMawItem;
  RowIndex: Integer;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    MultAqWell := ScreenObject.ModflowMawBoundary;
    if MultAqWell <> nil then
    begin
      if btnedInitialConcentration.Text <> '' then
      begin
        MultAqWell.StartingConcentrations[SpeciesIndex].Value := btnedInitialConcentration.Text;
      end
      else if MultAqWell.StartingConcentrations[SpeciesIndex].Value = '' then
      begin
        MultAqWell.StartingConcentrations[SpeciesIndex].Value := '0';
      end;

      if PestModifierAssigned[Ord(mccSpecifiedConcentration)] then
      begin
        MultAqWell.PestSpecifiedConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(mccSpecifiedConcentration)];
      end;
      if PestModifierAssigned[Ord(mccInjection)] then
      begin
        MultAqWell.PestInjectionConcentrations[SpeciesIndex].Value
          := PestModifier[Ord(mccInjection)];
      end;

      if FDataAssigned and PestMethodAssigned[Ord(mccSpecifiedConcentration)] then
      begin
        MultAqWell.PestSpecifiedConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(mccSpecifiedConcentration)];
      end;
      if FDataAssigned and PestMethodAssigned[Ord(mccInjection)] then
      begin
        MultAqWell.PestInjectionConcentrationMethods[SpeciesIndex].PestParamMethod
          := PestMethod[Ord(mccInjection)];
      end;
    end;

    if FDataAssigned then
    begin
      for TimeIndex := 0 to MultAqWell.Values.Count - 1 do
      begin
        AMawItem := MultAqWell.Values[TimeIndex] as TMawItem;
        RowIndex := TimeIndex+1+PestRowOffset;

        if rdgConcentrations.ItemIndex[Ord(mccStatus), RowIndex] >= 0 then
        begin
          AMawItem.GwtStatus[SpeciesIndex].GwtBoundaryStatus :=
            TGwtBoundaryStatus(rdgConcentrations.ItemIndex[Ord(mccStatus), RowIndex]);
        end;
        if rdgConcentrations.Cells[Ord(mccSpecifiedConcentration), RowIndex] <> '' then
        begin
          AMawItem.SpecifiedConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(mccSpecifiedConcentration), RowIndex];
        end;
        if rdgConcentrations.Cells[Ord(mccInjection), RowIndex] <> '' then
        begin
          AMawItem.InjectionConcentrations[SpeciesIndex].Value :=
            rdgConcentrations.Cells[Ord(mccInjection), RowIndex];
        end;
      end;
    end;
  end;
end;

end.
