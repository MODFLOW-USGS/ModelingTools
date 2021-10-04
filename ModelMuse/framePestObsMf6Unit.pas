unit framePestObsMf6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePestObsUnit, frameGridUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls, PestObsUnit;

type
  TPestMf6ObsColumns = (pm6Name, mp6ObsSeries, pm6Type, pm6Group, pm6Time,
    {pm6ObjectWeightFormula,} pm6Value, pm6Weight, pm6MawConnectionNumber, pm6Comment);

  TframePestObsMf6 = class(TframePestObs)
    cbMultilayer: TCheckBox;
    procedure frameObservationsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameObservationsGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure FrameResize(Sender: TObject);
    procedure frameObservationsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
  private
    { Private declarations }
    procedure EnableMultiLayer;
  protected
    procedure SetObsColumnCaptions; override;
    procedure GetDirectObs(Observations: TCustomComparisonCollection); override;
    procedure SetDirectObs(Observations: TCustomComparisonCollection); override;
    procedure GetObservationGroups; override;
  public
    procedure SpecifyObservationTypes(ObsTypes: TStrings);
    procedure SpecifyGroupTypes(ObsTypes: TStrings);
    { Public declarations }
  end;

var
  framePestObsMf6: TframePestObsMf6;

implementation

uses
  Modflow6ObsUnit, ModflowMawUnit, ModflowSfr6Unit, ModflowLakMf6Unit,
  ModflowUzfMf6Unit, ModflowCsubUnit, PestObsGroupUnit, frmGoPhastUnit;

resourcestring
  StrObservationSeriesType = 'Observation Series Type';
  StrObjectWeightFormul = 'Object Weight Formula';
  StrMAWConnectionNumbe = 'MAW Connection Number';

{$R *.dfm}

{ TframePestObsMf6 }

procedure TframePestObsMf6.EnableMultiLayer;
var
  RowIndex: Integer;
  ObSeries: TObSeries;
  ShouldEnable: Boolean;
  ObTypeName: string;
  ObGen: TObGeneral;
begin
  ShouldEnable := False;
  for RowIndex := 0 to frameObservations.seNumber.AsInteger do
  begin
    if TryGetObsSeries(frameObservations.Grid.Cells[Ord(mp6ObsSeries),RowIndex], ObSeries) then
    begin
      if ObSeries = osGeneral then
      begin
        ObTypeName := frameObservations.Grid.Cells[Ord(pm6Type), RowIndex];
        if TryGetGenOb(ObTypeName, ObGen) then
        begin
          if ObGen in [ogHead, ogDrawdown] then
          begin
            ShouldEnable := True;
            break;
          end;
        end;
      end;
    end;
  end;
  cbMultilayer.Enabled := ShouldEnable;
end;

procedure TframePestObsMf6.frameObservationsGridBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  ObSeries: TObSeries;
  ObTypeName: string;
  ObGen: TObGeneral;
  ObMaw: TMawOb;
  ObSfr: TSfrOb;
  ObLake: TLakOb;
  ObUzf: TUzfOb;
  ObCSub: TCSubOb;
begin
  inherited;
  if ARow > 0 then
  begin
    if ACol = Ord(mp6ObsSeries) then
    begin
      if not TryGetObsSeries(frameObservations.Grid.Cells[
        ACol,ARow], ObSeries) then
      begin
        frameObservations.Grid.Canvas.Brush.Color := clRed;
      end;
    end
    else if ACol = Ord(pm6Type) then
    begin
      if TryGetObsSeries(frameObservations.Grid.Cells[
        Ord(mp6ObsSeries),ARow], ObSeries) then
      begin
        ObTypeName := frameObservations.Grid.Cells[ACol, ARow];
        case ObSeries of
          osGeneral:
            begin
              if not TryGetGenOb(ObTypeName, ObGen) then
              begin
                frameObservations.Grid.Canvas.Brush.Color:= clRed;
              end;
            end;
          osMaw:
            begin
              if not TryGetMawOb(ObTypeName, ObMaw) then
              begin
                frameObservations.Grid.Canvas.Brush.Color:= clRed;
              end;
            end;
          osSfr:
            begin
              if not TryGetSfrOb(ObTypeName, ObSfr) then
              begin
                frameObservations.Grid.Canvas.Brush.Color:= clRed;
              end;
            end;
          osLak:
            begin
              if not TryGetLakOb(ObTypeName, ObLake) then
              begin
                frameObservations.Grid.Canvas.Brush.Color:= clRed;
              end;
            end;
          osUzf:
            begin
              if not TryGetUzfOb(ObTypeName, ObUzf) then
              begin
                frameObservations.Grid.Canvas.Brush.Color:= clRed;
              end;
            end;
          osCSub:
            begin
              if not TryGetCSubOb(ObTypeName, ObCSub) then
              begin
                frameObservations.Grid.Canvas.Brush.Color:= clRed;
              end;
            end;
          else
            begin
              Assert(False);
            end;
        end;
      end;
    end;
  end;
end;

procedure TframePestObsMf6.frameObservationsGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  ObsSeriesName: string;
  ObsSeries: TObSeries;
  PickList: TStrings;
  MawObsTypeName: string;
  MawOb: TMawOb;
//  ObsTypeName: string;
//  GenOb: TObGeneral;
//  MawOb: TMawOb;
begin
  inherited;
  if (ARow >= 0) then
  begin
    ObsSeriesName := frameObservations.Grid.Cells[Ord(mp6ObsSeries), ARow];
    if ACol = Ord(pm6MawConnectionNumber) then
    begin
      CanSelect := False;
    end;

    if TryGetObsSeries(ObsSeriesName, ObsSeries) then
    begin
      PickList := frameObservations.Grid.Columns[Ord(pm6Type)].PickList;
      case ObsSeries of
        osGeneral:
          begin
            FillObGenSeriesNames(PickList);
          end;
        osMaw:
          begin
            FillMawSeriesNames(PickList);
            if ACol = Ord(pm6MawConnectionNumber) then
            begin
              MawObsTypeName := frameObservations.Grid.Cells[Ord(pm6Type), ARow];
              if TryGetMawOb(MawObsTypeName,MawOb)then
              begin
                CanSelect := MawOb in [moFlowRateCells, moConductanceCells];
              end;
            end;
          end;
        osSfr:
          begin
            FillSfrSeriesNames(PickList);
          end;
        osLak:
          begin
            FillLakSeriesNames(PickList);
          end;
        osUzf:
          begin
            FillUzfSeriesNames(PickList);
          end;
        osCSub:
          begin
            FillCSubSeriesNames(PickList);
          end;
        else
          begin
            Assert(False);
          end;
      end;
    end;

//    if ACol = Ord(pm6ObjectWeightFormula) then
//    begin
//      CanSelect := False;
//      if TryGetObsSeries(ObsSeriesName, ObsSeries) then
//      begin
//        ObsTypeName := frameObservations.Grid.Cells[Ord(pm6Type), ARow];
//        case ObsSeries of
//          osGeneral:
//            begin
//              if TryGetGenOb(ObsTypeName, GenOb) then
//              begin
////                CanSelect := GenOb in [ogCHD, ogDrain, ogWell, ogGHB, ogRiv,
////                  ogRch, ogEVT, ogMvr];
//              end;
//            end;
//          osMaw:
//            begin
//              if TryGetMawOb(ObsTypeName, MawOb) then
//              begin
//
//              end;
//            end;
//          osSfr:
//            begin
//
//            end;
//          osLak:
//            begin
//
//            end;
//          osUzf:
//            begin
//
//            end;
//          osCSub:
//            begin
//
//            end;
//          else
//            Assert(False);
//        end;
//      end;
//    end;
  end;
end;

procedure TframePestObsMf6.frameObservationsGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  EnableMultiLayer;
end;

procedure TframePestObsMf6.FrameResize(Sender: TObject);
begin
  inherited;
  cbMultilayer.Parent := frameObservations.Panel;
  cbMultilayer.Top := frameObservations.lbNumber.Top + 24;
end;

procedure TframePestObsMf6.GetDirectObs(
  Observations: TCustomComparisonCollection);
var
  ItemIndex: Integer;
  Obs: TMf6CalibrationObs;
begin
  for ItemIndex := 0 to Observations.Count - 1 do
  begin
    Obs := Observations[ItemIndex] as TMf6CalibrationObs;
    frameObservations.Grid.Cells[Ord(pm6Name), ItemIndex + 1] := Obs.Name;
    frameObservations.Grid.Objects[Ord(pm6Name), ItemIndex + 1] := Obs;
    frameObservations.Grid.Cells[Ord(mp6ObsSeries), ItemIndex + 1] := ObsSeriesToString(Obs.ObSeries);
    frameObservations.Grid.Cells[Ord(pm6Type), ItemIndex + 1] := Obs.ObsTypeString;
    frameObservations.Grid.Cells[Ord(pm6Group), ItemIndex + 1] := Obs.ObservationGroup;
    frameObservations.Grid.RealValue[Ord(pm6Time), ItemIndex + 1] := Obs.Time;
//    frameObservations.Grid.Cells[Ord(pm6ObjectWeightFormula), ItemIndex + 1] := Obs.WeightFormula;
    frameObservations.Grid.RealValue[Ord(pm6Value), ItemIndex + 1] := Obs.ObservedValue;
    frameObservations.Grid.RealValue[Ord(pm6Weight), ItemIndex + 1] := Obs.Weight;
    frameObservations.Grid.IntegerValue[Ord(pm6MawConnectionNumber), ItemIndex + 1] := Obs.MawConnectionNumber;
    frameObservations.Grid.Cells[Ord(pm6Comment), ItemIndex + 1] := Obs.Comment;
  end;
  cbMultilayer.Checked := (Observations as TMf6CalibrationObservations).MultiLayer;
  EnableMultiLayer;
end;

procedure TframePestObsMf6.GetObservationGroups;
var
  ObsGroups: TPestObservationGroups;
  PickList: TStrings;
  GroupIndex: Integer;
begin
  ObsGroups := frmGoPhast.PhastModel. PestProperties.ObservationGroups;
  PickList := frameObservations.Grid.Columns[Ord(pm6Group)].PickList;
  PickList.Clear;
  PickList.Capacity := ObsGroups.Count;
  for GroupIndex := 0 to ObsGroups.Count - 1 do
  begin
    PickList.Add(ObsGroups[GroupIndex].ObsGroupName);
  end;

  PickList := frameObsComparisons.Grid.Columns[Ord(poccGroup)].PickList;
  PickList.Clear;
  PickList.Capacity := ObsGroups.Count;
  for GroupIndex := 0 to ObsGroups.Count - 1 do
  begin
    PickList.Add(ObsGroups[GroupIndex].ObsGroupName);
  end;
end;

procedure TframePestObsMf6.SetDirectObs(
  Observations: TCustomComparisonCollection);
var
  ObsCount: Integer;
  RowIndex: Integer;
  RowOK: Boolean;
  ColIndex: Integer;
  Obs: TMf6CalibrationObs;
  OtherObs: TMf6CalibrationObs;
  MyGuid: TGUID;
  ObSeries: TObSeries;
  ObTypeName: string;
  ObGen: TObGeneral;
  ObMaw: TMawOb;
  ObSfr: TSfrOb;
  ObLake: TLakOb;
  ObUzf: TUzfOb;
  ObCSub: TCSubOb;
begin
  ObsCount := 0;
  for RowIndex := 1 to frameObservations.seNumber.AsInteger do
  begin
    RowOK := True;
    for ColIndex := 0 to Ord(pm6Weight) do
    begin
      if ColIndex = Ord(pm6Group) then
      begin
        Continue;
      end;
      if frameObservations.Grid.Cells[ColIndex,RowIndex] = '' then
      begin
        RowOK := False;
        Break;
      end;
    end;
    if RowOK then
    begin
      RowOK := TryGetObsSeries(frameObservations.Grid.Cells[
        Ord(mp6ObsSeries),RowIndex], ObSeries);
      if RowOK then
      begin
        ObTypeName := frameObservations.Grid.Cells[Ord(pm6Type), RowIndex];
        case ObSeries of
          osGeneral:
            begin
              RowOK := TryGetGenOb(ObTypeName, ObGen);
            end;
          osMaw:
            begin
              RowOK := TryGetMawOb(ObTypeName, ObMaw);
              if RowOk then
              begin
                if ObMaw in [moFlowRateCells, moConductanceCells] then
                begin
                  RowOK := frameObservations.Grid.Cells[
                    Ord(pm6MawConnectionNumber),RowIndex] <> '';
                end;
              end;
            end;
          osSfr:
            begin
              RowOK := TryGetSfrOb(ObTypeName, ObSfr);
            end;
          osLak:
            begin
              RowOK := TryGetLakOb(ObTypeName, ObLake);
            end;
          osUzf:
            begin
              RowOK := TryGetUzfOb(ObTypeName, ObUzf);
            end;
          osCSub:
            begin
              RowOK := TryGetCSubOb(ObTypeName, ObCSub);
            end;
        end;
      end;
    end;
    if RowOK then
    begin
      if ObsCount < Observations.Count then
      begin
        Obs := Observations[ObsCount] as TMf6CalibrationObs
      end
      else
      begin
        Obs := Observations.Add as TMf6CalibrationObs;
      end;
      Inc(ObsCount);
      Obs.Name := frameObservations.Grid.Cells[Ord(pm6Name), RowIndex];
      if frameObservations.Grid.Objects[Ord(pm6Name), RowIndex] <> nil then
      begin
        OtherObs := frameObservations.Grid.Objects[Ord(pm6Name), RowIndex] as TMf6CalibrationObs;
        Obs.GUID  := OtherObs.GUID;
      end
      else
      begin
        if CreateGUID(MyGuid) = 0 then
        begin
          Obs.GUID := GUIDToString(MyGuid);
        end;
      end;
      Assert(TryGetObsSeries(frameObservations.Grid.Cells[Ord(mp6ObsSeries),RowIndex], ObSeries));
      Obs.ObSeries := ObSeries;
      Obs.ObsTypeString := frameObservations.Grid.Cells[Ord(pm6Type), RowIndex];
      Obs.ObservationGroup := frameObservations.Grid.Cells[Ord(pm6Group), RowIndex];
      Obs.Time := frameObservations.Grid.RealValue[Ord(pm6Time), RowIndex];
      Obs.ObservedValue := frameObservations.Grid.RealValue[Ord(pm6Value), RowIndex];
      Obs.Weight := frameObservations.Grid.RealValue[Ord(pm6Weight), RowIndex];
      if (ObSeries = osMaw)
        and (Obs.MawOb in [moFlowRateCells, moConductanceCells]) then
      begin
        Obs.MawConnectionNumber := frameObservations.Grid.IntegerValue[Ord(pm6MawConnectionNumber), RowIndex];
      end;
      Obs.Comment := frameObservations.Grid.Cells[Ord(pm6Comment), RowIndex];
    end;
  end;
  Observations.Count := ObsCount;
  (Observations as TMf6CalibrationObservations).MultiLayer := cbMultilayer.Checked;
end;

procedure TframePestObsMf6.SetObsColumnCaptions;
begin
  frameObservations.Grid.BeginUpdate;
  try
    frameObservations.Grid.Cells[Ord(pm6Name), 0] := StrObservationName;
    frameObservations.Grid.Cells[Ord(mp6ObsSeries), 0] := StrObservationSeriesType;;
    frameObservations.Grid.Cells[Ord(pm6Type), 0] := StrObservationType;
    frameObservations.Grid.Cells[Ord(pm6Group), 0] := StrObservationGroup;
    frameObservations.Grid.Cells[Ord(pm6Time), 0] := StrObservationTime;
    frameObservations.Grid.Cells[Ord(pm6Value), 0] := StrObservationValue;
    frameObservations.Grid.Cells[Ord(pm6Weight), 0] := StrObservationWeight;
    frameObservations.Grid.Cells[Ord(pm6MawConnectionNumber), 0] := StrMAWConnectionNumbe;
    frameObservations.Grid.Cells[Ord(pm6Comment), 0] := StrComment;
  finally
    frameObservations.Grid.EndUpdate;
  end;
end;

procedure TframePestObsMf6.SpecifyGroupTypes(ObsTypes: TStrings);
begin
  frameObservations.Grid.Columns[Ord(mp6ObsSeries)].PickList := ObsTypes;
end;

procedure TframePestObsMf6.SpecifyObservationTypes(ObsTypes: TStrings);
begin
  frameObservations.Grid.Columns[Ord(pm6Type)].PickList := ObsTypes;
end;

end.
