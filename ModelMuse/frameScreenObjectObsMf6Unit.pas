unit frameScreenObjectObsMf6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  UndoItemsScreenObjects, Vcl.ExtCtrls, Vcl.ComCtrls, ArgusDataEntry;

type
  TFlowObsRows = (forCHD, forDRN, forEVT, forGHB, forRCH, forRIV, forWEL, forToMvr);

//  Head
//well flow rate (maw)
//well cell flow rates (maw + icon)
//pumping rate
//flowing well flow rate
//storage flow rate
//constant-flow rate
//well conductance (conductance)
//individual well cell conductances (conductance + icon
//flowing well conductance

  TframeScreenObjectObsMf6 = class(TFrame)
    pnlCaption: TPanel;
    pgcMain: TPageControl;
    tabBasic: TTabSheet;
    tabMAW: TTabSheet;
    lblTypesOfFlowObservation: TLabel;
    lblBoundaryFlowObservations: TLabel;
    cbHeadObservation: TCheckBox;
    cbDrawdownObservation: TCheckBox;
    cbGroundwaterFlowObservation: TCheckBox;
    chklstFlowObs: TCheckListBox;
    chklstBoundaryFlow: TCheckListBox;
    chklstMAW: TCheckListBox;
    pnlName: TPanel;
    edObsName: TLabeledEdit;
    tabSFR: TTabSheet;
    chklstSFR: TCheckListBox;
    tabLAK: TTabSheet;
    chklstLAK: TCheckListBox;
    rgStreamObsLocation: TRadioGroup;
    tabUZF: TTabSheet;
    chklstUZF: TCheckListBox;
    rdeDepthFraction: TRbwDataEntry;
    lblDepthFraction: TLabel;
    tabCSUB: TTabSheet;
    chklstCSUB: TCheckListBox;
    pnlDelayBeds: TPanel;
    chklstDelayBeds: TCheckListBox;
    lblDelayInterbedNumber: TLabel;
    splCSub: TSplitter;
    tabCalibration: TTabSheet;
    procedure cbGroundwaterFlowObservationClick(Sender: TObject);
    procedure cbHeadObservationClick(Sender: TObject);
    procedure chklstFlowObsClick(Sender: TObject);
    procedure edObsNameChange(Sender: TObject);
    procedure chklstBoundaryFlowClickCheck(Sender: TObject);
    procedure chklstMAWClickCheck(Sender: TObject);
    procedure chklstSFRClickCheck(Sender: TObject);
    procedure chklstLAKClick(Sender: TObject);
    procedure chklstUZFClick(Sender: TObject);
    procedure chklstCSUBClick(Sender: TObject);
  private
    FOnChangeProperties: TNotifyEvent;
    FInitializing: Boolean;
    FActiveObs: Boolean;
    procedure Initialize;
    procedure DoOnChangeProperties;
    procedure UpdateEdObsNameColor;
    procedure SetActiveObs(const Value: Boolean);
    procedure EnableDepthFraction;
    { Private declarations }
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnChangeProperties: TNotifyEvent read FOnChangeProperties
      write FOnChangeProperties;
    property ActiveObs: Boolean read FActiveObs write SetActiveObs;
    { Public declarations }
  end;

implementation

uses
  Modflow6ObsUnit, ScreenObjectUnit, ModflowMawUnit, ModflowSfr6Unit,
  ModflowLakMf6Unit, ModflowUzfMf6Unit, ModflowCsubUnit, frmGoPhastUnit;

{$R *.dfm}

procedure TframeScreenObjectObsMf6.cbGroundwaterFlowObservationClick(Sender: TObject);
begin
  chklstFlowObs.Enabled := cbGroundwaterFlowObservation.Checked;
  DoOnChangeProperties;
end;

procedure TframeScreenObjectObsMf6.cbHeadObservationClick(Sender: TObject);
begin
  DoOnChangeProperties;
end;

procedure TframeScreenObjectObsMf6.chklstBoundaryFlowClickCheck(
  Sender: TObject);
begin
  DoOnChangeProperties;
end;

procedure TframeScreenObjectObsMf6.chklstCSUBClick(Sender: TObject);
begin
  DoOnChangeProperties
end;

procedure TframeScreenObjectObsMf6.chklstFlowObsClick(Sender: TObject);
begin
  DoOnChangeProperties;
end;

procedure TframeScreenObjectObsMf6.chklstLAKClick(Sender: TObject);
begin
  DoOnChangeProperties;
end;

procedure TframeScreenObjectObsMf6.chklstMAWClickCheck(Sender: TObject);
begin
  DoOnChangeProperties;
end;

procedure TframeScreenObjectObsMf6.chklstSFRClickCheck(Sender: TObject);
begin
  DoOnChangeProperties;
end;

procedure TframeScreenObjectObsMf6.chklstUZFClick(Sender: TObject);
begin
  DoOnChangeProperties;
  EnableDepthFraction;
end;

procedure TframeScreenObjectObsMf6.DoOnChangeProperties;
begin
  UpdateEdObsNameColor;

  if Assigned(OnChangeProperties) and not FInitializing then
  begin
    OnChangeProperties(Self);
  end;
end;

procedure TframeScreenObjectObsMf6.GetData(List: TScreenObjectEditCollection);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Mf6Obs: TModflow6Obs;
  FoundFirst: Boolean;
  AnObsChoice: TGwFlowOb;
  MawOb: TMawOb;
  SfrOb: TSfrOb;
  LakOb: TLakOb;
  UzfOb: TUzfOb;
  CSubOb: TCSubOb;
  DelayArray: array of Boolean;
  DelayIndex: Integer;
  Position: Integer;
begin
  FActiveObs := False;
  FInitializing := True;
  try
    Initialize;

    SetLength(DelayArray, chklstDelayBeds.Items.Count);
    for DelayIndex := 0 to chklstDelayBeds.Items.Count - 1 do
    begin
      DelayArray[DelayIndex] := False;
    end;
    FoundFirst := False;
    for ScreenObjectIndex := 0 to List.Count - 1 do
    begin
      AScreenObject := List[ScreenObjectIndex].ScreenObject;
      if AScreenObject.Modflow6Obs <> nil then
      begin
        FActiveObs := True;
        Mf6Obs := AScreenObject.Modflow6Obs;
        if not FoundFirst then
        begin
          edObsName.Text := Mf6Obs.Name;
          cbHeadObservation.Checked := Mf6Obs.HeadObs;
          cbDrawdownObservation.Checked := Mf6Obs.DrawdownObs;
          cbGroundwaterFlowObservation.Checked := Mf6Obs.GroundwaterFlowObs;
          if Mf6Obs.GroundwaterFlowObs then
          begin
            for AnObsChoice := Low(TGwFlowOb) to High(TGwFlowOb) do
            begin
              chklstFlowObs.Checked[Ord(AnObsChoice)] :=
                AnObsChoice in Mf6Obs.GwFlowObsChoices;
            end;
          end;

          chklstBoundaryFlow.Checked[Ord(forCHD)] := Mf6Obs.ChdFlowObs;
          chklstBoundaryFlow.Checked[Ord(forDRN)] := Mf6Obs.DrnFlowObs;
          chklstBoundaryFlow.Checked[Ord(forEVT)] := Mf6Obs.EvtFlowObs;
          chklstBoundaryFlow.Checked[Ord(forGHB)] := Mf6Obs.GhbFlowObs;
          chklstBoundaryFlow.Checked[Ord(forRCH)] := Mf6Obs.RchFlowObs;
          chklstBoundaryFlow.Checked[Ord(forRIV)] := Mf6Obs.RivFlowObs;
          chklstBoundaryFlow.Checked[Ord(forWEL)] := Mf6Obs.WelFlowObs;
          chklstBoundaryFlow.Checked[Ord(forToMvr)] := Mf6Obs.ToMvrFlowObs;

          for MawOb := Low(TMawOb) to High(TMawOb) do
          begin
            chklstMAW.Checked[Ord(MawOb)] :=
              MawOb in Mf6Obs.MawObs;
          end;

          for SfrOb := Low(TSfrOb) to High(TSfrOb) do
          begin
            chklstSFR.Checked[Ord(SfrOb)] :=
              SfrOb in Mf6Obs.SfrObs;
          end;
          rgStreamObsLocation.ItemIndex := Ord(Mf6Obs.SfrObsLocation);

          for LakOb := Low(TLakOb) to High(TLakOb) do
          begin
            chklstLAK.Checked[Ord(LakOb)] :=
              LakOb in Mf6Obs.LakObs;
          end;

          for UzfOb := Low(TUzfOb) to High(TUzfOb) do
          begin
            chklstUZF.Checked[Ord(UzfOb)] :=
              UzfOb in Mf6Obs.UzfObs;
          end;
          rdeDepthFraction.RealValue := Mf6Obs.UzfObsDepthFraction;

          for CSubOb := Low(TCSubOb) to High(TCSubOb) do
          begin
            chklstCSUB.Checked[Ord(CSubOb)] :=
              CSubOb in Mf6Obs.CSubObs.CSubObsSet;
          end;
          
          for DelayIndex := 0 to Mf6Obs.CSubDelayCells.Count - 1 do
          begin
            Position := Mf6Obs.CSubDelayCells[DelayIndex].Value - 1;
            if (Position >= 0) and (Position < chklstDelayBeds.Items.Count) then
            begin
              chklstDelayBeds.Checked[Position] := True;
              DelayArray[Position] := True;
            end;
          end;

          FoundFirst := True;
        end
        else
        begin
          edObsName.Enabled := False;
          if cbHeadObservation.State <> TCheckBoxState(Mf6Obs.HeadObs) then
          begin
            cbHeadObservation.State := cbGrayed;
          end;
          if cbDrawdownObservation.State <>
            TCheckBoxState(Mf6Obs.DrawdownObs) then
          begin
            cbDrawdownObservation.State := cbGrayed;
          end;
          if cbGroundwaterFlowObservation.State <>
            TCheckBoxState(Mf6Obs.GroundwaterFlowObs) then
          begin
            cbGroundwaterFlowObservation.State := cbGrayed;
          end;

          for AnObsChoice := Low(TGwFlowOb) to High(TGwFlowOb)do
          begin
            if chklstFlowObs.State[Ord(AnObsChoice)] <>
              TCheckBoxState(AnObsChoice in Mf6Obs.GwFlowObsChoices) then
            begin
              chklstFlowObs.State[Ord(AnObsChoice)] := cbGrayed;
            end;
          end;

          for MawOb := Low(TMawOb) to High(TMawOb) do
          begin
            if chklstMAW.State[Ord(MawOb)] <>
              TCheckBoxState(MawOb in Mf6Obs.MawObs) then
            begin
              chklstMAW.State[Ord(MawOb)] := cbGrayed;
            end;
          end;

          for SfrOb := Low(TSfrOb) to High(TSfrOb) do
          begin
            if chklstSFR.State[Ord(SfrOb)] <>
              TCheckBoxState(SfrOb in Mf6Obs.SfrObs) then
            begin
              chklstSFR.State[Ord(SfrOb)] := cbGrayed;
            end;
          end;

          for LakOb := Low(TLakOb) to High(TLakOb) do
          begin
            if chklstLAK.State[Ord(LakOb)] <>
              TCheckBoxState(LakOb in Mf6Obs.LakObs) then
            begin
              chklstLAK.State[Ord(LakOb)] := cbGrayed;
            end;
          end;

          for UzfOb := Low(TUzfOb) to High(TUzfOb) do
          begin
            if chklstUZF.State[Ord(UzfOb)] <>
              TCheckBoxState(UzfOb in Mf6Obs.UzfObs) then
            begin
              chklstUZF.State[Ord(UzfOb)] := cbGrayed;
            end;
          end;

          for CSubOb := Low(TCSubOb) to High(TCSubOb) do
          begin
            if chklstCSUB.State[Ord(CSubOb)] <>
              TCheckBoxState(CSubOb in Mf6Obs.CSubObs.CSubObsSet) then
            begin
              chklstCSUB.State[Ord(CSubOb)] := cbGrayed;
            end;
          end;

          for DelayIndex := 0 to Mf6Obs.CSubDelayCells.Count - 1 do
          begin
            Position := Mf6Obs.CSubDelayCells[DelayIndex].Value - 1;
            if (Position >= 0) and (Position < chklstDelayBeds.Items.Count) then
            begin
              if not DelayArray[Position] then
              begin
                chklstDelayBeds.State[Position] := cbGrayed;
              end;              
            end;
          end;


          if chklstBoundaryFlow.State[Ord(forCHD)] <>
            TCheckBoxState(Mf6Obs.ChdFlowObs) then
          begin
            chklstBoundaryFlow.State[Ord(forCHD)] := cbGrayed;
          end;

          if chklstBoundaryFlow.State[Ord(forDRN)] <>
            TCheckBoxState(Mf6Obs.DrnFlowObs) then
          begin
            chklstBoundaryFlow.State[Ord(forDRN)] := cbGrayed;
          end;

          if chklstBoundaryFlow.State[Ord(forEVT)] <>
            TCheckBoxState(Mf6Obs.EvtFlowObs) then
          begin
            chklstBoundaryFlow.State[Ord(forEVT)] := cbGrayed;
          end;

          if chklstBoundaryFlow.State[Ord(forGHB)] <>
            TCheckBoxState(Mf6Obs.GhbFlowObs) then
          begin
            chklstBoundaryFlow.State[Ord(forGHB)] := cbGrayed;
          end;

          if chklstBoundaryFlow.State[Ord(forRCH)] <>
            TCheckBoxState(Mf6Obs.RchFlowObs) then
          begin
            chklstBoundaryFlow.State[Ord(forRCH)] := cbGrayed;
          end;

          if chklstBoundaryFlow.State[Ord(forRIV)] <>
            TCheckBoxState(Mf6Obs.RivFlowObs) then
          begin
            chklstBoundaryFlow.State[Ord(forRIV)] := cbGrayed;
          end;

          if chklstBoundaryFlow.State[Ord(forWEL)] <>
            TCheckBoxState(Mf6Obs.WelFlowObs) then
          begin
            chklstBoundaryFlow.State[Ord(forWEL)] := cbGrayed;
          end;

          if chklstBoundaryFlow.State[Ord(forToMvr)] <>
            TCheckBoxState(Mf6Obs.ToMvrFlowObs) then
          begin
            chklstBoundaryFlow.State[Ord(forToMvr)] := cbGrayed;
          end;

          if rgStreamObsLocation.ItemIndex <> Ord(Mf6Obs.SfrObsLocation) then
          begin
            rgStreamObsLocation.ItemIndex := -1;
          end;

          if rdeDepthFraction.RealValue <> Mf6Obs.UzfObsDepthFraction then
          begin
            rdeDepthFraction.Text := '';
          end;
        end;
      end;
    end;
    EnableDepthFraction;
  finally
    FInitializing := False;
  end;
end;


procedure TframeScreenObjectObsMf6.Initialize;
var
  GWChoice: TGwFlowOb;
  MawIndex: Integer;
  SfrIndex: Integer;
  UzfIndex: Integer;
  CSubIndex: Integer;
  IbIndex: Integer;
begin
  pgcMain.ActivePageIndex := 0;

  edObsName.Enabled := True;
  edObsName.Text := '';
  cbHeadObservation.Checked := False;
  cbDrawdownObservation.Checked := False;
  cbGroundwaterFlowObservation.Checked := False;
  for GWChoice := Low(TGwFlowOb) to HIgh(TGwFlowOb) do
  begin
    chklstFlowObs.State[Ord(GWChoice)] := cbUnchecked;
  end;
  chklstFlowObs.Checked[Ord(gfoNearestNeighbor)] := True;

  for MawIndex := 0 to chklstMAW.Items.Count - 1 do
  begin
    chklstMAW.State[MawIndex] := cbUnchecked;
  end;

  for SfrIndex := 0 to chklstSFR.Items.Count - 1 do
  begin
    chklstSFR.State[SfrIndex] := cbUnchecked;
  end;

  for UzfIndex := 0 to chklstUZF.Items.Count - 1 do
  begin
    chklstUZF.State[UzfIndex] := cbUnchecked;
  end;

  for CSubIndex := 0 to chklstCSUB.Items.Count - 1 do
  begin
    chklstCSUB.State[CSubIndex] := cbUnchecked;
  end;
  
  chklstDelayBeds.Items.Clear;
  if frmGoPhast.PhastModel.ModflowPackages.CSubPackage.IsSelected then
  begin
    for IbIndex := 1 to frmGoPhast.PhastModel.ModflowPackages.CSubPackage.NumberOfDelayCells do
    begin
      chklstDelayBeds.Items.Add(IntToStr(IbIndex));
    end;
  end;

  tabCalibration.tabVisible := frmGoPhast.PhastModel.PestUsed;

  DoOnChangeProperties;

end;

procedure TframeScreenObjectObsMf6.edObsNameChange(Sender: TObject);
begin
  DoOnChangeProperties
end;

procedure TframeScreenObjectObsMf6.SetActiveObs(const Value: Boolean);
begin
  FActiveObs := Value;
  UpdateEdObsNameColor;
end;

procedure TframeScreenObjectObsMf6.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Mf6Obs: TModflow6Obs;
  BoundaryUsed: Boolean;
  NewChoices: TGwFlowObs;
  AnObsChoice: TGwFlowOb;
  NewMawObs: TMawObs;
  MawOb: TMawOb;
  NewSfrObs: TSfrObs;
  SfrOb: TSfrOb;
  NewLakObs: TLakObs;
  LakOb: TLakOb;
  NewUzfObs: TUzfObs;
  UzfOb: TUzfOb;
  CSubOb: TCSubOb;
  NewCSubObs: TSubObsSet;
  DelayArray: array of Boolean;
  DelayIndex: Integer;
  Position: Integer;
begin
  SetLength(DelayArray, chklstDelayBeds.Items.Count);
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Mf6Obs := Item.ScreenObject.Modflow6Obs;
    BoundaryUsed := (Mf6Obs <> nil) and Mf6Obs.Used;

    if ClearAll then
    begin
      if BoundaryUsed then
      begin
        Mf6Obs.Used := False;
      end;
    end
    else if SetAll or BoundaryUsed then
    begin
      if Mf6Obs = nil then
      begin
        Item.ScreenObject.CreateMf6Obs;
        Mf6Obs := Item.ScreenObject.Modflow6Obs;
      end;
      Mf6Obs.Used := True;

      if List.Count = 1 then
      begin
        Mf6Obs.Name := edObsName.Text;
      end;

      if cbHeadObservation.State <> cbGrayed then
      begin
        Mf6Obs.HeadObs := cbHeadObservation.Checked;
      end;
      if cbDrawdownObservation.State <> cbGrayed then
      begin
        Mf6Obs.DrawdownObs := cbDrawdownObservation.Checked;
      end;
      if cbGroundwaterFlowObservation.State <> cbGrayed then
      begin
        Mf6Obs.GroundwaterFlowObs := cbGroundwaterFlowObservation.Checked;
      end;
      NewChoices := Mf6Obs.GwFlowObsChoices;
      for AnObsChoice := Low(TGwFlowOb) to High(TGwFlowOb) do
      begin
        if chklstFlowObs.State[Ord(AnObsChoice)] <> cbGrayed then
        begin
          if chklstFlowObs.Checked[Ord(AnObsChoice)] then
          begin
            Include(NewChoices, AnObsChoice);
          end
          else
          begin
            Exclude(NewChoices, AnObsChoice);
          end;
        end;
      end;
      Mf6Obs.GwFlowObsChoices := NewChoices;

      NewMawObs := Mf6Obs.MawObs;
      for MawOb := Low(TMawOb) to High(TMawOb) do
      begin
        if chklstMAW.State[Ord(MawOb)] <> cbGrayed then
        begin
          if chklstMAW.Checked[Ord(MawOb)] then
          begin
            Include(NewMawObs, MawOb);
          end
          else
          begin
            Exclude(NewMawObs, MawOb);
          end;
        end;
      end;
      Mf6Obs.MawObs := NewMawObs;

      NewSfrObs := Mf6Obs.SfrObs;
      for SfrOb := Low(TSfrOb) to High(TSfrOb) do
      begin
        if chklstSFR.State[Ord(SfrOb)] <> cbGrayed then
        begin
          if chklstSFR.Checked[Ord(SfrOb)] then
          begin
            Include(NewSfrObs, SfrOb);
          end
          else
          begin
            Exclude(NewSfrObs, SfrOb);
          end;
        end;
      end;
      Mf6Obs.SfrObs := NewSfrObs;

      NewLakObs := Mf6Obs.LakObs;
      for LakOb := Low(TLakOb) to High(TLakOb) do
      begin
        if chklstLAK.State[Ord(LakOb)] <> cbGrayed then
        begin
          if chklstLAK.Checked[Ord(LakOb)] then
          begin
            Include(NewLakObs, LakOb);
          end
          else
          begin
            Exclude(NewLakObs, LakOb);
          end;
        end;
      end;
      Mf6Obs.LakObs := NewLakObs;

      NewUzfObs := Mf6Obs.UzfObs;
      for UzfOb := Low(TUzfOb) to High(TUzfOb) do
      begin
        if chklstUzf.State[Ord(UzfOb)] <> cbGrayed then
        begin
          if chklstUzf.Checked[Ord(UzfOb)] then
          begin
            Include(NewUzfObs, UzfOb);
          end
          else
          begin
            Exclude(NewUzfObs, UzfOb);
          end;
        end;
      end;
      Mf6Obs.UzfObs := NewUzfObs;

      NewCSubObs := Mf6Obs.CSubObs.CSubObsSet;
      for CSubOb := Low(TCSubOb) to High(TCSubOb) do
      begin
        if chklstCSUB.State[Ord(CSubOb)] <> cbGrayed then
        begin
          if chklstCSUB.Checked[Ord(CSubOb)] then
          begin
            Include(NewCSubObs, CSubOb);
          end
          else
          begin
            Exclude(NewCSubObs, CSubOb);
          end;
        end;
      end;
      Mf6Obs.CSubObs.CSubObsSet := NewCSubObs;

      if chklstBoundaryFlow.State[Ord(forCHD)] <> cbGrayed then
      begin
        Mf6Obs.ChdFlowObs := chklstBoundaryFlow.Checked[Ord(forCHD)];
      end;

      for DelayIndex := 0 to Length(DelayArray) - 1 do
      begin
        DelayArray[DelayIndex] := False;
      end;
      for DelayIndex := 0 to Mf6Obs.CSubDelayCells.Count - 1 do
      begin
        Position := Mf6Obs.CSubDelayCells[DelayIndex].Value -1;
        DelayArray[Position] := True;
      end;
      for DelayIndex := 0 to chklstDelayBeds.Items.Count -1 do
      begin
        if chklstDelayBeds.State[DelayIndex] <> cbGrayed then
        begin
          DelayArray[DelayIndex] := chklstDelayBeds.Checked[DelayIndex];
        end;
      end;
      Mf6Obs.CSubDelayCells.Clear;
      for DelayIndex := 0 to Length(DelayArray) - 1 do
      begin
        if DelayArray[DelayIndex] then
        begin
          Mf6Obs.CSubDelayCells.Add.Value := DelayIndex + 1;
        end;
      end;

      if chklstBoundaryFlow.State[Ord(forDRN)] <> cbGrayed then
      begin
        Mf6Obs.DrnFlowObs := chklstBoundaryFlow.Checked[Ord(forDRN)];
      end;

      if chklstBoundaryFlow.State[Ord(forEVT)] <> cbGrayed then
      begin
        Mf6Obs.EvtFlowObs := chklstBoundaryFlow.Checked[Ord(forEVT)];
      end;

      if chklstBoundaryFlow.State[Ord(forGHB)] <> cbGrayed then
      begin
        Mf6Obs.GhbFlowObs := chklstBoundaryFlow.Checked[Ord(forGHB)];
      end;

      if chklstBoundaryFlow.State[Ord(forRCH)] <> cbGrayed then
      begin
        Mf6Obs.RchFlowObs := chklstBoundaryFlow.Checked[Ord(forRCH)];
      end;

      if chklstBoundaryFlow.State[Ord(forRIV)] <> cbGrayed then
      begin
        Mf6Obs.RivFlowObs := chklstBoundaryFlow.Checked[Ord(forRIV)];
      end;

      if chklstBoundaryFlow.State[Ord(forWEL)] <> cbGrayed then
      begin
        Mf6Obs.WelFlowObs := chklstBoundaryFlow.Checked[Ord(forWEL)];
      end;

      if chklstBoundaryFlow.State[Ord(forToMvr)] <> cbGrayed then
      begin
        Mf6Obs.ToMvrFlowObs := chklstBoundaryFlow.Checked[Ord(forToMvr)];
      end;

      if rgStreamObsLocation.ItemIndex >= 0 then
      begin
        Mf6Obs.SfrObsLocation := TSfrObsLocation(rgStreamObsLocation.ItemIndex);
      end;

      if rdeDepthFraction.Text <> '' then
      begin
        Mf6Obs.UzfObsDepthFraction := rdeDepthFraction.RealValue;
      end;
    end;
  end;

end;

procedure TframeScreenObjectObsMf6.EnableDepthFraction;
begin
  rdeDepthFraction.Enabled := chklstUZF.State[Ord(uoWaterContent)] <> cbUnchecked;
end;

procedure TframeScreenObjectObsMf6.UpdateEdObsNameColor;
var
  ObsUsed: Boolean;
  ItemIndex: Integer;
begin
  ObsUsed := cbHeadObservation.Checked or cbDrawdownObservation.Checked
    or cbGroundwaterFlowObservation.Checked;
  if not ObsUsed then
  begin
    for ItemIndex := 0 to chklstBoundaryFlow.Items.Count - 1 do
    begin
      if chklstBoundaryFlow.State[ItemIndex] <> cbUnchecked then
      begin
        ObsUsed := True;
        break;
      end;
    end;
  end;
  if not ObsUsed then
  begin
    for ItemIndex := 0 to chklstMAW.Items.Count - 1 do
    begin
      if chklstMAW.State[ItemIndex] <> cbUnchecked then
      begin
        ObsUsed := True;
        break;
      end;
    end;
  end;
  if not ObsUsed then
  begin
    for ItemIndex := 0 to chklstSFR.Items.Count - 1 do
    begin
      if chklstSFR.State[ItemIndex] <> cbUnchecked then
      begin
        ObsUsed := True;
        break;
      end;
    end;
  end;
  if not ObsUsed then
  begin
    for ItemIndex := 0 to chklstUZF.Items.Count - 1 do
    begin
      if chklstUZF.State[ItemIndex] <> cbUnchecked then
      begin
        ObsUsed := True;
        break;
      end;
    end;
  end;
  if not ObsUsed then
  begin
    for ItemIndex := 0 to chklstCSUB.Items.Count - 1 do
    begin
      if chklstCSUB.State[ItemIndex] <> cbUnchecked then
      begin
        ObsUsed := True;
        break;
      end;
    end;
  end;
  if ObsUsed and ActiveObs and (edObsName.Text = '') then
  begin
    edObsName.Color := clRed;
  end
  else
  begin
    edObsName.Color := clWindow;
  end;
end;

end.
