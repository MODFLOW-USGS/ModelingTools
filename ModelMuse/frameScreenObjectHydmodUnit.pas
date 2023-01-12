unit frameScreenObjectHydmodUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, StdCtrls, RbwEdit, CheckLst, JvExStdCtrls, JvCombobox,
  JvListComb, UndoItemsScreenObjects;

type
  TBasicRow = (brHead, brDrawDown);
  TSfrRow = (srStage, srInflow, srOutflow, srExchange);
  TSubRow = (srPreconsolidationHead, srCompaction, srSubsidence);

  TframeScreenObjectHydmod = class(TFrame)
    pnlCaption: TPanel;
    lblHYDLBL: TLabel;
    edHYDLBL: TRbwEdit;
    rgINTYP: TRadioGroup;
    gbBasic: TGroupBox;
    gbSubsidence: TGroupBox;
    clbBasic: TCheckListBox;
    clbSub: TCheckListBox;
    comboLayerGroup: TJvImageComboBox;
    lblLayerGroup: TLabel;
    lblNoDelayBed: TLabel;
    comboNoDelayBed: TJvImageComboBox;
    clbLayer: TCheckListBox;
    lblLayer: TLabel;
    gbSFR: TGroupBox;
    clbSFR: TCheckListBox;
    procedure clbSFRClickCheck(Sender: TObject);
    procedure comboLayerGroupChange(Sender: TObject);
    procedure comboNoDelayBedChange(Sender: TObject);
    procedure edHYDLBLExit(Sender: TObject);
  private
    function InterpolationAllowed(
      const List: TScreenObjectEditCollection): boolean;
    procedure SetAllowedAssignmentMethod;
    procedure FillListOfScreenObjects(ListOfScreenObjects: TList;
      List: TScreenObjectEditCollection);
    { Private declarations }
  public
    procedure GetData(const List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, ModflowPackagesUnit, LayerStructureUnit,
  ModflowSubsidenceDefUnit, ScreenObjectUnit, GoPhastTypes, ModflowHydmodUnit;

{$R *.dfm}

procedure TframeScreenObjectHydmod.clbSFRClickCheck(Sender: TObject);
begin
  SetAllowedAssignmentMethod;
end;

procedure TframeScreenObjectHydmod.comboLayerGroupChange(Sender: TObject);
var
  LayerGroup: TLayerGroup;
  Index: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem;
begin
  {$IF CompilerVersion > 28}
  comboNoDelayBed.Items.ClearAndResetID;
  {$ENDIF}
  if comboLayerGroup.ItemIndex >= 0 then
  begin
    LayerGroup := comboLayerGroup.Items.Objects[comboLayerGroup.ItemIndex]
      as TLayerGroup;
    for Index := 0 to LayerGroup.SubNoDelayBedLayers.Count - 1 do
    begin
      NoDelayItem := LayerGroup.SubNoDelayBedLayers[Index];
      comboNoDelayBed.Items.AddObject(NoDelayItem.Name , NoDelayItem)
    end;
  end;
  if comboNoDelayBed.Items.Count > 0 then
  begin
    comboNoDelayBed.ItemIndex := 0;
  end;
  if Assigned(comboNoDelayBed.OnChange) then
  begin
    comboNoDelayBed.OnChange(nil);
  end;
end;

procedure TframeScreenObjectHydmod.comboNoDelayBedChange(Sender: TObject);
var
  LayerGroup: TLayerGroup;
  NoDelayItem: TSubNoDelayBedLayerItem;
  Index: Integer;
  UsedLayerItem: TUseLayerNumberItem;
begin
  clbLayer.Items.Clear;
  if (comboLayerGroup.ItemIndex >= 0)
    and (comboNoDelayBed.ItemIndex >= 0) then
  begin
    LayerGroup := comboLayerGroup.Items.Objects[comboLayerGroup.ItemIndex]
      as TLayerGroup;
    if LayerGroup.LayerCount > 1 then
    begin
      clbLayer.Enabled := True;
      NoDelayItem := comboNoDelayBed.Items.Objects[comboNoDelayBed.ItemIndex]
        as TSubNoDelayBedLayerItem;
      if NoDelayItem.UseInAllLayers then
      begin
        for Index := 1 to LayerGroup.LayerCount do
        begin
          clbLayer.Items.Add(IntToStr(Index));
          clbLayer.Checked[Index-1] := True;
        end;
      end
      else
      begin
        for Index := 0 to NoDelayItem.UsedLayers.Count - 1 do
        begin
          UsedLayerItem := NoDelayItem.UsedLayers[Index];
          clbLayer.Items.Add(IntToStr(UsedLayerItem.LayerNumber));
        end;
        if NoDelayItem.UsedLayers.Count = 1 then
        begin
          clbLayer.Checked[0] := True;
        end;
      end;
    end
    else
    begin
      clbLayer.Enabled := False;
    end;
  end
  else
  begin
    clbLayer.Enabled := False;
  end;
end;

procedure TframeScreenObjectHydmod.edHYDLBLExit(Sender: TObject);
var
  NewText: string;
begin
  NewText := string(AnsiString(StringReplace(Trim(edHYDLBL.Text),
    ' ', '_', [rfReplaceAll, rfIgnoreCase])));
  if edHYDLBL.Text <> NewText then
  begin
    edHYDLBL.Text := NewText
  end;
end;

procedure TframeScreenObjectHydmod.FillListOfScreenObjects(
  ListOfScreenObjects: TList; List: TScreenObjectEditCollection);
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    if (ScreenObject.ModflowHydmodData <> nil)
      and (ScreenObject.ModflowHydmodData.IsUsed) then
    begin
      ListOfScreenObjects.Add(ScreenObject);
    end;
  end;
end;

procedure TframeScreenObjectHydmod.GetData(
  const List: TScreenObjectEditCollection);
const
  CheckBoxStates: array[False..True] of TCheckBoxState = (cbUnchecked, cbChecked);
var
//  Packages: TModflowPackages;
  LayerStructure: TLayerStructure;
  Index: Integer;
  LayerGroup: TLayerGroup;
  ListOfScreenObjects: TList;
  FirstScreenObject: TScreenObject;
  ScreenObject: TScreenObject;
  FirstHydmodData: THydmodData;
  HydmodData: THydmodData;
  UniformLabel: Boolean;
  UniformAssignmentMethod: Boolean;
  UniformHead: Boolean;
  UniformDrawDown: Boolean;
  UniformSfrStage: Boolean;
  UniformSfrFlowIn: Boolean;
  UniformSfrFlowOut: Boolean;
  UniformSfrAquiferExchange: Boolean;
  UniformSubCompaction: Boolean;
  UniformSubSubsidence: Boolean;
  UniformSubLayerGroup: Boolean;
  UniformSubNoDelayBed: Boolean;
  UniformSubUsedLayers: Boolean;
  LayerNames: TStringList;
  NoDelayNames: TStringList;
  LayerNumber: Integer;
  Position: Integer;
  UniformSubPreconsolidationHead: Boolean;

begin
//  Packages := frmGoPhast.PhastModel.ModflowPackages;
  clbSFR.Enabled := frmGoPhast.PhastModel.SfrIsSelected;
  clbSub.Enabled := frmGoPhast.PhastModel.SubIsSelected;
  comboLayerGroup.Enabled := frmGoPhast.PhastModel.SubIsSelected;
  comboNoDelayBed.Enabled := frmGoPhast.PhastModel.SubIsSelected;

  {$IF CompilerVersion > 28}
  comboLayerGroup.Items.ClearAndResetID;
  {$ENDIF}
  LayerStructure := frmGoPhast.PhastModel.LayerStructure;
  for Index := 1 to LayerStructure.Count - 1 do
  begin
    LayerGroup := LayerStructure[Index];
    comboLayerGroup.Items.AddObject(LayerGroup.AquiferName, LayerGroup);
  end;

  // This will cause TCustomRadioGroup.UpdateButtons to be called.
  rgINTYP.WordWrap := not rgINTYP.WordWrap;
  rgINTYP.WordWrap := not rgINTYP.WordWrap;

  rgINTYP.Handle;
  rgINTYP.Buttons[Ord(amInterpolate)].Enabled := InterpolationAllowed(List);
//  rgINTYP.Controls[Ord(amInterpolate)].Enabled := InterpolationAllowed(List);

  ListOfScreenObjects := TList.Create();
  try
    FillListOfScreenObjects(ListOfScreenObjects, List);
    if ListOfScreenObjects.Count = 0 then
    begin
      edHYDLBL.Text := '';
      rgINTYP.ItemIndex := 0;
      for Index := 0 to clbBasic.Items.Count - 1 do
      begin
        clbBasic.State[Index] := cbUnchecked;
      end;
      for Index := 0 to clbSFR.Items.Count - 1 do
      begin
        clbSFR.State[Index] := cbUnchecked;
      end;
      for Index := 0 to clbSub.Items.Count - 1 do
      begin
        clbSub.State[Index] := cbUnchecked;
      end;
      comboLayerGroup.ItemIndex := -1;
      comboNoDelayBed.ItemIndex := -1;
      clbLayer.Clear;
    end
    else
    begin
      if ListOfScreenObjects.Count = 1 then
      begin
        FirstScreenObject := ListOfScreenObjects[0];
        FirstHydmodData := FirstScreenObject.ModflowHydmodData;
        UniformLabel := True;
        UniformAssignmentMethod := True;
        UniformHead := True;
        UniformDrawDown := True;
        UniformSfrStage := True;
        UniformSfrFlowIn := True;
        UniformSfrFlowOut := True;
        UniformSfrAquiferExchange := True;
        UniformSubPreconsolidationHead := True;
        UniformSubCompaction := True;
        UniformSubSubsidence := True;
        UniformSubLayerGroup := True;
        UniformSubNoDelayBed := True;
        UniformSubUsedLayers := True;
      end
      else
      begin
        FirstScreenObject := ListOfScreenObjects[0];
        FirstHydmodData := FirstScreenObject.ModflowHydmodData;

        UniformLabel := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformLabel :=
            FirstHydmodData.HydrographLabel = HydmodData.HydrographLabel;
          if not UniformLabel then
          begin
            break;
          end;
        end;

        UniformAssignmentMethod := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformAssignmentMethod :=
            FirstHydmodData.AssignmentMethod = HydmodData.AssignmentMethod;
          if not UniformAssignmentMethod then
          begin
            break;
          end;
        end;

        UniformHead := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformHead :=
            FirstHydmodData.Head = HydmodData.Head;
          if not UniformHead then
          begin
            break;
          end;
        end;

        UniformDrawDown := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformDrawDown :=
            FirstHydmodData.Drawdown = HydmodData.Drawdown;
          if not UniformDrawDown then
          begin
            break;
          end;
        end;

        UniformSfrStage := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformSfrStage :=
            FirstHydmodData.SfrStage = HydmodData.SfrStage;
          if not UniformSfrStage then
          begin
            break;
          end;
        end;

        UniformSfrFlowIn := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformSfrFlowIn :=
            FirstHydmodData.SfrInFlow = HydmodData.SfrInFlow;
          if not UniformSfrFlowIn then
          begin
            break;
          end;
        end;

        UniformSfrFlowOut := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformSfrFlowOut :=
            FirstHydmodData.SfrOutFlow = HydmodData.SfrOutFlow;
          if not UniformSfrFlowOut then
          begin
            break;
          end;
        end;

        UniformSfrAquiferExchange := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformSfrAquiferExchange :=
            FirstHydmodData.SfrAquiferExchange = HydmodData.SfrAquiferExchange;
          if not UniformSfrAquiferExchange then
          begin
            break;
          end;
        end;

        UniformSubPreconsolidationHead := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformSubPreconsolidationHead :=
            FirstHydmodData.SubPreconsolidationHead = HydmodData.SubPreconsolidationHead;
          if not UniformSubPreconsolidationHead then
          begin
            break;
          end;
        end;

        UniformSubCompaction := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformSubCompaction :=
            FirstHydmodData.SubCompaction = HydmodData.SubCompaction;
          if not UniformSubCompaction then
          begin
            break;
          end;
        end;

        UniformSubSubsidence := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformSubSubsidence :=
            FirstHydmodData.SubSubsidence = HydmodData.SubSubsidence;
          if not UniformSubSubsidence then
          begin
            break;
          end;
        end;

        UniformSubLayerGroup := True;
        for Index := 1 to ListOfScreenObjects.Count - 1 do
        begin
          ScreenObject := ListOfScreenObjects[Index];
          HydmodData := ScreenObject.ModflowHydmodData;
          UniformSubLayerGroup :=
            FirstHydmodData.SubLayerGroup = HydmodData.SubLayerGroup;
          if not UniformSubLayerGroup then
          begin
            break;
          end;
        end;

        if UniformSubLayerGroup then
        begin
          UniformSubNoDelayBed := True;
          for Index := 1 to ListOfScreenObjects.Count - 1 do
          begin
            ScreenObject := ListOfScreenObjects[Index];
            HydmodData := ScreenObject.ModflowHydmodData;
            UniformSubNoDelayBed :=
              FirstHydmodData.SubNoDelayBed = HydmodData.SubNoDelayBed;
            if not UniformSubNoDelayBed then
            begin
              break;
            end;
          end;
        end
        else
        begin
          UniformSubNoDelayBed := False;
        end;

        if UniformSubNoDelayBed then
        begin
          UniformSubUsedLayers := True;
          for Index := 1 to ListOfScreenObjects.Count - 1 do
          begin
            ScreenObject := ListOfScreenObjects[Index];
            HydmodData := ScreenObject.ModflowHydmodData;
            UniformSubUsedLayers :=
              FirstHydmodData.SubUsedLayers.IsSame(HydmodData.SubUsedLayers);
            if not UniformSubUsedLayers then
            begin
              break;
            end;
          end;
        end
        else
        begin
          UniformSubUsedLayers := False;
        end;
      end;
      if UniformLabel then
      begin
        edHYDLBL.Text := FirstHydmodData.HydrographLabel;
      end
      else
      begin
        edHYDLBL.Text := '';
      end;
      if UniformAssignmentMethod then
      begin
        rgINTYP.ItemIndex := Ord(FirstHydmodData.AssignmentMethod);
      end
      else
      begin
        rgINTYP.ItemIndex := -1;
      end;

      if UniformHead and UniformDrawDown then
      begin
        clbBasic.AllowGrayed := False;
      end
      else
      begin
        clbBasic.AllowGrayed := True;
      end;

      if UniformHead then
      begin
        clbBasic.State[Ord(brHead)] := CheckBoxStates[FirstHydmodData.Head];
      end
      else
      begin
        clbBasic.State[Ord(brHead)] := cbGrayed;
      end;

      if UniformDrawDown then
      begin
        clbBasic.State[Ord(brDrawDown)] :=
          CheckBoxStates[FirstHydmodData.Drawdown];
      end
      else
      begin
        clbBasic.State[Ord(brDrawDown)] := cbGrayed;
      end;

      if UniformSfrStage and UniformSfrFlowIn and UniformSfrFlowOut
        and UniformSfrAquiferExchange then
      begin
        clbSFR.AllowGrayed := False;
      end
      else
      begin
        clbSFR.AllowGrayed := True;
      end;

      if UniformSfrStage then
      begin
        clbSFR.State[Ord(srStage)] := CheckBoxStates[FirstHydmodData.SfrStage];
      end
      else
      begin
        clbSFR.State[Ord(srStage)] := cbGrayed;
      end;

      if UniformSfrFlowIn then
      begin
        clbSFR.State[Ord(srInflow)] := CheckBoxStates[FirstHydmodData.SfrInFlow];
      end
      else
      begin
        clbSFR.State[Ord(srInflow)] := cbGrayed;
      end;

      if UniformSfrFlowOut then
      begin
        clbSFR.State[Ord(srOutflow)] := CheckBoxStates[FirstHydmodData.SfrOutFlow];
      end
      else
      begin
        clbSFR.State[Ord(srOutflow)] := cbGrayed;
      end;

      if UniformSfrAquiferExchange then
      begin
        clbSFR.State[Ord(srExchange)] := CheckBoxStates[FirstHydmodData.SfrAquiferExchange];
      end
      else
      begin
        clbSFR.State[Ord(srExchange)] := cbGrayed;
      end;

      if UniformSubPreconsolidationHead
        and UniformSubCompaction and UniformSubSubsidence then
      begin
        clbSub.AllowGrayed := False;
      end
      else
      begin
        clbSub.AllowGrayed := True;
      end;

      if UniformSubPreconsolidationHead then
      begin
        clbSub.State[Ord(srPreconsolidationHead)] :=
          CheckBoxStates[FirstHydmodData.SubPreconsolidationHead];
      end
      else
      begin
        clbSub.State[Ord(srPreconsolidationHead)] := cbGrayed;
      end;

      if UniformSubCompaction then
      begin
        clbSub.State[Ord(srCompaction)] := CheckBoxStates[FirstHydmodData.SubCompaction];
      end
      else
      begin
        clbSub.State[Ord(srCompaction)] := cbGrayed;
      end;

      if UniformSubSubsidence then
      begin
        clbSub.State[Ord(srSubsidence)] := CheckBoxStates[FirstHydmodData.SubSubsidence];
      end
      else
      begin
        clbSub.State[Ord(srSubsidence)] := cbGrayed;
      end;

      if UniformSubLayerGroup then
      begin
        LayerNames := TStringList.Create;
        try
          for Index := 0 to comboLayerGroup.Items.Count - 1 do
          begin
            LayerNames.Add(comboLayerGroup.Items[Index].Text)
          end;
          comboLayerGroup.ItemIndex :=
            LayerNames.IndexOf(FirstHydmodData.SubLayerGroup);
          if Assigned(comboLayerGroup.OnChange) then
          begin
            comboLayerGroup.OnChange(nil);
          end;
        finally
          LayerNames.Free;
        end;
      end
      else
      begin
        comboLayerGroup.ItemIndex := -1;
      end;

      if UniformSubNoDelayBed then
      begin
        NoDelayNames := TStringList.Create;
        try
          for Index := 0 to comboNoDelayBed.Items.Count - 1 do
          begin
            NoDelayNames.Add(comboNoDelayBed.Items[Index].Text)
          end;
          comboNoDelayBed.ItemIndex :=
            NoDelayNames.IndexOf(FirstHydmodData.SubNoDelayBed);
          if Assigned(comboNoDelayBed.OnChange) then
          begin
            comboNoDelayBed.OnChange(nil);
          end;
        finally
          NoDelayNames.Free;
        end;
      end
      else
      begin
        comboNoDelayBed.ItemIndex := -1;
      end;

      if UniformSubUsedLayers then
      begin
        for Index := 0 to clbLayer.Items.Count - 1 do
        begin
          clbLayer.Checked[Index] := False;
        end;
        for Index := 0 to FirstHydmodData.SubUsedLayers.Count - 1 do
        begin
          LayerNumber := FirstHydmodData.SubUsedLayers[Index].LayerNumber;
          Position := clbLayer.Items.IndexOf(IntToStr(LayerNumber));
          if Position >= 0 then
          begin
            clbLayer.Checked[Position] := True;
          end;
        end;
      end
      else
      begin
        clbLayer.AllowGrayed := True;
        for Index := 0 to clbLayer.Items.Count - 1 do
        begin
          clbLayer.State[Index] := cbGrayed;
        end;
      end;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
  SetAllowedAssignmentMethod;

end;

function TframeScreenObjectHydmod.InterpolationAllowed(
  const List: TScreenObjectEditCollection): boolean;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  result := False;
  for Index := 0 to List.Count - 1 do
  begin
    ScreenObject := List[Index].ScreenObject;
    if (ScreenObject.ViewDirection = vdTop) and
      (ScreenObject.Count = ScreenObject.SectionCount) then
    begin
      result := True;
      break;
    end;
  end;

end;

procedure TframeScreenObjectHydmod.SetAllowedAssignmentMethod;
var
  Index: Integer;
  AnyChecked: Boolean;
  NewText: string;
begin
  AnyChecked := False;
  if clbSFR.Enabled then
  begin
    for Index := 0 to clbSFR.Items.Count - 1 do
    begin
      AnyChecked := clbSFR.Checked[Index];
      if AnyChecked then
      begin
        break;
      end;
    end;
  end;
  rgINTYP.Enabled := not AnyChecked;
  if AnyChecked then
  begin
    rgINTYP.ItemIndex := 0;
    edHYDLBL.MaxLength := 12;
    NewText := Copy(edHYDLBL.Text, 1, 12);
    if edHYDLBL.Text <> NewText then
    begin
      edHYDLBL.Text := NewText;
    end;
  end
  else
  begin
    edHYDLBL.MaxLength := 14;
  end;
end;

procedure TframeScreenObjectHydmod.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ListOfScreenObjects: TList;
  Index: Integer;
  ScreenObject: TScreenObject;
  ModflowHydmodData: THydmodData;
  SubUsedLayers: TUseLayersCollection;
  LayerNumber: Integer;
  Item: TUseLayerNumberItem;
  LayerIndex: Integer;
begin
  ListOfScreenObjects := TList.Create;
  try
    Assert(List.Count >= 1);
    for Index := 0 to List.Count - 1 do
    begin
      ListOfScreenObjects.Add(List[Index].ScreenObject);
    end;
    for Index := 0 to ListOfScreenObjects.Count - 1 do
    begin
      ScreenObject := ListOfScreenObjects[Index];
      ModflowHydmodData := ScreenObject.ModflowHydmodData;
      Assert(ModflowHydmodData <> nil);

      if SetAll then
      begin
        ModflowHydmodData.IsUsed := True;
      end
      else if ClearAll then
      begin
        ModflowHydmodData.IsUsed := False;
      end;

      if ModflowHydmodData.IsUsed then
      begin
        if edHYDLBL.Text <> '' then
        begin
          ModflowHydmodData.HydrographLabel := edHYDLBL.Text;
        end;
        if rgINTYP.ItemIndex >= 0 then
        begin
          rgINTYP.Handle;
          if rgINTYP.Enabled and rgINTYP.Buttons[Ord(amInterpolate)].Enabled then
//          if rgINTYP.Enabled and rgINTYP.Controls[Ord(amInterpolate)].Enabled then
          begin
            ModflowHydmodData.AssignmentMethod :=
              TAssignmentMethod(rgINTYP.ItemIndex);
          end
          else
          begin
            ModflowHydmodData.AssignmentMethod := amCell;
          end;
        end;
        if clbBasic.State[Ord(brHead)] <> cbGrayed then
        begin
          ModflowHydmodData.Head := clbBasic.Checked[Ord(brHead)];
        end;
        if clbBasic.State[Ord(brDrawDown)] <> cbGrayed then
        begin
          ModflowHydmodData.Drawdown := clbBasic.Checked[Ord(brDrawDown)];
        end;
        if clbSfr.State[Ord(srStage)] <> cbGrayed then
        begin
          ModflowHydmodData.SfrStage := clbSfr.Checked[Ord(srStage)];
        end;
        if clbSfr.State[Ord(srInflow)] <> cbGrayed then
        begin
          ModflowHydmodData.SfrInFlow := clbSfr.Checked[Ord(srInflow)];
        end;
        if clbSfr.State[Ord(srOutflow)] <> cbGrayed then
        begin
          ModflowHydmodData.SfrOutFlow := clbSfr.Checked[Ord(srOutflow)];
        end;
        if clbSfr.State[Ord(srExchange)] <> cbGrayed then
        begin
          ModflowHydmodData.SfrAquiferExchange := clbSfr.Checked[Ord(srExchange)];
        end;
        if clbSub.State[Ord(srPreconsolidationHead)] <> cbGrayed then
        begin
          ModflowHydmodData.SubPreconsolidationHead := clbSub.Checked[Ord(srPreconsolidationHead)];
        end;
        if clbSub.State[Ord(srCompaction)] <> cbGrayed then
        begin
          ModflowHydmodData.SubCompaction := clbSub.Checked[Ord(srCompaction)];
        end;
        if clbSub.State[Ord(srSubsidence)] <> cbGrayed then
        begin
          ModflowHydmodData.SubSubsidence := clbSub.Checked[Ord(srSubsidence)];
        end;
        if comboLayerGroup.Text <> '' then
        begin
          ModflowHydmodData.SubLayerGroup := comboLayerGroup.Text;
        end;
        if comboNoDelayBed.Text <> '' then
        begin
          ModflowHydmodData.SubNoDelayBed := comboNoDelayBed.Text;
        end;
        SubUsedLayers := ModflowHydmodData.SubUsedLayers;
        for LayerIndex := 0 to clbLayer.Items.Count - 1 do
        begin
          case clbLayer.State[LayerIndex] of
            cbUnchecked:
              begin
                LayerNumber := StrToInt(clbLayer.Items[LayerIndex]);
                Item := SubUsedLayers.GetItemByLayerNumber(LayerNumber);
                if Item <> nil then
                begin
                  SubUsedLayers.Delete(Item.Index);
                end;
              end;
            cbChecked:
              begin
                LayerNumber := StrToInt(clbLayer.Items[LayerIndex]);
                Item := SubUsedLayers.GetItemByLayerNumber(LayerNumber);
                if Item = nil then
                begin
                  Item := SubUsedLayers.Add;
                  Item.LayerNumber := LayerNumber;
                end;
              end;
            cbGrayed:
              begin
                // do nothing.
              end;
            else
              begin
                Assert(False);
              end;
          end;
        end;
      end;
    end;
  finally
    ListOfScreenObjects.Free;
  end;
end;

end.
