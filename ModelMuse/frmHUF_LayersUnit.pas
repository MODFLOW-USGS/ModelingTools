unit frmHUF_LayersUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  ArgusDataEntry, JvExStdCtrls, JvCombobox, JvListComb, Grids, RbwDataGrid4,
  CheckLst, HufDefinition, RequiredDataSetsUndoUnit, GoPhastTypes, GrayTabs,
  DataArrayInterfaceUnit;

type
  THufParamGridColumns = (hpgcName, hpgcType, hpgcUseZone, hpgcUseMultiplier);

  TfrmHUF_Layers = class(TfrmCustomGoPhast)
    Panel3: TPanel;
    GridPanel1: TGridPanel;
    sbAddUnit: TSpeedButton;
    sbInsertUnit: TSpeedButton;
    sbDeleteUnit: TSpeedButton;
    tvHufLayers: TTreeView;
    Splitter1: TSplitter;
    pcMain: TPageControl;
    Properties: TTabSheet;
    lblHydrogeologicUnitName: TLabel;
    edHydrogeologicUnitName: TEdit;
    lblHorizontalAnisotropy: TLabel;
    rdeHorizontalAnisotropy: TRbwDataEntry;
    lblVK_Method: TLabel;
    comboVK_Method: TJvImageComboBox;
    lblVerticalAnisotropy: TLabel;
    rdeVerticalAnisotropy: TRbwDataEntry;
    tabParameters: TTabSheet;
    tabPrint: TTabSheet;
    rdgParameters: TRbwDataGrid4;
    clbItemsToPrint: TCheckListBox;
    comboPrintFormat: TJvImageComboBox;
    lblPrintFormat: TLabel;
    Panel4: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject); override;
    procedure rdgParametersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure rdeHorizontalAnisotropyChange(Sender: TObject);
    procedure edHydrogeologicUnitNameChange(Sender: TObject);
    procedure rdeVerticalAnisotropyChange(Sender: TObject);
    procedure sbAddUnitClick(Sender: TObject);
    procedure sbInsertUnitClick(Sender: TObject);
    procedure tvHufLayersChange(Sender: TObject; Node: TTreeNode);
    procedure sbDeleteUnitClick(Sender: TObject);
    procedure comboVK_MethodChange(Sender: TObject);
    procedure rdgParametersStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure comboPrintFormatChange(Sender: TObject);
    procedure clbItemsToPrintClickCheck(Sender: TObject);
    procedure edHydrogeologicUnitNameExit(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
  private
    FSelectedUnits: TList;
    FSelectedTreeNodes: TList;
    FHydrogeologicUnits: THydrogeologicUnits;
    FSelectingUnits: Boolean;
    procedure GetData;
    procedure SetData;
    procedure UpdateSelectedUnits;
    procedure AssignHorizontalAnisotropy;
    procedure AssignVerticalAnisotropy(HufUnit: THydrogeologicUnit);
    procedure AssignVK_Method(HufUnit: THydrogeologicUnit);
    procedure AssignParameters(HufUnit: THydrogeologicUnit);
    function GenerateNewName: string;
    procedure AssignUseMultiplier(ParamName: string; ParamIndex: Integer;
      HufUnitsThatUseParameter: TList; FirstUsedParameter: THufUsedParameter);
    procedure AssignUseZone(ParamName: string; ParamIndex: Integer;
      HufUnitsThatUseParameter: TList; FirstUsedParameter: THufUsedParameter);
    procedure AssignPrintFormat(HufUnit: THydrogeologicUnit);
    procedure AssignPrint(HufUnit: THydrogeologicUnit);
    function CheckHuf: boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoHufLayers = class(TCustomCreateRequiredDataSetsUndo)
    private
      FNewHydrogeologicUnits: THydrogeologicUnits;
      FOldHydrogeologicUnits: THydrogeologicUnits;
      FNewDataSets: TIDataArrayList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignNewHufUnits(AModel: TBaseModel);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignOldHufUnits(AModel: TBaseModel);
    protected
      function Description: string; override;
    Public
      Constructor Create(var NewHydrogeologicUnits: THydrogeologicUnits);
     	Destructor Destroy; override;
     	procedure DoCommand; override;
     	procedure Undo; override;
  end;

var
  frmHUF_Layers: TfrmHUF_Layers;

implementation

uses
  frmGoPhastUnit, OrderedCollectionUnit, PhastModelUnit;

resourcestring
  StrChangeHydrogeologic = 'change hydrogeologic units';
  StrNoHydrogeologicUni = 'No hydrogeologic units have been defined.  Are yo' +
  'u sure you want to close the dialog box?';
  StrNoParametersHaveB = 'No parameters have been associated with the follow' +
  'ing Hydrogeologic units.  Are you sure you want to close the dialog box? ' +
  '%0:s%0:s%1:s';
  StrTheFollowingParame = 'The following parameters are not associated with ' +
  'any Hydrogeologic units.  Are you sure you want to close the dialog box? ' +
  '%0:s%0:s%1:s';
  StrParameters = 'Parameters';
  StrType = 'Type';
  StrUseZone = 'Use Zone';
  StrUseMultiplier = 'Use Multiplier';
  StrOneOrMoreHUFUnit = 'One or more HUF unit names are duplicated. You need' +
  ' to fix this. The duplicates names are %s.';

{$R *.dfm}

function TfrmHUF_Layers.CheckHuf: boolean;
var
  HufIndex: Integer;
  HufUnit: THydrogeologicUnit;
  Names: TStringList;
  ParamIndex: Integer;
  Parameter: THufParameter;
  IsUsed: Boolean;
  UsedNames: TStringList;
  DuplicateNames: TStringList;
begin
  if FHydrogeologicUnits.Count = 0 then
  begin
    result := MessageDlg(StrNoHydrogeologicUni,
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  end
  else
  begin
    result := True;
    Names := TStringList.Create;
    UsedNames := TStringList.Create;
    DuplicateNames := TStringList.Create;
    try
      UsedNames.CaseSensitive := False;
      UsedNames.Sorted := true;
      for HufIndex := 0 to FHydrogeologicUnits.Count - 1 do
      begin
        HufUnit := FHydrogeologicUnits[HufIndex];
        if HufUnit.HufUsedParameters.Count = 0 then
        begin
          Names.Add(HufUnit.HufName)
        end;
        if UsedNames.IndexOf(HufUnit.HufName) < 0 then
        begin
          UsedNames.Add(HufUnit.HufName);
        end
        else
        begin
          if DuplicateNames.IndexOf(HufUnit.HufName) < 0 then
          begin
            DuplicateNames.Add(HufUnit.HufName);
          end;
        end;
      end;
      if Names.Count  > 0 then
      begin
        result := MessageDlg(Format(StrNoParametersHaveB,
          [sLineBreak, Names.Text]), mtConfirmation, [mbYes, mbNo], 0) = mrYes;
      end;
      if DuplicateNames.Count > 0 then
      begin
        result := False;
        MessageDlg(Format(StrOneOrMoreHUFUnit,[DuplicateNames.Text]), mtError, [mbOK], 0);
      end;
    finally
      Names.Free;
      UsedNames.Free;
      DuplicateNames.Free;
    end;
    if result then
    begin
      Names := TStringList.Create;
      try
        for ParamIndex := 0 to frmGoPhast.PhastModel.HufParameters.Count - 1 do
        begin
          Parameter := frmGoPhast.PhastModel.HufParameters[ParamIndex];
          IsUsed := False;
          for HufIndex := 0 to FHydrogeologicUnits.Count - 1 do
          begin
            HufUnit := FHydrogeologicUnits[HufIndex];
            IsUsed := HufUnit.HufUsedParameters.IsUsed(Parameter.ParameterName);
            if IsUsed then
            begin
              break;
            end;
          end;
          if not IsUsed then
          begin
            Names.Add(Parameter.ParameterName)
          end;
        end;
        if Names.Count  > 0 then
        begin
          result := MessageDlg(Format(StrTheFollowingParame,
            [sLineBreak, Names.Text]), mtConfirmation, [mbYes, mbNo], 0) = mrYes;
        end;
      finally
        Names.Free;
      end;
    end;
  end;
end;

procedure TfrmHUF_Layers.btnOKClick(Sender: TObject);
begin
  inherited;
  if not CheckHuf then
  begin
    ModalResult := mrNone;
    Exit;
  end;
  SetData;
end;

procedure TfrmHUF_Layers.clbItemsToPrintClickCheck(Sender: TObject);
var
  ParamIndex: TPrintParam;
  NewValue: Boolean;
  Index: Integer;
  HufUnit: THydrogeologicUnit;
begin
  inherited;
  if FSelectingUnits or (csReading in ComponentState) then
  begin
    Exit;
  end;
  for ParamIndex := Low(TPrintParam) to High(TPrintParam) do
  begin
    if clbItemsToPrint.State[Ord(ParamIndex)] <> cbGrayed then
    begin
      NewValue := clbItemsToPrint.Checked[Ord(ParamIndex)];
      for Index := 0 to FSelectedUnits.Count - 1 do
      begin
        HufUnit := FSelectedUnits[Index];
        HufUnit.Print[ParamIndex] := NewValue;
      end;
    end;
  end;

end;

procedure TfrmHUF_Layers.comboPrintFormatChange(Sender: TObject);
var
  NewValue: Integer;
  Index: Integer;
  HufUnit: THydrogeologicUnit;
begin
  inherited;
  if FSelectingUnits or (csReading in ComponentState) then
  begin
    Exit;
  end;
  if comboPrintFormat.ItemIndex >= 0 then
  begin
    NewValue := comboPrintFormat.ItemIndex+1;
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      HufUnit := FSelectedUnits[Index];
      HufUnit.PrintFormat := NewValue;
    end;
  end;
end;

procedure TfrmHUF_Layers.comboVK_MethodChange(Sender: TObject);
var
  NewValue: TVK_Method;
  Index: Integer;
  HufUnit: THydrogeologicUnit;
begin
  inherited;
  if FSelectingUnits or (csReading in ComponentState) then
  begin
    Exit;
  end;
  if comboVK_Method.ItemIndex >= 0 then
  begin
    NewValue := TVK_Method(comboVK_Method.ItemIndex);
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      HufUnit := FSelectedUnits[Index];
      HufUnit.VK_Method := NewValue;
    end;
  end;
  UpdateSelectedUnits;
end;

procedure TfrmHUF_Layers.edHydrogeologicUnitNameChange(Sender: TObject);
var
  HufUnit: THydrogeologicUnit;
  Node: TTreeNode;
begin
  inherited;
  if FSelectingUnits or (csReading in ComponentState) then
  begin
    Exit;
  end;
  if (FSelectedUnits.Count = 1) then
  begin
    Assert(FSelectedTreeNodes.Count = 1);
    HufUnit := FSelectedUnits[0];
    HufUnit.HufName := string(AnsiString(edHydrogeologicUnitName.Text));
    Node := FSelectedTreeNodes[0];
    Node.Text := HufUnit.HufName;
  end;
end;

procedure TfrmHUF_Layers.edHydrogeologicUnitNameExit(Sender: TObject);
var
  HufUnit: THydrogeologicUnit;
begin
  if (FSelectedUnits.Count = 1) then
  begin
    Assert(FSelectedTreeNodes.Count = 1);
    HufUnit := FSelectedUnits[0];
    if HufUnit.HufName <> edHydrogeologicUnitName.Text then
    begin
      edHydrogeologicUnitName.Text := HufUnit.HufName;
    end;
  end;
end;

procedure TfrmHUF_Layers.FormCreate(Sender: TObject);
begin
  inherited;
  pcMain.ActivePageIndex := 0;
  rdgParameters.EditorMode := False;
  rdgParameters.Cells[Ord(hpgcName),0] := StrParameters;
  rdgParameters.Cells[Ord(hpgcType),0] := StrType;
  rdgParameters.Cells[Ord(hpgcUseZone),0] := StrUseZone;
  rdgParameters.Cells[Ord(hpgcUseMultiplier),0] := StrUseMultiplier;
  FHydrogeologicUnits:= THydrogeologicUnits.Create(nil);
  FSelectedUnits:= TList.Create;
  FSelectedTreeNodes:= TList.Create;
  GetData;
end;

procedure TfrmHUF_Layers.FormDestroy(Sender: TObject);
begin
  inherited;
  FSelectedTreeNodes.Free;
  FSelectedUnits.Free;
  FHydrogeologicUnits.Free;
end;

procedure TfrmHUF_Layers.GetData;
var
  Index: Integer;
  HGU: THydrogeologicUnit;
  Node: TTreeNode;
  Parameter: TModflowParameter;
begin
  rdgParameters.RowCount := frmGoPhast.PhastModel.HufParameters.Count + 1;
  for Index := 0 to frmGoPhast.PhastModel.HufParameters.Count - 1 do
  begin
    Parameter := frmGoPhast.PhastModel.HufParameters.Items[Index] as TModflowParameter;
    rdgParameters.Cells[Ord(hpgcName), Index+1] := Parameter.ParameterName;
    rdgParameters.Objects[Ord(hpgcType), Index+1] := Parameter;
    case Parameter.ParameterType of
      ptHUF_HK: rdgParameters.Cells[Ord(hpgcType), Index+1] := 'HK';
      ptHUF_HANI: rdgParameters.Cells[Ord(hpgcType), Index+1] := 'HANI' ;
      ptHUF_VK: rdgParameters.Cells[Ord(hpgcType), Index+1] := 'VK' ;
      ptHUF_VANI: rdgParameters.Cells[Ord(hpgcType), Index+1] := 'VANI' ;
      ptHUF_SS: rdgParameters.Cells[Ord(hpgcType), Index+1] := 'SS' ;
      ptHUF_SY: rdgParameters.Cells[Ord(hpgcType), Index+1] := 'SY' ;
      ptHUF_KDEP: rdgParameters.Cells[Ord(hpgcType), Index+1] := 'KDEP' ;
      else Assert(False);
    end;
  end;

  FHydrogeologicUnits.Assign(frmGoPhast.PhastModel.HydrogeologicUnits);
  for Index := 0 to FHydrogeologicUnits.Count - 1 do
  begin
    HGU := FHydrogeologicUnits[Index];
    Node := tvHufLayers.Items.Add(nil, HGU.HufName);
    Node.Data := HGU;
  end;
end;

procedure TfrmHUF_Layers.pcMainChange(Sender: TObject);
begin
  inherited;
  btnHelp.HelpKeyword := pcMain.ActivePage.HelpKeyword;
end;

procedure TfrmHUF_Layers.rdeHorizontalAnisotropyChange(Sender: TObject);
var
  Index: Integer;
  HufUnit: THydrogeologicUnit;
  NewValue: double;
begin
  inherited;
  if FSelectingUnits or (csReading in ComponentState) then
  begin
    Exit;
  end;
  if rdeHorizontalAnisotropy.Text <> '' then
  begin
    NewValue := StrToFloat(rdeHorizontalAnisotropy.Text);
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      HufUnit := FSelectedUnits[Index];
      HufUnit.HorizontalAnisotropy := NewValue;
    end;
  end;
end;

procedure TfrmHUF_Layers.rdeVerticalAnisotropyChange(Sender: TObject);
var
  Index: Integer;
  HufUnit: THydrogeologicUnit;
  NewValue: double;
begin
  inherited;
  if FSelectingUnits or (csReading in ComponentState) then
  begin
    Exit;
  end;
  if rdeVerticalAnisotropy.Text <> '' then
  begin
    NewValue := StrToFloat(rdeVerticalAnisotropy.Text);
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      HufUnit := FSelectedUnits[Index];
      HufUnit.VerticalAnisotropy := NewValue;
    end;
  end;
end;

procedure TfrmHUF_Layers.rdgParametersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow > 0)   then
  begin
    if (ACol in [Ord(hpgcUseZone), Ord(hpgcUseMultiplier)]) then
    begin
      CanSelect := rdgParameters.CheckState[Ord(hpgcName), ARow] <> cbUnchecked;
    end
    else if ACol = Ord(hpgcType) then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TfrmHUF_Layers.rdgParametersStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  NewValue: TCheckBoxState;
  Index: Integer;
  HGU: THydrogeologicUnit;
  ParameterName: string;
  UsedParam: THufUsedParameter;
begin
  inherited;
  if FSelectingUnits or (csReading in ComponentState) then
  begin
    Exit;
  end;
  NewValue := Value;
  if Value = cbGrayed then
  begin
    NewValue := cbUnchecked;
    rdgParameters.State[ACol, ARow] := NewValue;
  end;
  ParameterName := rdgParameters.Cells[Ord(hpgcName), ARow];
  case THufParamGridColumns(ACol) of
    hpgcName:
      begin
        for Index := 0 to FSelectedUnits.Count - 1 do
        begin
          HGU := FSelectedUnits[Index];
          UsedParam := HGU.HufUsedParameters.GetUsedParameterByName(ParameterName);
          case NewValue of
            cbUnchecked:
              if UsedParam <> nil then
              begin
                HGU.HufUsedParameters.Delete(UsedParam.Index);
              end;
            cbChecked:
              begin
                if UsedParam = nil then
                begin
                  UsedParam := HGU.HufUsedParameters.Add as THufUsedParameter;
                  UsedParam.ParameterName := ParameterName;
                  UsedParam.UseZone := rdgParameters.Checked[
                    Ord(hpgcUseZone), ARow];
                  UsedParam.UseMultiplier := rdgParameters.Checked[
                    Ord(hpgcUseMultiplier), ARow];
                  UsedParam.Parameter := rdgParameters.Objects[
                    Ord(hpgcType), ARow] as TModflowParameter;
                end;
              end
            else Assert(False);
          end;
          AssignHorizontalAnisotropy;
        end;
      end;
    hpgcUseZone:
      begin
        for Index := 0 to FSelectedUnits.Count - 1 do
        begin
          HGU := FSelectedUnits[Index];
          UsedParam := HGU.HufUsedParameters.GetUsedParameterByName(
            ParameterName);
          if UsedParam <> nil then
          begin
            UsedParam.UseZone := rdgParameters.Checked[
              Ord(hpgcUseZone), ARow];
          end;
        end;
      end;
    hpgcUseMultiplier:
      begin
        for Index := 0 to FSelectedUnits.Count - 1 do
        begin
          HGU := FSelectedUnits[Index];
          UsedParam := HGU.HufUsedParameters.GetUsedParameterByName(
            ParameterName);
          if UsedParam <> nil then
          begin
            UsedParam.UseMultiplier := rdgParameters.Checked[
              Ord(hpgcUseMultiplier), ARow];
          end;
        end;
      end;
    else Assert(False);
  end;
end;

procedure TfrmHUF_Layers.sbAddUnitClick(Sender: TObject);
var
  HGU: THydrogeologicUnit;
  Node: TTreeNode;
  NewName: string;
begin
  inherited;
  NewName := GenerateNewName;

  HGU := FHydrogeologicUnits.Add as THydrogeologicUnit;
  HGU.HufName := NewName;
  Node := tvHufLayers.Items.Add(nil, HGU.HufName);
  Node.Data := HGU;
  tvHufLayers.Selected := Node;
end;

procedure TfrmHUF_Layers.sbDeleteUnitClick(Sender: TObject);
var
  HGU: THydrogeologicUnit;
begin
  inherited;
  if tvHufLayers.Selected <> nil then
  begin
    HGU := tvHufLayers.Selected.Data;
    FHydrogeologicUnits.Delete(HGU.Index);
    tvHufLayers.Items.Delete(tvHufLayers.Selected);
  end;
end;

procedure TfrmHUF_Layers.sbInsertUnitClick(Sender: TObject);
var
  NewName: string;
  HGU: THydrogeologicUnit;
  Node: TTreeNode;
begin
  inherited;
  if tvHufLayers.Selected <> nil then
  begin
    NewName := GenerateNewName;

    HGU := tvHufLayers.Selected.Data;
    HGU := FHydrogeologicUnits.Insert(HGU.Index) as THydrogeologicUnit;
    HGU.HufName := NewName;
    Node := tvHufLayers.Items.Insert(tvHufLayers.Selected, HGU.HufName);
    Node.Data := HGU;
    tvHufLayers.Selected := Node;
  end;
end;

procedure TfrmHUF_Layers.SetData;
var
  Undo: TUndoHufLayers;
begin
  if not frmGoPhast.PhastModel.
    HydrogeologicUnits.IsSame(FHydrogeologicUnits) then
  begin
    Undo := TUndoHufLayers.Create(FHydrogeologicUnits);
    frmGoPhast.UndoStack.Submit(Undo);
  end;
end;

procedure TfrmHUF_Layers.tvHufLayersChange(Sender: TObject; Node: TTreeNode);
begin
  inherited;
  UpdateSelectedUnits;
end;

procedure TfrmHUF_Layers.UpdateSelectedUnits;
var
  Index: Integer;
  Node: TTreeNode;
  HufUnit: THydrogeologicUnit;
begin
  FSelectingUnits := True;
  try
    FSelectedUnits.Clear;
    FSelectedTreeNodes.Clear;
    for Index := 0 to tvHufLayers.Items.Count - 1 do
    begin
      Node := tvHufLayers.Items[Index];
      if Node.Selected then
      begin
        FSelectedUnits.Add(Node.Data);
        FSelectedTreeNodes.Add(Node);
      end;
    end;
    if FSelectedUnits.Count = 0 then
    begin
      edHydrogeologicUnitName.Enabled := False;
      rdeHorizontalAnisotropy.Enabled := False;
      comboVK_Method.Enabled := False;
      rdeVerticalAnisotropy.Enabled := False;
      rdgParameters.Enabled := False;
      comboPrintFormat.Enabled := False;
      clbItemsToPrint.Enabled := False;
      Exit;
    end
    else
    begin
      rdeHorizontalAnisotropy.Enabled := True;
      comboVK_Method.Enabled := True;
      rdeVerticalAnisotropy.Enabled := True;
      rdgParameters.Enabled := True;
      comboPrintFormat.Enabled := True;
      clbItemsToPrint.Enabled := True;
      if FSelectedUnits.Count = 1 then
      begin
        edHydrogeologicUnitName.Enabled := True;
        HufUnit := FSelectedUnits[0];
        edHydrogeologicUnitName.Text := HufUnit.HufName;
      end
      else
      begin
        edHydrogeologicUnitName.Enabled := False;
      end;
    end;
    HufUnit := FSelectedUnits[0];
    AssignHorizontalAnisotropy;
    AssignVK_Method(HufUnit);
    AssignVerticalAnisotropy(HufUnit);
    AssignParameters(HufUnit);
    AssignPrintFormat(HufUnit);
    AssignPrint(HufUnit);
  finally
    FSelectingUnits := False;
  end;
end;

procedure TfrmHUF_Layers.AssignHorizontalAnisotropy;
var
  HufUnit: THydrogeologicUnit;
  OtherHufUnit: THydrogeologicUnit;
  Index: Integer;
  AllTheSame: Boolean;
  AvailableUnits: TList;
begin
  AvailableUnits := TList.Create;
  try
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      HufUnit := FSelectedUnits[Index];
      if not HufUnit.UsesHaniParam then
      begin
        AvailableUnits.Add(HufUnit);
      end;
    end;
    if AvailableUnits.Count = 0 then
    begin
      rdeHorizontalAnisotropy.Enabled := False;
    end
    else
    begin
      rdeHorizontalAnisotropy.Enabled := True;
      HufUnit := AvailableUnits[0];
      AllTheSame := True;
      for Index := 1 to AvailableUnits.Count - 1 do
      begin
        OtherHufUnit := AvailableUnits[Index];
        if HufUnit.HorizontalAnisotropy <> OtherHufUnit.HorizontalAnisotropy then
        begin
          AllTheSame := False;
          break;
        end;
      end;
      if AllTheSame then
      begin
        rdeHorizontalAnisotropy.Text := FloatToStr(HufUnit.HorizontalAnisotropy);
      end
      else
      begin
        rdeHorizontalAnisotropy.Text := '';
      end;
    end;
  finally
    AvailableUnits.Free;
  end;
end;

procedure TfrmHUF_Layers.AssignPrintFormat(HufUnit: THydrogeologicUnit);
var
  OtherHufUnit: THydrogeologicUnit;
  Index: Integer;
  AllTheSame: Boolean;
begin
  AllTheSame := True;
  for Index := 1 to FSelectedUnits.Count - 1 do
  begin
    OtherHufUnit := FSelectedUnits[Index];
    if HufUnit.PrintFormat <> OtherHufUnit.PrintFormat then
    begin
      AllTheSame := False;
      break;
    end;
  end;
  if AllTheSame then
  begin
    comboPrintFormat.ItemIndex := HufUnit.PrintFormat-1;
  end
  else
  begin
    comboPrintFormat.ItemIndex := -1;
  end;
end;

procedure TfrmHUF_Layers.AssignPrint(HufUnit: THydrogeologicUnit);
var
  OtherHufUnit: THydrogeologicUnit;
  Index: Integer;
  AllTheSame: Boolean;
  PrintIndex: TPrintParam;
begin
  for PrintIndex := Low(TPrintParam) to High(TPrintParam) do
  begin
    AllTheSame := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      OtherHufUnit := FSelectedUnits[Index];
      if HufUnit.Print[PrintIndex] <> OtherHufUnit.Print[PrintIndex] then
      begin
        AllTheSame := False;
        break;
      end;
    end;
    if AllTheSame then
    begin
      clbItemsToPrint.Checked[Ord(PrintIndex)] := HufUnit.Print[PrintIndex];
    end
    else
    begin
      clbItemsToPrint.State[Ord(PrintIndex)] := cbGrayed;
    end;
  end;
end;

procedure TfrmHUF_Layers.AssignParameters(HufUnit: THydrogeologicUnit);
var
  AllTheSameUse: Boolean;
  Index: Integer;
  OtherHufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  ParamName: string;
  ParamUsed: Boolean;
  HufUnitsThatUseParameter: TList;
  OtherUsed: Boolean;
  FirstHufUnit: THydrogeologicUnit;
  FirstUsedParameter: THufUsedParameter;
begin
  HufUnitsThatUseParameter := TList.Create;
  try
    for ParamIndex := 1 to rdgParameters.RowCount - 1 do
    begin
      HufUnitsThatUseParameter.Clear;
      AllTheSameUse := True;
      ParamName := rdgParameters.Cells[Ord(hpgcName), ParamIndex];
      ParamUsed := HufUnit.HufUsedParameters.IsUsed(ParamName);
      if ParamUsed then
      begin
        HufUnitsThatUseParameter.Add(HufUnit);
      end;
      for Index := 1 to FSelectedUnits.Count - 1 do
      begin
        OtherHufUnit := FSelectedUnits[Index];
        OtherUsed := OtherHufUnit.HufUsedParameters.IsUsed(ParamName);
        if OtherUsed then
        begin
          HufUnitsThatUseParameter.Add(OtherHufUnit);
        end;
        if ParamUsed <> OtherUsed then
        begin
          AllTheSameUse := False;
        end;
      end;
      if AllTheSameUse then
      begin
        rdgParameters.Checked[Ord(hpgcName), ParamIndex] := ParamUsed;
      end
      else
      begin
        rdgParameters.State[Ord(hpgcName), ParamIndex] := cbGrayed;
      end;

      if HufUnitsThatUseParameter.Count > 0 then
      begin
        FirstHufUnit := HufUnitsThatUseParameter[0];
        FirstUsedParameter := FirstHufUnit.HufUsedParameters.
          GetUsedParameterByName(ParamName);
        AssignUseMultiplier(ParamName, ParamIndex, HufUnitsThatUseParameter,
          FirstUsedParameter);
        AssignUseZone(ParamName, ParamIndex, HufUnitsThatUseParameter,
          FirstUsedParameter);
      end
      else
      begin
        rdgParameters.Checked[Ord(hpgcUseMultiplier), ParamIndex] := False;
        rdgParameters.Checked[Ord(hpgcUseZone), ParamIndex] := False;
      end;
    end;
  finally
    HufUnitsThatUseParameter.Free;
  end;
end;

procedure TfrmHUF_Layers.AssignVerticalAnisotropy(HufUnit: THydrogeologicUnit);
var
  OtherHufUnit: THydrogeologicUnit;
  Index: Integer;
  AllTheSame: Boolean;
begin
  AllTheSame := True;
  for Index := 1 to FSelectedUnits.Count - 1 do
  begin
    OtherHufUnit := FSelectedUnits[Index];
    if HufUnit.VerticalAnisotropy <> OtherHufUnit.VerticalAnisotropy then
    begin
      AllTheSame := False;
      break;
    end;
  end;
  if AllTheSame then
  begin
    rdeVerticalAnisotropy.Text := FloatToStr(HufUnit.VerticalAnisotropy);
  end
  else
  begin
    rdeVerticalAnisotropy.Text := '';
  end;
end;

procedure TfrmHUF_Layers.AssignVK_Method(HufUnit: THydrogeologicUnit);
var
  OtherHufUnit: THydrogeologicUnit;
  Index: Integer;
  AllTheSame: Boolean;
begin
  AllTheSame := True;
  for Index := 1 to FSelectedUnits.Count - 1 do
  begin
    OtherHufUnit := FSelectedUnits[Index];
    if HufUnit.VK_Method <> OtherHufUnit.VK_Method then
    begin
      AllTheSame := False;
      break;
    end;
  end;
  if AllTheSame then
  begin
    comboVK_Method.ItemIndex := Ord(HufUnit.VK_Method);
  end
  else
  begin
    comboVK_Method.ItemIndex := -1;
  end;
  rdeVerticalAnisotropy.Enabled := comboVK_Method.ItemIndex <> 0;
end;

function TfrmHUF_Layers.GenerateNewName: string;
var
  UsedNames: TStringList;
  Index: Integer;
  HGU: THydrogeologicUnit;
  NewRoot: string;
begin
  UsedNames := TStringList.Create;
  try
    UsedNames.Sorted := True;
    UsedNames.CaseSensitive := False;
    for Index := 0 to FHydrogeologicUnits.Count - 1 do
    begin
      HGU := FHydrogeologicUnits[Index];
      UsedNames.Add(HGU.HufName);
    end;
    NewRoot := 'HGU';
    result := NewRoot;
    Index := 1;
    repeat
      result := NewRoot + IntToStr(Index);
      if Length(result) > 10 then
      begin
        NewRoot := Copy(NewRoot, 1, Length(NewRoot)-1);
        result := NewRoot + IntToStr(Index);
      end;
      Inc(Index);
    until  UsedNames.IndexOf(result) < 0;
  finally
    UsedNames.Free;
  end;
end;

procedure TfrmHUF_Layers.AssignUseMultiplier(ParamName: string;
  ParamIndex: Integer; HufUnitsThatUseParameter: TList;
  FirstUsedParameter: THufUsedParameter);
var
  OtherUsedParameter: THufUsedParameter;
  AllTheSameMultipliers: Boolean;
  Local_Index: Integer;
  OtherHufUnit: THydrogeologicUnit;
begin
  AllTheSameMultipliers := True;
  for Local_Index := 0 to HufUnitsThatUseParameter.Count - 1 do
  begin
    OtherHufUnit := HufUnitsThatUseParameter[Local_Index];
    OtherUsedParameter := OtherHufUnit.HufUsedParameters.
      GetUsedParameterByName(ParamName);
    if FirstUsedParameter.UseMultiplier <> OtherUsedParameter.UseMultiplier then
    begin
      AllTheSameMultipliers := False;
      break;
    end;
  end;
  if AllTheSameMultipliers then
  begin
    rdgParameters.Checked[Ord(hpgcUseMultiplier), ParamIndex] := FirstUsedParameter.UseMultiplier;
  end
  else
  begin
    rdgParameters.State[Ord(hpgcUseMultiplier), ParamIndex] := cbGrayed;
  end;
end;

procedure TfrmHUF_Layers.AssignUseZone(ParamName: string;
  ParamIndex: Integer; HufUnitsThatUseParameter: TList;
  FirstUsedParameter: THufUsedParameter);
var
  OtherUsedParameter: THufUsedParameter;
  AllTheSameZones: Boolean;
  Local_Index: Integer;
  OtherHufUnit: THydrogeologicUnit;
begin
  AllTheSameZones := True;
  for Local_Index := 0 to HufUnitsThatUseParameter.Count - 1 do
  begin
    OtherHufUnit := HufUnitsThatUseParameter[Local_Index];
    OtherUsedParameter := OtherHufUnit.HufUsedParameters.
      GetUsedParameterByName(ParamName);
    if FirstUsedParameter.UseZone <> OtherUsedParameter.UseZone then
    begin
      AllTheSameZones := False;
      break;
    end;
  end;
  if AllTheSameZones then
  begin
    rdgParameters.Checked[Ord(hpgcUseZone), ParamIndex] := FirstUsedParameter.UseZone;
  end
  else
  begin
    rdgParameters.State[Ord(hpgcUseZone), ParamIndex] := cbGrayed;
  end;
end;

{ TUndoHufLayers }

constructor TUndoHufLayers.Create(
  var NewHydrogeologicUnits: THydrogeologicUnits);
begin
  FNewDataSets := TIDataArrayList.Create;
  // Take ownership of NewHydrogeologicUnits
  FNewHydrogeologicUnits := NewHydrogeologicUnits;
  NewHydrogeologicUnits := nil;

  FOldHydrogeologicUnits := THydrogeologicUnits.Create(nil);
  FOldHydrogeologicUnits.Assign(frmGoPhast.PhastModel.HydrogeologicUnits);
end;

function TUndoHufLayers.Description: string;
begin
  result := StrChangeHydrogeologic;
end;

destructor TUndoHufLayers.Destroy;
begin
  FOldHydrogeologicUnits.Free;
  FNewHydrogeologicUnits.Free;
  FNewDataSets.Free;
  inherited;
end;

procedure TUndoHufLayers.DoCommand;
var
  AModel: TBaseModel;
begin
  inherited;
  AModel := frmGoPhast.PhastModel;
  AssignNewHufUnits(AModel);
  UpdatedRequiredDataSets;
  (AModel as TPhastModel).DataArrayManager.InvalidateHguFormulaDataSets;
end;

procedure TUndoHufLayers.Undo;
var
  AModel: TBaseModel;
begin
  inherited;
  AModel := frmGoPhast.PhastModel;
  AssignOldHufUnits(AModel);
  UpdatedRequiredDataSets;
  (AModel as TPhastModel).DataArrayManager.InvalidateHguFormulaDataSets;
end;

procedure TUndoHufLayers.AssignOldHufUnits(AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  ChildIndex: Integer;
  PhastModel: TPhastModel;
  ChildModel: TChildModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalModel.HydrogeologicUnits.NewDataSets := FNewDataSets;
  LocalModel.HydrogeologicUnits.RemoveNewDataSets;
  LocalModel.HydrogeologicUnits := FOldHydrogeologicUnits;
  if AModel is TPhastModel then
  begin
    PhastModel := TPhastModel(AModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        AssignOldHufUnits(ChildModel);
      end;
    end;
  end;
end;

procedure TUndoHufLayers.AssignNewHufUnits(AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  ChildIndex: Integer;
  PhastModel: TPhastModel;
  ChildModel: TChildModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalModel.HydrogeologicUnits.NewDataSets := FNewDataSets;
  LocalModel.HydrogeologicUnits.ClearNewDataSets;
  LocalModel.HydrogeologicUnits := FNewHydrogeologicUnits;
  if AModel is TPhastModel then
  begin
    PhastModel := TPhastModel(AModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        AssignNewHufUnits(ChildModel);
      end;
    end;
  end;
end;

end.
