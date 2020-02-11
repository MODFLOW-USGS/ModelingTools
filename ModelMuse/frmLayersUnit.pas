unit frmLayersUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, ComCtrls, ExtCtrls, StdCtrls, Buttons, Grids,
  RbwDataGrid4, ArgusDataEntry, GoPhastTypes, LayerStructureUnit, ImgList,
  JvExStdCtrls, JvCombobox, JvListComb, UndoItems, RbwController, RbwEdit,
  RequiredDataSetsUndoUnit, JvCheckBox, Mask, JvExMask, JvSpin,
  frameSubBedsUnit, ModflowSubsidenceDefUnit, frameDiscretizationUnit,
  System.ImageList, GrayTabs, Character;

type
  TDispersionCols = (drLayerNumber, drHorzTransDisp, drVerTransDisp, drDiffCoef);
  TConduitLayerColumns = (clcLayerNumber, clcUsed, clcVoid, clcLowerR,
    clcHigherR);

  TfrmLayers = class(TfrmCustomGoPhast)
    Splitter1: TSplitter;
    pcLayerGroups: TPageControl;
    tabBasics: TTabSheet;
    Label1: TLabel;
    tabDiscretization: TTabSheet;
    Panel4: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Panel3: TPanel;
    GridPanel1: TGridPanel;
    sbAddUnit: TSpeedButton;
    sbInsertUnit: TSpeedButton;
    sbDeleteUnit: TSpeedButton;
    ilCombo: TImageList;
    Label5: TLabel;
    {
     @unorderedlist(
       @item(0, non-simulated)
       @item(1, confined)
       @item(2, convertible in LPF and HUF, Unconfined in BCF)
       @item(3, limited convertible in BCF with constant transmissivity)
       @item(4, fully convertible in BCF with variable transmissivity)
      )
    }
    comboAquiferType: TJvImageComboBox;
    lblInterblockMethod: TLabel;
    lbVertKMethod: TLabel;
    rconLayerType: TRbwController;
    comboInterblockMethod: TJvImageComboBox;
    comboVertKMethod: TJvImageComboBox;
    edName: TRbwEdit;
    cbComputeSaturatedThickness: TJvCheckBox;
    tvLayerGroups: TTreeView;
    ilTreeView: TImageList;
    lblAnisotropy: TLabel;
    rdeAnisotropy: TRbwDataEntry;
    tabNoDelay: TTabSheet;
    frameSubNoDelayBeds: TframeSubBeds;
    tabDelay: TTabSheet;
    frameSubDelayBeds: TframeSubBeds;
    tabSWT: TTabSheet;
    frameSwt: TframeSubBeds;
    tabDispersion: TTabSheet;
    rdgDispersion: TRbwDataGrid4;
    pnlMultiEdit: TPanel;
    rdeMultiDispersionValues: TRbwDataEntry;
    frameDiscretization: TframeDiscretization;
    tabConduitLayers: TTabSheet;
    rdgConduitLayers: TRbwDataGrid4;
    pnl1: TPanel;
    rdeConduitLayers: TRbwDataEntry;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure sbDeleteUnitClick(Sender: TObject);
    procedure sbAddUnitClick(Sender: TObject);
    procedure sbInsertUnitClick(Sender: TObject);
    procedure comboAquiferTypeChange(Sender: TObject);
    procedure comboInterblockMethodChange(Sender: TObject);
    procedure comboVertKMethodChange(Sender: TObject);
    procedure pcLayerGroupsChange(Sender: TObject);
    procedure cbComputeSaturatedThicknessClick(Sender: TObject);
    procedure tvLayerGroupsChange(Sender: TObject; Node: TTreeNode);
    procedure rdeAnisotropyChange(Sender: TObject);
    procedure rdgDispersionSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgDispersionSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgDispersionColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure FormResize(Sender: TObject);
    procedure rdgDispersionHorizontalScroll(Sender: TObject);
    procedure rdgDispersionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeMultiDispersionValuesChange(Sender: TObject);
    procedure rdeConduitLayersChange(Sender: TObject);
    procedure rdgConduitLayersColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgConduitLayersHorizontalScroll(Sender: TObject);
    procedure rdgConduitLayersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgConduitLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgConduitLayersSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgConduitLayersStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure frameDiscretizationrdeVDiscretizationExit(Sender: TObject);
  private
    FLayerStructure: TLayerStructure;
    FSettingUnit: boolean;
    // @name contains the selected @link(TLayerGroup)s.
    FSelectedUnits: TList;
    FSelectedTreeNodes: TList;
    FUseSaturatedThickness: Boolean;
    FEditDiffusion: boolean;
    procedure UpdateDiscretization;
    procedure EnableOkButton;
    procedure GetData;
    Procedure SetData;
    function AddNewUnit(Position: integer): TTreeNode;
    function FindImageIndex(LayerGroup: TLayerGroup): integer;
    procedure UpdateSelectedUnits;
    procedure SetControlValues;
    procedure EnableComputeSatThick;
    procedure EnableK_Methods;
    procedure SetUpLayerTypeOptions;
    procedure SetUpAveragingOptions;
    procedure EnableAnisotropy;
    procedure GetSubsidenceLayers(Sender: TObject;
      var SubLayers: TCustomSubLayer);
    procedure GetNewSubsidenceName(Sender: TObject; var NewName: string);
    // Set the dispersion values
    procedure UpdateDispersionValues(ACol, ARow: Integer);
    procedure LayoutMultiDispersionControl;
    procedure LayoutConduitLayerControl;
    procedure AssignLayerCount(var ATab: TTabSheet; Grid: TRbwDataGrid4;
      LayerColumn: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoDefineLayers = class(TCustomCreateRequiredDataSetsUndo)
  private
    FNewLayerStructure: TLayerStructure;
    FOldLayerStructure: TLayerStructure;
    FNewDataSets: TList;
    FChildDiscretizations: TList;
  protected
    function Description: string; override;
  public
    constructor Create(var NewLayerStructure: TLayerStructure);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

resourcestring
  StrNewLayerGroup = 'New Layer Group';
  StrThisNameIsTooSim = 'This name is too similar to the name of another aqu' +
  'ifer.';

implementation

uses Math, RealListUnit, CursorsFoiledAgain, frmGoPhastUnit,
  ModflowPackagesUnit, frmErrorsAndWarningsUnit, PhastModelUnit, Contnrs;

resourcestring
  StrChangeLayerStructu = 'change layer structure';
  StrLayerBoundary = 'Layer boundary';
  StrHorizontalTransvers = 'Horizontal transverse dispersivity ratio (TRPT)';
  StrVerticalTransverse = 'Vertical transverse dispersivity ratio (TRPV)';
  StrDiffusionCoefficien = 'Diffusion coefficient (DMCOEF)';
  StrHarmonicMean0 = 'Harmonic mean (0)';
  StrArithmeticMean1 = 'Arithmetic mean (1)';
  StrLogarithmicMean2 = 'Logarithmic mean (2)';
  StrArithmeticAndLogar = 'Arithmetic and logarithmic (3)';
  StrLogarithmicMean1 = 'Logarithmic mean (1)';
  StrArithmeticAndLogar2 = 'Arithmetic and logarithmic (2)';
  StrOnlyTheTopLayerC = 'Only the top layer can be unconfined.';
  StrANonsimulatedLaye = 'A non-simulated layer group can not be next to ano' +
  'ther non-simulated layer group.';
  StrTheTopLayerGroup = 'The top layer group must be simulated.';
  StrTheBottomLayerGro = 'The bottom layer group must be simulated.';
  StrUseInAllLayers = 'Use in all layers';
  StrNonsimulated = 'Non-simulated';
  StrConfined = 'Confined';
  StrUnconfined = 'Unconfined';
  StrLimitedConvertible = 'Limited convertible';
  StrFullyConvertible = 'Fully convertible';
  StrConvertible = 'Convertible';
  StrAquiferNamesMustS = 'Aquifer names must start with a letter.';
  StrLayer = 'Layer';
  StrConduitLayerCL = 'Conduit Layer (CL)';
  StrMeanVoidDiameter = 'Mean void diameter (VOID)';
  StrLowerCriticalReyno = 'Lower critical Reynolds number (LCRITREY_L)';
  StrUpperCriticalReyno = 'Upper critical Reynolds number (TCRITREY_L)';

{$R *.dfm}

function TfrmLayers.FindImageIndex(LayerGroup: TLayerGroup): integer;
begin
  if not LayerGroup.Simulated then
  begin
    result := 4;
  end
  else
  begin
    if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected then
    begin
      case LayerGroup.AquiferType of
        0: // confined
          begin
            result := 1;
          end;
        1: // unconfined
          begin
            if LayerGroup.Index = 1 then
            begin
              result := 3;
            end
            else
            begin
              result := 2;
            end;
          end;
        2: // limited convertible
          begin
            result := 5;
          end;
        3: // fully convertible
          begin
            result := 2;
          end;
        else
          begin
            result := -1;
          end;
      end;
    end
    else
    begin
      case LayerGroup.AquiferType of
        0: // confined
          begin
            result := 1;
          end;
        1,2,3: // convertible
          begin
            result := 2;
          end;
        else
          begin
            result := -1;
          end;
      end;
    end;
  end;
end;

procedure TfrmLayers.FormCreate(Sender: TObject);
begin
  inherited;
//  pnlPaintboxParent.DoubleBuffered:= True;
  FSelectedUnits:= TList.Create;
  FSelectedTreeNodes:= TList.Create;
  pcLayerGroups.ActivePageIndex := 0;
  FLayerStructure:= TLayerStructure.Create(nil);
//  rdgSubLayerBoundaries.Cells[0,0] := StrLayerBoundary;

  rdgDispersion.BeginUpdate;
  try
    rdgDispersion.Cells[Ord(drHorzTransDisp),0] := StrHorizontalTransvers;
    rdgDispersion.Cells[Ord(drVerTransDisp),0] := StrVerticalTransverse;
    rdgDispersion.Cells[Ord(drDiffCoef),0] := StrDiffusionCoefficien;
  finally
    rdgDispersion.EndUpdate;
  end;

  rdgConduitLayers.BeginUpdate;
  try
    rdgConduitLayers.Cells[Ord(clcLayerNumber), 0] := StrLayer;
    rdgConduitLayers.Cells[Ord(clcUsed), 0] := StrConduitLayerCL;
    rdgConduitLayers.Cells[Ord(clcVoid), 0] := StrMeanVoidDiameter;
    rdgConduitLayers.Cells[Ord(clcLowerR), 0] := StrLowerCriticalReyno;
    rdgConduitLayers.Cells[Ord(clcHigherR), 0] := StrUpperCriticalReyno;
  finally
    rdgConduitLayers.EndUpdate;
  end;

  frameSubNoDelayBeds.OnGetSelectedSubLayers := GetSubsidenceLayers;
  frameSubDelayBeds.OnGetSelectedSubLayers := GetSubsidenceLayers;
  frameSwt.OnGetSelectedSubLayers := GetSubsidenceLayers;
  frameSubNoDelayBeds.OnGetNewName := GetNewSubsidenceName;
  frameSubDelayBeds.OnGetNewName := GetNewSubsidenceName;
  frameSwt.OnGetNewName := GetNewSubsidenceName;
  GetData;
  EnableComputeSatThick;

end;

procedure TfrmLayers.FormDestroy(Sender: TObject);
begin
  FLayerStructure.Free;
  FSelectedUnits.Free;
  FSelectedTreeNodes.Free;
  inherited;
end;

procedure TfrmLayers.FormResize(Sender: TObject);
begin
  inherited;
  LayoutMultiDispersionControl;
end;

procedure TfrmLayers.frameDiscretizationrdeVDiscretizationExit(Sender: TObject);
begin
  inherited;
  AssignLayerCount(tabDispersion, rdgDispersion, Ord(drLayerNumber));
  AssignLayerCount(tabConduitLayers, rdgConduitLayers, Ord(clcLayerNumber));
end;

procedure TfrmLayers.SetUpAveragingOptions;
var
  Packages: TModflowPackages;
  Item: TJvImageItem;
begin
  Packages := frmGoPhast.PhastModel.ModflowPackages;

  comboInterblockMethod.Items.Clear;

  Item := comboInterblockMethod.Items.Add;
  Item.Text := StrHarmonicMean0;

  if Packages.BcfPackage.IsSelected then
  begin
    Item := comboInterblockMethod.Items.Add;
    Item.Text := StrArithmeticMean1;

    Item := comboInterblockMethod.Items.Add;
    Item.Text := StrLogarithmicMean2;

    Item := comboInterblockMethod.Items.Add;
    Item.Text := StrArithmeticAndLogar;
  end
  else
  begin
    Item := comboInterblockMethod.Items.Add;
    Item.Text := StrLogarithmicMean1;

    Item := comboInterblockMethod.Items.Add;
    Item.Text := StrArithmeticAndLogar2;
  end;
end;

procedure TfrmLayers.EnableAnisotropy;
var

ShouldEnable: Boolean;
  Index: Integer;
  Group: TLayerGroup;
begin
  ShouldEnable :=  (FSelectedUnits.Count >= 1)
    and (frmGoPhast.ModelSelection <> msModflow2015)
    and frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected;
  if ShouldEnable then
  begin
    ShouldEnable := False;
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      Group := FSelectedUnits[Index];
      if Group.Simulated then
      begin
        ShouldEnable := True;
        break;
      end;
    end;
  end;
  rdeAnisotropy.Enabled := ShouldEnable
end;

procedure TfrmLayers.GetData;
var
  Index: integer;
  LayerGroup: TLayerGroup;
  NodeItem: TTreeNode;
begin
  FEditDiffusion := not frmGoPhast.PhastModel.AllDispersionMultiDiffusion;

  tabNoDelay.TabVisible := frmGoPhast.PhastModel.
    ModflowPackages.SubPackage.IsSelected
    and (frmGoPhast.ModelSelection <> msModflow2015);
  tabDelay.TabVisible := frmGoPhast.PhastModel.
    ModflowPackages.SubPackage.IsSelected
    and (frmGoPhast.ModelSelection <> msModflow2015);
  tabSwt.TabVisible := frmGoPhast.PhastModel.
    ModflowPackages.SwtPackage.IsSelected
    and (frmGoPhast.ModelSelection <> msModflow2015);

  SetUpLayerTypeOptions;
  SetUpAveragingOptions;

  FUseSaturatedThickness := frmGoPhast.PhastModel.
    ModflowPackages.LpfPackage.IsSelected
    and frmGoPhast.PhastModel.
    ModflowPackages.LpfPackage.UseSaturatedThickness;


  FLayerStructure.Assign(frmGoPhast.PhastModel.LayerStructure);
  for Index := 1 to FLayerStructure.Count - 1 do
  begin
    LayerGroup := FLayerStructure.Items[Index] as TLayerGroup;
    if not LayerGroup.Simulated and (frmGoPhast.ModelSelection = msModflow2015) then
    begin
      LayerGroup.Simulated := True;
    end;

    NodeItem := tvLayerGroups.Items.Add(nil, LayerGroup.AquiferName);
    NodeItem.Data := LayerGroup;

    NodeItem.StateIndex := FindImageIndex(LayerGroup);

  end;

  if tvLayerGroups.Items.Count > 0 then
  begin
    tvLayerGroups.Items[0].Selected := True;
  end;

  sbDeleteUnit.Enabled := FLayerStructure.Count > 2;

  UpdateSelectedUnits;
  SetControlValues;
end;

procedure TfrmLayers.GetNewSubsidenceName(Sender: TObject;
  var NewName: string);
begin
  if Sender = frameSubNoDelayBeds then
  begin
    NewName := 'ND_SYS_' + IntToStr(FLayerStructure.NoDelayCount);
  end
  else if Sender = frameSubDelayBeds then
  begin
    NewName := 'D_SYS_' + IntToStr(FLayerStructure.DelayCount);
  end
  else if Sender = frameSwt then
  begin
    NewName := 'WT_' + IntToStr(FLayerStructure.WaterTableCount);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TfrmLayers.UpdateDiscretization;
var
  List: TList;
  index: Integer;
  Group: TLayerGroup;
begin
  List := TList.Create;
  try
    List.Capacity := FSelectedUnits.Count;
    for index := 0 to FSelectedUnits.Count - 1 do
    begin
      Group := FSelectedUnits[index];
      List.Add(Group.GrowthControls)
    end;
    frameDiscretization.UpdateSelectedUnits(List);
  finally
    List.Free;
  end;
end;

procedure TfrmLayers.UpdateDispersionValues(ACol, ARow: Integer);
var
  ALayerGroup: TLayerGroup;
  AValue: Double;
  RowIndex: Integer;
  Index: Integer;
  Item: TRealItem;
  Column: TDispersionCols;
  SelectIndex: Integer;
  RealList: TRealList;
  RealCollection: TRealCollection;
begin
  if FSelectedUnits = nil then
  begin
    Exit;
  end;
  if not FSettingUnit and (ARow > 0) then
  begin
    Column := TDispersionCols(ACol);
    if Column in [drHorzTransDisp..drDiffCoef] then
    begin
      RealList := TRealList.Create;
      try
        for RowIndex := 1 to rdgDispersion.RowCount - 1 do
        begin
          if TryStrToFloat(rdgDispersion.Cells[ACol, RowIndex], AValue) then
          begin
            RealList.Add(AValue);
          end;
        end;
        for SelectIndex := 0 to FSelectedUnits.Count - 1 do
        begin
          RealCollection := nil;
          ALayerGroup := FSelectedUnits[SelectIndex];
          case Column of
            drHorzTransDisp:
              begin
                RealCollection := ALayerGroup.Mt3dmsHorzTransDisp;
              end;
            drVerTransDisp:
              begin
                RealCollection := ALayerGroup.Mt3dmsVertTransDisp;
              end;
            drDiffCoef:
              begin
                RealCollection := ALayerGroup.Mt3dmsDiffusionCoef;
              end;
          else
            begin
              Assert(False);
            end;
          end;
          while RealCollection.Count > RealList.Count do
          begin
            RealCollection.Delete(RealCollection.Count - 1);
          end;
          for Index := 0 to RealList.Count - 1 do
          begin
            if RealCollection.Count > Index then
            begin
              Item := RealCollection[Index];
            end
            else
            begin
              Item := RealCollection.Add;
            end;
            Item.Value := RealList[Index];
          end;
        end;
      finally
        RealList.Free;
      end;
    end;
  end;
end;

procedure TfrmLayers.LayoutMultiDispersionControl;
begin
  LayoutControls(rdgDispersion, rdeMultiDispersionValues, nil, rdgDispersion.LeftCol);
end;

procedure TfrmLayers.LayoutConduitLayerControl;
begin
  LayoutControls(rdgConduitLayers, rdeConduitLayers, nil,
    Max(rdgConduitLayers.LeftCol, Ord(clcVoid)));
end;

procedure TfrmLayers.GetSubsidenceLayers(Sender: TObject;
  var SubLayers: TCustomSubLayer);
var
  SelectedUnit: TLayerGroup;
begin
  SubLayers := nil;
  if (not FSettingUnit) and (FSelectedUnits.Count > 0) then
  begin
    Assert(FSelectedUnits.Count = 1);
    SelectedUnit := FSelectedUnits[0];
    if Sender = frameSubNoDelayBeds then
    begin
      SubLayers := SelectedUnit.SubNoDelayBedLayers;
    end
    else if Sender = frameSubDelayBeds then
    begin
      SubLayers := SelectedUnit.SubDelayBedLayers;
    end
    else if Sender = frameSwt then
    begin
      SubLayers := SelectedUnit.WaterTableLayers;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

procedure TfrmLayers.SetData;
var
  Undo: TUndoDefineLayers;
begin
  Assert(Assigned(FLayerStructure));
  Undo := TUndoDefineLayers.Create(FLayerStructure);
  frmGoPhast.UndoStack.Submit(Undo);
end;


procedure TfrmLayers.tvLayerGroupsChange(Sender: TObject; Node: TTreeNode);
begin
  inherited;
  UpdateSelectedUnits;
  SetControlValues;
end;

procedure TfrmLayers.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmLayers.cbComputeSaturatedThicknessClick(Sender: TObject);
var
  Index: Integer;
  SelectedUnit: TLayerGroup;
begin
  inherited;
  if not FSettingUnit then
  begin
    cbComputeSaturatedThickness.AllowGrayed := False;
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      if SelectedUnit.Simulated
        and (SelectedUnit.AquiferType = 1) then
      begin
        SelectedUnit.UseStartingHeadForSaturatedThickness :=
          cbComputeSaturatedThickness.Checked;
      end;
    end;
  end;
end;

procedure TfrmLayers.comboAquiferTypeChange(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TLayerGroup;
  SimulatedLayer: boolean;
  TreeNode: TTreeNode;
  ShowWarning: boolean;
begin
  inherited;
  if frmGoPhast.PhastModel.ModflowPackages.HufPackage.IsSelected
    or (frmGoPhast.ModelSelection = msModflow2015)  then
  begin
    if comboAquiferType.ItemIndex = 0 then
    begin
      comboAquiferType.ItemIndex := 1;
    end;
  end;
  if not FSettingUnit then
  begin
    ShowWarning := False;
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      SelectedUnit.Simulated := comboAquiferType.ItemIndex > 0;
      if SelectedUnit.Simulated then
      begin
        if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected
          and (SelectedUnit.Index <> 1)
          and (comboAquiferType.ItemIndex = 2) then
        begin
          SelectedUnit.AquiferType := 3;
          ShowWarning := True;
          if FSelectedUnits.Count = 1 then
          begin
            comboAquiferType.ItemIndex := 4
          end;
        end
        else
        begin
          SelectedUnit.AquiferType := comboAquiferType.ItemIndex -1;
        end;
      end;
      TreeNode := FSelectedTreeNodes[Index];
      TreeNode.StateIndex := FindImageIndex(SelectedUnit);
    end;
    if ShowWarning then
    begin
      MessageDlg(StrOnlyTheTopLayerC, mtWarning, [mbOK], 0);
    end;
  end;
  SimulatedLayer := comboAquiferType.ItemIndex > 0;
  tabDiscretization.TabVisible := SimulatedLayer;
  EnableK_Methods;
  rconLayerType.Enabled := SimulatedLayer;
  EnableComputeSatThick;
  EnableAnisotropy;
  EnableOkButton;
end;

procedure TfrmLayers.comboInterblockMethodChange(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TLayerGroup;
begin
  inherited;
  if not FSettingUnit then
  begin
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      SelectedUnit.InterblockTransmissivityMethod :=
        comboInterblockMethod.ItemIndex;
    end;
  end;
end;

procedure TfrmLayers.comboVertKMethodChange(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TLayerGroup;
begin
  inherited;
  if not FSettingUnit then
  begin
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      SelectedUnit.VerticalHydraulicConductivityMethod :=
        comboVertKMethod.ItemIndex;
    end;
  end;
end;

procedure TfrmLayers.edNameChange(Sender: TObject);
  function Alpha(C: Char): Boolean; inline;
  begin
//    Result := TCharacter.IsLetter(C) or (C = '_');
    Result := C.IsLetter or (C = '_');
  end;
var
  SelectedUnit: TLayerGroup;
  TreeNode: TTreeNode;
  Index: Integer;
  UsedNames: TStringList;
  ALayerGroup: TLayerGroup;
  TestName: string;
  FirstChar: Char;
  SelStart: Integer;
begin
  inherited;
  if (not FSettingUnit) and (FSelectedUnits.Count > 0) then
  begin
    UsedNames := TStringList.Create;
    try
      UsedNames.CaseSensitive := False;
      for Index := 1 to FLayerStructure.Count - 1 do
      begin
        ALayerGroup := FLayerStructure[Index];
        if FSelectedUnits.IndexOf(ALayerGroup) < 0 then
        begin
          UsedNames.Add(GenerateNewName(ALayerGroup.AquiferName));
        end;
      end;
      if edName.Text <> '' then
      begin
        FirstChar := edName.Text[1];
        if not Alpha(FirstChar) then
        begin
          SelStart := edName.SelStart;
          edName.Text := '_' + edName.Text;
          edName.SelStart := SelStart+1;
        end;
        TestName := GenerateNewName(edName.Text);
        Assert(FSelectedUnits.Count = 1);
        SelectedUnit := FSelectedUnits[0];
        for Index := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
        begin
          ALayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
          if ALayerGroup.ID <> SelectedUnit.ForeignId then
          begin
            UsedNames.Add(GenerateNewName(ALayerGroup.AquiferName));
          end;
        end;
        if UsedNames.IndexOf(TestName) >= 0 then
        begin
          Beep;
          MessageDlg(StrThisNameIsTooSim, mtWarning, [mbOK], 0);
          Exit;
        end;
        SelectedUnit.AquiferName := edName.Text;
      end;
      TreeNode := FSelectedTreeNodes[0];
      TreeNode.Text := edName.Text;
    finally
      UsedNames.Free;
    end;
  end;
end;

procedure TfrmLayers.pcLayerGroupsChange(Sender: TObject);
begin
  inherited;
  btnHelp.HelpKeyword := pcLayerGroups.ActivePage.HelpKeyword;
end;

procedure TfrmLayers.rdeConduitLayersChange(Sender: TObject);
var
  ColIndex: integer;
begin
  inherited;
  for ColIndex := Ord(clcVoid) to Ord(clcHigherR) do
  begin
    ChangeSelectedCellsInColumn(rdgConduitLayers, ColIndex,
      rdeConduitLayers.Text);
  end;
end;

procedure TfrmLayers.rdeAnisotropyChange(Sender: TObject);
var
  Index: Integer;
  SelectedUnit: TLayerGroup;
  AValue: double;
begin
  inherited;
  if (FSelectedUnits <> nil) and not FSettingUnit then
  begin
    if TryStrToFloat(rdeAnisotropy.Text, AValue) then
    begin
      for Index := 0 to FSelectedUnits.Count - 1 do
      begin
        SelectedUnit := FSelectedUnits[Index];
        SelectedUnit.HorizontalAnisotropy := AValue;
      end;
    end;
  end;
end;

procedure TfrmLayers.rdeMultiDispersionValuesChange(Sender: TObject);
var
  ColIndex: Integer;
begin
  inherited;
  for ColIndex := Ord(drHorzTransDisp) to Ord(drDiffCoef) do
  begin
    ChangeSelectedCellsInColumn(rdgDispersion, ColIndex,
      rdeMultiDispersionValues.Text);
  end;
end;

procedure TfrmLayers.EnableOkButton;
var
  Group: TLayerGroup;
  Group1, Group2: TLayerGroup;
  Index: Integer;
  ShouldShowMessage: boolean;
begin
  if FLayerStructure.Count = 0 then Exit;

  ShouldShowMessage := btnOK.Enabled;
  Group := FLayerStructure.Items[FLayerStructure.Count-1] as TLayerGroup;
  btnOK.Enabled := Group.Simulated;
  if btnOK.Enabled then
  begin
    Group := FLayerStructure.Items[1] as TLayerGroup;
    btnOK.Enabled := Group.Simulated;
    if btnOK.Enabled then
    begin
      for Index := 2 to FLayerStructure.Count - 2 do
      begin
        Group1 := FLayerStructure.Items[Index] as TLayerGroup;
        Group2 := FLayerStructure.Items[Index+1] as TLayerGroup;
        btnOK.Enabled := Group1.Simulated or Group2.Simulated;
        if not btnOK.Enabled then
        begin
          break;
          if ShouldShowMessage then
          begin
            Beep;
            MessageDlg(StrANonsimulatedLaye, mtError, [mbOK], 0);
          end;
        end;
      end;
    end
    else
    begin
      if ShouldShowMessage then
      begin
        Beep;
        MessageDlg(StrTheTopLayerGroup,
          mtError, [mbOK], 0);
      end;
    end;
  end
  else
  begin
    if ShouldShowMessage then
    begin
      Beep;
      MessageDlg(StrTheBottomLayerGro,
        mtError, [mbOK], 0);
    end;
  end;
end;

function TfrmLayers.AddNewUnit(Position: integer): TTreeNode;
var
  LayerGroup: TLayerGroup;
  Index: Integer;
  TreeNode: TTreeNode;
  Sibling: TTreeNode;
  LayerNames: TStringList;
  LayerGroupIndex: integer;
  NewLayerName: string;
begin
  for Index := 0 to FSelectedTreeNodes.Count - 1 do
  begin
    TreeNode := FSelectedTreeNodes[Index];
    TreeNode.Selected := False;
  end;
  FSelectedTreeNodes.Clear;
  FSelectedUnits.Clear;
  UpdateDiscretization;
  if FLayerStructure.Count = 0 then
  begin
    LayerGroup := FLayerStructure.Insert(0) as TLayerGroup;
    LayerGroup.AquiferName := kModelTop;
  end;
  LayerGroup := FLayerStructure.Insert(Position+1) as TLayerGroup;
  // LayerGroup.AquiferName can not be assigned in LayerGroup.Create
  // because that causes an error if you delete a layer group
  // and then undo the deletion.
  LayerNames := TStringList.Create;
  try
    for LayerGroupIndex := 0 to FLayerStructure.Count - 1 do
    begin
      LayerNames.Add(FLayerStructure[LayerGroupIndex].AquiferName);
    end;
    NewLayerName := StrNewLayerGroup;
    LayerGroupIndex := 1;
    while LayerNames.IndexOf(NewLayerName) >= 0 do
    begin
      NewLayerName := StrNewLayerGroup + IntToStr(LayerGroupIndex);
      Inc(LayerGroupIndex);
    end;
  finally
    LayerNames.Free;
  end;
  LayerGroup.AquiferName := NewLayerName;

  if Position < tvLayerGroups.Items.Count then
  begin
    Sibling := tvLayerGroups.Items[Position];
  end
  else
  begin
    Sibling := nil;
  end;

  result := tvLayerGroups.Items.Insert(Sibling,
    LayerGroup.AquiferName);
  result.Data := LayerGroup;
  result.Selected := True;
  result.StateIndex := 1;

  sbDeleteUnit.Enabled := True;
  EnableOkButton;
  tvLayerGroups.Invalidate;
end;

procedure TfrmLayers.UpdateSelectedUnits;
var
  Index: Integer;
  NodeItem: TTreeNode;
begin
  if csDestroying in ComponentState then Exit;
  FSelectedUnits.Clear;
  FSelectedTreeNodes.Clear;
  if tvLayerGroups.Selected <> nil then
  begin
    for Index := 0 to tvLayerGroups.Items.Count - 1 do
    begin
      NodeItem := tvLayerGroups.Items[Index];
      if NodeItem.Selected then
      begin
        if NodeItem.Data <> nil then
        begin
          FSelectedUnits.Add(NodeItem.Data);
          FSelectedTreeNodes.Add(NodeItem);
        end;
      end;
    end;
  end;
  UpdateDiscretization;
end;

procedure TfrmLayers.rdgConduitLayersColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutConduitLayerControl;
end;

procedure TfrmLayers.rdgConduitLayersHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutConduitLayerControl;
end;

procedure TfrmLayers.rdgConduitLayersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(rdgConduitLayers, rdeConduitLayers,
    [Ord(clcVoid),Ord(clcLowerR),Ord(clcHigherR)]);
end;

procedure TfrmLayers.rdgConduitLayersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow >= 1) and (ACol in [Ord(clcVoid),Ord(clcLowerR),Ord(clcHigherR)]) then
  begin
    CanSelect := rdgConduitLayers.Checked[Ord(clcUsed), ARow];
  end;
end;

procedure TfrmLayers.rdgConduitLayersSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  FloatValue: double;
  ItemIndex: integer;
  SelectIndex: integer;
  SelectedUnit: TLayerGroup;
  ConduitItem: TConduitLayerItem;
begin
  inherited;
  if FSelectedUnits = nil then
  begin
    Exit;
  end;
  if not FSettingUnit and (ARow > 0)
    and (ACol in [Ord(clcVoid),Ord(clcLowerR),Ord(clcHigherR)])
    and TryStrToFloat(Value, FloatValue) then
  begin
    ItemIndex := ARow-1;
    for SelectIndex := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[SelectIndex];
      while ItemIndex >= SelectedUnit.ConduitLayers.Count do
      begin
        SelectedUnit.ConduitLayers.Add;
      end;
      ConduitItem := SelectedUnit.ConduitLayers[ItemIndex];
      case ACol of
        Ord(clcVoid):
          begin
            ConduitItem.Void := FloatValue;
          end;
        Ord(clcLowerR):
          begin
            ConduitItem.LowerCriticalReynoldsNumber := FloatValue;
          end;
        Ord(clcHigherR):
          begin
            ConduitItem.HigherCriticalReynoldsNumber := FloatValue;
          end;
        else
          Assert(False);
      end;
    end;
  end;

end;

procedure TfrmLayers.rdgConduitLayersStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  ItemIndex: integer;
  SelectIndex: integer;
  SelectedUnit: TLayerGroup;
  ConduitItem: TConduitLayerItem;
begin
  inherited;
  if FSelectedUnits = nil then
  begin
    Exit;
  end;
  if not FSettingUnit and (ARow > 0)
    and (ACol = Ord(clcUsed)) then
  begin
    ItemIndex := ARow-1;
    for SelectIndex := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[SelectIndex];
      while ItemIndex >= SelectedUnit.ConduitLayers.Count do
      begin
        SelectedUnit.ConduitLayers.Add;
      end;
      ConduitItem := SelectedUnit.ConduitLayers[ItemIndex];
      ConduitItem.IsConduitLayer := Value = cbChecked;
    end;
  end;
end;

procedure TfrmLayers.rdgDispersionColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiDispersionControl;
end;

procedure TfrmLayers.rdgDispersionHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiDispersionControl;
end;

procedure TfrmLayers.rdgDispersionMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(rdgDispersion, rdeMultiDispersionValues,
    [Ord(drHorzTransDisp),Ord(drVerTransDisp),Ord(drDiffCoef)]);
end;

procedure TfrmLayers.rdgDispersionSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow  >= 1) and (ACol = Ord(drDiffCoef)) then
  begin
    CanSelect := FEditDiffusion
  end;
end;

procedure TfrmLayers.rdgDispersionSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  UpdateDispersionValues(ACol, ARow);
end;

procedure TfrmLayers.sbAddUnitClick(Sender: TObject);
begin
  inherited;
  AddNewUnit(tvLayerGroups.Items.Count);
end;

procedure TfrmLayers.sbDeleteUnitClick(Sender: TObject);
var
  Index: Integer;
  Item: TTreeNode;
  NewIndex: Integer;
begin
  inherited;
  NewIndex := 0;
  for Index := tvLayerGroups.Items.Count - 1 downto 0 do
  begin
    Item := tvLayerGroups.Items[Index];
    if Item.Selected then
    begin
      FLayerStructure.Delete(Index+1);
      NewIndex := Index-1;
      Item.Selected := False;
      tvLayerGroups.Items.Delete(Item);
    end;
  end;
  if NewIndex < 0 then
  begin
    NewIndex := tvLayerGroups.Items.Count - 1;
  end;
  tvLayerGroups.Items[NewIndex].Selected := True;
  sbDeleteUnit.Enabled := FLayerStructure.Count > 2;
  EnableOkButton;
end;

procedure TfrmLayers.sbInsertUnitClick(Sender: TObject);
var
  SelectIndex: integer;
begin
  inherited;
  if tvLayerGroups.Selected <> nil then
  begin
    SelectIndex := tvLayerGroups.Selected.Index;
    AddNewUnit(SelectIndex);
  end
  else
  begin
    sbAddUnitClick(nil);
  end;
end;

procedure TfrmLayers.AssignLayerCount(var ATab: TTabSheet; Grid: TRbwDataGrid4;
  LayerColumn: Integer);
var
  SelectIndex: Integer;
  RowCount: Integer;
  RowIndex: Integer;
  SelectedUnit: TLayerGroup;
begin
  RowCount := -1;
  for SelectIndex := 0 to FSelectedUnits.Count - 1 do
  begin
    SelectedUnit := FSelectedUnits[SelectIndex];
    if SelectedUnit.Simulated then
    begin
      RowCount := Max(SelectedUnit.LayerCount, RowCount);
    end;
  end;
  if RowCount < 0 then
  begin
    ATab.Visible := False;
    Exit;
  end;
  ATab.Visible := True;
  Inc(RowCount);
  Grid.RowCount := RowCount;
  for RowIndex := 1 to Grid.RowCount - 1 do
  begin
    Grid.Cells[LayerColumn, RowIndex] := IntToStr(RowIndex);
  end;
end;

procedure TfrmLayers.SetControlValues;
var
  SelectedUnit: TLayerGroup;
  FirstUnit: TLayerGroup;
  Same: boolean;
  procedure InitializeSubsidenceGrid(Frame: TframeSubBeds);
  var
    ColIndex: Integer;
  begin
    Frame.rdgSubBed.BeginUpdate;
    try
      if FSelectedUnits.Count = 1 then
      begin
        if FirstUnit.LayerCollection.Count = 0 then
        begin
          Frame.rdgSubBed.ColCount := 1;
        end
        else
        begin
          Frame.rdgSubBed.ColCount := 2
            + FirstUnit.LayerCollection.Count+1;
          Frame.rdgSubBed.Cells[1,0] := StrUseInAllLayers;
          Frame.rdgSubBed.Columns[1].AutoAdjustRowHeights := True;
          Frame.rdgSubBed.Columns[1].WordWrapCaptions := True;
          for ColIndex := Ord(scUseAll) to
            Frame.rdgSubBed.ColCount - 1 do
          begin
            Frame.rdgSubBed.Columns[ColIndex].Format := rcf4Boolean;
          end;
          for ColIndex := Ord(fcFirst) to
            Frame.rdgSubBed.ColCount - 1 do
          begin
            Frame.rdgSubBed.ColWidths[ColIndex] := 10;
            Frame.rdgSubBed.Columns[ColIndex].AutoAdjustColWidths := True;
            Frame.rdgSubBed.Cells[ColIndex,0] := IntToStr(ColIndex-1);
          end;
        end;
        Frame.rdgSubBed.Cells[0,0] := 'Name'
      end;
    finally
      Frame.rdgSubBed.EndUpdate;
    end;
  end;
  procedure AssignAquiferType;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.Simulated = SelectedUnit.Simulated;
      if not Same then
      begin
        break;
      end;
      if FirstUnit.Simulated then
      begin
        Same := FirstUnit.AquiferType = SelectedUnit.AquiferType;
        if not Same then
        begin
          break;
        end;
      end;
    end;
    if Same then
    begin
      if not FirstUnit.Simulated then
      begin
        comboAquiferType.ItemIndex := 0
      end
      else
      begin
        comboAquiferType.ItemIndex := FirstUnit.AquiferType + 1;
      end;
      comboAquiferTypeChange(comboAquiferType);
      tabDiscretization.TabVisible := comboAquiferType.ItemIndex > 0;
    end
    else
    begin
      comboAquiferType.ItemIndex := -1;
      tabDiscretization.TabVisible := FirstUnit.Simulated;
      if tabDiscretization.TabVisible then
      begin
        for Index := 1 to FSelectedUnits.Count - 1 do
        begin
          SelectedUnit := FSelectedUnits[Index];
          tabDiscretization.TabVisible := SelectedUnit.Simulated;
          if not tabDiscretization.TabVisible then
          begin
            break;
          end;
        end;
      end;
    end;
  end;
  procedure AssignTransmissivityMethod;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.InterblockTransmissivityMethod
        = SelectedUnit.InterblockTransmissivityMethod;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      comboInterblockMethod.ItemIndex :=
        FirstUnit.InterblockTransmissivityMethod;
    end
    else
    begin
      comboInterblockMethod.ItemIndex := -1;
    end;
  end;
  procedure AssignVerticalKMethod;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.VerticalHydraulicConductivityMethod
        = SelectedUnit.VerticalHydraulicConductivityMethod;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      comboVertKMethod.ItemIndex :=
        FirstUnit.VerticalHydraulicConductivityMethod;
    end
    else
    begin
      comboVertKMethod.ItemIndex := -1;
    end;
  end;
  procedure AssignComputeSaturatedThickness;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.UseStartingHeadForSaturatedThickness
        = SelectedUnit.UseStartingHeadForSaturatedThickness;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      cbComputeSaturatedThickness.Checked :=
        FirstUnit.UseStartingHeadForSaturatedThickness;
    end
    else
    begin
      cbComputeSaturatedThickness.AllowGrayed := True;
      cbComputeSaturatedThickness.State := cbGrayed;
    end;
  end;
  procedure AssignHorizontalAnisotropy;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.HorizontalAnisotropy
        = SelectedUnit.HorizontalAnisotropy;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      rdeAnisotropy.Text := FloatToStr(
        FirstUnit.HorizontalAnisotropy);
    end
    else
    begin
      rdeAnisotropy.Text := '';
    end;
  end;
  procedure AssignSubFrame(SubLayers: TCustomSubLayer; Frame: TframeSubBeds);
  var
    Index: Integer;
    ColIndex: Integer;
    ItemIndex: Integer;
    UseItem: TUseLayerNumberItem;
    AnItem: TCustomSubLayerItem;
  begin
    Frame.seCount.AsInteger := SubLayers.Count;
    Frame.seCountChange(nil);
    for Index := 0 to SubLayers.Count - 1 do
    begin
      AnItem := SubLayers.Items[Index] as TCustomSubLayerItem;
      Frame.rdgSubBed.Cells[Ord(scName), Index + 1] := AnItem.Name;
      if Frame.rdgSubBed.ColCount > 1 then
      begin
        Frame.rdgSubBed.Checked[Ord(scUseAll), Index + 1] :=
          AnItem.UseInAllLayers;
        if AnItem.UseInAllLayers then
        begin
          for ColIndex := Ord(fcFirst) to Frame.rdgSubBed.ColCount - 1 do
          begin
            Frame.rdgSubBed.Checked[ColIndex, Index + 1] := True;
          end;
        end
        else
        begin
          for ColIndex := Ord(fcFirst) to Frame.rdgSubBed.ColCount - 1 do
          begin
            Frame.rdgSubBed.Checked[ColIndex, Index + 1] := False;
          end;
          for ItemIndex := 0 to AnItem.UsedLayers.Count - 1 do
          begin
            UseItem := AnItem.UsedLayers[ItemIndex];
            ColIndex := UseItem.LayerNumber + 1;
            if ColIndex < Frame.rdgSubBed.ColCount then
            begin
              Frame.rdgSubBed.Checked[ColIndex, Index + 1] := True;
            end;
          end;
        end;
      end;
    end;
  end;
  procedure AssignNoDelayBeds;
  begin
    if FSelectedUnits.Count > 1 then
    begin
      tabNoDelay.TabVisible := False;
      Exit;
    end;
    tabNoDelay.TabVisible := frmGoPhast.PhastModel.
      ModflowPackages.SubPackage.IsSelected
      and (frmGoPhast.ModelSelection <> msModflow2015);
    InitializeSubsidenceGrid(frameSubNoDelayBeds);
    AssignSubFrame(FirstUnit.SubNoDelayBedLayers, frameSubNoDelayBeds);
  end;
  procedure AssignDelayBeds;
  begin
    if FSelectedUnits.Count > 1 then
    begin
      tabDelay.TabVisible := False;
      Exit;
    end;
    tabDelay.TabVisible := frmGoPhast.PhastModel.
      ModflowPackages.SubPackage.IsSelected
      and (frmGoPhast.ModelSelection <> msModflow2015);
    InitializeSubsidenceGrid(frameSubDelayBeds);
    AssignSubFrame(FirstUnit.SubDelayBedLayers, frameSubDelayBeds);
  end;
  procedure AssignSwtBeds;
  begin
    if FSelectedUnits.Count > 1 then
    begin
      tabSWT.TabVisible := False;
      Exit;
    end;
    tabSWT.TabVisible := frmGoPhast.PhastModel.
      ModflowPackages.SwtPackage.IsSelected
      and (frmGoPhast.ModelSelection <> msModflow2015);
    InitializeSubsidenceGrid(frameSwt);
    AssignSubFrame(FirstUnit.WaterTableLayers, frameSwt);
  end;
  procedure AssignDispersion;
  var
    SelectIndex: Integer;
    DisplayDispersion: Boolean;
    DispersionSame: Boolean;
    FirstSimUnit: TLayerGroup;
    RowIndex: integer;
    ColIndex: TDispersionCols;
    RealCollection: TRealCollection;
  begin
    DisplayDispersion := frmGoPhast.PhastModel.DispersionSelected;
    if not DisplayDispersion then
    begin
      tabDispersion.TabVisible := False;
      Exit;
    end;
    rdgDispersion.BeginUpdate;
    try
      AssignLayerCount(tabDispersion, rdgDispersion, Ord(drLayerNumber));
      if not tabDispersion.Visible then
      begin
        Exit;
      end;

      DispersionSame := True;
      FirstSimUnit := nil;
      for SelectIndex := 0 to FSelectedUnits.Count - 1 do
      begin
        SelectedUnit := FSelectedUnits[SelectIndex];
        if SelectedUnit.Simulated then
        begin
          FirstSimUnit := SelectedUnit;
          break;
        end;
      end;
      if FirstSimUnit.Mt3dmsHorzTransDisp.Count = 0 then
      begin
        FirstSimUnit.Mt3dmsHorzTransDisp.Add;
      end;
      if FirstSimUnit.Mt3dmsVertTransDisp.Count = 0 then
      begin
        FirstSimUnit.Mt3dmsVertTransDisp.Add;
      end;
      if FEditDiffusion and (FirstSimUnit.Mt3dmsDiffusionCoef.Count = 0) then
      begin
        FirstSimUnit.Mt3dmsDiffusionCoef.Add;
      end;
      for SelectIndex := 1 to FSelectedUnits.Count - 1 do
      begin
        SelectedUnit := FSelectedUnits[SelectIndex];
        if SelectedUnit.Simulated and (SelectedUnit <> FirstSimUnit) then
        begin
          if SelectedUnit.Mt3dmsHorzTransDisp.Count = 0 then
          begin
            SelectedUnit.Mt3dmsHorzTransDisp.Add;
          end;
          if SelectedUnit.Mt3dmsVertTransDisp.Count = 0 then
          begin
            SelectedUnit.Mt3dmsVertTransDisp.Add;
          end;
          if FEditDiffusion
            and (SelectedUnit.Mt3dmsDiffusionCoef.Count = 0) then
          begin
            SelectedUnit.Mt3dmsDiffusionCoef.Add;
          end;
          DispersionSame :=
            FirstSimUnit.Mt3dmsHorzTransDisp.IsSame(
              SelectedUnit.Mt3dmsHorzTransDisp)
            and FirstSimUnit.Mt3dmsVertTransDisp.IsSame(
              SelectedUnit.Mt3dmsVertTransDisp);
          if DispersionSame and FEditDiffusion then
          begin
            DispersionSame := FirstSimUnit.Mt3dmsDiffusionCoef.IsSame(
              SelectedUnit.Mt3dmsDiffusionCoef);
          end;
          if not DispersionSame then
          begin
            break;
          end;
        end;
      end;
      if DispersionSame then
      begin
        for RowIndex := 1 to rdgDispersion.RowCount - 1 do
        begin
          for ColIndex := drHorzTransDisp to drDiffCoef do
          begin
            RealCollection := nil;
            case ColIndex of
              drHorzTransDisp:
                begin
                  RealCollection := FirstSimUnit.Mt3dmsHorzTransDisp
                end;
              drVerTransDisp:
                begin
                  RealCollection := FirstSimUnit.Mt3dmsVertTransDisp
                end;
              drDiffCoef:
                begin
                  if not FEditDiffusion then
                  begin
                    rdgDispersion.Cells[Ord(ColIndex), RowIndex] := '';
                    Continue;
                  end;
                  RealCollection := FirstSimUnit.Mt3dmsDiffusionCoef
                end
              else
                begin
                  Assert(False);
                end;
            end;
            if RealCollection.Count > RowIndex-1 then
            begin
              rdgDispersion.Cells[Ord(ColIndex), RowIndex] :=
                FloatToStr(RealCollection[RowIndex-1].Value)
            end
            else
            begin
              rdgDispersion.Cells[Ord(ColIndex), RowIndex] := '';
            end;
          end;
        end;
      end
      else
      begin
        for RowIndex := 1 to rdgDispersion.RowCount - 1 do
        begin
          for ColIndex := drHorzTransDisp to drDiffCoef do
          begin
            rdgDispersion.Cells[Ord(ColIndex), RowIndex] := '';
          end;
        end;
      end;
    finally
      rdgDispersion.EndUpdate
    end;
  end;
  procedure AssignConduitLayers;
  var
    DisplayConduitLayers: Boolean;
    ConduitsSame: Boolean;
    SelectedUnit: TLayerGroup;
    FirstConduit: TConduitLayerCollection;
    SelectIndex: Integer;
    RowIndex: Integer;
    ColIndex: TConduitLayerColumns;
    AnItem: TConduitLayerItem;
  begin
    DisplayConduitLayers :=
      (frmGoPhast.PhastModel.ModelSelection = msModflowCfp)
      and frmGoPhast.PhastModel.ModflowPackages.ConduitFlowProcess.IsSelected
      and frmGoPhast.PhastModel.ModflowPackages.ConduitFlowProcess.ConduitLayersUsed;
    tabConduitLayers.TabVisible := DisplayConduitLayers;
    if not DisplayConduitLayers then
    begin
      Exit;
    end;
    rdgConduitLayers.BeginUpdate;
    try
      AssignLayerCount(tabConduitLayers, rdgConduitLayers, Ord(clcLayerNumber));
      if not tabConduitLayers.Visible then
      begin
        Exit;
      end;

      ConduitsSame := true;
      SelectedUnit := FSelectedUnits[0];
      FirstConduit := SelectedUnit.ConduitLayers;

      for SelectIndex := 1 to FSelectedUnits.Count - 1 do
      begin
        SelectedUnit := FSelectedUnits[SelectIndex];
        if not FirstConduit.IsSame(SelectedUnit.ConduitLayers) then
        begin
          ConduitsSame := False;
          break;
        end;
      end;

      if ConduitsSame then
      begin
        for RowIndex := 1 to rdgConduitLayers.RowCount - 1 do
        begin
          if (RowIndex-1) < FirstConduit.Count then
          begin
            AnItem := FirstConduit[RowIndex-1];
            rdgConduitLayers.Checked[Ord(clcUsed), RowIndex] :=
              AnItem.IsConduitLayer;
            rdgConduitLayers.Cells[Ord(clcVoid), RowIndex] :=
              FloatToStr(AnItem.Void);
            rdgConduitLayers.Cells[Ord(clcLowerR), RowIndex] :=
              FloatToStr(AnItem.LowerCriticalReynoldsNumber);
            rdgConduitLayers.Cells[Ord(clcHigherR), RowIndex] :=
              FloatToStr(AnItem.HigherCriticalReynoldsNumber);
          end
          else
          begin
            rdgConduitLayers.Checked[Ord(clcUsed), RowIndex] := False;
            for ColIndex := clcVoid to clcLowerR do
            begin
              rdgConduitLayers.Cells[Ord(ColIndex), RowIndex] := '';
            end;
          end;
        end;
      end
      else
      begin
        for RowIndex := 1 to rdgConduitLayers.RowCount - 1 do
        begin
          rdgConduitLayers.Checked[Ord(clcUsed), RowIndex] := False;
          for ColIndex := clcVoid to clcLowerR do
          begin
            rdgConduitLayers.Cells[Ord(ColIndex), RowIndex] := '';
          end;
        end;
      end;

    finally
      rdgConduitLayers.EndUpdate;
    end;
  end;
begin
  if csDestroying in ComponentState then Exit;

  FSettingUnit := True;
  edName.Enabled := FSelectedUnits.Count = 1;
  comboAquiferType.Enabled := (FSelectedUnits.Count >= 1)
    and (frmGoPhast.ModelSelection <> msModflow2015);
  EnableK_Methods;
  EnableComputeSatThick;
  EnableAnisotropy;

  if FSelectedUnits.Count = 0 then
  begin
    Exit;
  end;
  FirstUnit := FSelectedUnits[0];

  try
    if FSelectedUnits.Count = 1 then
    begin
      edName.Text := FirstUnit.AquiferName;
    end
    else
    begin
      edName.Text := '';
    end;
    FirstUnit := FSelectedUnits[0];
    UpdateDiscretization;
    frameDiscretization.SetControlValues;
    AssignAquiferType;
    AssignTransmissivityMethod;
    AssignVerticalKMethod;
    AssignComputeSaturatedThickness;
    AssignHorizontalAnisotropy;
    AssignNoDelayBeds;
    AssignDelayBeds;
    AssignSwtBeds;
    AssignDispersion;
    AssignConduitLayers;
  finally
    FSettingUnit := False;
  end;

end;

procedure TfrmLayers.EnableComputeSatThick;
var
  UnConfinedLayer: Boolean;
  Index: Integer;
  SelectedUnit: TLayerGroup;
begin
  UnConfinedLayer := False;
  if FUseSaturatedThickness and
    (FSelectedUnits.Count >= 1) then
  begin
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      if (SelectedUnit.AquiferType = 1) then
      begin
        UnConfinedLayer := True;
        break;
      end;
    end;
  end;
  cbComputeSaturatedThickness.Enabled := UnConfinedLayer
    and (frmGoPhast.ModelSelection <> msModflow2015)
    and (frmGoPhast.PhastModel.ModflowPackages.LpfPackage.IsSelected
    or frmGoPhast.PhastModel.ModflowPackages.UpwPackage.IsSelected);
end;

procedure TfrmLayers.EnableK_Methods;
var
  SimulatedLayer: Boolean;
  ShouldEnable: Boolean;
begin
  if (FSelectedUnits.Count >= 1) then
  begin
    SimulatedLayer := comboAquiferType.ItemIndex > 0;
    ShouldEnable := SimulatedLayer
      and (frmGoPhast.ModelSelection <> msModflow2015)
      and (frmGoPhast.PhastModel.ModflowPackages.LpfPackage.IsSelected
      or frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected
      or frmGoPhast.PhastModel.ModflowPackages.UpwPackage.IsSelected);
    comboInterblockMethod.Enabled := ShouldEnable;
    comboVertKMethod.Enabled := ShouldEnable;
  end
  else
  begin
    comboInterblockMethod.Enabled := False;
    comboVertKMethod.Enabled := False;
  end;
end;

procedure TfrmLayers.SetUpLayerTypeOptions;
var
  Packages: TModflowPackages;
  Item: TJvImageItem;
begin
  comboAquiferType.Items.Clear;
  Item := comboAquiferType.Items.Add;
  Item.Text := StrNonsimulated;
  Item.ImageIndex := 3;

  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    Item.Brush.Color := clBtnFace;
  end;

  Item := comboAquiferType.Items.Add;
  Item.Text := StrConfined;
  Item.ImageIndex := 0;

  Packages := frmGoPhast.PhastModel.ModflowPackages;
  if Packages.BcfPackage.isSelected then
  begin
    Item := comboAquiferType.Items.Add;
    Item.Text := StrUnconfined;
    Item.ImageIndex := 2;

    Item := comboAquiferType.Items.Add;
    Item.Text := StrLimitedConvertible;
    Item.ImageIndex := 4;

    Item := comboAquiferType.Items.Add;
    Item.Text := StrFullyConvertible;
    Item.ImageIndex := 1;
  end
  else if Packages.HufPackage.isSelected then
  begin
    Item := comboAquiferType.Items.Add;
    Item.Text := StrConvertible;
    Item.ImageIndex := 1;
  end
  else if Packages.LpfPackage.isSelected
    or Packages.UpwPackage.isSelected
    or Packages.NpfPackage.isSelected then
  begin
    Item := comboAquiferType.Items.Add;
    Item.Text := StrConvertible;
    Item.ImageIndex := 1;
  end
  else
  begin
    Assert(False);
  end;


  if Packages.HufPackage.IsSelected then
  begin
    comboAquiferType.Items[0].Brush.Color := clBtnFace;
  end
  else
  begin
    comboAquiferType.Items[0].Brush.Color := clWhite;
  end;
end;

{ TUndoDefineLayers }

constructor TUndoDefineLayers.Create(var NewLayerStructure: TLayerStructure);
var
  Index: Integer;
  ChildModel: TChildModel;
  NewDis: TChildDiscretizationCollection;
  NewIndex: Integer;
  NewLayerGroup: TLayerGroup;
  OldIndex: Integer;
  OldLayerGroup: TLayerGroup;
  procedure FixAquiferName(ANewLayerGroup: TLayerGroup);
var
  UsedNames: TStringList;
  Index: Integer;
  ALayerGroup: TLayerGroup;
  begin
    UsedNames := TStringList.Create;
    try
      UsedNames.CaseSensitive := False;
      for Index := 0 to FNewLayerStructure.Count - 1 do
      begin
        ALayerGroup := FNewLayerStructure[Index];
        if ALayerGroup <> ANewLayerGroup then
        begin
          UsedNames.Add(ALayerGroup.AquiferName)
        end;
      end;
      for Index := 0 to FOldLayerStructure.Count - 1 do
      begin
        ALayerGroup := FOldLayerStructure[Index];
        UsedNames.Add(ALayerGroup.AquiferName)
      end;
      Index := 1;
      while UsedNames.IndexOf(ANewLayerGroup.AquiferName + IntToStr(Index)) >= 0 do
      begin
        Inc(Index);
      end;
      ANewLayerGroup.AquiferName := ANewLayerGroup.AquiferName + IntToStr(Index);
    finally
      UsedNames.Free;
    end;
  end;
begin
  inherited Create;
  Assert(Assigned(NewLayerStructure));
  FChildDiscretizations := TObjectList.Create;
  for Index := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[Index].ChildModel;
    NewDis := TChildDiscretizationCollection.Create(nil);
    NewDis.Assign(ChildModel.Discretization);
    FChildDiscretizations.Add(NewDis);
  end;
  FNewDataSets := TList.Create;

  FNewLayerStructure:= NewLayerStructure;
  // TUndoDefineLayers takes ownership of NewLayerStructure.
  NewLayerStructure := nil;
  FOldLayerStructure:= TLayerStructure.Create(nil);
  FOldLayerStructure.Assign(frmGoPhast.PhastModel.LayerStructure);

  for NewIndex := 0 to FNewLayerStructure.Count - 1 do
  begin
    NewLayerGroup := FNewLayerStructure[NewIndex];
    for OldIndex := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      OldLayerGroup := frmGoPhast.PhastModel.LayerStructure[OldIndex];
      if AnsiSameText(NewLayerGroup.AquiferName, OldLayerGroup.AquiferName)
        and (OldLayerGroup.ID <> NewLayerGroup.ForeignId) then
      begin
        FixAquiferName(NewLayerGroup);
      end;
    end;
  end;
end;

function TUndoDefineLayers.Description: string;
begin
  result := StrChangeLayerStructu;
end;

destructor TUndoDefineLayers.Destroy;
begin
  FNewLayerStructure.Free;
  FOldLayerStructure.Free;
  FNewDataSets.Free;
  FChildDiscretizations.Free;
  inherited;
end;

procedure TUndoDefineLayers.DoCommand;
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LayerCountChanged: Boolean;
begin
  frmGoPhast.CanDraw := False;
  try
    inherited;
    LocalModel := frmGoPhast.PhastModel;
    LayerCountChanged := LocalModel.LayerStructure.LayerCount <> FNewLayerStructure.LayerCount;
    if LayerCountChanged then
    begin
      LocalModel.ThreeDDataSet := nil;
    end;
    LocalModel.LayerStructure.NewDataSets := FNewDataSets;
    LocalModel.LayerStructure.ClearNewDataSets;
    LocalModel.LayerStructure.Assign(FNewLayerStructure);
    LocalModel.LayerStructure.NewDataSets := nil;
    UpdatedRequiredDataSets;
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.UpdateGrid;
    end;
    frmGoPhast.PhastModel.ModflowGrid.NotifyGridChanged(nil);
    frmGoPhast.PhastModel.UpdateMapping;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.DataArrayManager.InvalidateAllDataSets;
    end;
    frmGoPhast.UpdateModelCubeBreaks;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoDefineLayers.Undo;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  NewDis: TChildDiscretizationCollection;
  LayerCountChanged: Boolean;
begin
  frmGoPhast.CanDraw := False;
  try
    inherited;
    frmGoPhast.PhastModel.LayerStructure.NewDataSets := FNewDataSets;
    LayerCountChanged := frmGoPhast.PhastModel.LayerStructure.LayerCount <> FNewLayerStructure.LayerCount;
    if LayerCountChanged then
    begin
      frmGoPhast.PhastModel.ThreeDDataSet := nil;
    end;
    frmGoPhast.PhastModel.LayerStructure.Assign(FOldLayerStructure);
    frmGoPhast.PhastModel.LayerStructure.RemoveNewDataSets;
    frmGoPhast.PhastModel.LayerStructure.NewDataSets := nil;
    UpdatedRequiredDataSets;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      NewDis := FChildDiscretizations[ChildIndex];
      ChildModel.Discretization.Assign(NewDis);
      ChildModel.UpdateGrid;
    end;
    frmGoPhast.PhastModel.ModflowGrid.NotifyGridChanged(nil);
    frmGoPhast.PhastModel.UpdateMapping;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.DataArrayManager.InvalidateAllDataSets;
    end;
    frmGoPhast.UpdateModelCubeBreaks;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

end.
