unit frameMt3dBasicPkgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ExtCtrls, ArgusDataEntry,
  ModflowPackageSelectionUnit, frameGridUnit, Mt3dmsChemSpeciesUnit, Mask,
  JvgListBox, JvExMask, JvSpin, Vcl.ComCtrls;

type
  TSpeciesColumn = (scName, scUseFile, scFileName);

  TframeMt3dBasicPkg = class(TframePackage)
    edMassUnit: TLabeledEdit;
    rdeInactiveConcentration: TRbwDataEntry;
    lblInactiveConcentration: TLabel;
    rdeMinimumSaturatedFraction: TRbwDataEntry;
    lblMinimumSaturatedFraction: TLabel;
    pnlSpecies: TPanel;
    Splitter1: TSplitter;
    frameGridImmobile: TframeGrid;
    frameGridMobile: TframeGrid;
    dlgOpenSelectFile: TOpenDialog;
    grpInitialConcentrationTimes: TGroupBox;
    lblStressPeriod: TLabel;
    seStressPeriod: TJvSpinEdit;
    seTimeStep: TJvSpinEdit;
    lblTimeStep: TLabel;
    lblTransportStep: TLabel;
    seTransportStep: TJvSpinEdit;
    comboVersion: TComboBox;
    lblVersion: TLabel;
    comboInitialConcentrationChoice: TComboBox;
    lblInitialConcentrationChoice: TLabel;
    pcMt3d_Basic: TPageControl;
    tabMT3D_Options: TTabSheet;
    tabSpecies: TTabSheet;
    tabMT3D_USGS_Options: TTabSheet;
    chklstOptions: TJvgCheckListBox;
    procedure frameGridSpeciesGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameGridMobileGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameGridImmobileGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameSpeciesGridStateChange(Sender: TObject; ACol,
      ARow: Integer; const Value: TCheckBoxState);
    procedure comboInitialConcentrationChoiceChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure comboVersionChange(Sender: TObject);
  private
    FOnEnableTimeControls: TNotifyEvent;
    procedure GetMt3dComponents(Mt3DComponents: TCustomChemSpeciesCollection;
      AFrame: TframeGrid);
    procedure SetMt3dComponents(Mt3DComponents: TCustomChemSpeciesCollection;
      AFrame: TframeGrid);
    procedure FixNames(Names: TStringList; AFrame: TframeGrid);
    procedure EnableTimeControls;
    procedure Enable_chklstOptions;
//    function Mt3dTimeControlsShouldBeEnabled: Boolean;
    procedure InitializeSpeciesGrids;
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    procedure GetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection;
      ImmobileComponents: TChemSpeciesCollection);
    procedure SetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection;
      ImmobileComponents: TChemSpeciesCollection);
    procedure SetTimeControlsEnabled(ShouldEnable: Boolean);
    property OnEnableTimeControls: TNotifyEvent read FOnEnableTimeControls
      write FOnEnableTimeControls;
    { Public declarations }
  end;

var
  frameMt3dBasicPkg: TframeMt3dBasicPkg;

implementation

uses
  PhastModelUnit, Grids, frmGoPhastUnit, GoPhastTypes;

{$R *.dfm}

resourcestring
  StrMobileSpecies = 'Mobile Species';
  StrImmobileSpecies = 'Immobile Species';
  StrUseInitialConcentr = 'Use Initial Concentration File';
  StrFileName = 'File Name';

{ TframeMt3dBasicPkg }

procedure TframeMt3dBasicPkg.frameGridImmobileGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
//  if (ACol = Ord(scFileName))
//    and not frameGridImmobile.Grid.Checked[Ord(scUseFile), ARow] then
//  begin
//    CanSelect := False;
//  end;
end;

procedure TframeMt3dBasicPkg.frameGridMobileGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
//  if (ACol = Ord(scFileName))
//    and not frameGridMobile.Grid.Checked[Ord(scUseFile), ARow] then
//  begin
//    CanSelect := False;
//  end;
end;

procedure TframeMt3dBasicPkg.frameSpeciesGridStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
//  EnableTimeControls;
end;

procedure TframeMt3dBasicPkg.frameGridSpeciesGridButtonClick(Sender: TObject;
  ACol, ARow: Integer);
var
  Grid: TStringGrid;
begin
  inherited;
//  Grid := Sender as TStringGrid;
//  dlgOpenSelectFile.FileName := Grid.Cells[ACol, ARow];
//  if dlgOpenSelectFile.Execute then
//  begin
//    Grid.Cells[ACol, ARow] := dlgOpenSelectFile.FileName;
//  end;
end;

procedure TframeMt3dBasicPkg.GetData(Package: TModflowPackageSelection);
var
  BasicPackage: TMt3dBasic;
  RowIndex: Integer;
  Option: TMt3dUsgsOption;
begin
//  {$IFDEF GWT}
//  if frmGoPhast.ModelSelection = msModflow2015 then
//  begin
//    if comboVersion.Items.Count = 2 then
//    begin
//      comboVersion.Items.Add('GWT');
//    end;
//  end
//  else
//  begin
//    if comboVersion.Items.Count = 3 then
//    begin
//      comboVersion.Items.Delete(2);
//    end;
//  end;
//  {$ENDIF}
  pcMt3d_Basic.ActivePageIndex := 0;
{  for RowIndex := 1 to frameGridMobile.Grid.RowCount - 1 do
  begin
    frameGridMobile.Grid.Objects[Ord(scName),RowIndex] := nil;
  end;
  for RowIndex := 1 to frameGridImmobile.Grid.RowCount - 1 do
  begin
    frameGridImmobile.Grid.Objects[Ord(scName),RowIndex] := nil;
  end;
  }
  inherited;
  BasicPackage := Package as TMt3dBasic;
  comboVersion.ItemIndex := Ord(BasicPackage.Mt3dVersion);
  edMassUnit.Text := BasicPackage.MassUnit;
  rdeInactiveConcentration.Text := FloatToStr(BasicPackage.InactiveConcentration);
  rdeMinimumSaturatedFraction.Text := FloatToStr(BasicPackage.MinimumSaturatedFraction);
  seStressPeriod.AsInteger := BasicPackage.InitialConcentrationStressPeriod;
  seTimeStep.AsInteger := BasicPackage.InitialConcentrationTimeStep;
  seTransportStep.AsInteger := BasicPackage.InitialConcentrationTransportStep;
  comboInitialConcentrationChoice.ItemIndex := Ord(BasicPackage.InitialChoice);

  InitializeSpeciesGrids;

  for Option := Low(TMt3dUsgsOption) to High(TMt3dUsgsOption) do
  begin
    if Option In BasicPackage.Mt3dUsgsOptions then
    begin
      chklstOptions.Checked[Ord(Option)] := cbChecked;
    end
    else
    begin
      chklstOptions.Checked[Ord(Option)] := cbUnchecked;
    end;
  end;

end;

procedure TframeMt3dBasicPkg.SetData(Package: TModflowPackageSelection);
var
  BasicPackage: TMt3dBasic;
  Mt3dUsgsOptions: TMt3dUsgsOptions;
  Option: TMt3dUsgsOption;
begin
  inherited;
  BasicPackage := Package as TMt3dBasic;
  BasicPackage.Mt3dVersion := TMt3dVersion(comboVersion.ItemIndex);
  BasicPackage.MassUnit := edMassUnit.Text;
  BasicPackage.InactiveConcentration :=
    StrToFloat(rdeInactiveConcentration.Text);
  BasicPackage.MinimumSaturatedFraction :=
    StrToFloat(rdeMinimumSaturatedFraction.Text);
  BasicPackage.InitialConcentrationStressPeriod := seStressPeriod.AsInteger;
  BasicPackage.InitialConcentrationTimeStep := seTimeStep.AsInteger;
  BasicPackage.InitialConcentrationTransportStep := seTransportStep.AsInteger;
  BasicPackage.InitialChoice :=
    TMt3dInitialChoice(comboInitialConcentrationChoice.ItemIndex);

  Mt3dUsgsOptions := [];
  for Option := Low(TMt3dUsgsOption) to High(TMt3dUsgsOption) do
  begin
    if chklstOptions.Checked[Ord(Option)] = cbChecked then
    begin
      Include(Mt3dUsgsOptions, Option);
    end;
  end;
  BasicPackage.Mt3dUsgsOptions := Mt3dUsgsOptions;
end;

procedure TframeMt3dBasicPkg.FixNames(Names: TStringList; AFrame: TframeGrid);
var
  Index: Integer;
  AName: string;
begin
  for Index := 1 to AFrame.seNumber.AsInteger do
  begin
    AName := Trim(AFrame.Grid.Cells[Ord(scName), Index]);
    if AName <> '' then
    begin
      AName := GenerateNewRoot(AName);
      if Names.IndexOf(AName) >= 0 then
      begin
        AFrame.Grid.Cells[Ord(scName), Index] := '';
        AFrame.Grid.Objects[Ord(scName), Index] := nil;
      end
      else
      begin
        Names.Add(AName);
      end;
    end
    else
    begin
      AFrame.Grid.Cells[Ord(scName), Index] := '';
      AFrame.Grid.Objects[Ord(scName), Index] := nil;
    end;
  end;
end;

procedure TframeMt3dBasicPkg.SetMt3dComponents(
  Mt3DComponents: TCustomChemSpeciesCollection; AFrame: TframeGrid);
var
  ItemIndex: Integer;
  Index: Integer;
  Item: TChemSpeciesItem;
  AList: TList;
  GridCol: TStrings;
  ItemRow: integer;
begin
  AList := TList.Create;
  try
    for Index := 1 to AFrame.seNumber.AsInteger do
    begin
      if Trim(AFrame.Grid.Cells[Ord(scName), Index]) <> '' then
      begin
        AList.Add(AFrame.Grid.Objects[Ord(scName), Index]);
      end;
    end;
    GridCol := AFrame.Grid.Cols[Ord(scName)];
    for Index := Mt3DComponents.Count - 1 downto 0 do
    begin
      if AList.IndexOf(Mt3DComponents[Index]) < 0 then
      begin
        Item := Mt3DComponents[Index];
        ItemRow := GridCol.IndexOfObject(Item);
        if ItemRow >= 1 then
        begin
          AFrame.Grid.Objects[Ord(scName), ItemRow] := nil;
        end;
        Mt3DComponents.Delete(Index);
      end;
    end;
    ItemIndex := 0;
    for Index := 1 to AFrame.seNumber.AsInteger do
    begin
      if (AFrame.Grid.Objects[Ord(scName), Index] = nil)
        and (Trim(AFrame.Grid.Cells[Ord(scName), Index]) <> '') then
      begin
        Item := Mt3DComponents.Add;
      end
      else
      begin
        Item := AFrame.Grid.Objects[Ord(scName), Index] as TChemSpeciesItem;
      end;
      if Item <> nil then
      begin
        Item.Index := ItemIndex;
        Item.Name := Trim(AFrame.Grid.Cells[Ord(scName), Index]);
        Item.UseInitialConcentrationFile := AFrame.Grid.Checked[Ord(scUseFile), Index];
        Item.InitialConcentrationFileName := AFrame.Grid.Cells[Ord(scFileName), Index];

        Inc(ItemIndex);
      end;
    end;
  finally
    AList.Free;
  end;
end;

procedure TframeMt3dBasicPkg.SetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection;
  ImmobileComponents: TChemSpeciesCollection);
var
  Names: TStringList;
begin
//  Names := TStringList.Create;
//  try
//    Names.Sorted := True;
//    Names.CaseSensitive := False;
//    FixNames(Names, frameGridMobile);
//    FixNames(Names, frameGridImmobile);
//  finally
//    Names.Free;
//  end;
//
//  SetMt3dComponents(MobileComponents, frameGridMobile);
//  SetMt3dComponents(ImmobileComponents, frameGridImmobile);
end;

procedure TframeMt3dBasicPkg.InitializeSpeciesGrids;
begin
//  frameGridMobile.Grid.Cells[Ord(scName), 0] := StrMobileSpecies;
//  frameGridMobile.Grid.Cells[Ord(scUseFile), 0] := StrUseInitialConcentr;
//  frameGridMobile.Grid.Cells[Ord(scFileName), 0] := StrFileName;
//
//  frameGridImmobile.Grid.Cells[Ord(scName), 0] := StrImmobileSpecies;
//  frameGridImmobile.Grid.Cells[Ord(scUseFile), 0] := StrUseInitialConcentr;
//  frameGridImmobile.Grid.Cells[Ord(scFileName), 0] := StrFileName;
end;

procedure TframeMt3dBasicPkg.SetTimeControlsEnabled(ShouldEnable: Boolean);
begin
  comboInitialConcentrationChoice.Enabled := ShouldEnable;
  ShouldEnable := ShouldEnable
    and (TMt3dInitialChoice(comboInitialConcentrationChoice.ItemIndex)
    = micSpecifyTimeStep);
  seStressPeriod.Enabled := ShouldEnable;
  seTimeStep.Enabled := ShouldEnable;
  seTransportStep.Enabled := ShouldEnable;
end;

//function TframeMt3dBasicPkg.Mt3dTimeControlsShouldBeEnabled: Boolean;
//var
//  RowIndex: Integer;
//begin
//  result := False;
//  for RowIndex := 1 to frameGridMobile.Grid.RowCount - 1 do
//  begin
//    result := frameGridMobile.Grid.Checked[Ord(scUseFile), RowIndex];
//    if result then
//    begin
//      break;
//    end;
//  end;
//  if not result then
//  begin
//    for RowIndex := 1 to frameGridImmobile.Grid.RowCount - 1 do
//    begin
//      result := frameGridImmobile.Grid.Checked[Ord(scUseFile), RowIndex];
//      if result then
//      begin
//        break;
//      end;
//    end;
//  end;
//end;

procedure TframeMt3dBasicPkg.comboInitialConcentrationChoiceChange(
  Sender: TObject);
begin
  inherited;
  EnableTimeControls;
end;

procedure TframeMt3dBasicPkg.comboVersionChange(Sender: TObject);
begin
  inherited;
  Enable_chklstOptions;
end;

procedure TframeMt3dBasicPkg.EnableTimeControls;
var
  ShouldEnable: Boolean;
begin
//  ShouldEnable := Mt3dTimeControlsShouldBeEnabled;
//  SetTimeControlsEnabled(ShouldEnable);
  if Assigned(OnEnableTimeControls) then
  begin
    OnEnableTimeControls(Self);
  end;
end;

procedure TframeMt3dBasicPkg.Enable_chklstOptions;
begin
  chklstOptions.Enabled := rcSelectionController.Enabled
    and (comboVersion.ItemIndex = 0);
end;

procedure TframeMt3dBasicPkg.GetMt3dComponents(
  Mt3DComponents: TCustomChemSpeciesCollection; AFrame: TframeGrid);
var
  Item: TChemSpeciesItem;
  Index: Integer;
  RowIndex: Integer;
begin
  AFrame.seNumber.AsInteger := Mt3DComponents.Count;
  AFrame.Grid.BeginUpdate;
  try
    for RowIndex := 1 to AFrame.Grid.RowCount - 1 do
    begin
      AFrame.Grid.Objects[Ord(scName), RowIndex] := nil;
    end;
    if Mt3DComponents.Count > 0 then
    begin
      for Index := 0 to Mt3DComponents.Count - 1 do
      begin
        Item := Mt3DComponents[Index];
        AFrame.Grid.Cells[Ord(scName), Index + 1] := Item.Name;
        AFrame.Grid.Checked[Ord(scUseFile), Index + 1] := Item.UseInitialConcentrationFile;
        AFrame.Grid.Cells[Ord(scFileName), Index + 1] := Item.InitialConcentrationFileName;
        AFrame.Grid.Objects[Ord(scName), Index + 1] := Item;
      end;
    end
    else
    begin
      AFrame.Grid.Cells[Ord(scName), 1] := '';
        AFrame.Grid.Objects[Ord(scName), 1] := nil;
    end;
  finally
    AFrame.Grid.EndUpdate;
  end;
  AFrame.seNumber.AsInteger := Mt3DComponents.Count;
end;

procedure TframeMt3dBasicPkg.GetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection;
  ImmobileComponents: TChemSpeciesCollection);
begin
//  GetMt3dComponents(MobileComponents, frameGridMobile);
//  GetMt3dComponents(ImmobileComponents, frameGridImmobile);
//  EnableTimeControls;
end;

procedure TframeMt3dBasicPkg.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  Enable_chklstOptions;
end;

end.
