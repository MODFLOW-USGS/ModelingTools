unit frmModflowOptionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, JvExStdCtrls, JvCheckBox, JvCombobox,
  ExtCtrls, ArgusDataEntry, ComCtrls, Buttons, ModflowOptionsUnit, UndoItems,
  JvListComb, Mask, JvExMask, JvSpin, JvExControls,
  RbwController, RequiredDataSetsUndoUnit, JvToolEdit,
  PhastModelUnit, GrayTabs;

type
  TModelOptions = class(TCollectionItem)
  private
    FDescription: TStrings;
    FOwhmBasicOptions: TStrings;
    procedure SetDescription(const Value: TStrings);
    procedure SetOwhmBasicOptions(const Value: TStrings);
  public
    Model: TCustomModel;
    CalculateFlow: boolean;
    PrintTime: boolean;
    InitialHeadsFile: string;
    WettingActive: boolean;
    WettingFactor: double;
    WettingIterations: integer;
    WettingEquation: integer;
    OpenInTextEditor: boolean;
    LengthUnit: integer;
    TimeUnit: integer;
    ProjectDate: string;
    Modeler: string;
    ProjectName: string;
    HDry: real;
    HNoFlow: real;
    StopError: Boolean;
    StopErrorCriterion: double;
    NewtonMF6: Boolean;
    UnderRelaxationMF6: Boolean;
    WriteBinaryGridFile: Boolean;
    property OwhmBasicOptions: TStrings read FOwhmBasicOptions write SetOwhmBasicOptions;
    property Description: TStrings read FDescription write SetDescription;
    procedure AssignOptionsToModel;
    procedure AssignModel(AModel: TCustomModel);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TModelOptionsCollection = class(TCollection)
  private
    function GetItems(Index: integer): TModelOptions;
    procedure SetItems(Index: integer; const Value: TModelOptions);
  public
    Constructor Create(Model: TPhastModel);
    function Add: TModelOptions;
    property Items[Index: integer]: TModelOptions read GetItems write SetItems; default;
    procedure AssignOptionsToModels;
  end;

  TfrmModflowOptions = class(TfrmCustomGoPhast)
    pcOptions: TPageControl;
    TabSheet1: TTabSheet;
    Label3: TLabel;
    edProjectName: TEdit;
    Label4: TLabel;
    Label2: TLabel;
    edDate: TEdit;
    edModeler: TEdit;
    Label1: TLabel;
    memoComments: TMemo;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    cbPRINTTIME: TJvCheckBox;
    cbCHTOCH: TJvCheckBox;
    Label5: TLabel;
    rdeHNOFLO: TRbwDataEntry;
    pnlBottom: TPanel;
    Label6: TLabel;
    comboTimeUnit: TJvComboBox;
    Label7: TLabel;
    comboLengthUnit: TJvComboBox;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Label8: TLabel;
    rdeHDRY: TRbwDataEntry;
    tabWetting: TTabSheet;
    lblWetFact: TLabel;
    rdeWettingFact: TRbwDataEntry;
    lblCheckDry: TLabel;
    seCheckDry: TJvSpinEdit;
    lblWettingEquation: TLabel;
    comboWettingEquation: TJvImageComboBox;
    rconWet: TRbwController;
    cbWetting: TCheckBox;
    cbOpenInTextEditor: TJvCheckBox;
    lblWettingDataSets: TLabel;
    feInitialHeads: TJvFilenameEdit;
    lblInitialHeads: TLabel;
    pnlModel: TPanel;
    lblModel: TLabel;
    comboModel: TComboBox;
    edMasUnit: TLabeledEdit;
    cbStopError: TJvCheckBox;
    lbl1: TLabel;
    rdeStopErrorCriterion: TRbwDataEntry;
    cbWriteBinaryGridFile: TCheckBox;
    cbNewton: TCheckBox;
    cbUnderRelaxation: TCheckBox;
    cbUseGsflowFormat: TCheckBox;
    tabOwhmOptions: TTabSheet;
    memoBasicOptions: TMemo;
    lblBasicOptions: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure rdeHNOFLOExit(Sender: TObject);
    procedure comboTimeUnitChange(Sender: TObject);
    procedure comboLengthUnitChange(Sender: TObject);
    procedure edProjectNameExit(Sender: TObject);
    procedure edDateExit(Sender: TObject);
    procedure edModelerExit(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rdeHDRYExit(Sender: TObject);
    procedure cbWettingClick(Sender: TObject);
    procedure pcOptionsChange(Sender: TObject);
    procedure comboModelChange(Sender: TObject);
    procedure cbStopErrorClick(Sender: TObject);
    procedure feInitialHeadsChange(Sender: TObject);
    procedure cbNewtonClick(Sender: TObject);
  private
    FCurrentOptions: TModelOptions;
    FModelOptionsCollection: TModelOptionsCollection;
    procedure SetCurrentOptions(const Value: TModelOptions);
    property CurrentOptions: TModelOptions read FCurrentOptions
      write SetCurrentOptions;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoGeneralOptions = class(TCustomCreateRequiredDataSetsUndo)
  private
    FNewOptionsCollection: TModelOptionsCollection;
    FOldOptionsCollection: TModelOptionsCollection;
    FOldMassUnits: string;
    FNewMassUnits: string;
    FNewUseGsflowFormat: Boolean;
    FOldUseGsflowFormat: Boolean;
    procedure SetMassUnit(MassUnit: string);
    procedure SetUseGsflowFormat(UseGsflowFormat: Boolean);
  protected
    function Description: string; override;
  public
    Constructor Create(var NewOptionsCollection: TModelOptionsCollection;
      const MassUnit: string; const UseGsflowFormat: Boolean);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmModflowOptions: TfrmModflowOptions;

implementation

uses frmGoPhastUnit,
  GoPhastTypes;

resourcestring
  StrGeneralOptions = 'general options';

{$R *.dfm}

procedure TfrmModflowOptions.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModflowOptions.FormCreate(Sender: TObject);
begin
  inherited;
  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    rdeHNOFLO.Enabled := False;
    rdeHDRY.Enabled := False;
  end;
  pcOptions.ActivePageIndex := 0;
  FModelOptionsCollection := TModelOptionsCollection.Create(frmGoPhast.PhastModel);
  GetData;
end;

procedure TfrmModflowOptions.FormDestroy(Sender: TObject);
begin
  inherited;
  FModelOptionsCollection.Free;
end;

procedure TfrmModflowOptions.GetData;
var
  ItemIndex: Integer;
  ModflowOptions: TModflowOptions;
  NewHeight: Integer;
begin
  FillComboWithModelNames(comboModel);
  Assert(comboModel.Items.Count = FModelOptionsCollection.Count);
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    comboModel.Items.Objects[ItemIndex] :=
      FModelOptionsCollection.Items[ItemIndex];
  end;
  ModflowOptions := frmGoPhast.PhastModel.ModflowOptions;
  edProjectName.Text := ModflowOptions.ProjectName;
  edDate.Text := ModflowOptions.ProjectDate;
  edModeler.Text := ModflowOptions.Modeler;
  memoComments.Lines.Assign(ModflowOptions.Description);
  rdeHDRY.Text := FloatToStr(ModflowOptions.HDry);
  rdeHNOFLO.Text := FloatToStr(ModflowOptions.HNoFlow);
  comboTimeUnit.ItemIndex := ModflowOptions.TimeUnit;
  comboLengthUnit.ItemIndex := ModflowOptions.LengthUnit;
  cbOpenInTextEditor.Checked := ModflowOptions.OpenInTextEditor;
  cbNewton.Checked := ModflowOptions.NewtonMF6;
  cbUnderRelaxation.Checked := ModflowOptions.UnderRelaxationMF6;
  cbWriteBinaryGridFile.Checked := ModflowOptions.WriteBinaryGridFile;
  cbNewton.Enabled := frmGoPhast.ModelSelection = msModflow2015;
  cbNewtonClick(nil);
  cbWriteBinaryGridFile.Enabled := cbNewton.Enabled;

  edMasUnit.Text := frmGoPhast.PhastModel.ModflowPackages.Mt3dBasic.MassUnit;
  cbWettingClick(nil);
  comboModel.ItemIndex := 0;
  comboModelChange(nil);

  cbUseGsflowFormat.Checked := frmGoPhast.PhastModel.UseGsflowFormat;

  cbStopError.Checked := ModflowOptions.StopError;
  rdeStopErrorCriterion.Text := FloatToStr(ModflowOptions.StopErrorCriterion);
  cbStopErrorClick(nil);

  memoBasicOptions.Lines := ModflowOptions.OwhmBasicOptions;

  if not frmGoPhast.PhastModel.LgrUsed then
  begin
    NewHeight := Height - pnlModel.Height;
    pnlModel.Visible := False;
    Height := NewHeight;
  end;

  if (frmGoPhast.PhastModel.ModelSelection = msModflowNWT)
    and frmGoPhast.PhastModel.ModflowPackages.UpwPackage.IsSelected then
  begin
    tabWetting.TabVisible := False;
  end
  else
  begin
    tabWetting.TabVisible := True;
  end;

  tabOwhmOptions.TabVisible := frmGoPhast.PhastModel.ModelSelection = msModflowOwhm2;
end;

procedure TfrmModflowOptions.cbNewtonClick(Sender: TObject);
begin
  inherited;
  cbUnderRelaxation.Enabled := cbNewton.Enabled and cbNewton.Checked;
  if cbNewton.Checked then
  begin
    cbWetting.Checked := False;
  end;
end;

procedure TfrmModflowOptions.cbStopErrorClick(Sender: TObject);
begin
  inherited;
  rdeStopErrorCriterion.Enabled := cbStopError.Checked;
end;

procedure TfrmModflowOptions.cbWettingClick(Sender: TObject);
begin
  inherited;
  rconWet.Enabled := cbWetting.Checked;
  lblWettingDataSets.Visible := cbWetting.Checked;
  if cbWetting.Checked then
  begin
    cbNewton.Checked := False;
  end;
end;

procedure TfrmModflowOptions.pcOptionsChange(Sender: TObject);
begin
  inherited;
  HelpKeyWord := pcOptions.ActivePage.HelpKeyword;
end;

procedure TfrmModflowOptions.comboLengthUnitChange(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.LengthUnit := comboLengthUnit.ItemIndex;
    end;
  end;
end;

procedure TfrmModflowOptions.comboModelChange(Sender: TObject);
begin
  inherited;
  CurrentOptions := comboModel.Items.Objects[
    comboModel.ItemIndex] as TModelOptions;
end;

procedure TfrmModflowOptions.comboTimeUnitChange(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.TimeUnit := comboTimeUnit.ItemIndex;
    end;
  end;
end;

procedure TfrmModflowOptions.edDateExit(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.ProjectDate := edDate.Text;
    end;
  end;
end;

procedure TfrmModflowOptions.edModelerExit(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.Modeler := edModeler.Text;
    end;
  end;
end;

procedure TfrmModflowOptions.edProjectNameExit(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.ProjectName := edProjectName.Text;
    end;
  end;
end;

procedure TfrmModflowOptions.feInitialHeadsChange(Sender: TObject);
begin
  inherited;
  if (feInitialHeads.FileName <> '')
    and not FileExists(feInitialHeads.FileName) then
  begin
    feInitialHeads.Color := clRed;
  end
  else
  begin
    feInitialHeads.Color := clWindow;
  end;
end;

procedure TfrmModflowOptions.rdeHDRYExit(Sender: TObject);
var
  Value: Extended;
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  if TryStrToFloat(rdeHDry.Text, Value) then
  begin
    for ItemIndex := 0 to comboModel.Items.Count - 1 do
    begin
      Options := comboModel.Items.Objects[
        comboModel.ItemIndex] as TModelOptions;
      if Options <> nil then
      begin
        Options.HDry := Value;
      end;
    end;
  end;
end;

procedure TfrmModflowOptions.rdeHNOFLOExit(Sender: TObject);
var
  Value: Extended;
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  if TryStrToFloat(rdeHNOFLO.Text, Value) then
  begin
    for ItemIndex := 0 to comboModel.Items.Count - 1 do
    begin
      Options := comboModel.Items.Objects[
        comboModel.ItemIndex] as TModelOptions;
      if Options <> nil then
      begin
        Options.HNoFlow := Value;
      end;
    end;
  end;
end;

procedure TfrmModflowOptions.SetCurrentOptions(const Value: TModelOptions);
var
  AValue: extended;
begin
  if FCurrentOptions <> Value then
  begin
    if FCurrentOptions <> nil then
    begin
      FCurrentOptions.CalculateFlow := cbCHTOCH.Checked;
      FCurrentOptions.PrintTime := cbPRINTTIME.Checked;
      FCurrentOptions.InitialHeadsFile := feInitialHeads.FileName;
      FCurrentOptions.WettingActive := cbWetting.Checked;
      if TryStrToFloat(rdeWettingFact.Text, AValue) then
      begin
        FCurrentOptions.WettingFactor := AValue;
      end;
      FCurrentOptions.WettingIterations := seCheckDry.AsInteger;
      FCurrentOptions.WettingEquation := comboWettingEquation.ItemIndex;
      FCurrentOptions.OpenInTextEditor := cbOpenInTextEditor.Checked;
      FCurrentOptions.Description := memoComments.Lines;
      FCurrentOptions.StopError := cbStopError.Checked;
      if TryStrToFloat(rdeStopErrorCriterion.Text, AValue) then
      begin
        FCurrentOptions.StopErrorCriterion := AValue;
      end;
      FCurrentOptions.NewtonMF6 := cbNewton.Checked;
      FCurrentOptions.UnderRelaxationMF6 := cbUnderRelaxation.Checked;
      FCurrentOptions.WriteBinaryGridFile := cbWriteBinaryGridFile.Checked;
      FCurrentOptions.OwhmBasicOptions := memoBasicOptions.Lines;
//      FCurrentOptions.
    end;
    FCurrentOptions := Value;
    if FCurrentOptions <> nil then
    begin
      cbCHTOCH.Checked := FCurrentOptions.CalculateFlow;
      cbPRINTTIME.Checked := FCurrentOptions.PrintTime;
      try
        feInitialHeads.FileName := FCurrentOptions.InitialHeadsFile;
      except on EComboEditError do
        begin
          // do nothing.
        end;
      end;
      cbWetting.Checked := FCurrentOptions.WettingActive;
      cbWettingClick(nil);
      rdeWettingFact.Text := FloatToStr(FCurrentOptions.WettingFactor);
      seCheckDry.AsInteger := FCurrentOptions.WettingIterations;
      comboWettingEquation.ItemIndex := FCurrentOptions.WettingEquation;
      cbOpenInTextEditor.Checked := FCurrentOptions.OpenInTextEditor;
      memoComments.Lines := FCurrentOptions.Description;
      cbStopError.Checked := FCurrentOptions.StopError;
      cbStopErrorClick(nil);
      rdeStopErrorCriterion.Text := FloatToStr(FCurrentOptions.StopErrorCriterion);
      cbNewton.Checked := FCurrentOptions.NewtonMF6;
      cbUnderRelaxation.Checked := FCurrentOptions.UnderRelaxationMF6;
      cbNewtonClick(nil);
      cbWriteBinaryGridFile.Checked := FCurrentOptions.WriteBinaryGridFile;
      memoBasicOptions.Lines := FCurrentOptions.OwhmBasicOptions;
    end;
  end;
end;

procedure TfrmModflowOptions.SetData;
var
  Undo: TUndoGeneralOptions;
begin
  CurrentOptions := nil;
  Undo:= TUndoGeneralOptions.Create(FModelOptionsCollection, edMasUnit.Text,
    cbUseGsflowFormat.Checked);
  frmGoPhast.UndoStack.Submit(Undo);
end;

{ TUndoGeneralOptions }

constructor TUndoGeneralOptions.Create(
  var NewOptionsCollection: TModelOptionsCollection; const MassUnit: string; const UseGsflowFormat: Boolean);
begin
  inherited Create;
  FNewOptionsCollection := NewOptionsCollection;
  NewOptionsCollection := nil;

  FOldOptionsCollection := TModelOptionsCollection.Create(frmGoPhast.PhastModel);
  FOldMassUnits := frmGoPhast.PhastModel.ModflowPackages.Mt3dBasic.MassUnit;
  FOldUseGsflowFormat := frmGoPhast.PhastModel.UseGsflowFormat;

  FNewMassUnits := MassUnit;
  FNewUseGsflowFormat := UseGsflowFormat;
end;

function TUndoGeneralOptions.Description: string;
begin
  result := StrGeneralOptions
end;

destructor TUndoGeneralOptions.Destroy;
begin
  FNewOptionsCollection.Free;
  FOldOptionsCollection.Free;
  inherited;
end;

procedure TUndoGeneralOptions.DoCommand;
begin
  inherited;
  FNewOptionsCollection.AssignOptionsToModels;
  SetMassUnit(FNewMassUnits);
  SetUseGsflowFormat(FNewUseGsflowFormat);
  UpdatedRequiredDataSets;
end;

procedure TUndoGeneralOptions.Undo;
begin
  inherited;
  FOldOptionsCollection.AssignOptionsToModels;
  SetMassUnit(FOldMassUnits);
  SetUseGsflowFormat(FOldUseGsflowFormat);
  UpdatedRequiredDataSets;
end;

procedure TUndoGeneralOptions.SetMassUnit(MassUnit: string);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  frmGoPhast.PhastModel.ModflowPackages.Mt3dBasic.MassUnit := MassUnit;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    ChildModel.ModflowPackages.Mt3dBasic.MassUnit := MassUnit;
  end;
end;

procedure TUndoGeneralOptions.SetUseGsflowFormat(UseGsflowFormat: Boolean);
begin
  frmGoPhast.PhastModel.UseGsflowFormat := UseGsflowFormat;
end;

{ TModelOptions }

procedure TModelOptions.AssignModel(AModel: TCustomModel);
var
  Options: TModflowOptions;
//var
//  Options
begin
  Model := AModel;
  Options := Model.ModflowOptions;
  CalculateFlow := Options.ComputeFluxesBetweenConstantHeadCells;
  PrintTime := Options.PrintTime;
  InitialHeadsFile := Options.InitialHeadFileName;
  WettingActive := Model.ModflowWettingOptions.WettingActive;
  WettingFactor := Model.ModflowWettingOptions.WettingFactor;
  WettingIterations := Model.ModflowWettingOptions.WettingIterations;
  WettingEquation := Model.ModflowWettingOptions.WettingEquation;

  OpenInTextEditor := Options.OpenInTextEditor;
  Description := Options.Description;
  LengthUnit := Options.LengthUnit;
  TimeUnit := Options.TimeUnit;
  ProjectDate := Options.ProjectDate;
  Modeler := Options.Modeler;
  ProjectName := Options.ProjectName;
  HDry := Options.HDry;
  HNoFlow := Options.HNoFlow;
  StopError := Options.StopError;
  StopErrorCriterion := Options.StopErrorCriterion;
  NewtonMF6 := Options.NewtonMF6;
  UnderRelaxationMF6 := Options.UnderRelaxationMF6;
  WriteBinaryGridFile := Options.WriteBinaryGridFile;
  OwhmBasicOptions := Options.OwhmBasicOptions;
end;

procedure TModelOptions.AssignOptionsToModel;
var
  Options: TModflowOptions;
begin
  Options := Model.ModflowOptions;
  Options.ComputeFluxesBetweenConstantHeadCells := CalculateFlow;
  Options.PrintTime := PrintTime;
  Options.InitialHeadFileName := InitialHeadsFile;
  Model.ModflowWettingOptions.WettingActive := WettingActive;
  Model.ModflowWettingOptions.WettingFactor := WettingFactor;
  Model.ModflowWettingOptions.WettingIterations := WettingIterations;
  Model.ModflowWettingOptions.WettingEquation := WettingEquation;

  Options.OpenInTextEditor := OpenInTextEditor;
  Options.Description := Description;
  Options.LengthUnit := LengthUnit;
  Options.TimeUnit := TimeUnit;
  Options.ProjectDate := ProjectDate;
  Options.Modeler := Modeler;
  Options.ProjectName := ProjectName;
  Options.HDry := HDry;
  Options.HNoFlow := HNoFlow;
  Options.StopError := StopError;
  Options.StopErrorCriterion := StopErrorCriterion;
  Options.NewtonMF6 := NewtonMF6;
  Options.UnderRelaxationMF6 := UnderRelaxationMF6;
  Options.WriteBinaryGridFile := WriteBinaryGridFile;
  Options.OwhmBasicOptions := OwhmBasicOptions;

end;

constructor TModelOptions.Create(Collection: TCollection);
begin
  inherited;
  FDescription := TStringList.Create;
  FOwhmBasicOptions := TStringList.Create;
end;

destructor TModelOptions.Destroy;
begin
  FOwhmBasicOptions.Free;
  FDescription.Free;
  inherited;
end;

procedure TModelOptions.SetDescription(const Value: TStrings);
begin
  FDescription.Assign(Value);
end;

procedure TModelOptions.SetOwhmBasicOptions(const Value: TStrings);
begin
  FOwhmBasicOptions.Assign(Value);
end;

{ TModelOptionsCollection }

function TModelOptionsCollection.Add: TModelOptions;
begin
  result := inherited Add as TModelOptions
end;

procedure TModelOptionsCollection.AssignOptionsToModels;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].AssignOptionsToModel;
  end;
end;

constructor TModelOptionsCollection.Create(Model: TPhastModel);
var
  Item: TModelOptions;
  Index: Integer;
  ChildModel: TChildModel;
begin
  inherited Create(TModelOptions);
  Item := Add;
  Item.AssignModel(Model);
  if Model.LgrUsed then
  begin
    for Index := 0 to Model.ChildModels.Count - 1 do
    begin
      ChildModel := Model.ChildModels[Index].ChildModel;
      Item := Add;
      Item.AssignModel(ChildModel);
   end;
  end;
end;

function TModelOptionsCollection.GetItems(Index: integer): TModelOptions;
begin
  result := inherited Items[Index] as TModelOptions
end;

procedure TModelOptionsCollection.SetItems(Index: integer;
  const Value: TModelOptions);
begin
  inherited Items[Index] := Value;
end;

end.
