{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFDualLst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of JvFDualLst.PAS at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit frmManageFluxObservationsUnit;

interface

uses System.UITypes,
  Types, Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  JvListBox, JvCtrls, JvComponent, JvExStdCtrls, JvExComCtrls, ComCtrls,
  JvComCtrls, frmCustomGoPhastUnit, Buttons, Mask, JvExMask, JvSpin, Grids,
  RbwDataGrid4, JvEdit, JvExExtCtrls, JvNetscapeSplitter, UndoItems,
  ModflowPackageSelectionUnit, FluxObservationUnit, RbwParser, JvCombobox,
  JvListComb, ArgusDataEntry, Mt3dmsFluxObservationsUnit, Menus,
  ScreenObjectUnit, GrayTabs;

type
  TFluxColumns = (fcName, fcTime, fcValue, fcStatistic, fcStatFlag, fcComment);
  TMt3dFluxColumns = (fmcName, fmcSpecies, fmcObsType, fmcTime, fmcValue, fmcWeight, fmcComment);

  TfrmManageFluxObservations = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    pnlMain: TPanel;
    pcMain: TJvPageControl;
    tabObservationsTimes: TTabSheet;
    lblNumObsTimes: TLabel;
    rdgFluxObsTimes: TRbwDataGrid4;
    seNumObsTimes: TJvSpinEdit;
    btnDelete: TButton;
    btnInsert: TButton;
    tabObjects: TTabSheet;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    lblFactor: TLabel;
    SrcList: TJvListBox;
    IncBtn: TButton;
    IncAllBtn: TButton;
    ExclBtn: TButton;
    ExclAllBtn: TButton;
    DstList: TJvListBox;
    edFactorFormula: TJvEdit;
    btnFactorFormula: TButton;
    pnlTop: TPanel;
    edObservationName: TJvEdit;
    lblObservationName: TLabel;
    btnDeleteObservation: TButton;
    btnAddObservation: TButton;
    rparserThreeDFormulaElements: TRbwParser;
    rdeMultiValueEdit: TRbwDataEntry;
    comboMultiStatFlag: TJvImageComboBox;
    lblTreatment: TLabel;
    comboTreatment: TComboBox;
    tvFluxObservations: TTreeView;
    tabMassFlux: TTabSheet;
    rdeMassFluxMultiValueEdit: TRbwDataEntry;
    comboMt3dmsSpecies: TJvImageComboBox;
    rdgConcFluxObsTimes: TRbwDataGrid4;
    seNumMt3dmsObsTimes: TJvSpinEdit;
    lblNumMt3dmsObsTimes: TLabel;
    btnDeleteMt3dmsFlux: TButton;
    btnInsertMt3dmsFlux: TButton;
    pmSelectEditAvailable: TPopupMenu;
    miSelectAvailable: TMenuItem;
    miEditAvailable: TMenuItem;
    miGotoAvailable: TMenuItem;
    pmSelectEditUsed: TPopupMenu;
    miSelectUsed: TMenuItem;
    miEditUsed: TMenuItem;
    miGoToUsed: TMenuItem;
    miHideAvailable: TMenuItem;
    miHideUsed: TMenuItem;
    pcGroup: TPageControl;
    tabObservationProperties: TTabSheet;
    tabObservationGroupNames: TTabSheet;
    rdgGroupNames: TRbwDataGrid4;
    procedure IncBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExclBtnClick(Sender: TObject);
    procedure ExclAllBtnClick(Sender: TObject);
    procedure SrcListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DstListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SrcListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DstListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SrcListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DstListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject); override;
    procedure ListClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnAddObservationClick(Sender: TObject);
    procedure btnDeleteObservationClick(Sender: TObject);
    procedure edObservationNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure rdgFluxObsTimesExit(Sender: TObject);
    procedure seNumObsTimesChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure edFactorFormulaChange(Sender: TObject);
    procedure btnFactorFormulaClick(Sender: TObject);
    procedure edFactorFormulaExit(Sender: TObject);
    procedure rdgFluxObsTimesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgFluxObsTimesEndUpdate(Sender: TObject);
    procedure edObservationNameExit(Sender: TObject);
    procedure rdgFluxObsTimesHorizontalScroll(Sender: TObject);
    procedure tabObservationsTimesResize(Sender: TObject);
    procedure rdgFluxObsTimesColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdeMultiValueEditChange(Sender: TObject);
    procedure comboMultiStatFlagChange(Sender: TObject);
    procedure rdgFluxObsTimesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure comboTreatmentChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvFluxObservationsChange(Sender: TObject; Node: TTreeNode);
    procedure rdeMassFluxMultiValueEditChange(Sender: TObject);
    procedure comboMt3dmsSpeciesChange(Sender: TObject);
    procedure seNumMt3dmsObsTimesChange(Sender: TObject);
    procedure rdgConcFluxObsTimesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgConcFluxObsTimesColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdgConcFluxObsTimesEndUpdate(Sender: TObject);
    procedure rdgConcFluxObsTimesExit(Sender: TObject);
    procedure rdgConcFluxObsTimesHorizontalScroll(Sender: TObject);
    procedure rdgConcFluxObsTimesSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure btnDeleteMt3dmsFluxClick(Sender: TObject);
    procedure btnInsertMt3dmsFluxClick(Sender: TObject);
    procedure rdgConcFluxObsTimesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure pmSelectEditAvailablePopup(Sender: TObject);
    procedure miEditAvailableClick(Sender: TObject);
    procedure miSelectAvailableClick(Sender: TObject);
    procedure miGotoAvailableClick(Sender: TObject);
    procedure miSelectUsedClick(Sender: TObject);
    procedure miEditUsedClick(Sender: TObject);
    procedure miGoToUsedClick(Sender: TObject);
    procedure pmSelectEditUsedPopup(Sender: TObject);
    procedure miHideUsedClick(Sender: TObject);
    procedure miHideAvailableClick(Sender: TObject);
    procedure rdgGroupNamesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FChobNode: TTreeNode;
    FChobObservations: TFluxObservationGroups;
    FSelectedGroup: TCustomFluxObservationGroups;
    FSelectedObservation: TCustomFluxObservationGroup;
    FChdScreenObjects: TList;
    FSettingObservation: Boolean;
    FUpdatingFormula: Boolean;
    FDrobObservations: TFluxObservationGroups;
    FDrnScreenObjects: TList;
    FGbobObservations: TFluxObservationGroups;
    FGhbScreenObjects: TList;
    FRvobObservations: TFluxObservationGroups;
    FRivScreenObjects: TList;
    FStobObservations: TFluxObservationGroups;
    FStrScreenObjects: TList;
    FGbobNode: TTreeNode;
    FDrobNode: TTreeNode;
    FRvobNode: TTreeNode;
    FStobNode: TTreeNode;
    FSettingTimeCount: Boolean;
    FHeadMassFluxObservations: TList;
    FWellMassFluxObservations: TList;
    FDrnMassFluxObservations: TList;
    FRivMassFluxObservations: TList;
    FGhbMassFluxObservations: TList;
    FRchMassFluxObservations: TList;
    FEvtMassFluxObservations: TList;
    FMassLoadingMassFluxObservations: TList;
    FResMassFluxObservations: TList;
    FLakMassFluxObservations: TList;
    FDrtMassFluxObservations: TList;
    FEtsMassFluxObservations: TList;
    FStrMassFluxObservations: TList;
    FFhbHeadsMassFluxObservations: TList;
    FFhbFlowsMassFluxObservations: TList;
    FMassFluxObs: TMassFluxObs;
    FHeadMassFluxNode: TTreeNode;
    FWellMassFluxNode: TTreeNode;
    FDrnMassFluxNode: TTreeNode;
    FRivMassFluxNode: TTreeNode;
    FGhbMassFluxNode: TTreeNode;
    FRchMassFluxNode: TTreeNode;
    FEVTMassFluxNode: TTreeNode;
    FMassLoadingMassFluxNode: TTreeNode;
    FResMassFluxNode: TTreeNode;
    FLakMassFluxNode: TTreeNode;
    FEtsMassFluxNode: TTreeNode;
    FDrtMassFluxNode: TTreeNode;
    FStrMassFluxNode: TTreeNode;
    FFhbHeadsMassFluxNode: TTreeNode;
    FFhbFlowsMassFluxNode: TTreeNode;
    procedure SetSelectedObservation(const Value: TCustomFluxObservationGroup);
    procedure AssignObsNames;
    procedure DisplayFactor;
    procedure UpdateFactor;
    procedure CreateVariables;
    procedure LayoutMt3dmsMultiFluxEdits;
    procedure ReadMassFluxObservations(Package: TModflowPackageSelection;
      const FluxTypeLabel: string;
      FluxObservations: TMt3dmsFluxObservationGroups; var ParentNode: TTreeNode);
    procedure SetSelectedMt3dmsObs(Mt3dmsObs: TMt3dmsFluxObservationGroup);
    procedure SetSelectedGroup(const Value: TCustomFluxObservationGroups);
    property SelectedObservation: TCustomFluxObservationGroup
      read FSelectedObservation write SetSelectedObservation;
    property SelectedGroup: TCustomFluxObservationGroups read FSelectedGroup
      write SetSelectedGroup;
    procedure GetData;
    procedure SetData;
    procedure ReadFluxObservations(Package: TModflowPackageSelection;
      FluxObservations: TFluxObservationGroups; var ParentNode: TTreeNode);
    procedure UpdateObjectsInSelectedObservation;
    procedure GetGlobalVariables;
    function CheckFormula(FunctionString: string; ShowError: boolean): boolean;
    procedure AssignFactor(NewFormula: string);
    procedure InitializeFirstRow(Grid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
    procedure CheckErrors;
    procedure LayoutMultiFluxEdits;
    procedure LayoutMt3dmsFluxEdits;
    procedure AssignValuesToSelectedGridCells(const NewText: string;
      Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
    procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
      const StartCol, EndCol: integer);
    procedure SetSelectedGroupAndObservation(TreeView: TTreeView);
    procedure SetStatFlagLabels;
    procedure SetSelectedFluxObs(FluxObs: TFluxObservationGroup);
    procedure SetObsNameLength(SpinEdit: TJvSpinEdit);
    procedure SetObsGridRowCount(SpinEdit: TJvSpinEdit; Grid: TRbwDataGrid4;
      DeleteButton: TButton);
    procedure InitializeNewObs(RowIndex, TimeColumn, ValueColumn,
      CommentColumn: Integer; Grid: TRbwDataGrid4;
      ObsTime: TCustomFluxObservationItem);
    procedure GetSelectedObjects(ListBox: TJvListBox; ScreenObjects: TScreenObjectList);
    procedure SelectObjects(ListBox: TJvListBox);
    procedure HideObjects(ListBox: TJvListBox);
    procedure EditAnObject(ListBox: TJvListBox);
    procedure GoToAnObject(ListBox: TJvListBox);
  public
    procedure SetButtons;
  end;

implementation

uses
  JvBoxProcs, frmGoPhastUnit, Math, GIS_Functions,
  GoPhastTypes, DataSetUnit, frmFormulaUnit, frmErrorsAndWarningsUnit,
  PhastModelUnit, Mt3dmsChemSpeciesUnit, UndoItemsScreenObjects, frmGoToUnit,
  Modflow6ObsUnit;

resourcestring
  StrName = 'Name';
  StrTime = 'Time';
  StrObservedValue = 'Observed value';
  StrComment = 'Comment';
  StrStatistic = 'Statistic';
  StrStatFlag = 'StatFlag';
  StrComponent = 'Component';
  StrWeight = 'Weight';
  StrObservationTimeOr = 'Observation Time or Frequency (FluxTimeObs)';
  StrObservationType = 'Observation type';
  StrErrorTheFormulaI = 'Error: the formula is does not result in a real num' +
  'ber';
  StrSpecifiedHeadMass = 'Specified Head Mass Flux';
  StrWellMassFlux = 'Well Mass Flux';
  StrDrainMassFlux = 'Drain Mass Flux';
  StrRiverMassFlux = 'River Mass Flux';
  StrGHBMassFlux = 'GHB Mass Flux';
  StrRechargeMassFlux = 'Recharge Mass Flux';
  StrEVTMassFlux = 'EVT Mass Flux';
  StrMassLoading = 'Mass Loading';
  StrResevoirMassFlux = 'Resevoir Mass Flux';
  StrLakeMassFlux = 'Lake Mass Flux';
  StrDRTMassFlux = 'DRT Mass Flux';
  StrSTRMassFlux = 'STR Mass Flux';
  StrFHBHeadMassFlux = 'FHB Head Mass Flux';
  StrFHBFlowMassFlux = 'FHB Flow Mass Flux';
  StrETSMassFlux = 'ETS Mass Flux';
  StrErrorInFormulaS = 'Error in formula: %s';
  StrYouMustDefineAtL = 'You must define at least one chemical species in th' +
  'e MT3DMS BTN package before this dialog box can be displayed. Chemical sp' +
  'ecies are defined in the MODFLOW Packages and Programs dialog box.';
  StrDoYouWantToSave = 'Do you want to save your changes first?';
  StrSomethingHasGoneW = 'Something has gone wrong. Please close this dialog' +
  ' box and try again. If problems persist, please contact the ModelMuse dev' +
  'eloper.';

{$R *.dfm}

var
  FPriorErrors: TStringList;
const
  // MODFLOW-2005 allows observation names to be up to 12 characters in length.
  // UCODE allows names up to a length of 20.
  MaxObservationNameLength = 12;

procedure TfrmManageFluxObservations.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := (SrcList.Items.Count = 0);
  DstEmpty := (DstList.Items.Count = 0);
  IncBtn.Enabled := not SrcEmpty and (SrcList.SelCount > 0);
  IncAllBtn.Enabled := not SrcEmpty;
  ExclBtn.Enabled := not DstEmpty and (DstList.SelCount > 0);
  ExclAllBtn.Enabled := not DstEmpty;
end;

procedure TfrmManageFluxObservations.GoToAnObject(ListBox: TJvListBox);
var
  ScreenObject: TScreenObject;
  UndoShowHide: TUndoShowHideScreenObject;
  ScreenObjects: TScreenObjectList;
begin
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedObjects(ListBox, ScreenObjects);
    if ScreenObjects.Count = 1 then
    begin
      ScreenObject := ScreenObjects[0];
      if not ScreenObject.Visible then
      begin
        UndoShowHide := TUndoShowHideScreenObject.Create;
        UndoShowHide.AddScreenObjectToChange(ScreenObject);
        frmGoPhast.UndoStack.Submit(UndoShowHide);
      end;
      GoToObject(ScreenObject);
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmManageFluxObservations.HideObjects(ListBox: TJvListBox);
var
  ScreenObjects: TScreenObjectList;
begin
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedObjects(ListBox, ScreenObjects);
    if ScreenObjects.Count > 0 then
    begin
      HideMultipleScreenObjects(ScreenObjects);
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmManageFluxObservations.EditAnObject(ListBox: TJvListBox);
var
  ScreenObject: TScreenObject;
  ScreenObjects: TScreenObjectList;
begin
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedObjects(ListBox, ScreenObjects);
    if ScreenObjects.Count = 1 then
    begin
      ScreenObject := ScreenObjects[0];
      SelectAScreenObject(ScreenObject);
      if (MessageDlg(StrDoYouWantToSave, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes) then
      begin
        SetData;
      end;
      Close;
      frmGoPhast.EditScreenObjects;
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmManageFluxObservations.SelectObjects(ListBox: TJvListBox);
var
  ScreenObjects: TScreenObjectList;
begin
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedObjects(ListBox, ScreenObjects);
    if ScreenObjects.Count > 0 then
    begin
      SelectMultipleScreenObjects(ScreenObjects);
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmManageFluxObservations.GetSelectedObjects(ListBox: TJvListBox; ScreenObjects: TScreenObjectList);
var
  Index: Integer;
begin
  for Index := 0 to ListBox.Items.Count - 1 do
  begin
    if ListBox.Selected[Index] then
    begin
      ScreenObjects.Add(ListBox.Items.Objects[Index] as TScreenObject);
    end;
  end;
end;

procedure TfrmManageFluxObservations.InitializeNewObs(RowIndex, TimeColumn,
  ValueColumn, CommentColumn: Integer; Grid: TRbwDataGrid4;
  ObsTime: TCustomFluxObservationItem);
begin
  ObsTime.Index := RowIndex - 1;
  if Grid.Cells[TimeColumn, RowIndex] = '' then
  begin
    ObsTime.Time := 0;
    Grid.Cells[TimeColumn, RowIndex] := '0';
  end
  else
  begin
    ObsTime.Time := StrToFloat(Grid.Cells[TimeColumn, RowIndex]);
  end;
  if Grid.Cells[ValueColumn, RowIndex] = '' then
  begin
    ObsTime.ObservedValue := 0;
    Grid.Cells[ValueColumn, RowIndex] := '0';
  end
  else
  begin
    ObsTime.ObservedValue := StrToFloat(Grid.Cells[ValueColumn, RowIndex]);
  end;
  if Grid.Cells[CommentColumn, RowIndex] = '' then
  begin
    ObsTime.Comment := '';
    Grid.Cells[CommentColumn, RowIndex] := '';
  end
  else
  begin
    ObsTime.Comment := Grid.Cells[CommentColumn, RowIndex];
  end;
end;

procedure TfrmManageFluxObservations.SetObsGridRowCount(SpinEdit: TJvSpinEdit;
  Grid: TRbwDataGrid4; DeleteButton: TButton);
var
  Index: Integer;
  ColIndex: Integer;
  NewRowCount: Integer;
begin
  Grid.Enabled := SpinEdit.Enabled;
  Grid.Invalidate;
  NewRowCount := Max(1, SpinEdit.AsInteger) + 1;
  if NewRowCount < Grid.RowCount then
  begin
    Grid.BeginUpdate;
    try
      for Index := NewRowCount to Grid.RowCount - 1 do
      begin
        for ColIndex := 0 to Grid.ColCount - 1 do
        begin
          Grid.Cells[ColIndex, Index] := '';
        end;
        Grid.Objects[0, Index] := nil;
      end;
    finally
      Grid.EndUpdate;
    end;
  end;
  InitializeFirstRow(Grid, SpinEdit);
  Grid.RowCount := NewRowCount;
  DeleteButton.Enabled := SpinEdit.Enabled and (SpinEdit.AsInteger >= 1);
end;

procedure TfrmManageFluxObservations.SetObsNameLength(SpinEdit: TJvSpinEdit);
begin
  if SpinEdit.AsInteger > 0 then
  begin
    edObservationName.MaxLength := MaxObservationNameLength - 2
      - Trunc(Log10(SpinEdit.AsInteger));
    if Length(edObservationName.Text) > edObservationName.MaxLength then
    begin
      edObservationName.Text := Copy(edObservationName.Text, 1,
        edObservationName.MaxLength);
      edObservationNameChange(nil);
    end;
  end
  else
  begin
    edObservationName.MaxLength := MaxObservationNameLength - 2;
  end;
end;

procedure TfrmManageFluxObservations.SetSelectedMt3dmsObs(
  Mt3dmsObs: TMt3dmsFluxObservationGroup);
var
  MaxTimeStringLength: Integer;
  CurrentObjects: TList;
  AvailableList: TList;
  ScreenObject: TScreenObject;
  TimeString: string;
  ObsTime: TMt3dmsFluxObservation;
  Index1: Integer;
  Index2: Integer;
  Index3: Integer;
  Index4: Integer;
begin
  seNumMt3dmsObsTimes.Enabled := True;
  rdgConcFluxObsTimes.Enabled := True;
  edObservationName.Enabled := True;
  comboTreatment.Enabled := True;
  edObservationName.Text := Mt3dmsObs.ObservationName;
  comboTreatment.ItemIndex := Ord(Mt3dmsObs.Purpose);
  btnInsertMt3dmsFlux.Enabled := True;
  SrcList.Enabled := True;
  DstList.Enabled := True;
  IncBtn.Enabled := True;
  IncAllBtn.Enabled := True;
  ExclBtn.Enabled := True;
  ExclAllBtn.Enabled := True;
  CurrentObjects := TList.Create;
  try
    for Index1 := 0 to Mt3dmsObs.ObservationFactors.Count - 1 do
    begin
      ScreenObject := Mt3dmsObs.ObservationFactors[Index1].
        ScreenObject as TScreenObject;
      CurrentObjects.Add(ScreenObject);
    end;
    AvailableList := nil;

    if tvFluxObservations.Selected.Parent = nil then
    begin
      Beep;
      MessageDlg(StrSomethingHasGoneW, mtInformation, [mbOK], 0);
      Exit;
    end;

    if tvFluxObservations.Selected.Parent = FHeadMassFluxNode then
    begin
      AvailableList := FHeadMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FWellMassFluxNode then
    begin
      AvailableList := FWellMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FDrnMassFluxNode then
    begin
      AvailableList := FDrnMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FRivMassFluxNode then
    begin
      AvailableList := FRivMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FGhbMassFluxNode then
    begin
      AvailableList := FGhbMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FRchMassFluxNode then
    begin
      AvailableList := FRchMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FEVTMassFluxNode then
    begin
      AvailableList := FEvtMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FMassLoadingMassFluxNode then
    begin
      AvailableList := FMassLoadingMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FResMassFluxNode then
    begin
      AvailableList := FResMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FLakMassFluxNode then
    begin
      AvailableList := FLakMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FEtsMassFluxNode then
    begin
      AvailableList := FEtsMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FDrtMassFluxNode then
    begin
      AvailableList := FDrtMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FStrMassFluxNode then
    begin
      AvailableList := FStrMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FFhbHeadsMassFluxNode then
    begin
      AvailableList := FFhbHeadsMassFluxObservations;
    end
    else if tvFluxObservations.Selected.Parent = FFhbFlowsMassFluxNode then
    begin
      AvailableList := FFhbFlowsMassFluxObservations;
    end
    else
    begin
      Assert(False);
    end;
    for Index2 := 0 to AvailableList.Count - 1 do
    begin
      ScreenObject := AvailableList[Index2];
      if CurrentObjects.IndexOf(ScreenObject) >= 0 then
      begin
        DstList.Items.AddObject(ScreenObject.Name, ScreenObject);
        if ScreenObject.Selected then
        begin
          DstList.Selected[DstList.Count - 1] := True;
        end;
      end
      else
      begin
        SrcList.Items.AddObject(ScreenObject.Name, ScreenObject);
        if ScreenObject.Selected then
        begin
          SrcList.Selected[SrcList.Count - 1] := True;
        end;
      end;
    end;
  finally
    CurrentObjects.Free;
  end;
  for Index3 := 1 to rdgConcFluxObsTimes.RowCount - 1 do
  begin
    rdgConcFluxObsTimes.Objects[Ord(fcName), Index3] := nil;
  end;
  seNumMt3dmsObsTimes.AsInteger := Mt3dmsObs.ObservationTimes.Count;
  InitializeFirstRow(rdgConcFluxObsTimes, seNumMt3dmsObsTimes);
  MaxTimeStringLength := Length(IntToStr(Mt3dmsObs.ObservationTimes.Count));
  rdgConcFluxObsTimes.BeginUpdate;
  try
    for Index4 := 0 to Mt3dmsObs.ObservationTimes.Count - 1 do
    begin
      ObsTime := Mt3dmsObs.ObservationTimes[Index4];
      TimeString := IntToStr(Index4 + 1);
      while Length(TimeString) < MaxTimeStringLength do
      begin
        TimeString := '0' + TimeString;
      end;
      rdgConcFluxObsTimes.Cells[Ord(fmcName), Index4 + 1] :=
        Mt3dmsObs.ObservationName + '_' + TimeString;
      rdgConcFluxObsTimes.ItemIndex[Ord(fmcObsType), Index4 + 1]
        := Ord(ObsTime.ObservationType);
      rdgConcFluxObsTimesSetEditText(rdgConcFluxObsTimes,
        Ord(fmcObsType), Index4 + 1,
        rdgConcFluxObsTimes.Cells[Ord(fmcObsType), Index4 + 1]);
      case ObsTime.ObservationType of
        otTime:
          begin
            rdgConcFluxObsTimes.Cells[Ord(fmcTime), Index4 + 1] :=
              FloatToStr(ObsTime.Time);
          end;
        otFrequency:
          begin
            rdgConcFluxObsTimes.Cells[Ord(fmcTime), Index4 + 1] :=
              IntToStr(ObsTime.ObservationFrequency);
          end;
        else
          Assert(False);
      end;

      rdgConcFluxObsTimes.Cells[Ord(fmcValue), Index4 + 1] :=
        FloatToStr(ObsTime.ObservedValue);
      rdgConcFluxObsTimes.Cells[Ord(fmcWeight), Index4 + 1] :=
        FloatToStr(ObsTime.Weight);
      rdgConcFluxObsTimes.Cells[Ord(fmcSpecies), Index4 + 1] :=
        ObsTime.Species;
      rdgConcFluxObsTimes.Cells[Ord(fmcComment), Index4 + 1] :=
        ObsTime.Comment;
      rdgConcFluxObsTimes.Objects[Ord(fmcName), Index4 + 1] := ObsTime;
    end;
    rdgConcFluxObsTimes.Invalidate;
  finally
    rdgConcFluxObsTimes.EndUpdate;
  end;
end;

procedure TfrmManageFluxObservations.SetSelectedFluxObs(
  FluxObs: TFluxObservationGroup);
var
  MaxTimeStringLength: Integer;
  CurrentObjects: TList;
  AvailableList: TList;
  ScreenObject: TScreenObject;
  TimeString: string;
  ObsTime: TFluxObservation;
  Index1: Integer;
  Index2: Integer;
  Index3: Integer;
  Index4: Integer;
begin
  seNumObsTimes.Enabled := True;
  rdgFluxObsTimes.Enabled := True;
  edObservationName.Enabled := True;
  comboTreatment.Enabled := True;
  edObservationName.Text := FluxObs.ObservationName;
  comboTreatment.ItemIndex := Ord(FluxObs.Purpose);
  SetStatFlagLabels;
  btnInsert.Enabled := True;
  SrcList.Enabled := True;
  DstList.Enabled := True;
  IncBtn.Enabled := True;
  IncAllBtn.Enabled := True;
  ExclBtn.Enabled := True;
  ExclAllBtn.Enabled := True;
  CurrentObjects := TList.Create;
  try
    for Index1 := 0 to FluxObs.ObservationFactors.Count - 1 do
    begin
      ScreenObject := FluxObs.ObservationFactors[Index1].
        ScreenObject as TScreenObject;
      CurrentObjects.Add(ScreenObject);
    end;
    AvailableList := nil;
    if tvFluxObservations.Selected.Parent = nil then
    begin
      Beep;
      MessageDlg(StrSomethingHasGoneW, mtInformation, [mbOK], 0);
      Exit;
    end;
    if tvFluxObservations.Selected.Parent = FChobNode then
    begin
      AvailableList := FChdScreenObjects;
    end
    else if tvFluxObservations.Selected.Parent = FDrobNode then
    begin
      AvailableList := FDrnScreenObjects;
    end
    else if tvFluxObservations.Selected.Parent = FGbobNode then
    begin
      AvailableList := FGhbScreenObjects;
    end
    else if tvFluxObservations.Selected.Parent = FRvobNode then
    begin
      AvailableList := FRivScreenObjects;
    end
    else if tvFluxObservations.Selected.Parent = FStobNode then
    begin
      AvailableList := FStrScreenObjects;
    end
    else
    begin
      Assert(False);
    end;
    for Index2 := 0 to AvailableList.Count - 1 do
    begin
      ScreenObject := AvailableList[Index2];
      if CurrentObjects.IndexOf(ScreenObject) >= 0 then
      begin
        DstList.Items.AddObject(ScreenObject.Name, ScreenObject);
        if ScreenObject.Selected then
        begin
          DstList.Selected[DstList.Count - 1] := True;
        end;
      end
      else
      begin
        SrcList.Items.AddObject(ScreenObject.Name, ScreenObject);
        if ScreenObject.Selected then
        begin
          SrcList.Selected[SrcList.Count - 1] := True;
        end;
      end;
    end;
  finally
    CurrentObjects.Free;
  end;
  for Index3 := 1 to rdgFluxObsTimes.RowCount - 1 do
  begin
    rdgFluxObsTimes.Objects[Ord(fcName), Index3] := nil;
  end;
  seNumObsTimes.AsInteger := FluxObs.ObservationTimes.Count;
  InitializeFirstRow(rdgFluxObsTimes, seNumObsTimes);
  MaxTimeStringLength := Length(IntToStr(FluxObs.ObservationTimes.Count));
  rdgFluxObsTimes.BeginUpdate;
  try
    for Index4 := 0 to FluxObs.ObservationTimes.Count - 1 do
    begin
      ObsTime := FluxObs.ObservationTimes[Index4];
      TimeString := IntToStr(Index4 + 1);
      while Length(TimeString) < MaxTimeStringLength do
      begin
        TimeString := '0' + TimeString;
      end;
      rdgFluxObsTimes.Cells[Ord(fcName), Index4 + 1] :=
        FluxObs.ObservationName + '_' + TimeString;
      rdgFluxObsTimes.Cells[Ord(fcTime), Index4 + 1] :=
        FloatToStr(ObsTime.Time);
      rdgFluxObsTimes.Cells[Ord(fcValue), Index4 + 1] :=
        FloatToStr(ObsTime.ObservedValue);
      rdgFluxObsTimes.Cells[Ord(fcStatistic), Index4 + 1] :=
        FloatToStr(ObsTime.Statistic);
      rdgFluxObsTimes.Cells[Ord(fcStatFlag), Index4 + 1] :=
        rdgFluxObsTimes.Columns[Ord(fcStatFlag)].
        PickList[Ord(ObsTime.StatFlag)];
      rdgFluxObsTimes.Cells[Ord(fcComment), Index4 + 1] :=
        ObsTime.Comment;
      rdgFluxObsTimes.Objects[Ord(fcName), Index4 + 1] := ObsTime;
    end;
    rdgFluxObsTimes.Invalidate;
  finally
    rdgFluxObsTimes.EndUpdate;
  end;
end;

type TRbwDataGrid4Crack = class(TRbwDataGrid4);

procedure TfrmManageFluxObservations.SetStatFlagLabels;
begin
  Assert(FSelectedObservation <> nil);
  case FSelectedObservation.Purpose of
    ofObserved, ofInacative:
      begin
        rdgFluxObsTimes.Columns[Ord(fcStatFlag)].PickList :=
          ObservationStatFlagLabels;
      end;
    ofPredicted:
      begin
        rdgFluxObsTimes.Columns[Ord(fcStatFlag)].PickList :=
          PredictionStatFlagLabels;
      end;
  else
    Assert(False);
  end;
  TRbwDataGrid4Crack(rdgFluxObsTimes).HideEditor;
end;

procedure TfrmManageFluxObservations.SetSelectedGroup(
  const Value: TCustomFluxObservationGroups);
var
  RowIndex: Integer;
  index: Integer;
  AnObsGroup: TCustomFluxObservationGroup;
begin
  FSelectedGroup := Value;
  rdgGroupNames.BeginUpdate;
  try
    for RowIndex := 1 to rdgGroupNames.RowCount - 1 do
    begin
      rdgGroupNames.Cells[1,RowIndex] := Format('%d', [RowIndex]);
      rdgGroupNames.Cells[1,RowIndex] := '';
      rdgGroupNames.Objects[1,RowIndex] := nil;
    end;
    if FSelectedGroup = nil then
    begin
      rdgGroupNames.RowCount := 2;
    end
    else
    begin
      rdgGroupNames.RowCount := Max(2, FSelectedGroup.Count + 1);
      for index := 0 to FSelectedGroup.Count - 1 do
      begin
        AnObsGroup := FSelectedGroup[index];
        rdgGroupNames.Cells[1,index+1] := AnObsGroup.ObservationName;
        rdgGroupNames.Objects[1,index+1] := AnObsGroup;
      end;
    end;

  finally
    rdgGroupNames.EndUpdate
  end;
end;

procedure TfrmManageFluxObservations.SetSelectedGroupAndObservation(TreeView: TTreeView);
var
  AnObject: TObject;
  GroupSelected: boolean;
begin
  GroupSelected := (TreeView.Selected <> nil)
    and
    ((TreeView.Selected.Data = FChobObservations)
    or (TreeView.Selected.Data = FDrobObservations)
    or (TreeView.Selected.Data = FGbobObservations)
    or (TreeView.Selected.Data = FRvobObservations)
    or (TreeView.Selected.Data = FStobObservations)

    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsHeadMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsWellMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsDrnMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsRivMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsGhbMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsRchMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsEvtMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsMassLoadingMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsResMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsLakMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsDrtMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsEtsMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsStrMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsFhbHeadMassFluxObservations)
    or (TreeView.Selected.Data = FMassFluxObs.Mt3dmsFhbFlowMassFluxObservations));

  btnDeleteObservation.Enabled := (TreeView.Selected <> nil)
    and not GroupSelected;

  if tabObservationsTimes.TabVisible then
  begin
    rdgFluxObsTimesExit(nil);
  end;
  if (TreeView.Selected = nil) then
  begin
    SelectedGroup := nil;
    SelectedObservation := nil;
  end
  else
  begin
    AnObject := TreeView.Selected.Data;
    if AnObject is TCustomFluxObservationGroups then
    begin
      SelectedGroup := TreeView.Selected.Data;
      SelectedObservation := nil;
    end
    else
    begin
      SelectedGroup := TreeView.Selected.Parent.Data;
      SelectedObservation := TreeView.Selected.Data;
    end;
  end;
  DisplayFactor;
end;

procedure TfrmManageFluxObservations.LayoutMultiFluxEdits;
var
  AColVisible: Boolean;
  Index: Integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  if (rdgFluxObsTimes = nil) or (rdeMultiValueEdit = nil)
     or (comboMultiStatFlag = nil) then
  begin
    Exit;
  end;
  AColVisible := False;
  for Index := Ord(fcTime) to Ord(fcStatFlag) do
  begin
    if rdgFluxObsTimes.ColVisible[Index] then
    begin
      LayoutControls(rdgFluxObsTimes, rdeMultiValueEdit, nil, Index,
        rdgFluxObsTimes.Margins.Left);
      rdeMultiValueEdit.Width := rdgFluxObsTimes.ColWidths[Index];
      AColVisible := True;
      break;
    end;
  end;
  if not AColVisible then
  begin
    rdeMultiValueEdit.Visible := False;
  end;
  LayoutControls(rdgFluxObsTimes, comboMultiStatFlag, nil, Ord(fcStatFlag),
    rdgFluxObsTimes.Margins.Left);
  comboMultiStatFlag.Width := rdgFluxObsTimes.ColWidths[Ord(fcStatFlag)];
end;

procedure TfrmManageFluxObservations.LayoutMt3dmsFluxEdits;
var
  AColVisible: Boolean;
  Index: Integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  if (rdgConcFluxObsTimes = nil) or (rdeMassFluxMultiValueEdit = nil)
     or (comboMt3dmsSpecies = nil) then
  begin
    Exit;
  end;
  AColVisible := False;
  for Index := Ord(fmcTime) to Ord(fmcWeight) do
  begin
    if rdgConcFluxObsTimes.ColVisible[Index] then
    begin
      LayoutControls(rdgConcFluxObsTimes, rdeMassFluxMultiValueEdit, nil, Index,
        rdgConcFluxObsTimes.Margins.Left);
      rdeMassFluxMultiValueEdit.Width := rdgConcFluxObsTimes.ColWidths[Index];
      AColVisible := True;
      break;
    end;
  end;
  if not AColVisible then
  begin
    rdeMassFluxMultiValueEdit.Visible := False;
  end;
  LayoutControls(rdgConcFluxObsTimes, comboMt3dmsSpecies, nil, Ord(fmcSpecies),
    rdgConcFluxObsTimes.Margins.Left);
  comboMt3dmsSpecies.Width := rdgConcFluxObsTimes.ColWidths[Ord(fmcSpecies)];
end;

procedure TfrmManageFluxObservations.LayoutMt3dmsMultiFluxEdits;
var
  AColVisible: Boolean;
  Index: Integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  if (rdgConcFluxObsTimes = nil) or (rdeMassFluxMultiValueEdit = nil)
     or (comboMt3dmsSpecies = nil) then
  begin
    Exit;
  end;
  AColVisible := False;
  for Index := Ord(fmcTime) to Ord(fmcWeight) do
  begin
    if rdgConcFluxObsTimes.ColVisible[Index] then
    begin
      LayoutControls(rdgConcFluxObsTimes, rdeMassFluxMultiValueEdit, nil, Index,
        rdgConcFluxObsTimes.Margins.Left);
      rdeMassFluxMultiValueEdit.Width := rdgConcFluxObsTimes.ColWidths[Index];
      AColVisible := True;
      break;
    end;
  end;
  if not AColVisible then
  begin
    rdeMassFluxMultiValueEdit.Visible := False;
  end;
  LayoutControls(rdgConcFluxObsTimes, comboMt3dmsSpecies, nil, Ord(fmcSpecies),
    rdgConcFluxObsTimes.Margins.Left);
  comboMt3dmsSpecies.Width := rdgConcFluxObsTimes.ColWidths[Ord(fmcSpecies)];
end;

procedure TfrmManageFluxObservations.CheckErrors;
var
  ErrorIndex: Integer;
  ErrorMessages: TStringList;
  ErrorRoots: TStringList;
  Index: Integer;
begin
  for Index := 0 to FPriorErrors.Count - 1 do
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel, FPriorErrors[Index]);
  end;
  FPriorErrors.Clear;

  ErrorRoots := TStringList.Create;
  ErrorMessages := TStringList.Create;
  try
    FChobObservations.CheckObservationTimes(ErrorRoots, ErrorMessages);
    FDrobObservations.CheckObservationTimes(ErrorRoots, ErrorMessages);
    FGbobObservations.CheckObservationTimes(ErrorRoots, ErrorMessages);
    FRvobObservations.CheckObservationTimes(ErrorRoots, ErrorMessages);
    FStobObservations.CheckObservationTimes(ErrorRoots, ErrorMessages);

    FMassFluxObs.Mt3dmsHeadMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsWellMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsDrnMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsRivMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsGhbMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsRchMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsEvtMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsMassLoadingMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsResMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsLakMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsDrtMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsEtsMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsStrMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsFhbHeadMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);
    FMassFluxObs.Mt3dmsFhbFlowMassFluxObservations.
      CheckObservationTimes(ErrorRoots, ErrorMessages);

    Assert(ErrorRoots.Count = ErrorMessages.Count);
    for ErrorIndex := 0 to ErrorRoots.Count - 1 do
    begin
      FPriorErrors.Add(ErrorRoots[ErrorIndex]);
      frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, ErrorRoots[ErrorIndex],
        ErrorMessages[ErrorIndex], ErrorMessages.Objects[ErrorIndex]);
    end;
    if ErrorRoots.Count > 0 then
    begin
      frmErrorsAndWarnings.ShowAfterDelay;
    end;
  finally
    ErrorRoots.Free;
    ErrorMessages.Free;
  end;
end;

procedure TfrmManageFluxObservations.InitializeFirstRow(Grid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit);
var
  ColIndex: Integer;
begin
  if SpinEdit.AsInteger = 0 then
  begin
    for ColIndex := 0 to Grid.ColCount - 1 do
    begin
      Grid.Cells[ColIndex, 1] := '';
    end;
    Grid.Objects[0, 1] := nil;
  end;
end;

procedure TfrmManageFluxObservations.AssignFactor(NewFormula: string);
var
  FactorObject: TObservationFactor;
  ObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Index: Integer;
begin
  for Index := 0 to DstList.Items.Count - 1 do
  begin
    if DstList.Selected[Index] then
    begin
      ScreenObject := DstList.Items.Objects[Index] as TScreenObject;
      ObjectIndex := FSelectedObservation.ObservationFactors.
        IndexOfScreenObject(ScreenObject);
      if (ObjectIndex >= 0) then
      begin
        FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
        FactorObject.Factor := NewFormula;
      end;
    end;
  end;
end;

function TfrmManageFluxObservations.CheckFormula(FunctionString: string;
  ShowError: boolean): boolean;
var
  CompiledFormula: TExpression;
begin
  result := True;
  try
    rparserThreeDFormulaElements.Compile(FunctionString);
  except on E: ErbwParserError do
    begin
      edFactorFormula.Color := clRed;
      if ShowError then
      begin
        Beep;
        MessageDlg(Format(StrErrorInFormulaS, [E.Message]), mtError, [mbOK], 0);
      end;
      result := False;
      Exit;
    end
  end;

  CompiledFormula := rparserThreeDFormulaElements.CurrentExpression;
  // check that the formula is OK.
  if not (CompiledFormula.ResultType in [rdtDouble, rdtInteger]) then
  begin
    edFactorFormula.Color := clRed;
    if ShowError then
    begin
      Beep;
      MessageDlg(StrErrorTheFormulaI,
        mtError, [mbOK], 0);
    end;
    result := False;
  end
  else
  begin
    edFactorFormula.Color := clWindow;
    if ShowError then
    begin
      FunctionString := CompiledFormula.Decompile;
      if FunctionString <> edFactorFormula.Text then
      begin
        edFactorFormula.Text := FunctionString;
        if Assigned(edFactorFormula.OnChange) then
        begin
          edFactorFormula.OnChange(edFactorFormula);
        end;
        if Assigned(edFactorFormula.OnExit) then
        begin
          edFactorFormula.OnExit(edFactorFormula);
        end;
      end;
    end;
  end;
end;

procedure TfrmManageFluxObservations.comboMt3dmsSpeciesChange(Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(comboMt3dmsSpecies.Text, rdgConcFluxObsTimes,
    Ord(fmcSpecies), Ord(fmcSpecies));
end;

procedure TfrmManageFluxObservations.comboMultiStatFlagChange(Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(comboMultiStatFlag.Text, rdgFluxObsTimes,
    Ord(fcStatFlag), Ord(fcStatFlag));
end;

procedure TfrmManageFluxObservations.comboTreatmentChange(Sender: TObject);
var
  Index: Integer;
begin
  inherited;
  if (FSelectedObservation <> nil) then
  begin
    FSelectedObservation.Purpose :=
      TObservationPurpose(comboTreatment.ItemIndex);
    if FSelectedObservation is TFluxObservationGroup then
    begin
      SetStatFlagLabels;
      if FSelectedObservation.Purpose = ofPredicted then
      begin
        rdgFluxObsTimes.BeginUpdate;
        try
          for Index := 1 to rdgFluxObsTimes.RowCount - 1 do
          begin
            if rdgFluxObsTimes.ItemIndex[Ord(fcStatFlag), Index] < 0 then
            begin
              rdgFluxObsTimes.ItemIndex[Ord(fcStatFlag), Index] := 0;
            end;
          end;
          rdgFluxObsTimesExit(nil);
        finally
          rdgFluxObsTimes.EndUpdate;
        end;
      end;
    end
    else
    begin

    end;
  end;
end;

procedure TfrmManageFluxObservations.ReadMassFluxObservations(
  Package: TModflowPackageSelection; const FluxTypeLabel: string;
  FluxObservations: TMt3dmsFluxObservationGroups;
  var ParentNode: TTreeNode);
var
  Index: Integer;
  ANode: TTreeNode;
  Item: TMt3dmsFluxObservationGroup;
begin
  if frmGoPhast.PhastModel.PackageIsSelected(
    frmGoPhast.PhastModel.ModflowPackages.Mt3dmsTransObs)
    and frmGoPhast.PhastModel.PackageIsSelected(Package) then
  begin
    ParentNode := tvFluxObservations.Items.Add(nil, FluxTypeLabel);
    ParentNode.Data := FluxObservations;
    for Index := 0 to FluxObservations.Count - 1 do
    begin
      Item := FluxObservations[Index];
      ANode := tvFluxObservations.Items.AddChild(ParentNode,
        Item.ObservationName);
      ANode.Data := Item;
    end;
  end
  else
  begin
    ParentNode := nil;
  end;
end;

procedure TfrmManageFluxObservations.ReadFluxObservations(
  Package: TModflowPackageSelection; FluxObservations: TFluxObservationGroups;
  var ParentNode: TTreeNode);
var
  Index: Integer;
  Item: TFluxObservationGroup;
  ANode: TTreeNode;
begin
  if frmGoPhast.PhastModel.PackageIsSelected(Package) then
  begin
    ParentNode := tvFluxObservations.Items.Add(nil, Package.PackageIdentifier);
    ParentNode.Data := FluxObservations;
    for Index := 0 to FluxObservations.Count - 1 do
    begin
      Item := FluxObservations[Index];
      ANode := tvFluxObservations.Items.AddChild(ParentNode,
        Item.ObservationName);
      ANode.Data := Item;
    end;
  end
  else
  begin
    ParentNode := nil;
  end;
end;

procedure TfrmManageFluxObservations.seNumMt3dmsObsTimesChange(Sender: TObject);
var
  Index: Integer;
  ObsTime: TMt3dmsFluxObservation;
  LocalSelectedObservation: TMt3dmsFluxObservationGroup;
  Grid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit;
begin
  inherited;
  if FSettingTimeCount then
  begin
    Exit;
  end;
  FSettingTimeCount := True;
  try
    LocalSelectedObservation := nil;
    if (FSelectedObservation <> nil) then
    begin
      if SelectedObservation is TMt3dmsFluxObservationGroup then
      begin
        LocalSelectedObservation := TMt3dmsFluxObservationGroup(SelectedObservation);
      end;
    end;
    Grid := rdgConcFluxObsTimes;
    SpinEdit := seNumMt3dmsObsTimes;
    if LocalSelectedObservation <> nil then
    begin
      SetObsNameLength(SpinEdit);
      SetObsGridRowCount(SpinEdit, Grid, btnDeleteMt3dmsFlux);
    end;
    if (not FSettingObservation) and (LocalSelectedObservation <> nil) then
    begin
      Grid.BeginUpdate;
      try
        for Index := 1 to SpinEdit.AsInteger do
        begin
          if Grid.Objects[Ord(fmcName),Index] = nil then
          begin
            ObsTime := LocalSelectedObservation.ObservationTimes.Add;
            Grid.Objects[Ord(fmcName),Index] := ObsTime;
            InitializeNewObs(Index, Ord(fmcTime), Ord(fmcValue), Ord(fmcComment),
              Grid, ObsTime);
            Grid.ItemIndex[Ord(fmcObsType),Index] := 0;

            if Grid.Cells[Ord(fmcWeight),Index] = '' then
            begin
              ObsTime.Weight := 1;
              Grid.Cells[Ord(fmcWeight),Index] := '1';
            end
            else
            begin
              ObsTime.Weight := StrToFloat(Grid.Cells[Ord(fmcWeight),Index]);
            end;
            if Grid.Cells[Ord(fmcSpecies),Index] = '' then
            begin
              Grid.Cells[Ord(fmcSpecies),Index] :=
                Grid.Columns[Ord(fmcSpecies)].PickList[0];
            end;
            ObsTime.Species := Grid.Cells[Ord(fmcSpecies),Index];
          end;
        end;
        for Index := LocalSelectedObservation.ObservationTimes.Count-1
          downto SpinEdit.AsInteger do
        begin
          LocalSelectedObservation.ObservationTimes.Delete(Index);
        end;
        InitializeFirstRow(Grid, SpinEdit);
      finally
        Grid.EndUpdate;
      end;
    end;
    AssignObsNames;
  finally
    FSettingTimeCount := False;
  end;
end;

procedure TfrmManageFluxObservations.seNumObsTimesChange(Sender: TObject);
var
  Index: Integer;
  ObsTime: TFluxObservation;
  LocalSelectedObservation: TFluxObservationGroup;
  Grid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit;
begin
  inherited;
  if FSettingTimeCount then
  begin
    Exit;
  end;
  FSettingTimeCount := True;
  try
    LocalSelectedObservation := nil;
    if (FSelectedObservation <> nil) then
    begin
      if SelectedObservation is TFluxObservationGroup then
      begin
        LocalSelectedObservation := TFluxObservationGroup(SelectedObservation);
      end;
    end;
    Grid := rdgFluxObsTimes;
    SpinEdit := seNumObsTimes;
    if LocalSelectedObservation <> nil then
    begin
      SetObsNameLength(SpinEdit);
      SetObsGridRowCount(SpinEdit, Grid, btnDelete);
    end;

    if (not FSettingObservation) and (LocalSelectedObservation <> nil) then
    begin
      Grid.BeginUpdate;
      try
        for Index := 1 to SpinEdit.AsInteger do
        begin
          if Grid.Objects[Ord(fcName),Index] = nil then
          begin
            ObsTime := LocalSelectedObservation.ObservationTimes.Add;
            Grid.Objects[Ord(fcName),Index] := ObsTime;

            InitializeNewObs(Index, Ord(fcTime), Ord(fcValue), Ord(fcComment),
              Grid, ObsTime);

            if Grid.Cells[Ord(fcStatistic),Index] = '' then
            begin
              ObsTime.Statistic := 0;
              Grid.Cells[Ord(fcStatistic),Index] := '0';
            end
            else
            begin
              ObsTime.Statistic := StrToFloat(Grid.Cells[Ord(fcStatistic),Index]);
            end;
            if Grid.Cells[Ord(fcStatFlag),Index] = '' then
            begin
              ObsTime.StatFlag := Low(TStatFlag);
              Grid.Cells[Ord(fcStatFlag),Index] :=
                Grid.Columns[Ord(fcStatFlag)].PickList[0];
            end
            else
            begin
              ObsTime.StatFlag := TStatFlag(Grid.Columns[Ord(fcStatFlag)].
                PickList.IndexOf(Grid.Cells[Ord(fcStatFlag),Index]));
            end;
          end;
        end;
        for Index := LocalSelectedObservation.ObservationTimes.Count-1
          downto SpinEdit.AsInteger do
        begin
          LocalSelectedObservation.ObservationTimes.Delete(Index);
        end;
        InitializeFirstRow(Grid, SpinEdit);
      finally
        Grid.EndUpdate;
      end;
    end;
    AssignObsNames;
  finally
    FSettingTimeCount := False;
  end;
end;

procedure TfrmManageFluxObservations.SetData;
var
  Undo: TUndoEditFluxObservations;
begin
  Undo := TUndoEditFluxObservations.Create;
  try
    Undo.AssignNewObservations(FChobObservations, FDrobObservations,
      FGbobObservations, FRvobObservations, FStobObservations, FMassFluxObs);
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmManageFluxObservations.SetSelectedObservation(
  const Value: TCustomFluxObservationGroup);
var
  Index: Integer;
  FluxObs: TFluxObservationGroup;
  Mt3dObs: TMt3dmsFluxObservationGroup;
begin
  edFactorFormulaExit(nil);
  if (FSelectedObservation <> Value) or (Value = nil) then
  begin
    FSettingObservation := True;
    try
      FSelectedObservation := Value;
      SrcList.Items.Clear;
      DstList.Items.Clear;
      if FSelectedObservation = nil then
      begin
        edObservationName.Enabled := False;
        comboTreatment.Enabled := False;
        SrcList.Enabled := False;
        DstList.Enabled := False;
        IncBtn.Enabled := False;
        IncAllBtn.Enabled := False;
        ExclBtn.Enabled := False;
        ExclAllBtn.Enabled := False;

        rdgFluxObsTimes.Enabled := False;
        rdgFluxObsTimes.Invalidate;
        seNumObsTimes.Enabled := False;
        btnInsert.Enabled := False;
        btnDelete.Enabled := False;
        rdgFluxObsTimes.Enabled := False;
        for Index := 1 to rdgFluxObsTimes.RowCount - 1 do
        begin
          rdgFluxObsTimes.Objects[Ord(fcName),Index] := nil;
        end;
        seNumObsTimes.AsInteger := 0;
        rdgFluxObsTimes.Invalidate;

        rdgConcFluxObsTimes.Enabled := False;
        rdgConcFluxObsTimes.Invalidate;
        seNumMt3dmsObsTimes.Enabled := False;
        btnInsertMt3dmsFlux.Enabled := False;
        btnDeleteMt3dmsFlux.Enabled := False;
        rdgConcFluxObsTimes.Enabled := False;
        for Index := 1 to rdgConcFluxObsTimes.RowCount - 1 do
        begin
          rdgConcFluxObsTimes.Objects[Ord(fmcName),Index] := nil;
        end;
        seNumMt3dmsObsTimes.AsInteger := 0;
        rdgConcFluxObsTimes.Invalidate;
      end
      else
      begin
        if Value is TFluxObservationGroup then
        begin
          tabObservationsTimes.TabVisible := True;
          if pcMain.ActivePage = tabMassFlux then
          begin
            pcMain.ActivePage := tabObservationsTimes;
          end;
          tabMassFlux.TabVisible := False;
          FluxObs := TFluxObservationGroup(Value);
          SetSelectedFluxObs(FluxObs);
        end;
        if Value is TMt3dmsFluxObservationGroup then
        begin
          tabMassFlux.TabVisible := True;
          if pcMain.ActivePage = tabObservationsTimes then
          begin
            pcMain.ActivePage := tabMassFlux;
          end;
          tabObservationsTimes.TabVisible := False;
          Mt3dObs := TMt3dmsFluxObservationGroup(Value);
          SetSelectedMt3dmsObs(Mt3dObs);
        end;
      end;
    finally
      FSettingObservation := False;
      seNumObsTimesChange(nil);
      seNumMt3dmsObsTimesChange(nil);
    end;
  end;
end;

procedure TfrmManageFluxObservations.IncBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(SrcList, DstList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageFluxObservations.IncAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(SrcList, DstList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageFluxObservations.ExclBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(DstList, SrcList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageFluxObservations.EnableMultiEditControl(Grid: TRbwDataGrid4;
  AControl: TControl; const StartCol, EndCol: integer);
var
  ShouldEnable: Boolean;
  ColIndex: Integer;
  RowIndex: Integer;
  EnableCount: Integer;
begin
  EnableCount := 0;
  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    for ColIndex := StartCol to EndCol do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex, RowIndex);
      if ShouldEnable then
      begin
        Inc(EnableCount);
        if EnableCount >= 2 then
        begin
          break;
        end;
      end;
    end;
  end;
  ShouldEnable := EnableCount >= 2;
  AControl.Enabled := ShouldEnable;
end;

procedure TfrmManageFluxObservations.ExclAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(DstList, SrcList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageFluxObservations.SrcListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(SrcList, Source, X, Y, State, Accept, SrcList.Sorted);
  if State = dsDragLeave then
    (Source as TJvListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TJvListBox).SelCount > 1) then
    (Source as TJvListBox).DragCursor := crMultiDrag;
end;

procedure TfrmManageFluxObservations.DstListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DstList, Source, X, Y, State, Accept, DstList.Sorted);
  if State = dsDragLeave then
    (Source as TJvListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TJvListBox).SelCount > 1) then
    (Source as TJvListBox).DragCursor := crMultiDrag;
end;

procedure TfrmManageFluxObservations.SrcListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if Source = DstList then
    ExclBtnClick(SrcList)
  else
  if Source = SrcList then
  begin
    BoxMoveFocusedItem(SrcList, SrcList.ItemAtPos(Point(X, Y), True));
    UpdateObjectsInSelectedObservation;
  end;
end;

procedure TfrmManageFluxObservations.btnAddObservationClick(Sender: TObject);
var
  Observations: TCustomFluxObservationGroups;
  ObservationGroup: TCustomFluxObservationGroup;
  ANode: TTreeNode;
  ObsName: string;
  ParentNode : TTreeNode;
  NodeList: TList;
  GroupName: string;
begin
  inherited;
  NodeList := TList.Create;
  try
    NodeList.Add(FChobNode);
    NodeList.Add(FGbobNode);
    NodeList.Add(FDrobNode);
    NodeList.Add(FRvobNode);
    NodeList.Add(FStobNode);
    NodeList.Add(FHeadMassFluxNode);
    NodeList.Add(FWellMassFluxNode);
    NodeList.Add(FDrnMassFluxNode);
    NodeList.Add(FRivMassFluxNode);
    NodeList.Add(FGhbMassFluxNode);
    NodeList.Add(FRchMassFluxNode);
    NodeList.Add(FEVTMassFluxNode);
    NodeList.Add(FMassLoadingMassFluxNode);
    NodeList.Add(FResMassFluxNode);
    NodeList.Add(FLakMassFluxNode);
    NodeList.Add(FEtsMassFluxNode);
    NodeList.Add(FDrtMassFluxNode);
    NodeList.Add(FStrMassFluxNode);
    NodeList.Add(FFhbHeadsMassFluxNode);
    NodeList.Add(FFhbFlowsMassFluxNode);
    NodeList.Pack;
    if (tvFluxObservations.Selected = nil) and (NodeList.Count > 0) then
    begin
      tvFluxObservations.Selected := NodeList[0];
    end;
    if tvFluxObservations.Selected = nil then
    begin
      Exit;
    end;
    if NodeList.IndexOf(tvFluxObservations.Selected) >= 0 then
    begin
      ParentNode := tvFluxObservations.Selected;
    end
    else
    begin
      ParentNode := tvFluxObservations.Selected.Parent;
    end;
  finally
    NodeList.Free;
  end;

  GroupName := '';
  if ParentNode = FChobNode then
  begin
    ObsName := 'Chob';
    GroupName := StrCHOBflows;
  end
  else if ParentNode = FGbobNode then
  begin
    ObsName := 'Gbob';
    GroupName := StrGHBflows;
  end
  else if ParentNode = FDrobNode then
  begin
    ObsName := 'Drob';
    GroupName := StrDRNflows;
  end
  else if ParentNode = FRvobNode then
  begin
    ObsName := 'Rvob';
    GroupName := StrRIVflows;
  end
  else if ParentNode = FStobNode then
  begin
    ObsName := 'Stob';
    GroupName := StrSTRflows;
  end
  else if ParentNode = FHeadMassFluxNode then
  begin
    ObsName := 'HMfob';
  end
  else if ParentNode = FWellMassFluxNode then
  begin
    ObsName := 'WMfob';
  end
  else if ParentNode = FDrnMassFluxNode then
  begin
    ObsName := 'DnMfob';
  end
  else if ParentNode = FRivMassFluxNode then
  begin
    ObsName := 'RvMfob';
  end
  else if ParentNode = FGhbMassFluxNode then
  begin
    ObsName := 'GbMfob';
  end
  else if ParentNode = FRchMassFluxNode then
  begin
    ObsName := 'RcMfob';
  end
  else if ParentNode = FEVTMassFluxNode then
  begin
    ObsName := 'EtMfob';
  end
  else if ParentNode = FMassLoadingMassFluxNode then
  begin
    ObsName := 'MlMfob';
  end
  else if ParentNode = FResMassFluxNode then
  begin
    ObsName := 'ReMfob';
  end
  else if ParentNode = FLakMassFluxNode then
  begin
    ObsName := 'LkMfob';
  end
  else if ParentNode = FEtsMassFluxNode then
  begin
    ObsName := 'EsMfob';
  end
  else if ParentNode = FDrtMassFluxNode then
  begin
    ObsName := 'DrMfob';
  end
  else if ParentNode = FStrMassFluxNode then
  begin
    ObsName := 'StMfob';
  end
  else if ParentNode = FFhbHeadsMassFluxNode then
  begin
    ObsName := 'FhbHob';
  end
  else if ParentNode = FFhbFlowsMassFluxNode then
  begin
    ObsName := 'FhbFob';
  end
  else
  begin
    Assert(False);
  end;

  Observations := ParentNode.Data;
  ObservationGroup := Observations.Add as TCustomFluxObservationGroup;
  ObservationGroup.ObservationGroup := GroupName;
  ObservationGroup.ObservationName := ObsName
    + IntToStr(ParentNode.Count+1);
  ANode := tvFluxObservations.Items.AddChild(ParentNode,
    ObservationGroup.ObservationName);
  ANode.Data := ObservationGroup;
  tvFluxObservations.Selected := ANode;
  SetSelectedGroupAndObservation(tvFluxObservations);
end;

procedure TfrmManageFluxObservations.btnDeleteClick(Sender: TObject);
var
  LocalSelectedObservation: TFluxObservationGroup;
begin
  inherited;
  LocalSelectedObservation := SelectedObservation as TFluxObservationGroup;
  if (rdgFluxObsTimes.SelectedRow >= 1)
    and (rdgFluxObsTimes.SelectedRow <= rdgFluxObsTimes.RowCount)then
  begin
    if seNumObsTimes.AsInteger > 1 then
    begin
      LocalSelectedObservation.ObservationTimes.
        Delete(rdgFluxObsTimes.SelectedRow-1);
      rdgFluxObsTimes.DeleteRow(rdgFluxObsTimes.SelectedRow);
    end
    else
    begin
      if rdgFluxObsTimes.Objects[Ord(fcName), rdgFluxObsTimes.SelectedRow] <> nil then
      begin
        LocalSelectedObservation.ObservationTimes.
          Delete(rdgFluxObsTimes.SelectedRow-1);
        rdgFluxObsTimes.Objects[Ord(fcName), rdgFluxObsTimes.SelectedRow] := nil;
      end;
    end;
    seNumObsTimes.AsInteger := seNumObsTimes.AsInteger -1;
    AssignObsNames;
  end;
end;

procedure TfrmManageFluxObservations.btnDeleteMt3dmsFluxClick(Sender: TObject);
var
  LocalSelectedObservation: TMt3dmsFluxObservationGroup;
begin
  inherited;
  LocalSelectedObservation := SelectedObservation as TMt3dmsFluxObservationGroup;
  if (rdgConcFluxObsTimes.SelectedRow >= 1)
    and (rdgConcFluxObsTimes.SelectedRow <= rdgConcFluxObsTimes.RowCount)then
  begin
    if seNumMt3dmsObsTimes.AsInteger > 1 then
    begin
      LocalSelectedObservation.ObservationTimes.
        Delete(rdgConcFluxObsTimes.SelectedRow-1);
      rdgConcFluxObsTimes.DeleteRow(rdgConcFluxObsTimes.SelectedRow);
    end
    else
    begin
      if rdgConcFluxObsTimes.Objects[Ord(fmcName), rdgConcFluxObsTimes.SelectedRow] <> nil then
      begin
        LocalSelectedObservation.ObservationTimes.
          Delete(rdgConcFluxObsTimes.SelectedRow-1);
        rdgConcFluxObsTimes.Objects[Ord(fmcName), rdgConcFluxObsTimes.SelectedRow] := nil;
      end;
    end;
    seNumMt3dmsObsTimes.AsInteger := seNumMt3dmsObsTimes.AsInteger -1;
    AssignObsNames;
  end;
end;

procedure TfrmManageFluxObservations.btnDeleteObservationClick(Sender: TObject);
var
  ParentNode: TTreeNode;
  Observations: TCustomFluxObservationGroups;
  Item: TCustomFluxObservationGroup;
  Index: Integer;
  AnObject: TObject;
begin
  inherited;
  Assert(tvFluxObservations.Selected <> nil);
  AnObject := tvFluxObservations.Selected.Data;
  Assert(AnObject is TCustomFluxObservationGroup);
  Item := tvFluxObservations.Selected.Data;
  ParentNode := tvFluxObservations.Selected.Parent;
  Assert(ParentNode <> nil);
  Observations := ParentNode.Data;
  if Observations is TCustomFluxObservationGroups then
  begin
    for Index := 1 to rdgFluxObsTimes.RowCount - 1 do
    begin
      rdgFluxObsTimes.Objects[Ord(fcName),Index] := nil;
    end;
  end
  else
  begin

  end;
  tvFluxObservations.Items.Delete(tvFluxObservations.Selected);
  SelectedGroup := nil;
  SelectedObservation := nil;
  Observations.Remove(Item);
end;

procedure TfrmManageFluxObservations.btnFactorFormulaClick(Sender: TObject);
var
  FirstScreenObject: TScreenObject;
  Index: Integer;
  Variable: TCustomValue;
  FunctionString: string;
  ObjectIndex: Integer;
  FactorObject: TObservationFactor;
begin
  inherited;
  FirstScreenObject := nil;
  for Index := 0 to DstList.Items.Count - 1 do
  begin
    if DstList.Selected[Index] then
    begin
      FirstScreenObject := DstList.Items.Objects[Index] as TScreenObject;
      break;
    end;
  end;
  if FirstScreenObject = nil then
  begin
    Exit;
  end;

  FunctionString := edFactorFormula.Text;
  if FunctionString = '' then
  begin
    ObjectIndex := FSelectedObservation.ObservationFactors.
      IndexOfScreenObject(FirstScreenObject);
    Assert(ObjectIndex >= 0);
    FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
    FunctionString := FactorObject.Factor;
  end;

//  with TfrmFormula.Create(nil) do
  with frmFormula do
  begin
    try
      Initialize;
      IncludeGIS_Functions(eaBlocks);
      RemoveGetVCont;
      RemoveHufFunctions;
      PopupParent := self;

      for Index := 0 to rparserThreeDFormulaElements.VariableCount - 1 do
      begin
        Variable := rparserThreeDFormulaElements.Variables[Index];
        if rbFormulaParser.IndexOfVariable(Variable.Name) < 0 then
        begin
          rbFormulaParser.RegisterVariable(Variable);
        end;
      end;

      IncludeTimeSeries := False;
      UpdateTreeList;
      Formula := FunctionString;

      ShowModal;
      if ResultSet then
      begin
        FunctionString := Formula;
      end
      else
      begin
        if FunctionString = '' then
          FunctionString := '0';
      end;
    finally
      Initialize;
//      Free;
    end;
  end;

  CheckFormula(FunctionString, True)
end;

procedure TfrmManageFluxObservations.AssignObsNames;
var
  Index: Integer;
  MaxStringLength: Integer;
  TimeString: string;
  SpinEdit: TJvSpinEdit;
  Grid: TRbwDataGrid4;
  NameColumn: Integer;
begin
  if FSelectedObservation is TFluxObservationGroup then
  begin
    SpinEdit := seNumObsTimes;
    Grid := rdgFluxObsTimes;
    NameColumn := Ord(fcName);
  end
  else
  begin
    SpinEdit := seNumMt3dmsObsTimes;
    Grid := rdgConcFluxObsTimes;
    NameColumn := Ord(fmcName);
  end;
  MaxStringLength := Length(IntToStr(SpinEdit.AsInteger));
  Grid.BeginUpdate;
  try
    for Index := 1 to SpinEdit.AsInteger do
    begin
      TimeString := IntToStr(Index);
      While Length(TimeString) < MaxStringLength do
      begin
        TimeString := '0' + TimeString;
      end;
      Grid.Cells[NameColumn, Index] :=
        edObservationName.Text + '_' + TimeString;
    end;
    if SpinEdit.AsInteger = 0 then
    begin
      Grid.Cells[NameColumn, 1] := '';
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TfrmManageFluxObservations.AssignValuesToSelectedGridCells(
  const NewText: string; Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempText: string;
begin
  for ColIndex := StartCol to EndCol do
  begin
    if Grid.Columns[ColIndex].Format = rcf4Integer then
    begin
      TempText := IntToStr(Round(StrToFloat(NewText)));
    end
    else
    begin
      TempText := NewText;
    end;
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      if Grid.IsSelectedCell(ColIndex, RowIndex) then
      begin
        Grid.Cells[ColIndex, RowIndex] := TempText;
        if Assigned(Grid.OnSetEditText) then
        begin
          Grid.OnSetEditText(Grid, ColIndex, RowIndex, TempText);
        end;
      end;
    end;
  end;
  if Assigned(Grid.OnExit) then
  begin
    Grid.OnExit(Grid);
  end;
end;

procedure TfrmManageFluxObservations.btnInsertClick(Sender: TObject);
begin
  inherited;
  if (rdgFluxObsTimes.SelectedRow >= 1)
    and (rdgFluxObsTimes.SelectedRow <= rdgFluxObsTimes.RowCount)then
  begin
    rdgFluxObsTimes.InsertRow(rdgFluxObsTimes.SelectedRow);
    seNumObsTimes.AsInteger := seNumObsTimes.AsInteger + 1;
  end;
  seNumObsTimesChange(Sender);
end;

procedure TfrmManageFluxObservations.btnInsertMt3dmsFluxClick(Sender: TObject);
begin
  inherited;
  if (rdgConcFluxObsTimes.SelectedRow >= 1)
    and (rdgConcFluxObsTimes.SelectedRow <= rdgConcFluxObsTimes.RowCount)then
  begin
    rdgConcFluxObsTimes.InsertRow(rdgConcFluxObsTimes.SelectedRow);
    seNumMt3dmsObsTimes.AsInteger := seNumMt3dmsObsTimes.AsInteger + 1;
  end;
  seNumMt3dmsObsTimesChange(Sender);
end;

procedure TfrmManageFluxObservations.DisplayFactor;
var
  FirstFormula: string;
  FoundFormula: boolean;
  Index: Integer;
  ScreenObject: TScreenObject;
  ObjectIndex: integer;
  FactorObject: TObservationFactor;
begin
  edFactorFormula.Enabled := DstList.Enabled and (DstList.SelCount > 0);
  btnFactorFormula.Enabled := edFactorFormula.Enabled;
  if edFactorFormula.Enabled then
  begin
    FoundFormula := False;
    FirstFormula := '';
    for Index := 0 to DstList.Items.Count - 1 do
    begin
      if DstList.Selected[Index] then
      begin
        ScreenObject := DstList.Items.Objects[Index] as TScreenObject;
        ObjectIndex := FSelectedObservation.ObservationFactors.
          IndexOfScreenObject(ScreenObject);
        Assert(ObjectIndex >= 0);
        FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
        if FoundFormula then
        begin
          if FirstFormula <> FactorObject.Factor then
          begin
            FirstFormula := '';
          end;
        end
        else
        begin
          FirstFormula := FactorObject.Factor;
          FoundFormula := True;
        end;
      end;
    end;
    edFactorFormula.Text := FirstFormula;
  end;
end;

procedure TfrmManageFluxObservations.DstListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if Source = SrcList then
    IncBtnClick(DstList)
  else
  if Source = DstList then
  begin
    BoxMoveFocusedItem(DstList, DstList.ItemAtPos(Point(X, Y), True));
    UpdateObjectsInSelectedObservation;
  end;
end;

procedure TfrmManageFluxObservations.SrcListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Incr: Integer;
begin
  if not SrcList.Sorted then
  begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
    begin
      if Key = VK_DOWN then
        Incr := 1
      else
        Incr := -1;
      BoxMoveFocusedItem(SrcList, SrcList.ItemIndex + Incr);
      Key := 0;
      UpdateObjectsInSelectedObservation;
    end;
  end;
end;

procedure TfrmManageFluxObservations.tabObservationsTimesResize(
  Sender: TObject);
begin
  inherited;
  LayoutMultiFluxEdits;
end;

procedure TfrmManageFluxObservations.tvFluxObservationsChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  SetSelectedGroupAndObservation(tvFluxObservations);
end;

procedure TfrmManageFluxObservations.UpdateObjectsInSelectedObservation;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  DestinationList: TList;
begin
  DestinationList := TList.Create;
  try
    for Index := 0 to DstList.Items.Count - 1 do
    begin
      DestinationList.Add(DstList.Items.Objects[Index]);
    end;

    for Index := FSelectedObservation.ObservationFactors.Count - 1 downto 0 do
    begin
      ScreenObject := FSelectedObservation.
        ObservationFactors.Items[Index].ScreenObject as TScreenObject;
      if DestinationList.IndexOf(ScreenObject) < 0 then
      begin
        FSelectedObservation.ObservationFactors.Delete(Index);
      end;
    end;

    for Index := 0 to DestinationList.Count - 1 do
    begin
      ScreenObject := DestinationList[Index];
      FSelectedObservation.AddObject(ScreenObject);
    end;
  finally
    DestinationList.Free;
  end;
  DisplayFactor;
end;

procedure TfrmManageFluxObservations.DstListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Incr: Integer;
begin
  if not DstList.Sorted then
  begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
    begin
      if Key = VK_DOWN then
        Incr := 1
      else
        Incr := -1;
      BoxMoveFocusedItem(DstList, DstList.ItemIndex + Incr);
      UpdateObjectsInSelectedObservation;
      Key := 0;
    end;
  end;
end;

procedure TfrmManageFluxObservations.UpdateFactor;
var
  NewFormula: string;
begin
  if FUpdatingFormula then Exit;
  FUpdatingFormula := True;
  try
    NewFormula := edFactorFormula.Text;
    if (NewFormula = '') or not CheckFormula(NewFormula, False) then
    begin
      Exit;
    end;
    NewFormula := edFactorFormula.Text;
    AssignFactor(NewFormula);
  finally
    FUpdatingFormula := False;
  end;
end;

procedure TfrmManageFluxObservations.edFactorFormulaChange(Sender: TObject);
begin
  inherited;
  UpdateFactor;
end;

procedure TfrmManageFluxObservations.edFactorFormulaExit(Sender: TObject);
var
  NewFormula: string;
begin
  if FUpdatingFormula then Exit;
  FUpdatingFormula := True;
  try
    NewFormula := edFactorFormula.Text;
    if (NewFormula = '') or not CheckFormula(NewFormula, True) then
    begin
      Exit;
    end;
    NewFormula := edFactorFormula.Text;
    AssignFactor(NewFormula);
  finally
    FUpdatingFormula := False;
  end;
end;

procedure TfrmManageFluxObservations.edObservationNameChange(Sender: TObject);
begin
  inherited;
  if (FSelectedObservation <> nil) then
  begin
    Assert(tvFluxObservations.Selected.Data = FSelectedObservation);
    FSelectedObservation.ObservationName := string(AnsiString(edObservationName.Text));
    tvFluxObservations.Selected.Text := edObservationName.Text;
    AssignObsNames;
    rdgGroupNames.Cells[1, FSelectedObservation.Index+1] :=
      SelectedObservation.ObservationName;
  end;
end;

procedure TfrmManageFluxObservations.edObservationNameExit(Sender: TObject);
begin
  inherited;
  edObservationName.Text := string(AnsiString(StringReplace(edObservationName.Text,
    ' ', '_', [rfReplaceAll])));
end;

procedure TfrmManageFluxObservations.CreateVariables;
var
  Index: Integer;
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataArray := DataArrayManager.DataSets[Index];
    if not DataArray.Visible then
    begin
      Continue;
    end;
    if DataArray.EvaluatedAt = eaBlocks then
    begin
      case DataArray.DataType of
        rdtDouble: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, 0.0, DataArray.DisplayName);
        rdtInteger: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, 0, DataArray.DisplayName);
        rdtBoolean: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, False, DataArray.DisplayName);
        rdtString: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, '', DataArray.DisplayName);
      end;
    end;
  end;
end;

procedure TfrmManageFluxObservations.FormCreate(Sender: TObject);
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Mt3dmsIsSelected
    and (PhastModel.MobileComponents.Count = 0)
    and (PhastModel.ImmobileComponents.Count = 0) then
  begin
    Beep;
    MessageDlg(StrYouMustDefineAtL, mtError, [mbOK], 0);
    ModalResult := mrCancel;
    Exit;
  end;
  FPriorErrors.Sorted := True;
  FPriorErrors.Duplicates := dupIgnore;

  FChobObservations := TFluxObservationGroups.Create(nil);
  FChobObservations.FluxObservationType := fotHead;
  FChdScreenObjects := TList.Create;

  FDrobObservations := TFluxObservationGroups.Create(nil);
  FDrobObservations.FluxObservationType := fotDrain;
  FDrnScreenObjects := TList.Create;

  FGbobObservations := TFluxObservationGroups.Create(nil);
  FGbobObservations.FluxObservationType := fotGHB;
  FGhbScreenObjects := TList.Create;

  FRvobObservations := TFluxObservationGroups.Create(nil);
  FRvobObservations.FluxObservationType := fotRiver;
  FRivScreenObjects := TList.Create;

  FStobObservations := TFluxObservationGroups.Create(nil);
  FStobObservations.FluxObservationType := fotSTR;
  FStrScreenObjects := TList.Create;

  FMassFluxObs.CreateAll;
//  FMassFluxObs.Mt3dmsHeadMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FHeadMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsWellMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FWellMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsDrnMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FDrnMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsRivMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FRivMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsGhbMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FGhbMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsRchMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FRchMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsEvtMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FEvtMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsMassLoadingMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FMassLoadingMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsResMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FResMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsLakMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FLakMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsDrtMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FDrtMassFluxObservations := TList.Create;

//  FMassFluxObs.Mt3dmsEtsMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  FEtsMassFluxObservations := TList.Create;

  FStrMassFluxObservations := TList.Create;

  FFhbHeadsMassFluxObservations := TList.Create;
  FFhbFlowsMassFluxObservations := TList.Create;

  rdgFluxObsTimes.Cells[Ord(fcName),0] := StrName;
  rdgFluxObsTimes.Cells[Ord(fcTime),0] := StrTime;
  rdgFluxObsTimes.Cells[Ord(fcValue),0] := StrObservedValue;
  rdgFluxObsTimes.Cells[Ord(fcStatistic),0] := StrStatistic;
  rdgFluxObsTimes.Cells[Ord(fcStatFlag),0] := StrStatFlag;
  rdgFluxObsTimes.Cells[Ord(fcComment),0] := StrComment;


  rdgConcFluxObsTimes.Cells[Ord(fmcName),0] := StrName;
  rdgConcFluxObsTimes.Cells[Ord(fmcSpecies),0] := StrComponent;
  rdgConcFluxObsTimes.Cells[Ord(fmcObsType),0] := StrObservationType;
  rdgConcFluxObsTimes.Cells[Ord(fmcTime),0] := StrObservationTimeOr;
  rdgConcFluxObsTimes.Cells[Ord(fmcValue),0] := StrObservedValue;
  rdgConcFluxObsTimes.Cells[Ord(fmcWeight),0] := StrWeight;
  rdgConcFluxObsTimes.Cells[Ord(fmcComment),0] := StrComment;

  rdgGroupNames.Cells[0,0] := 'N';
  rdgGroupNames.Cells[1,0] := 'Group Name';

  LayoutMultiFluxEdits;
  LayoutMt3dmsMultiFluxEdits;

  AddGIS_Functions(rparserThreeDFormulaElements,
    frmGoPhast.PhastModel.ModelSelection, eaBlocks);
  GetGlobalVariables;
  CreateVariables;

  pcGroup.ActivePageIndex := 0;

  GetData;
end;

procedure TfrmManageFluxObservations.FormDestroy(Sender: TObject);
begin
  inherited;
  FChdScreenObjects.Free;
  FChobObservations.Free;
  FDrnScreenObjects.Free;
  FDrobObservations.Free;
  FGhbScreenObjects.Free;
  FGbobObservations.Free;
  FRivScreenObjects.Free;
  FRvobObservations.Free;
  FStrScreenObjects.Free;
  FStobObservations.Free;
  FHeadMassFluxObservations.Free;
  FWellMassFluxObservations.Free;
  FDrnMassFluxObservations.Free;
  FRivMassFluxObservations.Free;
  FGhbMassFluxObservations.Free;
  FRchMassFluxObservations.Free;
  FEvtMassFluxObservations.Free;
  FMassLoadingMassFluxObservations.Free;
  FResMassFluxObservations.Free;
  FLakMassFluxObservations.Free;
  FDrtMassFluxObservations.Free;
  FEtsMassFluxObservations.Free;
  FStrMassFluxObservations.Free;
  FFhbHeadsMassFluxObservations.Free;
  FFhbFlowsMassFluxObservations.Free;
  FMassFluxObs.FreeAll;
//  FMassFluxObs.Mt3dmsHeadMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsWellMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsMassLoadingMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsGhbMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsRivMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsResMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsRchMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsDrtMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsEtsMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsEvtMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsDrnMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsLakMassFluxObservations.Free;
//  FMassFluxObs.Mt3dmsStrMassFluxObservations.Free;

end;

procedure TfrmManageFluxObservations.FormResize(Sender: TObject);
begin
  { Delphi 5, 6, 7 and 2005 compatible code }
  IncBtn.Left := 4 + (tabObjects.Width - IncBtn.Width) div 2;
  IncAllBtn.Left := IncBtn.Left;
  ExclBtn.Left := IncBtn.Left;
  ExclAllBtn.Left := IncBtn.Left;
  SrcList.Width := (tabObjects.Width - (8 + 7 + IncBtn.Width + 7 + 8)) div 2;
  SrcLabel.Left := SrcList.Left;
  DstList.Width := SrcList.Width;
  DstList.Left := IncBtn.Left + IncBtn.Width + 7;
  DstLabel.Left := DstList.Left;
end;

procedure TfrmManageFluxObservations.FormShow(Sender: TObject);
begin
  inherited;
  ListClick(nil);
  rdgFluxObsTimes.Options := rdgFluxObsTimes.Options - [goEditing];
  rdgFluxObsTimes.Options := rdgFluxObsTimes.Options + [goEditing];
  pcMain.ActivePageIndex := 0;

//  rdgFluxObsTimes.hi
end;

procedure TfrmManageFluxObservations.GetData;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  Column: TRbwColumn4;
  Mt3dColumn: TRbwColumn4;
  ChemItem: TChemSpeciesItem;
  SpeciesIndex: Integer;
  ModelSelection: TModelSelection;
//  CalibrationObservations: TMf6CalibrationObservations;
begin
  ModelSelection := frmGoPhast.ModelSelection;
  Mt3dColumn := rdgConcFluxObsTimes.Columns[Ord(fmcSpecies)];
  Mt3dColumn.PickList.Clear;
  for SpeciesIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
  begin
    ChemItem := frmGoPhast.PhastModel.MobileComponents[SpeciesIndex];
    Mt3dColumn.PickList.Add(ChemItem.Name)
  end;
  for SpeciesIndex := 0 to frmGoPhast.PhastModel.ImmobileComponents.Count - 1 do
  begin
    ChemItem := frmGoPhast.PhastModel.ImmobileComponents[SpeciesIndex];
    Mt3dColumn.PickList.Add(ChemItem.Name)
  end;
  comboMt3dmsSpecies.Items.Assign(Mt3dColumn.PickList);

  Column := rdgFluxObsTimes.Columns[Ord(fcStatFlag)];
  Assert(comboMultiStatFlag.Items.Count = Column.PickList.Count);
  for Index := 0 to Column.PickList.Count - 1 do
  begin
    comboMultiStatFlag.Items[Index].Text := Column.PickList[Index];
  end;

  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if (ScreenObject.ModflowChdBoundary <> nil)
      and ScreenObject.ModflowChdBoundary.Used then
    begin
      FChdScreenObjects.Add(ScreenObject);
      FHeadMassFluxObservations.Add(ScreenObject);
    end
    else if (ModelSelection = msModflow2015)
      and (ScreenObject.Modflow6Obs <> nil)
      and (ogCHD in ScreenObject.Modflow6Obs.General) then
    begin
      FChdScreenObjects.Add(ScreenObject);
    end;

    if (ScreenObject.ModflowDrnBoundary <> nil)
      and ScreenObject.ModflowDrnBoundary.Used then
    begin
      FDrnScreenObjects.Add(ScreenObject);
      FDrnMassFluxObservations.Add(ScreenObject);
    end
    else if (ModelSelection = msModflow2015)
      and (ScreenObject.Modflow6Obs <> nil)
      and (ogDrain in ScreenObject.Modflow6Obs.General) then
    begin
      FDrnScreenObjects.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowGhbBoundary <> nil)
      and ScreenObject.ModflowGhbBoundary.Used then
    begin
      FGhbScreenObjects.Add(ScreenObject);
      FGhbMassFluxObservations.Add(ScreenObject);
    end
    else if (ModelSelection = msModflow2015)
      and (ScreenObject.Modflow6Obs <> nil)
      and (ogGHB in ScreenObject.Modflow6Obs.General) then
    begin
      FGhbScreenObjects.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowRivBoundary <> nil)
      and ScreenObject.ModflowRivBoundary.Used then
    begin
      FRivScreenObjects.Add(ScreenObject);
      FRivMassFluxObservations.Add(ScreenObject);
    end
    else if (ModelSelection = msModflow2015)
      and (ScreenObject.Modflow6Obs <> nil)
      and (ogRiv in ScreenObject.Modflow6Obs.General) then
    begin
      FRivScreenObjects.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowStrBoundary <> nil)
      and ScreenObject.ModflowStrBoundary.Used then
    begin
      FStrScreenObjects.Add(ScreenObject);
      FStrMassFluxObservations.Add(ScreenObject);
    end;

    if (ScreenObject.ModflowWellBoundary <> nil)
      and ScreenObject.ModflowWellBoundary.Used then
    begin
      FWellMassFluxObservations.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowDrtBoundary <> nil)
      and ScreenObject.ModflowDrtBoundary.Used then
    begin
      FDrtMassFluxObservations.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowRchBoundary <> nil)
      and ScreenObject.ModflowRchBoundary.Used then
    begin
      FRchMassFluxObservations.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowEvtBoundary <> nil)
      and ScreenObject.ModflowEvtBoundary.Used then
    begin
      FEvtMassFluxObservations.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowEtsBoundary <> nil)
      and ScreenObject.ModflowEtsBoundary.Used then
    begin
      FEtsMassFluxObservations.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowResBoundary <> nil)
      and ScreenObject.ModflowResBoundary.Used then
    begin
      FResMassFluxObservations.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowLakBoundary <> nil)
      and ScreenObject.ModflowLakBoundary.Used then
    begin
      FLakMassFluxObservations.Add(ScreenObject);
    end;
//    if (ScreenObject.ModflowStrBoundary <> nil)
//      and ScreenObject.ModflowStrBoundary.Used then
//    begin
//      FStrMassFluxObservations.Add(ScreenObject);
//    end;
    if ((ScreenObject.ModflowFhbHeadBoundary <> nil)
      and ScreenObject.ModflowFhbHeadBoundary.Used) then
    begin
      FFhbHeadsMassFluxObservations.Add(ScreenObject);
    end;
    if ((ScreenObject.ModflowFhbFlowBoundary <> nil)
      and ScreenObject.ModflowFhbFlowBoundary.Used) then
    begin
      FFhbFlowsMassFluxObservations.Add(ScreenObject);
    end;
    if (ScreenObject.Mt3dmsConcBoundary <> nil)
      and ScreenObject.Mt3dmsConcBoundary.Used
      and ScreenObject.Mt3dmsConcBoundary.MassLoadingBoundary then
    begin
      FMassLoadingMassFluxObservations.Add(ScreenObject);
    end;
  end;

  FChobObservations.Assign(frmGoPhast.PhastModel.HeadFluxObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015)
    or frmGoPhast.PhastModel.PestUsed then
  begin
    ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.ChobPackage,
      FChobObservations, FChobNode);
  end;

  FDrobObservations.Assign(frmGoPhast.PhastModel.DrainObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015)
    or frmGoPhast.PhastModel.PestUsed then
  begin
    ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.DrobPackage,
      FDrobObservations, FDrobNode);
  end;

  FGbobObservations.Assign(frmGoPhast.PhastModel.GhbObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015)
    or frmGoPhast.PhastModel.PestUsed then
  begin
    ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.GbobPackage,
      FGbobObservations, FGbobNode);
  end;

  FRvobObservations.Assign(frmGoPhast.PhastModel.RiverObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015)
    or frmGoPhast.PhastModel.PestUsed then
  begin
    ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.RvobPackage,
      FRvobObservations, FRvobNode);
  end;

  FStobObservations.Assign(frmGoPhast.PhastModel.StreamObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.StobPackage,
      FStobObservations, FStobNode);
  end;

  FMassFluxObs.Mt3dmsHeadMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsHeadMassFluxObservations);
  ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.ChdBoundary,
    StrSpecifiedHeadMass, FMassFluxObs.Mt3dmsHeadMassFluxObservations, FHeadMassFluxNode);

  FMassFluxObs.Mt3dmsWellMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsWellMassFluxObservations);
  ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.WelPackage,
    StrWellMassFlux, FMassFluxObs.Mt3dmsWellMassFluxObservations, FWellMassFluxNode);

  FMassFluxObs.Mt3dmsDrnMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsDrnMassFluxObservations);
  ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.DrnPackage,
    StrDrainMassFlux, FMassFluxObs.Mt3dmsDrnMassFluxObservations, FDrnMassFluxNode);

  FMassFluxObs.Mt3dmsRivMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsRivMassFluxObservations);
  ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.RivPackage,
    StrRiverMassFlux, FMassFluxObs.Mt3dmsRivMassFluxObservations, FRivMassFluxNode);

  FMassFluxObs.Mt3dmsGhbMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsGhbMassFluxObservations);
  ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.GhbBoundary,
    StrGHBMassFlux, FMassFluxObs.Mt3dmsGhbMassFluxObservations, FGhbMassFluxNode);

  FMassFluxObs.Mt3dmsRchMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsRchMassFluxObservations);
  ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.RchPackage,
    StrRechargeMassFlux, FMassFluxObs.Mt3dmsRchMassFluxObservations, FRchMassFluxNode);

  FMassFluxObs.Mt3dmsEvtMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsEvtMassFluxObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.EvtPackage,
      StrEVTMassFlux, FMassFluxObs.Mt3dmsEvtMassFluxObservations, FEVTMassFluxNode);
  end;

  FMassFluxObs.Mt3dmsMassLoadingMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsMassLoadingMassFluxObservations);
  ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.Mt3dmsSourceSink,
    StrMassLoading, FMassFluxObs.Mt3dmsMassLoadingMassFluxObservations, FMassLoadingMassFluxNode);

  FMassFluxObs.Mt3dmsResMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsResMassFluxObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.ResPackage,
      StrResevoirMassFlux, FMassFluxObs.Mt3dmsResMassFluxObservations, FResMassFluxNode);
  end;

  FMassFluxObs.Mt3dmsLakMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsLakMassFluxObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.LakPackage,
      StrLakeMassFlux, FMassFluxObs.Mt3dmsLakMassFluxObservations, FLakMassFluxNode);
  end;

  FMassFluxObs.Mt3dmsDrtMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsDrtMassFluxObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.DrtPackage,
      StrDRTMassFlux, FMassFluxObs.Mt3dmsDrtMassFluxObservations, FDrtMassFluxNode);
  end;

  FMassFluxObs.Mt3dmsStrMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsStrMassFluxObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.StrPackage,
      StrSTRMassFlux, FMassFluxObs.Mt3dmsStrMassFluxObservations, FStrMassFluxNode);
  end;

  FMassFluxObs.Mt3dmsFhbHeadMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsFhbHeadMassFluxObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.FhbPackage,
      StrFHBHeadMassFlux, FMassFluxObs.Mt3dmsFhbHeadMassFluxObservations, FFhbHeadsMassFluxNode);
  end;

  FMassFluxObs.Mt3dmsFhbFlowMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsFhbFlowMassFluxObservations);
  if (frmGoPhast.ModelSelection <> msModflow2015) then
  begin
    ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.FhbPackage,
      StrFHBFlowMassFlux, FMassFluxObs.Mt3dmsFhbFlowMassFluxObservations, FFhbFlowsMassFluxNode);
  end;

  FMassFluxObs.Mt3dmsEtsMassFluxObservations.Assign(
    frmGoPhast.PhastModel.Mt3dmsEtsMassFluxObservations);
  ReadMassFluxObservations(frmGoPhast.PhastModel.ModflowPackages.EtsPackage,
    StrETSMassFlux, FMassFluxObs.Mt3dmsEtsMassFluxObservations, FEtsMassFluxNode);

  SelectedObservation := nil;
end;

procedure TfrmManageFluxObservations.GetGlobalVariables;
var
  CompilerList: TList;
begin
  CompilerList := TList.Create;
  try
    CompilerList.Add(rparserThreeDFormulaElements);
    frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
  finally
    CompilerList.Free;
  end;
end;

procedure TfrmManageFluxObservations.ListClick(Sender: TObject);
begin
  SetButtons;
  DisplayFactor;
end;

procedure TfrmManageFluxObservations.miEditAvailableClick(Sender: TObject);
//var
//  ListBox: TJvListBox;
begin
  inherited;
  EditAnObject(SrcList);

end;

procedure TfrmManageFluxObservations.miEditUsedClick(Sender: TObject);
begin
  inherited;
  EditAnObject(DstList);
end;

procedure TfrmManageFluxObservations.miGotoAvailableClick(Sender: TObject);
//var
//  ListBox: TJvListBox;
begin
  inherited;
  GoToAnObject(SrcList);
end;

procedure TfrmManageFluxObservations.miGoToUsedClick(Sender: TObject);
begin
  inherited;
  GoToAnObject(DstList);
end;

procedure TfrmManageFluxObservations.miHideAvailableClick(Sender: TObject);
begin
  inherited;
  HideObjects(SrcList);
end;

procedure TfrmManageFluxObservations.miHideUsedClick(Sender: TObject);
begin
  inherited;
  HideObjects(DstList);
end;

procedure TfrmManageFluxObservations.miSelectAvailableClick(Sender: TObject);
//var
//  ListBox: TJvListBox;
begin
  inherited;
  SelectObjects(SrcList);

end;

procedure TfrmManageFluxObservations.miSelectUsedClick(Sender: TObject);
begin
  inherited;
  SelectObjects(DstList);

end;

procedure TfrmManageFluxObservations.OkBtnClick(Sender: TObject);
begin
  inherited;
  CheckErrors;
  SetData;
end;

procedure TfrmManageFluxObservations.pmSelectEditAvailablePopup(
  Sender: TObject);
begin
  inherited;
  miSelectAvailable.Enabled := SrcList.SelCount > 0;
  miEditAvailable.Enabled := SrcList.SelCount = 1;
  miGotoAvailable.Enabled := SrcList.SelCount = 1;
  miHideAvailable.Enabled := SrcList.SelCount > 0;
end;

procedure TfrmManageFluxObservations.pmSelectEditUsedPopup(Sender: TObject);
begin
  inherited;
  miSelectUsed.Enabled := DstList.SelCount > 0;
  miEditUsed.Enabled := DstList.SelCount = 1;
  miGotoUsed.Enabled := DstList.SelCount = 1;
  miHideUsed.Enabled := DstList.SelCount > 0;
end;

procedure TfrmManageFluxObservations.rdeMassFluxMultiValueEditChange(
  Sender: TObject);
begin
  inherited;
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  AssignValuesToSelectedGridCells(rdeMassFluxMultiValueEdit.Text,
    rdgConcFluxObsTimes, Ord(fmcTime), Ord(fmcWeight));
end;

procedure TfrmManageFluxObservations.rdeMultiValueEditChange(Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(rdeMultiValueEdit.Text, rdgFluxObsTimes,
    Ord(fcTime), Ord(fcStatistic));
end;

procedure TfrmManageFluxObservations.rdgConcFluxObsTimesBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  if not rdgConcFluxObsTimes.Enabled then
  begin
    rdgConcFluxObsTimes.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TfrmManageFluxObservations.rdgConcFluxObsTimesColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  inherited;
  LayoutMt3dmsFluxEdits;
end;

procedure TfrmManageFluxObservations.rdgConcFluxObsTimesEndUpdate(
  Sender: TObject);
begin
  inherited;
  if (not FSettingTimeCount) and (seNumMt3dmsObsTimes <> nil) then
  begin
    seNumMt3dmsObsTimes.AsInteger := rdgConcFluxObsTimes.RowCount -1;
  end;
end;

procedure TfrmManageFluxObservations.rdgConcFluxObsTimesExit(Sender: TObject);
var
  Index: Integer;
  ObsTime: TMt3dmsFluxObservation;
  AValue: Extended;
  AnInt: Integer;
begin
  inherited;

  for Index := 1 to seNumMt3dmsObsTimes.AsInteger do
  begin
    if Index < rdgConcFluxObsTimes.RowCount then
    begin
      ObsTime := rdgConcFluxObsTimes.Objects[Ord(fcName),Index] as TMt3dmsFluxObservation;
      if ObsTime <> nil then
      begin
        ObsTime.ObservationType := TObservationType(rdgConcFluxObsTimes.ItemIndex[Ord(fmcObsType),Index]);
        case ObsTime.ObservationType of
          otTime:
            begin
              if TryStrToFloat(rdgConcFluxObsTimes.Cells[Ord(fmcTime),Index], AValue) then
              begin
                ObsTime.Time := AValue;
              end;
            end;
          otFrequency:
            begin
              if TryStrToInt(rdgConcFluxObsTimes.Cells[Ord(fmcTime),Index], AnInt) then
              begin
                ObsTime.ObservationFrequency := AnInt;
              end;
            end;
          else
            Assert(false);
        end;
        if TryStrToFloat(rdgConcFluxObsTimes.Cells[Ord(fmcValue),Index], AValue) then
        begin
          ObsTime.ObservedValue := AValue;
        end;
        ObsTime.Species := rdgConcFluxObsTimes.Cells[Ord(fmcSpecies),Index];
        if TryStrToFloat(rdgConcFluxObsTimes.Cells[Ord(fmcWeight),Index], AValue) then
        begin
          ObsTime.Weight := AValue;
        end;
        ObsTime.Comment := rdgConcFluxObsTimes.Cells[Ord(fmcComment),Index];
      end;
    end;
  end;
end;

procedure TfrmManageFluxObservations.rdgConcFluxObsTimesHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMt3dmsFluxEdits;
end;

procedure TfrmManageFluxObservations.rdgConcFluxObsTimesSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  EnableMultiEditControl(rdgConcFluxObsTimes, rdeMassFluxMultiValueEdit,
    Ord(fmcTime), Ord(fmcWeight));
  EnableMultiEditControl(rdgConcFluxObsTimes, comboMt3dmsSpecies,
    Ord(fmcSpecies), Ord(fmcSpecies));
end;

procedure TfrmManageFluxObservations.rdgConcFluxObsTimesSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  ItemIndex: Integer;
  ObservationType: TObservationType;
  AColumn: TRbwColumn4;
begin
  inherited;
  if (ACol = Ord(fmcObsType)) and (ARow >= rdgConcFluxObsTimes.FixedRows) then
  begin
    AColumn := rdgConcFluxObsTimes.Columns[Ord(fmcTime)];
    ItemIndex := rdgConcFluxObsTimes.ItemIndex[ACol, ARow];
    if ItemIndex >= 0 then
    begin
      ObservationType := TObservationType(ItemIndex);
      case ObservationType of
        otTime:
          begin
            AColumn.CheckMin := False;
            rdgConcFluxObsTimes.UseSpecialFormat[Ord(fmcTime), ARow] := False;
            AColumn.Min := frmGoPhast.PhastModel.ModflowStressPeriods[0].StartTime;
            AColumn.CheckMin := True;
          end;
        otFrequency:
          begin
            AColumn.CheckMin := False;
            rdgConcFluxObsTimes.SpecialFormat[Ord(fmcTime), ARow] := rcf4Integer;
            rdgConcFluxObsTimes.UseSpecialFormat[Ord(fmcTime), ARow] := True;
            AColumn.Min := 1;
            AColumn.CheckMin := True;
          end;
        else
          Assert(False);
      end;
    end
    else
    begin
      AColumn.CheckMin := False;
      rdgConcFluxObsTimes.UseSpecialFormat[Ord(fmcTime), ARow] := False;
    end;
  end;

end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  if not rdgFluxObsTimes.Enabled then
  begin
    rdgFluxObsTimes.Canvas.Brush.Color := clBtnFace;
  end;
  
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  inherited;
  LayoutMultiFluxEdits;
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesEndUpdate(Sender: TObject);
begin
  inherited;
  if (not FSettingTimeCount) and (seNumObsTimes <> nil) then
  begin
    seNumObsTimes.AsInteger := rdgFluxObsTimes.RowCount -1;
  end;
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesExit(Sender: TObject);
var
  Index: Integer;
  ObsTime: TFluxObservation;
  AValue: Extended;
begin
  inherited;

  for Index := 1 to seNumObsTimes.AsInteger do
  begin
    if Index < rdgFluxObsTimes.RowCount then
    begin
      ObsTime := rdgFluxObsTimes.Objects[Ord(fcName),Index] as TFluxObservation;
      if ObsTime <> nil then
      begin
        if TryStrToFloat(rdgFluxObsTimes.Cells[Ord(fcTime),Index], AValue) then
        begin
          ObsTime.Time := AValue;
        end;
        if TryStrToFloat(rdgFluxObsTimes.Cells[Ord(fcValue),Index], AValue) then
        begin
          ObsTime.ObservedValue := AValue;
        end;
        if TryStrToFloat(rdgFluxObsTimes.Cells[Ord(fcStatistic),Index], AValue) then
        begin
          ObsTime.Statistic := AValue;
        end;
        ObsTime.StatFlag := TStatFlag(rdgFluxObsTimes.ItemIndex[Ord(fcStatFlag),Index]);
        ObsTime.Comment := rdgFluxObsTimes.Cells[Ord(fcComment),Index];
      end;
    end;
  end;
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMultiFluxEdits;
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  EnableMultiEditControl(rdgFluxObsTimes, rdeMultiValueEdit,
    Ord(fcTime), Ord(fcStatistic));
  EnableMultiEditControl(rdgFluxObsTimes, comboMultiStatFlag,
    Ord(fcStatFlag), Ord(fcStatFlag));
end;

procedure TfrmManageFluxObservations.rdgGroupNamesSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  AnObject: TObject;
  AnObsGroup: TCustomFluxObservationGroup;
  AnItem: TTreeNode;
begin
  inherited;
  AnObject := rdgGroupNames.Objects[ACol, ARow];
  if AnObject <> nil then
  begin
    AnObsGroup := AnObject as TCustomFluxObservationGroup;
    AnObsGroup.ObservationName := Value;
    if AnObsGroup = SelectedObservation then
    begin
      edObservationName.Text := AnObsGroup.ObservationName;
      AssignObsNames;
    end;
//    ANode := tvFluxObservations.Items.GetFirstNode;
    for AnItem in tvFluxObservations.Items do
    begin
      if AnItem.Data = AnObsGroup then
      begin
        AnItem.Text := AnObsGroup.ObservationName;
        break;
      end;
    end;
  end;
end;

{ TMassFluxObs }

initialization
  FPriorErrors:= TStringList.Create;

finalization
  FPriorErrors.Free;

end.
