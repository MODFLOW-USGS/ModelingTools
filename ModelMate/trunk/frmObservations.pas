unit frmObservations;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DataGrid, StdCtrls,
  GlobalTypesUnit, DependentsUnit, GlobalBasicData, GlobalData,
  JupiterUnit, frmAddParOrDepUnit, frmAddGroupUnit,
  ModelMateUtilities, frmConfigureObsTablesUnit, ComCtrls, ExtCtrls,
  JvExExtCtrls, JvNetscapeSplitter, Utilities, Menus, RbwDataGrid4, Buttons,
  frmRenameGroup, JvBaseDlg, JvProgressDialog;

type
  TFormObservations = class(TForm)
    StatusBar1: TStatusBar;
    pnlUpper: TPanel;
    lblObsTable: TLabel;
    btnAddObs: TButton;
    btnDeleteSelectedObservations: TButton;
    pnlLower: TPanel;
    btnDeleteSelObsGps: TButton;
    btnAddObsGp: TButton;
    lblObsGpsTable: TLabel;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    rbwdgObs: TRbwDataGrid4;
    btnConfigure: TButton;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    rbwdgObsGps: TRbwDataGrid4;
    btnRenameGp: TButton;
    JvProgressDialog1: TJvProgressDialog;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure dgObsGpsSelectCell(Sender: TObject; ACol, ARow: Integer;
        var CanSelect: Boolean);
    procedure rbwdgObsSelectCell(Sender: TObject; ACol, ARow: Integer;
        var CanSelect: Boolean);
    procedure rbwdgObsUserChanged(Sender: TObject);
    procedure rbwdgObsAfterDelete(Sender: TObject);
    procedure rbwdgObsAfterInsert(Sender: TObject);
    procedure dgObsGpsAfterInsert(Sender: TObject);
    procedure dgObsGpsAfterDelete(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure btnDeleteSelectedObservationsClick(Sender: TObject);
    procedure btnAddObsClick(Sender: TObject);
    procedure btnAddObsGpClick(Sender: TObject);
    procedure btnDeleteSelObsGpsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbwdgObsGpsPopulate(const dsSource: TDepSet);
    procedure rbwdgObsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rbwdgObsExit(Sender: TObject);
    procedure rbwdgObsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnConfigureClick(Sender: TObject);
    procedure rbwdgObsGpsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbwdgObsGpsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rbwdgObsGpsEndUpdate(Sender: TObject);
    procedure rbwdgObsGpsEnter(Sender: TObject);
    procedure rbwdgObsGpsExit(Sender: TObject);
    procedure rbwdgObsGpsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure rbwdgObsGpsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rbwdgObsGpsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rbwdgObsGpsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure btnRenameGpClick(Sender: TObject);
    procedure JvNetscapeSplitter1Moved(Sender: TObject);
    procedure rbwdgObsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    IgnoreGpsStateChange, ObsGpCellChanged: boolean;
    ObservationSetupLocalChanged: boolean;
    ObsGpCellCurrent: GridCell;
    ObsGpCellText: string;
    ObsGpSelected: string;
    ObsGpsLocalChanged: boolean;
    ObsLocalChanged: boolean;
    ObsTableModified: boolean;
    procedure AssignObservations;
    procedure AssignObsGps;
    procedure CheckObsGpNames;
    procedure CountObsLines(const RbwGrid: TRbwDataGrid4; var NLines: integer; var NNames: integer;
                            var FirstBlankRow: integer);
    procedure InitializeObsTables;
    procedure NumberRows;
    procedure OpenConfigObsForm;
    procedure rbwdgObsPopulate(dsSource: TDepSet);
    function SaveObs(const IgnoreErrors: boolean): boolean;
    function SaveObsData(const IgnoreErrors: boolean): boolean;
    function SaveObsGps(): boolean;
  public
    { Public declarations }
    NeedObsPopulate: boolean;
    ObservationDataChanged: boolean;
    ObservationSetupLocal: TObservationSetup;
    ObsGpsLocal: TDepSet;
    ObsSetLocal: TDepSet;
  end;

var
  FormObservations: TFormObservations;

  procedure InitializeObservationUnit;

implementation

{$R *.dfm}

var
  ObsSetLocalTemp: TDepSet;
  ObsTableCol0Width: integer;
  ObsCellSelected: boolean = False;

procedure InitializeObservationUnit;
begin

end;
{ TFormObservations }

procedure TFormObservations.AssignObservations;
// Assign observations and attributes in rbwdgObs to ObsSetLocal
var
  ICol, IOA, IObs, IRow, NObs: integer;
  Caption, Msg, TempString: string;
  ClearedLocalSet: boolean;
begin
  ClearedLocalSet := False;
  NObs := rbwdgObs.RowCount - 1;
  if (not (ObsSetLocal.Count = NObs)) then
    begin
      ObsSetLocal.Clear;
      ClearedLocalSet := True;
    end;
  if (NObs = 1) and (IsBlank(rbwdgObs.Cells[1,1])) then
    NObs := 0;
  if NObs > 0 then
    begin
      for IObs := 0 to NObs - 1 do
        begin
          IRow := IObs+1;
          TempString := rbwdgObs.Cells[1,IRow];
          if J_Valid_Name(TempString,MaxLenDepName) then
            begin
              if ClearedLocalSet then ObsSetLocal.Add;
              ObsSetLocal.Items[IObs].Name := ConvertString20(TempString);
              // Assign attributes to this observation, as defined in table.
              // Loop through columns (observation attributes).
              for ICol := 1 to rbwdgObs.ColCount - 1 do
                begin
                  Caption := rbwdgObs.Cells[ICol,0];
                  // Find index of observation attribute that has this caption.
                  IOA := PosDepCap(Caption);
                  // Assign data grid value to observation attribute.
                  TempString := rbwdgObs.Cells[ICol, IRow];
                  ObsSetLocal.Items[IObs].AllAtts[IOA].Text := TempString;
                end;
            end
          else
            begin
              if TempString <> '' then
                begin
                  Msg := 'Invalid name: ' + TempString;
                  ShowMessage(Msg);
                end;
            end;
        end;
    end;
end;

procedure TFormObservations.AssignObsGps;
var
  ICol, IOA, IObsGp, IRow, NObsGps: integer;
  TempCaption, TempString: string;
  datTemp: TDepAttType;
  ClearedLocalGps: boolean;
begin
  ClearedLocalGps := False;
// Assign observation groups and attributes in rbwdgObsGps to ObsGpsLocal.
  NObsGps := rbwdgObsGps.RowCount - 1;
  if (not (ObsGpsLocal.Count = NObsGps)) then
    begin
      ObsGpsLocal.Clear;
      ClearedLocalGps := True;
    end;
  if NObsGps > 0 then
    begin
      for IObsGp := 0 to NObsGps - 1 do
        begin
          if ClearedLocalGps then ObsGpsLocal.Add;
          IRow := IObsGp + 1;
          TempString := rbwdgObsGps.Cells[0,IRow];
          ObsGpsLocal.Items[IObsGp].Name := ConvertString20(TempString);
          // Assign attributes to this observation group, as defined in table.
          // Loop through columns (observation-group attributes).
          for ICol := 0 to rbwdgObsGps.ColCount - 1 do
            begin
              TempCaption := rbwdgObsGps.Cells[ICol,0];
              // Find index of dependent attribute that has this caption.
              IOA := PosDepCap(TempCaption);
              // Assign data grid value to dependent attribute.
              datTemp := ObsGpsLocal.Items[IObsGp].AllAtts[IOA].DepAttType;
              if (datTemp = datNonDetect) or (datTemp = datUseFlag) then
                begin
                  ObsGpsLocal.Items[IObsGp].AllAtts[IOA].Text :=
                      BooleanToYesOrNo(rbwdgObsGps.Checked[ICol,IRow]);
                end
              else
                begin
                  TempString := rbwdgObsGps.Cells[ICol, IRow];
                  ObsGpsLocal.Items[IObsGp].AllAtts[IOA].Text := TempString;
                end;
            end;
        end;
      if ClearedLocalGps then
        begin
          TempString := 'Need to add code in TFormObservations.AssignObsGps' 
                        + ' to restore hidden attributes';
          ShowMessage(TempString);
        end;
    end;
end;

procedure TFormObservations.btnAddObsClick(Sender: TObject);
var
  ModRes: integer;
begin
  AssignObservations;
  frmAddParOrDep := TfrmAddParOrDep.Create(Self);
  frmAddParOrDep.PDUse := pduObservation;
  DepSetTemp.Assign(ObsSetLocal);
  DepGpsTemp.Assign(ObsGpsLocal);
  ModRes := frmAddParOrDep.ShowModal;
  if ModRes = mrOK then
    begin
      ObsSetLocal.Assign(DepSetTemp);
      ObsGpsLocal.Assign(DepGpsTemp);
      ObsLocalChanged := True;
      NeedObsPopulate := True;
      rbwdgObsPopulate(ObsSetLocal);
    end;
  FreeAndNil(frmAddParOrDep);
end;

procedure TFormObservations.btnAddObsGpClick(Sender: TObject);
// Add an observation group
var
  LocalFrmAddGroup: TfrmAddGroup;
  NGpsOld: integer;
  ModRes: integer;
begin
  NGpsOld := ObsGpsLocal.Count;
  LocalFrmAddGroup := TfrmAddGroup.Create(Self);
  LocalFrmAddGroup.GpUse := guObsGroup;
  DepGpsTemp.Assign(ObsGpsLocal);
  ModRes := LocalFrmAddGroup.ShowModal;
  if ModRes = mrOK then
    begin
      if NGpsOld <> DepGpsTemp.Count then
        begin
          ObsGpsLocal.Assign(DepGpsTemp);
          ObsGpsLocalChanged := True;
          NeedObsPopulate := True;
          rbwdgObsGpsPopulate(ObsGpsLocal);
          rbwdgObsPopulate(ObsSetLocal);  // TODO 2 : Is this needed?
        end;
    end;
  FreeAndNil(LocalFrmAddGroup);
end;

procedure TFormObservations.btnCancelClick(Sender: TObject);
begin
  //InitializeObsTables;
  if ObsTableModified then NeedObsPopulate := True;
end;

procedure TFormObservations.btnOKClick(Sender: TObject);
var
  IgnoreErrors, OK: boolean;
  ErrRow: integer;
  ErrMsg, ErrName: string;
  Selection: TGridRect;
begin
  IgnoreErrors := False;
  ModalResult := mrNo;
  OK := CheckNamesInColumn(rbwdgObs,1,MaxLenDepName,ErrName,ErrRow);
  if (rbwdgObs.RowCount = 2) and (IsBlank(ErrName)) then OK := True;
  if not OK then
    begin
        ErrMsg := 'Invalid observation name: ' + ErrName;
        ShowMessage(ErrMsg);
        Selection.Left := 1;
        Selection.Right := 1;
        Selection.Top := ErrRow;
        Selection.Bottom := ErrRow;
        rbwdgObs.Selection := Selection;
        rbwdgObs.TopRow := ErrRow;
        rbwdgObs.SetFocus;
        ModalResult := mrNone;
    end;
  if OK then
    begin
      // Check observation group names
      OK := CheckNamesInColumn(rbwdgObsGps,0,MaxLenDepName,ErrName,ErrRow);
      if (rbwdgObsGps.RowCount = 2) and (IsBlank(ErrName)) then OK := True;
      if not OK then
        begin
          ErrMsg := 'Invalid name: ' + ErrName;
          ShowMessage(ErrMsg);
          OK := False;
          Selection.Left := 0;
          Selection.Right := 0;
          Selection.Top := ErrRow;
          Selection.Bottom := ErrRow;
          rbwdgObsGps.Selection := Selection;
          rbwdgObsGps.TopRow := ErrRow;
          rbwdgObsGps.SetFocus;
          ModalResult := mrNone;
        end;
    end;
  if OK then
    begin
      if SaveObsData(IgnoreErrors) then
        begin
          if ObservationSetupLocalChanged or ObsLocalChanged or ObsGpsLocalChanged then
            begin
              ObservationDataChanged := True;
            end;
          ModalResult := mrOK;
          Close;  // if FFStyle is modal, Close changes ModalResult to mrCancel (=2)
          ModalResult := mrOK;
        end
      else
        begin
          ModalResult := mrNone;
        end;
    end;
end;

procedure TFormObservations.btnRenameGpClick(Sender: TObject);
var
  I, ModRes, SelRowFirst, SelRowLast: integer;
  Messg, NewGpName, OldGpName: string;
begin
  ClearAllSelections(rbwdgObs);
  rbwdgObs.EditorMode := False;
  SelRowFirst := rbwdgObsGps.Selection.Top;
  SelRowLast := rbwdgObsGps.Selection.Bottom;
  if (SelRowFirst = SelRowLast) and (SelRowFirst > 0) then
    begin
      I := SelRowFirst;
      OldGpName := rbwdgObsGps.Cells[0,I];
      ObsGpCellCurrent.Row := I;
      ObsGpCellCurrent.Column := 0;
      ObsGpCellCurrent.TextOld := OldGpName;
      ObsGpCellCurrent.TextNew := OldGpName;
      FormRenameGroup.CellData := ObsGpCellCurrent;
      FormRenameGroup.GpUse := guObsGroup;
      FormRenameGroup.edtNewGroup.MaxLength := MaxLenGpName;
      ModRes := FormRenameGroup.ShowModal;
      if ModRes = mrOk then
        begin
          NewGpName := FormRenameGroup.CellData.TextNew;
          rbwdgObsGps.Cells[ObsGpCellCurrent.Column,ObsGpCellCurrent.Row]
                := NewGpName;
          ObsSetLocal.ChangeGroupNames(OldGpName,NewGpName);
          NeedObsPopulate := True;  // TODO 2 : May be more efficient to edit data grid directly (down group column)
          rbwdgObsPopulate(ObsSetLocal);
          ClearAllSelections(rbwdgObsGps);
          AssignObsGps;
          ObsGpsLocalChanged := True;
          ObsLocalChanged := True;
        end
      else
        begin
          rbwdgObsGps.Cells[ObsGpCellCurrent.Column,ObsGpCellCurrent.Row]
                := ObsGpCellCurrent.TextOld;
        end;
      ObsGpCellChanged := False;
    end
  else if SelRowFirst <= 0 then
    begin
      Messg := 'Select an observation group to be renamed';
      ShowMessage(Messg);
    end;
end;

procedure TFormObservations.CheckObsGpNames;
var
  IRowCurr: Integer;
  OldGroup: string;
  K: Integer;
  Messg: string;
  NewGroup: string;
begin
  for IRowCurr := 1 to rbwdgObsGps.RowCount - 1 do
    begin
      OldGroup := ConvertString(ObsGpsLocal.Items[IRowCurr - 1].Name);
      NewGroup := rbwdgObsGps.Cells[0,IRowCurr];
      if NewGroup <> OldGroup then
        begin
          // If group name has changed, ensure that old group name is unused.
          K := ObsSetLocal.NumDepByGroup(OldGroup);
          if K > 0 then
            // Old group name is in use.
            begin
              Messg := 'Group name in use may not be changed: ' + OldGroup;
              ShowMessage(Messg);
              rbwdgObsGps.Cells[0, IRowCurr] := OldGroup;
            end
          else
            // Old group name not in use and may be changed
            begin
              // Check validity of new group name
              NewGroup := rbwdgObsGps.Cells[0, IRowCurr];
              if not J_Valid_Name(NewGroup,MaxLenGpName) then
                begin
                  if NewGroup <> '' then
                    begin
                      Messg := 'Invalid name: ' + NewGroup;
                      ShowMessage(Messg);
                    end;
                  rbwdgObsGps.Cells[0, IRowCurr] := OldGroup;
                end;
            end;
        end;
    end;
end;

procedure TFormObservations.CountObsLines(const RbwGrid: TRbwDataGrid4;
  var NLines, NNames, FirstBlankRow: integer);
var
  I, J, NCol, NGridRows: integer;
begin
  NGridRows := RbwGrid.RowCount - 1;
  NCol := RbwGrid.ColCount;
  NLines := 0;
  NNames := 0;
  FirstBlankRow := 0;
  for I := 1 to NGridRows do
    begin
      if IsNonBlank(RbwGrid.Cells[0,I]) then
        begin
          NNames := NNames + 1;
        end
      else
        begin
          if FirstBlankRow = 0 then
            FirstBlankRow := I;
        end;
      for J := 0 to NCol - 1 do
        begin
          if RbwGrid.Cells[J,I] <> '' then
            begin
              NLines := NLines + 1;
              // exit inner loop
              break;
            end;
        end;
    end;
end;

procedure TFormObservations.btnConfigureClick(Sender: TObject);
begin
  OpenConfigObsForm;
  rbwdgObsPopulate(ObsSetLocal);
  rbwdgObsGpsPopulate(ObsGpsLocal);
  rbwdgObs.Invalidate;
  rbwdgObs.Repaint;
  rbwdgObsGps.Invalidate;
  rbwdgObsGps.Repaint;
end;

procedure TFormObservations.btnDeleteSelectedObservationsClick(Sender: TObject);
var
  DelRow, I, J, SelRowFirst, SelRowLast: integer;
  TempObsSet: TDepSet;
  Messg: string;
begin
  SelRowFirst := rbwdgObs.Selection.Top;
  SelRowLast := rbwdgObs.Selection.Bottom;
  if SelRowFirst > 0 then
    begin
      DelRow := 0;
      for I := rbwdgObs.RowCount - 1 downto 1 do
        begin
          if (RowContainsSelectedCell(rbwdgObs,I))
                or ((I >= SelRowFirst) and (I <= SelRowLast)) then
            begin
              if rbwdgObs.RowCount > 2 then
                begin
                  rbwdgObs.DeleteRow(I);
                end
              else
                begin
                  rbwdgObs.Cells[1,1] := '';
                  for J := 2 to rbwdgObs.ColCount-1 do
                    begin
                      rbwdgObs.Cells[J,1] := '';
                    end;
                  TempObsSet := TDepSet.Create;
                  rbwdgObsPopulate(TempObsSet);
                  TempObsSet.Free;
                end;
              DelRow := I;
              // Delete corresponding item in ObsSetLocal.
              if (ObsSetLocal.Count > 0) then ObsSetLocal.Delete(I-1);
            end;
        end;
      if DelRow > 0 then
        begin
          ObsLocalChanged := True;
          NumberRows;
          if (DelRow < rbwdgObs.RowCount - 1) then
            begin
              rbwdgObs.Row := DelRow;
            end;
          ClearAllSelections(rbwdgObs);
          AssignObservations;
        end;
    end
  else
    begin
      Messg := 'Select observation(s) to be deleted';
      ShowMessage(Messg);
    end;
end;

procedure TFormObservations.btnDeleteSelObsGpsClick(Sender: TObject);
// Delete selected observation groups, but only if group is unused
var
  DelRow, I, K, SelRowFirst, SelRowLast: integer;
  GpName: string;
  Messg: string;
begin
  ClearAllSelections(rbwdgObs);
  rbwdgObs.EditorMode := False;
  if (rbwdgObsGps.RowCount > 2) and (CountSelectedRows(rbwdgObsGps) < rbwdgObsGps.RowCount-1) then
    begin
      SelRowFirst := rbwdgObsGps.Selection.Top;
      SelRowLast := rbwdgObsGps.Selection.Bottom;
      if SelRowFirst > 0 then
        begin
          DelRow := 0;
          for I := rbwdgObsGps.RowCount - 1 downto 1 do
            begin
              if (RowContainsSelectedCell(rbwdgObsGps,I))
                    or ((I >= SelRowFirst) and (I <= SelRowLast)) then
                begin
                  GpName := rbwdgObsGps.Cells[0,I];
                  K := ObsSetLocal.NumDepByGroup(GpName);
                  if K = 0 then
                    begin
                      if rbwdgObsGps.RowCount > 2 then
                        begin
                          rbwdgObsGps.DeleteRow(I);
                        end;
                      DelRow := I;
                      // Delete corresponding item in ObsGpsLocal.
                      if ObsGpsLocal.Count > 0 then ObsGpsLocal.Delete(I-1);
                    end
                  else
                    begin
                      Messg := 'Group in use may not be deleted: ' + GpName;
                      ShowMessage(Messg);
                    end;
                end;
            end;
          if DelRow > 0 then
            begin
              ClearAllSelections(rbwdgObsGps);
              AssignObsGps;
              ObsGpsLocalChanged := True;
              NeedObsPopulate := True;
              rbwdgObsPopulate(ObsSetLocal);
            end;
        end
      else
        begin
          Messg := 'Select observation group(s) to be deleted';
          ShowMessage(Messg);
        end;
    end
  else
    begin
      Messg := 'At least one group must be defined.  Group(s) not deleted';
      ShowMessage(Messg);
    end;
end;

procedure TFormObservations.rbwdgObsAfterDelete(Sender: TObject);
begin
  NumberRows;
  AssignObservations;
  ObsLocalChanged := True;
  ObservationsChanged := True;
  StatusBar1.Panels[0].Text := '';
end;

procedure TFormObservations.rbwdgObsAfterInsert(Sender: TObject);
begin
  NumberRows;
end;

procedure TFormObservations.rbwdgObsExit(Sender: TObject);
begin
  AssignObservations;
  if ObsSetLocal <> PCurrent.ObsSet then
    begin
      ObsLocalChanged := True;
    end;
  rbwdgObs.Invalidate;
  rbwdgObs.Repaint;
end;

procedure TFormObservations.rbwdgObsGpsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  Invalidate;
  Repaint;
end;

procedure TFormObservations.rbwdgObsGpsEndUpdate(Sender: TObject);
begin
  if ObsGpCellChanged then
    begin
      if ObsGpCellCurrent.Column = 0 then
        begin
          rbwdgObsGps.Cells[ObsGpCellCurrent.Column,ObsGpCellCurrent.Row]
                    := ObsGpCellCurrent.TextOld;
          ObsGpCellChanged := False;
        end;
    end;
end;

procedure TFormObservations.rbwdgObsGpsEnter(Sender: TObject);
begin
//  rbwdgObsGpsPopulate(ObsGpsCurrent);
end;

procedure TFormObservations.rbwdgObsGpsExit(Sender: TObject);
var
  FirstBlankObsGpRow, NObsGpLines, NObsGpNames: integer;
  OK: boolean;
begin
  // Check observation group table entries
  // Count lines not filled with blanks in observation groups table
  // If NObsGpNames < NObsGpLines, there's at least one blank group name
  CountObsLines(rbwdgObsGps,NObsGpLines,NObsGpNames,FirstBlankObsGpRow);
  if NObsGpLines = NObsGpNames then
    begin
      OK := True;
    end
  else
    begin
      ShowMessage('Error in Observation Groups Table: At least one group is unnamed');
      rbwdgObsGps.SetFocus;
      rbwdgObsGps.Column := 0;
      rbwdgObsGps.Row := FirstBlankObsGpRow;
      OK := False;
    end;
  if OK then
    begin
      CheckObsGpNames;
      AssignObsGps;
    end;
  rbwdgObsGps.Invalidate;
  rbwdgObsGps.Repaint;
end;

procedure TFormObservations.rbwdgObsGpsGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  ObsGpCellText := Value;
  ObsGpCellChanged := False;
  if ACol = 0 then
    begin
      ObsGpSelected := Value;
    end;
end;

procedure TFormObservations.rbwdgObsGpsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    begin
      SetColorSelectedRow(rbwdgObsGps,False);
    end
  else
    begin
      SetColorSelectedRow(rbwdgObsGps,True);
    end;
  if ssShift in Shift then
    begin
      AllowEditing(rbwdgObsGps,False);
      SetColorSelectedRow(rbwdgObsGps,False);
    end
  else
    begin
      AllowEditing(rbwdgObsGps,True);
    end;
end;

procedure TFormObservations.rbwdgObsGpsPopulate(const dsSource: TDepSet);
var
  I, IA, IGP, IP1, J, NRows, NumCols: integer;
  BlankString, TempString: string;
  setATDisplayGps: set of TDepAttType;
  datTemp: TDepAttType;
  TempBool: boolean;
begin
  BlankString := '';
  if ObsGpsLocal.Count = 0 then
    begin
      ObsGpsLocal.SetGpDefault(dcObs);
    end;

  case PCurrent.ActiveApp of
    aaUcode:
      begin
        setATDisplayGps := [datStatistic, datStatFlag, datUseFlag,
            datPlotSymbol, datWtMultiplier, datCovMatrix,
            datNonDetect, datWtOSConstant];
      end;
    aaPest:
      begin
        setATDisplayGps := [];
      end;
    aaApp3:
      begin
        setATDisplayGps := [];
      end;
    aaApp4:
      begin
        setATDisplayGps := [];
      end;
  end;
  // Count number of columns needed
  NumCols := 1; // Always one column for observation group
  for I := 0 to NumDepAttributes - 1 do
    if (ObservationSetupLocal.ObsAttributes[I].DepAttType in setATDisplayGps) and
       (ObservationSetupLocal.ObsAttributes[I].ControlMethod = cmByGroup) then
            NumCols := NumCols + 1;
  rbwdgObsGps.ColCount := NumCols;
  if rbwdgObsGps.ColCount >1 then
    begin
      rbwdgObsGps.FixedCols := 1;
    end;
  // Clear table headings and cells
  for I := 0 to rbwdgObsGps.ColCount - 1 do
    begin
      rbwdgObsGps.Columns[I].CaptionAlignment := taLeftJustify;
      rbwdgObsGps.Cells[I,0] := BlankString;
      rbwdgObsGps.Columns[I].AutoAdjustColWidths := True;
      for J := 0 to rbwdgObsGps.RowCount - 1 do
        begin
          try
            rbwdgObsGps.Cells[I, J] := BlankString;
          except
            On E: EListError do
              ShowMessage('Caught EListError exception');
          end;
        end;
    end;
  // After all cells are cleared, assign ColWidths to minimal width.
  for I := 0 to rbwdgObsGps.ColCount - 1 do
    begin
      rbwdgObsGps.Columns[I].ComboUsed := False;
      rbwdgObsGps.ColWidths[I] := rbwdgObsGps.DefaultColWidth;
      rbwdgObsGps.Columns[I].AutoAdjustColWidths := True;
    end;
  // Populate column headings and assign properties for selected columns
  rbwdgObsGps.DefaultColWidth := 50;
  for J := 0 to rbwdgObsGps.ColCount - 1 do
    begin
      rbwdgObsGps.Columns[J].AutoAdjustColWidths := True;
    end;
  NRows := dsSource.Count + 1;
  if NRows < 2 then NRows := 2;
  rbwdgObsGps.RowCount := NRows;
  J := 0;
  rbwdgObsGps.Cells[J,0] := 'Group Name';
  for I := 0 to NumDepAttributes - 1 do
    if (ObservationSetupLocal.ObsAttributes[I].ControlMethod = cmByGroup) and
       (ObservationSetupLocal.ObsAttributes[I].DepAttType in setATDisplayGps) then
      begin
        case ObservationSetupLocal.ObsAttributes[I].DepAttType of
          datStatistic: begin
              J := J + 1;
              rbwdgObsGps.Cells[J,0] :=
                   ObservationSetupLocal.ObsAttributes[I].Caption;
              rbwdgObsGps.Columns[J].Format := rcf4Real;
              rbwdgObsGps.Columns[J].ComboUsed := False;
              rbwdgObsGps.Columns[J].LimitToList := False;
            end;
          datStatFlag: begin
              J := J + 1;
              rbwdgObsGps.Cells[J,0] :=
                   ObservationSetupLocal.ObsAttributes[I].Caption;
              rbwdgObsGps.Columns[J].Format := rcf4String;
              rbwdgObsGps.Columns[J].ComboUsed := True;
              rbwdgObsGps.Columns[J].PickList := slStatFlag;
              rbwdgObsGps.Columns[J].LimitToList := True;
            end;
          datUseFlag: begin
              J := J + 1;
              rbwdgObsGps.Cells[J,0] :=
                   ObservationSetupLocal.ObsAttributes[I].Caption;
              rbwdgObsGps.Columns[J].Format := rcf4Boolean;
              rbwdgObsGps.Columns[J].ComboUsed := False;
              rbwdgObsGps.Columns[J].LimitToList := False;
            end;
          datPlotSymbol: begin
              J := J + 1;
              rbwdgObsGps.Cells[J,0] :=
                   ObservationSetupLocal.ObsAttributes[I].Caption;
              rbwdgObsGps.Columns[J].Format := rcf4Integer;
              rbwdgObsGps.Columns[J].ComboUsed := False;
              rbwdgObsGps.Columns[J].LimitToList := False;
            end;
          datWtMultiplier: begin
              J := J + 1;
              rbwdgObsGps.Cells[J,0] :=
                   ObservationSetupLocal.ObsAttributes[I].Caption;
              rbwdgObsGps.Columns[J].Format := rcf4Real;
              rbwdgObsGps.Columns[J].ComboUsed := False;
              rbwdgObsGps.Columns[J].LimitToList := False;
            end;
          datCovMatrix: begin
              J := J + 1;
              rbwdgObsGps.Cells[J,0] :=
                   ObservationSetupLocal.ObsAttributes[I].Caption;
              rbwdgObsGps.Columns[J].Format := rcf4String;
            end;
          datNonDetect: begin
              J := J + 1;
              rbwdgObsGps.Cells[J,0] :=
                   ObservationSetupLocal.ObsAttributes[I].Caption;
              rbwdgObsGps.Columns[J].Format := rcf4Boolean;
              rbwdgObsGps.Columns[J].ComboUsed := False;
              rbwdgObsGps.Columns[J].LimitToList := False;
            end;
          datWtOSConstant: begin
              J := J + 1;
              rbwdgObsGps.Cells[J,0] :=
                   ObservationSetupLocal.ObsAttributes[I].Caption;
              rbwdgObsGps.Columns[J].Format := rcf4Real;
              rbwdgObsGps.Columns[J].ComboUsed := False;
              rbwdgObsGps.Columns[J].LimitToList := False;
            end;
        end;
    end;
  //
  // Populate table cells
  // Loop through groups; each iteration populates a row
  for IGP := 0 to dsSource.Count - 1 do
  begin
    IP1 := IGP + 1;
    rbwdgObsGps.Cells[0,IP1] := ConvertString(dsSource.Items[IGP].Name);
    J := 0;
    for IA := 0 to NumDepAttributes - 1 do
    begin
      datTemp := ObservationSetupLocal.ObsAttributes[IA].DepAttType;
      if (ObservationSetupLocal.ObsAttributes[IA].ControlMethod = cmByGroup) and
         (datTemp in setATDisplayGps) then
        begin
          J := J + 1;
          TempString := dsSource.Items[IGP].AllAtts[IA].Text;
          if TempString = '' then
            TempString := dsSource.Items[IGP].AllAtts.Items[IA].DefaultText(dcObs);
          datTemp := dsSource.Items[IGP].AllAtts[IA].DepAttType;
          if ((datTemp = datUseFlag) or (datTemp = datNonDetect)) then
            begin
              // Check or uncheck box depending on boolean value.
              TempBool := YesOrNoToBoolean(TempString);
              IgnoreGpsStateChange := True;
              rbwdgObsGps.Checked[J,IP1] := TempBool;
              IgnoreGpsStateChange := False;
            end
          else
            begin
              rbwdgObsGps.Cells[J,IP1] := TempString;
            end;
        end;
    end;
  end;
//  NumberRows;
  rbwdgObsGps.Invalidate;
end;

procedure TFormObservations.rbwdgObsGpsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Hint: string;
  LocalRect: TGridRect;
begin
  if rbwdgObsGps.Drawing then
  begin
    Exit;
  end;
  if (ACol = rbwdgObsGps.Column) and (ACol >= 0) and (ARow >= 0) then
    begin
      Hint := GetObsHint(rbwdgObsGps, ACol);
      StatusBar1.Panels[0].Text := ' ' + Hint;
      if ACol = 0 then
        begin
          LocalRect.Left := 0;
          LocalRect.Right := rbwdgObsGps.ColCount - 1;
          LocalRect.Top := ARow;
          LocalRect.Bottom := ARow;
          rbwdgObsGps.Selection := LocalRect;
        end;
    end;
end;

procedure TFormObservations.rbwdgObsGpsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if Value <> ObsGpCellText then
    begin
      ObsGpCellCurrent.Row := ARow;
      ObsGpCellCurrent.Column := ACol;
      ObsGpCellCurrent.Checked := rbwdgObsGps.Checked[ACol,ARow];
      ObsGpCellCurrent.TextOld := ObsGpCellText;
      ObsGpCellCurrent.TextNew := Value;
      ObsGpCellChanged := True;
      ObsGpsLocalChanged := True;
    end;
end;

procedure TFormObservations.rbwdgObsGpsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  if not IgnoreGpsStateChange then
    begin
      AssignObsGps;
      ObsGpsLocalChanged := True;
    end;
end;

procedure TFormObservations.rbwdgObsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    begin
      SetColorSelectedRow(rbwdgObs,False);
    end
  else
    begin
      SetColorSelectedRow(rbwdgObs,True);
    end;
  if ssShift in Shift then
    begin
      AllowEditing(rbwdgObs,False);
      SetColorSelectedRow(rbwdgObs,False);
    end
  else
    begin
      AllowEditing(rbwdgObs,True);
    end;
end;

procedure TFormObservations.rbwdgObsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  NumberRows;
end;

procedure TFormObservations.dgObsGpsAfterDelete(Sender: TObject);
begin
  ObsGpsLocalChanged := True;
end;

procedure TFormObservations.dgObsGpsAfterInsert(Sender: TObject);
begin
  rbwdgObsGpsPopulate(ObsGpsLocal);
end;

procedure TFormObservations.dgObsGpsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  StatusBar1.Panels[0].Text := '';
end;

procedure TFormObservations.rbwdgObsPopulate(dsSource: TDepSet);
// Populate data grid rbwdgObs
var
  DatPos, I, IA, IP1, J, NRows, NumCols: integer;
  setATDisplayObs: set of TDepAttType;
  datTemp: TDepAttType;
  GpList: TStringList;
  TempString: string;
  TempBool: boolean;
begin
  GpList := TStringList.Create;
  try
    if NeedObsPopulate then
      begin
        for I := 0 to ObsGpsLocal.Count - 1 do
          GpList.Add(ConvertString(ObsGpsLocal.Items[I].Name));
        case PCurrent.ActiveApp of
        aaUcode:
          begin
            setATDisplayObs := [datObsValue, datStatistic,
                datStatFlag, datEquation, datUseFlag, datPlotSymbol,
                datWtMultiplier, datCovMatrix, datNonDetect, datWtOSConstant];
          end;
        aaPest:
          begin
          end;
        aaApp3:
          begin
          end;
        aaApp4:
          begin
          end;
        end;
        // Count number of columns needed
        NumCols := 3; // Always columns for observation number, name, and group
        for I := 0 to NumDepAttributes - 1 do
          if (ObservationSetupLocal.ObsAttributes[I].DepAttType in setATDisplayObs) and
             (ObservationSetupLocal.ObsAttributes[I].ControlMethod = cmByItem) then
                  NumCols := NumCols + 1;
        rbwdgObs.ColCount := NumCols;
        // Clear table headings and cells
        for I := 0 to rbwdgObs.ColCount - 1 do
          begin
            rbwdgObs.Cells[I,0] := '';
            rbwdgObs.Columns[I].CaptionAlignment := taLeftJustify;
            rbwdgObs.Columns[I].AutoAdjustColWidths := False;
            for J := 1 to rbwdgObs.RowCount - 1 do
                rbwdgObs.Cells[I, J] := '';
          end;
        // After all cells are cleared, assign ColWidths to minimal width.
        for I := 0 to rbwdgObs.ColCount - 1 do
          begin
            rbwdgObs.Columns[I].ComboUsed := False;
            rbwdgObs.ColWidths[I] := rbwdgObs.DefaultColWidth;
          end;
        // Populate column headings and assign properties for selected columns
        NRows := dsSource.Count + 1;
        if NRows < 2 then NRows := 2;
        rbwdgObs.RowCount := NRows;
        rbwdgObs.Cells[0,0] := 'Obs #';

        rbwdgObs.Cells[1,0] := 'Observation Name';
        rbwdgObs.Columns[1].Format := rcf4String;
        rbwdgObs.Columns[1].ComboUsed := False;

        rbwdgObs.Cells[2,0] := 'Group Name';
        rbwdgObs.Columns[2].Format := rcf4String;
        rbwdgObs.Columns[2].ComboUsed := True;
        rbwdgObs.Columns[2].PickList := GpList;
        rbwdgObs.Columns[2].LimitToList := True;

        J := 2;
        for I := 0 to NumDepAttributes - 1 do
          if (ObservationSetupLocal.ObsAttributes[I].ControlMethod = cmByItem) and
             (ObservationSetupLocal.ObsAttributes[I].DepAttType in setATDisplayObs) then
          begin
            case ObservationSetupLocal.ObsAttributes[I].DepAttType of
              datObsName: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4String;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
              datObsValue: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4Real;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
              datStatistic: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4Real;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
              datStatFlag: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4String;
                  rbwdgObs.Columns[J].ComboUsed := True;
                  rbwdgObs.Columns[J].PickList := slStatFlag;
                  rbwdgObs.Columns[J].LimitToList := True;
                end;
              datUseFlag: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4Boolean;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
              datPlotsymbol: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4Integer;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
              datEquation: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4String;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
              datWtMultiplier: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4Real;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
              datCovMatrix: begin
                  { TODO 3 -cObservations : Code this option to display list of defined CovMatrix names }
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4String;
                  rbwdgObs.Columns[J].ComboUsed := False;
                end;
              datNonDetect: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4Boolean;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
              datWtOSConstant: begin
                  J := J + 1;
                  rbwdgObs.Cells[J,0] :=
                       ObservationSetupLocal.ObsAttributes[I].Caption;
                  rbwdgObs.Columns[J].Format := rcf4Real;
                  rbwdgObs.Columns[J].ComboUsed := False;
                  rbwdgObs.Columns[J].LimitToList := False;
                end;
            end;
          end;
        // Populate table cells
        // Loop through groups; each iteration populates a row
        DatPos := DepAttPos(datGroupName);
        JvProgressDialog1.InitValues(0,dsSource.Count,1,0,'Progress','Processing observations table');
        JvProgressDialog1.Smooth := True;
        JvProgressDialog1.ShowCancel := False;
        JvProgressDialog1.Show;
        for I := 0 to dsSource.Count - 1 do
          begin
            IP1 := I + 1;
            rbwdgObs.Cells[0,IP1] := IntToStr(IP1);
            rbwdgObs.Cells[1,IP1] := ConvertString(dsSource.Items[I].Name);
            rbwdgObs.Cells[2,IP1] := dsSource.Items[I].AllAtts.Items[DatPos].Text;
            J := 2;
            for IA := 0 to NumDepAttributes - 1 do
            begin
              if (ObservationSetupLocal.ObsAttributes[IA].ControlMethod = cmByItem) and
                 (ObservationSetupLocal.ObsAttributes[IA].DepAttType in setATDisplayObs) then
                begin
                  J := J + 1;
                  TempString := dsSource.Items[I].AllAtts[IA].Text;
                  datTemp := dsSource.Items[I].AllAtts[IA].DepAttType;
                  if datTemp = datNonDetect then
                    begin
                      TempBool := YesOrNoToBoolean(dsSource.Items[I].AllAtts[IA].Text);
                      rbwdgObs.Checked[J,IP1] := TempBool;
                    end
                  else
                    begin
                      if TempString = '' then
                        TempString := dsSource.Items[I].AllAtts.Items[IA].DefaultText(dcObs);
                      rbwdgObs.Cells[J,IP1] := TempString;
                    end;
                end;
            end;
            JvProgressDialog1.Position := I;
          end;
        // Set selections to be invisible and invalid
        ClearAllSelections(rbwdgObs);
        // After all cells are populated, set AutoAdjustColWidths to True.
        for I := 0 to rbwdgObs.ColCount - 1 do
          begin
            rbwdgObs.Columns[I].AutoAdjustColWidths := True;
          end;
        //
        SetColorSelectedRow(rbwdgObs,True);
        StatusBar1.Panels[0].Text := '';
        rbwdgObs.Invalidate;
        ObsTableModified := True;
        NeedObsPopulate := False;
      end;
  finally
    GpList.Free;
    JvProgressDialog1.Hide;
  end;
end;

procedure TFormObservations.OpenConfigObsForm;
var
  IgnoreErrors: Boolean;
  ModRes: Integer;
begin
  IgnoreErrors := True;
  SaveObsData(IgnoreErrors);  // this is a boolean function
  FormConfigureObsTables.ObservationSetupConfigure.Assign(ObservationSetupLocal);
  ModRes := FormConfigureObsTables.ShowModal;
  if ModRes = mrOK then
    begin
      if FormConfigureObsTables.ObservationSetupConfigureChanged then
        begin
          ObservationSetupLocal.Assign(FormConfigureObsTables.ObservationSetupConfigure);
          ObservationSetupLocalChanged := True;
          NeedObsPopulate := True;
        end;
    end;
end;

procedure TFormObservations.InitializeObsTables;
begin
  rbwdgObs.ColWidths[0] := ObsTableCol0Width;
  rbwdgObsPopulate(ObsSetLocal);
  rbwdgObsGpsPopulate(ObsGpsLocal);
  ClearAllSelections(rbwdgObs);
end;

procedure TFormObservations.JvNetscapeSplitter1Moved(Sender: TObject);
begin
  ClearAllSelections(rbwdgObs);
  ClearAllSelections(rbwdgObsGps);
end;

procedure TFormObservations.rbwdgObsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = rbwdgObs.Column) and (ACol >= 0) and (ARow >= 0) then
    begin
      StatusBar1.Panels[0].Text := ' ' + GetObsHint(rbwdgObs, ACol);
      if ARow > 0 then
        begin
          ObsCellSelected := True;
        end
      else
        ObsCellSelected := False;
    end;
end;

procedure TFormObservations.rbwdgObsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  ObsLocalChanged := True;
end;

procedure TFormObservations.rbwdgObsUserChanged(Sender: TObject);
// Save observation changes to ObsSetLocal
var
  IColCurr, IRowCurr: integer;
  Messg, TempString: string;
  IgnoreChange: boolean;
begin
  IgnoreChange := False;
  IColCurr := rbwdgObs.Col;
  IRowCurr := rbwdgObs.Row;
  rbwdgObs.Cells[0,IRowCurr] := IntToStr(IRowCurr);
  if IColCurr = 1 then
    begin
      // Changed cell is for observation name
      // Check validity of observation name
      TempString := rbwdgObs.Cells[1, IRowCurr];
      if J_Valid_Name(TempString,MaxLenDepName) then
        begin
          AssignObservations;
//          UpdateCurrentProject;
        end
      else
        begin
          if TempString <> '' then
            begin
              Messg := 'Invalid name: ' + TempString;
              ShowMessage(Messg);
            end;
          rbwdgObs.Cells[1, IRowCurr] := ConvertString(ObsSetLocalTemp.Items[IRowCurr-1].Name);
          IgnoreChange := True;
        end;
    end;
  if not IgnoreChange then
    begin
      AssignObservations;
      ObsLocalChanged := True;
      ObservationsChanged := True;
    end;
  StatusBar1.Panels[0].Text := '';
end;

procedure TFormObservations.FormCreate(Sender: TObject);
begin
  Left := 15;
  Top := 25;
  ObservationSetupLocal := TObservationSetup.Create(self);
  ObsSetLocal := TDepSet.Create;
  ObsSetLocalTemp  := TDepSet.Create;
  ObsGpsLocal := TDepSet.Create;
  ObsTableCol0Width := 65;
  NeedObsPopulate := True;
end;

procedure TFormObservations.FormDestroy(Sender: TObject);
begin
  ObservationSetupLocal.Free;
  ObsSetLocal.Free;
  ObsSetLocalTemp.Free;
  ObsGpsLocal.Free;
end;

procedure TFormObservations.FormResize(Sender: TObject);
var
  AvailableHeight: integer;
begin
  AvailableHeight := self.Height - pnlUpper.Height - StatusBar1.Height - 37;
  if pnlLower.Height > AvailableHeight then
    pnlLower.Height := AvailableHeight;
end;

procedure TFormObservations.FormShow(Sender: TObject);
begin
  ObsSetLocalTemp.Assign(ObsSetLocal);
  InitializeObsTables;
  ObsTableModified := False;
  ObservationSetupLocalChanged := False;
  ObsLocalChanged := False;
  ObsGpsLocalChanged := False;
  StatusBar1.Panels[0].Text := 'Click in a column to display hint';
  Invalidate;
end;

procedure TFormObservations.NumberRows;
var
  I, NR: integer;
begin
  NR:= rbwdgObs.RowCount;
  for I := 1 to NR - 1 do
    rbwdgObs.Cells[0,I] := IntToStr(I);
end;

function TFormObservations.SaveObs(const IgnoreErrors: boolean): boolean;
var
  I, IAtt, IErr, II, IRow, J, JJ, NCols, NObs, NRdata: integer;
  Caption, Msg: string;
  datTemp: TDepAttType;
  ClearedLocalSet: boolean;
begin
  result := True;
  ClearedLocalSet := False;
  // need code
  IErr := 0;
  Msg := '';
  // Save observation information
  NRdata := rbwdgObs.RowCount - 1;
  NObs := NRdata;
  NCols := rbwdgObs.ColCount;
  if not IgnoreErrors then
    begin
      // Ensure all rows are fully populated
      NObs := 0;
      for IRow := 1 to NRdata do // IRow is the row index in the observations table
        begin
          if rbwdgObs.Cells[1,IRow] <> '' then // Observation name
            begin
              for J := 2 to NCols - 1 do
                begin
                  if (rbwdgObs.Cells[J,IRow] = '') and (rbwdgObs.Columns[J].Format <> rcf4Boolean) then
                    begin
                      Msg := 'Error: Missing data for observation ' + IntToStr(IRow);
                      IErr := 1;
                    end;
                end;
              NObs := NObs+1;
            end
          else
            begin
              if IRow > 1 then
                begin
                  Msg := 'Error: Missing data for row ' + IntToStr(IRow);
                  IErr := 1;
                end;
            end;
        end;
    end;
    //
  if IErr <> 0 then
    begin
      ShowMessage(Msg);
    end
  else
    begin
      if not NObs = ObsSetLocal.Count then
        begin
          ObsSetLocal.Clear;
          ClearedLocalSet := True;
        end;
      for I := 1 to NObs do
        begin
          if ClearedLocalSet then ObsSetLocal.Add;
          II := I-1;
          ObsSetLocal.Items[II].Name := ConvertString20(rbwdgObs.Cells[1,I]);
          if I>1 then
            begin
              J := 1;
              while (J<I) and (IErr=0) do
                begin
                  JJ := J-1;
                  if SameText(ConvertString(ObsSetLocal.Items[II].Name),ConvertString(ObsSetLocal.Items[JJ].Name)) then
                    begin
                      if not IgnoreErrors then
                        begin
                          IErr := I;
                          Msg := 'Error: Observations ' + IntToStr(J) + ' and ' + IntToStr(IErr)
                                 + ' have duplicate names';
                          ShowMessage(Msg);
                        end;
                    end;
                  J := J+1;
                end;
            end;
        end;
      //
      if IErr=0 then
        begin
          for J := 1 to NCols - 1 do // loop through columns
            begin
              // Define IAtt as attribute number for attribute in this column
              Caption := rbwdgObs.Cells[J,0];
              IAtt := PosDepCap(Caption);
              datTemp := DepAttributeTypes[IAtt];
              for I := 1 to NObs do // loop through rows in this column
                begin
                  II := I-1;
                  if (datTemp = datNonDetect) then
                    begin
                      ObsSetLocal.Items[II].AllAtts.Items[IAtt].Text :=
                          BooleanToYesOrNo(rbwdgObs.Checked[J,I]);
                    end
                  else
                    begin
                      ObsSetLocal.Items[II].AllAtts.Items[IAtt].Text := rbwdgObs.Cells[J,I];
                      ObsSetLocal.Items[II].AllAtts.Items[IAtt].DepAttType := datTemp;
                    end;
                end;
            end;
        end;
    end;
  if IErr = 0 then
    begin
      ObsSetLocalTemp.Assign(ObsSetLocal);
    end
  else
    begin
      result := False;
    end;
end; // function TFormObservations.SaveObs

function TFormObservations.SaveObsData(const IgnoreErrors: boolean): boolean;
var
  OK: Boolean;
begin
  OK := True;
  StatusBar1.Panels[0].Text := '';
  if ObsLocalChanged or ObservationSetupLocalChanged then
    begin
      if not SaveObs(IgnoreErrors) then
        begin
          OK := False;
        end;
    end;
  if ObsGpsLocalChanged or ObservationSetupLocalChanged then
    begin
      if not SaveObsGps then
        begin
          OK := False;
        end;
    end;
  if OK then
    begin
      StatusBar1.Panels[0].Text := 'Data Saved';
    end;
  result := OK;
end;  // function TFormObservations.SaveObsData.

function TFormObservations.SaveObsGps(): boolean;
var
  I, IAtt, IErr, II, IRow, J, JJ, NCols, NObsGps, NRdata: integer;
  Caption, Msg: string;
  datTemp: TDepAttType;
  ClearedLocalGps: boolean;
begin
  result := True;
  ClearedLocalGps := False;
  IErr := 0;
  Msg := '';
  // Save observation groups information
  NRdata := rbwdgObsGps.RowCount - 1;
  NObsGps := 0;
  NCols := rbwdgObsGps.ColCount;
  // Ensure all rows are fully populated
  for IRow := 1 to NRdata do // IRow is the row index in the observation groups table
    begin
      if rbwdgObsGps.Cells[0,IRow] <> '' then // Observation group name
        begin
          for J := 1 to NCols - 1 do
            begin
              if (rbwdgObsGps.Cells[J,IRow] = '') and (rbwdgObsGps.Columns[J].Format <> rcf4Boolean) then
                begin
                  Msg := 'Error: Missing data for group ' + rbwdgObsGps.Cells[0,IRow];
                  IErr := 1;
                end
            end;
          NObsGps := NObsGps + 1;
        end
      else
        begin
          Msg := 'Warning: Duplicate group name not saved';
          IErr := 1;
        end;
    end;
    //
  if IErr <> 0 then
    begin
      ShowMessage(Msg);
    end
  else
    begin
      if (not (NObsGps = ObsGpsLocal.Count)) then
        begin
          ObsGpsLocal.Clear;
          ClearedLocalGps := True;
        end;
      for I := 1 to NObsGps do
        begin
          if ClearedLocalGps then ObsGpsLocal.Add;
          II := I-1;
          ObsGpsLocal.Items[II].Name := ConvertString20(rbwdgObsGps.Cells[0,I]);
          //
          if I>1 then
            begin
              J := 1;
              while (J<I) and (IErr=0) do
                begin
                  JJ := J-1;
                  if SameText(ConvertString(ObsGpsLocal.Items[II].Name), ConvertString(ObsGpsLocal.Items[JJ].Name)) then
                    begin
                      IErr := I;
                      Msg := 'Error: Duplicate group name: ' + ConvertString(ObsGpsLocal.Items[II].Name);
                      ShowMessage(Msg);
                    end;
                  J := J + 1;
                end;
            end;
        end;
      if IErr=0 then
        begin
          for J := 0 to NCols - 1 do // loop through columns.
            begin
              // Define IAtt as attribute number for attribute in this column.
              Caption := rbwdgObsGps.Cells[J,0];
              IAtt := PosDepCap(Caption);
              datTemp := DepAttributeTypes[IAtt];
              for I := 1 to NObsGps do // loop through rows in this column.
                begin
                  II := I-1;
                  ObsGpsLocal.Items[II].AllAtts.Items[IAtt].DepAttType := datTemp;
                  if (datTemp = datNonDetect) or (datTemp = datUseFlag) then
                    begin
                      ObsGpsLocal.Items[II].AllAtts.Items[IAtt].Text := BooleanToYesOrNo(rbwdgObsGps.Checked[J,I]);
                    end
                  else
                    begin
                      ObsGpsLocal.Items[II].AllAtts.Items[IAtt].Text := rbwdgObsGps.Cells[J,I];
                    end;
                end;
            end;
        end;
    end;
  if IErr <> 0 then
    begin
      result := False;
    end
  else
    begin
      ObservationsChanged := True;
      ProjChanged := True;
    end;
end;

procedure TFormObservations.StatusBar1Click(Sender: TObject);
begin
  ShowMessage(StatusBar1.Panels[0].Text);
end;

initialization
  InitializeObservationUnit;

end.
