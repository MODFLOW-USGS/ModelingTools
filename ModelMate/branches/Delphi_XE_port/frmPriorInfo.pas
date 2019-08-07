unit frmPriorInfo;

interface


uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, Grids, DataGrid, StdCtrls,
  PriorInfoUnit, frmAddParOrDepUnit, ModelMateUtilities,
  GlobalBasicData, ComCtrls, JupiterUnit, frmAddGroupUnit,
  frmConfigurePriTablesUnit, GlobalTypesUnit, GlobalData, ExtCtrls,
  JvExExtCtrls, JvNetscapeSplitter, Utilities, Menus, Buttons, RbwDataGrid4,
  frmRenameGroup;

type
  TFormPriorInfo = class(TForm)
    StatusBar1: TStatusBar;
    pnlUpper: TPanel;
    lblPriTable: TLabel;
    btnAddPri: TButton;
    btnDeleteSelectedPrior: TButton;
    pnlLower: TPanel;
    lblPriGpsTable: TLabel;
    btnAddPriGp: TButton;
    btnDeleteSelPriGps: TButton;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    btnConfigure: TButton;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    rbwdgPri: TRbwDataGrid4;
    rbwdgPriGps: TRbwDataGrid4;
    btnRenameGp: TButton;
    procedure btnAddPriClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure btnDeleteSelectedPriorClick(Sender: TObject);
    procedure btnAddPriGpClick(Sender: TObject);
    procedure btnDeleteSelPriGpsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnConfigureClick(Sender: TObject);
    procedure rbwdgPriMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbwdgPriExit(Sender: TObject);
    procedure rbwdgPriSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rbwdgPriStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rbwdgPriGpsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbwdgPriGpsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rbwdgPriGpsEndUpdate(Sender: TObject);
    procedure rbwdgPriGpsPopulate(const psSource: TPriSet);
    procedure rbwdgPriGpsExit(Sender: TObject);
    procedure rbwdgPriGpsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure rbwdgPriGpsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rbwdgPriGpsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rbwdgPriGpsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure btnRenameGpClick(Sender: TObject);
    procedure JvNetscapeSplitter1Moved(Sender: TObject);
  private
    { Private declarations }
    IgnoreGpsStateChange, PriGpCellChanged: boolean;
    PriGpCellCurrent: GridCell;
    PriGpCellText: string;
    PriGpSelected: string;
    procedure AssignPrior;
    procedure AssignPriGps;
    procedure CheckPriGpNames;
    procedure CountPriLines(const RbwGrid: TRbwDataGrid4; var NLines: integer;
                            var NNames: integer; var FirstBlankRow: integer);
    procedure InitializePriTables;
    procedure NumberRows;
    function SavePri(const IgnoreErrors: boolean): boolean;
    function SavePriData(const IgnoreErrors: boolean): boolean;
    function SavePriGps: boolean;
    procedure OpenConfigPriForm;
  public
    { Public declarations }
    PriorDataChanged: boolean;
    PriorSetupLocal: TPriorSetup;
    PriorSetupLocalChanged: boolean;
    PriSetLocal: TPriSet;
    PriGpsLocal: TPriSet;
    PriLocalChanged: boolean;
    PriGpsLocalChanged: boolean;
    procedure rbwdgPriPopulate(psSource: TPriSet);
  end;

var
  FormPriorInfo: TFormPriorInfo;

implementation

{$R *.dfm}
var
  UsePriGpsLocal: boolean;
  NumGpsLocal: integer;
  PriCellSelected: boolean = False;
  PriTableCol0Width: integer;

procedure TFormPriorInfo.AssignPriGps;
var
  ICol, IPA, IGp, IRow, NGps: integer;
  TempCaption, TempString: string;
  piatTemp: TPriAttType;
  ClearedLocalGps: boolean;
begin
// Assign prior groups and attributes in rbwdgPriGps to PriGpsLocal.
  ClearedLocalGps := False;
  NGps := rbwdgPriGps.RowCount - 1;
  if (not (PriGpsLocal.Count = NGps)) then
    begin
      PriGpsLocal.Clear;
      ClearedLocalGps := True;
    end;
  if NGps > 0 then
    begin
      for IGp := 0 to NGps - 1 do
        begin
          if ClearedLocalGps then PriGpsLocal.Add;
          IRow := IGp+1;
          TempString := rbwdgPriGps.Cells[0,IRow];
          PriGpsLocal.Items[IGp].Name := TempString;
          // Assign attributes to this prior group, as defined in table.
          // Loop through columns (prior-group attributes).
          for ICol := 0 to rbwdgPriGps.ColCount - 1 do
            begin
              TempCaption := rbwdgPriGps.Cells[ICol,0];
              // Find index of dependent attribute that has this caption.
              IPA := PosPriCap(TempCaption);
              // Assign data grid value to dependent attribute.
              piatTemp := PriGpsLocal.Items[IGp].AllAtts[IPA].PriAttType;
              if (piatTemp = piatUseFlag) then
                begin
                  PriGpsLocal.Items[IGp].AllAtts[IPA].Text :=
                      BooleanToYesOrNo(rbwdgPriGps.Checked[ICol,IRow]);
                end
              else
                begin
                  TempString := rbwdgPriGps.Cells[ICol, IRow];
                  PriGpsLocal.Items[IGp].AllAtts[IPA].Text := TempString;
                end;
            end;
        end;
      if ClearedLocalGps then
        begin
          TempString := 'Need to add code in TFormPriorInfo.AssignPriGps to restore hidden attributes';
          ShowMessage(TempString);
        end;
    end;
end;

procedure TFormPriorInfo.AssignPrior;
var
  ICol, IPA, IPri, IRow, NPri: integer;
  Caption, Msg, TempString: string;
  ClearedLocalSet: boolean;
begin
  ClearedLocalSet := False;
  NPri := rbwdgPri.RowCount - 1;
  if (not (PriSetLocal.Count = NPri)) then
    begin
      PriSetLocal.Clear;
      ClearedLocalSet := True;
    end;
  if (NPri = 1) and (IsBlank(rbwdgPri.Cells[1,1])) then
    NPri := 0;
  if NPri > 0 then
    begin
      for IPri := 0 to NPri - 1 do
        begin
          IRow := IPri+1;
          TempString := rbwdgPri.Cells[1,IRow];
          if J_Valid_Name(TempString,MaxLenDepName) then
            begin
              if ClearedLocalSet then PriSetLocal.Add;
              PriSetLocal.Items[IPri].Name := TempString;
              // Assign attributes to this prior, as defined in table.
              // Loop through columns (prior attributes).
              for ICol := 1 to rbwdgPri.ColCount - 1 do
                begin
                  Caption := rbwdgPri.Cells[ICol,0];
                  // Find index of prior attribute that has this caption.
                  IPA := PosPriCap(Caption);
                  // Assign data grid value to prior attribute.
                  TempString := rbwdgPri.Cells[ICol, IRow];
                  PriSetLocal.Items[IPri].AllAtts[IPA].Text := TempString;
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

procedure TFormPriorInfo.btnAddPriClick(Sender: TObject);
var
  ModRes: integer;
begin
  AssignPrior;
  frmAddParOrDep := TfrmAddParOrDep.Create(Self);
  frmAddParOrDep.PDUse := pduPrior;
  PriSetTemp.Assign(PriSetLocal);
  PriGpsTemp.Assign(PriGpsLocal);
  ModRes := frmAddParOrDep.ShowModal;
  if ModRes = mrOK then
    begin
      PriSetLocal.Assign(PriSetTemp);
      PriGpsLocal.Assign(PriGpsTemp);
      PriLocalChanged := True;
      rbwdgPriPopulate(PriSetLocal);
    end;
  FreeAndNil(frmAddParOrDep);
end;

procedure TFormPriorInfo.btnAddPriGpClick(Sender: TObject);
// Add a prior info group.
var
  LocalFrmAddGroup: TfrmAddGroup;
  NGpsOld: integer;
  ModRes: integer;
begin
//  PriGpsCurrent.Assign(PriGpsLocal);
  NGpsOld := PriGpsLocal.Count;
  LocalFrmAddGroup := TfrmAddGroup.Create(Self);
  LocalFrmAddGroup.GpUse := guPriGroup;
  PriGpsTemp.Assign(PriGpsLocal);
  ModRes := LocalFrmAddGroup.ShowModal;
  if ModRes = mrOK then
    begin
      if NGpsOld <> PriGpsTemp.Count then
        begin
          PriGpsLocal.Assign(PriGpsTemp);
          PriGpsLocalChanged := True;
          rbwdgPriGpsPopulate(PriGpsLocal);
          rbwdgPriPopulate(PriSetLocal);  // TODO 2 : Is this needed?
        end;
    end;
  FreeAndNil(LocalFrmAddGroup);
end;

procedure TFormPriorInfo.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPriorInfo.btnConfigureClick(Sender: TObject);
begin
  OpenConfigPriForm;
  rbwdgPriPopulate(PriSetLocal);
  rbwdgPriGpsPopulate(PriGpsLocal);
  rbwdgPri.Invalidate;
  rbwdgPri.Repaint;
  rbwdgPriGps.Invalidate;
  rbwdgPriGps.Repaint;
end;

procedure TFormPriorInfo.btnDeleteSelectedPriorClick(Sender: TObject);
var
  DelRow, I, SelRowFirst, SelRowLast: integer;
  TempPriSet: TPriSet;
  Messg: string;
begin
  SelRowFirst := rbwdgPri.Selection.Top;
  SelRowLast := rbwdgPri.Selection.Bottom;
  if SelRowFirst > 0 then
    begin
      DelRow := 0;
      for I := rbwdgPri.RowCount - 1 downto 1 do
        begin
          if (RowContainsSelectedCell(rbwdgPri,I))
                or ((I >= SelRowFirst) and (I <= SelRowLast)) then
            begin
              if rbwdgPri.RowCount > 2 then
                begin
                  rbwdgPri.DeleteRow(I);
                end
              else
                begin
                  rbwdgPri.Cells[1,1] := '';
                  TempPriSet := TPriSet.Create;
                  rbwdgPriPopulate(TempPriSet);
                  TempPriSet.Free;
                end;
              DelRow := I;
            end;
        end;
      if DelRow > 0 then
        begin
          PriLocalChanged := True;
          NumberRows;
          if (DelRow < rbwdgPri.RowCount - 1) then
            begin
              rbwdgPri.Row := DelRow;
            end;
          ClearAllSelections(rbwdgPri);
          AssignPrior;
        end;
    end
  else
    begin
      Messg := 'Select prior-information item(s) to be deleted';
      ShowMessage(Messg);
    end;
end;

procedure TFormPriorInfo.btnDeleteSelPriGpsClick(Sender: TObject);
// Delete selected prior info groups, but only if group is unused.
var
  DelRow, I, K, SelRowFirst, SelRowLast: integer;
  GpName: string;
  Messg: string;
  TempPriGpSet: TPriSet;
begin
  ClearAllSelections(rbwdgPri);
  rbwdgPri.EditorMode := False;
  if (rbwdgPriGps.RowCount > 2) and
      (CountSelectedRows(rbwdgPriGps) < rbwdgPriGps.RowCount-1) then
    begin
      SelRowFirst := rbwdgPriGps.Selection.Top;
      SelRowLast := rbwdgPriGps.Selection.Bottom;
      if SelRowFirst > 0 then
        begin
          DelRow := 0;
          for I := rbwdgPriGps.RowCount - 1 downto 1 do
            begin
              if (RowContainsSelectedCell(rbwdgPriGps,I))
                    or ((I >= SelRowFirst) and (I <= SelRowLast)) then
                begin
                  GpName := rbwdgPriGps.Cells[0,I];
                  K := PriSetLocal.NumPriByGroup(GpName);
                  if K = 0 then
                    begin
                      if rbwdgPriGps.RowCount > 2 then
                        begin
                          rbwdgPriGps.DeleteRow(I);
                        end
                      else
                        begin    // TODO 3 : I think this code is unneeded.
                          rbwdgPriGps.Cells[0,1] := '';
                          TempPriGpSet := TPriSet.Create;
                          rbwdgPriGpsPopulate(TempPriGpSet);
                          TempPriGpSet.Free;
                        end;
                      DelRow := I;
                      // Delete corresponding item in PriGpsLocal.
                      PriGpsLocal.Delete(I-1);
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
              ClearAllSelections(rbwdgPriGps);
              AssignPriGps;
//              PriGpsCurrent.Assign(PriGpsLocal);
              PriGpsLocalChanged := True;
              rbwdgPriPopulate(PriSetLocal);
            end;
        end
      else
        begin
          Messg := 'Select prior-information group(s) to be deleted';
          ShowMessage(Messg);
        end;
    end
  else
    begin
      Messg := 'At least one group must be defined.  Group(s) not deleted';
      ShowMessage(Messg);
    end;
end;

procedure TFormPriorInfo.btnOKClick(Sender: TObject);
var
  IgnoreErrors, OK: boolean;
  ErrRow: integer;
  ErrMsg, ErrName: string;
  Selection: TGridRect;
begin
  IgnoreErrors := False;
  ModalResult := mrNo;
  OK := CheckNamesInColumn(rbwdgPri,1,MaxLenDepName,ErrName,ErrRow);
  if (rbwdgPri.RowCount = 2) and (IsBlank(ErrName)) then OK := True;
  if not OK then
    begin
        ErrMsg := 'Invalid prior-information name: ' + ErrName;
        ShowMessage(ErrMsg);
        Selection.Left := 1;
        Selection.Right := 1;
        Selection.Top := ErrRow;
        Selection.Bottom := ErrRow;
        rbwdgPri.Selection := Selection;
        rbwdgPri.TopRow := ErrRow;
        rbwdgPri.SetFocus;
        ModalResult := mrNone;
    end;
  if OK then
    begin
      // Check prior info group names
      OK := CheckNamesInColumn(rbwdgPriGps,0,MaxLenDepName,ErrName,ErrRow);
      if (rbwdgPriGps.RowCount = 2) and (IsBlank(ErrName)) then OK := True;
      if not OK then
        begin
          ErrMsg := 'Invalid name: ' + ErrName;
          ShowMessage(ErrMsg);
          OK := False;
          Selection.Left := 0;
          Selection.Right := 0;
          Selection.Top := ErrRow;
          Selection.Bottom := ErrRow;
          rbwdgPriGps.Selection := Selection;
          rbwdgPriGps.TopRow := ErrRow;
          rbwdgPriGps.SetFocus;
          ModalResult := mrNone;
        end;
    end;
  if OK then
    begin
      if SavePriData(IgnoreErrors) then
        begin
          if PriorSetupLocalChanged or PriLocalChanged or PriGpsLocalChanged then
            begin
//              PriSetCurrent.Assign(PriSetLocal);
//              PriGpsCurrent.Assign(PriGpsLocal);
//              PriorSetupCurrent.Assign(PriorSetupLocal);
              PriorDataChanged := True;
            end;
          ModalResult := mrOK;
          Close;                 // if FFStyle is modal, Close changes ModalResult to mrCancel (=2)
          ModalResult := mrOK;
        end
      else
        begin
          ModalResult := mrNone;
        end;
    end;
end;

procedure TFormPriorInfo.btnRenameGpClick(Sender: TObject);
var
  I, ModRes, SelRowFirst, SelRowLast: integer;
  Messg, NewGpName, OldGpName: string;
begin
  ClearAllSelections(rbwdgPri);
  rbwdgPri.EditorMode := False;
  SelRowFirst := rbwdgPriGps.Selection.Top;
  SelRowLast := rbwdgPriGps.Selection.Bottom;
  if (SelRowFirst = SelRowLast) and (SelRowFirst > 0) then
    begin
      I := SelRowFirst;
      OldGpName := rbwdgPriGps.Cells[0,I];
      PriGpCellCurrent.Row := I;
      PriGpCellCurrent.Column := 0;
      PriGpCellCurrent.TextOld := OldGpName;
      PriGpCellCurrent.TextNew := OldGpName;
      FormRenameGroup.CellData := PriGpCellCurrent;
      FormRenameGroup.GpUse := guPriGroup;
      FormRenameGroup.edtNewGroup.MaxLength := MaxLenGpName;
      ModRes := FormRenameGroup.ShowModal;
      if ModRes = mrOk then
        begin
          NewGpName := FormRenameGroup.CellData.TextNew;
          rbwdgPriGps.Cells[PriGpCellCurrent.Column,PriGpCellCurrent.Row]
                := NewGpName;
          PriSetLocal.ChangeGroupNames(OldGpName,NewGpName);
          rbwdgPriPopulate(PriSetLocal);
          ClearAllSelections(rbwdgPriGps);
          AssignPriGps;
          //PriGpsCurrent.Assign(PriGpsLocal);
          PriGpsLocalChanged := True;
          PriLocalChanged := True;
        end
      else
        begin
          rbwdgPriGps.Cells[PriGpCellCurrent.Column,PriGpCellCurrent.Row]
                := PriGpCellCurrent.TextOld;
        end;
      PriGpCellChanged := False;
    end
  else if SelRowFirst <= 0 then
    begin
      Messg := 'Select a prior-information group to be renamed';
      ShowMessage(Messg);
    end;
end;

procedure TFormPriorInfo.CheckPriGpNames;
var
  IRowCurr: Integer;
  OldGroup: string;
  K: Integer;
  Messg: string;
  NewGroup: string;
begin
  for IRowCurr := 1 to rbwdgPriGps.RowCount - 1 do
    begin
      OldGroup := PriGpsLocal.Items[IRowCurr - 1].Name;
      NewGroup := rbwdgPriGps.Cells[0,IRowCurr];
      if NewGroup <> OldGroup then
        begin
          // If group name has changed, ensure that old group name is unused.
          K := PriSetLocal.NumPriByGroup(OldGroup);
          if K > 0 then
            // Old group name is in use.
            begin
              Messg := 'Group name in use may not be changed: ' + OldGroup;
              ShowMessage(Messg);
              rbwdgPriGps.Cells[0, IRowCurr] := OldGroup;
            end
          else
          // Old group name not in use and may be changed
            begin
              // Check validity of new group name
              NewGroup := rbwdgPriGps.Cells[0, IRowCurr];
              if not J_Valid_Name(NewGroup,MaxLenGpName) then
                begin
                  if NewGroup <> '' then
                    begin
                      Messg := 'Invalid name: ' + NewGroup;
                      ShowMessage(Messg);
                    end;
                  rbwdgPriGps.Cells[0, IRowCurr] := OldGroup;
                end;
            end;
        end;
    end;
end;

procedure TFormPriorInfo.CountPriLines(const RbwGrid: TRbwDataGrid4; var NLines,
  NNames, FirstBlankRow: integer);
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

procedure TFormPriorInfo.OpenConfigPriForm;
var
  IgnoreErrors: Boolean;
  ModRes: Integer;
begin
  IgnoreErrors := True;
  SavePriData(IgnoreErrors);  // this is a boolean function
  FormConfigPriTables.PriorSetupConfigure.Assign(PriorSetupLocal);
  ModRes := FormConfigPriTables.ShowModal;
  if ModRes = mrOK then
    begin
      if FormConfigPriTables.PriorSetupConfigureChanged then
        begin
          PriorSetupLocal.Assign(FormConfigPriTables.PriorSetupConfigure);
          PriorSetupLocalChanged := True;
        end;
    end;
end;

procedure TFormPriorInfo.FormCreate(Sender: TObject);
begin
  //CenterForm(self);
  Left := 35;
  Top := 45;
  PriorSetupLocal := TPriorSetup.Create(self);
  PriSetLocal := TPriSet.Create;
  PriGpsLocal := TPriSet.Create;
end;

procedure TFormPriorInfo.FormDestroy(Sender: TObject);
begin
  PriorSetupLocal.Free;
  PriSetLocal.Free;
  PriGpsLocal.Free;
end;

procedure TFormPriorInfo.FormResize(Sender: TObject);
var
  AvailableHeight: integer;
begin
  AvailableHeight := self.Height - pnlUpper.Height - StatusBar1.Height - 37;
  if pnlLower.Height > AvailableHeight then
    pnlLower.Height := AvailableHeight;

end;

procedure TFormPriorInfo.FormShow(Sender: TObject);
begin
  InitializePriTables;
  PriorSetupLocalChanged := False;
  PriLocalChanged := False;
  PriGpsLocalChanged := False;
end;

procedure TFormPriorInfo.InitializePriTables;
var
  I: Integer;
  DefText: string;
begin
  rbwdgPri.ColWidths[0] := PriTableCol0Width;
  rbwdgPriPopulate(PriSetLocal);
  rbwdgPriGpsPopulate(PriGpsLocal);
  PriLocalChanged := False;
  PriGpsLocalChanged := False;
  PriorSetupLocalChanged := False;
  NumGpsLocal := PriGpsLocal.Count;
  with PriGpsLocal do
  begin
    if Count = 1 then
    begin
      if (Items[0].Name = 'DefaultPrior') then
      begin
        for I := 0 to NumPriAttributes - 1 do
        begin
          DefText := Items[0].AllAtts.Items[I].DefaultText;
        end;
      end;
    end;
  end;
  NumGpsLocal := rbwdgPriGps.RowCount - 1;
  StatusBar1.Panels[0].Text := 'Click in a column to display hint';
  ClearAllSelections(rbwdgPri);
end;

procedure TFormPriorInfo.JvNetscapeSplitter1Moved(Sender: TObject);
begin
  ClearAllSelections(rbwdgPri);
  ClearAllSelections(rbwdgPriGps);
end;

procedure TFormPriorInfo.NumberRows;
var
  I, NR: integer;
begin
  NR:= rbwdgPri.RowCount;
  for I := 1 to NR - 1 do
    rbwdgPri.Cells[0,I] := IntToStr(I);
end;

procedure TFormPriorInfo.rbwdgPriExit(Sender: TObject);
begin
  AssignPrior;
  if PriSetLocal <> PCurrent.PriSet then
    begin
      PriLocalChanged := True;
    end;
  rbwdgPri.Invalidate;
  rbwdgPri.Repaint;
end;

procedure TFormPriorInfo.rbwdgPriGpsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  Invalidate;
  Repaint;
end;

procedure TFormPriorInfo.rbwdgPriGpsEndUpdate(Sender: TObject);
begin
  if PriGpCellChanged then
    begin
      if PriGpCellCurrent.Column = 0 then
        begin
          rbwdgPriGps.Cells[PriGpCellCurrent.Column,PriGpCellCurrent.Row]
                    := PriGpCellCurrent.TextOld;
          PriGpCellChanged := False;
        end;
    end;
end;

procedure TFormPriorInfo.rbwdgPriGpsExit(Sender: TObject);
var
  FirstBlankGpRow, NGpLines, NGpNames: integer;
  OK: boolean;
begin
  // Check group table entries
  // Count lines not filled with blanks in groups table
  // If NGpNames < NGpLines, there's at least one blank group name
  CountPriLines(rbwdgPriGps,NGpLines,NGpNames,FirstBlankGpRow);
  if NGpLines = NGpNames then
    begin
      OK := True;
    end
  else
    begin
      ShowMessage('Error in Prior Information Groups Table: At least one group is unnamed');
      rbwdgPriGps.SetFocus;
      rbwdgPriGps.Column := 0;
      rbwdgPriGps.Row := FirstBlankGpRow;
      OK := False;
    end;
  if OK then
    begin
      CheckPriGpNames;
      AssignPriGps;
    end;
  rbwdgPriGps.Invalidate;
  rbwdgPriGps.Repaint;
end;

procedure TFormPriorInfo.rbwdgPriGpsGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  PriGpCellText := Value;
  PriGpCellChanged := False;
  if ACol = 0 then
    begin
      PriGpSelected := Value;
    end;
end;

procedure TFormPriorInfo.rbwdgPriGpsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    begin
      SetColorSelectedRow(rbwdgPriGps,False);
    end
  else
    begin
      SetColorSelectedRow(rbwdgPriGps,True);
    end;
  if ssShift in Shift then
    begin
      AllowEditing(rbwdgPriGps,False);
      SetColorSelectedRow(rbwdgPriGps,False);
    end
  else
    begin
      AllowEditing(rbwdgPriGps,True);
    end;
end;

procedure TFormPriorInfo.rbwdgPriGpsPopulate(const psSource: TPriSet);
var
  I, IA, IGP, IP1, J, NRows, NumCols: integer;
  BlankString, TempString: string;
  setATDisplayGps: set of TPriAttType;
  piatTemp: TPriAttType;
  TempBool: boolean;
begin
  BlankString := '';
  if PriGpsLocal.Count = 0 then
    begin
      PriGpsLocal.SetGpDefault;
    end;
  case PCurrent.ActiveApp of
    aaUcode:
      begin
        setATDisplayGps := [piatStatistic, piatStatFlag, piatUseFlag,
            piatPlotSymbol, piatWtMultiplier, piatCovMatrix];
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
  NumCols := 1; // Always one column for prior info group
  for I := 0 to NumPriAttributes - 1 do
    if (PriorSetupLocal.PriAttributes[I].PriAttType in setATDisplayGps) and
       (PriorSetupLocal.PriAttributes[I].ControlMethod = cmByGroup) then
            NumCols := NumCols + 1;
  rbwdgPriGps.ColCount := NumCols;
  if rbwdgPriGps.ColCount = 1 then
    begin
      btnRenameGp.Enabled := False;
    end
  else
    begin
      rbwdgPriGps.FixedCols := 1;
      btnRenameGp.Enabled := True;
    end;
  // Clear table headings and cells
  for I := 0 to rbwdgPriGps.ColCount - 1 do
    begin
      rbwdgPriGps.Columns[I].CaptionAlignment := taLeftJustify;
      rbwdgPriGps.Cells[I,0] := BlankString;
      for J := 0 to rbwdgPriGps.RowCount - 1 do
        begin
          try
            rbwdgPriGps.Cells[I, J] := BlankString;
          except
            On E: EListError do
              ShowMessage('Caught EListError exception');
          end;
        end;
    end;
  // After all cells are cleared, assign ColWidths to minimal width.
  for I := 0 to rbwdgPriGps.ColCount - 1 do
    begin
      rbwdgPriGps.Columns[I].ComboUsed := False;
      rbwdgPriGps.ColWidths[I] := rbwdgPriGps.DefaultColWidth;
      rbwdgPriGps.Columns[I].AutoAdjustColWidths := True;
    end;
  // Populate column headings and assign properties for selected columns
  NRows := psSource.Count + 1;
  if NRows < 2 then NRows := 2;
  rbwdgPriGps.RowCount := NRows;
  J := 0;
  rbwdgPriGps.Cells[J,0] := 'Group Name';
  for I := 0 to NumPriAttributes - 1 do
    if (PriorSetupLocal.PriAttributes[I].ControlMethod = cmByGroup) and
       (PriorSetupLocal.PriAttributes[I].PriAttType in setATDisplayGps) then
    begin
      case PriorSetupLocal.PriAttributes[I].PriAttType of
        piatStatistic: begin
            J := J + 1;
            rbwdgPriGps.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPriGps.Columns[J].Format := rcf4Real;
            rbwdgPriGps.Columns[J].ComboUsed := False;
            rbwdgPriGps.Columns[J].LimitToList := False;
          end;
        piatStatFlag: begin
            J := J + 1;
            rbwdgPriGps.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPriGps.Columns[J].Format := rcf4String;
            rbwdgPriGps.Columns[J].ComboUsed := True;
            rbwdgPriGps.Columns[J].PickList := slPriStatFlag;
            rbwdgPriGps.Columns[J].LimitToList := True;
          end;
        piatUseFlag: begin
            J := J + 1;
            rbwdgPriGps.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPriGps.Columns[J].Format := rcf4Boolean;
            rbwdgPriGps.Columns[J].ComboUsed := False;
            rbwdgPriGps.Columns[J].LimitToList := False;
          end;
        piatPlotSymbol: begin
            J := J + 1;
            rbwdgPriGps.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPriGps.Columns[J].Format := rcf4Integer;
            rbwdgPriGps.Columns[J].ComboUsed := False;
            rbwdgPriGps.Columns[J].LimitToList := False;
          end;
        piatWtMultiplier: begin
            J := J + 1;
            rbwdgPriGps.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPriGps.Columns[J].Format := rcf4Real;
            rbwdgPriGps.Columns[J].ComboUsed := False;
            rbwdgPriGps.Columns[J].LimitToList := False;
          end;
        piatCovMatrix: begin
            J := J + 1;
            rbwdgPriGps.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPriGps.Columns[J].Format := rcf4String;
          end;
      end;
    end;
  // Populate table cells
  // Loop through groups; each iteration populates a row.
  for IGP := 0 to psSource.Count - 1 do
  begin
    IP1 := IGP + 1;
    rbwdgPriGps.Cells[0,IP1] := psSource.Items[IGP].Name;
    J := 0;
    for IA := 0 to NumPriAttributes - 1 do
    begin
      piatTemp := PriorSetupLocal.PriAttributes[IA].PriAttType;
      if (PriorSetupLocal.PriAttributes[IA].ControlMethod = cmByGroup) and
         (piatTemp in setATDisplayGps) then
        begin
          J := J + 1;
          TempString := psSource.Items[IGP].AllAtts[IA].Text;
          if TempString = '' then
            TempString := psSource.Items[IGP].AllAtts.Items[IA].DefaultText;
          piatTemp := psSource.Items[IGP].AllAtts[IA].PriAttType;
          if piatTemp = piatUseFlag then
            begin
              // Check or uncheck box depending on boolean value.
              TempBool := YesOrNoToBoolean(TempString);
              IgnoreGpsStateChange := True;
              rbwdgPriGps.Checked[J,IP1] := TempBool;
              IgnoreGpsStateChange := False;
            end
          else
            begin
              rbwdgPriGps.Cells[J,IP1] := TempString;
            end;
        end;
    end;
  end;
  NumberRows;
  rbwdgPriGps.Invalidate;
end;

procedure TFormPriorInfo.rbwdgPriGpsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Hint: string;
  LocalRect: TGridRect;
begin
  if (ACol = rbwdgPriGps.Column) and (ACol >= 0) and (ARow >= 0) then
    begin
      Hint := GetPriHint(rbwdgPriGps, ACol);
      StatusBar1.Panels[0].Text := ' ' + Hint;
      if ACol = 0 then
        begin
          LocalRect.Left := 0;
          LocalRect.Right := rbwdgPriGps.ColCount - 1;
          LocalRect.Top := ARow;
          LocalRect.Bottom := ARow;
          rbwdgPriGps.Selection := LocalRect;
        end;
    end;
end;

procedure TFormPriorInfo.rbwdgPriGpsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if Value <> PriGpCellText then
    begin
      PriGpCellCurrent.Row := ARow;
      PriGpCellCurrent.Column := ACol;
      PriGpCellCurrent.Checked := rbwdgPriGps.Checked[ACol,ARow];
      PriGpCellCurrent.TextOld := PriGpCellText;
      PriGpCellCurrent.TextNew := Value;
      PriGpCellChanged := True;
      PriGpsLocalChanged := True;
    end;
end;

procedure TFormPriorInfo.rbwdgPriGpsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  if not IgnoreGpsStateChange then
    begin
      AssignPriGps;
      PriGpsLocalChanged := True;
    end;
end;

procedure TFormPriorInfo.rbwdgPriMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    begin
      SetColorSelectedRow(rbwdgPri,False);
    end
  else
    begin
      SetColorSelectedRow(rbwdgPri,True);
    end;
  if ssShift in Shift then
    begin
      AllowEditing(rbwdgPri,False);
      SetColorSelectedRow(rbwdgPri,False);
    end
  else
    begin
      AllowEditing(rbwdgPri,True);
    end;
end;

procedure TFormPriorInfo.rbwdgPriPopulate(psSource: TPriSet);
var
  PiatPos, GpCol, I, IA, IP1, J, NRows, NumCols: integer;
  setATDisplay: set of TPriAttType;
  GpList: TStringList;
  TempString: string;
begin
  GpList := TStringList.Create;
  try
  for I := 0 to PriGpsLocal.Count - 1 do
    GpList.Add(PriGpsLocal.Items[I].Name);
  case PCurrent.ActiveApp of
  aaUcode:
    begin
      setATDisplay := [piatPriValue, piatStatistic,
          piatStatFlag, piatEquation, piatUseFlag, piatPlotSymbol,
          piatWtMultiplier, piatCovMatrix];
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
  NumCols := 3; // Always columns for prior item number, name, and group
  for I := 0 to NumPriAttributes - 1 do
    if (PriorSetupLocal.PriAttributes[I].PriAttType in setATDisplay) and
       (PriorSetupLocal.PriAttributes[I].ControlMethod = cmByItem) then
            NumCols := NumCols + 1;
  rbwdgPri.ColCount := NumCols;
  // Clear table headings and cells
  for I := 0 to rbwdgPri.ColCount - 1 do
    begin
      rbwdgPri.Cells[I,0] := '';
      rbwdgPri.Columns[I].CaptionAlignment := taLeftJustify;
//      rbwdgPri.Columns[I].AutoAdjustColWidths := True;
      for J := 1 to rbwdgPri.RowCount - 1 do
          rbwdgPri.Cells[I, J] := '';
  end;
  // After all cells are cleared, assign ColWidths to minimal width.
  for I := 0 to rbwdgPri.ColCount - 1 do
    begin
      rbwdgPri.Columns[I].ComboUsed := False;
      rbwdgPri.ColWidths[I] := rbwdgPri.DefaultColWidth;
      rbwdgPri.Columns[I].AutoAdjustColWidths := True;
    end;
  // Populate column headings and assign properties for selected columns
  NRows := psSource.Count + 1;
  if NRows < 2 then NRows := 2;
  rbwdgPri.RowCount := NRows;
  rbwdgPri.Cells[0,0] := 'Prior #';

  rbwdgPri.Cells[1,0] := 'Prior Name';
  rbwdgPri.Columns[1].Format := rcf4String;
  rbwdgPri.Columns[1].ComboUsed := False;

  rbwdgPri.Cells[2,0] := 'Group Name';
  rbwdgPri.Columns[2].Format := rcf4String;
  rbwdgPri.Columns[2].ComboUsed := True;
  rbwdgPri.Columns[2].PickList := GpList;
  rbwdgPri.Columns[2].LimitToList := True;

  GpCol := 0;
  J := 2;
  for I := 0 to NumPriAttributes - 1 do
    if (PriorSetupLocal.PriAttributes[I].ControlMethod = cmByItem) and
       (PriorSetupLocal.PriAttributes[I].PriAttType in setATDisplay) then
    begin
      case PriorSetupLocal.PriAttributes[I].PriAttType of
        piatPriorName: begin
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4String;
            rbwdgPri.Columns[J].ComboUsed := False;
            rbwdgPri.Columns[J].LimitToList := False;
          end;
        piatPriValue: begin
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4Real;
            rbwdgPri.Columns[J].ComboUsed := False;
            rbwdgPri.Columns[J].LimitToList := False;
          end;
        piatStatistic: begin
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4Real;
            rbwdgPri.Columns[J].ComboUsed := False;
            rbwdgPri.Columns[J].LimitToList := False;
          end;
        piatStatFlag: begin
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4String;
            rbwdgPri.Columns[J].ComboUsed := True;
            rbwdgPri.Columns[J].PickList := slPriStatFlag;
            rbwdgPri.Columns[J].LimitToList := True;
          end;
        piatUseFlag: begin
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4Boolean;
            rbwdgPri.Columns[J].ComboUsed := False;
            rbwdgPri.Columns[J].LimitToList := False;
          end;
        piatPlotsymbol: begin
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4Integer;
            rbwdgPri.Columns[J].ComboUsed := False;
            rbwdgPri.Columns[J].LimitToList := False;
          end;
        piatEquation: begin
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4String;
            rbwdgPri.Columns[J].ComboUsed := False;
            rbwdgPri.Columns[J].LimitToList := False;
          end;
        piatWtMultiplier: begin
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4Real;
            rbwdgPri.Columns[J].ComboUsed := False;
            rbwdgPri.Columns[J].LimitToList := False;
          end;
        piatCovMatrix: begin
            { TODO 3 -cPrior Info : Code this option to display list of defined CovMatrix names }
            J := J + 1;
            rbwdgPri.Cells[J,0] :=
                 PriorSetupLocal.PriAttributes[I].Caption;
            rbwdgPri.Columns[J].Format := rcf4String;
            rbwdgPri.Columns[J].ComboUsed := False;
          end;
      end;
    end;
  // Populate table cells
  // Loop through groups; each iteration populates a row
  PiatPos := PriAttPos(piatGroupName);
  for I := 0 to psSource.Count - 1 do
  begin
    IP1 := I + 1;
    rbwdgPri.Cells[0,IP1] := IntToStr(IP1);
    rbwdgPri.Cells[1,IP1] := psSource.Items[I].Name;
    rbwdgPri.Cells[2,IP1] := psSource.Items[I].AllAtts.Items[PiatPos].Text;
    J := 2;
    for IA := 0 to NumPriAttributes - 1 do
    begin
      if (PriorSetupLocal.PriAttributes[IA].ControlMethod = cmByItem) and
         (PriorSetupLocal.PriAttributes[IA].PriAttType in setATDisplay) then
        begin
          J := J + 1;
          TempString := psSource.Items[I].AllAtts[IA].Text;
          if TempString = '' then
            TempString := psSource.Items[I].AllAtts.Items[IA].DefaultText;
          rbwdgPri.Cells[J,IP1] := TempString;
          if (J = GpCol) and (rbwdgPri.Cells[J,IP1] = '') then
            begin
              rbwdgPri.Cells[J,IP1] := rbwdgPriGps.Cells[1,1];
            end;
        end;
    end;
  end;
  NumberRows;
  // Set selections to be invisible and invalid
  ClearAllSelections(rbwdgPri);
  //
  SetColorSelectedRow(rbwdgPri,True);
  StatusBar1.Panels[0].Text := '';
  rbwdgPri.Invalidate;
  finally
    GpList.Free;
  end;
end;

procedure TFormPriorInfo.rbwdgPriSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = rbwdgPri.Column) and (ACol >= 0) and (ARow >= 0) then
    begin
      StatusBar1.Panels[0].Text := ' ' + GetPriHint(rbwdgPri, ACol);
      if ARow > 0 then
        begin
          PriCellSelected := True;
        end
      else
        PriCellSelected := False;
    end;
end;

procedure TFormPriorInfo.rbwdgPriStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  PriLocalChanged := True;
end;

function TFormPriorInfo.SavePri(const IgnoreErrors: boolean): boolean;
var
  I, IAtt, IErr, II, IRow, J, JJ, NCols, NPri, NRdata: integer;
  Caption, Msg: string;
  piatTemp: TPriAttType;
  ClearedLocalSet: boolean;
begin
  result := True;
  ClearedLocalSet := False;
  // need code
  IErr := 0;
  Msg := '';
  // Save prediction information
  NRdata := rbwdgPri.RowCount - 1;
  NPri := NRdata;
  NCols := rbwdgPri.ColCount;
  if not IgnoreErrors then
    begin
      // Ensure all rows are fully populated
      NPri := 0;
      for IRow := 1 to NRdata do // IRow is the row index in the prior info table
        begin
          if rbwdgPri.Cells[1,IRow] <> '' then // Prior name
            begin
              for J := 2 to NCols - 1 do
                begin
                  // Find dependent attribute type
//                  Caption := rbwdgPri.Cells[J,0];
                  if (rbwdgPri.Cells[J,IRow] = '') and (rbwdgPri.Columns[J].Format <> rcf4Boolean) then
                    begin
                      Msg := 'Error: Missing data for prior info item ' + IntToStr(IRow);
                      IErr := 1;
                    end;
                end;
              NPri := NPri+1;
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
      if (not (NPri = PriSetLocal.Count)) then
        begin
          PriSetLocal.Clear;
          ClearedLocalSet := True;
        end;
      for I := 1 to NPri do
        begin
          if ClearedLocalSet then PriSetLocal.Add;
          II := I-1;
          PriSetLocal.Items[II].Name := rbwdgPri.Cells[1,I];
          if I>1 then
            begin
              J := 1;
              while (J<I) and (IErr=0) do
                begin
                  JJ := J-1;
                  if SameText(PriSetLocal.Items[II].Name,PriSetLocal.Items[JJ].Name) then
                    begin
                      if not IgnoreErrors then
                        begin
                          IErr := I;
                          Msg := 'Error: Prior info items ' + IntToStr(J) + ' and ' + IntToStr(IErr)
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
              Caption := rbwdgPri.Cells[J,0];
              IAtt := PosPriCap(Caption);
              piatTemp := PriAttributeTypes[IAtt];
              for I := 1 to NPri do // loop through rows in this column
                begin
                  II := I-1;
                  PriSetLocal.Items[II].AllAtts.Items[IAtt].Text := rbwdgPri.Cells[J,I];
                  PriSetLocal.Items[II].AllAtts.Items[IAtt].PriAttType := piatTemp;
                end;
            end;
        end;
    end;
  if IErr = 0 then
    begin
//      PriSetTemp.Assign(PriSetLocal);
    end
  else
    begin
      result := False;
    end;
end;

function TFormPriorInfo.SavePriData(const IgnoreErrors: boolean): boolean;
var
  OK: Boolean;
begin
  OK := True;
  StatusBar1.Panels[0].Text := '';
  if PriLocalChanged or PriorSetupLocalChanged then
    begin
      if not SavePri(IgnoreErrors) then
        begin
          OK := False;
        end;
    end;
  if PriGpsLocalChanged or PriorSetupLocalChanged then
    begin
      if SavePriGps then
        begin
        // TODO 1 Assignments to "current" objects probably is wrong. 
//          PCurrent.PriGpSet.Assign(PriGpsLocal);
//          PriGpsCurrent.Assign(PriGpsLocal);
        end
      else
        begin
          OK := False;
        end;
    end;
  if PriorSetupLocalChanged then
    begin
        // TODO 1 Assignments to "current" objects probably is wrong. 
//      PCurrent.PriorSetup.Assign(PriorSetupLocal);
//      PriorSetupCurrent.Assign(PriorSetupLocal);
      PriorSetupChanged := True;
    end;
//  PCurrent.UsePriGps := UsePriGpsLocal;
  if OK then
    begin
      StatusBar1.Panels[0].Text := 'Data Saved';
    end;
  result := OK;
end;

function TFormPriorInfo.SavePriGps: boolean;
var
  I, IAtt, IErr, II, IRow, J, JJ, NCols, NGps, NRdata: integer;
  Caption, Msg: string;
  piatTemp: TPriAttType;
  ClearedLocalGps: boolean;
begin
  result := True;
  ClearedLocalGps := False;
  IErr := 0;
  Msg := '';
  // Save prior groups information
  NRdata := rbwdgPriGps.RowCount - 1;
  NGps := 0;
  NCols := rbwdgPriGps.ColCount;
  // Ensure all rows are fully populated
  for IRow := 1 to NRdata do // IRow is the row index in the prior groups table
    begin
      if rbwdgPriGps.Cells[0,IRow] <> '' then // Prior group name
        begin
          for J := 1 to NCols - 1 do
            begin
              if (rbwdgPriGps.Cells[J,IRow] = '') and (rbwdgPriGps.Columns[J].Format <> rcf4Boolean) then
                begin
                  Msg := 'Error: Missing data for group ' + rbwdgPriGps.Cells[0,IRow];
                  IErr := 1;
                end
            end;
          NGps := NGps + 1;
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
      if (not (NGps = PriGpsLocal.Count)) then
        begin
          PriGpsLocal.Clear;
          ClearedLocalGps := True;
        end;
      for I := 1 to NGps do
        begin
          if ClearedLocalGps then PriGpsLocal.Add;
          II := I-1;
          PriGpsLocal.Items[II].Name := rbwdgPriGps.Cells[0,I];
          //
          if I>1 then
            begin
              J := 1;
              while (J<I) and (IErr=0) do
                begin
                  JJ := J-1;
                  if SameText(PriGpsLocal.Items[II].Name, PriGpsLocal.Items[JJ].Name) then
                    begin
                      IErr := I;
                      Msg := 'Error: Duplicate group name: ' + PriGpsLocal.Items[II].Name;
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
              Caption := rbwdgPriGps.Cells[J,0];
              IAtt := PosPriCap(Caption);
              piatTemp := PriAttributeTypes[IAtt];
              for I := 1 to NGps do // loop through rows in this column.
                begin
                  II := I-1;
                  PriGpsLocal.Items[II].AllAtts.Items[IAtt].PriAttType := piatTemp;
                  if (rbwdgPriGps.Columns[J].Format <> rcf4Boolean) then
                    begin
                      PriGpsLocal.Items[II].AllAtts.Items[IAtt].Text := rbwdgPriGps.Cells[J,I];
                    end
                  else
                    begin
                      PriGpsLocal.Items[II].AllAtts.Items[IAtt].Text := BooleanToYesOrNo(rbwdgPriGps.Checked[J,I]);
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
      PriorDataChanged := True;
      PriGpsLocalChanged := True;
    end;
end;

procedure TFormPriorInfo.StatusBar1Click(Sender: TObject);
begin
  ShowMessage(StatusBar1.Panels[0].Text);
end;

end.
