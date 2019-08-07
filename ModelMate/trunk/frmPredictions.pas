unit frmPredictions;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DataGrid,
  DependentsUnit, GlobalBasicData, GlobalData, GlobalTypesUnit,
  JupiterUnit, ModelMateUtilities, frmConfigurePredTablesUnit,
  frmAddGroupUnit, frmAddParOrDepUnit, ComCtrls, ExtCtrls, JvExExtCtrls,
  JvNetscapeSplitter, Utilities, Menus, Buttons, RbwDataGrid4, frmRenameGroup;

type
  TFormPredictions = class(TForm)
    StatusBar1: TStatusBar;
    pnlLower: TPanel;
    lblPredGpsTable: TLabel;
    btnAddPredGp: TButton;
    btnDeleteSelPredGps: TButton;
    pnlUpper: TPanel;
    lblPredTable: TLabel;
    btnAddPred: TButton;
    btnDeleteSelectedPredictions: TButton;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    btnConfigure: TButton;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    rbwdgPred: TRbwDataGrid4;
    rbwdgPredGps: TRbwDataGrid4;
    btnRenameGp: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnAddPredClick(Sender: TObject);
    procedure btnAddPredGpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnDeleteSelectedPredictionsClick(Sender: TObject);
    procedure btnDeleteSelPredGpsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PredictionsTables1Click(Sender: TObject);
    procedure btnConfigureClick(Sender: TObject);
    procedure rbwdgPredMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbwdgPredExit(Sender: TObject);
    procedure rbwdgPredSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rbwdgPredStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rbwdgPredGpsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbwdgPredGpsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rbwdgPredGpsEndUpdate(Sender: TObject);
    procedure rbwdgPredGpsPopulate(const dsSource: TDepSet);
    procedure rbwdgPredGpsExit(Sender: TObject);
    procedure rbwdgPredGpsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure rbwdgPredGpsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rbwdgPredGpsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rbwdgPredGpsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure btnRenameGpClick(Sender: TObject);
    procedure JvNetscapeSplitter1Moved(Sender: TObject);
    procedure rbwdgPredMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    IgnoreGpsStateChange, PredGpCellChanged: boolean;
    PredGpCellCurrent: GridCell;
    PredGpCellText: string;
    PredGpSelected: string;
    procedure AssignPredictions;
    procedure AssignPredGps;
    procedure CheckPredGpNames;
    procedure CountPredLines(const RbwGrid: TRbwDataGrid4; var NLines: integer; var NNames: integer;
                            var FirstBlankRow: integer);
    procedure InitializePredTables;
    procedure NumberRows;
    function SavePred(const IgnoreErrors: boolean): boolean;
    function SavePredData(const IgnoreErrors: boolean): boolean;
    function SavePredGps: boolean;
    procedure OpenConfigPredsTablesForm;
  public
    { Public declarations }
    PredictionDataChanged: boolean;
    PredictionSetupLocal: TPredictionSetup;
    PredictionSetupLocalChanged: boolean;
    PredSetLocal: TDepSet;
    PredGpsLocal: TDepSet;
    PredLocalChanged: boolean;
    PredGpsLocalChanged: boolean;
    procedure rbwdgPredPopulate(dsSource: TDepSet);
  end;

var
  FormPredictions: TFormPredictions;

implementation

{$R *.dfm}

var
  PredSetLocalTemp: TDepSet;
  NumGpsLocal: integer;
  PredCellSelected: boolean = False;
  PredTableCol0Width: integer;

{ TFormPredictions }

procedure TFormPredictions.AssignPredictions;
// Assign predictions and attributes in rbwdgPred to PredSetLocal
var
  ICol, IPA, IPred, IRow, NPred: integer;
  Caption, Msg, TempString: string;
  ClearedLocalSet: boolean;
begin
  ClearedLocalSet := False;
  NPred := rbwdgPred.RowCount - 1;
  if (not (PredSetLocal.Count = NPred)) then
    begin
      PredSetLocal.Clear;
      ClearedLocalSet := True;
    end;
  if (NPred = 1) and (IsBlank(rbwdgPred.Cells[1,1])) then
    NPred := 0;
  if NPred > 0 then
    begin
      for IPred := 0 to NPred - 1 do
        begin
          IRow := IPred+1;
          TempString := rbwdgPred.Cells[1,IRow];
          if J_Valid_Name(TempString,MaxLenDepName) then
            begin
              if ClearedLocalSet then PredSetLocal.Add;
              PredSetLocal.Items[IPred].Name := ConvertString20(TempString);
              // Assign attributes to this prediction, as defined in table.
              // Loop through columns (prediction attributes).
              for ICol := 1 to rbwdgPred.ColCount - 1 do
                begin
                  Caption := rbwdgPred.Cells[ICol,0];
                  // Find index of prediction attribute that has this caption.
                  IPA := PosDepCap(Caption);
                  // Assign data grid value to prediction attribute.
                  TempString := rbwdgPred.Cells[ICol, IRow];
                  PredSetLocal.Items[IPred].AllAtts[IPA].Text := TempString;
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

procedure TFormPredictions.AssignPredGps;
var
  ICol, IPA, IGp, IRow, NGps: integer;
  TempCaption, TempString: string;
  datTemp: TDepAttType;
  ClearedLocalGps: boolean;
begin
  ClearedLocalGps := False;
  // Assign prediction groups and attributes in rbwdgPredGps to PredGpsLocal.
  NGps := rbwdgPredGps.RowCount - 1;
  if (not (PredGpsLocal.Count = NGps)) then
    begin
      PredGpsLocal.Clear;
      ClearedLocalGps := True;
    end;
  if NGps > 0 then
    begin
      for IGp := 0 to NGps - 1 do
        begin
          if ClearedLocalGps then PredGpsLocal.Add;
          IRow := IGp+1;
          TempString := rbwdgPredGps.Cells[0,IRow];
          PredGpsLocal.Items[IGp].Name := ConvertString20(TempString);
          // Assign attributes to this prediction group, as defined in table.
          // Loop through columns (prediction-group attributes).
          for ICol := 0 to rbwdgPredGps.ColCount - 1 do
            begin
              TempCaption := rbwdgPredGps.Cells[ICol,0];
              // Find index of dependent attribute that has this caption.
              IPA := PosDepCap(TempCaption);
              // Assign data grid value to dependent attribute.
              datTemp := PredGpsLocal.Items[IGp].AllAtts[IPA].DepAttType;
              if (datTemp = datNonDetect) or (datTemp = datUseFlag) then
                begin
                  PredGpsLocal.Items[IGp].AllAtts[IPA].Text :=
                      BooleanToYesOrNo(rbwdgPredGps.Checked[ICol,IRow]);
                end
              else
                begin
                  TempString := rbwdgPredGps.Cells[ICol, IRow];
                  PredGpsLocal.Items[IGp].AllAtts[IPA].Text := TempString;
                end;
            end;
        end;
      if ClearedLocalGps then
        begin
          TempString := 'Need to add code in TFormPredictions.AssignPredGps to restore hidden attributes';
          ShowMessage(TempString);
        end;
    end;
end;

procedure TFormPredictions.btnAddPredClick(Sender: TObject);
var
  ModRes: integer;
begin
  AssignPredictions; // to PredSetLocal
  frmAddParOrDep := TfrmAddParOrDep.Create(Self);
  frmAddParOrDep.PDUse := pduPrediction;
  DepSetTemp.Assign(PredSetLocal);
  DepGpsTemp.Assign(PredGpsLocal);
  ModRes := frmAddParOrDep.ShowModal;
  if ModRes = mrOK then
    begin
      PredSetLocal.Assign(DepSetTemp);
      PredGpsLocal.Assign(DepGpsTemp);
      PredLocalChanged := True;
      rbwdgPredPopulate(PredSetLocal);
    end;
  FreeAndNil(frmAddParOrDep);
end;

procedure TFormPredictions.btnAddPredGpClick(Sender: TObject);
// Add a prediction group.
var
  LocalFrmAddGroup: TfrmAddGroup;
  NGpsOld: integer;
  ModRes: integer;
begin
  NGpsOld := PredGpsLocal.Count;
  LocalFrmAddGroup := TfrmAddGroup.Create(Self);
  LocalFrmAddGroup.GpUse := guPredGroup;
  DepGpsTemp.Assign(PredGpsLocal);
  ModRes := LocalFrmAddGroup.ShowModal;
  if ModRes = mrOK then
    begin
      if NGpsOld <> DepGpsTemp.Count then
        begin
          PredGpsLocal.Assign(DepGpsTemp);
          PredGpsLocalChanged := True;
          rbwdgPredGpsPopulate(PredGpsLocal);
          rbwdgPredPopulate(PredSetLocal);  // TODO 2 : Is this needed?
        end;
    end;
  FreeAndNil(LocalFrmAddGroup);
end;

procedure TFormPredictions.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPredictions.btnConfigureClick(Sender: TObject);
begin
  OpenConfigPredsTablesForm;
  rbwdgPredPopulate(PredSetLocal);
  rbwdgPredGpsPopulate(PredGpsLocal);
  rbwdgPred.Invalidate;
  rbwdgPred.Repaint;
  rbwdgPredGps.Invalidate;
  rbwdgPredGps.Repaint;
end;

procedure TFormPredictions.btnDeleteSelectedPredictionsClick(Sender: TObject);
var
  DelRow, I, J, SelRowFirst, SelRowLast: integer;
  TempPredSet: TDepSet;
  Messg: string;
begin
  SelRowFirst := rbwdgPred.Selection.Top;
  SelRowLast := rbwdgPred.Selection.Bottom;
  if SelRowFirst > 0 then
    begin
      DelRow := 0;
      for I := rbwdgPred.RowCount - 1 downto 1 do
        begin
          if (RowContainsSelectedCell(rbwdgPred,I))
                or ((I >= SelRowFirst) and (I <= SelRowLast)) then
            begin
              if rbwdgPred.RowCount > 2 then
                begin
                  rbwdgPred.DeleteRow(I);
                end
              else
                begin
                  rbwdgPred.Cells[1,1] := '';
                  for J := 2 to rbwdgPred.ColCount-1 do
                    begin
                      rbwdgPred.Cells[J,1] := '';
                    end;
                  TempPredSet := TDepSet.Create;
                  rbwdgPredPopulate(TempPredSet);
                  TempPredSet.Free;
                end;
              DelRow := I;
              // Delete corresponding item in PredSetLocal.
              if (PredSetLocal.Count > 0) then PredSetLocal.Delete(I-1);
            end;
        end;
      if DelRow > 0 then
        begin
          PredLocalChanged := True;
          NumberRows;
          if (DelRow < rbwdgPred.RowCount - 1) then
            begin
              rbwdgPred.Row := DelRow;
            end;
          ClearAllSelections(rbwdgPred);
          AssignPredictions;
        end;
    end
  else
    begin
      Messg := 'Select prediction(s) to be deleted';
      ShowMessage(Messg);
    end;
end;

procedure TFormPredictions.btnDeleteSelPredGpsClick(Sender: TObject);
// Delete selected prediction groups, but only if group is unused
var
  DelRow, I, K, SelRowFirst, SelRowLast: integer;
  GpName: string;
  Messg: string;
begin
  ClearAllSelections(rbwdgPred);
  rbwdgPred.EditorMode := False;
  if (rbwdgPredGps.RowCount > 2) and (CountSelectedRows(rbwdgPredGps) < rbwdgPredGps.RowCount-1) then
    begin
      SelRowFirst := rbwdgPredGps.Selection.Top;
      SelRowLast := rbwdgPredGps.Selection.Bottom;
      if SelRowFirst > 0 then
        begin
          DelRow := 0;
          for I := rbwdgPredGps.RowCount - 1 downto 1 do
            begin
              if (RowContainsSelectedCell(rbwdgPredGps,I))
                    or ((I >= SelRowFirst) and (I <= SelRowLast)) then
                begin
                  GpName := rbwdgPredGps.Cells[0,I];
                  K := PredSetLocal.NumDepByGroup(GpName);
                  if K = 0 then
                    begin
                      if rbwdgPredGps.RowCount > 2 then
                        begin
                          rbwdgPredGps.DeleteRow(I);
                        end;
                      DelRow := I;
                      // Delete corresponding item in PredGpsLocal.
                      if PredGpsLocal.Count > 0 then PredGpsLocal.Delete(I-1);
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
              ClearAllSelections(rbwdgPredGps);
              AssignPredGps;
              PredGpsLocalChanged := True;
              rbwdgPredPopulate(PredSetLocal);
            end;
        end
      else
        begin
          Messg := 'Select prediction group(s) to be deleted';
          ShowMessage(Messg);
        end;
    end
  else
    begin
      Messg := 'At least one group must be defined.  Group(s) not deleted';
      ShowMessage(Messg);
    end;
end;

procedure TFormPredictions.btnOKClick(Sender: TObject);
var
  IgnoreErrors, OK: boolean;
  ErrRow: integer;
  ErrMsg, ErrName: string;
  Selection: TGridRect;
begin
  IgnoreErrors := False;
  ModalResult := mrNo;
  OK := CheckNamesInColumn(rbwdgPred,1,MaxLenDepName,ErrName,ErrRow);
  if (rbwdgPred.RowCount = 2) and (IsBlank(ErrName)) then OK := True;
  if not OK then
    begin
        ErrMsg := 'Invalid prediction name: ' + ErrName;
        ShowMessage(ErrMsg);
        Selection.Left := 1;
        Selection.Right := 1;
        Selection.Top := ErrRow;
        Selection.Bottom := ErrRow;
        rbwdgPred.Selection := Selection;
        rbwdgPred.TopRow := ErrRow;
        rbwdgPred.SetFocus;
        ModalResult := mrNone;
    end;
  if OK then
    begin
      // Check prediction group names
      OK := CheckNamesInColumn(rbwdgPredGps,0,MaxLenDepName,ErrName,ErrRow);
      if (rbwdgPredGps.RowCount = 2) and (IsBlank(ErrName)) then OK := True;
      if not OK then
        begin
          ErrMsg := 'Invalid name: ' + ErrName;
          ShowMessage(ErrMsg);
          OK := False;
          Selection.Left := 0;
          Selection.Right := 0;
          Selection.Top := ErrRow;
          Selection.Bottom := ErrRow;
          rbwdgPredGps.Selection := Selection;
          rbwdgPredGps.TopRow := ErrRow;
          rbwdgPredGps.SetFocus;
          ModalResult := mrNone;
        end;
    end;
  if OK then
    begin
      if SavePredData(IgnoreErrors) then
        begin
          if PredictionSetupLocalChanged or PredLocalChanged or PredGpsLocalChanged then
            begin
              PredictionDataChanged := True;
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

procedure TFormPredictions.btnRenameGpClick(Sender: TObject);
var
  I, ModRes, SelRowFirst, SelRowLast: integer;
  Messg, NewGpName, OldGpName: string;
begin
  ClearAllSelections(rbwdgPred);
  rbwdgPred.EditorMode := False;
  SelRowFirst := rbwdgPredGps.Selection.Top;
  SelRowLast := rbwdgPredGps.Selection.Bottom;
  if (SelRowFirst = SelRowLast) and (SelRowFirst > 0) then
    begin
      I := SelRowFirst;
      OldGpName := rbwdgPredGps.Cells[0,I];
      PredGpCellCurrent.Row := I;
      PredGpCellCurrent.Column := 0;
      PredGpCellCurrent.TextOld := OldGpName;
      PredGpCellCurrent.TextNew := OldGpName;
      FormRenameGroup.CellData := PredGpCellCurrent;
      FormRenameGroup.GpUse := guPredGroup;
      FormRenameGroup.edtNewGroup.MaxLength := MaxLenGpName;
      ModRes := FormRenameGroup.ShowModal;
      if ModRes = mrOk then
        begin
          NewGpName := FormRenameGroup.CellData.TextNew;
          rbwdgPredGps.Cells[PredGpCellCurrent.Column,PredGpCellCurrent.Row]
                := NewGpName;
          PredSetLocal.ChangeGroupNames(OldGpName,NewGpName);
          rbwdgPredPopulate(PredSetLocal);
          ClearAllSelections(rbwdgPredGps);
          AssignPredGps;
          PredGpsLocalChanged := True;
          PredLocalChanged := True;
        end
      else
        begin
          rbwdgPredGps.Cells[PredGpCellCurrent.Column,PredGpCellCurrent.Row]
                := PredGpCellCurrent.TextOld;
        end;
      PredGpCellChanged := False;
    end
  else if SelRowFirst <= 0 then
    begin
      Messg := 'Select a prediction group to be renamed';
      ShowMessage(Messg);
    end;
end;

procedure TFormPredictions.CheckPredGpNames;
var
  IRowCurr: Integer;
  OldGroup: string;
  K: Integer;
  Messg: string;
  NewGroup: string;
begin
  for IRowCurr := 1 to rbwdgPredGps.RowCount - 1 do
    begin
      OldGroup := ConvertString(PredGpsLocal.Items[IRowCurr - 1].Name);
      NewGroup := rbwdgPredGps.Cells[0,IRowCurr];
      if NewGroup <> OldGroup then
        begin
          // If group name has changed, ensure that old group name is unused.
          K := PredSetLocal.NumDepByGroup(OldGroup);
          if K > 0 then
            // Old group name is in use.
            begin
              Messg := 'Group name in use may not be changed: ' + OldGroup;
              ShowMessage(Messg);
              rbwdgPredGps.Cells[0, IRowCurr] := OldGroup;
            end
          else
          // Old group name not in use and may be changed
            begin
              // Check validity of new group name
              NewGroup := rbwdgPredGps.Cells[0, IRowCurr];
              if not J_Valid_Name(NewGroup,MaxLenGpName) then
                begin
                  if NewGroup <> '' then
                    begin
                      Messg := 'Invalid name: ' + NewGroup;
                      ShowMessage(Messg);
                    end;
                  rbwdgPredGps.Cells[0, IRowCurr] := OldGroup;
                end;
            end;
        end;
    end;
end;

procedure TFormPredictions.CountPredLines(const RbwGrid: TRbwDataGrid4;
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

procedure TFormPredictions.OpenConfigPredsTablesForm;
var
  IgnoreErrors: Boolean;
  ModRes: Integer;
begin
  IgnoreErrors := True;
  SavePredData(IgnoreErrors);  // this is a boolean function
  FormConfigurePredTables.PredictionSetupConfigure.Assign(PredictionSetupLocal);
  ModRes := FormConfigurePredTables.ShowModal;
  if ModRes = mrOK then
    begin
      if FormConfigurePredTables.PredictionSetupConfigureChanged then
        begin
          PredictionSetupLocal.Assign(FormConfigurePredTables.PredictionSetupConfigure);
          PredictionSetupLocalChanged := True;
        end;
    end;
end;

procedure TFormPredictions.FormCreate(Sender: TObject);
begin
  Left := 25;
  Top := 35;
  PredictionSetupLocal := TPredictionSetup.Create(self);
  PredSetLocal := TDepSet.Create;
  PredSetLocalTemp  := TDepSet.Create;
  PredGpsLocal := TDepSet.Create;
  PredTableCol0Width := 65;
end;

procedure TFormPredictions.FormDestroy(Sender: TObject);
begin
  PredictionSetupLocal.Free;
  PredSetLocal.Free;
  PredSetLocalTemp.Free;
  PredGpsLocal.Free;
end;

procedure TFormPredictions.FormResize(Sender: TObject);
var
  AvailableHeight: integer;
begin
  AvailableHeight := self.Height - pnlUpper.Height - StatusBar1.Height - 37;
  if pnlLower.Height > AvailableHeight then
    pnlLower.Height := AvailableHeight;
end;

procedure TFormPredictions.FormShow(Sender: TObject);
begin
  InitializePredTables;
  PredictionSetupLocalChanged := False;
  PredLocalChanged := False;
  PredGpsLocalChanged := False;
end;

procedure TFormPredictions.InitializePredTables;
var
  I: Integer;
  DefText: string;
begin
  rbwdgPred.ColWidths[0] := PredTableCol0Width;
  rbwdgPredPopulate(PredSetLocal);
  rbwdgPredGpsPopulate(PredGpsLocal);
  PredLocalChanged := False;
  PredGpsLocalChanged := False;
  PredictionSetupLocalChanged := False;
  NumGpsLocal := PredGpsLocal.Count;
  // Determine whether groups table should be enabled.
  with PredGpsLocal do
  begin
    if Count = 1 then
    begin
      if (Items[0].Name = 'DefaultPreds') then
      begin
        for I := 0 to NumDepAttributes - 1 do
        begin
          DefText := Items[0].AllAtts.Items[I].DefaultText(dcObs);
        end;
      end;
    end;
  end;
  NumGpsLocal := rbwdgPredGps.RowCount - 1;
  StatusBar1.Panels[0].Text := 'Click in a column to display hint';
  ClearAllSelections(rbwdgPred);
end;

procedure TFormPredictions.JvNetscapeSplitter1Moved(Sender: TObject);
begin
  ClearAllSelections(rbwdgPred);
  ClearAllSelections(rbwdgPredGps);
end;

procedure TFormPredictions.NumberRows;
var
  I, NR: integer;
begin
  NR:= rbwdgPred.RowCount;
  for I := 1 to NR - 1 do
    rbwdgPred.Cells[0,I] := IntToStr(I);
end;

procedure TFormPredictions.PredictionsTables1Click(Sender: TObject);
begin
  OpenConfigPredsTablesForm;
end;

procedure TFormPredictions.rbwdgPredExit(Sender: TObject);
begin
  AssignPredictions;
  if PredSetLocal <> PCurrent.PredSet then
    begin
      PredLocalChanged := True;
    end;
  rbwdgPred.Invalidate;
  rbwdgPred.Repaint;
end;

procedure TFormPredictions.rbwdgPredGpsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  Invalidate;
  Repaint;
end;

procedure TFormPredictions.rbwdgPredGpsEndUpdate(Sender: TObject);
begin
  if PredGpCellChanged then
    begin
      if PredGpCellCurrent.Column = 0 then
        begin
          rbwdgPredGps.Cells[PredGpCellCurrent.Column,PredGpCellCurrent.Row]
                    := PredGpCellCurrent.TextOld;
          PredGpCellChanged := False;
        end;
    end;
end;

procedure TFormPredictions.rbwdgPredGpsExit(Sender: TObject);
var
  FirstBlankGpRow, NGpLines, NGpNames: integer;
  OK: boolean;
begin
  // Check group table entries
  // Count lines not filled with blanks in groups table
  // If NGpNames < NGpLines, there's at least one blank group name
  CountPredLines(rbwdgPredGps,NGpLines,NGpNames,FirstBlankGpRow);
  if NGpLines = NGpNames then
    begin
      OK := True;
    end
  else
    begin
      ShowMessage('Error in Prediction Groups Table: At least one group is unnamed');
      rbwdgPredGps.SetFocus;
      rbwdgPredGps.Column := 0;
      rbwdgPredGps.Row := FirstBlankGpRow;
      OK := False;
    end;
  if OK then
    begin
      CheckPredGpNames;
      AssignPredGps;
    end;
  rbwdgPredGps.Invalidate;
  rbwdgPredGps.Repaint;
end;

procedure TFormPredictions.rbwdgPredGpsGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  PredGpCellText := Value;
  PredGpCellChanged := False;
  if ACol = 0 then
    begin
      PredGpSelected := Value;
    end;
end;

procedure TFormPredictions.rbwdgPredGpsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    begin
      SetColorSelectedRow(rbwdgPredGps,False);
    end
  else
    begin
      SetColorSelectedRow(rbwdgPredGps,True);
    end;
  if ssShift in Shift then
    begin
      AllowEditing(rbwdgPredGps,False);
      SetColorSelectedRow(rbwdgPredGps,False);
    end
  else
    begin
      AllowEditing(rbwdgPredGps,True);
    end;
end;

procedure TFormPredictions.rbwdgPredGpsPopulate(const dsSource: TDepSet);
var
  I, IA, IGP, IP1, J, NRows, NumCols: integer;
  BlankString, TempString: string;
  setATDisplayGps: set of TDepAttType;
  datTemp: TDepAttType;
  TempBool: boolean;
begin
  BlankString := '';
  if PredGpsLocal.Count = 0 then
    begin
      PredGpsLocal.SetGpDefault(dcPred);
    end;
  case PCurrent.ActiveApp of
    aaUcode:
      begin
        setATDisplayGps := [datMeasStatistic, datMeasStatFlag, datUseFlag,
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
    if (PredictionSetupLocal.PredAttributes[I].DepAttType in setATDisplayGps) and
       (PredictionSetupLocal.PredAttributes[I].ControlMethod = cmByGroup) then
            NumCols := NumCols + 1;
  rbwdgPredGps.ColCount := NumCols;
  if rbwdgPredGps.ColCount >1 then
    begin
      rbwdgPredGps.FixedCols := 1;
    end;
  // Clear table headings and cells
  for I := 0 to rbwdgPredGps.ColCount - 1 do
    begin
      rbwdgPredGps.Columns[I].CaptionAlignment := taLeftJustify;
      rbwdgPredGps.Cells[I,0] := BlankString;
      rbwdgPredGps.Columns[I].AutoAdjustColWidths := True;
      for J := 0 to rbwdgPredGps.RowCount - 1 do
        begin
          try
            rbwdgPredGps.Cells[I, J] := BlankString;
          except
            On E: EListError do
              ShowMessage('Caught EListError exception');
          end;
        end;
    end;
  // After all cells are cleared, assign ColWidths to minimal width.
  for I := 0 to rbwdgPredGps.ColCount - 1 do
    begin
      rbwdgPredGps.Columns[I].ComboUsed := False;
      rbwdgPredGps.ColWidths[I] := rbwdgPredGps.DefaultColWidth;
      rbwdgPredGps.Columns[I].AutoAdjustColWidths := True;
    end;
  // Populate column headings and assign properties for selected columns
  NRows := dsSource.Count + 1;
  if NRows < 2 then NRows := 2;
  rbwdgPredGps.RowCount := NRows;
  J := 0;
  rbwdgPredGps.Cells[J,0] := 'Group Name';
  for I := 0 to NumDepAttributes - 1 do
    if (PredictionSetupLocal.PredAttributes[I].ControlMethod = cmByGroup) and
       (PredictionSetupLocal.PredAttributes[I].DepAttType in setATDisplayGps) then
    begin
      case PredictionSetupLocal.PredAttributes[I].DepAttType of
        datMeasStatistic: begin
            J := J + 1;
            rbwdgPredGps.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPredGps.Columns[J].Format := rcf4Real;
            rbwdgPredGps.Columns[J].ComboUsed := False;
            rbwdgPredGps.Columns[J].LimitToList := False;
          end;
        datMeasStatFlag: begin
            J := J + 1;
            rbwdgPredGps.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPredGps.Columns[J].Format := rcf4String;
            rbwdgPredGps.Columns[J].ComboUsed := True;
            rbwdgPredGps.Columns[J].PickList := slMeasStatFlag;
            rbwdgPredGps.Columns[J].LimitToList := True;
          end;
        datUseFlag: begin
            J := J + 1;
            rbwdgPredGps.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPredGps.Columns[J].Format := rcf4Boolean;
            rbwdgPredGps.Columns[J].ComboUsed := False;
            rbwdgPredGps.Columns[J].LimitToList := False;
          end;
        datPlotSymbol: begin
            J := J + 1;
            rbwdgPredGps.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPredGps.Columns[J].Format := rcf4Integer;
            rbwdgPredGps.Columns[J].ComboUsed := False;
            rbwdgPredGps.Columns[J].LimitToList := False;
          end;
        datWtMultiplier: begin
            J := J + 1;
            rbwdgPredGps.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPredGps.Columns[J].Format := rcf4Real;
            rbwdgPredGps.Columns[J].ComboUsed := False;
            rbwdgPredGps.Columns[J].LimitToList := False;
          end;
        datCovMatrix: begin
            J := J + 1;
            rbwdgPredGps.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPredGps.Columns[J].Format := rcf4String;
          end;
        datNonDetect: begin
            J := J + 1;
            rbwdgPredGps.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPredGps.Columns[J].Format := rcf4Boolean;
            rbwdgPredGps.Columns[J].ComboUsed := False;
            rbwdgPredGps.Columns[J].LimitToList := False;
          end;
        datWtOSConstant: begin
            J := J + 1;
            rbwdgPredGps.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPredGps.Columns[J].Format := rcf4Real;
            rbwdgPredGps.Columns[J].ComboUsed := False;
            rbwdgPredGps.Columns[J].LimitToList := False;
          end;
      end;
    end;
  // Populate table cells
  // Loop through groups; each iteration populates a row
  for IGP := 0 to dsSource.Count - 1 do
  begin
    IP1 := IGP + 1;
    rbwdgPredGps.Cells[0,IP1] := ConvertString(dsSource.Items[IGP].Name);
    J := 0;
    for IA := 0 to NumDepAttributes - 1 do
    begin
      datTemp := PredictionSetupLocal.PredAttributes[IA].DepAttType;
      if (PredictionSetupLocal.PredAttributes[IA].ControlMethod = cmByGroup) and
         (datTemp in setATDisplayGps) then
        begin
          J := J + 1;
          TempString := dsSource.Items[IGP].AllAtts[IA].Text;
          if TempString = '' then
            TempString := dsSource.Items[IGP].AllAtts.Items[IA].DefaultText(dcPred);
          datTemp := dsSource.Items[IGP].AllAtts[IA].DepAttType;
          if ((datTemp = datUseFlag) or (datTemp = datNonDetect)) then
            begin
              // Check or uncheck box depending on boolean value.
              TempBool := YesOrNoToBoolean(TempString);
              IgnoreGpsStateChange := True;
              rbwdgPredGps.Checked[J,IP1] := TempBool;
              IgnoreGpsStateChange := False;
            end
          else
            begin
              rbwdgPredGps.Cells[J,IP1] := TempString;
            end;
        end;
    end;
  end;
  NumberRows;
  rbwdgPredGps.Invalidate;
end;

procedure TFormPredictions.rbwdgPredGpsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Hint: string;
  LocalRect: TGridRect;
begin
  if (ACol = rbwdgPredGps.Column) and (ACol >= 0) and (ARow >= 0) then
    begin
      Hint := GetPredHint(rbwdgPredGps, ACol);
      StatusBar1.Panels[0].Text := ' ' + Hint;
      if ACol = 0 then
        begin
          LocalRect.Left := 0;
          LocalRect.Right := rbwdgPredGps.ColCount - 1;
          LocalRect.Top := ARow;
          LocalRect.Bottom := ARow;
          rbwdgPredGps.Selection := LocalRect;
        end;
    end;
end;

procedure TFormPredictions.rbwdgPredGpsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if Value <> PredGpCellText then
    begin
      PredGpCellCurrent.Row := ARow;
      PredGpCellCurrent.Column := ACol;
      PredGpCellCurrent.Checked := rbwdgPredGps.Checked[ACol,ARow];
      PredGpCellCurrent.TextOld := PredGpCellText;
      PredGpCellCurrent.TextNew := Value;
      PredGpCellChanged := True;
      PredGpsLocalChanged := True;
    end;
end;

procedure TFormPredictions.rbwdgPredGpsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  if not IgnoreGpsStateChange then
    begin
      AssignPredGps;
      PredGpsLocalChanged := True;
    end;
end;

procedure TFormPredictions.rbwdgPredMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
    begin
      SetColorSelectedRow(rbwdgPred,False);
    end
  else
    begin
      SetColorSelectedRow(rbwdgPred,True);
    end;
  if ssShift in Shift then
    begin
      AllowEditing(rbwdgPred,False);
      SetColorSelectedRow(rbwdgPred,False);
    end
  else
    begin
      AllowEditing(rbwdgPred,True);
    end;
end;

procedure TFormPredictions.rbwdgPredMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  NumberRows;
end;

procedure TFormPredictions.rbwdgPredPopulate(dsSource: TDepSet);
var
  DatPos, GpCol, I, IA, IP1, J, NRows, NumCols: integer;
  setATDisplay: set of TDepAttType;
  datTemp: TDepAttType;
  GpList: TStringList;
  TempString: string;
  TempBool: boolean;
begin
  GpList := TStringList.Create;
  try
  for I := 0 to PredGpsLocal.Count - 1 do
    GpList.Add(ConvertString(PredGpsLocal.Items[I].Name));
  case PCurrent.ActiveApp of
  aaUcode:
    begin
      setATDisplay := [datRefValue, datMeasStatistic,
          datMeasStatFlag, datEquation, datUseFlag, datPlotSymbol,
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
  NumCols := 3; // Always columns for prediction number, name, and group
  for I := 0 to NumDepAttributes - 1 do
    if (PredictionSetupLocal.PredAttributes[I].DepAttType in setATDisplay) and
       (PredictionSetupLocal.PredAttributes[I].ControlMethod = cmByItem) then
            NumCols := NumCols + 1;
  rbwdgPred.ColCount := NumCols;
  // Clear table headings and cells
  for I := 0 to rbwdgPred.ColCount - 1 do
    begin
      rbwdgPred.Cells[I,0] := '';
      rbwdgPred.Columns[I].CaptionAlignment := taLeftJustify;
      rbwdgPred.Columns[I].AutoAdjustColWidths := True;
      for J := 1 to rbwdgPred.RowCount - 1 do
          rbwdgPred.Cells[I, J] := '';
    end;
  // After all cells are cleared, assign ColWidths to minimal width.
  for I := 0 to rbwdgPred.ColCount - 1 do
    begin
      rbwdgPred.Columns[I].ComboUsed := False;
      rbwdgPred.ColWidths[I] := rbwdgPred.DefaultColWidth;
      rbwdgPred.Columns[I].AutoAdjustColWidths := True;
    end;
  // Populate column headings and assign properties for selected columns
  NRows := dsSource.Count + 1;
  if NRows < 2 then NRows := 2;
  rbwdgPred.RowCount := NRows;
  rbwdgPred.Cells[0,0] := 'Pred #';

  rbwdgPred.Cells[1,0] := 'Prediction Name';
  rbwdgPred.Columns[1].Format := rcf4String;
  rbwdgPred.Columns[1].ComboUsed := False;

  rbwdgPred.Cells[2,0] := 'Group Name';
  rbwdgPred.Columns[2].Format := rcf4String;
  rbwdgPred.Columns[2].ComboUsed := True;
  rbwdgPred.Columns[2].PickList := GpList;
  rbwdgPred.Columns[2].LimitToList := True;

  GpCol := 0;
  J := 2;
  for I := 0 to NumDepAttributes - 1 do
    if (PredictionSetupLocal.PredAttributes[I].ControlMethod = cmByItem) and
       (PredictionSetupLocal.PredAttributes[I].DepAttType in setATDisplay) then
    begin
      case PredictionSetupLocal.PredAttributes[I].DepAttType of
        datPredName: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4String;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
        datRefValue: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4Real;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
        datMeasStatistic: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4Real;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
        datMeasStatFlag: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4String;
            rbwdgPred.Columns[J].ComboUsed := True;
            rbwdgPred.Columns[J].PickList := slMeasStatFlag;
            rbwdgPred.Columns[J].LimitToList := True;
          end;
        datUseFlag: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4Boolean;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
        datPlotsymbol: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4Integer;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
        datEquation: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4String;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
        datWtMultiplier: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4Real;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
        datCovMatrix: begin
            { TODO 3 -cPredictions : Code this option to display list of defined CovMatrix names }
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4String;
            rbwdgPred.Columns[J].ComboUsed := False;
          end;
        datNonDetect: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4Boolean;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
        datWtOSConstant: begin
            J := J + 1;
            rbwdgPred.Cells[J,0] :=
                 PredictionSetupLocal.PredAttributes[I].Caption;
            rbwdgPred.Columns[J].Format := rcf4Real;
            rbwdgPred.Columns[J].ComboUsed := False;
            rbwdgPred.Columns[J].LimitToList := False;
          end;
      end;
    end;
  // Populate table cells
  // Loop through groups; each iteration populates a row
  DatPos := DepAttPos(datGroupName);
  for I := 0 to dsSource.Count - 1 do
  begin
    IP1 := I + 1;
    rbwdgPred.Cells[0,IP1] := IntToStr(IP1);
    rbwdgPred.Cells[1,IP1] := ConvertString(dsSource.Items[I].Name);
    rbwdgPred.Cells[2,IP1] := dsSource.Items[I].AllAtts.Items[DatPos].Text;
    J := 2;
    for IA := 0 to NumDepAttributes - 1 do
    begin
      if (PredictionSetupLocal.PredAttributes[IA].ControlMethod = cmByItem) and
         (PredictionSetupLocal.PredAttributes[IA].DepAttType in setATDisplay) then
        begin
          J := J + 1;
          TempString := dsSource.Items[I].AllAtts[IA].Text;
          datTemp := dsSource.Items[I].AllAtts[IA].DepAttType;
          if datTemp = datNonDetect then
            begin
              TempBool := YesOrNoToBoolean(dsSource.Items[I].AllAtts[IA].Text);
              rbwdgPred.Checked[J,IP1] := TempBool;
            end
          else
            begin
              if TempString = '' then
                TempString := dsSource.Items[I].AllAtts.Items[IA].DefaultText(dcObs);
              rbwdgPred.Cells[J,IP1] := TempString;
              if (J = GpCol) and (rbwdgPred.Cells[J,IP1] = '') then
                begin
                  rbwdgPred.Cells[J,IP1] := rbwdgPredGps.Cells[1,1];
                end;
            end;
        end;
    end;
  end;
  NumberRows;
  // Set selections to be invisible and invalid
  ClearAllSelections(rbwdgPred);
  //
  SetColorSelectedRow(rbwdgPred,True);
  StatusBar1.Panels[0].Text := '';
  rbwdgPred.Invalidate;
  finally
    GpList.Free;
  end;
end;

procedure TFormPredictions.rbwdgPredSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = rbwdgPred.Column) and (ACol >= 0) and (ARow >= 0) then
    begin
      StatusBar1.Panels[0].Text := ' ' + GetObsHint(rbwdgPred, ACol);
      if ARow > 0 then
        begin
          PredCellSelected := True;
        end
      else
        PredCellSelected := False;
    end;
end;

procedure TFormPredictions.rbwdgPredStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  PredLocalChanged := True;
end;

function TFormPredictions.SavePred(const IgnoreErrors: boolean): boolean;
var
  I, IAtt, IErr, II, IRow, J, JJ, NCols, NPred, NRdata: integer;
  Caption, Msg: string;
  datTemp: TDepAttType;
  ClearedLocalSet: boolean;
begin
  result := True;
  ClearedLocalSet := False;
  // need code
  IErr := 0;
  Msg := '';
  // Save prediction information
  NRdata := rbwdgPred.RowCount - 1;
  NPred := NRdata;
  NCols := rbwdgPred.ColCount;
  if not IgnoreErrors then
    begin
      // Ensure all rows are fully populated
      NPred := 0;
      for IRow := 1 to NRdata do // IRow is the row index in the observations table
        begin
          if rbwdgPred.Cells[1,IRow] <> '' then // Prediction name
            begin
              for J := 2 to NCols - 1 do
                begin
                  // Find dependent attribute type
                  if (rbwdgPred.Cells[J,IRow] = '') and (rbwdgPred.Columns[J].Format <> rcf4Boolean) then
                    begin
                      Msg := 'Error: Missing data for prediction ' + IntToStr(IRow);
                      IErr := 1;
                    end;
                end;
              NPred := NPred+1;
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
      if (not (NPred = PredSetLocal.Count)) then
        begin
          PredSetLocal.Clear;
          ClearedLocalSet := True;
        end;
      for I := 1 to NPred do
        begin
          if ClearedLocalSet then PredSetLocal.Add;
          II := I-1;
          PredSetLocal.Items[II].Name := ConvertString20(rbwdgPred.Cells[1,I]);
          if I>1 then
            begin
              J := 1;
              while (J<I) and (IErr=0) do
                begin
                  JJ := J-1;
                  if SameText(ConvertString(PredSetLocal.Items[II].Name),ConvertString(PredSetLocal.Items[JJ].Name)) then
                    begin
                      if not IgnoreErrors then
                        begin
                          IErr := I;
                          Msg := 'Error: Predictions ' + IntToStr(J) + ' and ' + IntToStr(IErr)
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
              Caption := rbwdgPred.Cells[J,0];
              IAtt := PosDepCap(Caption);
              datTemp := DepAttributeTypes[IAtt];
              for I := 1 to NPred do // loop through rows in this column
                begin
                  II := I-1;
                  if (datTemp = datNonDetect) then
                    begin
                      PredSetLocal.Items[II].AllAtts.Items[IAtt].Text :=
                          BooleanToYesOrNo(rbwdgPred.Checked[J,I]);
                    end
                  else
                    begin
                      PredSetLocal.Items[II].AllAtts.Items[IAtt].Text := rbwdgPred.Cells[J,I];
                      PredSetLocal.Items[II].AllAtts.Items[IAtt].DepAttType := datTemp;
                    end;
                end;
            end;
        end;
    end;
  if IErr = 0 then
    begin
      PredSetLocalTemp.Assign(PredSetLocal);
    end
  else
    begin
      result := False;
    end;
end;

function TFormPredictions.SavePredData(const IgnoreErrors: boolean): boolean;
var
  OK: Boolean;
begin
  OK := True;
  StatusBar1.Panels[0].Text := '';
  if PredLocalChanged or PredictionSetupLocalChanged then
    begin
      if not SavePred(IgnoreErrors) then
        begin
          OK := False;
        end;
    end;
  if PredGpsLocalChanged or PredictionSetupLocalChanged then
    begin
      if not SavePredGps then
        begin
          OK := False;
        end;
    end;
  if OK then
    begin
      StatusBar1.Panels[0].Text := 'Data Saved';
    end;
  result := OK;
end;

function TFormPredictions.SavePredGps: boolean;
var
  I, IAtt, IErr, II, IRow, J, JJ, NCols, NGps, NRdata: integer;
  Caption, Msg: string;
  datTemp: TDepAttType;
  ClearedLocalGps: boolean;
begin
  result := True;
  ClearedLocalGps := False;
  IErr := 0;
  Msg := '';
  // Save prediction groups information
  NRdata := rbwdgPredGps.RowCount - 1;
  NGps := 0;
  NCols := rbwdgPredGps.ColCount;
  // Ensure all rows are fully populated
  for IRow := 1 to NRdata do // IRow is the row index in the prediction groups table
    begin
      if rbwdgPredGps.Cells[0,IRow] <> '' then // Prediction group name
        begin
          for J := 1 to NCols - 1 do
            begin
              if (rbwdgPredGps.Cells[J,IRow] = '') and (rbwdgPredGps.Columns[J].Format <> rcf4Boolean) then
                begin
                  Msg := 'Error: Missing data for group ' + rbwdgPredGps.Cells[0,IRow];
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
      if (not (NGps = PredGpsLocal.Count)) then
        begin
          PredGpsLocal.Clear;
          ClearedLocalGps := True;
        end;
      for I := 1 to NGps do
        begin
          if ClearedLocalGps then PredGpsLocal.Add;
          II := I-1;
          PredGpsLocal.Items[II].Name := ConvertString20(rbwdgPredGps.Cells[0,I]);
          //
          if I>1 then
            begin
              J := 1;
              while (J<I) and (IErr=0) do
                begin
                  JJ := J-1;
                  if SameText(ConvertString(PredGpsLocal.Items[II].Name), ConvertString(PredGpsLocal.Items[JJ].Name)) then
                    begin
                      IErr := I;
                      Msg := 'Error: Duplicate group name: ' + ConvertString(PredGpsLocal.Items[II].Name);
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
              Caption := rbwdgPredGps.Cells[J,0];
              IAtt := PosDepCap(Caption);
              datTemp := DepAttributeTypes[IAtt];
              for I := 1 to NGps do // loop through rows in this column.
                begin
                  II := I-1;
                  PredGpsLocal.Items[II].AllAtts.Items[IAtt].DepAttType := datTemp;
                  if (datTemp = datNonDetect) or (datTemp = datUseFlag) then
                    begin
                      PredGpsLocal.Items[II].AllAtts.Items[IAtt].Text := BooleanToYesOrNo(rbwdgPredGps.Checked[J,I]);
                    end
                  else
                    begin
                      PredGpsLocal.Items[II].AllAtts.Items[IAtt].Text := rbwdgPredGps.Cells[J,I];
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
      PredictionsChanged := True;
      ProjChanged := True;
    end;
end;

procedure TFormPredictions.StatusBar1Click(Sender: TObject);
begin
  ShowMessage(StatusBar1.Panels[0].Text);
end;

end.
