unit frmParallelRunners;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, ExtCtrls, Buttons, StdCtrls,
  JvBaseDlg, JvBrowseFolder,
  GlobalData, GlobalTypesUnit, ParallelRunnerUnit, Utilities;

type
  TFormParallelRunners = class(TForm)
    Panel1: TPanel;
    dgRunners: TRbwDataGrid4;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    bffDialog: TJvBrowseForFolderDialog;
    btnDeleteRunner: TBitBtn;
    btnMoveRowUp: TSpeedButton;
    btnMoveRowDown: TSpeedButton;
    btnSortByRunTime: TButton;
    btnAddOneRow: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnAddRunnerClick(Sender: TObject);
    procedure dgRunnersButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DeleteRunner();
    procedure btnDeleteRunnerClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure dgRunnersColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure btnMoveRowUpClick(Sender: TObject);
    procedure btnMoveRowDownClick(Sender: TObject);
    procedure btnSortByRunTimeClick(Sender: TObject);
    procedure dgRunnersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnAddOneRowClick(Sender: TObject);
  private
    { Private declarations }
    Cols013Width: integer;
    procedure ExchangeRows(SelRow: Integer; NewRow: Integer);
//    procedure PopulateGrid; overload;
    procedure PopulateGrid(Runners: TParallelRunners); overload;
    function StoreRunnerData: boolean;
    function VerifyRunnerData(var ErrMsg: string): boolean;
    procedure AddOneRow;
  public
    { Public declarations }
    NumUsableRunners: integer;
  end;

var
  FormParallelRunners: TFormParallelRunners;
  ParallelRunnersLocal: TParallelRunners;

implementation

{$R *.dfm}

procedure TFormParallelRunners.btnAddOneRowClick(Sender: TObject);
begin
  AddOneRow;
end;

procedure TFormParallelRunners.btnAddRunnerClick(Sender: TObject);
var
  IRow: integer;
begin
  dgRunners.RowCount := dgRunners.RowCount + 1;
  IRow := dgRunners.RowCount-1;
  dgRunners.Cells[0,IRow] := '';
  dgRunners.Checked[1,IRow] := True;
  dgRunners.Cells[2,IRow] := '';
  dgRunners.Cells[3,IRow] := '0.001';
end;

procedure TFormParallelRunners.btnDeleteRunnerClick(Sender: TObject);
begin
  DeleteRunner();
end;

procedure TFormParallelRunners.btnMoveRowDownClick(Sender: TObject);
var
  NewRow, SelRow: integer;
begin
  SelRow := dgRunners.SelectedRow;
  if SelRow < dgRunners.RowCount-1 then
    begin
      NewRow := SelRow + 1;
      ExchangeRows(NewRow, SelRow);
      dgRunners.Row := NewRow;
    end;

end;

procedure TFormParallelRunners.btnMoveRowUpClick(Sender: TObject);
var
  NewRow, SelRow: integer;
begin
  SelRow := dgRunners.SelectedRow;
  if SelRow > 1 then
    begin
      NewRow := SelRow - 1;
      ExchangeRows(SelRow, NewRow);
      dgRunners.Row := NewRow;
    end;
end;

procedure TFormParallelRunners.btnOKClick(Sender: TObject);
begin
  if StoreRunnerData then
    begin
      ParallelRunnersGlobal.Assign(ParallelRunnersLocal);
      self.ModalResult := mrOK;
    end
  else
    begin
      self.ModalResult := 0;
    end;
end;

procedure TFormParallelRunners.btnSortByRunTimeClick(Sender: TObject);
var
  Changed, Done: boolean;
  I, IP1, NewLastRow, NR: integer;
begin
  NR := dgRunners.RowCount - 1;
  NewLastRow := NR;
  if NR > 1 then
    begin
      Done := False;
      while not Done do
        begin
          Changed := False;
          NR := NewLastRow;
          if NR > 1 then
            begin
              for I := 1 to NR-1 do
                begin
                  IP1 := I + 1;
                  if dgRunners.Cells[3,I] > dgRunners.Cells[3,IP1] then
                    begin
                      ExchangeRows(I,IP1);
                      Changed := True;
                    end;
                end;
            end;
          NewLastRow := NR - 1;
          if not Changed then Done := True;
        end;
    end;
end;

procedure TFormParallelRunners.DeleteRunner;
var
  DelRow, I, SelRowFirst, SelRowLast: integer;
  TempRunners: TParallelRunners;
begin
  SelRowFirst := dgRunners.Selection.Top;
  SelRowLast := dgRunners.Selection.Bottom;
  DelRow := 0;
  for I := dgRunners.RowCount - 1 downto 1 do
    begin
      if (RowContainsSelectedCell(dgRunners,I))
            or ((I >= SelRowFirst) and (I <= SelRowLast)) then
        begin
          if dgRunners.RowCount > 2 then
            begin
              dgRunners.DeleteRow(I);
            end
          else
            begin
              dgRunners.Cells[1,1] := '';
              TempRunners := TParallelRunners.Create;
              PopulateGrid(TempRunners);
              TempRunners.Free;
            end;
          DelRow := I;
        end;
    end;
  if DelRow > 0 then
    begin
//      ObsLocalChanged := True;
//      NumberRows;
      if (DelRow < dgRunners.RowCount - 1) then
        begin
          dgRunners.Row := DelRow;
        end;
      ClearAllSelections(dgRunners);
//      AssignObservations;
    end;
//begin
//  dgRunners.DeleteRow(dgRunners.SelectedRow);
end;

procedure TFormParallelRunners.dgRunnersButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  DirName: string;
begin
  if ACol = 2 then
    begin
      if dgRunners.Cells[ACol, ARow] <> '' then
        begin
          bffDialog.Directory := dgRunners.Cells[ACol, ARow];
        end
      else
        if ARow > 1 then
          begin
            if dgRunners.Cells[ACol, ARow-1] <> '' then
              begin
                bffDialog.Directory := dgRunners.Cells[ACol, ARow-1];
              end
          end;
      if bffDialog.Execute then
        begin
          dgRunners.Cells[ACol,ARow] := bffDialog.Directory;
          DirName := bffDialog.DisplayName;
          if dgRunners.Cells[0,ARow] = '' then
            begin
              dgRunners.Cells[0,ARow] := DirName;
            end;
        end;
    end;
end;

procedure TFormParallelRunners.dgRunnersColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  if self.Visible then
    begin
      Cols013Width := Width - dgRunners.ColWidths[2];
      Invalidate;
    end;
end;

procedure TFormParallelRunners.dgRunnersKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    if dgRunners.Row = dgRunners.RowCount-1 then
    begin
      AddOneRow;
    end;
  end;
end;

procedure TFormParallelRunners.ExchangeRows(SelRow: Integer; NewRow: Integer);
var
  Cell0: string;
  Cell3: string;
  Cell2: string;
  Cell1: Boolean;
begin
  Cell0 := dgRunners.Cells[0, NewRow];
  Cell1 := dgRunners.Checked[1, NewRow];
  Cell2 := dgRunners.Cells[2, NewRow];
  Cell3 := dgRunners.Cells[3, NewRow];
  dgRunners.Cells[0, NewRow] := dgRunners.Cells[0, SelRow];
  dgRunners.Checked[1, NewRow] := dgRunners.Checked[1, SelRow];
  dgRunners.Cells[2, NewRow] := dgRunners.Cells[2, SelRow];
  dgRunners.Cells[3, NewRow] := dgRunners.Cells[3, SelRow];
  dgRunners.Cells[0, SelRow] := Cell0;
  dgRunners.Checked[1, SelRow] := Cell1;
  dgRunners.Cells[2, SelRow] := Cell2;
  dgRunners.Cells[3, SelRow] := Cell3;
end;

procedure TFormParallelRunners.FormCreate(Sender: TObject);
begin
  FormParallelRunners.NumUsableRunners := 0;
  dgRunners.ColWidths[0] := 110;
  dgRunners.ColWidths[1] := 40;
  dgRunners.ColWidths[2] := 464;
  dgRunners.ColWidths[3] := 165;
end;

procedure TFormParallelRunners.FormResize(Sender: TObject);
begin
  dgRunners.ColWidths[2] := Width - Cols013Width;
end;

procedure TFormParallelRunners.FormShow(Sender: TObject);
var
  muTemp: TModelUse;
//  RowCountLocal: integer;
begin
  if dgRunners.Col >= 0 then dgRunners.Row := 1;
  ParallelRunnersLocal.Assign(ParallelRunnersGlobal);
  PopulateGrid(ParallelRunnersLocal);
  muTemp := muCalib;
  bffDialog.Directory := PCurrent.AbsModelDirectory(muTemp);
  Cols013Width := Width - dgRunners.ColWidths[2];
end;

//procedure TFormParallelRunners.PopulateGrid;
//var
//  I: Integer;
//  NumRunners: Integer;
//  IRow: Integer;
//begin
//  dgRunners.RowCount := 10;
//  dgRunners.Repaint;
//  NumRunners := ParallelRunnersLocal.Count;
//  if NumRunners = 0 then
//  begin
//    dgRunners.RowCount := 2;
//    // Assign defaults for blank row.
//    dgRunners.Cells[0,1] := '';
//    dgRunners.Checked[1,1] := True;
//    dgRunners.Cells[2,1] := '';
//    dgRunners.Cells[3,1] := '0.001';
//  end
//  else
//  begin
//    dgRunners.RowCount := NumRunners + 1;
//  end;
//  dgRunners.Cells[0, 0] := 'Runner Name';
//  dgRunners.Cells[1, 0] := 'Use?';
//  dgRunners.Cells[2, 0] := 'Runner Directory';
//  dgRunners.Cells[3, 0] := 'Expected Run Time (sec)';
//  if NumRunners > 0 then
//  begin
//    for I := 0 to NumRunners - 1 do
//    begin
//      IRow := I + 1;
//      // Populate one row with stored data
//      dgRunners.Cells[0, IRow] := ParallelRunnersLocal.Items[I].Name;
//      dgRunners.Checked[1, IRow] := ParallelRunnersLocal.Items[I].Use;
//      dgRunners.Cells[2, IRow] := ParallelRunnersLocal.Items[I].AbsDirectory;
//      dgRunners.Cells[3, IRow] := FloatToStr(ParallelRunnersLocal.Items[I].ExpectedRunTime);
//    end;
//  end;
//  dgRunners.Repaint;
//end;

procedure TFormParallelRunners.PopulateGrid(Runners: TParallelRunners);
var
  I: Integer;
  NumRunners: Integer;
  IRow: Integer;
begin
  dgRunners.RowCount := 10;
  dgRunners.Repaint;
  NumRunners := Runners.Count;
  if NumRunners = 0 then
  begin
    dgRunners.RowCount := 2;
    // Assign defaults for blank row.
    dgRunners.Cells[0,1] := '';
    dgRunners.Checked[1,1] := True;
    dgRunners.Cells[2,1] := '';
    dgRunners.Cells[3,1] := '0.001';
  end
  else
  begin
    dgRunners.RowCount := NumRunners + 1;
  end;
  dgRunners.Cells[0, 0] := 'Runner Name';
  dgRunners.Cells[1, 0] := 'Use?';
  dgRunners.Cells[2, 0] := 'Runner Directory';
  dgRunners.Cells[3, 0] := 'Expected Run Time (sec)';
  if NumRunners > 0 then
  begin
    for I := 0 to NumRunners - 1 do
    begin
      IRow := I + 1;
      // Populate one row with stored data
      dgRunners.Cells[0, IRow] := Runners.Items[I].Name;
      dgRunners.Checked[1, IRow] := Runners.Items[I].Use;
      dgRunners.Cells[2, IRow] := Runners.Items[I].AbsDirectory;
      dgRunners.Cells[3, IRow] := FloatToStr(Runners.Items[I].ExpectedRunTime);
    end;
  end;
  dgRunners.Invalidate;
end;

function TFormParallelRunners.StoreRunnerData: boolean;
var
  NumRows: Integer;
  ErrMsg: string;
  ParallelRunnersTemp: TParallelRunners;
  IRow: Integer;
  I: Integer;
begin
  // Verify runner data shown in dgRunners
  if VerifyRunnerData(ErrMsg) then
  begin
    result := True;
    NumRows := dgRunners.RowCount - 1;
    if (NumRows = 1) and (IsBlank(dgRunners.Cells[0,1])) and (IsBlank(dgRunners.Cells[2,1])) then
      NumRows := 0;
    if NumRows > 0 then
    begin
      ParallelRunnersTemp := TParallelRunners.Create;
      for IRow := 1 to NumRows do
      begin
        I := IRow - 1;
        ParallelRunnersTemp.Add;
        ParallelRunnersTemp.Items[I].Name := dgRunners.Cells[0, IRow];
        ParallelRunnersTemp.Items[I].Use := dgRunners.Checked[1, IRow];
        ParallelRunnersTemp.Items[I].Directory := dgRunners.Cells[2, IRow];
        ParallelRunnersTemp.Items[I].ExpectedRunTime := StrToFloat(dgRunners.Cells[3, IRow]);
      end;
      NumUsableRunners := ParallelRunnersTemp.NumUsable;
      ParallelRunnersLocal.Assign(ParallelRunnersTemp);
      ParallelRunnersTemp.Free;
    end
  end
  else
  begin
    ShowMessage(ErrMsg);
    result := False;
  end;
end;

function TFormParallelRunners.VerifyRunnerData(var ErrMsg: string): boolean;
// (Disallow rows with some blanks, eliminate rows with all blanks)
var
  IRow, NumRows: integer;
  Name, Dir: string;
  RunTime: double;
begin
  result := True;
  ErrMsg := '';
  NumRows := dgRunners.RowCount;
  if NumRows > 1 then
    begin
      for IRow := NumRows - 1 downto 1 do
        begin
          if result then
            begin
              Name := dgRunners.Cells[0,IRow];
              Dir := dgRunners.Cells[2,IRow];
              if ((Name = '') or (Name = ' ')) and ((Dir = '') or (Dir = ' ')) then
                begin
                  // Row is empty; delete it.
                  if dgRunners.RowCount > 2 then
                    begin
                      dgRunners.DeleteRow(IRow);
                    end
                  else
                    begin
                      dgRunners.Cells[0,1] := '';
                      dgRunners.Cells[2,1] := '';
                    end;
                end
              else
                begin
                  // If either Name or Dir is blank, it's an error.
                  if (Name = '') or (Name = ' ') then
                    begin
                      ErrMsg := 'Runner name missing for runner directory "'
                                + Dir + '"';
                      result := False;
                    end
                  else if (Dir = '') or (Dir = ' ') then
                    begin
                      ErrMsg := 'Runner directory missing for runner "'
                                + Name + '"';
                      result := False;
                    end
                  else
                    try
                     RunTime := StrToFloat(dgRunners.Cells[3,IRow]);
                      if Runtime <= 0 then
                        begin
                          ErrMsg := 'Expected Run Time must be > 0 for runner "'
                                    + Name + '"';
                          result := False;
                        end;
                    except
                      on EConvertError do
                        begin
                          ErrMsg := 'Error converting "' + dgRunners.Cells[3,IRow]
                              + '" to a floating-point number for runner "'
                              + Name + '"';
                          result := False;
                        end;
                    end;
                end;
            end
        end;
    end;
end;

procedure TFormParallelRunners.AddOneRow;
var
  IRow: Integer;
begin
  IRow := dgRunners.RowCount;
  dgRunners.RowCount := dgRunners.RowCount + 1;
  dgRunners.Row := IRow;
  dgRunners.Checked[1, IRow] := True;
end;

initialization
  ParallelRunnersLocal := TParallelRunners.Create;

finalization
  ParallelRunnersLocal.Free;

end.
