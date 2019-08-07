unit frmRunnerFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, StdCtrls, Buttons,
  JvBaseDlg, JvFindFiles, JvDialogs,
  JupiterUnit, GlobalData, GlobalTypesUnit, Utilities;

type
  TFormRunnerFiles = class(TForm)
    dgRunnerFiles: TRbwDataGrid4;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    odRunnerFile: TJvOpenDialog;
    OpenDialog1: TOpenDialog;
    btnAddRow: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure dgRunnerFilesButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure FormShow(Sender: TObject);
    procedure dgRunnerFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure btnAddRowClick(Sender: TObject);
  private
    { Private declarations }
    RunnerFilesLocal: TRunnerFiles;
    procedure PopulateGrid(const RunnerFiles: TRunnerFiles);
    function StoreRunnerFiles(): boolean;
    function VerifyRunnerFiles(Errmsg: string): boolean;
    procedure AddOneRow;
  public
    { Public declarations }
  end;

var
  FormRunnerFiles: TFormRunnerFiles;

implementation

{$R *.dfm}

{ TFormRunnerFiles }

procedure TFormRunnerFiles.btnAddRowClick(Sender: TObject);
begin
  AddOneRow;
end;

procedure TFormRunnerFiles.btnOKClick(Sender: TObject);
begin
  if StoreRunnerFiles then
    begin
      RunnerFilesGlobal.Assign(RunnerFilesLocal);
      self.ModalResult := mrOK;
    end
  else
    begin
      self.ModalResult := 0;
    end;
end;

procedure TFormRunnerFiles.dgRunnerFilesButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  I, IRow, J: integer;
  TempString: string;
begin
  if dgRunnerFiles.Cells[ACol, ARow] <> '' then
    begin
      odRunnerFile.InitialDir := ExtractFileDir(dgRunnerFiles.Cells[ACol, ARow]);
    end
  else
    begin
      if ARow > 1 then
        begin
          if dgRunnerFiles.Cells[ACol, ARow-1] <> '' then
            begin
              odRunnerFile.InitialDir := ExtractFileDir(dgRunnerFiles.Cells[ACol, ARow-1]);
            end;
        end;
    end;
  if odRunnerFile.Execute then
    begin
      dgRunnerFiles.Cells[ACol,ARow] := odRunnerFile.FileName;
      if odRunnerFile.Files.Count > 1 then
        begin
          for I := 1 to odRunnerFile.Files.Count - 1 do
            begin
              dgRunnerFiles.InsertRow(dgRunnerFiles.RowCount);
              IRow := dgRunnerFiles.RowCount - 1;
              dgRunnerFiles.Cells[ACol,IRow] := odRunnerFile.Files.Strings[I];
            end;
        end;
      // Eliminate duplicate file names.
      if dgRunnerFiles.RowCount > 2 then
        begin
          for I := 1 to dgRunnerFiles.RowCount - 2 do
            begin
              TempString := dgRunnerFiles.Cells[0,I];
              for J := dgRunnerFiles.RowCount-1 downto I+1 do
                begin
                  if AnsiSameText(dgRunnerFiles.Cells[0,J],TempString) then
                    begin
                      dgRunnerFiles.Cells[0,J] := '';
                      dgRunnerFiles.DeleteRow(J);
                    end;
                end;
            end;
        end;
      // Delete empty rows.
      if dgRunnerFiles.RowCount > 2 then
        begin
          for I := dgRunnerFiles.RowCount-1 downto 2 do
            begin
              if IsBlank(dgRunnerFiles.Cells[0,I]) then
                dgRunnerFiles.DeleteRow(I);
            end;
        end;
      dgRunnerFiles.Invalidate;
      dgRunnerFiles.Repaint;
    end;
end;

procedure TFormRunnerFiles.dgRunnerFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    if dgRunnerFiles.Row = dgRunnerFiles.RowCount-1 then
    begin
      AddOneRow;
    end;
  end;
end;

procedure TFormRunnerFiles.FormCreate(Sender: TObject);
begin
  RunnerFilesLocal := TRunnerFiles.Create;
end;

procedure TFormRunnerFiles.FormDestroy(Sender: TObject);
begin
  RunnerFilesLocal.Free;
  odRunnerFile.Free;
end;

procedure TFormRunnerFiles.FormResize(Sender: TObject);
begin
  dgRunnerFiles.ColWidths[0] := ClientWidth;
end;

procedure TFormRunnerFiles.FormShow(Sender: TObject);
var
  muTemp: TModelUse;
begin
  RunnerFilesLocal.Assign(RunnerFilesGlobal);
  PopulateGrid(RunnerFilesLocal);
  muTemp := muCalib;
  odRunnerFile.InitialDir := PCurrent.AbsModelDirectory(muTemp);
end;

procedure TFormRunnerFiles.PopulateGrid(const RunnerFiles: TRunnerFiles);
var
  I: Integer;
  NumFiles: Integer;
  IRow: Integer;
begin
  dgRunnerFiles.RowCount := 10;
  dgRunnerFiles.Repaint;
  NumFiles := RunnerFiles.Count;
  if NumFiles = 0 then
  begin
    dgRunnerFiles.RowCount := 2;
    // Assign defaults for blank row.
    dgRunnerFiles.Cells[0,1] := '';
  end
  else
  begin
    dgRunnerFiles.RowCount := NumFiles + 1;
  end;
  dgRunnerFiles.Cells[0, 0] := 'File Name';
  if NumFiles > 0 then
  begin
    for I := 0 to NumFiles - 1 do
    begin
      IRow := I + 1;
      // Populate one row with stored data
      dgRunnerFiles.Cells[0, IRow] := RunnerFiles.Items[I].FileName;
    end;
  end;
  dgRunnerFiles.Repaint;
end;

function TFormRunnerFiles.StoreRunnerFiles: boolean;
var
  NumRows: Integer;
  ErrMsg: string;
  RunnerFilesTemp: TRunnerFiles;
  IRow: Integer;
  I: Integer;
begin
  // Verify runner data shown in dgRunners
  if VerifyRunnerFiles(ErrMsg) then
  begin
    result := True;
    NumRows := dgRunnerFiles.RowCount - 1;
    if (NumRows = 1) and (IsBlank(dgRunnerFiles.Cells[0,1])) then
      NumRows := 0;
    if NumRows > 0 then
    begin
      RunnerFilesTemp := TRunnerFiles.Create;
      try
        for IRow := 1 to NumRows do
        begin
          I := IRow - 1;
          RunnerFilesTemp.Add;
          RunnerFilesTemp.Items[I].FileName := dgRunnerFiles.Cells[0, IRow];
        end;
        RunnerFilesLocal.Assign(RunnerFilesTemp);
      finally
        RunnerFilesTemp.Free;
      end;
    end
  end
  else
  begin
    ShowMessage(ErrMsg);
    result := False;
  end;
end;

function TFormRunnerFiles.VerifyRunnerFiles(Errmsg: string): boolean;
// (Eliminate blank rows)
var
  IRow, NumRows: integer;
  Name: string;
  TempRunnerFiles: TRunnerFiles;
begin
  result := True;
  ErrMsg := '';
  NumRows := dgRunnerFiles.RowCount;
  if NumRows > 1 then
    begin
      for IRow := NumRows - 1 downto 1 do
        begin
          if result then
            begin
              Name := dgRunnerFiles.Cells[0,IRow];
              if ((Name = '') or (Name = ' ')) then
                begin
                  // Row is empty; delete it.
                  if dgRunnerFiles.RowCount > 2 then
                    begin
                      dgRunnerFiles.DeleteRow(IRow);
                    end
                  else
                    begin
                      dgRunnerFiles.Cells[0,1] := '';
                      TempRunnerFiles := TRunnerFiles.Create;
                      PopulateGrid(TempRunnerFiles);
                      TempRunnerFiles.Free;
                    end;
                end
            end
        end;
    end;
end;

procedure TFormRunnerFiles.AddOneRow;
begin
  dgRunnerFiles.RowCount := dgRunnerFiles.RowCount + 1;
  dgRunnerFiles.Row := dgRunnerFiles.RowCount - 1;
end;

end.
