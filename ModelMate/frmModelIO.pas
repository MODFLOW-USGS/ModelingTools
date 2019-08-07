unit frmModelIO;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DataGrid, Math, StdCtrls,
  GlobalTypesUnit, GlobalBasicData, GlobalData, JvBaseDlg, Utilities,
  JupiterUnit, Buttons;

type

  TFormModelIO = class(TForm)
    dgModelIO: TEcDataGrid;
    odModelFile: TOpenDialog;
    odAppFile: TOpenDialog;
    Label1: TLabel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormResize(Sender: TObject);
    procedure dgModelIOEditButtonClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure SaveFileNames(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure dgModelIOUserChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ModelUse: TModelUse;
    MIOPairs: TModelIOPairs;
  end;

var
  FormModelIO: TFormModelIO;
  ModelIOFormUse: TMIOFileUse;

implementation

{$R *.dfm}

var
  IFormErrors: integer;
  DataChanged: boolean;

procedure TFormModelIO.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormModelIO.btnOKClick(Sender: TObject);
var
  //Msg: string;
  Continue: boolean;
  //ModRes: integer;
begin
  Continue := True;
  SaveFileNames(Sender);
  if IFormErrors > 0 then
    begin
//      Msg := 'Warning:  One or files do not exist. ' +
//             'Click OK to continue anyway or click Cancel ' +
//             'to return to Program Locations window';
//      ModRes := MessageDlg(Msg, mtConfirmation, [mbOK, mbCancel], 0, mbOK);
//      if ModRes = mrCancel then Continue := False;
      Continue := false;
      self.ModalResult := mrNone;
    end;
  if Continue then
    begin
      case ModelIOFormUse of
        fuInput:
          begin
            case ModelUse of
              muCalib: PCurrent.MIFiles.Assign(MIOPairs);
              muPred: PCurrent.MIFilesPred.Assign(MIOPairs);
            end;
          end;
        fuOutput:
          begin
            case ModelUse of
              muCalib: PCurrent.MOFiles.Assign(MIOPairs);
              muPred: PCurrent.MOFilesPred.Assign(MIOPairs);
            end;
          end;
      end;
      if DataChanged then
      begin
        ProjChanged := true;
      end;
      Close;
    end;
end;

procedure TFormModelIO.SaveFileNames(Sender: TObject);
var
  CountPairs, I, J, NRows: integer;
  Msg, ModFile, AppFile: string;
  AppFileName, ModFileName: string;
  NonBlankPair: bool;
begin
  case ModelIOFormUse of
    fuInput:
      begin
        ModFile := 'model-input files';
        AppFile := 'template files';
      end;
    fuOutput:
      begin
        ModFile := 'model-ouput files';
        AppFile := 'instruction files';
      end;
  end;
  IFormErrors := 0;
  CountPairs := 0;
  Msg := '';
  NRows := dgModelIO.RowCount;
  // Check that rows are populated with file name pairs or are blank
  for I := 1 to NRows - 1 do
    begin
      NonBlankPair := true;
      if dgModelIO.Cells[0,I] <> '' then
        begin
          if dgModelIO.Cells[1,I] = '' then
            begin
              Msg := 'Error: Mismatch between ' + Modfile + ' and ' + AppFile;
              IFormErrors := IFormErrors + 1;
              NonBlankPair := false;
            end;
        end
      else
        begin
          NonBlankPair := false;
          begin
            if dgModelIO.Cells[1,I] <> '' then
              begin
                Msg := 'Error: Mismatch between ' + Modfile + ' and ' + AppFile;
                IFormErrors := IFormErrors + 1;
              end;
          end;
        end;

      if NonBlankPair then
      begin
        CountPairs := CountPairs + 1;
      end;
    end;

    // Check for invalid duplicate file names
    if (Msg = '') and (CountPairs > 1) then
    begin
      // Check that file names do not duplicate earlier entries
      for I := 2 to CountPairs do
      begin
        ModFileName := dgModelIO.Cells[0,I];
        AppFileName := dgModelIO.Cells[1,I];
        for J := 1 to I-1 do
        begin
          // Duplicate app file (template or instruction file) is an error regardless of form use.
          if AppFileName = dgModelIO.Cells[1,J] then
          begin
            case ModelIOFormuse of
              fuInput:
                Msg := 'Error: Duplicate template file: ' + AppFileName;
              fuOutput:
                Msg := 'Error: Duplicate instruction file: ' + AppFileName;
            end;
            IFormErrors := IFormErrors + 1;
          end;

          // Duplicate model file is an error only for model-input files
          if ModFileName = dgModelIO.Cells[0,J] then
          begin
            if ModelIOFormUse = fuInput then
            begin
              Msg := 'Error: Duplicate model-input file: ' + ModFileName;
              IFormErrors := IFormErrors + 1;
            end;
          end;
        end;
      end;
    end;

    if Msg <> '' then
      begin
        ShowMessage(Msg);
      end
    else
      begin
        J := -1;
        MIOPairs.Clear;
        for I := 1 to NRows - 1 do
          begin
            if dgModelIO.Cells[0,I] <> '' then
              begin
                J := J+1;
                MIOPairs.Add;
                MIOPairs.Items[J].ModelFile := dgModelIO.Cells[0,I];
                MIOPairs.Items[J].AppFile := dgModelIO.Cells[1,I];
              end;
          end;
        ProjChanged := True;
      end;
end;

procedure TFormModelIO.dgModelIOEditButtonClick(Sender: TObject);
var
  IC, IR: integer;
begin
  IR := dgModelIO.Row;
  IC := dgModelIO.Col;
  if IC = 0 then
    begin
      odModelFile.FileName := dgModelIO.Cells[IC,IR];
      if odModelFile.Execute then
        dgModelIO.Cells[IC,IR] := odModelFile.FileName;
    end
  else
    begin
      odAppFile.FileName := dgModelIO.Cells[IC,IR];
      if odAppFile.Execute then
        dgModelIO.Cells[IC,IR] := odAppFile.FileName;
    end;
end;

procedure TFormModelIO.dgModelIOUserChanged(Sender: TObject);
begin
  DataChanged := True;
  //ProjChanged := True;
end;

procedure TFormModelIO.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  MIOPairs := TModelIOPairs.Create;
end;

procedure TFormModelIO.FormDestroy(Sender: TObject);
begin
  MIOPairs.Free;
end;

procedure TFormModelIO.FormResize(Sender: TObject);
// Maintain proportional column spacing
var
  W, Wmin, W0int, W1int, Wscroll: Word;
  dW0, dW0new, dW1, dW1new, dP: double;
begin
  Wmin := 100;
  WScroll := 25;
  dW0 := dgModelIO.ColWidths[0];
  dW1 := dgModelIO.ColWidths[1];
  dP := dW0/(dW0+dW1);
  W := dgModelIO.Width - Wscroll;
  dW0new := dP * W;
  W0int := trunc(dW0new);
  if W0int < Wmin then W0int := Wmin;
  dW1new := (1.0-dP) * W;
  W1int := trunc(dW1new);
  if W1int < Wmin then W1int := Wmin;
  dgModelIO.ColWidths[0] := W0int;
  dgModelIO.ColWidths[1] := W1int;
end;

procedure TFormModelIO.FormShow(Sender: TObject);
var
  I: integer;
  AbsDir, AbsPath, RelPath: string;
begin
//  AbsDir := PCurrent.AbsAppDirectory(ModelUse);
  AbsDir := ProjectDirectory;
  // Clear cells
  case MOdelIOFormUse of
    fuInput:
      begin
        case ModelUse of
          muCalib: MIOPairs.Assign(PCurrent.MIFiles);
          muPred: MIOPairs.Assign(PCurrent.MIFilesPred);
        end;
      end;
    fuOutput:
      begin
        case ModelUse of
          muCalib: MIOPairs.Assign(PCurrent.MOFiles);
          muPred: MIOPairs.Assign(PCurrent.MOFilesPred);
        end;
      end;
  end;
  dgModelIO.RowCount := MIOPairs.Count + 2;
  for I := 1 to dgModelIO.RowCount - 1 do
    begin
      dgModelIO.Cells[0,I] := '';
      dgModelIO.Cells[1,I] := '';
    end;
  // Populate cells from MIFiles or MOFiles or MIFilesPred or MOFilesPred.
  case ModelIOFormUse of
    fuInput:
      begin
        dgModelIO.Columns[0].Title.Caption := 'Model-Input File';
        dgModelIO.Columns[1].Title.Caption := 'Template File';
        odAppFile.Filter := 'Jupiter Template Files (*.jtf)|*.jtf|All Files (*.*)|*.*';
        case ModelUse of
          muCalib: Caption := 'Model-Input and Template Files';
          muPred: Caption := 'Predictive-Model Input and Template Files';
        end;
      end;
    fuOutput:
      begin
        dgModelIO.Columns[0].Title.Caption := 'Model-Output File';
        dgModelIO.Columns[1].Title.Caption := 'Instruction File';
        odAppFile.Filter := 'Jupiter Instruction Files (*.jif)|*.jif|All Files (*.*)|*.*';
        case ModelUse of
          muCalib: Caption := 'Model-Output and Instruction Files';
          muPred: Caption := 'Predictive-Model Output and Instruction Files';
        end;
      end;
  end;
  for I := 1 to MIOPairs.Count do
    begin
      RelPath := MIOPairs.Items[I-1].ModelFile;
      AbsPath := PathToAbsPath(AbsDir,RelPath);
      dgModelIO.Cells[0,I] := AbsPath;
      RelPath := MIOPairs.Items[I-1].AppFile;
      AbsPath := PathToAbsPath(AbsDir,RelPath);
      dgModelIO.Cells[1,I] := AbsPath;
    end;
  IFormErrors := 0;
  DataChanged := False;
  Invalidate;
end;

end.
