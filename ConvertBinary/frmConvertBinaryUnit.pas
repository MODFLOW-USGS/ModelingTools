unit frmConvertBinaryUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TfrmConvertBinary = class(TForm)
    edtInput: TEdit;
    lblInputBinaryFile: TLabel;
    lblOutputCsvFile: TLabel;
    edtOutputFile: TEdit;
    btnBrowseInput: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    btnBrowseOutput: TButton;
    btnConvert: TButton;
    procedure edtInputExit(Sender: TObject);
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ConvertFile(const InputFileName: string; OutputFileName: string = '');
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmConvertBinary: TfrmConvertBinary;

implementation

uses
  System.IOUtils, FMX.DialogService.Sync, ReadModflowArrayUnit;

{$R *.fmx}

procedure TfrmConvertBinary.btnBrowseInputClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    edtInput.Text := dlgOpen.FileName;
    edtInputExit(nil);
  end;
end;

procedure TfrmConvertBinary.btnBrowseOutputClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    edtOutputFile.Text  := dlgSave.FileName;
  end;

end;

procedure TfrmConvertBinary.btnConvertClick(Sender: TObject);
begin
  ConvertFile(edtInput.Text, edtOutputFile.Text);
  ShowMessage('Conversion complete');
end;

procedure TfrmConvertBinary.ConvertFile(const InputFileName: string; OutputFileName: string = '');
var
  AMessage: string;
  InputFile: TFileStream;
  Precision: TModflowPrecision;
  KSTP, KPER: Integer;
  PERTIM, TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL, NROW, ILAY: Integer;
  AnArray: TModflowDoubleArray;
  OutputFile: TStreamWriter;
  InputFileSize: Int64;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if not TFile.Exists(InputFileName) then
  begin
    AMessage := Format('"%s" does not exist', [InputFileName]);
    TDialogServiceSync.MessageDialog(AMessage, TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
    Exit;
  end;
  try
    InputFile := TFile.OpenRead(InputFileName);
    try

      Precision := CheckArrayPrecision(InputFile);
      if not (Precision in [mpSingle, mpDouble]) then
      begin
        AMessage := Format('Unknown error when opening "%s".', [InputFileName]);
        Assert(False, AMessage);
        Exit;
      end;

      InputFile.Position := 0;
      case Precision of
        mpSingle:
          begin
            ReadSinglePrecisionModflowBinaryRealArray(InputFile,
              KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
          end;
        mpDouble:
          begin
            ReadDoublePrecisionModflowBinaryRealArray(InputFile,
              KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
          end;
      end;

      if OutputFileName = '' then
      begin
        OutputFileName := ChangeFileExt(InputFileName, '.csv')
      end;

      OutputFile := TFile.CreateText(OutputFileName);
      try
        OutputFile.Write('TOTIM,PERTIM,KSTP,KPER,ILAY');
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            OutputFile.Write(Format(',{Row: %d Column: %d}', [RowIndex+1, ColIndex+1]));
          end;
        end;
        OutputFile.WriteLine;

        InputFile.Position := 0;
        InputFileSize := InputFile.Size;
        while (InputFile.Position < InputFileSize) do
        begin
          case Precision of
            mpSingle:
              begin
                ReadSinglePrecisionModflowBinaryRealArray(InputFile,
                  KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
              end;
            mpDouble:
              begin
                ReadDoublePrecisionModflowBinaryRealArray(InputFile,
                  KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
              end;
          end;
          OutputFile.Write(TOTIM);
          OutputFile.Write(',');
          OutputFile.Write(PERTIM);
          OutputFile.Write(',');
          OutputFile.Write(KSTP);
          OutputFile.Write(',');
          OutputFile.Write(KPER);
          OutputFile.Write(',');
          OutputFile.Write(ILAY);
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              OutputFile.Write(',');
              OutputFile.Write(AnArray[RowIndex, ColIndex]);
            end;
          end;
          OutputFile.WriteLine;
        end;

      finally
        OutputFile.Free;
      end;

    finally
      InputFile.Free;
    end;
//
  except on E: Exception do
    TDialogServiceSync.MessageDialog(E.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
  end;
end;

procedure TfrmConvertBinary.edtInputExit(Sender: TObject);
begin
  edtOutputFile.Text := ChangeFileExt(edtInput.Text, '.csv');
end;

procedure TfrmConvertBinary.FormCreate(Sender: TObject);
begin
  if ParamCount >= 2 then
  begin
    ConvertFile(ParamStr(1), ParamStr(2));
    Application.Terminate;
  end
  else if ParamCount = 1 then
  begin
    ConvertFile(ParamStr(1));
    Application.Terminate;
  end;
end;

end.
