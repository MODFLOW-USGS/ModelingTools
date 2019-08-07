unit frmUcodeBasicSettingsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvToolEdit, Mask, JvExMask, GlobalBasicData, Utilities,
  Buttons;

type
  TFormUcodeFileNames = class(TForm)
    gbMainModel: TGroupBox;
    edtOutputPrefix: TEdit;
    lblOutputPrefix: TLabel;
    lblMainInputFile: TLabel;
    gbPredModel: TGroupBox;
    lblPredInputName: TLabel;
    Label1: TLabel;
    edtOutputPrefixPred: TEdit;
    fedMainInputFileName: TJvFilenameEdit;
    fedPredInputFileName: TJvFilenameEdit;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MainInputFile: string;
    MainInputFilePred: string;
    OutputPrefix: string;
    OutputPrefixPred: string;
  end;

var
  FormUcodeFileNames: TFormUcodeFileNames;

implementation

{$R *.dfm}

procedure TFormUcodeFileNames.btnOKClick(Sender: TObject);
begin
  if fedMainInputFileName.FileName <> MainInputFile then
    begin
      MainInputFile := fedMainInputFileName.FileName;
      ProjChanged := True;
    end;
  if edtOutputPrefix.Text <> OutputPrefix then
    begin
      OutputPrefix := edtOutputPrefix.Text;
      ProjChanged := True;
    end;
  if fedPredInputFileName.FileName <> MainInputFilePred then
    begin
      MainInputFilePred := fedPredInputFileName.FileName;
      ProjChanged := True;
    end;
  if edtOutputPrefixPred.Text <> OutputPrefixPred then
    begin
      OutputPrefixPred := edtOutputPrefixPred.Text;
      ProjChanged := True;
    end;
end;

procedure TFormUcodeFileNames.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

procedure TFormUcodeFileNames.FormShow(Sender: TObject);
begin
  fedMainInputFileName.FileName := MainInputFile;
  edtOutputPrefix.Text := OutputPrefix;
  fedPredInputFileName.FileName := MainInputFilePred;
  edtOutputPrefixPred.Text := OutputPrefixPred;
end;

end.
