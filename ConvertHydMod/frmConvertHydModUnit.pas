unit frmConvertHydModUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, JvExMask,
  JvToolEdit, Vcl.ExtCtrls;

type
  TfrmConvertHydMod = class(TForm)
    edHydMod: TJvFilenameEdit;
    lblHydMod: TLabel;
    edText: TJvFilenameEdit;
    lblText: TLabel;
    btnConvert: TButton;
    rgOutputType: TRadioGroup;
    procedure btnConvertClick(Sender: TObject);
    procedure edHydModExit(Sender: TObject);
    procedure edHydModAfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure rgOutputTypeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmConvertHydMod: TfrmConvertHydMod;

implementation

uses
  System.IOUtils, ReadModflowArrayUnit;

{$R *.dfm}

procedure TfrmConvertHydMod.btnConvertClick(Sender: TObject);
var
  HydModData: THydModData;
  OutputFile: TStreamWriter;
  ALine: string;
  StringBuilder: TStringBuilder;
  LabelIndex: Integer;
  TimeIndex: Integer;
  Separator: Char;
begin
  if not TFile.Exists(edHydMod.FileName) or (edText.FileName = '')  then
  begin
    Beep;
    ShowMessage('Please specify file names');
    Exit
  end;

  if TFile.Exists(edText.FileName) then
  begin
    Beep;
    if not (MessageDlg(Format('Are you sure you want to replace %s?',
      [edText.FileName]), mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes) then
    begin
      Exit
    end;
  end;

  if edText.FilterIndex = 2 then
  begin
    Separator := #9;
  end
  else
  begin
    Separator := ',';
  end;

  Screen.Cursor := crHourGlass;
  HydModData := THydModData.Create;
  try
    HydModData.ReadFile(edHydMod.FileName);
    OutputFile := TFile.CreateText(edText.FileName);
    StringBuilder := TStringBuilder.Create;
    try
      ALine := 'Time Unit = ' + HydModData.TimeUnit.ToString;
      OutputFile.WriteLine(ALine);

      StringBuilder.Append('Time');
      for LabelIndex := 0 to HydModData.LabelCount - 1 do
      begin
        StringBuilder.Append(Separator);
        StringBuilder.Append(HydModData.Labels[LabelIndex]);
      end;
      OutputFile.WriteLine(StringBuilder.ToString);

      for TimeIndex := 0 to HydModData.TimeCount - 1 do
      begin
        StringBuilder.Clear;
        StringBuilder.Append(HydModData.Times[TimeIndex]);
        for LabelIndex := 0 to HydModData.LabelCount - 1 do
        begin
          StringBuilder.Append(Separator);
          StringBuilder.Append(HydModData.Values[LabelIndex, TimeIndex]);
        end;
        OutputFile.WriteLine(StringBuilder.ToString);
      end;
    finally
      StringBuilder.Free;
      OutputFile.Free;
    end;
  finally
    HydModData.Free;
    Screen.Cursor := crDefault;
  end;
  Beep;
  ShowMessage('Done')
end;

procedure TfrmConvertHydMod.edHydModAfterDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  if rgOutputType.ItemIndex = 0 then
  begin
    edText.FileName := ChangeFileExt(AName, '.csv');
  end
  else
  begin
    edText.FileName := ChangeFileExt(AName, '.txt');
  end;

end;

procedure TfrmConvertHydMod.edHydModExit(Sender: TObject);
begin
  if edText.FileName = '' then
  begin
    if rgOutputType.ItemIndex = 0 then
    begin
      edText.FileName := ChangeFileExt(edHydMod.FileName, '.csv');
    end
    else
    begin
      edText.FileName := ChangeFileExt(edHydMod.FileName, '.txt');
    end;
  end;
end;

procedure TfrmConvertHydMod.rgOutputTypeClick(Sender: TObject);
begin
  edText.FilterIndex := rgOutputType.ItemIndex+1;
  if edText.FileName <> '' then
  begin
    if rgOutputType.ItemIndex = 0 then
    begin
      edText.FileName := ChangeFileExt(edText.FileName, '.csv');
    end
    else
    begin
      edText.FileName := ChangeFileExt(edText.FileName, '.txt');
    end;
  end;
end;

end.
