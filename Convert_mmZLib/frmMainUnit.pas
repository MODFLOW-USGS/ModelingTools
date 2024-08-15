unit frmMainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.ZLib;

type
  TfrmMain = class(TForm)
    btn1: TButton;
    dlgOpen1: TOpenDialog;
    Label1: TLabel;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btn1Click(Sender: TObject);
var
  FFileStream: TBufferedFileStream;
  DecompressionStream: TZDecompressionStream;

  var
  FileStream: TFileStream;     // Text file defining one or more objects.
  MemStream: TMemoryStream;    // Temporarily hold an object.
  TextStream: TMemoryStream;   // Text form of a stream.

begin
  if dlgOpen1.Execute then
  begin
    FFileStream := TBufferedFileStream.Create(dlgOpen1.FileName,
      fmOpenRead or fmShareDenyWrite, 0);
    TextStream := TMemoryStream.Create;
    MemStream := TMemoryStream.Create;
    try
      DecompressionStream := TDecompressionStream.Create(FFileStream);
      try
        MemStream.CopyFrom(DecompressionStream);
        MemStream.Position := 0;
        ObjectBinaryToText(MemStream, TextStream);

      finally
        DecompressionStream.Free;
      end;
      FileStream := TFileStream.Create(ChangeFileExt(dlgOpen1.FileName, '.gpt'), fmCreate);
      try
        TextStream.Position := 0;
        FileStream.CopyFrom(TextStream, TextStream.Size);
      finally
        FileStream.Free;
      end;
    finally
      FFileStream.Free;
      MemStream.Free;
      TextStream.Free;
    end;

    Beep;
    ShowMessage('Done');
  end;
end;

end.
