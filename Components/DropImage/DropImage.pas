unit DropImage;

interface

uses
  Windows, Messages, ShellAPI, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TFileDropEvent = procedure(Sender : TObject; files: tstringlist) of object;

  TFileDropImage = class(TWinControl)
  private
    FFileDrop: TFileDropEvent;
    FImage: TImage;
    procedure WMDROPFILES(var Message: TWMDROPFILES); message WM_DROPFILES;
    procedure SetAcceptFiles(const Value: Boolean);
    { Private declarations }
  protected
  { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    property AcceptFiles: Boolean write SetAcceptFiles stored False;
    { Public declarations }
  published
    property Image : TImage read FImage;
    property OnFileDrop: TFileDropEvent read FFileDrop write FFileDrop;
    { Published declarations }
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TFileDropImage]);
end;

{ TFileDropImage }

constructor TFileDropImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage:= TImage.Create(self);
  FImage.Parent := self;
  FImage.Align := alClient;	
  FFileDrop := nil;
  Width := 32;
  Height := 32;
end;

procedure TFileDropImage.SetAcceptFiles(const Value: Boolean);
begin
    DragAcceptFiles(Handle, Value);
end;

procedure TFileDropImage.WMDROPFILES(var Message: TWMDROPFILES);
var
  NumFiles: integer;
  buffer: array[0..255] of char;
  i: integer;
  l: TStringList;
begin
  if Assigned(FFiledrop) then
  begin
    l := TStringList.Create;
    try
      NumFiles := DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0);
      {thanks to Mike Heydon for D5 adjustment of parameters}
      for i := 0 to NumFiles - 1 do {Accept the dropped file}
      begin
        DragQueryFile(Message.Drop, i, buffer, sizeof(buffer));
        l.append(StrPas(buffer))
      end;
      FFileDrop(self, l);
    finally
      l.free
    end;
  end
end;

end.
