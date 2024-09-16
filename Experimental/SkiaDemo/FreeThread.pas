unit FreeThread;

interface

uses System.Classes;

type
  TMyThread = class(TThread)
  private
    FAnObject: TObject;
  public
    procedure Execute; override;
    constructor Create(AnObject: TObject);
  end;



implementation

{ TMyThread }

constructor TMyThread.Create(AnObject: TObject);
begin
  FAnObject := AnObject;
  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TMyThread.Execute;
begin
  FAnObject.Free;
end;

end.
