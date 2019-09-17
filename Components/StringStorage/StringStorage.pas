unit StringStorage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TStringStorage = class(TComponent)
  private
    FStringVariable : string;
    { Private declarations }
  published
    property StringVariable : string read FStringVariable write FStringVariable;
    { Published declarations }
  end;

  TRealStorage = class(TComponent)
  private
    FReal: double;
  published
    property Real : double read FReal write FReal;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TStringStorage, TRealStorage]);
end;

{ TRealStorage }

end.
