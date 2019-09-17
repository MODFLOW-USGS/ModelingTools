unit siComboBox;

{
By Igor Siticov
http://www.sicomponents.com/
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TsiComboBox = class(TComboBox)
  private
    { Private declarations }
    FFirst: boolean;
    FRect: TRect;
    FListHandle: HWND;
    FCWX: integer;
    FCX: integer;
    FCY: integer;
  protected
    { Protected declarations }
// We should override this for changing behavior
    procedure WndProc(var Message: TMessage); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
// Properties correspond the width, top and left positions of drop down list:
    property CWX: integer read FCWX write FCWX default 0;
    property CX: integer read FCX write FCX default 0;
    property CY: integer read FCY write FCY default 0;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('siComponents', [TsiComboBox]);
end;

{ TsiComboBox }

constructor TsiComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FFirst := True;
  FCX := 0;
  FCWX := 0;
  FCY := 0;
end;

procedure TsiComboBox.WndProc(var Message: TMessage);
//Start modifications added by Bjarne Winkler
Var
  CurRect: TRect;
//End modifications added by Bjarne Winkler
//Start modifications added by Richard Winston
  ItemCount : integer;
// end modifications by Richard Winston
begin
  inherited;
  if Message.Msg = WM_CTLCOLORLISTBOX then begin
    // Getting handle of list box
    FListHandle := Message.LParam;
    // Getting rectangle of list box
//Start modifications added by Bjarne Winkler
    GetWindowRect ( FListHandle, CurRect );
    If (CurRect.Left <> FRect.Left) Or (CurRect.Top <> FRect.Top) Then
    Begin
      FRect := CurRect;
    End;
//End modifications added by Bjarne Winkler
// start modifications by Richard Winston

  ItemCount := Items.Count;
  if ItemCount = 0 then ItemCount := 1;
  if Itemcount > DropDownCount then
  begin
    ItemCount := DropDownCount;
  end;
// end modifications by Richard Winston
    // Resizing list box rectangle
    MoveWindow ( FListHandle, FRect.left + FCX, FRect.top + FCY,
                           (FRect.right - FRect.left + FCWX),
                           ItemHeight*ItemCount + 2, TRUE );
{    MoveWindow ( FListHandle, FRect.left + FCX, FRect.top + FCY,
                           (FRect.right - FRect.left + FCWX),
                           FRect.bottom - FRect.top, TRUE );  }
// end modifications by Richard Winston
  end;
end;

end.

