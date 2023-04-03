{@abstract(The main purpose of @name is to define @link(TfrmVerticalExaggeration)
  which is used to edit the vertical exaggeration.)}
unit frmVerticalExaggerationUnit;

interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, frmCustomGoPhastUnit, ArgusDataEntry;

type
  {@abstract(@name is used to edit the vertical exaggeration)}
  TfrmVerticalExaggeration = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name shows help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TLabel;
    // @name displays "Vertical exaggeration:".
    lblVerticalExaggeration: TLabel;
    // @name: TRbwDataEntry;
    // @name is used to edit the vertical exaggeration.
    rdeVerticalExaggeration: TRbwDataEntry;
    btnDefault: TButton;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name puts the focus in @link(rdeVerticalExaggeration).
    procedure FormShow(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure rdeVerticalExaggerationExit(Sender: TObject);
  private
    // @name displays the vertical exaggeration in @classname.
    procedure GetData;
    // @name checks that a valid vertical exaggeration has been entered.
    // if so, the vertical exaggeration is changed using a
    // @link(TUndoVerticalExaggeration).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, UndoItems;

resourcestring
  StrTheVerticalExagger = 'The Vertical exaggeration must be a positive numb' +
  'er.';

{$R *.dfm}

{ TfrmVerticalExaggeration }

procedure TfrmVerticalExaggeration.GetData;
begin
  rdeVerticalExaggeration.Text :=
    FloatToStr(TUndoVerticalExaggeration.GetOldVE);
end;

procedure TfrmVerticalExaggeration.rdeVerticalExaggerationExit(Sender: TObject);
begin
  inherited;
  if (rdeVerticalExaggeration.Text = '')
    or (rdeVerticalExaggeration.Text = '0') then
  begin
    btnDefaultClick(nil);
  end;
end;

procedure TfrmVerticalExaggeration.SetData;
var
  VE: real;
begin
  if Trim(rdeVerticalExaggeration.Text) = '' then
  begin
    VE := frmGoPhast.DefaultVE;
  end
  else
  begin
    VE := StrToFloat(rdeVerticalExaggeration.Text);
  end;
  if VE = 0 then
  begin
    Beep;
    MessageDlg(StrTheVerticalExagger,
      mtError, [mbOK], 0);
    Exit;
  end;
  ModalResult := mrOK;
  frmGoPhast.UndoStack.Submit(TUndoVerticalExaggeration.Create(VE))
end;

procedure TfrmVerticalExaggeration.btnDefaultClick(Sender: TObject);
begin
  inherited;
  rdeVerticalExaggeration.Text := FloatToStr(frmGoPhast.DefaultVE);
end;

procedure TfrmVerticalExaggeration.btnOKClick(Sender: TObject);
begin
  SetData;
end;

procedure TfrmVerticalExaggeration.FormShow(Sender: TObject);
begin
  inherited;
  rdeVerticalExaggeration.SetFocus;
end;

procedure TfrmVerticalExaggeration.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

end.

