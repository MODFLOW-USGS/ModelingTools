{@abstract(The main purpose of @name is to define @link(TfrmHintDelay) which is
  used to set the hint delay time.)}
unit frmHintDelayUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, ArgusDataEntry;

type
  {@abstract(@name is used to set the hint delay time.)
  See TfrmGoPhast.@link(TfrmGoPhast.miHintDelayClick).}
  TfrmHintDelay = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // @name closes the @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TLabel;
    // @name displays "Hint display time (s)".
    lblHintDisplayTime: TLabel;
    // @name: TRbwDataEntry;
    // @name shows the hint delay time.
    rdeHintDelay: TRbwDataEntry;
    // @name creates an instance of @classname.
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
  private
    // @name retrieves the hint delay time.
    procedure GetData;
    // @name sets the hint delay time.
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit;

{$R *.dfm}

{ TfrmHintDelay }

procedure TfrmHintDelay.GetData;
begin
  rdeHintDelay.Text := FloatToStr(Application.HintHidePause / 1000);
end;

procedure TfrmHintDelay.SetData;
begin
  Application.HintHidePause := Round(StrToFloat(rdeHintDelay.Text) * 1000);
end;

procedure TfrmHintDelay.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmHintDelay.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

end.

