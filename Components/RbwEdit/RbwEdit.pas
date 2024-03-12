unit RbwEdit;

interface

uses
  Graphics, SysUtils, Classes, Controls, StdCtrls;

type
  TRbwEdit = class(TEdit)
  private
    FDisabledColor: TColor;
    FEnabledColor: TColor;
    procedure SetDisabledColor(const Value: TColor);
    procedure SetEnabledColor(const Value: TColor);
    { Private declarations }
  protected
    procedure SetEnabled(Value: boolean); override;
    { Protected declarations }
  public
    Constructor Create(AnOwner: TComponent); override;
    { Public declarations }
  published
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor
      default clBtnFace;
    property EnabledColor: TColor read FEnabledColor write SetEnabledColor
      default clWindow;
    { Published declarations }
  end;

procedure Register;

implementation

{#BACKUP *.ICO}

procedure Register;
begin
  RegisterComponents('RBW', [TRbwEdit]);
end;

{ TRbwEdit }

constructor TRbwEdit.Create(AnOwner: TComponent);
begin
  inherited;
  FDisabledColor := clBtnFace;
  FEnabledColor := clWindow;
end;


procedure TRbwEdit.SetDisabledColor(const Value: TColor);
begin
  FDisabledColor := Value;
  if not Enabled then
  begin
    Color := Value;
  end;
end;

procedure TRbwEdit.SetEnabled(Value: boolean);
begin
  if Enabled <> Value then
  begin
    inherited ;
    if Value then
    begin
      Color := EnabledColor;
    end
    else
    begin
      Color := DisabledColor;
    end;
  end;
end;

procedure TRbwEdit.SetEnabledColor(const Value: TColor);
begin
  FEnabledColor := Value;
  if Enabled then
  begin
    Color := Value;
  end;
end;

end.
