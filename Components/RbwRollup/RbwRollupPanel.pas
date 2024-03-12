unit RbwRollupPanel;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics,
  Winapi.Messages;

type
  TRbwRollupPanel = class(TPanel)
  private
    FpnlHitTest: TPanel;
    FLabel: TPaintBox;
    FCollapsed: Boolean;
    FMainWidth: Integer;
    FRollupCaption: TCaption;
    procedure SetCollapsed(const Value: Boolean);
    procedure SetRollupCaption(const Value: TCaption);
//    function GetRollupCaption: TCaption;
    procedure ToggleCollapsed(Sender: TObject);
    function GetWidth: Integer;
    procedure SetWidth(const Value: Integer);
    procedure DrawRollupCaption(Sender: TObject);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    function GetLabelWidth: Integer;
    procedure SetLabelWidth(const Value: Integer);
    procedure SetExpandedWidth(const Value: integer);

    { Private declarations }
  protected
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property RollupCaption: TCaption read FRollupCaption write SetRollupCaption;
    property Width: Integer read GetWidth write SetWidth;
    property Font: TFont read GetFont write SetFont;
    property LabelWidth: Integer read GetLabelWidth write SetLabelWidth;
    property ExpandedWidth: integer read FMainWidth write SetExpandedWidth;
    { Published declarations }
  end;

procedure Register;

implementation

{#BACKUP *.ICO}

procedure Register;
begin
  RegisterComponents('RBW', [TRbwRollupPanel]);
end;

{ TRbwRollupPanel }

procedure TRbwRollupPanel.CMFontChanged(var Message: TMessage);
begin
  FLabel.Font := Font;
end;

constructor TRbwRollupPanel.Create(AOwner: TComponent);
begin
  inherited;
  FpnlHitTest := TPanel.Create(self);
  FpnlHitTest.Parent := self;
  FpnlHitTest.Align := alLeft;
  FpnlHitTest.Width := 27;
//  FpnlHitTest.BiDiMode := bdRightToLeft;
  FpnlHitTest.OnClick := ToggleCollapsed;
  FLabel := TPaintBox.Create(Self);
  FLabel.Parent := FpnlHitTest;
  FLabel.Align := alClient;
  FLabel.OnClick := ToggleCollapsed;
  ShowCaption := False;
  FLabel.OnPaint := DrawRollupCaption;
  FLabel.Font.Orientation := 900;
end;

procedure TRbwRollupPanel.DrawRollupCaption(Sender: TObject);
begin
  FLabel.Canvas.Font.Orientation := -900;
  FLabel.Canvas.TextOut(FLabel.Width-2, 10, FRollupCaption);
end;

function TRbwRollupPanel.GetFont: TFont;
begin
  Result := inherited Font;
end;

function TRbwRollupPanel.GetLabelWidth: Integer;
begin
  result := FpnlHitTest.Width;
end;

function TRbwRollupPanel.GetWidth: Integer;
begin
  result := inherited Width;
end;

procedure TRbwRollupPanel.SetCollapsed(const Value: Boolean);
begin
  if FCollapsed <> Value then
  begin
    FCollapsed := Value;
    if FCollapsed then
    begin
      FMainWidth := Width - FpnlHitTest.Width;
    end;
    if FCollapsed then
    begin
      ClientWidth := FpnlHitTest.Width;
    end
    else
    begin
      ClientWidth := FpnlHitTest.Width + FMainWidth;
    end;
  end;
end;

procedure TRbwRollupPanel.SetExpandedWidth(const Value: integer);
begin
  if FMainWidth <> Value then
  begin
    FMainWidth := Value;
    if not Collapsed then
    begin
      Width := FMainWidth + FpnlHitTest.Width;
    end;
  end;
end;

procedure TRbwRollupPanel.SetFont(const Value: TFont);
begin
  inherited Font := Value;
  FLabel.Font := Value;
end;

procedure TRbwRollupPanel.SetLabelWidth(const Value: Integer);
begin
  FpnlHitTest.Width := Value;
end;

procedure TRbwRollupPanel.SetRollupCaption(const Value: TCaption);
begin
  FRollupCaption := Value;
  FLabel.Invalidate;
end;

procedure TRbwRollupPanel.SetWidth(const Value: Integer);
begin
  inherited Width := Value;
  if not Collapsed then
  begin
    FMainWidth := Width - FpnlHitTest.Width;
  end;
end;

procedure TRbwRollupPanel.ToggleCollapsed(Sender: TObject);
begin
  Collapsed := not Collapsed;
end;

end.
