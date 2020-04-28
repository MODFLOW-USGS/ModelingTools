unit frameActivatibleFeatureUnit;

interface

uses
  StdCtrls, Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TActivateEvent = procedure(Sender: TObject; CheckState: TCheckBoxState) of object;

  TframeActivatibleFeature = class(TFrame)
  private
    FOnActivate: TActivateEvent;
    procedure SetCheckState(const Value: TCheckBoxState);
    { Private declarations }
  protected
    FCheckState: TCheckBoxState;
  public
    property OnActivate: TActivateEvent read FOnActivate write FOnActivate;
    procedure RefreshNodeState;
    property CheckState: TCheckBoxState read FCheckState write SetCheckState;
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframeActivatibleFeature.RefreshNodeState;
begin
  if Assigned(OnActivate) then
  begin
    OnActivate(self, FCheckState);
  end;
end;

procedure TframeActivatibleFeature.SetCheckState(const Value: TCheckBoxState);
begin
  FCheckState := Value;
end;

end.
