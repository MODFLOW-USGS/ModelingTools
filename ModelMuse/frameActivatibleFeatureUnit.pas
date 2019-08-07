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
    { Private declarations }
  protected
    FCheckState: TCheckBoxState;
  public
    property OnActivate: TActivateEvent read FOnActivate write FOnActivate;
    procedure RefreshNodeState;
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

end.
