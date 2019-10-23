unit frameAvailableObjectsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  JvExStdCtrls, JvListBox;

type
  TframeAvailableObjects = class(TFrame)
    lblSrcObjects: TLabel;
    lbSrcObjects: TJvListBox;
    lblDstObjects: TLabel;
    lbDstObjects: TJvListBox;
    btnIncObjects: TButton;
    btnIncAllObjects: TButton;
    btnExclObjects: TButton;
    btnExclAllObjects: TButton;
    procedure FrameResize(Sender: TObject);
    procedure btnIncObjectsClick(Sender: TObject);
    procedure btnIncAllObjectsClick(Sender: TObject);
    procedure btnExclObjectsClick(Sender: TObject);
    procedure btnExclAllObjectsClick(Sender: TObject);
    procedure lbDstObjectsClick(Sender: TObject);
    procedure lbSrcObjectsClick(Sender: TObject);
  private
    procedure SetButtons;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
 JvBoxProcs;

{$R *.dfm}

procedure TframeAvailableObjects.btnExclAllObjectsClick(Sender: TObject);
begin
  // BoxMoveAllItems is in JvBoxProcs.
  BoxMoveAllItems(lbDstObjects, lbSrcObjects);
  SetButtons;
end;

procedure TframeAvailableObjects.btnExclObjectsClick(Sender: TObject);
begin
  // BoxMoveSelectedItems is in JvBoxProcs.
  BoxMoveSelectedItems(lbDstObjects, lbSrcObjects);
  SetButtons;
end;

procedure TframeAvailableObjects.btnIncAllObjectsClick(Sender: TObject);
begin
  // BoxMoveAllItems is in JvBoxProcs.
  BoxMoveAllItems(lbSrcObjects, lbDstObjects);
  SetButtons;
end;

procedure TframeAvailableObjects.btnIncObjectsClick(Sender: TObject);
begin
  // BoxMoveSelectedItems is in JvBoxProcs.
  BoxMoveSelectedItems(lbSrcObjects, lbDstObjects);
  SetButtons;
end;

procedure TframeAvailableObjects.FrameResize(Sender: TObject);
const
  Margin = 8;
begin
  lbSrcObjects.Left := Margin;
  lblSrcObjects.Left := lbSrcObjects.Left;

  btnIncObjects.Left := (Width - btnIncObjects.Width) div 2;
  btnIncAllObjects.Left := btnIncObjects.Left;
  btnExclObjects.Left := btnIncObjects.Left;
  btnExclAllObjects.Left := btnIncObjects.Left;

  lbSrcObjects.Width := (Width - btnIncObjects.Width - Margin*4) div 2;
  lbDstObjects.Width := lbSrcObjects.Width;

  lbDstObjects.Left := btnIncObjects.Left + btnIncObjects.Width + Margin;
  lblDstObjects.Left := lbDstObjects.Left;

  lbSrcObjects.Height := Height - lbSrcObjects.Top - Margin;
  lbDstObjects.Height := lbSrcObjects.Height;
end;

procedure TframeAvailableObjects.lbDstObjectsClick(Sender: TObject);
begin
  SetButtons;
end;

procedure TframeAvailableObjects.lbSrcObjectsClick(Sender: TObject);
begin
  SetButtons;
end;

procedure TframeAvailableObjects.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := (lbSrcObjects.Items.Count = 0);
  DstEmpty := (lbDstObjects.Items.Count = 0);
  btnIncObjects.Enabled := not SrcEmpty and (lbSrcObjects.SelCount > 0);
  btnIncAllObjects.Enabled := not SrcEmpty;
  btnExclObjects.Enabled := not DstEmpty and (lbDstObjects.SelCount > 0);
  btnExclAllObjects.Enabled := not DstEmpty;
end;

end.
