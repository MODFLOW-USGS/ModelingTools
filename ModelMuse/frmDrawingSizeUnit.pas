unit frmDrawingSizeUnit;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, clxDataEntry, FrameDrawingSizeUnit, frmCustomGoPhastUnit,
  QButtons;

type
  TfrmDrawingSize = class(TfrmCustomGoPhast)
    frameTop: TframeDrawingSize;
    frameSide: TframeDrawingSize;
    frameFront: TframeDrawingSize;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure frameTopadeMax1Change(Sender: TObject);
    procedure frameTopadeMax2Change(Sender: TObject);
    procedure frameFrontadeMax1Change(Sender: TObject);
    procedure frameFrontadeMax2Change(Sender: TObject);
    procedure frameSideadeMax1Change(Sender: TObject);
    procedure frameSideadeMax2Change(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    function IsValid: boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDrawingSize: TfrmDrawingSize;

implementation

uses frmGoPhastUnit, UndoItems;

{$R *.dfm}

{ TfrmDrawingSize }

function TfrmDrawingSize.IsValid: boolean;
begin
  result := frameTop.IsValid and frameFront.IsValid and frameSide.IsValid;
end;

procedure TfrmDrawingSize.GetData;
begin
  with frmGoPhast do
  begin
    with frameTop do
    begin
      if frameTopView = nil then
      begin
        adeMin1.Enabled := False;
        adeMax1.Enabled := False;
        adeMin2.Enabled := False;
        adeMax2.Enabled := False;
      end
      else
      begin
        with frameTopView.ZoomBox do
        begin
{
          adeMin1.Text := FloatToStr(MinX);
          adeMax1.Text := FloatToStr(MaxX);
          adeMin2.Text := FloatToStr(MinY);
          adeMax2.Text := FloatToStr(MaxY);
}
        end;
      end;
    end;
    with frameFront do
    begin
      if frameFrontView = nil then
      begin
        adeMin1.Enabled := False;
        adeMax1.Enabled := False;
        adeMin2.Enabled := False;
        adeMax2.Enabled := False;
      end
      else
      begin
        with frameFrontView.ZoomBox do
        begin
{
          adeMin1.Text := FloatToStr(MinX);
          adeMax1.Text := FloatToStr(MaxX);
          adeMin2.Text := FloatToStr(MinY);
          adeMax2.Text := FloatToStr(MaxY);
          }
        end;
      end;
    end;
    with frameSide do
    begin
      if frameSideView = nil then
      begin
        adeMin1.Enabled := False;
        adeMax1.Enabled := False;
        adeMin2.Enabled := False;
        adeMax2.Enabled := False;
      end
      else
      begin
        with frameSideView.ZoomBox do
        begin
{         adeMin1.Text := FloatToStr(MinY);
          adeMax1.Text := FloatToStr(MaxY);
          adeMin2.Text := FloatToStr(MinX);
          adeMax2.Text := FloatToStr(MaxX);
          }
        end;
      end;
    end;
  end;
end;

procedure TfrmDrawingSize.SetData;
var
  NewDrawingSize: TDrawingSize;
begin
  with frmGoPhast do
  begin
    with frameTop do
    begin
      if frameTopView <> nil then
      begin
        with NewDrawingSize do
        begin
          TopMinX := StrToFloat(adeMin1.Text);
          TopMaxX := StrToFloat(adeMax1.Text);
          TopMinY := StrToFloat(adeMin2.Text);
          TopMaxY := StrToFloat(adeMax2.Text);
        end;
      end;
    end;
    with frameFront do
    begin
      if frameFrontView <> nil then
      begin
        with NewDrawingSize do
        begin
          FrontMinX := StrToFloat(adeMin1.Text);
          FrontMaxX := StrToFloat(adeMax1.Text);
          FrontMinY := StrToFloat(adeMin2.Text);
          FrontMaxY := StrToFloat(adeMax2.Text);
        end;
      end;
    end;
    with frameSide do
    begin
      if frameSideView <> nil then
      begin
        with NewDrawingSize do
        begin
          SideMinX := StrToFloat(adeMin2.Text);
          SideMaxX := StrToFloat(adeMax2.Text);
          SideMinY := StrToFloat(adeMin1.Text);
          SideMaxY := StrToFloat(adeMax1.Text);
        end;
      end;
    end;
  end;
  frmGoPhast.UndoStack.Submit(TUndoDrawingSize.Create(NewDrawingSize));
end;

procedure TfrmDrawingSize.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmDrawingSize.FormShow(Sender: TObject);
begin
  inherited;
  frameTop.adeMax1.SetFocus;
end;

procedure TfrmDrawingSize.FormCreate(Sender: TObject);
begin
 inherited;
GetData;
end;

procedure TfrmDrawingSize.frameTopadeMax1Change(Sender: TObject);
begin
  inherited;
  frameTop.adeMax1Change(Sender);
  btnOK.Enabled := IsValid;
end;

procedure TfrmDrawingSize.frameTopadeMax2Change(Sender: TObject);
begin
  inherited;
  frameTop.adeMax2Change(Sender);
  btnOK.Enabled := IsValid;
end;

procedure TfrmDrawingSize.frameFrontadeMax1Change(Sender: TObject);
begin
  inherited;
  frameFront.adeMax1Change(Sender);
  btnOK.Enabled := IsValid;
end;

procedure TfrmDrawingSize.frameFrontadeMax2Change(Sender: TObject);
begin
  inherited;
  frameFront.adeMax2Change(Sender);
  btnOK.Enabled := IsValid;
end;

procedure TfrmDrawingSize.frameSideadeMax1Change(Sender: TObject);
begin
  inherited;
  frameSide.adeMax1Change(Sender);
  btnOK.Enabled := IsValid;
end;

procedure TfrmDrawingSize.frameSideadeMax2Change(Sender: TObject);
begin
  inherited;
  frameSide.adeMax2Change(Sender);
  btnOK.Enabled := IsValid;
end;

end.
