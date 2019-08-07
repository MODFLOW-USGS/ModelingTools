unit frameCustomMetaDataUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, MetaDataInterfacesUnit, FMX.Objects, FMX.ScrollBox,
  FMX.Memo, FMX.Edit;

type
  TframeCustomMetaData = class(TFrame)
    memoDescription: TMemo;
    splResize: TSplitter;
    pnlBottom: TPanel;
    Edit1: TEdit;
    procedure memoDescriptionApplyStyleLookup(Sender: TObject);
    procedure memoDescriptionResize(Sender: TObject);
  private
    FData: ICustomMetaData;
    FMemoColor: TAlphaColor;
    procedure SetMemoColor(const Value: TAlphaColor);
    { Private declarations }
  public
    procedure GetMetaData(Data: ICustomMetaData); virtual;
    constructor Create(AOwner: TComponent); override;
    property MemoColor: TAlphaColor read FMemoColor write SetMemoColor;
    { Public declarations }
  end;

  TMetaDataFrameClass = class of TframeCustomMetaData;

implementation

uses
  frmMetaDataUnit, frameMetaDataEditorUnit;



{$R *.fmx}

{ TframeCustomMetaData }

constructor TframeCustomMetaData.Create(AOwner: TComponent);
var
  ControlIndex: Integer;
  AControl: TControl;
begin
  inherited;
  if Frame <> nil then
  begin
    Self.DragMode := Frame.DragMode;
    Self.OnDragOver := Frame.OnDragOver;
    Self.OnDragDrop := Frame.OnDragDrop;

    for ControlIndex := 0 to Controls.Count - 1 do
    begin
      AControl := Controls[ControlIndex];
      if (AControl <> splResize) and (AControl <> memoDescription) then
      begin
        AControl.DragMode := Frame.DragMode;
        AControl.OnDragOver := Frame.OnDragOver;
        AControl.OnDragDrop := Frame.OnDragDrop;
      end;
    end;

  end;
end;

procedure TframeCustomMetaData.GetMetaData(Data: ICustomMetaData);
//var
//  LocalUsed: Boolean;
begin
  FData := Data;
  memoDescription.Height := FData.TextHeight;
  memoDescription.Text := FData.ShortName + ': ' + FData.Description;
  if FData.Used then
  begin
    Edit1.Text := 'Used';
  end
  else
  begin
    Edit1.Text := 'not Used';
  end;
//  LocalUsed := FData.Used;
//  FData.Used := not LocalUsed;
//  FData.Used := LocalUsed;
end;

procedure TframeCustomMetaData.memoDescriptionApplyStyleLookup(Sender: TObject);
var
  Obj: TFmxObject;
  Rectangle1: TRectangle;
  Margins: TBounds;
begin
  Obj := memoDescription.FindStyleResource('background');
  if Obj <> nil then
  begin
    Margins := TBounds.Create(TRectF.Create(-2, -2, -2, -2));
    try
      TControl(Obj).Margins := Margins;
    finally
      Margins.Free;
    end;
    Rectangle1              := TRectangle.Create(Obj);
    Obj.AddObject(Rectangle1);
    Rectangle1.Align        := TAlignLayout.Client;
    Rectangle1.Fill.Color   := FMemoColor; // $FFF0F0F0;
    Rectangle1.Stroke.Color := TAlphaColorRec.Null;
    Rectangle1.HitTest      := False;
    Rectangle1.SendToBack;
//          Rectangle1.Free;
     end;
end;

procedure TframeCustomMetaData.memoDescriptionResize(Sender: TObject);
begin
  if FData <> nil then
  begin
    FData.TextHeight := memoDescription.Height;
  end;
end;

procedure TframeCustomMetaData.SetMemoColor(const Value: TAlphaColor);
begin
  FMemoColor := Value;
end;

end.
