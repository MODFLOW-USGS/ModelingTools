unit frmScaleRotateMoveUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, JvExStdCtrls, JvGroupBox,
  ArgusDataEntry, Buttons, UndoItemsScreenObjects, GoPhastTypes, JvCombobox,
  JvListComb;

type
  TfrmScaleRotateMove = class(TfrmCustomGoPhast)
    gbScale: TJvGroupBox;
    gbRotate: TJvGroupBox;
    gbMove: TJvGroupBox;
    rdeXScale: TRbwDataEntry;
    lblXScale: TLabel;
    cbLockAspectRatio: TCheckBox;
    lblYScale: TLabel;
    rdeYScale: TRbwDataEntry;
    lblAngle: TLabel;
    rdeAngle: TRbwDataEntry;
    lblMoveX: TLabel;
    rdeMoveX: TRbwDataEntry;
    lblMoveY: TLabel;
    rdeMoveY: TRbwDataEntry;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    gbCenter: TGroupBox;
    comboCenterOfRotation: TJvImageComboBox;
    lblXCenter: TLabel;
    rdeXCenter: TRbwDataEntry;
    lblYCenter: TLabel;
    rdeYCenter: TRbwDataEntry;
    procedure rdeXScaleChange(Sender: TObject);
    procedure cbLockAspectRatioClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure EnableOk(Sender: TObject);
    procedure comboCenterOfRotationChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rdeAngleChange(Sender: TObject);
    procedure rdeMoveXChange(Sender: TObject);
    procedure rdeMoveYChange(Sender: TObject);
  private
//    FScaleOnClick: TNotifyEvent;
//    FRotateOnClick: TNotifyEvent;
//    FMoveOnClick: TNotifyEvent;
    FViewDirection: TViewDirection;
    procedure SetViewDirection(const Value: TViewDirection);
    property ViewDirection: TViewDirection read FViewDirection write SetViewDirection;
    procedure EnableRotationCenterControls;
    procedure EnableOkButton;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoScaleRotateMove = class(TCustomUpdateScreenObjectUndo)
  private
    FOldPoints: T2DRealPointArray;
    FNewPoints: T2DRealPointArray;
    FDescription: string;
    procedure SetDescription(Angle, YScale, XScale, YOffset, XOffset: Double);
    procedure NotifyGui;
  public
    Constructor Create(XScale, YScale, Angle, XCenter, YCenter, XOffset, YOffset: double);
    // Description tells what @classname does.
    function Description: string; override;
    // @name applies the transformations to the selected @link(TScreenObject)s.
    procedure DoCommand; override;
    // @name applies the transformations
    // to the selected @link(TScreenObject)s again.
    procedure Redo; override;
    // @name restores the selected @link(TScreenObject)s
    // to their original positions.
    procedure Undo; override;
    function ShouldUse: boolean;
  end;

implementation

uses
  Math, ScreenObjectUnit, frmGoPhastUnit;

resourcestring
  StrZScaleFactor = 'Z scale factor';
  StrZCenter = 'Z center';
  StrZOffset = 'Z offset';
  StrYScaleFactor = 'Y scale factor';
  StrYCenter = 'Y center';
  StrYOffset = 'Y offset';
  StrScale = 'scale';
  StrRotate = 'rotate';
  StrMove = 'move';
  StrObjects = ' objects';
  FormatChange1 = '%0:s%1:s';
  FormatChange2 = '%0:s and %1:s%2:s';
  Change3 = 'scale, rotate, and move objects';

{$R *.dfm}

procedure TfrmScaleRotateMove.btnOKClick(Sender: TObject);
var
  MaxX, MinX, MaxY, MinY: double;
  Index: Integer;
  ScreenObject: TScreenObject;
  FoundFirst: boolean;
  XCenter, YCenter: double;
  XScale, YScale: double;
  Angle: double;
  XOffSet, YOffset: double;
  Undo: TUndoScaleRotateMove;
  temp: Double;
begin
  inherited;
  if gbScale.Checked then
  begin
    XScale := StrToFloat(rdeXScale.Text);
    YScale := StrToFloat(rdeYScale.Text);
  end
  else
  begin
    XScale := 1;
    YScale := 1;
  end;

  if gbRotate.Checked  then
  begin
    Angle := StrToFloat(rdeAngle.Text);
  end
  else
  begin
    Angle := 0;
  end;

  if (XScale <> 1) or (YScale <> 1) or (Angle <> 0) then
  begin
    if (comboCenterOfRotation.ItemIndex <> 0) then
    begin
      XCenter := StrToFloat(rdeXCenter.Text);
      YCenter := StrToFloat(rdeYCenter.Text);
    end
    else
    begin
      MaxX := 0;
      MinX := 0;
      MaxY := 0;
      MinY := 0;
      FoundFirst := False;
      for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if ScreenObject.Selected then
        begin
          if FoundFirst then
          begin
            if MaxX < ScreenObject.MaxX then
            begin
              MaxX := ScreenObject.MaxX;
            end;
            if MinX > ScreenObject.MinX then
            begin
              MinX := ScreenObject.MinX;
            end;
            if MaxY < ScreenObject.MaxY then
            begin
              MaxY := ScreenObject.MaxY;
            end;
            if MinY > ScreenObject.MinY then
            begin
              MinY := ScreenObject.MinY;
            end;
          end
          else
          begin
            MaxX := ScreenObject.MaxX;
            MinX := ScreenObject.MinX;
            MaxY := ScreenObject.MaxY;
            MinY := ScreenObject.MinY;
          end;
        end;
      end;
      XCenter := (MaxX + MinX)/2;
      YCenter := (MaxY + MinY)/2;
    end;
  end
  else
  begin
    XCenter := 0;
    YCenter := 0;
  end;

  if gbMove.Checked then
  begin
    XOffSet := StrToFloat(rdeMoveX.Text);
    YOffSet := StrToFloat(rdeMoveY.Text);
  end
  else
  begin
    XOffSet := 0;
    YOffSet := 0;
  end;

  if ViewDirection = vdSide then
  begin
    temp := XScale;
    XScale := YScale;
    YScale := temp;

    temp := XOffSet;
    XOffSet := YOffSet;
    YOffSet := temp;

    Angle := -Angle;
  end;

  Undo := TUndoScaleRotateMove.Create(XScale, YScale, Angle,
    XCenter, YCenter, XOffset, YOffset);
  if Undo.ShouldUse then
  begin
    frmGoPhast.UndoStack.Submit(Undo);
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TfrmScaleRotateMove.cbLockAspectRatioClick(Sender: TObject);
begin
  inherited;
  if cbLockAspectRatio.Checked then
  begin
    rdeXScaleChange(rdeXScale);
  end;
end;

procedure TfrmScaleRotateMove.comboCenterOfRotationChange(Sender: TObject);
begin
  inherited;
  EnableRotationCenterControls;
end;

procedure TfrmScaleRotateMove.EnableOk(Sender: TObject);
begin
  EnableOkButton;
//  if Assigned(FScaleOnClick) then
//  begin
//    FScaleOnClick(Sender);
//  end;
//  if Assigned(FRotateOnClick) then
//  begin
//    FRotateOnClick(Sender);
//  end;
//  if Assigned(FMoveOnClick) then
//  begin
//    FMoveOnClick(Sender);
//  end;
  EnableRotationCenterControls;
end;

procedure TfrmScaleRotateMove.EnableRotationCenterControls;
var
  ShouldEnable: Boolean;
begin
  if ([csLoading,csReading] * ComponentState) <> [] then
  begin
    Exit;
  end;
  ShouldEnable := gbScale.Checked or gbRotate.Checked;
  comboCenterOfRotation.Enabled := ShouldEnable;
  ShouldEnable := ShouldEnable and (comboCenterOfRotation.ItemIndex > 0);
  lblXCenter.Enabled := ShouldEnable;
  rdeXCenter.Enabled := ShouldEnable;
  lblYCenter.Enabled := ShouldEnable;
  rdeYCenter.Enabled := ShouldEnable;
end;

procedure TfrmScaleRotateMove.EnableOkButton;
var
  ShouldEnable: Boolean;
  Value: Extended;
begin
  if ([csLoading,csReading] * ComponentState) <> [] then
  begin
    Exit;
  end;
  ShouldEnable := False;
  if gbScale.Checked then
  begin
    if TryStrToFloat(rdeXScale.Text, Value) then
    begin
      if Value <> 1 then
      begin
        ShouldEnable := True;
      end;
    end;
    if TryStrToFloat(rdeYScale.Text, Value) then
    begin
      if Value <> 1 then
      begin
        ShouldEnable := True;
      end;
    end;
  end;
  if gbRotate.Checked then
  begin
    if TryStrToFloat(rdeAngle.Text, Value) then
    begin
      if Value <> 0 then
      begin
        ShouldEnable := True;
      end;
    end;
  end;
  if gbMove.Checked then
  begin
    if TryStrToFloat(rdeMoveX.Text, Value) then
    begin
      if Value <> 0 then
      begin
        ShouldEnable := True;
      end;
    end;
    if TryStrToFloat(rdeMoveY.Text, Value) then
    begin
      if Value <> 0 then
      begin
        ShouldEnable := True;
      end;
    end;
  end;
  btnOK.Enabled := ShouldEnable;
end;

procedure TfrmScaleRotateMove.FormCreate(Sender: TObject);
//  procedure EnableCheckResponse(GroupBox: TJvGroupBox;
//    var OnClick: TNotifyEvent);
//  var
//    CheckBox: TCheckBox;
//  begin
//    Assert(GroupBox.ComponentCount = 1);
//    CheckBox := GroupBox.Components[0] as TCheckBox;
//    OnClick := CheckBox.OnClick;
//    CheckBox.OnClick := EnableOk;
//  end;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  inherited;
  gbScale.Checked := False;
  gbRotate.Checked := False;
  gbMove.Checked := False;
//  EnableCheckResponse(gbScale, FScaleOnClick);
//  EnableCheckResponse(gbRotate, FRotateOnClick);
//  EnableCheckResponse(gbMove, FMoveOnClick);

  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Selected then
    begin
      ViewDirection := ScreenObject.ViewDirection;
    end;
  end;
end;

procedure TfrmScaleRotateMove.rdeAngleChange(Sender: TObject);
begin
  inherited;
  EnableOkButton;
end;

procedure TfrmScaleRotateMove.rdeMoveXChange(Sender: TObject);
begin
  inherited;
  EnableOkButton;
end;

procedure TfrmScaleRotateMove.rdeMoveYChange(Sender: TObject);
begin
  inherited;
  EnableOkButton;
end;

procedure TfrmScaleRotateMove.rdeXScaleChange(Sender: TObject);
var
  Edit: TRbwDataEntry;
  OtherEdit: TRbwDataEntry;
begin
  inherited;
  if cbLockAspectRatio <> nil then
  begin
    if cbLockAspectRatio.Checked then
    begin
      if Sender = rdeXScale then
      begin
        Edit := rdeXScale;
        OtherEdit := rdeYScale;
      end
      else
      begin
        Assert(Sender = rdeYScale);
        Edit := rdeYScale;
        OtherEdit := rdeXScale;
      end;
      OtherEdit.Text := Edit.Text;
    end;
  end;
  EnableOkButton;
end;

procedure TfrmScaleRotateMove.SetViewDirection(const Value: TViewDirection);
begin
  FViewDirection := Value;
  case FViewDirection of
    vdTop:
      begin
        // do nothing
      end;
    vdFront:
      begin
        lblYScale.Caption := StrZScaleFactor;
        lblYCenter.Caption := StrZCenter;
        lblMoveY.Caption := StrZOffset;
      end;
    vdSide:
      begin
        lblXScale.Caption := StrYScaleFactor;
        lblXCenter.Caption := StrYCenter;
        lblMoveX.Caption := StrYOffset;
        lblYScale.Caption := StrZScaleFactor;
        lblYCenter.Caption := StrZCenter;
        lblMoveY.Caption := StrZOffset;
      end;
    else Assert(False);
  end;
end;

{ TUndoScaleRotateMove }

constructor TUndoScaleRotateMove.Create(XScale, YScale, Angle,
  XCenter, YCenter, XOffset, YOffset: double);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  PointIndex: Integer;
  DeltaX, DeltaY: double;
  OffSetLength: double;
  PointAngle: double;
begin
  inherited Create;
  SetDescription(Angle, YScale, XScale, YOffset, XOffset);
  SetPostSelection;
  SetLength(FOldPoints, FNewSelectedScreenObjects.Count);
  SetLength(FNewPoints, FNewSelectedScreenObjects.Count);
  if Angle <> 0 then
  begin
    Angle := Angle/180*Pi;
  end;
  for Index := 0 to FNewSelectedScreenObjects.Count - 1 do
  begin
    ScreenObject := FNewSelectedScreenObjects[Index];
    SetLength(FOldPoints[Index], ScreenObject.Count);
    SetLength(FNewPoints[Index], ScreenObject.Count);
    ScreenObject.MovePoints(FOldPoints[Index]);
    ScreenObject.MovePoints(FNewPoints[Index]);
    for PointIndex := 0 to Length(FNewPoints[Index]) - 1 do
    begin
      if XScale <> 1 then
      begin
        DeltaX := FNewPoints[Index,PointIndex].X - XCenter;
        FNewPoints[Index,PointIndex].X :=
          XCenter + DeltaX*XScale
      end;
      if YScale <> 1 then
      begin
        DeltaY := FNewPoints[Index,PointIndex].Y - YCenter;
        FNewPoints[Index,PointIndex].Y :=
          YCenter + DeltaY*YScale
      end;
      if Angle <> 0 then
      begin
        DeltaX := FNewPoints[Index,PointIndex].X - XCenter;
        DeltaY := FNewPoints[Index,PointIndex].Y - YCenter;
        PointAngle := ArcTan2(DeltaY, DeltaX);
        OffSetLength := Sqrt(Sqr(DeltaX)+Sqr(DeltaY));
        DeltaX := Cos(PointAngle+Angle)*OffSetLength;
        DeltaY := Sin(PointAngle+Angle)*OffSetLength;
        FNewPoints[Index,PointIndex].X :=
          XCenter + DeltaX;
        FNewPoints[Index,PointIndex].Y :=
          YCenter + DeltaY;
      end;
      FNewPoints[Index,PointIndex].X :=
        FNewPoints[Index,PointIndex].X + XOffset;
      FNewPoints[Index,PointIndex].Y :=
        FNewPoints[Index,PointIndex].Y + YOffset;
    end;
  end;
end;

function TUndoScaleRotateMove.Description: string;
begin
  result := FDescription;
end;

procedure TUndoScaleRotateMove.DoCommand;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to FNewSelectedScreenObjects.Count - 1 do
  begin
    ScreenObject := FNewSelectedScreenObjects[Index];
    ScreenObject.MoveToPoints(FNewPoints[Index]);
    ScreenObject.Invalidate;
  end;
  NotifyGui;
  inherited;
end;

procedure TUndoScaleRotateMove.NotifyGui;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to FNewSelectedScreenObjects.Count - 1 do
  begin
    ScreenObject := FNewSelectedScreenObjects[Index];
    UpdateScreenObject(ScreenObject);
  end;
  UpdateSelectionRectangle;
end;

procedure TUndoScaleRotateMove.Redo;
begin
  DoCommand;
end;

procedure TUndoScaleRotateMove.Undo;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to FNewSelectedScreenObjects.Count - 1 do
  begin
    ScreenObject := FNewSelectedScreenObjects[Index];
    ScreenObject.MoveToPoints(FOldPoints[Index]);
    ScreenObject.Invalidate;
  end;
  NotifyGui;
  inherited;
end;

procedure TUndoScaleRotateMove.SetDescription(Angle, YScale, XScale, YOffset, XOffset: Double);
var
  Descriptions: TStringList;
begin
  Descriptions := TStringList.Create;
  try
    if (XScale <> 1) or (YScale <> 1) then
    begin
      Descriptions.Add(StrScale);
    end;
    if Angle <> 0 then
    begin
      Descriptions.Add(StrRotate);
    end;
    if (XOffset <> 0) or (YOffset <> 0) then
    begin
      Descriptions.Add(StrMove);
    end;
    Assert(Descriptions.Count > 0);
    case Descriptions.Count of
      1:
        begin
          FDescription := Format(FormatChange1, [Descriptions[0], StrObjects]);
        end;
      2:
        begin
          FDescription := Format(FormatChange2,
            [Descriptions[0], Descriptions[1], StrObjects]);
        end;
      3:
        begin
          FDescription := Change3;
        end;
    else
      Assert(False);
    end;
  finally
    Descriptions.Free;
  end;
end;

function TUndoScaleRotateMove.ShouldUse: boolean;
begin
  result := FNewSelectedScreenObjects.Count > 0;
end;

end.
