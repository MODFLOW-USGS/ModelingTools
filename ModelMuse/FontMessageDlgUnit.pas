unit FontMessageDlgUnit;

interface

uses Classes, Qt, QConsts, QDialogs, QGraphics, QForms, QControls;

function MessageDlg(const Caption: WideString; const Msg: WideString;
  DlgType: TMsgDlgType; Button1, Button2, Button3: TMsgDlgBtn; HelpCtx: Longint;
  X, Y: Integer;  const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone;
  Bitmap: TBitmap = nil): Integer; overload;

function MessageDlg(const Caption: WideString; const Msg: WideString;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X, Y: Integer; const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone; Bitmap: TBitmap = nil): Integer; overload;

function MessageDlg(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone;
  Bitmap: TBitmap = nil): Integer; overload;

function MessageDlg(const Caption: WideString; const Msg: WideString;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint;
  const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone; Bitmap: TBitmap = nil): Integer; overload;

function MessageDlgPos(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone; Bitmap: TBitmap = nil): Integer; overload;

implementation

resourcestring
  SMsgDlgWarning = 'Warning';
  SMsgDlgError = 'Error';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm = 'Confirm';
  SMsgDlgYes = '&Yes';
  SMsgDlgNo = '&No';
  SMsgDlgOK = 'OK';
  SMsgDlgCancel = 'Cancel';
  SMsgDlgHelp = '&Help';
  SMsgDlgHelpNone = 'No help available';
  SMsgDlgHelpHelp = 'Help';
  SMsgDlgAbort = '&Abort';
  SMsgDlgRetry = '&Retry';
  SMsgDlgIgnore = '&Ignore';
  SMsgDlgAll = '&All';
  SMsgDlgNoToAll = 'N&o to All';
  SMsgDlgYesToAll = 'Yes to &All';

type
  TOpenApplication = class(TApplication);


function MessageDlg(const Caption: WideString; const Msg: WideString;
  DlgType: TMsgDlgType; Button1, Button2, Button3: TMsgDlgBtn; HelpCtx: Longint;
  X, Y: Integer;  const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone;
  Bitmap: TBitmap = nil): Integer; overload;
const
  ConfirmResName = 'MSGDLG_CONFIRM';
  MessageBox_DefaultMask = $100;
  MessageBox_EscapeMask = $200;
var
  MB: QMessageBoxH;
  I: Integer;
  Btns: array[0..2] of Integer;
  CaptureControl: TControl;
  Title: WideString;
  FreeBitmap: Boolean;
  DlgParent: QWidgetH;
  DlgLabel: QLabelH;
{$IFDEF LINUX}
  SigQuitStatus,
  SigIntStatus: TSignalState;
{$ENDIF}
  ButtonText: WideString;
//  palette: QPaletteH;
//  QC: QColorH;

begin
  Result := -1;
  if Application.Terminated then
    Exit;

  FreeBitmap := (Bitmap = nil) and (DlgType = mtConfirmation);
  case DlgType of
    mtCustom: Title := Caption;
    mtInformation: Title := SMsgDlgInformation;
    mtError: Title := SMsgDlgError;
    mtWarning: Title := SMsgDlgWarning;
    mtConfirmation:
      begin
        Title := SMsgDlgConfirm;
        if Bitmap = nil then
        begin
          Bitmap := TBitmap.Create;
          Bitmap.LoadFromResourceName(hInstance, ConfirmResName);
        end;
      end;
  end;

  if DefaultBtn = mbNone then DefaultBtn := Button1;

  I := 0;
  FillChar(Btns, SizeOf(Btns), Ord(mbNone));
  if Button1 <> mbNone then
  begin
    Btns[I] := Ord(Button1);
    if DefaultBtn = Button1 then
      Btns[I] := Btns[I] or MessageBox_DefaultMask;
    Inc(I);
  end;
  if Button2 <> mbNone then
  begin
    Btns[I] := Ord(Button2);
    if DefaultBtn = Button2 then
      Btns[I] := Btns[I] or MessageBox_DefaultMask;
    Inc(I);
  end;
  if Button3 <> mbNone then
  begin
    Btns[I] := Ord(Button3);
    if DefaultBtn = Button3 then
      Btns[I] := Btns[I] or MessageBox_DefaultMask;
  end;

  if DlgType = mtConfirmation then
  begin
    DlgType := mtCustom;
    Bitmap.Transparent := True;
  end;

  DlgParent := QApplication_activeWindow(Application.Handle);
  if DlgParent = nil then
    if (Screen.ActiveCustomForm <> nil) and Screen.ActiveCustomForm.HandleAllocated then
      DlgParent := Screen.ActiveCustomForm.Handle
    else
      DlgParent := TOpenApplication(Application).AppWidget;
  MB := QMessageBox_create(PWideString(@Title), PWideString(@Msg),
    QMessageBoxIcon(DlgType), Btns[0], Btns[1], Btns[2], DlgParent, nil, True, WFlags(WidgetFlags_WStyle_Dialog));
  try
    QWidget_setFont(MB, Font.Handle);
    {palette := QWidget_palette(MB);
    QC := QColor(Color);
    try
      QWidget_setBackgroundColor(MB, QC);
      QPalette_setColor(palette, QColorGroupColorRole_NColorRoles,  QC);
    finally
      QColor_destroy(QC);
    end;   }
    ButtonText := SOKButton;
    QMessageBox_setButtonText(MB, Integer(mbOk), @ButtonText);
    ButtonText := SCancelButton;
    QMessageBox_setButtonText(MB, Integer(mbCancel), @ButtonText);
    ButtonText := SYesButton;
    QMessageBox_setButtonText(MB, Integer(mbYes), @ButtonText);
    ButtonText := SNoButton;
    QMessageBox_setButtonText(MB, Integer(mbNo), @ButtonText);
    ButtonText := SAbortButton;
    QMessageBox_setButtonText(MB, Integer(mbAbort), @ButtonText);
    ButtonText := SRetryButton;
    QMessageBox_setButtonText(MB, Integer(mbRetry), @ButtonText);
    ButtonText := SIgnoreButton;
    QMessageBox_setButtonText(MB, Integer(mbIgnore), @ButtonText);

    if Bitmap <> nil then
      QMessageBox_setIconPixmap(MB, Bitmap.Handle);
    if (X >= 0) and (Y >= 0) then QDialog_move(MB, X, Y);

    //force wordbreak alignment of dialog label.
    DlgLabel := QLabelH(QObject_child(MB, 'text', nil));  //do not localize
    if DlgLabel <> nil then
      QLabel_setAlignment(DlgLabel, QLabel_alignment(DlgLabel)
        or Integer(AlignmentFlags_WordBreak));

    CaptureControl := GetCaptureControl;
    try
      SetCaptureControl(nil);
      QForms.Application.ModalStarted(nil);
      try
{$IFDEF LINUX}
        SigIntStatus := InquireSignal(RTL_SIGINT);
        SigQuitStatus := InquireSignal(RTL_SIGQUIT);
        if SigIntStatus = ssHooked then
          UnhookSignal(RTL_SIGINT);
        if SigQuitStatus = ssHooked then
          UnhookSignal(RTL_SIGQUIT);
        try
{$ENDIF}
          try
            Result := QDialog_exec(MB);
            if Result = mrNone then
              Result := mrCancel;
          except
            Application.HandleException(DlgParent);
          end;
{$IFDEF LINUX}
        finally
          if SigIntStatus = ssHooked then
            HookSignal(RTL_SIGINT);
          if SigQuitStatus = ssHooked then
            HookSignal(RTL_SIGQUIT);
        end;
{$ENDIF}
      finally
        QForms.Application.ModalFinished(nil);
      end;
    finally
      SetCaptureControl(CaptureControl);
      QWidget_setActiveWindow(DlgParent);
    end;
  finally
    QMessageBox_destroy(MB);
    if FreeBitmap then Bitmap.Free;
  end;
{$IFDEF LINUX}
  Application.ProcessMessages;
  QApplication_sendPostedEvents;
  QApplication_processEvents(Application.Handle);
{$ENDIF}
end;

function MessageDlg(const Caption: WideString; const Msg: WideString;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X, Y: Integer; const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone; Bitmap: TBitmap = nil): Integer; overload;
var
  Btns: array[0..2] of TMsgDlgBtn;
  B: TMsgDlgBtn;
  I: Integer;
begin
  { Button order is hard-coded in Windows. We follow their conventions here. }
  if Buttons = mbYesNoCancel then
  begin
    Btns[0] := mbYes;
    Btns[1] := mbNo;
    Btns[2] := mbCancel;
  end
  else
  if Buttons = mbYesNo then
  begin
    Btns[0] := mbYes;
    Btns[1] := mbNo;
    Btns[2] := mbNone;
  end
  else
  if Buttons = mbOkCancel then
  begin
    Btns[0] := mbOk;
    Btns[1] := mbCancel;
    Btns[2] := mbNone;
  end
  else
  if Buttons = mbAbortRetryIgnore then
  begin
    Btns[0] := mbAbort;
    Btns[1] := mbRetry;
    Btns[2] := mbIgnore;
  end
  else
  begin
    I := 0;
    FillChar(Btns, SizeOf(Btns), Ord(mbNone));

    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if (B in Buttons) and (B <> mbNone) then
      begin
        if I > High(Btns) then
          raise EInvalidOperation.CreateRes(@STooManyMessageBoxButtons);

        Btns[I] := B;
        Inc(I);
      end;
  end;

  Result := MessageDlg(Caption, Msg, DlgType, Btns[0], Btns[1], Btns[2],
    HelpCtx, X, Y, Font, DefaultBtn, Bitmap);
end;

function MessageDlg(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone;
  Bitmap: TBitmap = nil): Integer; overload;
begin
  Result := MessageDlg(Application.Title, Msg, DlgType, Buttons, HelpCtx, -1,
    -1, Font, DefaultBtn, Bitmap);
end;

function MessageDlg(const Caption: WideString; const Msg: WideString;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint;
  const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone; Bitmap: TBitmap = nil): Integer; overload;
begin
  Result := MessageDlg(Caption, Msg, DlgType, Buttons, HelpCtx, -1, -1,
    Font, DefaultBtn, Bitmap);
end;

function MessageDlgPos(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const Font: TFont; DefaultBtn: TMsgDlgBtn = mbNone; Bitmap: TBitmap = nil): Integer; overload;
begin
  Result := MessageDlg(Application.Title, Msg, DlgType, Buttons, HelpCtx,
    X, Y, Font, DefaultBtn, Bitmap);
end;


end.
 
