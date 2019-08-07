{
By Richard B. Winston (rbwinst@usgs.gov)
This unit is in the public domain.
http://water.usgs.gov/software/software_notice.html
}


unit QCreateMessageDlgUnit;

interface

uses QForms, QDialogs;

function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TForm;

implementation

uses SysUtils, Classes, QConsts, QGraphics, QControls, QStdCtrls, QExtCtrls;

//  DialogBoxIcons.dcr contains the icons used on the dialog box.
{$R DialogBoxIcons.dcr}

type
  TMessageForm = Class(TForm)
    Message: TLabel;
    Image: TImage;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;


{ TMessageForm }

constructor TMessageForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  Position := poMainFormCenter;

  Message := TLabel.Create(self);
  Message.Parent := self;
  Message.WordWrap := True;
  Message.Left := 50;
  Message.Top := 13;

  Image:= TImage.Create(self);
  Image.Parent := self;
  Image.Top := 10;
  Image.Left := 10;
  Image.Width := 32;
  Image.Height := 32;
  Image.Transparent := True;

  // Use the Application.Font.
  ParentFont := True;
end;

function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): TForm;
var
  AMessage: TMessageForm ;
  BitMap: TBitMap;
  AButton: TButton;
  Left: integer;
  Captions: TStringList;
  ModalResults : array[0..Ord(High(TMsgDlgBtn))] of integer;
  resultIndex: integer;
  Width, temp, ButtonWidth: integer;
  MaxWidth: integer;
  ButtonTop, ButtonHeight: integer;
begin
  // Create the result form.
  AMessage := TMessageForm.CreateNew(Application);
  result := AMessage;

  // Create a bitmap to hold the icon for the type of message dialog
  // that will be shown.
  BitMap :=  TBitMap.Create;
  try
    // Set the width of the dialog box.
    MaxWidth := Screen.Width * 2 div 3;
    Width := AMessage.Canvas.TextWidth(Msg) + 58;
    if Width > MaxWidth then
    begin
      Width := MaxWidth
    end;
    AMessage.Width := Width;

    // Set the width of the message.
    // The icon is positioned at X = 10 and has a width of 32.
    // There is a border of 8 pixels on either side of the message
    AMessage.Message.Width := AMessage.Width - 58;
    AMessage.Message.Caption := Msg;

    // Assign the caption
    AMessage.Caption := Application.Title;

    // Assign the icon (which are stored in DialogBoxIcons.dcr).
    case DlgType of
      mtCustom:
        begin
          // do nothing.  The user can supply an icon or change the
          // position of the message as desired.
        end;
      mtInformation:
        begin
          BitMap.LoadFromResourceName(hInstance, 'BMINFORMATION');
        end;
      mtWarning:
        begin
          BitMap.LoadFromResourceName(hInstance, 'BMWARNING');
        end;
      mtError:
        begin
          BitMap.LoadFromResourceName(hInstance, 'BMERROR');
        end;
      mtConfirmation:
        begin
          BitMap.LoadFromResourceName(hInstance, 'BMCONFIRM');
        end;
    else Assert(False);
    end;
    AMessage.Image.Picture.Assign(BitMap);

    resultIndex := -1;
    Captions := TStringList.Create;
    try
      // Store the button captions in Captions and the modal result values
      // in ModalResults.
      if mbOk in Buttons then
      begin
        Captions.Add(SOKButton);
        Inc(resultIndex);
        ModalResults[resultIndex] := mrOK;
      end;

      if mbCancel in Buttons then
      begin
        Captions.Add(SCancelButton);
        Inc(resultIndex);
        ModalResults[resultIndex] := mrCancel;
      end;

      if mbYes in Buttons then
      begin
        Captions.Add(SYesButton);
        Inc(resultIndex);
        ModalResults[resultIndex] := mrYes;
      end;

      if mbNo in Buttons then
      begin
        Captions.Add(SNoButton);
        Inc(resultIndex);
        ModalResults[resultIndex] := mrNo;
      end;

      if mbAbort in Buttons then
      begin
        Captions.Add(SAbortButton);
        Inc(resultIndex);
        ModalResults[resultIndex] := mrAbort;
      end;

      if mbRetry in Buttons then
      begin
        Captions.Add(SRetryButton);
        Inc(resultIndex);
        ModalResults[resultIndex] := mrRetry;
      end;

      if mbIgnore in Buttons then
      begin
        Captions.Add(SIgnoreButton);
        Inc(resultIndex);
        ModalResults[resultIndex] := mrIgnore;
      end;

      // determine the appropriate widths and heights of the buttons.
      Width := 0;
      ButtonHeight := 0;
      for resultIndex := 0 to Captions.Count -1 do
      begin
        temp := AMessage.Canvas.TextWidth(Captions[resultIndex]);
        if temp > Width then
        begin
          Width := Temp
        end;
        temp := AMessage.Canvas.TextHeight(Captions[resultIndex]);
        if temp > ButtonHeight then
        begin
          ButtonHeight := Temp
        end;
      end;
      Inc(Width, 8);
      Inc(ButtonHeight, 8);

      AButton := TButton.Create(AMessage);
      try
        AButton.Parent := AMessage;
        ButtonWidth := AButton.Width;
        if AButton.Width < Width then
        begin
          ButtonWidth := Width;
        end;
        if AButton.Height > ButtonHeight then
        begin
          ButtonHeight := AButton.Height;
        end;
      finally
        AButton.Free;
      end;

      // With a short message but many buttons, the dialog box might be
      // too wide and will need to be enlarged
      Left := (AMessage.Width - (ButtonWidth+8)*Captions.Count) div 2 + 4;
      If Left < 4 then
      begin
        AMessage.Width := (ButtonWidth+8)*Captions.Count;

        Left := (AMessage.Width - (ButtonWidth+8)*Captions.Count) div 2 + 4;
        AMessage.Message.Width := AMessage.Width - 58;
        // resize the message.
        AMessage.Message.Caption := Msg + ' ';
        AMessage.Message.Caption := Msg;
      end;

      // Figure out where the tops of the buttons should be.
      // The buttons should be below both the icon and the message with
      // a border of 8 pixels.
      ButtonTop := AMessage.Image.Top + AMessage.Image.Height + 8;
      if ButtonTop < AMessage.Message.Top + AMessage.Message.Height + 8 then
      begin
        ButtonTop := AMessage.Message.Top + AMessage.Message.Height + 8;
      end;

      // Set the height of the result form.
      AMessage.Height := ButtonTop + ButtonHeight + 8;

      // create buttons and assign properties.
      for resultIndex := 0 to Captions.Count -1 do
      begin
        AButton := TButton.Create(AMessage);
        AButton.Anchors := [akBottom];
        AButton.Parent := AMessage;
        AButton.Caption := Captions[resultIndex];

        AButton.Left := Left;
        AButton.Top := ButtonTop;
        AButton.Width := ButtonWidth;
        AButton.Height := ButtonHeight;
        AButton.ModalResult := ModalResults[resultIndex];
        AButton.Name := 'Button' + IntToStr(resultIndex+1);
        Inc(Left, ButtonWidth+8);
      end;

    finally
      Captions.Free;
    end;
  finally
    BitMap.Free;
  end;

  // Force the message to resize when the dialog box is resized
  AMessage.Message.Anchors := [akleft, akTop, akRight];

  // Don't allow the form to be made too small.
  AMessage.Constraints.MinWidth := AMessage.Width;
  AMessage.Constraints.MinHeight := AMessage.Height;
end;

end.
