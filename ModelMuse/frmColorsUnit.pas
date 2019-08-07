{@abstract(The main purpose of @name is to define @link(ColorValues)
  and @link(TfrmColors). These help control the 3D appearance of the model.)
  @link(ColorValues) stores values needed for 3D display of the model.
  @link(TfrmColors) allows the user to edit the values of @link(ColorValues)
    and thus affect the appearance of the 3D view of the model.}
unit frmColorsUnit;

interface

uses
  System.UITypes, SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons,
  ArgusDataEntry;

type
  {
  @abstract(@name stores values needed for 3D display of the model.)

  @longcode(#
  TColorValues = record
    X: double;
    Y: double;
    Z: double;
    Ambient: double;
    Diffuse: double;
    Specular: double;
  end;
  #)
  }
  TColorValues = record
    // @name is the X coordinate of the view point.
    X: double;
    // @name is the Y coordinate of the view point.
    Y: double;
    // @name is the Z coordinate of the view point.
    Z: double;
    // @name is the intensity of the ambient light.
    Ambient: double;
    // @name is the intensity of the diffuse light.
    Diffuse: double;
    // @name is the intensity of the specular light.
    Specular: double;
  end;

  {@abstract(@name allows the user to edit the values of @link(ColorValues)
    and thus affect the appearance of the 3D view of the model.)}
  TfrmColors = class(TfrmCustomGoPhast)
    // @name: TButton;
    // See @link(btnApplyClick)
    btnApply: TButton;
    // @name: TBitBtn;
    // @name closes the dialog box.
    btnClose: TBitBtn;
    // @name: TBitBtn;
    // @name shows help for the dialog box.
    btnHelp: TBitBtn;
    // @name: TLabel;
    // @name labels the edit box for the ambient light intensity.
    lblAmbient: TLabel;
    // @name: TLabel;
    // @name labels the edit box for the diffuse light intensity.
    lblDiffuse: TLabel;
    // @name: TLabel;
    // @name labels group of edit boxes for light intensity.
    lblLightIntensity: TLabel;
    // @name: TLabel;
    // @name labels group of edit boxes for light position.
    lblLightPosition: TLabel;
    // @name: TLabel;
    // @name labels the edit box for the specular light intensity.
    lblSpecular: TLabel;
    // @name: TLabel;
    // @name labels the edit box for the X-coordinate of the light position.
    lblX: TLabel;
    // @name: TLabel;
    // @name labels the edit box for the Y-coordinate of the light position.
    lblY: TLabel;
    // @name: TLabel;
    // @name labels the edit box for the Z-coordinate of the light position.
    lblZ: TLabel;
    // @name: TRbwDataEntry;
    // @name is  the edit box for the ambient light intensity.
    rdeAmb: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is  the edit box for the diffuse light intensity.
    rdeDiff: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is  the edit box for the specular light intensity.
    rdeSpec: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is the edit box for the X-coordinate of the light position.
    rdeX: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is the edit box for the Y-coordinate of the light position.
    rdeY: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is the edit box for the Z-coordinate of the light position.
    rdeZ: TRbwDataEntry;
    // @name transfers the data from this @classname to @link(ColorValues).
    procedure btnApplyClick(Sender: TObject);
    // @name asks the user if he want to apply the data before closing the
    // dialog box.
    procedure FormHide(Sender: TObject);
    // @name reads the values from ColorValues into this @classname.
    procedure FormShow(Sender: TObject);
    // @name sets @link(Modified) to @true.
    procedure rdeValuesChange(Sender: TObject);
  private
    // @name: boolean;
    // @name is set to true when the user has changed something is this
    // dialog box.
    Modified: boolean;
    // @name copies the data from this @classname to @link(ColorValues).
    procedure ApplyData;
    // @name reads @link(ColorValues) and copies those values to
    // the controls in this @classname.
    procedure GetData;
    { Private declarations }
  public
    // @name closes this @classname.
    procedure HideMe;
    { Public declarations }
  end;

// @name sets default values for @link(ColorValues).
procedure SetDefaults;

var
  // @name is the instance of @link(TfrmColors) used in GoPhast.
  frmColors: TfrmColors;
  // @name stores the values used for 3D display of the model.
  ColorValues: TColorValues;

implementation

uses frmGoPhastUnit;

resourcestring
  StrDoYouWantToApply = 'Do you want to apply the values you have specified?';

{$R *.dfm}

procedure TfrmColors.ApplyData;
begin
  with ColorValues do
  begin
    try
      X := StrToFloat(rdeX.Text);
    except on EConvertError do
      begin
      end;
    end;

    try
      Y := StrToFloat(rdeY.Text);
    except on EConvertError do
      begin
      end;
    end;

    try
      Z := StrToFloat(rdeZ.Text);
    except on EConvertError do
      begin
      end;
    end;

    try
      Ambient := StrToFloat(rdeAmb.Text);
    except on EConvertError do
      begin
      end;
    end;

    try
      Diffuse := StrToFloat(rdeDiff.Text);
    except on EConvertError do
      begin
      end;
    end;

    try
      Specular := StrToFloat(rdeSpec.Text);
    except on EConvertError do
      begin
      end;
    end;
  end;
  Modified := False;
  frmGoPhast.frame3DView.glWidModelView.Invalidate;
end;

procedure TfrmColors.btnApplyClick(Sender: TObject);
begin
  inherited;
  ApplyData;
end;

procedure TfrmColors.GetData;
begin
  with ColorValues do
  begin
    rdeX.Text := FloatToStr(X);
    rdeY.Text := FloatToStr(Y);
    rdeZ.Text := FloatToStr(Z);
    rdeAmb.Text := FloatToStr(Ambient);
    rdeDiff.Text := FloatToStr(Diffuse);
    rdeSpec.Text := FloatToStr(Specular);
    Modified := False;
  end;
end;

procedure SetDefaults;
begin
  with ColorValues do
  begin
    X := 60;
    Y := 60;
    Z := 40;
    Ambient := 1;
    Diffuse := 0;
    Specular := 0;
  end;
end;

procedure TfrmColors.rdeValuesChange(Sender: TObject);
begin
  inherited;
  Modified := True;
end;

procedure TfrmColors.FormShow(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmColors.FormHide(Sender: TObject);
begin
  inherited;
  if Modified and (MessageDlg(StrDoYouWantToApply, mtInformation,
    [mbYes, mbNo], 0) = mrYes) then
  begin
    ApplyData;
  end;
end;

procedure TfrmColors.HideMe;
begin
  Modified := False;
  Close;
end;

initialization
  SetDefaults;

end.

