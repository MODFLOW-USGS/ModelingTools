{@abstract(The main purpose @name is to define @link(TfrmPixelPoint) which
  allows the user to specify the real world
  coordinates of a pixel location on a bitmap.)}
unit frmPixelPointUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, 
  GoPhastTypes, ArgusDataEntry;

type
  {@abstract(@name is used to allow the user to specify the real world
    coordinates of a pixel location on a bitmap.)}
  TfrmPixelPoint = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TLabel;
    // @name displays "X".
    lblX: TLabel;
    // @name: TLabel;
    // @name displays "Y".
    lblY: TLabel;
    // @name: TRbwDataEntry;
    // The user specifies the real-world X-coordinate of the pixel in @name
    rdeX: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // The user specifies the real-world Y-coordinate of the pixel in @name
    rdeY: TRbwDataEntry;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
  private
    // @name: integer;
    // @name is the X-pixel coordinate in an image.
    FPixelX: integer;
    // @name is the Y-pixel coordinate in an image.
    FPixelY: integer;
    // @name specifies a new real-world location of a pixel and stores it
    //in TfrmImportBitmap.@link(TfrmImportBitmap.dgPoints).
    procedure SetData;
    { Private declarations }
  public
  // @name stores the x and y coordinates of a pixel.  
  public procedure GetData(const AViewDirection: TViewDirection; const X, Y:
    integer);
    { Public declarations }
  end;

implementation

uses frmImportBitmapUnit;

resourcestring
  StrZ = 'Z';

{$R *.dfm}

{ TfrmPixelPoint }

procedure TfrmPixelPoint.GetData(const AViewDirection: TViewDirection; const X,
  Y: integer);
begin
  case AViewDirection of
    vdTop:
      begin
        // do nothing/
      end;
    vdFront:
      begin
        lblY.Caption := StrZ;
      end;
    vdSide:
      begin
        lblX.Caption := StrZ;
      end;
  else
    Assert(False);
  end;
  FPixelX := X;
  FPixelY := Y;
end;

procedure TfrmPixelPoint.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmPixelPoint.SetData;
begin
  frmImportBitmap.AddPoint(FPixelX, FPixelY,
    StrToFloat(rdeX.Text), StrToFloat(rdeY.Text));
end;

end.

