{@abstract(The main purpose @name is to define @link(TfrmPixelPoint) which
  allows the user to specify the real world
  coordinates of a pixel location on a bitmap.)}
unit frmPixelPointUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, 
  GoPhastTypes, ArgusDataEntry, Vcl.Grids, RbwDataGrid4;

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
    rdgPoints: TRbwDataGrid4;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
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

procedure TfrmPixelPoint.FormCreate(Sender: TObject);
begin
  inherited;
  rdgPoints.Cells[0,0] := 'X';
  rdgPoints.Cells[1,0] := 'Y';
  rdgPoints.Cells[2,0] := 'ID (Optional)';
end;

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
        rdgPoints.Cells[1,0] := StrZ;
      end;
    vdSide:
      begin
        rdgPoints.Cells[0,0] := StrZ;
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
    StrToFloat(rdgPoints.Cells[0,1]), StrToFloat(rdgPoints.Cells[1,1]),
    rdgPoints.Cells[2,1]);
end;

end.

