{@name is used to define @link(TframeInitialGridPosition).
@link(TframeInitialGridPosition) specifies the initial position of the grid.
It has controls for specifying the X, Y, and Z coordinates of the grid origin
as well as the grid angle and vertical exaggeration.
}
unit frameInitialGridPositionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ArgusDataEntry;

type
  {@link(TframeInitialGridPosition) specifies the initial position of the grid.
  It has controls for specifying the X, Y, and Z coordinates of the grid origin
  as well as the grid angle and vertical exaggeration.

  @member(rdeAngle @name is used to specify the grid angle.)
  @member(lblGridAngle @name labels @link(rdeAngle).)

  @member(rdeExaggeration @name is used to specify the vertical exaggeration.)
  @member(lblVerticalExaggeration @name labels @link(rdeExaggeration).)

  @member(lblGridOrigin @name labels the group of controls that specify the
    grid origin.  @seealso(rdeX) @seealso(rdeY) @seealso(rdeZ))

  @member(rdeX @name is used to specify X-coordinate of the grid origin.)
  @member(lblOriginX @name labels @link(rdeX).)

  @member(rdeY @name is used to specify X-coordinate of the grid origin.)
  @member(lblOriginY @name labels @link(rdeY).)

  @member(rdeZ @name is used to specify X-coordinate of the grid origin.)
  @member(lblOriginZ @name labels @link(rdeZ).)
  }
  TframeInitialGridPosition = class(TFrame)
    rdeAngle: TRbwDataEntry;
    lblGridAngle: TLabel;
    rdeExaggeration: TRbwDataEntry;
    lblVerticalExaggeration: TLabel;
    lblGridOrigin: TLabel;
    rdeX: TRbwDataEntry;
    lblOriginX: TLabel;
    rdeY: TRbwDataEntry;
    lblOriginY: TLabel;
    rdeZ: TRbwDataEntry;
    lblOriginZ: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
