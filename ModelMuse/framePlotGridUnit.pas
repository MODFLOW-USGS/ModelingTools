unit framePlotGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameGridUnit, Grids, RbwDataGrid4,
  StdCtrls, Mask, JvExMask, JvSpin, Buttons, ExtCtrls;

type
  TframePlotGrid = class(TframeGrid)
    pbPlot: TPaintBox;
    splPlot: TSplitter;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  framePlotGrid: TframePlotGrid;

implementation

{$R *.dfm}

end.
