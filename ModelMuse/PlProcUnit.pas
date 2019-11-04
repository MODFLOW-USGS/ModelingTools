{
This unit defines function that can be used to communicate with PLPROC.
http://www.pesthomepage.org/About_Us.php
}
unit PlProcUnit;

interface

uses
  ModflowIrregularMeshUnit, CustomModflowWriterUnit;

type
  // @name is used to write a file that can be read by read_mf_usg_grid_specs().
  TUsgGridSpecWrite = class(TCustomFileWriter)
  public
    procedure WriteUsgGridSpecs(DISV: TModflowDisvGrid);
  end;

implementation

{ TUsgGridSpecWrite }

procedure TUsgGridSpecWrite.WriteUsgGridSpecs(DISV: TModflowDisvGrid);
begin

end;

end.
