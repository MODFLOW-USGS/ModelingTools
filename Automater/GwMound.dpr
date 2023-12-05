program GwMound;

uses
  Forms,
  frmGwMoundUnit in 'frmGwMoundUnit.pas' {frmGwMound},
  HantushUnit in 'HantushUnit.pas',
  WellFunctionUnit in 'WellFunctionUnit.pas',
  FastGEO in 'FastGEO.pas',
  S_StarValues in 'S_StarValues.pas',
  RealListUnit in 'RealListUnit.pas',
  RootFinder in 'RootFinder.pas',
  BMSearch in 'BMSearch.pas',
  ReadModflowArrayUnit in 'ReadModflowArrayUnit.pas',
  IntListUnit in 'IntListUnit.pas',
  frmCustomBasinUnit in 'frmCustomBasinUnit.pas' {frmCustomBasin},
  BigCanvasMethods in 'BigCanvasMethods.pas',
  IniFileUtilities in 'IniFileUtilities.pas',
  InteractiveTools in 'InteractiveTools.pas',
  CursorsFoiledAgain in 'CursorsFoiledAgain.pas',
  frmAncestorUnit in 'frmAncestorUnit.pas' {frmAncestor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmGwMound, frmGwMound);
  Application.DefaultFont := frmGwMound.Font;
  Application.CreateForm(TfrmCustomBasin, frmCustomBasin);
  Application.Run;
end.
