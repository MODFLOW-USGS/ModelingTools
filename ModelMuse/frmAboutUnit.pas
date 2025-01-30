{@Abstract(@name defines the about box for GoPhast. See @link(TfrmAbout).)

@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit frmAboutUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls, Grids,
  RbwDataGrid4, JvExStdCtrls, JvHtControls, ComCtrls;

type
  {@abstract(@name shows a USGS logo, the version of the model and
    acknowledgements for code by others incorporated into or used by GoPhast.)
    If you double
    click on the logo, you see the version of the program that created the
    file that is open.}
  TfrmAbout = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // @name is the button to close the About dialog box.
    btnClose: TBitBtn;
    // @name: TImage;
    // @name displays the USGS visual identifier.
    ImageLogo: TImage;
    // @name: TLabel;
    // @name gives the name of the developer of GoPhast.
    lblDeveloperName: TLabel;
    // @name: TLabel;
    // @name gives the version number of GoPhast that created the file that
    // is currently open.
    lblFileVersion: TLabel;
    // @name: TLabel;
    // @name gives the name of the program.
    lblGoPhast: TLabel;
    // @name: TLabel;
    // @name gives the version number of GoPhast.
    lblVersion: TLabel;
    // @name: TLabel;
    // @name displays "Version".
    lblVersionCaption: TLabel;
    // @name: TPanel;
    // @name is the bottom panel.
    pnlBottom: TPanel;
    // @name: TPanel;
    // @name is the top panel.
    pnlTop: TPanel;
    dgCredit: TRbwDataGrid4;
    htlblWinston: TJvHTLabel;
    htlblVersion2: TJvHTLabel;
    btnGoToWeb: TBitBtn;
    htlblVersion3: TJvHTLabel;
    htlbl1: TJvHTLabel;
    ctgrypnlgrp1: TCategoryPanelGroup;
    ctgrypnl1: TCategoryPanel;
    memoDisclaimer: TMemo;
    ctgrypnl2: TCategoryPanel;
    reReference: TRichEdit;
    htlblBoyce: TJvHTLabel;
    // @name initialized the data in @link(dgCredit) as well as
    // @link(lblFileVersion) and @link(lblVersion).
    procedure FormCreate(Sender: TObject); override;
    // @name causes the instance of @classname to be destroyed when
    // it is hidden.
    procedure FormHide(Sender: TObject);
    // If the user double-clicks on @link(ImageLogo),
    // @name displays the version of GoPhast that created the file that
    // is currently open.
    procedure ImageLogoDblClick(Sender: TObject);
    procedure btnGoToWebClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  // @name is the variable used to hold an instance of the about form (@link(TfrmAbout)).
  frmAbout: TfrmAbout;

implementation

uses frmGoPhastUnit, RbwInternetUtilities,
  DisclaimerTextUnit;

{$R *.dfm}

procedure TfrmAbout.FormHide(Sender: TObject);
begin
  inherited;
  frmAbout := nil;
  Release
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  inherited;
  reReference.CaretPos := Point(0,0);
end;

procedure TfrmAbout.btnGoToWebClick(Sender: TObject);
var
  Browser: string;
begin
  inherited;
  LaunchURL(Browser, 'https://www.usgs.gov/software/modelmuse-a-graphical-user-interface-groundwater-models');
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  Row: integer;
begin
  inherited;
  dgCredit.FixedColor := Color;

  memoDisclaimer.WordWrap := True;
  memoDisclaimer.Lines.Clear;
  memoDisclaimer.Lines.Add(DisclaimerString);
  memoDisclaimer.Lines.Add('https://water.usgs.gov/software/help/notice/');

  lblVersion.Caption := frmGoPhast.PhastModel.Version;
  lblFileVersion.Caption := frmGoPhast.PhastModel.FileVersion;

  Row := 0;
  dgCredit.ColWidths[1] := 120;
  dgCredit.ColWidths[2] := 200;

  dgCredit.BeginUpdate;
  try
    dgCredit.Cells[0, Row] := 'Credit for:';
    dgCredit.Cells[1, Row] := 'Author(s):';
    dgCredit.Cells[2, Row] := 'URL or Reference';

  {  Inc(Row);
    dgCredit.Cells[0, Row] := 'Unofficial VisualCLX Patches Version '
      + FloatToStr(PatchedVCLX);
    dgCredit.Cells[1, Row] := 'Andreas Hausladen';
    dgCredit.Cells[2, Row] := 'http://www.kylix-patch.de.vu/';  }

  {  Inc(Row);
    dgCredit.Cells[0, Row] := 'QBindings.pas';
    dgCredit.Cells[1, Row] := 'Andreas Hausladen';
    dgCredit.Cells[2, Row] := 'http://www.kylix-patch.de.vu/';  }

  //  Inc(Row);
  //  dgCredit.Cells[0, Row] := 'QJpegLoader.pas';
  //  dgCredit.Cells[1, Row] := 'Andreas Hausladen';
  //  dgCredit.Cells[2, Row] := 'http://www.kylix-patch.de.vu/';

  //  Inc(Row);
  //  dgCredit.Cells[0, Row] := 'MemCheck.pas';
  //  dgCredit.Cells[1, Row] := 'Jean Marc Eber & Vincent Mahon';
  //  dgCredit.Cells[2, Row] := 'http://v.mahon.free.fr/pro/freeware/memcheck';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'rwXMLParser.pas, rwXMLConv.pas';
    dgCredit.Cells[1, Row] := 'Andre Mens, Rapware';
    dgCredit.Cells[2, Row] :=
      'www.rapware.com and http://codecentral.borland.com/codecentral/ccweb.exe/listing?id=15597';

  //  Inc(Row);
  //  dgCredit.Cells[0, Row] := 'rwXMLConv.pas';
  //  dgCredit.Cells[1, Row] := 'Andre Mens, Rapware';
  //  dgCredit.Cells[2, Row] :=
  //    'www.rapware.com and http://codecentral.borland.com/codecentral/ccweb.exe/listing?id=15597';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'XBase1.pas';
    dgCredit.Cells[1, Row] := 'Jamie Hart, Guy Chabant';
    dgCredit.Cells[2, Row] := 'http://aafi.free.fr';

    Inc(Row);
    dgCredit.Cells[0, Row] :=
      'AbstractTypedList.pas, ObserverIntfU.pas, ObserverListU.pas, ObserverU.pas, ObserverProxyU.pas';
    dgCredit.Cells[1, Row] := 'Peter Below';
    dgCredit.Cells[2, Row] := '';

  {  Inc(Row);
    dgCredit.Cells[0, Row] := 'ObserverIntfU.pas';
    dgCredit.Cells[1, Row] := 'Peter Below';
    dgCredit.Cells[2, Row] := '';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'ObserverListU.pas';
    dgCredit.Cells[1, Row] := 'Peter Below';
    dgCredit.Cells[2, Row] := '';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'ObserverU.pas';
    dgCredit.Cells[1, Row] := 'Peter Below';
    dgCredit.Cells[2, Row] := '';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'ObserverProxyU.pas';
    dgCredit.Cells[1, Row] := 'Peter Below';
    dgCredit.Cells[2, Row] := '';  }

    Inc(Row);
    dgCredit.Cells[0, Row] := 'Undo.pas';
    dgCredit.Cells[1, Row] := 'Warren Kovach';
    dgCredit.Cells[2, Row] :=
      'Kovach, Warren, 1998. Multiple Undo, The Delphi Magazine, 33:8-17';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'SparseArrayUnit.pas';
    dgCredit.Cells[1, Row] := 'Borland';
    dgCredit.Cells[2, Row] := 'modified from QGrids.pas';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'DXF_read.pas, DXF_Structs.pas, DXF_Utils.pas, DXF_write.pas';
    dgCredit.Cells[1, Row] := 'John Biddiscombe';
    dgCredit.Cells[2, Row] := '';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'OpenGL12x.pas';
    dgCredit.Cells[1, Row] := 'Project JEDI, Mike Lischke';
    dgCredit.Cells[2, Row] := 'http://delphi-jedi.org, '
      + 'http://www.soft-gems.net/Graphics.php';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'QGLWidget.pas';
    dgCredit.Cells[1, Row] := 'Qingrui Li';
    dgCredit.Cells[2, Row] := 'http://jijigaga.com/qrli/en.htm';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'arcball.pas';
    dgCredit.Cells[1, Row] := 'Chris Rorden';
    dgCredit.Cells[2, Row] :=
      'http://www.psychology.nottingham.ac.uk/staff/cr1/3d.html, '
      + 'http://www.delphi3d.net/listfiles.php?category=1';

  //  Inc(Row);
  //  dgCredit.Cells[0, Row] := 'Tipue search engine';
  //  dgCredit.Cells[1, Row] := '';
  //  dgCredit.Cells[2, Row] :=
  //    'http://www.tipue.com/';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'FastMM4.pas';
    dgCredit.Cells[1, Row] := 'Pierre le Riche';
    dgCredit.Cells[2, Row] :=
      'https://github.com/pleriche/FastMM4';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'UTM zones image';
    dgCredit.Cells[1, Row] := 'Peter H. Dana';
    dgCredit.Cells[2, Row] :=
      'http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html';

//    Inc(Row);
//    dgCredit.Cells[0, Row] := 'JasPerLib.dll';
//    dgCredit.Cells[1, Row] := 'Michael D. Adams';
//    dgCredit.Cells[2, Row] :=
//      'http://www.ece.uvic.ca/~mdadams/jasper/';

//    Inc(Row);
//    dgCredit.Cells[0, Row] := 'jpeg.dcu';
//    dgCredit.Cells[1, Row] := 'Gabriel Corneanu';
//    dgCredit.Cells[2, Row] :=
//      'http://cc.embarcadero.com/Item/19723';
//
//    Inc(Row);
//    dgCredit.Cells[0, Row] := 'TIFFRead.dll';
//    dgCredit.Cells[1, Row] := 'Nick Chislin, Michael Vinther';
//    dgCredit.Cells[2, Row] :=
//      'mv@logicnet.dk http://www.logicnet.dk/lib/';

//    Inc(Row);
//    dgCredit.Cells[0, Row] := 'ImageFileLib';
//    dgCredit.Cells[1, Row] := 'Michael Vinther, Vit Kovalcik, Rune  Møller';
//    dgCredit.Cells[2, Row] :=
//      'mv@logicnet.dk http://www.logicnet.dk/lib/ https://sourceforge.net/projects/imagefilelib/';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'FastGEO.pas';
    dgCredit.Cells[1, Row] := 'Arash Partow';
    dgCredit.Cells[2, Row] :=
      'http://fastgeo.partow.net http://www.partow.net/projects/fastgeo/index.html';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'TMenuItemHint';
    dgCredit.Cells[1, Row] := 'Zarko Gajic';
    dgCredit.Cells[2, Row] :=
      'http://delphi.about.com/od/vclusing/a/menuitemhints.htm';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'Line intersection code in ScreenObjectUnit.pas; '
      + 'TLine.GetIntersection in SelectUnit.pas';
    dgCredit.Cells[1, Row] := 'Mukesh Prasad';
    dgCredit.Cells[2, Row] :=
      'http://www1.acm.org/pubs/tog/GraphicsGems/gemsii/xlines.c';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'Color schemes';
    dgCredit.Cells[1, Row] := 'A. Light, P.J. Bartlein';
    dgCredit.Cells[2, Row] :=
      'http://geography.uoregon.edu/datagraphics/color_scales.htm';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'Coordinate conversion data';
    dgCredit.Cells[1, Row] := 'Snyder, 1987; Peter H. Dana';
    dgCredit.Cells[2, Row] :=
      'http://www.colorado.edu/geography/gcraft/notes/coordsys/coordsys_f.html '
      + 'http://earth-info.nga.mil/GandG/coordsys/datums/ellips.txt';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'LinRegression.pas';
    dgCredit.Cells[1, Row] := 'SysTools';
    dgCredit.Cells[2, Row] := '';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'SwapDoubleBytes in RbwInternetUtilities.pas';
    dgCredit.Cells[1, Row] := 'Justin Swett';
    dgCredit.Cells[2, Row] := 'http://community.borland.com/article/0,1410,28964,00.html';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'TScreenObject.ScreenObjectArea in ScreenObjectUnit.pas';
    dgCredit.Cells[1, Row] := 'Daniel Sunday';
    dgCredit.Cells[2, Row] := 'http://www.acm.org/jgt/papers/Sunday02/FastArea.html';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'TScreenObject.MinDistPointLine in ScreenObjectUnit.pas';
    dgCredit.Cells[1, Row] := 'Nils Haeck';
    dgCredit.Cells[2, Row] := 'http://www.simdesign.nl/tips/tip001.html';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'Triangle Interp. and Fitted Surface interpolation methods';
    dgCredit.Cells[1, Row] := 'Robert J. Renka';
    dgCredit.Cells[2, Row] := 'http://www.netlib.org/toms/751 '
      + 'http://www.netlib.org/toms/752';

    Inc(Row);
    dgCredit.Cells[0, Row] := 'ThickLine in RbwModelCube.pas';
    dgCredit.Cells[1, Row] := '';
    dgCredit.Cells[2, Row] := 'http://www.delphipages.com/threads/thread.cfm?ID=109695&G=109693';

//    Inc(Row);
//    dgCredit.Cells[0, Row] := 'THelpRouter';
//    dgCredit.Cells[1, Row] := 'EC Software';
//    dgCredit.Cells[2, Row] := 'http://www.ec-software.com/downloads_delphi.html';

//    Inc(Row);
//    dgCredit.Cells[0, Row] := 'TAbZipper';
//    dgCredit.Cells[1, Row] := 'TurboPower Software';
//    dgCredit.Cells[2, Row] := 'http://sourceforge.net/projects/tpabbrevia/';

  //  dgCredit.Cells[0, Row] := 'TZip';
  //  dgCredit.Cells[1, Row] := 'Angus Johnson';
  //  dgCredit.Cells[2, Row] := 'angusj@myrealbox.com http://www.angusj.com/delphi/';

  //  Inc(Row);
  //  Assert(Row < dgCredit.RowCount);
  //  dgCredit.Cells[0, Row] := 'Zipdll.dll Unzdll.dll';
  //  dgCredit.Cells[1, Row] := 'Eric W. Engler, R.Peters';
  //  dgCredit.Cells[2, Row] := 'englere@abraxis.com '
  //    + 'http://www.geocities.com/SiliconValley/Network/2114/ '
  //    + 'http://www.info-zip.org/ '
  //    + 'http://www.geocities.com/rjpeters_au/zipmaster.html';

//    Inc(Row);
//    Assert(Row < dgCredit.RowCount);
//    dgCredit.Cells[0, Row] := 'FastCode.pas,and related files';
//    dgCredit.Cells[1, Row] := 'The Fastcode Project';
//    dgCredit.Cells[2, Row] := 'http://fastcode.sourceforge.net/';

//    Inc(Row);
//    Assert(Row < dgCredit.RowCount);
//    dgCredit.Cells[0, Row] := 'RtlVclOptimize.pas';
//    dgCredit.Cells[1, Row] := 'Andreas Hausladen';
//    dgCredit.Cells[2, Row] :=
//      'http://andy.jgknet.de/dspeedup/index.php?page=download#RtlVclOptimize';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'FastMove.pas';
    dgCredit.Cells[1, Row] := 'John O''Harrow';
    dgCredit.Cells[2, Row] := 'http://fastcode.sourceforge.net/';

//    Inc(Row);
//    Assert(Row < dgCredit.RowCount);
//    dgCredit.Cells[0, Row] := 'FastObj.pas, FastSys.pas, PatchLib.pas';
//    dgCredit.Cells[1, Row] := 'Gabriel Corneanu';
//    dgCredit.Cells[2, Row] := 'gabrielcorneanu@gmail.com';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'GlScene';
    dgCredit.Cells[1, Row] := 'Eric Grange and Mike Lischke';
    dgCredit.Cells[2, Row] := 'http://glscene.sourceforge.net/';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'JCL/JVCL';
    dgCredit.Cells[1, Row] := 'Project JEDI';
    dgCredit.Cells[2, Row] := 'http://www.delphi-jedi.org/';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'frmManageFluxObservationsUnit';
    dgCredit.Cells[1, Row] := 'Modified from JvDualListForm in JVCL';
    dgCredit.Cells[2, Row] := 'http://www.delphi-jedi.org/';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'gpc';
    dgCredit.Cells[1, Row] := 'Alan Murta, Advanced Interfaces Group, '
      + 'University of Manchester.';
    dgCredit.Cells[2, Row] := 'http://www.cs.man.ac.uk/~toby/alan/software/ '
      + 'http://www.cs.man.ac.uk/~toby/alan/software/gpc.html';

{    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'TntControls';
    dgCredit.Cells[1, Row] := 'Troy Wolbrink';
    dgCredit.Cells[2, Row] := 'http://web.archive.org/web/20070202020227/http://www.tntware.com/delphicontrols/unicode/';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'TntExtendedEditors';
    dgCredit.Cells[1, Row] := 'Matt Harrison (matt@lummiec.co.uk)';
    dgCredit.Cells[2, Row] := 'http://lummie.co.uk/delphi-components/tnt-extended-editors/'; }

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'GetClosestPrime function in HashTableFacadeUnit.pas';
    dgCredit.Cells[1, Row] := 'Julian M Bucknall';
    dgCredit.Cells[2, Row] := 'http://www.boyet.com/FixedArticles/EZDSL.html';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'Virtual Tree View';
    dgCredit.Cells[1, Row] := 'Mike Lischke and Joachim Marder';
    dgCredit.Cells[2, Row] := 'http://www.jam-software.com/virtual-treeview/';
    //http://www.jam-software.de/virtual-treeview/

    {  Inc(Row);
      dgCredit.Cells[0,Row] := 'OpenGL12.pas';
      dgCredit.Cells[1,Row] := 'Mike Lischke';
      dgCredit.Cells[2,Row] := 'http://www.soft-gems.net/Graphics.php'; }

  //  Inc(Row);
  //  Assert(Row < dgCredit.RowCount);
  //  dgCredit.Cells[0, Row] := 'LinarBitmap.pas';
  //  dgCredit.Cells[1, Row] := 'Michael Vinther';
  //  dgCredit.Cells[2, Row] := 'mv@logicnet·dk';

//    Inc(Row);
//    Assert(Row < dgCredit.RowCount);
//    dgCredit.Cells[0, Row] := 'HashTrie.pas';
//    dgCredit.Cells[1, Row] := 'SoftComplete Development';
//    dgCredit.Cells[2, Row] := 'http://www.softcomplete.com/freeware.asp';

//    Inc(Row);
//    Assert(Row < dgCredit.RowCount);
//    dgCredit.Cells[0, Row] := 'UnitList.pas, UnitCRC.pas';
//    dgCredit.Cells[1, Row] := 'ContinuIT BV';
//    dgCredit.Cells[2, Row] := 'http://www.continuit.nl/index.php?LANGUAGE=EN&PAGE=FREEWARE';

//    Inc(Row);
//    Assert(Row < dgCredit.RowCount);
//    dgCredit.Cells[0, Row] := 'rmControls';
//    dgCredit.Cells[1, Row] := 'Ryan Mills';
//    dgCredit.Cells[2, Row] := 'http://www.mills-enterprise.ca/download/rmctl192.zip';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'MadExcept version 3';
    dgCredit.Cells[1, Row] := 'Mathias Rauen';
    dgCredit.Cells[2, Row] := 'http://www.madshi.net/madExceptDescription.htm';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'ssButtonEd.pas';
    dgCredit.Cells[1, Row] := 'Simon Armstrong';
    dgCredit.Cells[2, Row] := 'http://www.sadmansoftware.com/delphi/freeware/';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'GraphicsEx.pas';
    dgCredit.Cells[1, Row] := 'Mike Lischke';
    dgCredit.Cells[2, Row] :=
      'http://www.soft-gems.net/index.php?option=com_content&task=view&id=13&Itemid=33';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'Pcx.pas';
    dgCredit.Cells[1, Row] := 'Davie Reed';
    dgCredit.Cells[2, Row] := 'http://www.efg2.com/Lab/Library/Delphi/Graphics/PCX.ZIP';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'addbtn95.pas';
    dgCredit.Cells[1, Row] := 'PA van Lonkhuyzen';
    dgCredit.Cells[2, Row] := 'http://delphi.icm.edu.pl/ftp/d20free/addbtn95.zip';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'DataGrid.pas';
    dgCredit.Cells[1, Row] := 'EC Software';
    dgCredit.Cells[2, Row] := 'http://www.ec-software.com';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'TripackProcedures.pas, TriPackRoutines.pas';
    dgCredit.Cells[1, Row] := 'Robert Renka';
    dgCredit.Cells[2, Row] := 'http://www.netlib.org/toms/751, '
      + 'http://www.netlib.org/toms/, '
      + 'http://www.acm.org/publications/policies/softwarecrnotice';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'TriCP_Routines.pas';
    dgCredit.Cells[1, Row] := 'A. Preusser';
    dgCredit.Cells[2, Row] := 'http://www.netlib.org/toms/626, http://' +
      'www.netlib.org/toms/, http://www.acm.org/publications/policies/' +
      'softwarecrnotice';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'CuthillMcKeeRenumbering.pas';
    dgCredit.Cells[1, Row] := 'Ciprian Zavoianu';
    dgCredit.Cells[2, Row] := 'http://ciprian-zavoianu.blogspot.com/2009/01/project-bandwidth-reduction_18.html';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'TMMJLabel, MMJLabel.pas';
    dgCredit.Cells[1, Row] := 'Mihaela Mihaljevic Jakic';
    dgCredit.Cells[2, Row] := 'mickj@hi.hinet.hr';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'xygraph.pas';
    dgCredit.Cells[1, Row] := 'Wilko C Emmens';
    dgCredit.Cells[2, Row] := 'wcemmens@solcon.nl, http://home.solcon.nl/wcemmens/xygraph.htm';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'Gmsh';
    dgCredit.Cells[1, Row] := 'Christophe Geuzaine and Jean-François Remacle';
    dgCredit.Cells[2, Row] := 'http://gmsh.info/';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'Xml.VerySimple';
    dgCredit.Cells[1, Row] := 'Dennis Spreen';
    dgCredit.Cells[2, Row] := 'http://blog.spreendigital.de/2014/09/13/verysimplexml-2-0/ https://code.google.com/archive/p/verysimplexml/';

    Inc(Row);
    Assert(Row < dgCredit.RowCount);
    dgCredit.Cells[0, Row] := 'JmBasics.pas, JmFloatMatrix.pas, JmFloatVector.pas, JmTypes.pas, JediMath.inc, JmJedi.inc';
    dgCredit.Cells[1, Row] := 'JediMath';
    dgCredit.Cells[2, Row] := 'https://sourceforge.net/projects/jedimath/files/, http://sourceforge.net/projects/jedimath/';

    Assert(Row = dgCredit.RowCount-1);
  finally
    dgCredit.EndUpdate
  end;

  Width := 32 + dgCredit.ColWidths[0] + dgCredit.ColWidths[1] +
    dgCredit.ColWidths[2];
  if Width > Screen.Width then
    Width := Screen.Width;

  reReference.WordWrap := True;
end;

procedure TfrmAbout.ImageLogoDblClick(Sender: TObject);
begin
  inherited;
  lblFileVersion.Visible := not lblFileVersion.Visible;
end;

end.

