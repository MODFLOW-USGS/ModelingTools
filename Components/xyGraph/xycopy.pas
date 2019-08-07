unit XYCopy;
(*
This unit is a utility for easy printing and copying of graphs.
It enables the selection of a mode for copy or print, and to set the printer.

If there are multiple graphs, each buffered by XYPutBuffer, it also
enables a selection of one of these with a ComboBox.
For each graph the information is presented like:
  LL {N} WWxHH [CVM] [BMP] [STR] [B4D]
meaning: LL    the label of the graph as set by XYPutBuffer
         {N}   the number of graph elements (calls to XYStartGraph, 1-8)
         WWxHH dimensions of the canvas in pixels
         CVM   graph is plotted in canvas mode, no high-res or metafiles
         BMP   graph contains a bitmap that is saved as a bitmap in metafiles
         B4D   graph contains a 4D bitmap that is changed in high-res or metafiles
         STR   graph contains red/green stereo image
A preceding dot indicates that this graph is visible on the active form.
If there are no graphs stored into the buffer the combobox is disabled and
will show information of the present graph(s) with -- as label.

How to implement this utility:
- add XYCopy.pas to your project: click Project | Add to Project ...
- add XYCopy to the uses clause of your program/unit
- start the utility with a call to XYCopyStart

It is possible to set default names for .BMP or .WMF files with the variables
  xybmpname and xywmfname.
The variable xywmfback determines if a frame and background is to be
  created in metafiles (default = false).

The variable xycvplot is only required for creating metafiles in canvas mode.
It has to be set to the procedure that plots the entire graph like:
  xycvplot := myplottingprocedure;
this will be detected automatically and the metafile creation will be
  enabled accordingly.
*)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TCopyForm = class(TForm)
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    PrinterSetupDialog1: TPrinterSetupDialog;
    ComboBox1: TComboBox;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CopyForm: TCopyForm;

  xybmpname : string = '';
  xywmfname : string = '';
  xywmfback : boolean = false;

  xycvplot : Tprocedure;

procedure xycopystart;

implementation

uses xygraph;

{$R *.DFM}

var buf,cvmode : boolean;
    bufl : integer;
    frm : string;
    oldindex : integer;

procedure xycopystart;
begin
   frm := screen.activeform.name;
   copyform.showmodal;
end;

procedure TCopyform.BitBtn3Click(Sender: TObject);
begin copyform.modalresult := 1; end;

procedure TCopyform.BitBtn2Click(Sender: TObject);
begin printersetupdialog1.execute; end;

procedure go;
var ok : boolean;
begin
  ok := xywmfback;
  if buf then xygetbuffer(bufl);
  case copyform.radiogroup1.itemindex of
   0 : begin xycopytoclipboard; showmessage('Graph copied to clipboard'); end;
   1 : begin xyprint(true); end;
   2 : begin xyprint2(0); end;
   3 : begin xysaveasbitmap(xybmpname,msgon,ok); end;
   4 : begin if not cvmode then xysaveasmetafile(xywmfname,msgon,ok,1)
        else xysaveasmetafile(xywmfname,msgon,ok,1,xycvplot); end;
   5 : begin if not cvmode then xysaveasmetafile(xywmfname,msgon,ok,2)
        else xysaveasmetafile(xywmfname,msgon,ok,2,xycvplot); end;
   6 : begin if not cvmode then xysaveasmetafile(xywmfname,msgon,ok,4)
        else xysaveasmetafile(xywmfname,msgon,ok,4,xycvplot); end;
  end;
end;

procedure TCopyform.BitBtn1Click(Sender: TObject);
{this enters filenames and starts the action by a timer delay}
var s : string;
   ok : boolean;
procedure getname1;
begin
  s := xybmpname;
  ok := inputquery('Save graph as bitmap file','Enter filename',s);
  if (s='') then ok := false; if not ok then exit;
  s := changefileext(s,'.bmp'); xybmpname := s;
end;
procedure getname2;
begin
  s := xywmfname;
  ok := inputquery('Save graph as metafile','Enter filename',s);
  if (s='') then ok := false; if not ok then exit;
  s := changefileext(s,'.wmf'); xywmfname := s;
end;
begin
  copyform.modalresult := 1;
  copyform.hide;
  ok := true;
  case radiogroup1.itemindex of
   3 : getname1; 4..6 : getname2; end;
 if ok then timer1.enabled := true;
end;

procedure dummy; begin end;

procedure TCopyForm.FormCreate(Sender: TObject);
begin
  radiogroup1.itemindex := 0;
  oldindex := 0;
  xycvplot := dummy;
end;

procedure setmode(n : integer);
var metaok : boolean;
{this disables some options for graphs in canvas mode}
begin
  cvmode := xybuffers[n].cvm;
  with copyform.RadioGroup1 do
  begin
  metaok := (not cvmode) or (@xycvplot <> @dummy);
  Tradiobutton(components[2]).enabled := not cvmode;
  Tradiobutton(components[4]).enabled := metaok;
  Tradiobutton(components[5]).enabled := metaok;
  Tradiobutton(components[6]).enabled := metaok;
  if (not metaok) then
   if itemindex in [4,5,6] then itemindex := 3;
  if cvmode then
   if (itemindex=2) then itemindex := 1;
  end;
end;

procedure TCopyForm.FormShow(Sender: TObject);
{this will fill the combobox with correct information}
var i,n : integer;
function tekst(n:integer): string;
var s : string;
begin
  with xybuffers[n] do
    begin
      if (n=0) then s := '--' else s := inttostr(lbl);
      s := s+' {'+inttostr(ngr)+'} '+inttostr(cw)+'x'+inttostr(ch);
      if cvm then s := s + ' CVM' else
      if bmp then s := s + ' BMP' else
      if b4d then s := s + ' B4D' else
      if str then s := s + ' STR';
      if (frm=form) then s := #149' '+s else s := '   ' + s;
    end;
  result := s;
end;
begin
  n := length(xybuffers);
  with combobox1 do with items do
  if (n=1) then
    begin
      text := tekst(0); buf := false;
      itemindex := 0; enabled := false;
      setmode(0); oldindex := 0;
    end
  else
    begin
      text := '';
      for i := 1 to n-1 do add(tekst(i));
      if (oldindex>n-2) then oldindex := 0;
      itemindex := oldindex; enabled := true;
      buf := true; bufl := xybuffers[1].lbl;
      setmode(1);
    end;
end;

procedure TCopyForm.ComboBox1Change(Sender: TObject);
var n : integer;
begin
  n := combobox1.itemindex;
  bufl := xybuffers[n+1].lbl;
  setmode(n+1);
  oldindex := n;
end;

procedure TCopyForm.Timer1Timer(Sender: TObject);
{this timer delay ensures that the graphs are refreshed
  before they are copied from the screen}
begin
  timer1.enabled := false; go;
end;

end.
