unit xyun;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Button3: TButton;
    StatusBar1: TStatusBar;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    TabSheet3: TTabSheet;
    PaintBox3: TPaintBox;
    Edit1: TEdit;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    procedure DisplayHint(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses xygraph, xycopy, xygraph3d, math;

var res,err : array[1..12] of integer;
    labels : array of string;
    d0,d1,dd : double;
    y,m,d,h,n,s,ms : word;
    dmin : double;
    ystring : string;

{$R *.DFM}

(* This program is a test program for XYGRAPH. It demonstrates many
   options of XYGRAPH (but not all). Experiment with it; change parameters
   and see what the effect is. Add procedure calls.
   Or in other words: enjoy yourself.

   Please refer to XYGRAPH.HLP for descriprion of all procedures.

   The use of the PRINT&COPY form is explained in the unit XYCopy.pas *)

function sinx(x:single):single; begin sinx := sin(x); end;
function sinx2(x:single):single; begin sinx2 := sin(sqr(x)); end;

const nspokes=6;
var axes : array[0..nspokes-1] of Tradar;
    bdata : array of Tbardata;
    data, data2, data3, data4 : Tdatatype;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var i,y,e : integer;
    logscale : boolean;
begin
   logscale := checkbox1.checked;
   xycleargraph(paintbox1,clwhite,clblack,8/8);

   { - - - first graph - - - }

   xystartgraph(0,50,4,65,40,70,50,40,clipon);
   xyxaxis(clblack,1,100,0,0,'X-axis',1,gridoff,logscale,fixed,0);
   xyyaxis(clblack,1,100,0,0,'@First',1,gridoff,logscale,fixed);
   xybrush.style := bsdiagcross;
   xy3dquad(point3d(1,1,0),point3d(1,100,0),point3d(100,100,0),-1,clyellow,false,false);
   xybrush.style := bssolid;

   xymove(0,0); for i := 1 to 9 do
     begin xydraw(i*10,i*10); xysymbol(1,0,1); end;
   if logscale then xylegendentry(3,'symb 1')
               else xylegendentry(0,'line 1');

   xysetlinestyle(16,0,0);
   xyyaxis(clblue,1,10,0,0,'Second',2,gridoff,logscale,fixed);
   xylinewidth(1);
   xymove(0,0); for i := 1 to 9 do
     begin
       xydraw(i*10,i*10);
       xysymbol(2,8,1);
     end;
   if logscale then xylegendentry(3,'symb 2')
               else xylegendentry(0,'line 2');
   xysetlinestyle(0,0,0);

   xyyaxis(clgreen,1,10000,0,0,'Third',3,gridoff,logscale,fixed);
   xylinewidth(2);
   xymove(0,0); for i := 1 to 9 do
     begin xydraw(i*10,i*10); xysymbol(3,8,0); end;
   if logscale then xylegendentry(3,'symb 3')
               else xylegendentry(0,'line 3');
   xylinewidth(1);

   xyyaxis(clred,1,1000,0,0,'@Fourth',4,gridoff,logscale,fixed);
   xymove(0,0); for i := 1 to 9 do
     begin xydraw(i*10,i*10); xysymbol(4,8,2); end;
   if logscale then xylegendentry(3,'symb 4')
               else xylegendentry(0,'line 4');
   xytitle(clgray,'GRAPH 1');

   { - - - second graph - - - }

   xystartgraph(50,100,4,65,70,10,30,20,clipon);
   xysetgridlines(clred);
   with xygraphdata[0] do
     xy3dquad(point3d(x1,y1,0),point3d(x1,y2,0),point3d(x2,y2,0),
     -1,$d0ffd0,false,false);

   if checkbox1.checked then xylabelaxis(clblack,labels,gridoff)
     else xyyearaxis(clblack,3,0,gridoff);
   xyset(45,-90,3); xyset(41,1,3);
   xyyaxis(clblack,0,180,0,0,'Results',5,gridon,lin,fixed);
   if logscale then
     begin
       xypen.color := clfuchsia;
       for i := 1 to 12 do
         begin
           y := res[i]; e := err[i];
           if (i=1) then xymove(i,y) else xydraw(i,y);
           xyerrorbar(i,y,0,0.4,0.4,e*2,-e*2,e,-e,rel,userc,0);
         end;
       xylegendentry(4,'errbar');
     end
   else
     begin
       for i := 1 to 12 do xybar(clfuchsia,i,0.7,res[i],0);
       xylegendentry(1,'bar');
     end;

   xytitle(clgray,'GRAPH 2');

   { - - - third graph - - - }

   xysetfont('Times New Roman',10,2,0,0);
   xystartgraph(0,100,65,100,35,55,20,25,clipon);
   if checkbox1.checked then xysetratio(1);
   xyxaxis(clblack,-10,10,0,0,'',1,gridoff,logscale,fixed,0);
   xyyaxis(clblack,-1.05,1.05,0,0,'sine functions',5,gridoff,logscale,fixed);
   xylinewidth(2);
   xysetlinestyle(15,60,2);
   xydrawfunction(4,sinx);
   xypen.color := clred;
   xylinewidth(1);
   xysetlinestyle(0,0,0);
   xydrawfunction(0,sinx2,-9,9);

   xyfinish;

   xytitle(clgray,'GRAPH 3');
   xydeffont;

   { - - - additional instructions - - - }

   xylegendmake(3,paintbox1.width div 2,24,0,1,
     0,round(paintbox1.height*0.55),0,frameon);

   xysetusercoordinates(0,0);
   xytextangle(clblack,'XYGRAPH|3.0',paintbox1.width-10,
     round(paintbox1.height*0.82),0,1,1,-90);
   xytitle(clmaroon,'@2D CARTESIAN CO-ORDINATES');

   xyinitruler(clgray,20,round(paintbox1.height*0.65)-xycharheight div 2,1,0+8);
end;

procedure TForm1.PaintBox2Paint(Sender: TObject);
var i : integer;
begin
   xycleargraph(paintbox2,clwhite,clblack,8/8);
  {--------- POLAR DEMO ----------------------------------------}

   xystartgraph(0,50,5,100,40,40,40,40,clipon);
   xysetgridlines(clgreen);
   with xygraphdata[0] do
    xy3dcircle(point3d((x1+x2) div 2,(y1+y2) div 2,0),point3d(0,0,0),
      min((x2-x1) div 2,(y2-y1) div 2),0,-1,$fff0f0,false,false,0);

   if checkbox1.checked then
        xypolargraph(clmaroon,1,2,90,0,reverse,angle,gridon,true)
   else xypolargraph(clmaroon,1,2,90,0,normal,straight,gridon,true);
   xyplotarray(data4,0,2);
   xytitle(clgray,'POLAR GRAPH');

   xystartgraph(50,100,5,100,40,40,40,40,clipon);
   if checkbox1.checked then
        xyradargraph(clnavy,axes,2,90,1,normal,straight,sci,0)
   else xyradargraph(clnavy,axes,0,90,1,normal,straight,sci,0);
   xylinewidth(2);
   xypen.color := clred; xyradardraw([0.5,1,1.5,2,2.5],1,1);
   xypen.color := clgreen; xyradardraw([0.8,0.8,0.8,0.8,0.8],1,2);
   xypen.color := clblue; xyradardraw([0.5,1,0.5,1,0.5,1,0.5,1],1,2);
   xytitle(clgray,'RADAR GRAPH');  

   xyfinish;
   xytitle(clmaroon,'@2D POLAR CO-ORDINATES');
   xyinitruler(clblack,paintbox2.width div 2,paintbox2.height-16,1,0);
end;

procedure TForm1.PaintBox3Paint(Sender: TObject);
var tm : boolean;
begin
  xycleargraph(paintbox3,clwhite,clblack,1.0);

  {--------- TIMEAXIS DEMO ----------------------------------------}

  if checkbox1.checked then
    begin
      dmin := 1;
      xystartgraph(0,100,0,40,30,20,30,60,clipon);
      xytimeaxis(clblack,d0-dd,d0+dd,'date',1,0,0,0,2,false,tm);
    end
  else
    begin
      dmin := 1/24/60/60;
      xystartgraph(0,100,0,40,30,20,30,50,clipon);
      xytimeaxis(clblack,d0-dd,d0+dd,'date|time and date '+ystring,-1,1,1+4,1,2,false,tm);
    end;
  xyyaxis(clblack,0,100,0,0,'',1,gridoff,lin,fixed);
  xymove(d0,0); xydraw(d0,100);
  d1 := encodedate(y,m,d)+encodetime(h,n,s,0); xymove(d1,0); xydraw(d1,90);
  d1 := encodedate(y,m,d)+encodetime(h,n,0,0); xymove(d1,0); xydraw(d1,80);
  d1 := encodedate(y,m,d)+encodetime(h,0,0,0); xymove(d1,0); xydraw(d1,70);
  d1 := encodedate(y,m,d); xymove(d1,0); xydraw(d1,60);
  d1 := encodedate(y,m,1); xymove(d1,0); xydraw(d1,50);
  d1 := encodedate(y,1,1); xymove(d1,0); xydraw(d1,40);
  d1 := encodedate(2000,1,1); xymove(d1,0); xydraw(d1,30);
  d1 := encodedate(1900,1,1); xymove(d1,0); xydraw(d1,20);
  xytitle(clmaroon,'TimeAxis demo, press + to zoom in, - to zoom out, SPACE to reset');

  {------- PLOTARRAY DEMO ------------------------------------------}

  xystartgraph(0,33,45,100,30,0,20,40,clipon);
  if checkbox1.checked then
    begin
    end
  else
    begin
      xyxaxis(clblack,0,20,0,0,'X-axis',false,false,false);
      xyyaxis(clgreen,0,25,0,0,'Y-axis',1,false,false,false);
      xysymbol(2,4,2);
    end;
  xyplotarray(data,0,2);
  xytitle(clmaroon,'PlotArray single');

  xystartgraph(33,67,45,100,30,0,20,40,clipon);
  if checkbox1.checked then
    begin
      {do nothing - xyplotarray defines axes }
    end
  else
    begin
      xyxaxis(clblack,0,20,0,0,'X-axis',false,false,false);
      xyyaxis(clblack,0,50,0,0,'Y-1',1,false,false,false);
      xyyaxis(clred,0,50,0,0,'Y-2',2,false,false,false);
      xyyaxis(clblue,0,50,0,0,'Y-3',3,false,false,false);
      xysymbol(2,4,2);
    end;
  xyplotarray(data2,0,3);
  xytitle(clmaroon,'PlotArray multiple');

  xystartgraph(67,100,45,100,30,8,20,40,clipon);
  if checkbox1.checked then
    begin
      xylabels[0] := 'X data';
    end
  else
    begin
      xyxaxis(clblack,0,20,0,0,'X-axis',false,false,false);
      xyyaxis(clblack,0,40,0,0,'Y-axis',1,false,false,false);
    end;
  xyplotarray(data3,0,3);
  xytitle(clmaroon,'PlotArray advanced');

  if checkbox1.checked then
     xyinitruler(clred,8,round(paintbox3.height * 0.45)-20,1,3)
  else
     xyinitruler(clmaroon,8,round(paintbox3.height * 0.45)-20,1,0)
end;

procedure showdata;
var s : string;
begin
  with form1.Edit1 do with xyexportd do
    begin
      str(xw:5:2,s); text := 'x='+s;
      str(sinx(xw):6:3,s);  text := text + ',  sin(x)='+s;
      str(sinx2(xw):6:3,s); text := text + ',  sin(x^2)='+s;
    end;
end;

procedure setarrays;
var i : integer;
const styl1 : array[1..7] of integer = (4,clfuchsia,1,4,2,2,2);
      styl2 : array[1..7] of integer = (1,clblack,2,4,0,0,0);
      styl3 : array[1..7] of integer = (0,clgreen,1,0,1,6,1);
begin
  xysetdataarray(data,20,1);
  for i := 1 to 20 do data[i,1] := i+random*5;

  xysetdataarray(data2,19,3);
  for i := 1 to 19 do data2[i,1] := i+random*5;
  for i := 1 to 19 do data2[i,2] := i+random*2+12;
  for i := 1 to 19 do data2[i,3] := i+random*5+20;
  data2[0,2] := 2;

  xysetdataarray(data3,20+7,3);
  for i := 1 to 20 do
    begin data3[i+7,0] := i; data3[i+7,1] := 10+random*10;
      data3[i+7,2] := 18+random*10; data3[i+7,3] := 26+random*10; end;
  for i := 1 to 7 do
    begin data3[i,1] := styl1[i]; data3[i,2] := styl2[i]; data3[i,3] := styl3[i];end;
  data3[0,0] := 1; data3[0,1] := 1; data3[0,2] := 1; data3[0,3] := 1;

  xysetdataarray(data4,5,2);
  for i := 1 to 5 do data4[i,0] := 1 + i/10;
  for i := 1 to 5 do data4[i,1] := i*60;
  for i := 1 to 5 do data4[i,2] := i*50;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  xyexportd.xp := -1;
  xymousedown(button,shift,x,y);
  if (xyexportd.xp>=0) and (xyexportd.igr=3) and
   not (ssCtrl in shift) then
     begin
       Edit1.top := round(paintbox1.height*0.65)-6;
       Edit1.show;
       showdata;
     end;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  xymousemove(shift,x,y);
  if (xyexportd.xp>=0) and (xyexportd.igr=3) and
   not (ssCtrl in shift) then showdata;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  xyexportd.xp := -1;
  edit1.hide;
  xymouseup(button,shift,x,y);
end;

procedure TForm1.PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin xymousedown(button,shift,x,y); end;

procedure TForm1.PaintBox2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin xymousemove(shift,x,y); end;

procedure TForm1.PaintBox2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin xymouseup(button,shift,x,y); end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin xykeydown(key,shift); end;

procedure TForm1.DisplayHint(Sender: TObject);
begin StatusBar1.SimpleText := Application.Hint; end;

procedure TForm1.FormCreate(Sender: TObject);
var i : integer;
begin
  randomize; for i := 1 to 12 do res[i] := 100 + random(50);
  for i := 1 to 12 do err[i] := 5 + random(10);
  setlength(labels,12); for i := 0 to 11 do labels[i] := inttostr(i+1);
  checkbox1.hint := '|change to: graph 1: log scale, symbol legend; graph 2: labelaxis, errorbar; graph 3: fixed Y/X ratio';
  button3.hint := '|print, copy or save graph';
  paintbox1.hint := '|graph area; click for ruler function; control-click to zoom';
  paintbox2.hint := '|graph area; click for ruler function; control-click to zoom';
  paintbox3.hint := '|graph area; click for ruler function; control-click to zoom';
  for i := 1 to nspokes do with axes[i-1] do
    begin txt := 'Nr. '+inttostr(i); mi := 0; ma := i; end;
  setlength(bdata,3);
  with bdata[0] do begin ypos := 20; col := clred; end;
  with bdata[1] do begin ypos := 30; col := clgreen; end;
  with bdata[2] do begin ypos := 40; col := clblue; end;
  d0 := date+time; dd := 1; decodedate(d0,y,m,d); decodetime(d0,h,n,s,ms);
  ystring := '('+inttostr(y)+')';
  Application.OnHint := DisplayHint;

  setarrays;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  xyunzoom;
  if (pagecontrol1.activepage=tabsheet1) then paintbox1.repaint else
  if (pagecontrol1.activepage=tabsheet2) then paintbox2.repaint
  else
    begin if (dd<dmin) then dd := dmin; paintbox3.Repaint; end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin xycopystart; end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  if (pagecontrol1.activepage=tabsheet1) then
  checkbox1.hint := '|change to: graph 1: log scale, symbol legend; graph 2: labelaxis, errorbar; graph 3: fixed Y/X ratio'
  else if (pagecontrol1.activepage=tabsheet2) then
  checkbox1.hint := '|change: polar: reverse, orientation, angle;  radar: frame style (equal axes)'
  else
  checkbox1.hint := '|change: top: mode and date/time presentation; bottom: autoscale; both: ruler';
  xyunzoom;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if not (pagecontrol1.activepage=tabsheet3) then exit;
  if (key='-') then
    begin if (dd<dmin) then dd := dmin; dd := dd * 1.02; paintbox3.refresh; end;
  if (key='+') then begin dd := dd / 1.02; paintbox3.refresh; end;
  if (key=#32) then begin dd := 1; paintbox3.refresh; end;
end;

end.


