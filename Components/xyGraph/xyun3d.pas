unit xyun3d;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Spin;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    PaintBox2: TPaintBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button3: TButton;
    SpinEdit8: TSpinEdit;
    CheckBox7: TCheckBox;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label7: TLabel;
    ScrollBar1: TScrollBar;
    CheckBox1: TCheckBox;
    SpinEdit7: TSpinEdit;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    RadioGroup1: TRadioGroup;
    GroupBox3: TGroupBox;
    SpinEdit1: TSpinEdit;
    GroupBox4: TGroupBox;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    Edit1: TEdit;
    GroupBox5: TGroupBox;
    PaintBox1: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    CheckBox4: TCheckBox;
    SpinEdit6: TSpinEdit;
    Button1: TButton;
    CheckBox9: TCheckBox;
    TabSheet2: TTabSheet;
    PaintBox3: TPaintBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    SpinEdit9: TSpinEdit;
    SpinEdit10: TSpinEdit;
    SpinEdit11: TSpinEdit;
    SpinEdit12: TSpinEdit;
    Button2: TButton;
    CheckBox8: TCheckBox;
    CheckBox10: TCheckBox;
    TabSheet3: TTabSheet;
    PaintBox4: TPaintBox;
    Label14: TLabel;
    SpinEdit15: TSpinEdit;
    SpinEdit16: TSpinEdit;
    Label16: TLabel;
    SpinEdit18: TSpinEdit;
    Elements: TRadioGroup;
    Button4: TButton;
    RadioGroup2: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpinEdit7Change(Sender: TObject);
    procedure SpinEdit8Change(Sender: TObject);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure UpDown3Click(Sender: TObject; Button: TUDBtnType);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControl1Change(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure SpinEdit9Change(Sender: TObject);
    procedure SpinEdit10Change(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure PaintBox4Paint(Sender: TObject);
    procedure SpinEdit15Change(Sender: TObject);
    procedure SpinEdit16Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure DisplayHint(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

(* This sample program shows a very complicated situation, three separate graphs
   of which one in canvas mode, and the animation made things even worse.
   Making this revealed several bugs in the program and as such it has been
   useful, but you are advised to keep things more simple.
   This program is only intended to show some possibilities of
   XYGRAPH3D and XYGRAPH4D.

   The use of the PRINT&COPY form is explained in the unit XYCopy.pas *)

uses xygraph, xygraph3d, xygraph4d, xycopy;

{ constants for the animation: adjust these to the speed of your computer}
const timerinterval = 200;   {set timer speed, interval in ms}
      rotationincrement = 10; {set rotation increment in degr}

const dataA : array[1..9] of single =
         (0,0.1,0.2,0.3,0.45,0.6,0.8,1,1.25);
const dataB : array[0..5] of array[0..5] of single =
           (  ( 0, -2, -1,  0,  1,  2),
              (-2,0.5,0.5,0.5,0.5,0.5),
              (-1,0.5,  1,  1,  1,0.5),
              ( 0,0.5,  1,  0,  1,0.5),
              ( 1,0.5,  1,  1,  1,0.5),
              ( 2,0.5,0.5,0.5,0.5,0.5) );

const colorscheme : array[1..2,0..8] of Tcolor =
  ( ($000000, $804040, $ff8080, $00c000, $00c0c0, $4080a0, $204060, $808080, $FFFFFF),
    ($000000, $000060, $800080, $ff0000, $ff8080, $20ff20, $00ffff, $0080ff, $0000e0));
 {($000000, $004080, $0000ff, $0080ff, $00ffff, $00ff00, $ff4040, $c000c0, $808080, $ffffff)}

const data4a : array[1..11] of single = (-2,-1.5,-1,-0.6,-0.3,0,0.3,0.6,1,1.5,2);
      data4b : array[1..21] of single =
( -2,-1.8,-1.6,-1.4,-1.2,-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2);

var data1,data2,data3,datac : Tdatatype;
    hoek1, hoek2, mode1, mode2, cmode : integer;
    lh,lp : integer;
    data0 : array[0..4,1..10] of single;
    data4da, data4db : T4dtype;

procedure draw1; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {this procedure plots the 3D surface in the animation plot on Image1}
var ok : boolean;
    hoek2 : integer;
    s : string;
    d : single;
    const backcol = $000050;
begin
  if not form1.visible then exit;
  hoek2 := form1.scrollbar1.position*5;
  with form1 do with image1 do with canvas do begin
   if checkbox6.checked then d := 0.12 else d := 0;
   picture.bitmap.height := height; picture.bitmap.width := width;
   xycanvasgraph(canvas,width,height,backcol,0,1,true);
   xystartgraph(0,100,0,100,0,0,0,0,false);
   if (radiogroup1.itemindex=0) then
     xy3dsetframe(-1.3,1.3,-1.3,1.3,0,1,1,1,1,0)
   else
     xy3dsetframe(-2,2,-2,2,0,1,1,1,1,0);
   xy3dsetview(hoek1,hoek2,4,0,0,0,0,0,0);
   if checkbox9.checked then xy3dsetstereo(4,0,0);
   xy3dsetsun(lp,lh,1,1);
   if checkbox1.checked then xy3dshowframe(2,clyellow,-1,-1,-1,3);
   if (radiogroup1.itemindex=0) then xy3dloadsurface(data1,ok,spinedit1.value,1)
      else xy3dloadsurface(data2,ok,spinedit1.value,1);
   {xy3dloadsurface(datac,ok,0,8);}

   xy3dshowsurface(mode1,d,clwhite,clblack,clwhite,
     checkbox5.checked,spliton,0,0);
   xy3dcloseframe;
   s := inttostr(hoek1)+' / '+inttostr(hoek2);
   xybrush.color := backcol;
   xytext(clwhite,s,0,0,1,1,0);
   xyputbuffer(3);
  end;
end;

procedure draw2; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {this procedure plots the cross-section from the contourplot on Image2}
var i,n : integer;
    ymi,yma : single;
    const backcol = $500000;
begin
  if not form1.visible then exit;
  with form1 do with image1 do with canvas do begin
   picture.bitmap.height := height; picture.bitmap.width := width;
   xycanvasgraph(canvas,width,height,backcol,backcol,0.9,true);
   xystartgraph(0,100,0,100,20,5,35,35,false);
   n := length(xy3dcrosssection); if (n<2) then exit;
   ymi := xy3dcrosssection[0].z; yma := ymi;
   for i := 1 to n-1 do with xy3dcrosssection[i] do
     if (z<ymi) then ymi := z else if (z>yma) then yma := z;
   if (ymi=yma) then begin ymi := ymi-0.5; yma := ymi+1; end;
   xysetratio(1); xyunzoom;
   xyxaxis(clwhite,0,xy3dcrosssection[n-1].l,0,0,'position',false,false,false);
   xyyaxis(clwhite,ymi,yma,0,0,'height',1,false,false,false);
   xymove(0,xy3dcrosssection[0].z);
   for i := 1 to n-1 do with xy3dcrosssection[i] do xydraw(l,z);
   xytitle(clyellow,'cross-section');
   xyputbuffer(3);
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {this procedure plots the general 3D plot with lines, bars and symbols}
var x,y,sy,wi : integer;
    d : single;
    ok : boolean;
begin
  wi := paintbox1.width;
  if checkbox4.checked then d := 5 else d := 0;
  xycleargraph(paintbox1,clsilver,clblack,1.0);  
  xystartgraph(0,100,0,100,20,20,20,20,clipoff);
  xyset(100,3); xyset(110,3); xyset(120,3);
  xy3dsetframe(0,11,0,6,0,10,1.5,0.6,spinedit5.value / 100,0);
  xy3dsetlabels('X-axis','Y-axis','Z-axis','','','',false,2,2,2,0,0);
  xy3dsetview(spinedit3.value,spinedit4.value,d,0,0,0,0,0,0);
  if checkbox9.checked then xy3dsetstereo(3,-0.5,3,0);
  xy3dshowframe(spinedit2.value,clblack,clwhite,clwhite,clwhite,0);

  xy3dsettransmode(1,0,0);
  y := 5; data3[0,1] := y-0.25; data3[0,2] := y+0.25;
  xy3dloadsurface(data3,ok,0,0);
  xy3dshowsurface(5,0.3,clblack,clred,clred,shell,splitoff,1,0);
  xylegendentry(2,'surface');

  y := 4; for x := 10 downto 1 do
    xy3dbar(point3d(x-0.4,y-0.25,0),point3d(x+0.4,y+0.25,data0[3,x]),
        clblack,cllime,-1,clgreen,-1,-1,-1,false,(x>5));
  xylegendentry(2,'3D bar');

  xypen.color := clblack;

  y := 3; xylinewidth(2);
  for x := 10 downto 1 do
    if (x=10) then xy3dmove(x,y,data0[2,x]) else xy3ddraw(x,y,data0[2,x]);
  xylegendentry(0,'line');
  xylinewidth(1); xysetlinestyle(8,80,0);
  for x := 10 downto 1 do
    begin xy3dmove(x,y,0); xy3ddraw(x,y,data0[2,x]);
      xy3dsymbol(0,0,0,1,5,0,0,lightoff,false,1);
     end;
  xysetlinestyle(0,0,0);

  y := 2; sy := spinedit6.value;
  xysetlinestyle(8,70,0);
  xy3dsettransmode(1,0.5,0);
  for x := 10 downto 1 do
    begin
      xy3dmove(x,data0[0,x],0); xy3ddraw(x,data0[0,x],data0[1,x]);
      if (sy in [3,4]) then xylinewidth(2);
      xy3dsymbol(x,data0[0,x],data0[1,x],sy,0,0,claqua,lightoff,(x>5),0);
      xylegendentry(2,'3D symbol');
      xylinewidth(1);
    end;
  xysetlinestyle(0,0,0);
  xy3dcloseframe;

  xylegendmake(1,wi div 2,0,0,1,0,0,0,true);

  with paintbox1 do xyinitruler(clblack,width-4,height-xycharheight-2,-1);
  xyputbuffer(1);
end;

procedure TForm1.PaintBox2Paint(Sender: TObject); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {this procedure plots the contourplot in different styles}
var ok : boolean;
    i,grcol,wi,he : integer;
begin
  {xy3dsetcrosssection(0,0);}
  with form1 do begin
   wi := paintbox2.width; he := paintbox2.height;
   xycleargraph(paintbox2,$500000,0,0.9);
   xystartgraph(0,100,0,100,20,35,5,20,false);
   if radiogroup1.itemindex=0 then xy3dloadsurface(data1,ok,spinedit1.value,1)
      else xy3dloadsurface(data2,ok,spinedit1.value,1);
   xy3dsetsun(lp,lh,1,1); grcol := clwhite;
   case mode2 of
    0 : begin
        xy3dshowcontour(0,clwhite,$e0ffff,'','',0,0,1,1.5,0,16,normal,splitoff,false,dummy,0);
        xy3dheightscale(wi-30,30,wi-22,he-20,'height',0);
        for i := 0 to 20 do
         if (i in [5,10,15,20]) then xy3dheightline(i/20,clred,0)
           else xy3dheightline(i/20,clblack,0);
         grcol := clgreen;
        end;
    1 : begin
         xy3dshowcontour(1,clwhite,$fff0f0,'','',0,0,1,1.5,0,16,normal,spliton,
          checkbox7.checked,dummy,0);
        xy3dheightscale(wi-30,30,wi-22,he-20,'height',0);
        end;
   2,3 : begin
        if (cmode>0) then xy3dsetcolors(colorscheme[cmode]);
        xy3dshowcontour(mode2,clwhite,$fff0f0,'','',0,0,1,1.5,0,16,checkbox3.checked,spliton,
         checkbox7.checked,dummy,3);
        xy3dheightscale(wi-30,30,wi-22,he-20,'height',0);
        end;
   4,5 :  begin
        xy3dshowcontour(mode2,clwhite,$fff0f0,'','',0,0,1,1.5,0,16,normal,spliton,
          checkbox7.checked,dummy,3);
        xy3dheightscale(wi-30,30,wi-22,he-20,'height',0);
        end;
   end;
   if checkbox2.checked then
     if radiogroup1.itemindex=0 then xy3dshowgrid(grcol,0,0)
        else xy3dshowgrid(grcol,3,1);
  end;
  xy3dinitruler(-1,80,1,0);
  xyputbuffer(2);
end;

procedure setlight;
begin
  if (lp>=360) then dec(lp,360);
  if (lp<0) then inc(lp,360);
  form1.Edit1.text := inttostr(lp)+' / '+inttostr(lh);
  if (mode2>2) then form1.paintbox2.refresh;
end;

{------------------------------------------------------------------------------}

procedure TForm1.PaintBox3Paint(Sender: TObject);
 {this procedure plots various objects in 3D-polar co-ordinates}
var i,d : integer;
    y,z:single;
begin
  if checkbox8.checked then d := 5 else d := 0;
  if checkbox10.checked then xycleargraph(paintbox3,clblack,clblack,9/8)
     else xycleargraph(paintbox3,clsilver,clblack,9/8);

  {this plots the objects in the cylindrical plot}
  xystartgraph(0,50,5,100,40,40,40,40,clipon);
  xy3dcylframe(1,2,0,2,-1,form1.spinedit12.value/100,false,0);
  xy3dsetsun(form1.spinedit3.value,form1.spinedit4.value,0.5,2);
  xy3dsetview(form1.spinedit10.value,form1.spinedit11.value,d,0,0,0,0,0,0);
  xy3dsetlabels('','','','','','',true,0,0,1,0,1+2);
  if checkbox10.checked then xy3dsetstereo(4,0,0,0);
  xy3dshowframe(spinedit9.value,clblack,clyellow,clyellow,clyellow,0);
  if (spinedit11.value>0) then
    begin
      for i := 0 to 5 do
        xy3dsymbol(1.8,i*60,0.3,1,0,0,clblue,lightoff,opaque,0);
      xy3dcircle(point3d(0,0,0.6),point3d(0,0,0),1.5,6,0,clred,lightoff,opaque,1);
      xy3dcylinder(point3d(0,0,1.0),point3d(0,0,1.6),1.6,1.3,6,0,claqua,-1,-1,lighton,opaque,1);
    end
  else
    begin
      xy3dcylinder(point3d(0,0,1.0),point3d(0,0,1.6),1.6,1.3,6,0,claqua,-1,-1,true,opaque,1);
      xy3dcircle(point3d(0,0,0.6),point3d(0,0,0),1.5,6,0,clred,lightoff,opaque,1);
      for i := 0 to 5 do
        xy3dsymbol(1.8,i*60,0.3,1,0,0,clblue,lightoff,opaque,0);
    end;
  xytitle(clnavy,'CYLINDRICAL');

  {this plots the globe-type object in the spherical plot}
  xystartgraph(50,100,5,100,40,40,40,40,clipon);
  xy3dspherframe(0,1,form1.spinedit12.value/100,false,0);
  xy3dsetview(form1.spinedit10.value,form1.spinedit11.value,d,0,0,0,0,0,0);
  if checkbox10.checked then xy3dsetstereo(4,0,0,0);
  xy3dshowframe(spinedit9.value,clblack,clwhite,clwhite,clwhite,0);
  for i := 0 to 11 do
    xy3dcircle(point3d(0,0,0),point3d(0,i*15,0),4,0,clblack,-1,false,false,0);
  for i := -5 to 5 do
    xy3dcircle(point3d(0,0,(i-0)*15),point3d(0,0,90),4,0,clblack,-1,false,false,0);
  xylinewidth(2); xy3dmove(1,0,-90);
  for i := 1 to 90 do
    begin y := i*15; z := -90+i*2;
    if xy3dinfront(1,y-7.5,z-1) then xypen.color := clred else xypen.color := clmaroon;
    xy3ddraw(1,y,z);
    end;
  xytitle(clnavy,'SPHERICAL');
  xytitle(clmaroon,'@3D POLAR CO-ORDINATES');
end;

{------------------------------------------------------------------------------}

procedure TForm1.PaintBox4Paint(Sender: TObject);
 {this procedure shows the 4D plots}
var ok : boolean;
    a,b,c,d : single;
const zp : Tpoint3d = (x:0;y:0;z:0);
begin
  xycleargraph(paintbox4,$f0ffff,clblack,8/8);
  xyset(3,1);
  if (elements.itemindex=0) then xy4dloadvolume(data4da,ok,0)
                            else xy4dloadvolume(data4db,ok,0);
  xy4dsetscale(0,0,colorscheme[2],16,true,true);

  xystartgraph(55,100,0,100,20,20,20,20,clipon);
  xy3dsetframe(-2.5,2.5,-2.5,2.5,-2.5,2.5,1,1,1.2,0);
  xy3dsetview(form1.spinedit15.value,form1.spinedit16.value,4,0,0,0,0,0,0);
  xy3dshowframe(4,clmaroon,-1,-1,-1,0);
  case radiogroup2.itemindex of
    0 : xy4dshowvolume(point3d(2,2,2),point3d(-2,-2,-2),point3d(0,0,0),
        0,0,clwhite,clsilver,clgray,spinedit18.value,3,4);
    1: xy4dsectvolume(1,1.2,-1,-0.3,zp,zp,
        0,clwhite,clsilver,clgray,spinedit18.value,2+4);
    2: xy4disovolume(0.37,point3d(0,-3,-3),point3d(3,3,3),clgray,-1,
        spinedit18.value,true,false,2+64);
  end;

  xystartgraph(10,55,0,100,20,20,20,20,clipon);
  xy3dsetframe(-2.5,2.5,-2.5,2.5,-2.5,2.5,1,1,1.2,0);
  xy3dsetview(form1.spinedit15.value,form1.spinedit16.value,4,0);
  xy3dshowframe(4,clmaroon,-1,-1,-1,0);
  case radiogroup2.itemindex of
   0 : xy4dshowvolume(point3d(-2,-2,2),point3d(0.25,0.50,-2),point3d(0,0,0),
       4,0,clwhite,clsilver,clgray,spinedit18.value,3,2+4);
   1 : begin xy4dcalcplane(point3d(-1,1,0),point3d(-1,1,1),point3d(0,0,0),a,b,c,d,ok);
       xy4dsectvolume(a,b,c,d,zp,zp,
        0,clwhite,clsilver,clgray,spinedit18.value,2+4); end;
   2 : xy4disovolume(0.15,zp,zp,clgray,-1,
        spinedit18.value,true,false,2+64);
  end;

  xy4dheightscale(10,10,20,paintbox4.height-10,'',0);
  xytitle(clmaroon,'@4D PLOT DEMO');
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure TForm1.DisplayHint(Sender: TObject);
begin
  StatusBar1.SimpleText := Application.Hint;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i,j,n,x,y,z : integer;
const l2 = 9; n2 = l2*2; f = 1.5;
begin
  Application.OnHint := DisplayHint;
  hoek1 := 0; mode1 := 6;
  hoek2 := 110; mode2 := 4;
  cmode := 0;
  updown2.position := 0;
  updown3.position := 0;
  spinedit7.value := mode1;
  spinedit8.value := mode2;
  button3.enabled := false;
  radiogroup1.itemindex := 1;
  radiogroup2.itemindex := 0;
  scrollbar1.position := 3;
  checkbox4.checked := true;
  checkbox5.checked := false;
  lh := 0; lp := 105; setlight;
  timer1.interval := timerinterval;

  xy3dsetdataarray(data1,n2-1,n2-1);
  xy3dsetdataarray(datac,n2-1,n2-1);
  for i := 1 to l2 do
    begin data1[i,0] := -dataA[l2+1-i]; data1[i+l2-1,0] := dataA[i]; end;
  for i := 1 to n2-1 do data1[0,i] := data1[i,0];
  for i := 1 to n2-1 do for j := 1 to n2-1 do
     data1[i,j] := exp(-(sqr(data1[i,0])+sqr(data1[0,j])*2));
  for i := 1 to n2-1 do for j := 1 to n2-1 do datac[i,j] := (i+j)/1;

  xy3dsetdataarray(data2,5,5);
  for i := 0 to 5 do for j := 0 to 5 do data2[i,j] := dataB[i,j];

  randomize;
  for i := 1 to 4 do for j := 1 to 10 do
    data0[i,j] := i*1.5 + random * 1.5;
  for j := 1 to 10 do data0[0,j] := 1 + random * 1;

  xy3dsetdataarray(data3,10,2);
  for j := 1 to 2 do for i := 1 to 10 do data3[i,j] := data0[4,i];

  n := length(data4a); xy4dsetarray(data4da,n,n,n);
  for i := 1 to n do  begin data4da[i,0,0] := data4a[i];
   data4da[0,i,0] := data4a[i]; data4da[0,0,i] := data4a[i]; end;
  for x := 1 to n do for y := 1 to n do for z := 1 to n do data4da[x,y,z] :=
    exp((-sqr(data4da[x,0,0])-sqr(data4da[0,y,0])*f-sqr(data4da[0,0,z])/f)/4);
  n := length(data4b); xy4dsetarray(data4db,n,n,n);
  for i := 1 to n do  begin data4db[i,0,0] := data4b[i];
   data4db[0,i,0] := data4b[i]; data4db[0,0,i] := data4b[i]; end;
  for x := 1 to n do for y := 1 to n do for z := 1 to n do data4db[x,y,z] :=
    exp((-sqr(data4db[x,0,0])-sqr(data4db[0,y,0])*f-sqr(data4db[0,0,z])/f)/4);

  xyset(0,1);
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if pagecontrol1.activepage<>tabsheet1 then exit;;
  hoek1 := (hoek1 + rotationincrement) mod 360; draw1;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin setlight; end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin paintbox2.refresh; end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin paintbox2.refresh; timer1.enabled := true; end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
 with checkbox7 do if (radiogroup1.itemindex=1) then
   begin checked := false; enabled := false; end else enabled := true;
 paintbox2.refresh; timer1.enabled := true;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin cmode := (cmode + 1) mod 3; paintbox2.refresh; end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin paintbox1.refresh; end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin paintbox1.refresh; end;

procedure TForm1.SpinEdit7Change(Sender: TObject);
begin mode1 := spinedit7.value; end;

procedure TForm1.SpinEdit8Change(Sender: TObject);
begin
  mode2 := spinedit8.value;
  checkbox3.enabled := (mode2 in [2,3]);
  button3.enabled := checkbox3.enabled;
  paintbox2.refresh;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  xysetlinestyle(0,0,0); timer1.enabled := false;
  xygetbuffer(1); xymousedown(button,shift,x,y);
end;

procedure TForm1.PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  xysetlinestyle(0,0,0); timer1.enabled := false;
  xygetbuffer(2); xymousedown(button,shift,x,y);
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin xymousemove(shift,x,y); end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin xymouseup(button,shift,x,y);
 if (button=mbleft) then timer1.enabled := true;
end;

procedure TForm1.PaintBox2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 xymouseup(button,shift,x,y);
 if (button=mbleft) then
  if (ssshift in shift) then draw2
    else timer1.enabled := true;
end;

procedure TForm1.Image2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin timer1.enabled := false; xygetbuffer(2); xymousedown(button,shift,x,y); end;

procedure TForm1.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
  if (button=btnext) then lp := lp + 5 else lp := lp - 5;
  setlight;
end;

procedure TForm1.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
  if (button=btnext) and (lh<90) then  begin lh := lh + 5; setlight; end;
  if (button=btprev) and (lh>-90) then begin lh := lh - 5; setlight; end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin timer1.enabled := false; xycopystart; end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin if checkbox5.checked then checkbox6.checked := false; end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin if checkbox6.checked then checkbox5.checked := false; end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  timer1.enabled := true;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  timer1.enabled := (pagecontrol1.activepage=tabsheet1);
  xyclearbuffer;
end;

procedure TForm1.SpinEdit9Change(Sender: TObject);
begin paintbox3.refresh; end;

procedure TForm1.SpinEdit10Change(Sender: TObject);
begin
  with spinedit10 do if (value<0) then value := 355 else
   if (value=360) then value := 0;
  paintbox3.refresh;
end;

procedure TForm1.CheckBox8Click(Sender: TObject);
begin paintbox3.refresh; end;

procedure TForm1.CheckBox9Click(Sender: TObject);
begin paintbox1.refresh; end;

procedure TForm1.CheckBox10Click(Sender: TObject);
begin paintbox3.refresh; end;

procedure TForm1.SpinEdit15Change(Sender: TObject);
begin
  with spinedit15 do if (value<0) then value := 355 else
   if (value=360) then value := 0;
  paintbox4.refresh;
end;

procedure TForm1.SpinEdit16Change(Sender: TObject);
begin paintbox4.refresh; end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin paintbox4.refresh; end;

end.
