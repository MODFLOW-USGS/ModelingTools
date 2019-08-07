unit xygraph3d;

{=========================================================================}
{ See Copyright notes at XYGRAPH.                                         }
{=========================================================================}

interface

{$R+}

uses Windows , {Messages,} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    {StdCtrls, Spin,} ExtCtrls, Clipbrd{, AppEvnts}, xycommon, xygraph;

const lighton = true; lightoff = false;
      spliton = false; splitoff = true;
      detail = false; fast = true;                         
      shell = false; solid = true;
      opaque = false; transp = true;
     {normal = false; reverse = true; --> XYGraph}
      flat = false; txt3d = true;

type Tfacetype = record point1, point2, point3 : Tpoint3D;
                  mode : integer;  color : Tcolor; end;
     Tcontpoint = record x,y,z,l : single; end;
     Tsimplefunction2type = function(x,y:single):single;

var  xy3dviewx,xy3dviewy,xy3dviewz,xy3dviewr,xy3dviewa,xy3dviewh : single;
     xy3dcrosssection : array of Tcontpoint;

procedure dummy;
function point3D(x,y,z:single):Tpoint3D;

procedure xy3dloadpolyhedron(var ph:array of Tfacetype);
procedure xy3dshowpolyhedron(xoff,yoff,zoff,fac:single;frcol:Tcolor;
  light,trans:boolean);

procedure xy3dloadsurface(var dat:Tdatatype;var ok : boolean;mode,opt:integer);
procedure xy3dcreatesurface(func:Tsimplefunction2type; xmin,xmax : single; nx:integer;
  ymin,ymax:single; ny:integer; var data:Tdatatype);
procedure xy3dshowsurface(mode:integer;dikte:single;col1,col2,col3:Tcolor;
   solid,nosplit:boolean;fcol,just,opt:integer); overload;

procedure xy3dplotcluster(var dat:Tdatatype;mode,st,sz:integer;col1,col2:Tcolor;opt:integer);

procedure xy3daxisscale(min,max:single;nc:integer;fm:single;fix:boolean;ax:integer);
procedure xy3dsetframe(x1,x2,y1,y2,z1,z2,yfac,zfac,scale:single;opt:integer);
procedure xy3dcylframe(r1,r2,z1,z2,zfac,scale:single;rev:boolean;opt:integer);
procedure xy3dspherframe(r1,r2,scale:single;rev:boolean;opt:integer);
procedure xy3dsetview(pos,height:integer;dis:single;xoff,yoff:integer; {XXXXXXX}
   xc,yc,zc:single;opt:integer); overload;
procedure xy3dsetstereo(ang,phase:single;grcol,opt:integer); overload;
procedure xy3dsetlabels(xl,yl,zl,xrl,yrl,zrl:string;lbl3d:boolean;
   xm,ym,zm:integer;scl3d:single;opt:integer); overload;
procedure xy3dshowframe(mode:integer;frcol,xycol,xzcol,yzcol:Tcolor;opt:integer);
procedure xy3dcloseframe;
procedure xy3dsetsun(pos,height:integer;contrast:single;mode:integer);
procedure xy3dmove(x,y,z:single);
procedure xy3ddraw(x,y,z:single);
procedure xy3dbar(p1,p2:Tpoint3D;frcol,cx1,cx2,cy1,cy2,cz1,cz2:Tcolor;
  light,trans:boolean);
procedure xy3dtriangle(p1,p2,p3:Tpoint3D;col1,col2:Tcolor;light,trans:boolean);
procedure xy3dquad(p1,p2,p3:Tpoint3D;col1,col2:Tcolor;light,trans:boolean);
procedure xy3dsymbol(x,y,z:single;st,sz:integer;col1,col2:Tcolor;
  light,trans:boolean;opt:integer);
procedure xy3dusertoabs(x,y,z:single;var xp,yp:integer);
procedure xy3dpolygon(col1,col2:Tcolor;poly:array of Tpoint3D;
     light,trans:boolean);
procedure xy3dsettransmode(mode:integer;level:single;opt:integer);
function xy3dinfront(x,y,z:single):boolean;

procedure xy3dsetcolors(cs:array of Tcolor); overload;
procedure xy3dsetcolors(cs:array of Tcolor;cmin,cmax:single;res:integer;rev,log:boolean); overload;
procedure xy3dshowcontour(mode:integer;col1,col2:Tcolor; xtxt,ytxt : string;
  yfac,hmin,hmax,hfac:single; hm,cr:integer;rev,nospl,fast:boolean;
  fresh:Tprocedure;opt:integer); overload;
procedure xy3dshowcontour(mode:integer;col1,col2:Tcolor; xtxt,ytxt : string;
  yfac,hmin,hmax,hfac:single; hm,cr:integer;rev,nospl,fast:boolean;
  opt:integer); overload;
procedure xy3dheightscale(x1,y1,x2,y2:integer;txt:string;opt:integer);
procedure xy3dheightline(h:single;col:Tcolor;opt:integer);
procedure xy3dshowgrid(col:Tcolor;size,mode:integer);
procedure xy3dsetcrosssection(np,opt:integer);

procedure xy3dinitruler(cl:Tcolor;xp,yp,j:integer);

procedure xy3dcircle(p1,p2:Tpoint3D;r:single;np:integer;
  col1,col2:Tcolor;light,trans:boolean;opt:integer);
procedure xy3dcylinder(p1,p2:Tpoint3D;ra1,ra2:single;np:integer;
  colfr,colbo,colf1,colf2:Tcolor;light,trans:boolean;opt:integer);

{the following procedures are needed for communication with XYGRAPH;
  do not call them directly}

procedure zoomcontour;
procedure makecrosssect(x1,y1,x2,y2:single;np:integer;var ok:boolean);
procedure copyzoom3d(n:integer;x0,x1,y0,y1:single);
procedure init3d; procedure start3d;
function rul3dhoogte(xw,yw:single):single;
function checkrul3d:boolean;
procedure xyclearbuffer3d(n:integer);
procedure xyputbuffer3d(n:integer);
procedure xygetbuffer3d(n:integer);
procedure initxygraph3d;

{the following procedures are needed for communication with XYGRAPH4D;
  do not call them directly}

function color3(p1,p2,p3:Tpoint3D;col:Tcolor):integer;
procedure setstereo(st:integer); 

{for backward compatability}

procedure xy3dsetstereo(ang,phase:single;grcol:integer); overload;
procedure xy3dsetdataarray(var data:Tdatatype;nx,ny:integer);
procedure xy3dsetlabels(xl,yl,zl,xrl,yrl,zrl:string;lbl3d:boolean;
   xm,ym,zm:integer;opt:integer); overload;
procedure xy3dshowsurface(mode:integer;dikte:single;col1,col2,col3:Tcolor;
   solid,nosplit:boolean;just,opt:integer); overload;
procedure xy3dsetview(pos,height:integer;dis:single;opt:integer); overload;

implementation

uses math;

var xmi,xma,xfr : single;         {waarden X-as}
    ymi,yma,yfr : single;         {waarden Y-as}
    zmi,zma,zfr : single;         {waarden Z-as}
    rmi,rma,rmix,rmax : single;   {waarden R-as}
    cmi,cma,lcmi,lcma: single;    {waarden kleurenschaal}
    polrev : boolean;             {cylinder hoek omgekeerd}
    sphcor: boolean;              {spher. coordinaten correct}
    xp1,xp2,yp1,yp2:integer;      {scherm einden}
    nx1,nx2,ny1,ny2 : integer;    {bereik cellen bij contour}
    diam : single;                {halve diagonaal lengte}
    imsize : integer;             {grootte beeld in pixels}
    oct : integer;                {octant van kijkrichting}
    ok3d, okfr, frame, scale, surf, polyh: boolean;
    endframe: array[1..2,0..8] of integer; {waardes frame sluiting}
    csxx,csyy,cszz : single;      {richtings cosinussen licht}
    csxk,csyk,cszk : single;      {richtings cosinussen kijk}
    licht : array[0..255] of byte;{licht waardes afh contrast}
    dead : integer;               {dead point view}
    drmode : integer;             {mode voor driehoekjes}
    nx,ny,nxc,nyc : integer;      {aantallen punten in opp, midden punten}
    flatno : boolean;             {vlakken niet splitsen}
    rul3d : boolean;              {3d ruler}
    hasbmp : boolean;             {er is een bitmap}
    lastx, lasty, lastz : single; {laatste coordinaten}
    transoff : boolean;           {transparent off}
    ncrosssect : integer;         {aantal data points}
    crosssectopt : integer;       {opties voor doorsnede}
    si15,co15 : array[0..24] of single; {sin en cos per 15 gr}
    xlabel, ylabel, zlabel : string;    {frame labels}
    xrlabel, yrlabel, zrlabel : string; {reverse frame labels}
    xlmode, ylmode, zlmode : integer; {label mode}
    zlvert : boolean;             {z label verticaal}
    nostlabels : boolean;         {geen labels bij stereo}
    labels3d : boolean;           {labels in echte 3d}
    size3d : single;              {schaalfactor 3d text}
    ticx, ticy, ticz : single;    {tics in user coord}
    ticx2, ticy2, ticz2 : single; {tics in user coord}

    stereoang : array[0..3] of record sp,cp,sh,ch,sr,cr : single; xc : integer; end; {kijkhoeken}
    stereopos : array[1..3] of record sxp,syp : integer; end; {posities}
    stdx : single;

    point3 : array[0..2] of Tpoint;
    point4 : array[0..3] of Tpoint;
    point5 : array[0..4] of Tpoint;

var  datap,colsp,crsdp : datapointer;         {pointer naar data}

type dimtype = record xmi,xma,ymi,yma,zmi,zma : single; end;

var  srf,crs : dimtype;

type buftype3d = record  {totale grafiek}
                  rl3d,bmp,m3d : boolean;
                  graphdat3d : grafstype3d;
               end;

var buffer3d : array of buftype3d;
    q : integer;

type phface = record n1,tel,col:integer; xc,yc,zc : single; end;
var  polyhpoint : array of Tpoint3D;
     polyhface : array of phface;

type contcalltype = record
                     ccmode:integer;cccol1,cccol2:Tcolor;
                     ccxtxt,ccytxt : string;
                     ccyfac,cchmin,cchmax,cchfac:single;
                     cchm,cccr:integer;
                     ccrev,ccnospl,ccfast:boolean;
                     ccfresh:Tprocedure;
                     ccopt:integer;
                     {--------------}
                     ccxp1,ccxp2,ccyp1,ccyp2:integer;
                    end;
var contcall : contcalltype;

const grtorad : single = pi/180;

procedure dummy; begin end; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

function point3D(x,y,z:single):Tpoint3D; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var p : Tpoint3D;
begin p.x := x; p.y := y; p.z := z; result := p; end;

procedure start3d; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
begin
  xy3dsetlabels('X','Y','Z','-X','-Y','-Z',false,0,0,0,0,0);
  for i := 1 to 4 do fs3d[i].go := false;
end;

procedure init3d; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
begin
  mode3d := false; ok3d := false; okfr := false; frame := false;
  scale := false; surf := false; rul3d := false; polyh := false;
  hasbmp := false; datap := nil; colsp := nil; vol := false; 
  for i := 1 to maxgraf do with graphs3d[i] do
    begin dp := nil; dp4d := nil; f3dsize := 0;
       scl := false; cnt := false; isbmp := false; bmp4d := false;
       z1 := 0; z2 := 0; c1 := 0; c2 := 0;
       sf := false; m3d := false; vl := false;
       xs1 := 0; xs2 := 0; ys1 := 0; ys2 := 0;
       xc1 := 0; xc2 := 0; yc1 := 0; yc2 := 0;
    end;
  transmode := 1; transval := 0.5; transoff := false;
  stereo := false; labels3d := false; size3d := 1;
end;

procedure poltocart(var x,y:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var r,h : single;
    si,co:extended;
begin
  r := (x-rmi)/(rma-rmi); if (r<0) then r := 0;
  h := y*grtorad; if polrev then h := -h;
  sincos(h,si,co); x := r*co; y := r*si;
end;

procedure sphtocart(var x,y,z:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var r,h1,h2 : single;
    s1,c1,s2,c2:extended;
begin
  r := (x-rmi)/(rma-rmi); if (r<0) then r := 0;
  if sphcor then begin h1 := z; h2 := 90-y; end
            else begin h1 := y; h2 := z; end;
  h1 := (h1*grtorad); if polrev then h1 := -h1;
  if (h2<-90) then h2 := 90 else if (h2>90) then h2 := 90;
  h2 := (h2*grtorad); sincos(h1,s1,c1); sincos(h2,s2,c2);
  z := r*s2; x := r*c1*c2; y := r*s1*c2;
end;

procedure position(x,y,z:single;var xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXX}
var x1,x2,x3,y1,y2,y3,f,ff : single; {NB: kopie in 4D, aangepast}
const mi : integer = -32760; ma : integer = 32760;
begin
  if (polartype=3) then poltocart(x,y) else
  if (polartype=4) then sphtocart(x,y,z);
  x := (x-xmid)*facx; y := (y-ymid)*facy; z := (z-zmid)*facz;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp; {draaiing om z-as}
  x2 := x1; y2 := y1*sina + z*cosa;             {draaiing om as in xy vlak}
  x3 := x2*cosr - y2*sinr; y3 := x2*sinr + y2*cosr; {draaiing om z-as}
  if (r3d=0) then ff := fac3d else
    ff := fac3d*r3d/(r3d+y1*cosa-z*sina);       {factor voor perspectief}
  if zoomc then f := scale3d else f := 1;
  xp := round(xc0+(xpc-xc0)*f+ff*x3);
  yp := round(yc0+(ypc-yc0)*f-ff*y3);
  if (xp<mi) then xp := mi else if (xp>ma) then xp := ma;
  if (yp<mi) then yp := mi else if (yp>ma) then yp := ma;
end;

function factor(x,y,z:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {berekent vergrotingsfactor}
var y1 : single;
begin
  if (r3d=0) then result := 1 else
    begin
       x := (x-xmid)*facx; y := (y-ymid)*facy;
       z := (z-zmid)*facz; y1 := x*sinp + y*cosp;
       result := r3d/(r3d+y1*cosa-z*sina);
    end;
end;

procedure line3d(x1,y1,z1,x2,y2,z2:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xp1,yp1,xp2,yp2:integer; {NB: kopie in 4D}
begin
  position(x1,y1,z1,xp1,yp1); position(x2,y2,z2,xp2,yp2);
  xyline(-1,xp1,yp1,xp2,yp2);
end;

function hcolor(h:single;col:Tcolor):Tcolor; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var t : integer;
begin
  t := round(h*127.5+127.5);
  if (t<0) then t := 0 else if (t>255) then t := 255;
  t := licht[t];
  case zonmode of
    1 : result := mixcolor(col,0,t,255-t);
    2 : if (t>127) then result := mixcolor(col,$ffffff,255-t,t-128)
                   else result := mixcolor(col,0,      t,    127-t);
  else result := col;
  end;
end;

function topoint(x,y,z:single):Tpoint; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xp,yp : integer; {NB: kopie in 4D}
begin position(x,y,z,xp,yp); result := point(xp,yp); end;

function topoint2(p:Tpoint3D):Tpoint; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xp,yp : integer; {NB: koie in 4D}
begin position(p.x,p.y,p.z,xp,yp); result := point(xp,yp); end;

procedure rectxy(x1,y1,x2,y2,z:single;col1,col2:Tcolor;light,trans:boolean); {X}
var h : single;
begin
  point4[0] := topoint(x1,y1,z); point4[1] := topoint(x1,y2,z);
  point4[2] := topoint(x2,y2,z); point4[3] := topoint(x2,y1,z);
  h := cszz; if (viewz<z) then h := -h;
  if (light) and (zonmode>0) then col2 := hcolor(h,col2);
  drawpoly(col1,col2,point4,trans);
end;

procedure rectxz(x1,z1,x2,z2,y:single;col1,col2:Tcolor;light,trans:boolean);{XX}
var h : single;
begin
  point4[0] := topoint(x1,y,z1); point4[1] := topoint(x1,y,z2);
  point4[2] := topoint(x2,y,z2); point4[3] := topoint(x2,y,z1);
  h := csyy; if (viewy<y) then h := -h;
  if (light) and (zonmode>0) then col2 := hcolor(h,col2);
  drawpoly(col1,col2,point4,trans);
end;

procedure rectyz(y1,z1,y2,z2,x:single;col1,col2:Tcolor;light,trans:boolean); {XX}
var h : single;
begin
  point4[0] := topoint(x,y1,z1); point4[1] := topoint(x,y2,z1);
  point4[2] := topoint(x,y2,z2); point4[3] := topoint(x,y1,z2);
  h := csxx; if (viewx<x) then h := -h;
  if (light) and (zonmode>0) then col2 := hcolor(h,col2);
  drawpoly(col1,col2,point4,trans);
end;

function slope(x2,y2,x1,y1:integer):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {berekent helling van lijn tussen twee punten}
begin
  if (x2=x1) then if y2>y1 then result := -90 else result := 90
  else result := round(arctan2(y1-y2,x2-x1)*180/pi);
end;

procedure text1(s:string;x1,y1,x2,y2,d,h,m : integer); {XXXXXXXXXXXXXXXXXXXXXXX}
  {tekent as namen (captions), d=lengte tic; h=helling; m=mode}
var x0,y0,xj : integer;
    l,f : single;
begin
  if (s='') then exit;
  l := sqrt(sqr(x2-x1) + sqr(y2-y1)); if (l=0) then f := 0 else f := d/l;
  x0 := x1 - round(f*(x2-x1)); y0 := y1 - round(f*(y2-y1));
  if (m=4) then {zlabel in kolom}
     begin
       h := h+90; if (h>90) or (h<-90) then h := h + 180;
       bigtext(-1,s,x0,y0,0,0,0,h,2);
     end
  else if (m=2) then bigtext(-1,s,x0,y0,0,0,0,h,0) {schuin}
  else
  with xycanvas do
    begin
      if (m=0) or (x1=x2) then xj := 0
      else if (x1<x2) then xj := -1 else xj := 1;
      simpletext(-1,s,x0,y0,xj,0);
    end;
end;

procedure text2(s:string;x1,y1,x2,y2,d,h,m : integer;zas:boolean); {XXXXXXXXXXX}
  {tekent as tics en getallen; d=lengte tic; h=helling; m=mode}
  {m+8=geen lijn}
var x0,y0,tw,th,tw2,th2,dd : integer;
    l,f,slope : single;
begin
  l := sqrt(sqr(x2-x1) + sqr(y2-y1)); if (l=0) then f := 0 else f := d/l;
  x0 := x1 - round(f*(x2-x1)); y0 := y1 - round(f*(y2-y1));
  if (m<8) then xyline(-1,x0,y0,x1,y1); m := m and 7;

  if (s='') then exit;

  s := ' '+s+' ';
  tw := xycanvas.textwidth(s); th := xycanvas.textheight(s);
  tw2 := tw div 2; th2 := th div 2; slope := th/tw;

  if (m=2) then {schuin}
    if (kijkhoogte>=0) or zas then bigtext(-1,s,x0,y0,0, 1,0,h,0)
                              else bigtext(-1,s,x0,y0,0,-1,0,h,0)
  else
  with xycanvas do begin

  if zas then
    begin
      if (x2>x1) then simpletext(-1,s,x0,y0,-1,0)
                 else simpletext(-1,s,x0,y0, 1,0);
    end
  else
    begin
      if (l=0) then {ontaard}
        begin x0 := x0; y0 := y0; end
      else
      if (abs(y2-y1)>abs(x2-x1)*slope) then
        begin
          dd := round(abs(th2*(x2-x1)/(y2-y1)));
          if (x2>x1) then x0 := x0-dd  else x0 := x0+dd;
          if (y2>y1) then y0 := y0-th2 else y0 := y0+th2;
        end
      else
        begin
          dd := round(abs(tw2*(y2-y1)/(x2-x1)));
          if (y2>y1) then y0 := y0-dd  else y0 := y0+dd;
          if (x2>x1) then x0 := x0-tw2 else x0 := x0+tw2;
        end;
      simpletext(-1,s,x0,y0,0,0);
    end;
  end;
end;

procedure text3(s:string;x1,y1,x2,y2,d,m:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXX}
  {tekent namen in verlengde van lijn}
var x0,y0,h : integer;
    l,f : single;
begin
  if (s='') then exit;
  if (m<2) then begin text1(s,x1,y1,x2,y2,d,0,m); exit; end;
  d := d - xycanvas.textwidth('0');
  l := sqrt(sqr(x2-x1) + sqr(y2-y1)); if (l=0) then f := 0 else f := d/l;
  x0 := x1 - round(f*(x2-x1)); y0 := y1 - round(f*(y2-y1));
  h := slope(x2,y2,x1,y1); if (h>90) or (h<-90) then h := h + 180;
  if (x1>x2) then bigtext(-1,s,x0,y0, 1,0,0,h,0)
             else bigtext(-1,s,x0,y0,-1,0,0,h,0)
end;

procedure text3z(s:string;x1,y1,x2,y2,d:integer;doline:boolean); {XXXXXXXXXXXXX}
  {speciale versie voor z-as met tic}
var x0,y0,h : integer;
    l,f : single;
begin
  l := sqrt(sqr(x2-x1) + sqr(y2-y1)); if (l=0) then f := 0 else f := d/l;
  x0 := x1 - round(f*(x2-x1)); y0 := y1 - round(f*(y2-y1));
  if doline then xyline(-1,x0,y0,x1,y1);
  if (s='') then exit;
  h := slope(x2,y2,x1,y1); if (h>90) or (h<-90) then h := h + 180;
  if (x1>x2) then bigtext(-1,s,x0,y0, 1,0,0,h,0)
             else bigtext(-1,s,x0,y0,-1,0,0,h,0)
end;

function color3(p1,p2,p3:Tpoint3D;col:Tcolor):integer; {XXXXXXXXXXXXXXXXXXXXXXX}
var pxy,pyz,pzx,mx,my,mz,t,h1,h2,dx,dy,dz,l,cx,cy,cz:single;
    x0,y0,z0,x1,x2,y1,y2,z1,z2:single;
 { berekent kleur van vlak door 3 punten }   
 { vlak: mx(x-x0)+my(y-y0)+mz(z-z0)=0; normeren naar O -> }
 { mx.x + my.y + mz.x = 0, mi = ri.cosinussen }
 { vul in: door twee punten x1,y1,z1 en x2,y2,z2 en los op }
begin
  if (col<0) or (zonmode=0) then begin result := col; exit; end;
  if (polartype=3) then
    begin poltocart(p1.x,p1.y); poltocart(p2.x,p2.y); poltocart(p3.x,p3.y); end;
  if (polartype=4) then begin sphtocart(p1.x,p1.y,p1.z);
     sphtocart(p2.x,p2.y,p2.z); sphtocart(p3.x,p3.y,p3.z); end;

  x0 := (p1.x+p2.x+p3.x)/3; y0 := (p1.y+p2.y+p3.y)/3; z0 := (p1.z+p2.z+p3.z)/3;

  x1 := p1.x-p3.x; y1 := p1.y-p3.y; z1 := p1.z-p3.z;
  x2 := p2.x-p3.x; y2 := p2.y-p3.y; z2 := p2.z-p3.z;
  pxy := x1*y2-x2*y1; pyz := y1*z2-y2*z1; pzx := z1*x2-z2*x1;
  mx := pyz; my := pzx; mz := pxy;
  t := sqrt(sqr(mx)+sqr(my)+sqr(mz));
  if (t=0) then begin result := col; exit; end;
  mx := mx/t; my := my/t; mz := mz/t;
  h1 := mx*csxx + my*csyy + mz*cszz;

  if (r3d=0) then {kijkhoek is altijd vast}
    begin
      h2 := mx*csxk + my*csyk + mz*cszk;
      if (h2<0) then h1 := -h1;
    end
  else
    begin {bereken echte kijkhoek}
      dx := (viewx-x0)*facx; dy := (viewy-y0)*facy;
      dz := (viewz-z0)*facz;
      l := sqrt(sqr(dx)+sqr(dy)+sqr(dz));
      cz := dz/l; cy := dy/l; cx := dx/l;
      h2 := mx*cx+my*cy+mz*cz;
      if (h2<0) then h1 := -h1;
    end;
  result := hcolor(h1,col);
end;

procedure setstereo(st:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with stereoang[st] do
    begin sinp := sp; cosp := cp; sina := sh; cosa := ch;
     sinr := sr; cosr := cr;  xpc := xc; end;
  xypen.color := stereocol[st]; stereomode := st;
  if dowmf then
    begin
      if (st=0) then rop2wmf(r2_copypen)
      else if (st=1) then rop2wmf(r2_mergepen);
      if (st=0) then checkwmfpen
      else if (st=1) then checkwmfrgpen else wmfgrnpen;
    end
  else
    if (st>0) then setrop2(xycanvas.handle,r2_mergepen)
      else setrop2(xycanvas.handle,r2_copypen);
  linestyle := 0;
end;

procedure xylinewidth(w:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{copie van procedure ivm stereo}
begin
  if (w>0) then xypen.width := w*res;
  if stereo then
    if dowmf then
       begin rop2wmf(r2_mergepen); {checkwmfpen;} end
    else
      setrop2(xycanvas.handle,r2_mergepen);
end;

{==============================================================================}
{======== Procedures voor polyhedra ===========================================}
{==============================================================================}

procedure xy3dloadpolyhedron(var ph:array of Tfacetype); {XXXXXXXXXXXXXXXXXXXXX}
var n,i,j,np,nf : integer;
begin
  n := length(ph); if (n=0) then exit;
  setlength(polyhpoint,0); setlength(polyhface,0);
  np := 0; nf := 0; polyh := false;
  for i := 0 to n-1 do with ph[i] do
    begin
      if (mode<0) or (mode>4) then mode := 0;
      if (mode in [0,4]) then
        begin
          inc(nf); setlength(polyhface,nf);
          with polyhface[nf-1] do
            begin col := color; n1 := np; tel := 3; end;
          inc(np,3); setlength(polyhpoint,np);
          polyhpoint[np-3] := point1; polyhpoint[np-2] := point2;
          polyhpoint[np-1] := point3;
        end;
      if (mode=4) then
        begin
          inc(np); setlength(polyhpoint,np); inc(polyhface[nf-1].tel);
          with polyhpoint[np-1] do
              begin x := point1.x+point3.x-point2.x;
                    y := point1.y+point3.y-point2.y;
                    z := point1.z+point3.z-point2.z; end;
        end;
      if (mode in [1,2,3]) and (nf>0) then
        begin
          inc(np); setlength(polyhpoint,np); inc(polyhface[nf-1].tel);
          polyhpoint[np-1] := point1;
        end;
      if (mode in [2,3]) and (nf>0) then
        begin
          inc(np); setlength(polyhpoint,np); inc(polyhface[nf-1].tel);
          polyhpoint[np-1] := point2;
        end;
      if (mode = 3) and (nf>0) then
        begin
          inc(np); setlength(polyhpoint,np); inc(polyhface[nf-1].tel);
          polyhpoint[np-1] := point3;
        end;
    end;
  for i := 1 to nf do with polyhface[i-1] do
    begin
       xc := 0; yc := 0; zc := 0;
       for j := n1 to n1+tel-1 do with polyhpoint[j] do
         begin xc := xc + x; yc := yc + y; zc := zc + z; end;
       xc := xc / tel; yc := yc / tel; zc := zc / tel;
    end;
  polyh := (nf>0);
end;

procedure xy3dshowpolyhedron(xoff,yoff,zoff,fac:single;frcol:Tcolor; {XXXXXXXXX}
      light,trans:boolean);
var poly : array of Tpoint3D;
    dist : array of double;
    order : array of integer;
    i,j,n,t,cl : integer;
    d : double;
begin
  if (polartype>0) then exit;
  cl := -1;
  if not polyh then cl := backcolor else
    for i := length(polyhface)-1 downto 0 do
       with polyhface[i] do if (col>=0) then cl := col;

  with lastsymbol do
   begin xp := 0; yp := 0; cd3d := 30; size := 0; width := xypen.width;
     if (frcol>=0) then color := frcol else if (cl<0) then color := frontcolor
       else color := cl;
     cl1 := cl; cl2 := -1; cl3 := -1;
     if (cl>=0) then fill := 3 else fill := 0;
   end;

  if (not ok3d) or (not polyh) or (fac<0) then exit;

  n := length(polyhface); setlength(dist,n); setlength(order,n);
  for i := 0 to n-1 do with polyhface[i] do {bepaal afstanden}
    dist[i] := sqrt( sqr(viewx-xc) + sqr(viewy-yc) + sqr(viewz-zc) );

  for i := 0 to n-1 do order[i] := i;       {sorteer op afstand}
  for i := 1 to n-1 do
    begin t := order[i]; d := dist[t]; j := i;
      while (j>0) and (dist[order[j-1]]<d) do
             begin order[j] := order[j-1]; dec(j); end;
       order[j] := t;
    end;

  if (fac=0) then fac := 1;
  for i := 0 to n-1 do with polyhface[order[i]] do
    begin
      setlength(poly,tel);
      for j := 0 to tel-1 do
        begin
          poly[j].x := polyhpoint[j+n1].x*fac+xoff;
          poly[j].y := polyhpoint[j+n1].y*fac+yoff;
          poly[j].z := polyhpoint[j+n1].z*fac+zoff;
        end;
      xy3dpolygon(frcol,col,poly,light,trans);
    end;
end;

{==============================================================================}
{======== Procedures voor kleur weergave ======================================}
{==============================================================================}

var contzlog : boolean;
    colfac : single;

procedure setcolorrange(mi,ma:single;log:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXX}
var t : single;
begin
  if (mi<>0) or (ma<>0) then begin cmi := mi; cma := ma; end;
  if (cmi>cma) then begin t := cma; cma := cmi; cmi := t; end;
  contzlog := log; if (cmi<=0) or (cma<0) then contzlog := false;
  if contzlog then
    begin if (cma=cmi) then begin cmi := cmi/1.4; cma := cmi*2; end;
      lcmi := ln(cmi); lcma := ln(cma); colfac := 255/(lcma-lcmi); end
  else
    begin if (cma=cmi) then begin cmi := cmi-0.5; cma := cmi+1; end;
      colfac := 255/(cma-cmi); end;
end;

procedure setcolorarray(cs : array of Tcolor); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i,nc,ti,tf,cl:integer;  t,f : single;
begin
  nc := length(cs); if (nc<1) then exit;
  {if revcol then for i := 0 to nc-1 do cs[i] := reversecolor(cs[i]);}
  if (nc=1) then
    begin for i := 0 to 255 do colors[i] := cs[0]; exit; end;

  colors[0] := cs[0]; colors[255] := cs[nc-1]; f := (nc-1)/255;
  if (nc>255) then
    for i := 1 to 254 do colors[i] := cs[round(i*f)]
  else
  for i := 1 to 254 do
    begin
      t := i*f; ti := trunc(t); tf := round((t-ti)*256);
      cl := mixcolor(cs[ti],cs[ti+1],256-tf,tf);
      colors[i] := cl;
    end;
  for i := 0 to 255 do revcolors[i] := reversecolor(colors[i]);
end;

{==============================================================================}
{======== Procedures voor surfaces laden ======================================}
{==============================================================================}

procedure xy3dloadsurface(var dat:Tdatatype;var ok : boolean;mode,opt:integer);
   {mode: splitsing in driehoekjes; 0 = nooit, 1=vier
     2=/ 3=\ 4=hoogste 5=laagste 6=->centrum 7=Tcentrum
    opt : opties;
      bit 0 set = niet splitsen vlakke vierkanten
      bit 1 set = sta lege cellen toe
      bit 2 set = rapporteer lege cellen
      bit 3 set = lees als kleurentabel
   }
var i,j,n : integer; t : single; first, cols : boolean;
begin
  cols := (opt and 8)>0;
  if cols then
    begin
     ok := false;
     if not surf then exit; {er was nog geen oppervlak}
     if (length(dat)-1<>nx) or (length(dat[1])-1<>ny) then exit; {verkeerde afmetingen}
     first := true; cmi := 0; cma := 0; {bereken grenzen}
     for i := 1 to nx do for j := 1 to ny do
        begin t := dat[i,j];
          if (t<>empty) then
          if first then
            begin cmi := t; cma := t; first := false; end
          else
            begin if (t<cmi) then cmi := t;
                  if (t>cma) then cma := t; end;
        end;
     setcolorrange(0,0,false);
     colsp := @dat; graphs3d[igraph].cp := colsp;
     ok := true; exit;
    end;

  ok := true; surf := false;
  graphs3d[igraph].dp := nil; graphs3d[igraph].cp := nil;
  nx := length(dat)-1; ny := length(dat[1])-1;
  if (nx<2) or (ny<2) or (mode>7) then ok := false;
  for i := 2 to nx do if (dat[i,0]<=dat[i-1,0]) then ok := false;
  for i := 2 to ny do if (dat[0,i]<=dat[0,i-1]) then ok := false;
  if not ok then exit;

  nxc := 0; nyc := 0; hasempty := false;
  for i := 1 to nx-1 do
    if (dat[i,0]+dat[i+1,0])<(dat[1,0]+dat[nx,0]) then nxc := i;
  for i := 1 to ny-1 do
    if (dat[0,i]+dat[0,i+1])<(dat[0,1]+dat[0,ny]) then nyc := i;
  srf.xmi := dat[1,0]; srf.xma := dat[nx,0];
  srf.ymi := dat[0,1]; srf.yma := dat[0,ny];

  with srf do
  if (opt and 2=0) then {geen lege velden}
    begin
      zmi := dat[1,1]; zma := zmi;
      for i := 1 to nx do for j := 1 to ny do
        begin t := dat[i,j]; if (t<zmi) then zmi := t
           else if (t>zma) then zma := t; end;
      empty := zma+1;
    end
  else {wel lege velden}
    begin
      zmi := 1e38; zma := -1e38; first := true;
      empty := dat[0,0]; n := 0;
      for i := 1 to nx do for j := 1 to ny do
        begin t := dat[i,j];
          if (t=empty) then inc(n) else
          if first then
            begin zmi := t; zma := t; first := false; end
          else
            begin if (t<zmi) then zmi := t;
                  if (t>zma) then zma := t; end;
        end;
      hasempty := (n>0);
      if (opt and 4 >0) then if (n>0) then
        showmessage(inttostr(n)+' of '+inttostr(nx*ny)+' nodes empty');
      if first then begin zmi := 0; zma := 1; end;
    end;

  surf := true;  graphs3d[igraph].sf := surf;
  datap := @dat; graphs3d[igraph].dp := datap;
  colsp := @dat; graphs3d[igraph].cp := colsp;
  drmode := mode; flatno := (opt and 1>0);
  if not ok3d then xy3dsetsun(110,20,1,1);
  with plotfield do with contcall do
    begin ccxp1 := x1; ccxp2 := x2; ccyp1 := y1; ccyp2 := y2; end;
  cmi := srf.zmi; cma := srf.zma; zmi := cmi; zma := cma;
  xy3dsetcolors([clblack,clwhite]);
  with graphs3d[igraph] do with srf do begin
    s1 := xmi; s2 := xma; s3 := ymi; s4 := yma; s5 := zmi; s6 := zma; end;
end;

procedure xy3dsetdataarray(var data:Tdatatype;nx,ny:integer); {XXXXXXXXXXXXXXXX}
begin xysetdataarray(data,nx,ny); end;

procedure xy3dcreatesurface(func:Tsimplefunction2type; xmin,xmax:single; nx:integer;
  ymin,ymax:single; ny:integer; var data:Tdatatype); 
  {maaakt en vult een data array dmv function}
var i,j : integer;
    dx,dy : single;
begin
  if (nx<2) then nx := 2; dx := (xmax-xmin)/(nx-1);
  if (ny<2) then ny := 2; dy := (ymax-ymin)/(ny-1);
  xysetdataarray(data,nx,ny);
  for i := 1 to nx do data[i,0] := xmin + (i-1) * dx;
  for j := 1 to ny do data[0,j] := ymin + (j-1) * dy;
  for i := 1 to nx do for j := 1 to ny do data[i,j] := func(data[i,0],data[0,j]);
end;

{==============================================================================}
{======== Procedures voor contour weergave ====================================}
{==============================================================================}

var cres, hmode, cmode, ccol, ascol : integer;
    crev, nosplit : boolean;
    xps,yps : array of integer;
    lzmi, lzma, ldz : single;
const zoom = 1 shl 16;

function hoogte2(x,y:integer;fx,fy : single):single; {XXXXXXXXXXXXXXXXXXXXXXXXX}
{berekent hoogte in vak x,y met fracties fx,fy}
var h1,h2,h3,h4,h12,h23,h34,h41,h13,h24,hm,h : single;
    n : integer;
begin
  h1 := datap^[x,y];    h2 := datap^[x+1,y];
  h4 := datap^[x,y+1];  h3 := datap^[x+1,y+1];
  if hasempty then
   if (h1=empty) or (h2=empty) or (h3=empty) or (h4=empty) then
    begin result := empty; exit; end;
  if nosplit then begin
    result := h1*(1-fx)*(1-fy) + h4*(1-fx)*(fy) +
              h2*(fx)*(1-fy) + h3*(fx)*(fy); exit; end;

  h12 := (h1+h2)/2; h23 := (h2+h3)/2;
  h34 := (h3+h4)/2; h41 := (h4+h1)/2;
  h13 := (h1+h3)/2; h24 := (h2+h4)/2;

  case drmode of
   2 : hm := h13;          3 : hm := h24;
   4 : hm := max(h13,h24); 5 : hm := min(h13,h24);
   6 : if ((x>nxc) and (y>nxc)) or ((x<=nxc) and (y<=nyc)) then hm := h13 else hm := h24;
   7 : if ((x>nxc) and (y<=nxc)) or ((x<=nxc) and (y>nyc)) then hm := h13 else hm := h24;
  else hm := (h1+h2+h3+h4)/4;
  end;

  n := 1; if (fy>fx) then n := 3; if (fx+fy>1) then inc(n);
  { n = driehoek, 1=o, 2=r 3=l 4=b }
  case n of
   1 : h := h1 + fx*(h2-h1) + fy*2*(hm-h12);
   2 : h := h2 + fy*(h3-h2) - (1-fx)*2*(h23-hm);
   3 : h := h1 + fx*2*(hm-h41) + fy*(h4-h1);
   4 : h := h4 + fx*(h3-h4) - (1-fy)*2*(h34-hm);
  end;
  result := h;
end;

procedure zoekvak(xw,yw:single;var xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
begin
  xp := nx-1; if (xw<datap^[2,0]) then xp := 1 else
  for i := 1 to nx-1 do if (datap^[i,0]<xw) then xp := i;
  yp := ny-1; if (yw<datap^[0,2]) then yp := 1 else
  for i := 1 to ny-1 do if (datap^[0,i]<yw) then yp := i;
end;

function hoogtexy(xw,yw:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{berekent hoogte op coordinaten xw,yw}
var xp,yp,i : integer;
    x1,x2,y1,y2,fx,fy : single;
begin
  with graphs3d[igraph] do
    if not (sf and m3d) then begin result := 0; exit; end;
  {if not (surf and mode3d) then begin result := 0; exit; end;}
  datap := graphs3d[igraph].dp;
  nx := length(datap^)-1; ny := length(datap^[1])-1;
  with srf do begin
  if (xw<xmi) then xw := xmi else if (xw>xma) then xw := xma;
  if (yw<ymi) then yw := ymi else if (yw>yma) then yw := yma; end;
  {zoek welk vak}
  xp := nx-1;
  if (xw<datap^[2,0]) then xp := 1 else
  for i := 1 to nx-1 do if (datap^[i,0]<xw) then xp := i;
  yp := ny-1; if (yw<datap^[0,2]) then yp := 1 else
  for i := 1 to ny-1 do if (datap^[0,i]<yw) then yp := i;

  x1 := datap^[xp,0]; x2 := datap^[xp+1,0];
  y1 := datap^[0,yp]; y2 := datap^[0,yp+1];
  fx := (xw-x1)/(x2-x1); fy := (yw-y1)/(y2-y1);
  result := hoogte2(xp,yp,fx,fy);
end;

function rul3dhoogte(xw,yw:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var h : single;
    y : integer;
begin
  h := hoogtexy(xw,yw);
  if hasempty then if (h=empty) then begin result := empty; exit; end;
  with graphs3d[igraph] do with xycanvas do if scl then
    begin
     if not contzlog then y := ys1 + round((c2-h)/(c2-c1)*(ys2-ys1))
           else y := ys1 + round(ln(c2/h)/ln(c2/c1)*(ys2-ys1));
     if (y<ys1) then y := ys1 else if (y>ys2) then y := ys2;
     xypen.color := clwhite; xypen.mode := pmxor;
     moveto(xs1,y); lineto(xs2+1,y);
     xypen.mode := pmcopy;
   end;
  if contzlog then case hmode of 1 : h := h/zmi; 2 : h := h/zma; end
              else case hmode of 1 : h := h-zmi; 2 : h := zma-h; end;
  result := h;
end;

procedure makecrosssect(x1,y1,x2,y2:single;np:integer;var ok:boolean); {XXXXXXX}
var i,n,t : integer;
    s : string;
    dx,dy,dxx,dyy : single;
    ok2 : boolean;
begin
  ok2 := ok; ok := false;
  if (ncrosssect<0) then exit;
  with graphs3d[igraph] do if not (sf and m3d) then exit;
  with graphs3d[igraph] do with srf do begin {bepaal surface}
    xmi := s1; xma := s2; ymi := s3; yma := s4; zmi := s5; zma := s6;
    datap := dp;
  end;

  if ok2 and (crosssectopt and 2 > 0) then {prompt}
    begin
      s := inttostr(ncrosssect);
      if inputquery('Create cross-setction',
        'How many data points in the cross-section array?',s) then
      begin val(s,n,t); if (t=0) then ncrosssect := n else exit; end
      else exit;
    end;

  ok := false; n := ncrosssect; if (n=0) then n := np;
  if (n=0) then exit; if (n=1) then n := 2;
  setlength(xy3dcrosssection,n);
  dx := (x2-x1)/(n-1); dy := (y2-y1)/(n-1);
  for i := 0 to n-1 do with xy3dcrosssection[i] do
    begin
      x := x1 + i*dx; y := y1 + i*dy;
      z := hoogtexy(x,y);
      if hasempty and (z=empty) then z := 0;
      l := sqrt(sqr(x-x1)+sqr(y-y1));
    end;
  xy3dcrosssection[0].l := 0; {harde 0 ivm check}

  dxx := dx/2; dyy := dy/2; with srf do
    ok2 := (x1+dxx>xmi) and (x2-dxx<xma) and (y1+dyy>ymi) and (y2-dyy<yma);
  s := 'Cross-section of '+inttostr(n)+' points created';
  if (crosssectopt and 4 > 0) then {confirmation}
    if ok2 then showmessage(s)
    else
      begin
        s := s + ' but some'+#13#10+'points may have fallen outside the surface';
        application.messagebox(@s[1],'Cross section',mb_OK+mb_iconwarning);
      end;
  {onthoud instellingen}
  with crs do begin xmi := x1; xma := x2; ymi := y1; yma := y2; zmi := np; end;
  crsdp := datap; ok := true;
end;

procedure clearcrosssection; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  setlength(xy3dcrosssection,1); with xy3dcrosssection[0] do
     begin x := 0; y := 0; z := 0; l := -1; end;
end;

procedure xy3dsetcrosssection(np,opt:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var ok : boolean;
begin
  crosssectopt := 0; ok := false;
  if (np<0) then begin ncrosssect := -1; clearcrosssection; end
     else ncrosssect := np;
  if (opt and 8>0) and (xy3dcrosssection[0].l=0) and (crsdp=datap) then
    with crs do makecrosssect(xmi,ymi,xma,yma,round(zmi),ok);
  crosssectopt := opt;
  crsok := (np<>-1) or (opt and 1 >0);
end;

function hoek(dhx,dhy,dx,dy:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{berekent hoek van richtingen bij vaste kijkrichting van boven}
var csx,csy,csz,t,h1 : single;
const zhfac = 1;
begin
   csx := (dhx*facz)/(dx*1); csy := (dhy*facz)/(dy*facy); csz := 1;
   t:= sqrt(sqr(csx)+sqr(csy)+sqr(csz));
   csx := csx/t; csy := csy/t; csz := csz/t;
   h1 := csx*csxx + csy*csyy + csz*cszz;
   result := h1;
end;

function hoek4(h1,h2,h3,h4,dx,dy:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{berekent hoek van 4 hoekpunten}
begin
  result := hoek((h1+h4-h2-h3)/2,(h1+h2-h3-h4)/2,dx,dy);
end;

function color0(h:single):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{bepaalt kleur nummer bij bepaalde hoogte}
var n : integer;
begin
  if (h<cmi) then n := 0 else
  if (h>cma) then n := 255 else
  if contzlog then n := round((ln(h)-lcmi)*colfac)
       else n := round((h-cmi)*colfac);
  if crev then n := 255 - n;
  result := n;
end;

function color(h:single):Tcolor; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{bepaalt kleur bij bepaalde hoogte}
var n : integer;
begin
  n := color0(h);
  case cmode of
    1 : if odd(n div cres) then result := 0 else result := $ffffff;
   2,3 : begin result := colors[resscale[n]]; end;
   else result := 0;
  end
end;

function revcolor(h:single):Tcolor; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{idem in reverse}
var n : integer;
begin
  n := color0(h);
  case cmode of
    1 : if odd(n div cres) then result := 0 else result := $ffffff;
   2,3 : begin result := revcolors[resscale[n]]; end;
   else result := 0;
  end
end;

procedure xy3dheightscale(x1,y1,x2,y2:integer;txt:string;opt:integer); {XXXXXXX}
  { x1,x2,y1,y2 { gebied op scherm
    txt : text bij as
    opt : opties bit 0 set = schaal links ipv rechts
                 bit 1 set = text sideways
    }
var t,y,cl,p,clh,clh0,c0,y0,yy,pw,col : integer;
    h : single;
    oldlog : boolean;
begin
  if not ({(surf or colgen) and} mode3d) then exit;
  if (x1=x2) or (y1=y2) then exit;
  if (x1>x2) then begin t := x1; x1 := x2; x2 := t; end;
  if (y1>y2) then begin t := y1; y1 := y2; y2 := t; end;
  x1 := x1*res+xoff; x2 := x2*res+xoff;
  y1 := y1*res+yoff; y2 := y2*res+yoff;
  sx1 := x1; sx2 := x2; sy1 := y1; sy2 := y2;
  case opt and 3 of 0:p:=4; 1:p:=1; 2:p:=7; 3:p:=5; end;
  if {colgen} (hmode<0) then col := xypen.color else col := ascol;
  restartgraph(x1,x2,y1,y2,true);
  copyzoom(igraph,0,1,0,1);
  if contzlog then
  case hmode of
   1 : xyyaxis(col,1,cma/cmi,0,0,txt,p,false,true,false);
   2 : xyyaxis(col,cmi/cma,1,0,0,txt,p,false,true,false);
  else xyyaxis(col,cmi,cma,  0,0,txt,p,false,true,false);
  end
  else
  case hmode of
   1 : xyyaxis(col,0,cma-cmi,0,0,txt,p,false,false,false);
   2 : xyyaxis(col,cma-cmi,0,0,0,txt,p,false,false,false);
  else xyyaxis(col,cmi,cma,  0,0,txt,p,false,false,false);
  end;
  oldlog := contzlog; contzlog := false;
  with graphs3d[igraph] do copyzoom(igraph,xf0,xf1,yf0,yf1);
  case cmode of
   0 : begin xyrectangle(-1,ccol,x1,y1,x2+1,y2+1); end;
   1,2,3 : begin
        y0 := y1;
        pw := xypen.width; xypen.width := res;
        for y := y1 to y2 do
          begin h := cmi + (y2-y)/(y2-y1)*(cma-cmi);
            clh := color(h);
            if (y=y1) then clh0 := clh else
            if (clh<>clh0) or (y=y2) then
             begin
               if (y=y2) then yy := y2+1 else yy := y;
               c0 := clh0;
               xyrectangle(clh0,c0,x1,y0,x2+1,yy);
               clh0 := clh; y0 := y;
             end;
         end;
         xypen.width := pw;
        end;
   4..5 : begin if (zonmode=1) then cl := {mixcolor(ccol,0,1,1)}0 else cl := ccol;
           xyrectangle(-1,cl,x1,y1,x2+1,y2+1);
         end;
   end;
  contzlog := oldlog;
  restartgraph(xp1,xp2,yp1,yp2,false); scale := true;
  if (hmode>=0) then with graphs3d[igraph] do
    begin xs1 := sx1; xs2 := sx2; ys1 := sy1; ys2 := sy2; scl := true; end;
end;

procedure xy3dheightline(h:single;col:Tcolor;opt:integer); {XXXXXXXXXXXXXXXXXXX}
 { h = hoogte (afh van hoogte mode!)
   col = kleur
   opt : opties bit 0 set = geen hoogte lijn in schaal
  }
var ok,first : boolean;
    x,y,x0,x1,x2,x3,x4,y0,y1,y2,y3,y4,xx,yy,xx0,yy0 : integer;
    f,h0,h1,h2,h3,h4,h13,h24 : single;
procedure doe; {---------------------------------------------------------------}
begin
  with xycanvas do
  if first then begin moveto(xx,yy); xx0 := xx; yy0 := yy; first := false; end
   else begin if (xx<>xx0) or (yy<>yy0) then
      begin lineto(xx,yy); first := true; end;
         pixels[xx,yy] := col; end;
end;
procedure doedriehoek(x1,x2,x3,y1,y2,y3:integer;h1,h2,h3:single); {------------}
begin
  if (h1=h2) and (h1=h3) then exit; first := true;
  if (h1<>h2) then
     if ( (h1>=h) and (h2<=h) ) or ( (h2>=h) and (h1<=h) ) then
         begin f := (h-h1)/(h2-h1); xx := round(x1*(1-f)+(x2*f));
                   yy := round(y1*(1-f)+(y2*f)); doe; end;
  if (h2<>h3) then
     if ( (h2>=h) and (h3<=h) ) or ( (h3>=h) and (h2<=h) ) then
         begin f := (h-h2)/(h3-h2); xx := round(x2*(1-f)+(x3*f));
                   yy := round(y2*(1-f)+(y3*f)); doe; end;
  if (h3<>h1) then
     if ( (h3>=h) and (h1<=h) ) or ( (h1>=h) and (h3<=h) ) then
         begin f := (h-h3)/(h1-h3); xx := round(x3*(1-f)+(x1*f));
                   yy := round(y3*(1-f)+(y1*f)); doe; end;
end;
begin {------------------------------------------------------------------------}
  if not (surf and mode3d) then exit;
  if contzlog then case hmode of 1 : h := h*zmi; 2 : h := zma*h; end
              else case hmode of 1 : h := zmi+h; 2 : h := zma-h; end;
  if (h<zmi) or (h>zma) then exit;
  xypen.color := col;
  if not (doprint or dowmf) then
  for x := nx1 to nx2 do  for y := ny1 to ny2 do
    begin
      ok := true;
      h1 := datap^[x,y];   h2 := datap^[x+1,y];
      h4 := datap^[x,y+1]; h3 := datap^[x+1,y+1];
      if hasempty then
        if (h1=empty) or (h2=empty) or (h3=empty) or (h4=empty) then ok := false;
      if (h1<h) and (h2<h) and (h3<h) and (h4<h) then ok := false;
      if (h1>h) and (h2>h) and (h3>h) and (h4>h) then ok := false;
      if ok then
        begin
          h13 := (h1+h3)/2; h24 := (h2+h4)/2;
          x1 := xps[x]; x2 := xps[x+1]; x3 := x2; x4 := x1;
          y1 := yps[y]; y2 := y1; y3 := yps[y+1]; y4 := y3;
          x0 := (x1+x2) div 2; y0 := (y1+y4) div 2;
          case drmode of
           2 : h0 := h13;          3 : h0 := h24;
           4 : h0 := max(h13,h24); 5 : h0 := min(h13,h24);
           6 : if ((x>nxc) and (y>nxc)) or ((x<=nxc) and (y<=nyc)) then h0 := h13 else h0 := h24;
           7 : if ((x>nxc) and (y<=nxc)) or ((x<=nxc) and (y>nyc)) then h0 := h13 else h0 := h24;
          else h0 := (h1+h2+h3+h4)/4;
          end;
          doedriehoek(x1,x2,x0,y1,y2,y0,h1,h2,h0);
          doedriehoek(x2,x3,x0,y2,y3,y0,h2,h3,h0);
          doedriehoek(x3,x4,x0,y3,y4,y0,h3,h4,h0);
          doedriehoek(x4,x1,x0,y4,y1,y0,h4,h1,h0);
        end;
    end;

  if (opt and 1 = 0 ) and scale and (h>=cmi) and (h<=cma) then
    begin
      if not contzlog then y := sy1 + round((cma-h)/(cma-cmi)*(sy2-sy1))
         else y := sy1 + round((lcma-ln(h))/(lcma-lcmi)*(sy2-sy1));
      xyline(-1,sx1,y,sx2+1,y);
    end;
end;

procedure xy3dshowgrid(col:Tcolor;size,mode:integer); {XXXXXXXXXXXXXXXXXXXXXXXX}
var x,y,xp,yp,x1,y1,x2,y2 : integer;
begin
  if not (surf and mode3d) then exit;
  if (doprint or dowmf) then exit;
  xypen.color := col; if (mode>0) then xypen.mode := pmxor;
  xylinewidth(1); size := size * res;
  with xycanvas do
  for x := nx1 to nx2 do for y := ny1 to ny2 do
    begin xp := xps[x]; yp := yps[y];
    if (size>0) then
      begin
         x1 := xp-size; if (x1<xp1) then x1 := xp1;
         x2 := xp+size; if (x2>xp2) then x2 := xp2;
         xyline(-1,x1,yp,x2+1,yp) ;
         y1 := yp-size; if (y1<yp1) then y1 := yp1;
         y2 := yp+size; if (y2>yp2) then y2 := yp2;
         xyline(-1,xp,y1,xp,y2+1);
      end;
    if (mode>0) then col := col xor pixels[xp,yp];
    xypixel(xp,yp,col);
    end;
  xypen.mode := pmcopy;
end;

procedure xy3dsetcolors(cs:array of Tcolor;cmin,cmax:single;res:integer;rev,log:boolean);
begin
  setcolorarray(cs); setcolorrange(cmin,cmax,log);
  cres := res; if (cres<1) then cres := 1;
  setresscale(cres);
  crev := rev; cmode := 2; hmode := -1;
end;

procedure xy3dsetcolors(cs:array of Tcolor); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin xy3dsetcolors(cs,0,0,1,false,false); end;

procedure xy3dshowcontour(mode:integer;col1,col2:Tcolor; xtxt,ytxt : string;
  yfac,hmin,hmax,hfac:single; hm,cr:integer;rev,nospl,fast:boolean;
  fresh:Tprocedure;opt:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{ mode 0=vlak 1=zw/wit 2,3=kleur 4=foto 5=foto fijn
  col1 : kleur assen                                        
  col2 : kleur vlak (mode 0 en 4,5)
  xtxt,ytxt : text bij assen
  yfac : rel.schaalfactor Y
  hmin,hmax : hoogteschaal; kies 0,0 voor autom.
  hm : mode hoogte; 0 = als h; 1 = vanaf onder; 2 = vanaf boven
  hfac : schaalfactor Z (alleen mode 4,5)
  cr : resolutie kleur
  rev : kleurenschaal omgekeerd (alleen mode 1,2 en 3);
  nospl : vierk worden niet gepslitst (alleen mode 1,2,3 en 4)
  fast : fast mode on
  fresh : refresh procedure
  opt :  bit 0 en 1 = refresh mode
         bit 2 : text Y-as sideways
         bit 3 : z schaal log
         bit 4 : nearest neighbour bij fast en mode 2}
type regel = array[0..8000] of cardinal;
     pregel = ^regel;
var x,y,lx,ly,refr,pw,n,n1,n2,p,revback,p1,p2 : integer;
    t,f1,f2,ff,fxy : single;
    pr : pregel;
    bm : Tbitmap;
    pxy : array of integer;
    dozoom, nearest : boolean;

procedure fotopart(x,y:integer;s4:boolean); {----------------------------------}
var h,h1,h2,h3,h4 : single; {1=lo, 2=ro, 3=rb, 4=lb}
    h12,h41,h23,h34,hm,xw,yw,h13,h24 : single;
    xp,yp,xp2,yp2,xm,ym : integer;
    b : boolean;
procedure driehoek(dhx,dhy:single;n1,n2,n3:integer); { - - - - - - - - - - - - }
var h : single;
begin
   h := hoek(dhx,dhy,xw,yw);
   xypen.color := hcolor(h,col2); xybrush.color := xypen.color;
   xypolygon(-1,-1,[point5[n1],point5[n2],point5[n3]]);
end;
begin { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  xw := datap^[x+1,0]-datap^[x,0]; yw := (datap^[0,y+1]-datap^[0,y])*facy;
  xp := xps[x]; xp2 := xps[x+1];
  yp := yps[y]; yp2 := yps[y+1];
  h1 := datap^[x,y];   h2 := datap^[x+1,y];
  h4 := datap^[x,y+1]; h3 := datap^[x+1,y+1];
  if hasempty then
   if (h1=empty) or (h2=empty) or (h3=empty) or (h4=empty) then exit;
  h12 := (h1+h2)/2; h41 := (h1+h4)/2;
  h23 := (h2+h3)/2; h34 := (h3+h4)/2;
  h13 := (h1+h3)/2; h24 := (h2+h4)/2;
  hm := (h1+h2+h3+h4)/4;
  xm := (xp+xp2) div 2; ym := (yp+yp2) div 2;

  point5[0] := point(xm,ym);
  point5[1] := point(xp,yp);   point5[2] := point(xp2,yp);
  point5[3] := point(xp2,yp2); point5[4] := point(xp,yp2);

  store; xypen.width := 1;
  with xycanvas do begin

  if s4 then {vierkant fijn}
   begin
    h := hoek4(h1,h12,hm,h41,xw/2,yw/2);
    xyrectangle(-1,hcolor(h,col2),xp,yp+1,xm,ym);

    h := hoek4(h12,h2,h23,hm,xw/2,yw/2);
    xyrectangle(-1,hcolor(h,col2),xm,yp+1,xp2+1,ym);

    h := hoek4(hm,h23,h3,h34,xw/2,yw/2);
    xyrectangle(-1,hcolor(h,col2),xm,ym,xp2+1,yp2);

    h := hoek4(h41,hm,h34,h4,xw/2,yw/2);
    xyrectangle(-1,hcolor(h,col2),xp,ym,xm,yp2);
   end else

  if nosplit or (flatno and (abs(h13-h24)<(zma-zmi)/1000)) then {vierhoek}
   begin
    h := hoek4(h1,h2,h3,h4,xw,yw);
    xyrectangle(-1,hcolor(h,col2),xp,yp+1,xp2+1,yp2);
   end else

  if (drmode=1) then {vier driehoeken}
    begin
     driehoek(h1-h2,(h12-hm)*2,0,1,2); driehoek((hm-h23)*2,h2-h3,0,2,3);
     driehoek(h4-h3,(hm-h34)*2,0,3,4); driehoek((h41-hm)*2,h1-h4,0,4,1);
    end
  else {twee driehoeken}
    begin
      case drmode of
       2 : b := true;      3 : b := false;
       4 : b := (h13>h24); 5 : b := (h13<h24);
       6 : b := ((x>nxc) and (y>nxc)) or ((x<=nxc) and (y<=nyc));
       7 : b := ((x>nxc) and (y<=nxc)) or ((x<=nxc) and (y>nyc));
       end;

    if b then {diagonaal /}
      begin driehoek(h1-h2,h2-h3,1,2,3); driehoek(h4-h3,h1-h4,3,4,1); end
    else   {diagonaal \}
      begin driehoek(h1-h2,h1-h4,4,1,2); driehoek(h4-h3,h2-h3,2,3,4); end;

   end;
   restore;
   end;
end;

procedure fotopart2(x,y:integer); {--------------------------------------------}
var h1,h2,h3,h4,fx,fy,h : single;
    xp,yp,dxp,dyp,tx,ty,ix,iy : integer;
function hh(x,y:integer):single;
var dhx,dhy,dx,dy : single;
begin
  if (x=1) then
    begin dhx := datap^[x+1,y]-datap^[x,y]; dx := datap^[x+1,0]-datap^[x,0]; end
  else
  if (x=nx) then
    begin dhx := datap^[x,y]-datap^[x-1,y]; dx := datap^[x,0]-datap^[x-1,0]; end
  else
    begin dhx := datap^[x+1,y]-datap^[x-1,y]; dx := datap^[x+1,0]-datap^[x-1,0]; end;

  if (y=1) then
    begin dhy := datap^[x,y+1]-datap^[x,y]; dy := datap^[0,y+1]-datap^[0,y]; end
  else
  if (y=ny) then
    begin dhy := datap^[x,y]-datap^[x,y-1]; dy := datap^[0,y]-datap^[0,y-1]; end
  else
    begin dhy := datap^[x,y+1]-datap^[x,y-1]; dy := datap^[0,y+1]-datap^[0,y-1]; end;

  result := hoek(-dhx,-dhy,dx,dy);
end;
begin
  xp := xps[x];       yp := yps[y];
  dxp := xps[x+1]-xp; dyp := yps[y+1]-yp;
  if (x=nx-1) then tx := dxp  else tx := dxp-1;
  if (y=ny-1) then ty := -dyp else ty := -dyp-1;
  if (tx<0) or (ty<0) then exit;

  h1 := hh(x,y); h2 := hh(x+1,y); h3 := hh(x+1,y+1); h4 := hh(x,y+1);
  if hasempty then
   if (h1=empty) or (h2=empty) or (h3=empty) or (h4=empty) then exit;

  bm.width := tx+1; bm.height := ty+1;
  for iy := 0 to ty do
  begin
    pr := bm.ScanLine[ty-iy];
    for ix := 0 to tx do
    begin
      fx := ix/dxp; fy := -iy/dyp;
      h := h1*(1-fx)*(1-fy) + h4*(1-fx)*(fy) +
          h2*(fx)*(1-fy) + h3*(fx)*(fy);
      pr[ix] := reversecolor(hcolor(h,col2));
    end;
   end;
   xycanvas.draw(xp,yp-ty,bm);
end;

procedure drawpart(x,y:integer); {---------------------------------------------}
var h1,h2,h3,h4,h12,h23,h34,h41,h13,h24,hm,h,fx,fy : single;
    n,dx,dy,xp,yp,ix,iy,tx,ty : integer;
begin
  h1 := datap^[x,y];    h2 := datap^[x+1,y];
  h4 := datap^[x,y+1];  h3 := datap^[x+1,y+1];
  if hasempty then
   if (h1=empty) or (h2=empty) or (h3=empty) or (h4=empty) then exit;
  h12 := (h1+h2)/2; h23 := (h2+h3)/2;
  h34 := (h3+h4)/2; h41 := (h4+h1)/2;
  h13 := (h1+h3)/2; h24 := (h2+h4)/2;
  if not nosplit then case drmode of
   2 : hm := h13;          3 : hm := h24;
   4 : hm := max(h13,h24); 5 : hm := min(h13,h24);
   6 : if ((x>nxc) and (y>nxc)) or ((x<=nxc) and (y<=nyc)) then hm := h13 else hm := h24;
   7 : if ((x>nxc) and (y<=nxc)) or ((x<=nxc) and (y>nyc)) then hm := h13 else hm := h24;
  else hm := (h1+h2+h3+h4)/4;
  end;

  xp := xps[x];      yp := yps[y];
  dx := xps[x+1]-xp; dy := yps[y+1]-yp;
  if (dx=0) or (dy=0) then exit;
  if (x=nx-1) then tx := dx  else tx := dx-1;
  if (y=ny-1) then ty := -dy else ty := -dy-1;
  if (tx<0) or (ty<0) then exit;
  bm.width := tx+1; bm.height := ty+1;
  for iy := 0 to ty do
  begin
    pr := bm.ScanLine[ty-iy];
    for ix := 0 to tx do
    begin
      fx := ix/dx; fy := -iy/dy;
      if nosplit then
        h := h1*(1-fx)*(1-fy) + h4*(1-fx)*(fy) +
           h2*(fx)*(1-fy) + h3*(fx)*(fy)
      else
        begin
          n := 1; if (fy>fx) then n := 3; if (fx+fy>1) then inc(n);
          { n = driehoek, 1=o, 2=r 3=l 4=b }
          case n of
           1 : h := h1 + fx*(h2-h1) + fy*2*(hm-h12);
           2 : h := h2 + fy*(h3-h2) - (1-fx)*2*(h23-hm);
           3 : h := h1 + fx*2*(hm-h41) + fy*(h4-h1);
           4 : h := h4 + fx*(h3-h4) - (1-fy)*2*(h34-hm);
          end;
        end;
      pr[ix] := revcolor(h);
    end;
   end;
   xycanvas.draw(xp,yp-ty,bm);
end;

procedure tekensnel; {---------------------------------------------------------}
var x,y,nx,ny,cl,nx0,ny0 : integer;
    h1,h2,h3,h4,h,xw,yw : single;
begin
  bm.width := xp2-xp1+1; bm.height := 1;
  pr := bm.ScanLine[0]; nx0 := -1; ny0 := -1;
  for y := 0 to yp2-yp1 do
    begin
      ny := yps[y]; if (ny<>ny0) then
      for x := 0 to xp2-xp1 do
        begin
          nx := xps[x]; if (nx<>nx0) then
          begin
            if (cmode=2) then
              begin
                h1 := datap^[nx,ny];
                if (h1=empty) then cl := revback
                else cl := revcolor(h1)
              end
            else begin
            h1 := datap^[nx,ny];    h2 := datap^[nx+1,ny];
            h4 := datap^[nx,ny+1];  h3 := datap^[nx+1,ny+1];
            if hasempty and ((h1=empty) or (h2=empty) or (h3=empty) or (h4=empty))
            then cl := revback else
            if (cmode<4) then
               begin h := (h1+h2+h3+h4)/4; cl := revcolor(h); end
            else
              begin
                xw := datap^[nx+1,0]-datap^[nx,0];
                yw := (datap^[0,ny+1]-datap^[0,ny])*facy;
                h := hoek4(h1,h2,h3,h4,xw,yw);
                cl := reversecolor(hcolor(h,col2));
              end;
              end;
          end;
          nx0 := nx; pr[x] := cl;
        end;
      ny0 := ny;
      xycanvas.draw(xp1,yp2-y,bm);
    end;
end;

begin {------------------------------------------------------------------------}
  if (not surf) or (polartype>0) then exit;
  if (mode<0) or (mode>5) then exit;

  dozoom := (opt>=zoom); opt := opt and (zoom-1);
  if not dozoom then with contcall do begin {sla alle instellingen op}
    ccmode := mode; cccol1 := col1; cccol2 := col2;
    ccxtxt := xtxt; ccytxt := ytxt; ccyfac := yfac;
    cchmin := hmin; cchmax:= hmax; cchfac := hfac;
    cchm := hm; cccr := cr; ccrev := rev;
    ccnospl := nospl; ccfast := fast;
    ccfresh := fresh; ccopt := opt; end;

  mode3d := true; graphs3d[igraph].m3d := mode3d;
  hmode := hm; cres := cr; crev := rev; cmode := mode; nosplit := nospl;
  case opt and 3 of 0:refr:=0; 1:refr:=1; 2:refr:=2; 3:refr:=4; end;
  if not cvmode then refr := 0;
  if (drmode<1) then nosplit := true;
  contzlog := (opt and 8) > 0; revback := reversecolor(backcolor);
  nearest := fast and (cmode=2) and ( (opt and 16)>0 );

  if dozoom then {wis oud beeld} with xygraphdata[igraph] do
    begin
      copyzoom(igraph,0,1,0,1);
      xyxaxis(-255,xaxis.min,xaxis.max,0,0,xtxt,false,false,false);
      if (opt and 4=0) then p := 1 else p := 5;
      xyyaxis(-255,yaxis[1].min,yaxis[1].max,0,0,ytxt,p,false,false,false);
      xybrush.color := backcolor; xypen.color := backcolor;
      xycanvas.rectangle(x1,y1,x2+1,y2+1);
    end;

  with graphs3d[igraph] do begin {bepaal zoom instellingen}
  xmi := srf.xmi + (xf0+1e-6)*(srf.xma-srf.xmi);
  xma := srf.xmi + (xf1-1e-6)*(srf.xma-srf.xmi);
  ymi := srf.ymi + (yf0+1e-6)*(srf.yma-srf.ymi);
  yma := srf.ymi + (yf1-1e-6)*(srf.yma-srf.ymi);
  zoekvak(xmi,ymi,nx1,ny1); zoekvak(xma,yma,nx2,ny2);
  xmi := datap^[nx1,0]; xma := datap^[nx2+1,0];
  ymi := datap^[0,ny1]; yma := datap^[0,ny2+1];
  xf0 := (xmi-srf.xmi)/(srf.xma-srf.xmi); xf1 := (xma-srf.xmi)/(srf.xma-srf.xmi);
  yf0 := (ymi-srf.ymi)/(srf.yma-srf.ymi); yf1 := (yma-srf.ymi)/(srf.yma-srf.ymi);
  copyzoom(igraph,xf0,xf1,yf0,yf1);
  end;

  setcolorrange(hmin,hmax,(opt and 8) > 0);
  setresscale(cres);

  if (col1<0) then col1 := frontcolor;
  if (col2<0) then col2 := clwhite;
  ccol := col2; ascol := col1;

  if (yfac=0) then yfac := 1;
  if (yfac<0) then yfac := (xma-xmi)/(yma-ymi)*abs(yfac);
  facy := yfac; facz := hfac; scale := false;

  with contcall do {oorspronkelijke instellingen scherm}
    begin xp1 := ccxp1; xp2 := ccxp2; yp1 := ccyp1; yp2 := ccyp2; end;
  f1 := (xp2-xp1)/(xma-xmi); f2 := (yp2-yp1)/(yma-ymi)/yfac;
  ff := min(f1,f2);
  lx := round((xma-xmi)*ff); ly := round((yma-ymi)*yfac*ff);
  xp1 := xp1 + ( (xp2-xp1)-lx) div 2; xp2 := xp1 + lx;
  yp1 := yp1 + ( (yp2-yp1)-ly) div 2; yp2 := yp1 + ly;
  restartgraph(xp1,xp2,yp1,yp2,false);

  setlength(pxy,nx+1); fxy := lx/(xma-xmi);
  for x := 1 to nx do pxy[x] := round(fxy*(datap^[x,0]-xmi));
  if fast then
    begin
      {xps bevat per pixel welke cel daar staat}
      setlength(xps,xp2-xp1+1);
      if nearest then
        begin
          for x := nx1 to nx2 do begin n1 := pxy[x]; n2 := pxy[x+1];
          if (x=nx1) then p1 := 0 else p1 := (n1+pxy[x-1]) div 2;
          p2 := (n1+n2) div 2;
          for n := p1 to p2-1 do xps[n] := x; end;
          for n := p2 to xp2-xp1 do xps[n] := nx2+1;
        end
      else
        begin
          for x := nx1 to nx2 do begin n1 := pxy[x]; n2 := pxy[x+1];
            for n := n1 to n2-1 do xps[n] := x; end;
          if (cmode=2) then xps[xp2-xp1] := nx2+1 else xps[xp2-xp1] := nx2; 
        end
    end
  else {xps bevat de pixelpos van elke cel}
    begin setlength(xps,nx+1); for x := 1 to nx do xps[x] := xp1 + pxy[x]; end;

  setlength(pxy,ny+1); fxy := ly/(yma-ymi);
  for y := 1 to ny do pxy[y] := round(fxy*(datap^[0,y]-ymi));
  if fast then
    begin
      setlength(yps,yp2-yp1+1);
      if nearest then
        begin
          for y := ny1 to ny2 do begin n1 := pxy[y]; n2 := pxy[y+1];
          if (y=ny1) then p1 := 0 else p1 := (n1+pxy[y-1]) div 2;
          p2 := (n1+n2) div 2;
          for n := p1 to p2-1 do yps[n] := y; end;
          for n := p2 to yp2-yp1 do yps[n] := ny2+1;
        end
      else
        begin
          for y := ny1 to ny2 do begin n1 := pxy[y]; n2 := pxy[y+1];
            for n := n1 to n2-1 do yps[n] := y; end;
          if (cmode=2) then yps[yp2-yp1] := ny2+1 else yps[yp2-yp1] := ny2;
        end;
    end
  else
    begin setlength(yps,ny+1); for y := 1 to ny do yps[y] := yp2 - pxy[y]; end;

  xyxaxis(col1,srf.xmi,srf.xma,0,0,xtxt,false,false,false);
  if (opt and 4=0) then p := 1 else p := 5;
  xyyaxis(col1,srf.ymi,srf.yma,0,0,ytxt,p,false,false,false);
  setcont;

  if doprint then begin
     with graphs3d[igraph] do printpart(xc1,yc1,xc2,yc2) end else
  if dowmf then begin
     with graphs3d[igraph] do bitmapwmf(xc1,yc1,xc2,yc2) end else
  begin

  bm := tbitmap.create;
  bm.pixelformat := pf32bit;

  if (cmode=0) then
   begin xypen.color := col2; xybrush.color := col2;
         xycanvas.rectangle(xp1,yp1,xp2+1,yp2+1); end
  else
  if fast then tekensnel
  else
    for y := ny1 to ny2 do
      begin for x := nx1 to nx2 do
        if (cmode=5) then fotopart2(x,y) else
        if (cmode<4) then drawpart(x,y) else fotopart(x,y,(cmode=5));
        if (y<ny-1) then if (refr=1) or ( (refr=2) and not odd(y) )
          or ( (refr=4) and (y and 3 = 0) ) then fresh;
      end;

  bm.free;

  if fast then {zet alsnog de pixelpos van elke cel in xps}
    begin
      setlength(xps,nx+1); fxy := lx/(xma-xmi);
      for x := 1 to nx do xps[x] := xp1 + round(fxy*(datap^[x,0]-xmi));
      setlength(yps,ny+1); fxy := ly/(yma-ymi);
      for y := 1 to ny do yps[y] := yp2 - round(fxy*(datap^[0,y]-ymi));
    end;

  with graphs3d[igraph] do
    begin z1 := zmi; z2 := zma; c1 := cmi; c2 := cma;
     hm := hmode; drm := drmode;
     xc1 := xp1; xc2 := xp2; yc1 := yp1; yc2 := yp2;
     cnt := true; isbmp := true; empt := empty; hasempt := hasempty; end;
  hasbmp := true; xybuffers[0].bmp := true;

  with xygraphdata[igraph] do
   begin
     with yaxis[2] do begin min := zmi; max := zma; on := true; log := contzlog; end;
     with yaxis[3] do begin min := zmi; max := zma; on := true; log := contzlog; end;
     if (hmode>0) then with yaxis[3] do
      if contzlog then
       case hmode of
         1 : begin min := 1; max := zma/zmi; end;
         2 : begin min := zmi/zma; max := 1; end
        end
     else
       case hmode of
         1 : begin min := 0; max := zma-zmi; end;
         2 : begin max := zma-zmi; min := 0; end;
       end;
   end;
  xygraphdata[0] := xygraphdata[igraph];
  end;
end;

procedure xy3dshowcontour(mode:integer;col1,col2:Tcolor; xtxt,ytxt : string;
  yfac,hmin,hmax,hfac:single; hm,cr:integer;rev,nospl,fast:boolean;opt:integer);
begin xy3dshowcontour(mode,col1,col2,xtxt,ytxt,yfac,hmin,hmax,hfac,
  hm,cr,rev,nospl,fast,dummy,opt); end;

procedure zoomcontour; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with contcall  do
  xy3dshowcontour(ccmode,cccol1,cccol2,ccxtxt,ccytxt,ccyfac,cchmin,cchmax,
    cchfac,cchm,cccr,ccrev,ccnospl,ccfast,ccfresh,ccopt+zoom);
end;

{==============================================================================}
{======== Procedures voor surface weergave ====================================}
{==============================================================================}

function hoek3(dhx,dhy,dx,dy:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{hoek van kijkrichting}
var csx,csy,csz,t : single;
begin
   csx := (dhx*facz)/(dx*facx); csy := (dhy*facz)/(dy*facy); csz := 1;
   t:= sqrt(sqr(csx)+sqr(csy)+sqr(csz));
   csx := csx/t; csy := csy/t; csz := csz/t;
   result := csx*csxk + csy*csyk + csz*cszk;
end;

function hoek4Y(h1,h2,h3,h4,dx,dy:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin result := hoek3((h1+h4-h2-h3)/2,(h1+h2-h3-h4)/2,dx,dy); end;

function hoek2(dhx,dhy,dx,dy:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{hoek van zon met kijkrichting}
var csx,csy,csz,t,h1,h2 : single;
begin
   csx := (dhx*facz)/(dx*facx); csy := (dhy*facz)/(dy*facy); csz := 1;
   t:= sqrt(sqr(csx)+sqr(csy)+sqr(csz));
   csx := csx/t; csy := csy/t; csz := csz/t;
   h1 := csx*csxx + csy*csyy + csz*cszz;

   h2 := csx*csxk + csy*csyk + csz*cszk;
   if (h2>0) then result := h1 else result := -h1;
end;

function hoek4X(h1,h2,h3,h4,dx,dy:single):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  result := hoek2((h1+h4-h2-h3)/2,(h1+h2-h3-h4)/2,dx,dy);
end;

procedure xy3dshowsurface(mode:integer;dikte:single;col1,col2,col3:Tcolor;
    solid,nosplit:boolean;just,opt:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {oud: bit 4 (8)  = kleur vlakken volgens hoogte
       bit 5 (16) = kleur vlakken volgens hoek ipv gem
  -->  00 = fcol=0; 01 = fcol=1; 11 = fcol = 2}
var t : integer;
begin
  if (opt and 8)=0 then t := 0 else
  if (opt and 16)=0 then t := 1 else t := 2;
  opt := opt and 7;
  xy3dshowsurface(mode,dikte,col1,col2,col3,solid,nosplit,t,just,opt);
end;

procedure xy3dshowsurface(mode:integer;dikte:single;col1,col2,col3:Tcolor;
    solid,nosplit:boolean;fcol,just,opt:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xps, yps : array of array of smallint;
    x,y,xp,yp,xx,yy,d,o,stm:integer;
    dik,foto,nogrid,trans,back,lyncol : boolean;
    dh1,dh2,dh,t : single;
    col : Tcolor;
{ mode: 0=outline 1=punten 2=X-lijnen 3=Y-lijnen 4=draadmodel 5=vlakken 6=foto
  dik : dikte in usercoord
  col1 : rasterkleur (mode 0-4)
  col2 : vlak kleur  (mode 4)
  col3 : vlak kleur  (mode 5)
  nosplit : geen splitsing van vierkanten (mode 4,5)
  fcol : face color (0=niet 1=gem 2=hoek 3=2x2 4=ruit2x2 5=3x3 6=ruit3x3 etc)
  just : justification voor dikte
  opt : options  bit 1 (1)  = show grid in mode 5
                 bit 2 (2)  = teken transparant in mode 4 en 5
                 bit 3 (4)  = kleur lijnen/punten volgens hoogte
}

function heightcol(h:single):Tcolor; {---------------------------------------}
var hh : integer;
begin
  h := (h-srf.zmi)/(srf.zma-srf.zmi)-dh; hh := round(h*255);
  if (hh<0) then hh := 0 else if (hh>255) then hh := 255;
  result := colors[hh];
end;

procedure tekenrandx(front,open:boolean); {------------------------------------}
var h,h1,h2,x1,x2,yp : single;
    x,y : integer;
begin
  if front then y := 1 else y := ny; yp := datap^[0,y];
  if not open then if ( front and (viewy>yp) ) or ( not front and (viewy<yp) ) then exit;
  if foto then
    begin h := csyy; if front then h := -h; xybrush.color := hcolor(h,col3); end;
  if nogrid then begin xypen.width := 1;
    if not open then xypen.color := xybrush.color; end;
  if open then xybrush.style := bsclear;
  for x := 1 to nx-1 do
    begin
      x1 := datap^[x,0]; x2 := datap^[x+1,0];
      h1 := datap^[x,y]; h2 := datap^[x+1,y];
      point4[0] := topoint(x1,yp,h1+dh1); point4[1] := topoint(x2,yp,h2+dh1);
      if solid then begin
      point4[2] := topoint(x2,yp,dikte); point4[3] := topoint(x1,yp,dikte); end
      else begin
      point4[2] := topoint(x2,yp,h2+dh2); point4[3] := topoint(x1,yp,h1+dh2); end;
      if trans and not open then transpolygon(-1,-1,point4)
        else xypolygon(-1,-1,point4);
    end;
  xybrush.style := bssolid;
end;

procedure tekenrandy(front,open:boolean); {------------------------------------}
var h,h1,h2,y1,y2,xp : single;
    x,y : integer;
begin
  if front then x := 1 else x := nx; xp := datap^[x,0];
  if not open then if ( front and (viewx>xp) ) or ( not front and (viewx<xp) ) then exit;
  if foto then
    begin h := csxx; if front then h := -h; xybrush.color := hcolor(h,col3); end;
  if nogrid then begin xypen.width := 1;
    if not open then xypen.color := xybrush.color; end;
  if open then xybrush.style := bsclear;
  for y := 1 to ny-1 do
    begin
      y1 := datap^[0,y]; y2 := datap^[0,y+1];
      h1 := datap^[x,y]; h2 := datap^[x,y+1];
      point4[0] := topoint(xp,y1,h1+dh1); point4[1] := topoint(xp,y2,h2+dh1);
      if solid then begin
      point4[2] := topoint(xp,y2,dikte); point4[3] := topoint(xp,y1,dikte); end
      else begin
      point4[2] := topoint(xp,y2,h2+dh2); point4[3] := topoint(xp,y1,h1+dh2); end;
      if trans and not open then transpolygon(-1,-1,point4)
        else xypolygon(-1,-1,point4);
    end;
  xybrush.style := bssolid;
end;

procedure outline; {-----------------------------------------------------------}
var x,y : integer;
begin
  with srf do
  if solid then xy3dbar(point3d(xmi,ymi,dikte),point3D(xma,yma,zma),
      col1,-1,-1,-1,-1,-1,-1,false,false)
  else xy3dbar(point3d(xmi,ymi,zmi+dh2),point3d(xma,yma,zma+dh1),
      col1,-1,-1,-1,-1,-1,-1,false,false);

  y := 1; xy3dmove(datap^[1,0],datap^[0,y],datap^[1,y]);
  for x := 2 to nx do xy3ddraw(datap^[x,0],datap^[0,y],datap^[x,y]);
  y := ny; xy3dmove(datap^[1,0],datap^[0,y],datap^[1,y]);
  for x := 2 to nx do xy3ddraw(datap^[x,0],datap^[0,y],datap^[x,y]);

  x := 1; xy3dmove(datap^[x,0],datap^[0,1],datap^[x,1]);
  for y := 2 to ny do xy3ddraw(datap^[x,0],datap^[0,y],datap^[x,y]);
  x := nx; xy3dmove(datap^[x,0],datap^[0,1],datap^[x,1]);
  for y := 2 to ny do xy3ddraw(datap^[x,0],datap^[0,y],datap^[x,y]);

  with lastsymbol do
   begin xp := 0; yp := 0; cd3d := 20; size := 0; fill := 0;
     width := xypen.width; color := col1; cl1 := -1; cl2 := -1; cl3 := -1;
  end;
end;

procedure closebottom; {-------------------------------------------------------}
var x1,y1,x2,y2,h : single;
begin
  if (viewz>dikte) then exit;
  x1 := srf.xmi; x2 := srf.xma;
  y1 := srf.ymi; y2 := srf.yma;
  point4[0] := topoint(x1,y1,dikte); point4[1] := topoint(x1,y2,dikte);
  point4[2] := topoint(x2,y2,dikte); point4[3] := topoint(x2,y1,dikte);
  if foto then begin h := cszz; xybrush.color := hcolor(-h,col3); end;
  if nogrid then begin xypen.width := 1; xypen.color := xybrush.color; end;
  if trans then transpolygon(-1,-1,point4) else xypolygon(-1,-1,point4);
end;

procedure tekenvak(x,y,m:integer); {-------------------------------------------}
var h,dx,dy,h1,h2,h3,h4,h12,h23,h34,h14,h13,h24,hm,xm,ym,c0,c1,c2,c3,c4,c : single;
    b : boolean;
    pw,pc,t : integer;
    p1,p2,p3,p4,p12,p23,p34,p41,p0 : Tpoint;

procedure driehoek(dhx,dhy:single;n1,n2,n3:integer); { - - - - - - - - - - - - }
begin
  pw := xypen.width;
  if foto then
    begin h := hoek2(dhx,dhy,dx,dy); xybrush.color := hcolor(h,col3); end;
  if nogrid then begin xypen.width := 1; xypen.color := xybrush.color; end;
  if trans then transpolygon(-1,-1,[point5[n1],point5[n2],point5[n3]])
  else xypolygon(-1,-1,[point5[n1],point5[n2],point5[n3]]);
  xypen.width := pw;
end;
procedure doe12;  begin driehoek(h1-h2,(h12-hm)*2,0,1,2); end; { - - - - - - - }
procedure doe23;  begin driehoek((hm-h23)*2,h2-h3,0,2,3); end; { - - - - - - - }
procedure doe34;  begin driehoek(h4-h3,(hm-h34)*2,0,3,4); end; { - - - - - - - }
procedure doe41;  begin driehoek((h14-hm)*2,h1-h4,0,4,1); end; { - - - - - - - }
procedure doe123; begin driehoek(h1-h2,h2-h3,1,2,3); end; { - - - - - - - - - -}
procedure doe234; begin driehoek(h4-h3,h2-h3,2,3,4); end; { - - - - - - - - - -}
procedure doe341; begin driehoek(h4-h3,h1-h4,3,4,1); end; { - - - - - - - - - -}
procedure doe412; begin driehoek(h1-h2,h1-h4,4,1,2); end; { - - - - - - - - - -}

begin { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  case oct of
    7,8 : y := ny-y;
    5,6 : ;
    3,4 : x := nx-x;
    1,2 : begin x := nx-x; y := ny-y; end;
  end;

  h1 := datap^[x,y];   h2 := datap^[x+1,y];
  h4 := datap^[x,y+1]; h3 := datap^[x+1,y+1];
  if hasempty then
    if (h1=empty) or (h2=empty) or (h3=empty) or (h4=empty) then exit;
  dx := datap^[x+1,0]-datap^[x,0]; dy := datap^[0,y+1]-datap^[0,y];
  c1 := colsp^[x,y];   c2 := colsp^[x+1,y];
  c4 := colsp^[x,y+1]; c3 := colsp^[x+1,y+1];
  c0 := (c1+c2+c3+c4)/4;

  if dik then
    begin
      h := hoek4y(h1,h2,h3,h4,dx,dy);
      if (h>0) then dh := dh1 else dh := dh2;
      h1 := h1+dh; h2 := h2+dh; h3 := h3+dh; h4 := h4+dh;
    end;

  h13 := (h1+h3)/2; h24 := (h2+h4)/2;

  if nosplit or (flatno and (abs(h13-h24)<(zma-zmi)/1000)) then
    begin
      if dik then
        begin
          point4[0] := topoint(datap^[x,0],  datap^[0,y],  h1);
          point4[1] := topoint(datap^[x+1,0],datap^[0,y],  h2);
          point4[2] := topoint(datap^[x+1,0],datap^[0,y+1],h3);
          point4[3] := topoint(datap^[x,0],  datap^[0,y+1],h4);
        end
      else
        begin
          point4[0] := point(xps[x,y],    yps[x,y]);
          point4[1] := point(xps[x+1,y],  yps[x+1,y]);
          point4[2] := point(xps[x+1,y+1],yps[x+1,y+1]);
          point4[3] := point(xps[x,y+1],  yps[x,y+1]);
        end;
      pw := xypen.width;
      if (fcol>2) and not foto then
        begin
          p1 := point4[0]; p2 := point4[1]; p3 := point4[2]; p4 := point4[3];
          xypen.width := 1; pc := xypen.color;
          t := (fcol+1) div 2; if not odd(fcol) then t := -t;
          plotrect(p1,p2,p3,p4,color0(c1),color0(c2),color0(c3),color0(c4),t);
          drawpoly(pc,-1,[p1,p2,p3,p4],false);
          xypen.color := pc;
        end
      else
        begin
          if foto then
            begin h := hoek4x(h1,h2,h3,h4,dx,dy); xybrush.color := hcolor(h,col3); end
          else if (fcol=2) then xybrush.color := color(c1)
          else if (fcol=1) then xybrush.color := color(c0);
          if nogrid then begin xypen.width := 1; xypen.color := xybrush.color; end;
          if (mode>4) then
            if trans then transpolygon(-1,-1,point4) else xypolygon(-1,-1,point4);
        end;

      if lyncol and not trans then {teken lijntjes alsnog in kleur}
      if dowmf then
        begin
          movewmf(xps[x,y],yps[x,y]);
          xypen.color := color((c1+c2)/2); checkwmfpen; drawwmf(xps[x+1,y],yps[x+1,y]);
          xypen.color := color((c2+c3)/2); checkwmfpen; drawwmf(xps[x+1,y+1],yps[x+1,y+1]);
          xypen.color := color((c3+c4)/2); checkwmfpen; drawwmf(xps[x,y+1],yps[x,y+1]);
          xypen.color := color((c4+c1)/2); checkwmfpen; drawwmf(xps[x,y],yps[x,y]);
        end
      else with xycanvas do
        begin
          moveto(xps[x,y],yps[x,y]);
          xypen.color := color((c1+c2)/2); lineto(xps[x+1,y],yps[x+1,y]);
          xypen.color := color((c2+c3)/2); lineto(xps[x+1,y+1],yps[x+1,y+1]);
          xypen.color := color((c3+c4)/2); lineto(xps[x,y+1],yps[x,y+1]);
          xypen.color := color((c4+c1)/2); lineto(xps[x,y],yps[x,y]);
        end;
      xypen.width := pw;
   end

  else

    begin
      h12 := (h1+h2)/2; h14 := (h1+h4)/2;
      h23 := (h2+h3)/2; h34 := (h3+h4)/2;
      h13 := (h1+h3)/2; h24 := (h2+h4)/2;
      hm := (h1+h2+h3+h4)/4;
      xm := (datap^[x+1,0]+datap^[x,0])/2;
      ym := (datap^[0,y+1]+datap^[0,y])/2;
      point5[0] := topoint(xm,ym,hm);
      if dik then
        begin
          point5[1] := topoint(datap^[x,0],  datap^[0,y],  h1);
          point5[2] := topoint(datap^[x+1,0],datap^[0,y],  h2);
          point5[3] := topoint(datap^[x+1,0],datap^[0,y+1],h3);
          point5[4] := topoint(datap^[x,0],  datap^[0,y+1],h4);
        end
      else
        begin
          point5[1] := point(xps[x,y],    yps[x,y]);
          point5[2] := point(xps[x+1,y],  yps[x+1,y]);
          point5[3] := point(xps[x+1,y+1],yps[x+1,y+1]);
          point5[4] := point(xps[x,y+1],  yps[x,y+1]);
        end;
      if (drmode=1) then
        case oct of
         1 : begin doe34;doe23;doe41;doe12; end;
         2 : begin doe23;doe34;doe12;doe41; end;
         3 : begin doe23;doe12;doe34;doe41; end;
         4 : begin doe12;doe23;doe41;doe34; end;
         5 : begin doe12;doe41;doe23;doe34; end;
         6 : begin doe41;doe12;doe34;doe23; end;
         7 : begin doe41;doe34;doe12;doe23; end;
         8 : begin doe34;doe41;doe23;doe12; end;
        end
      else
        begin
          case drmode of
           2 : b := true;      3 : b := false;
           4 : b := (h13>h24); 5 : b := (h13<h24);
           6 : b := ((x>nxc) and (y>nxc)) or ((x<=nxc) and (y<=nyc));
           7 : b := ((x>nxc) and (y<=nxc)) or ((x<=nxc) and (y>nyc));
          end;
        if b then
         if oct in [2,3,4,5] then begin doe123; doe341; end
          else begin doe341; doe123; end
        else
        if oct in [1,2,3,8] then begin doe234; doe412; end
          else begin doe412; doe234; end
          end;
      end;
end;

procedure tekenopen(dh:single); {----------------------------------------------}
var x,y:integer;
begin
  for y := 1 to ny do
   begin
     xy3dmove(datap^[1,0],datap^[0,y],datap^[1,y]+dh);
     for x := 2 to nx do
        xy3ddraw(datap^[x,0],datap^[0,y],datap^[x,y]+dh); end;
  for x := 1 to nx do
   begin
     xy3dmove(datap^[x,0],datap^[0,1],datap^[x,1]+dh);
     for y := 2 to ny do
        xy3ddraw(datap^[x,0],datap^[0,y],datap^[x,y]+dh); end;
end;

procedure rasterbottom; {------------------------------------------------------}
var x,y : integer;
begin
  for y := 2 to ny-1 do
   begin
     xy3dmove(datap^[1,0],datap^[0,y],dikte);
     xy3ddraw(datap^[nx,0],datap^[0,y],dikte);
   end;
  for x := 2 to nx-1 do
   begin
     xy3dmove(datap^[x,0],datap^[0,1],dikte);
     xy3ddraw(datap^[x,0],datap^[0,ny],dikte);
   end;
end;

procedure xlijn(y,d,o:integer);{-----------------------------------------------}
var x,yy,xx : integer;
    t,tt : single;
begin
  with xycanvas do begin
  tt := datap^[1,y];
  if dowmf then movewmf(xps[1,y],yps[1,y]) else moveto(xps[1,y],yps[1,y]);
  for x := 2 to nx do
    begin
      t := datap^[x,y];
      if hasempty and ((t=empty) or (tt=empty)) then
        if dowmf then movewmf(xps[x,y],yps[x,y]) else moveto(xps[x,y],yps[x,y])
      else
        begin
         if lyncol then xypen.color := color((colsp^[x,y]+colsp^[x-1,y])/2);
         if dowmf then begin checkwmfpen; drawwmf(xps[x,y],yps[x,y]); end
          else lineto(xps[x,y],yps[x,y]);
        end;
      tt := t;
    end;
  if (d=0) then exit;
  yy := y + d; if (yy<1) or (yy>ny) then exit;

  for x := 1 to nx do
    begin
      if (o<0) then xx := nx+1-x else xx := x;
      t := datap^[xx,y]; tt := datap^[xx,yy];
      if not ( hasempty and ((t=empty) or (tt=empty)) ) then
        begin
          if lyncol then xypen.color := color((colsp^[xx,y]+colsp^[xx,yy])/2);
          xyline(-1,xps[xx,y],yps[xx,y],xps[xx,yy],yps[xx,yy]);
        end;
    end;
  end;
end;
procedure ylijn(x,d,o:integer);{-----------------------------------------------}
var y,xx,yy : integer;
    t,tt : single;
begin
  with xycanvas do begin
  tt := datap^[x,1];
  if dowmf then movewmf(xps[x,1],yps[x,1]) else moveto(xps[x,1],yps[x,1]);
  for y := 2 to ny do
    begin
      t := datap^[x,y]; tt := datap^[x,y-1];
      if hasempty and ((t=empty) or (tt=empty)) then
        if dowmf then movewmf(xps[x,y],yps[x,y]) else moveto(xps[x,y],yps[x,y])
      else
      begin
        if lyncol then xypen.color := color((colsp^[x,y]+colsp^[x,y-1])/2);
        if dowmf then begin checkwmfpen; drawwmf(xps[x,y],yps[x,y]); end
         else lineto(xps[x,y],yps[x,y]);
      end;
      {tt := t;}
    end;
  if (d=0) then exit;
  xx := x + d; if (xx<1) or (xx>nx) then exit;

  for y := 1 to ny do
    begin
      if (o<0) then yy := ny+1-y else yy := y;
      t := datap^[x,yy]; tt := datap^[xx,yy];
      if not ( hasempty and ((t=empty) or (tt=empty)) ) then
        begin
          if lyncol then xypen.color := color((colsp^[x,yy]+colsp^[xx,yy])/2);
          xyline(-1,xps[x,yy],yps[x,yy],xps[xx,yy],yps[xx,yy]);
        end;
    end;
  end;
end;
procedure berekenpunten; {-----------------------------------------------------}
var i,x,y : integer;
begin
  setlength(xps,nx+1); setlength(yps,nx+1);
  for i := 0 to nx do
    begin setlength(xps[i],ny+1); setlength(yps[i],ny+1); end;
  for x := 1 to nx do for y := 1 to ny do
    begin
      t := datap^[x,y]; if hasempty then if (t=empty) then t := zmid; 
      position(datap^[x,0],datap^[0,y],t,xp,yp);
        xps[x,y] := xp; yps[x,y] := yp; end;
end;
begin {------------------------------------------------------------------------}
  if (polartype>0) then exit;
  if (mode<0) or (mode>6) then mode := 0;
  foto := (mode=6); nogrid := foto and (opt and 1 = 0);
  trans := (opt and 2 > 0) and (not transoff);
  lyncol := (opt and 4 > 0){ and (mode>1)};
  if (mode<>5) then fcol := 0;
  if lyncol and (fcol>0) then nogrid := true;
  back := trans; if nogrid and (transopt and 2=0) then back := false;
  if stereo then begin lyncol := false; fcol := 0; end;
  cmode := 2;

  if (col1<0) then col1 := frontcolor;
  if (col2<0) then col2 := backcolor;
  if (col3<0) then col3 := backcolor;
  with lastsymbol do
   begin xp := 0; yp := 0; cd3d := 20+mode; size := 0; fill := 0;
     width := xypen.width; color := col1; cl1 := col2; cl2 := col1; cl3 := -1;
   if (mode=6) then
     begin
       if (zonmode<>1) then cl1 := col3 else cl1 := mixcolor(col3,0,1,1);
       cl2 := cl1;
     end;
   if (mode>=5) then fill := 3;
   if nogrid then color := cl1;
  end;
  if (not solid) and (dikte<0) then exit; {alleen symbool}
  if not (ok3d and surf) then exit;       {geen oppervlak}

  if (drmode<1) then nosplit := true;
  if hasempty then begin dikte := 0; solid := false; end;
  dik := (dikte>0) and not solid;
  if dik then
  if (just>0) then begin dh1 := 0; dh2 := -dikte; end else
  if (just<0) then begin dh1 := dikte; dh2 := 0; end else
    begin dh1 := dikte/2; dh2 := -dh1; end
  else begin dh1 := 0; dh2 := 0; dh := 0; end;
  if (mode>4) then
    if (dik or solid) or (drmode>0) then begin lyncol := false; fcol := 0; end;
  if stereo and (mode>4) then
    if not (dik or solid) then mode := 4 else mode := 7;

  xypen.color := col1; xybrush.color := col2;

  if dowmf then checkwmfpen;

  if (mode=0) then outline;

  if mode in [1..4] then
    for stm := 1 to 2 do if (stm=1) or stereo then begin

  if stereo then
    begin setstereo(stm); col1 := stereocol[stm]; end;
  berekenpunten;

  if (mode=1) then with xycanvas do
    for x := 1 to nx do for y := 1 to ny do
      begin
        t := datap^[x,y]; if not hasempty or (t<>empty) then
          begin
           if not lyncol then col := col1 else col := color(colsp^[x,y]);
           xypixel(xps[x,y],yps[x,y],col);
          end;
      end;
  if (mode = 2) then if oct in [3,4,5,6] then for y := 1 to ny do xlijn(y,0,0)
        else for y := ny downto 1 do xlijn(y,0,0);
  if (mode = 3) then if oct in [5,6,7,8] then for x := 1 to nx do ylijn(x,0,0)
       else for x := nx downto 1 do ylijn(x,0,0);

  if (mode = 4) then if not lyncol then
    begin for y := 1 to ny do xlijn(y,0,0);
       for x := nx downto 1 do ylijn(x,0,0); end
  else
    begin
     if (oct in [1,4,5,8]) then
       for y := 1 to ny do
         begin
           yy := y; d := 1;
           if oct in [1,8] then begin yy := ny+1-y; d := -1; end;
           o := 1 ; if oct in [1,4] then o := -1;
           xlijn(yy,d,o);
         end
     else
       for x := 1 to nx do
         begin
           xx := x; d := 1;
           if oct in [2,3] then begin xx := nx+1-x; d := -1; end;
           o := 1 ; if oct in [2,7] then o := -1;
           ylijn(xx,d,o);
         end;
    end;

  end;

  if mode in [5,6] then
    begin
      if not dik then berekenpunten;
      if nogrid then xypen.color := col3;
      if back and dik then begin tekenopen(dh1); tekenopen(dh2); end;
      if back and (dik or solid) then
        begin tekenrandx(false,true); tekenrandx(true,true);
              tekenrandy(false,true); tekenrandy(true,true); end;
      xybrush.color := col2;
      if oct in [1,4,5,8] then
         for y := 1 to ny-1 do for x := 1 to nx-1 do tekenvak(x,y,mode-4)
       else
         for x := 1 to nx-1 do for y := 1 to ny-1 do tekenvak(x,y,mode-4);
      if dik or solid then tekenrandy(oct in [1,2,3,4],false);
      if dik or solid then tekenrandx(oct in [1,2,7,8],false);
      if solid then closebottom;
    end;

  if (mode=7) then
    begin
      if dik then begin tekenopen(dh1); tekenopen(dh2); end
        else tekenopen(0);
      for stm := 1 to 2 do begin setstereo(stm);
        tekenrandx(false,true); tekenrandx(true,true);
        tekenrandy(false,true); tekenrandy(true,true);
      end;
      if solid then rasterbottom;
    end;

end;

{==============================================================================}
{======== Procedures voor cluster weergave ====================================}
{==============================================================================}

procedure xy3dplotcluster(var dat:Tdatatype;mode,st,sz:integer;col1,col2:Tcolor;opt:integer);
var i,t,j,n : integer;
    d : double;
    dist : array of double;
    order : array of integer;
    clr : boolean;
    cl1,cl2,col3 : Tcolor;
begin
  n := length(dat); if (n<2) then exit; clr := (n>3);
  n := length(dat[0]);

  if (col1<0) and (col2<0) then
     begin col1 := frontcolor; col2 := backcolor; end;

  setlength(dist,n); setlength(order,n);
  for i := 0 to n-1 do order[i] := i;

  for i := 0 to n-1 do with polyhface[i] do {bepaal afstanden}
    dist[i] := sqr(viewx-dat[0,i]) + sqr(viewy-dat[1,i]) + sqr(viewz-dat[2,i]);

  for i := 1 to n-1 do  {sorteer op afstand}
    begin t := order[i]; d := dist[t]; j := i;
      while (j>0) and (dist[order[j-1]]<d) do
             begin order[j] := order[j-1]; dec(j); end;
       order[j] := t;
    end;

  for i := 0 to n-1 do
    begin
      t := order[i]; if clr then
        begin
          col3 := color(dat[3,t]);
          if (mode=0) then begin cl1 := col3; cl2 := -1;   end else
          if (mode=1) then begin cl1 := col1; cl2 := col3; end else
                           begin cl1 := col3; cl2 := col3; end;
        end
      else
        if (mode=0) then begin cl1 := col1; cl2 := -1;   end else
        if (mode=1) then begin cl1 := col1; cl2 := col2; end else
                         begin cl1 := col2; cl2 := col2; end;
      xy3dsymbol(dat[0,t],dat[1,t],dat[2,t],st,sz,cl1,cl2,false,false,0);
    end;
end;

{==============================================================================}
{======== Procedures voor algemene weergave ===================================}
{==============================================================================}

procedure xy3dsettransmode(mode:integer;level:single;opt:integer); {XXXXXXXXXXX}
begin
  transmode := mode; transval := level; transopt := opt;
  if (transmode<0) then transmode := 0 else
  if (transmode>3) then transmode := 1;
  if (doprint or dowmf) and (transmode>1) then
    begin
      transmode := 1;
      if (mode=3) then transopt := 0;
    end;
  transoff := (transmode=0) or ( (transmode=2) and (level<1/16) );
end;

procedure xy3dsetframe(x1,x2,y1,y2,z1,z2,yfac,zfac,scale:single;opt:integer); {}
  {x1,x2,y1,y2,z1,z2 = afmetingen
   yfac,zfac = rel factor y en z
   scale = vergrotingsfactor
   opt : opties }
var t : single;
begin
  okfr := false; ok3d := false; frame := false; polartype := 0;
  with fs3d[1] do if go then begin x1 := min; x2 := max; end;
  with fs3d[2] do if go then begin y1 := min; y2 := max; end;
  with fs3d[3] do if go then begin z1 := min; z2 := max; end;
  with plotfield do
    begin xp1 := x1; xp2 := x2; yp1 := y1; yp2 := y2; end;
  if (xp2<=xp1) or (yp2<=yp1) then exit;
  if (x1=x2) or (y1=y2) or (z1=z2) then exit;
  if (x1>x2) then begin t := x1; x1 := x2; x2 := t; end;
  if (y1>y2) then begin t := y1; y1 := y2; y2 := t; end;
  if (z1>z2) then begin t := z1; z1 := z2; z2 := t; end;
  if (yfac=0) then yfac := 1;
  if (zfac=0) then zfac := 1;
  if (scale=0) then scale := 1;
  xmi := x1; xma := x2; xmid := (x1+x2)/2; lastx := xmid;
  ymi := y1; yma := y2; ymid := (y1+y2)/2; lasty := ymid;
  zmi := z1; zma := z2; zmid := (z1+z2)/2; lastz := zmid;
  facx := 2/(x2-x1);
  if (yfac>0) then facy := yfac*facx else facy := 2/(y2-y1)*abs(yfac);
  if (zfac>0) then facz := zfac*facx else facz := 2/(z2-z1)*abs(zfac);
  scale3d := scale; okfr := true;
  mode3d := true; graphs3d[igraph].m3d := mode3d;
end;

procedure xy3dcylframe(r1,r2,z1,z2,zfac,scale:single;rev:boolean;opt:integer); {}
  {r1,r2,z1,z2 = afmetingen
   zfac = rel factor z
   scale = vergrotingsfactor
   opt : opties }
var t : single;
begin
  okfr := false; ok3d := false; frame := false;
  polartype := 3; polrev := rev;
  with fs3d[3] do if go then begin z1 := min; z2 := max; end;
  with fs3d[4] do if go then begin r1 := min; r2 := max; end;
  with plotfield do
    begin xp1 := x1; xp2 := x2; yp1 := y1; yp2 := y2; end;
  if (xp2<=xp1) or (yp2<=yp1) then exit;
  if (r1=r2) or (z1=z2) then exit;
  if (z1>z2) then begin t := z1; z1 := z2; z2 := t; end;
  if (zfac=0) then zfac := 1;
  if (scale=0) then scale := 1;
  xmi := -1; xma := 1; xmid := 0; lastx := xmid;
  ymi := -1; yma := 1; ymid := 0; lasty := ymid;
  zmi := z1; zma := z2; zmid := (z1+z2)/2; lastz := zmid;
  rmi := r1; rma := r2; rmix := min(r1,r2); rmax := max(r1,r2);
  facx := rmax-rmix; facy := facx;
  if (zfac>0) then facz := zfac*facx else facz := 2/(z2-z1)*abs(zfac);
  scale3d := scale; okfr := true;
  mode3d := true; graphs3d[igraph].m3d := mode3d;
end;

procedure xy3dspherframe(r1,r2,scale:single;rev:boolean;opt:integer); {XXXXXXXX}
  {r1,r2 = afmetingen
   scale = vergrotingsfactor
   opt : opties }
begin
  okfr := false; ok3d := false; frame := false;
  sphcor := (opt and 1) > 0; if sphcor then
    begin rev := false; r1 := 0; r2 := abs(r2); end;
  polartype := 4; polrev := rev;
  with fs3d[4] do if go then begin r1 := min; r2 := max; end;
  with plotfield do
    begin xp1 := x1; xp2 := x2; yp1 := y1; yp2 := y2; end;
  if (xp2<=xp1) or (yp2<=yp1) then exit;
  if (r1=r2) then exit;
  xmi := -1; xma := 1; xmid := 0; lastx := xmid;
  ymi := -1; yma := 1; ymid := 0; lasty := ymid;
  zmi := -1; zma := 1; zmid := 0; lastz := zmid;
  rmi := r1; rma := r2; rmix := min(r1,r2); rmax := max(r1,r2);
  facx := 1; facy := facx; facz := facx;
  scale3d := scale; okfr := true;
  mode3d := true; graphs3d[igraph].m3d := mode3d;
end;

procedure xy3dsetview(pos,height:integer;dis:single;xoff,yoff:integer; {XXXXXXX}
   xc,yc,zc:single;opt:integer);
  {pos,height = kijk positie en - hoek
   dis = relatieve kijk afstand (>1, 0=geen perspectief)
   xoff,yoff : verschuiving beeld
   xc,yc,zc : punt voor kijkrichring, 0 = default
   opt : opties
     bit 1 = centre zoom}

{const plane: array[1..12] of string =
   ('X-Y','-Y-X','-XY','YX',
    'XZ','-YZ','-XZ','YZ',
    'XY','-YX','-X-Y','Y-X');}
var f : single;
    a : integer;
begin
  ok3d := false; if not okfr then exit;
  while (pos>=360) do dec(pos,360);
  while (pos<-0) do inc(pos,360);
  if (pos=0) then oct := 1 else oct := (pos-1) div 45 + 1;
  if (height>90) then height := 90;
  if (height<-90) then height := -90;
  if (dis<=1e-6) then dis := 0 else
    if (dis<1) then dis := 1;
  kijkhoek := pos; kijkhoogte := height;
  zoomc := (opt and 1 >0);

  cosp := cos(pos*grtorad);     if (abs(cosp)<0.01) then cosp := 0;
  sinp := sin(pos*grtorad);     if (abs(sinp)<0.01) then sinp := 0;
  cosa := cos(height*grtorad);  if (abs(cosa)<0.01) then cosa := 0;
  sina := sin(height*grtorad);  if (abs(sina)<0.01) then sina := 0;
  sinr := 0; cosr := 1;
  diam := sqrt( sqr((xma-xmi)*facx) + sqr((yma-ymi)*facy) +
             sqr((zma-zmi)*facz) );
  fac3d := min(xp2-xp1,yp2-yp1) / diam * scale3d;
  imsize := round(fac3d*diam);
  if (dis<=0) then r3d := 0 else r3d := diam/2*dis;
  xc0 := (xp1+xp2) div 2; xpc := xc0 + round(xoff/100*(xp2-xp1)); xpc0 := xpc;
  yc0 := (yp1+yp2) div 2; ypc := yc0 + round(yoff/100*(yp2-yp1));
  dead := 0; if (r3d=0) then
    if ((height+90) mod 90 = 0) then
     if (polartype=3) then
       begin
         if (height=90) then dead := 13 else
         if (height=-90) then dead := 14 else dead := 0
       end
     else if (pos mod 90 = 0) then
       dead := (pos div 90) + ( (height+90) div 90 )*4 + 1;
  if (dis=0) then stdx := 0
   else stdx := min(xp2-xp1,yp2-yp1) * scale3d / dis * 2;

  cszk := sina; csxk := -cosa*sinp; csyk := -cosa*cosp;

  if (r3d=0) then f := 1000 else f := r3d;
  if (xc<>0) then xmid := xc;
  if (yc<>0) then ymid := yc;
  if (zc<>0) then zmid := zc;
  viewz := zmid + (f * sina)/facz;
  viewx := xmid - (f * cosa * sinp)/facx;
  viewy := ymid - (f * cosa * cosp)/facy;
  xy3dviewx := viewx; xy3dviewy := viewy; xy3dviewz := viewz;
  xy3dviewr := 0; xy3dviewa := 0; xy3dviewh := 0;
  if polrev then a := 90+pos else a := 270-pos; a := (a+360) mod 360;

  if (polartype=3) then
    begin
      xy3dviewr := rmi + sqrt(sqr(viewx)+sqr(viewy))*(rma-rmi);
      xy3dviewa := a;
    end;
  if (polartype=4) then
    begin
      xy3dviewr := rmi + sqrt(sqr(viewx)+sqr(viewy)+sqr(viewz))*(rma-rmi);
      xy3dviewa := a;
      if sphcor then xy3dviewh := 90-height else xy3dviewh := height;
    end;

  ifac3d := 1/fac3d;
  if (r3d<>0) then ir3d := 1/r3d else ir3d := 0;
  ifacx := 1/facx; ifacy := 1/facy; ifacz := 1/facz;
  if (sinp<>0) then isinp := 1/sinp else isinp := 0;
  if (cosp<>0) then icosp := 1/cosp else icosp := 0;
  if (sina<>0) then isina := 1/sina else isina := 0;
  if (cosa<>0) then icosa := 1/cosa else icosa := 0;

  ok3d := true;
  xy3dsetsun(70,20,1,1);
  pixx3d := facx*fac3d; pixy3d := facy*fac3d; pixz3d := facz*fac3d;
  if (height>=0) then zfr := zmi else zfr := zma;
  if oct in [1,2,3,4] then xfr := xma else xfr := xmi;
  if oct in [1,2,7,8] then yfr := yma else yfr := ymi;
end;

procedure xy3dsetview(pos,height:integer;dis:single;opt:integer); {============}
begin xy3dsetview(pos,height,dis,0,0,0,0,0,opt); end;

procedure xy3dsetstereo(ang,phase:single;grcol,opt:integer); {=================}
var a,ca,sa,cb,sb,cc,sc,cn,sn,cd,sd,hk,hg,h,aa : single;
    dx : integer;
begin
  stereo := false;
  if (grcol<0) then grcol := 0;
  if (grcol>255) then stereocol[2] := grcol else
  case grcol of
    1 : stereocol[2] := $80ff00;
    2 : stereocol[2] := $ffff00;
    3 : stereocol[2] := $ff8000;
  else  stereocol[2] := $00ff00;
  end;
  stereocol[1] := $0000ff; stereocol[0] := stereocol[1] or stereocol[2];
  stereocol[3] := stereocol[0];

  if (backcolor<>clblack) then
    begin
      xypen.color := clblack; xybrush.color := clblack;
      with graphfield do xyrectangle(0,0,x1,y1,x2+1,y2+1);
    end;
  backcolor := clblack; frontcolor := stereocol[0];
  stereo := true; nostlabels := (opt and 1 = 1);

   {    /|    bereken nieuwe hoeken }
   { c / | b  met regels van Napier }
   {  /  |    }
   { -----    }
   {   a      }

  hk := kijkhoek*grtorad; hg := kijkhoogte*grtorad;
  if (abs(ang)<0.1) or (stdx=0) then
    begin
      h := 0; sc := cosa; cc := abs(sina); cd := 1; sd := 0;
    end
  else
    begin
      a := ang/2*grtorad;
      ca := cos(a); sa := sin(a); {halve hoek}
      cb := sina; sb := cosa;{hoogte vanaf 90}
      cc := abs(cb) * ca; sc := sqrt(1-sqr(cc)); {echte hoogte vanaf 90}
      cn := (abs(sb)/sc)*ca; sn := sqrt(1-sqr(cn)); {nieuwe halve tophoek}
      h := arccos(cn); if (a<0) then h := - h;{id}
      sd := sn*cb; cd := sqrt(1-sqr(sd)); {draaiings hoek}
    end;
  if (hg<0) then cc := -cc;
  dx := round(stdx*sa*phase*2);

  with stereoang[0] do begin sp := sinp; cp := cosp;
    sh := sina; ch := cosa; cr := 1; sr := 0; xc := xpc0; end;
  with stereoang[1] do
    begin aa := hk+h; sp := sin(aa); cp := cos(aa);
      sh := cc; ch := sc; cr := cd; sr := -sd; xc := xpc0+dx div 2;
     end;
  with stereoang[2] do
    begin aa := hk-h; sp := sin(aa); cp := cos(aa);
      sh := cc; ch := sc; cr := cd; sr := sd; xc := xpc0 +dx div 2 - dx;
    end;
  with stereoang[3] do begin sp := sinp; cp := cosp;
    sh := sina; ch := cosa; cr := 1; sr := 0; xc := xpc0; end;
  stereomode := 0; xybuffers[0].str := true;
end;

procedure xy3dsetstereo(ang,phase:single;grcol:integer); {=====================}
begin xy3dsetstereo(ang,phase,grcol,1); end;

{==============================================================================}

var grofs : array[1..3] of single;
    nachs : array[1..3] of integer;
    sttel : integer;
    xx,yy : array[0..3] of single;
    zz : array[0..1] of single;

function formtext(w:single;ax:integer):string; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var s : string;
begin
  with fs3d[ax] do begin
    if fix then str(w:1:ndec,s) else
    begin w := w * fac; str(w:1:ndec,s); s := s + exp; end;
  end;
  result := s;
end;

procedure xy3daxisscale(min,max:single;nc:integer;fm:single;fix:boolean;ax:integer);
 {set as met vaste indeling}
var n : integer;
    w : single;
begin
  if (ax<1) or (ax>4) then exit;
  if abs(max-min)<=(abs(max)+abs(min))*1e-6 then
    begin min := min-1; max := min+2; end;
  if (ax<>4) then if (max<min) then begin w := min; min := max; max := w; end;
  if (nc<1) then nc := 1;

  fs3d[ax].min := min; fs3d[ax].max := max; fs3d[ax].fix := fix;
  fs3d[ax].nc := nc; fs3d[ax].nf := 0; fs3d[ax].fm := fm;
  fs3d[ax].go := true;

  n := trunc(abs(fm)); w := 1;
  w := power(10,n); if (fm>0) then w := 1/w;
  if (fm<0) then n := -n;
  fs3d[ax].ndec := round(frac(abs(fm))*10);
  fs3d[ax].fac := w;
  if (prmi(4)>0) then fs3d[ax].exp := 'e'+inttostr(n)
    else fs3d[ax].exp := 'E'+inttostr(n);
end;

procedure verdeling3d(span:single;len,dis:integer; var grof: single; {XXXXXXXXX}
  var nach:integer; stm,ax : integer);
var fac,scale : single;
    n : integer;
    fs : boolean;
begin
   inc(sttel); fs := false;
   if (stm=2) then begin grof := grofs[sttel]; nach := nachs[sttel]; exit; end;
   if (ax in [1..4]) then with fs3d[ax] do if go then
     begin grof := abs(max-min)/nc; nach := ndec; fs := true; end;
   if not fs then begin
   if (dis=0) then dis := 1; n := round(len/dis);
   if (n<1) then n := 1;
   scale := span/(n+1e-6); fac := 1; nach:=0;
   while (scale>=5) do begin scale := scale/10; fac := fac*10; dec(nach); end;
   while (scale<0.5) do
      begin scale := scale*10; fac := fac/10; inc(nach); end;
   if (scale<1) then grof := fac else
   if (scale<2) then grof := 2*fac else
     grof := 5*fac;
   if (nach<0) then nach := 0;  end;
   grofs[sttel] := grof; nachs[sttel] := nach;
end;

procedure xy3dsetlabels(xl,yl,zl,xrl,yrl,zrl:string;lbl3d:boolean; {XXXXXXXXXXX}
   xm,ym,zm:integer;scl3d:single;opt:integer);
  {set labels en modes}
  {opt 1 = verticale z label}
  {opt 2 = anti-alias}
  {opt 4 = wmf altijd anti-alias}
procedure process(var tl,trl,tlabel,trlabel:string;c:char);
begin
  if (tl='') and (trl='') then begin tlabel := c; trlabel := '-'+c; exit; end;
  if (tl='')  then tlabel := c       else if (tl=' ')  then tlabel := ''  else tlabel := tl;
  if (trl='') then trlabel := tlabel else if (trl=' ') then trlabel := '' else trlabel := trl;
end;
begin
  process(xl,xrl,xlabel,xrlabel,'X');
  process(yl,yrl,ylabel,yrlabel,'Y');
  process(zl,zrl,zlabel,zrlabel,'Z');
  labels3d := lbl3d; size3d := scl3d;
  if (size3d<=0) then size3d := 1;
  xlmode := xm; ylmode := ym; zlmode := zm;
  if not labels3d then
    begin
     if (xlmode>2) then xlmode := 2;
     if (ylmode>2) then ylmode := 2;
     if (zlmode>2) then zlmode := 2;
    end;
  if (xlmode<0) or (xlmode>3) then xlmode := 0;
  if (ylmode<0) or (ylmode>3) then ylmode := 0;
  if (zlmode<0) or (zlmode>3) then zlmode := 0;
  zlvert := (opt and 1 >0);
  antial := (opt and 2 >0);
  if {stereo or} doprint then antial := false;
  if dowmf then if (res>1) and (opt and 4 =0) then antial := false;
end;

procedure xy3dsetlabels(xl,yl,zl,xrl,yrl,zrl:string;lbl3d:boolean;
   xm,ym,zm:integer;opt:integer);
begin xy3dsetlabels(xl,yl,zl,xrl,yrl,zrl,lbl3d,xm,ym,zm,0,opt); end;

procedure init3dlabels(t,h,ch:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var f0,f1,f2,ff:single;
    l : integer;
    ok : boolean;
begin
  f0 := 1;
  l := (t+2) and 3; ff := factor(xx[l],yy[l],zz[h]);
  if (ff>f0) then f0 := ff;
  l := (t+1) and 3; f1 := factor(xx[l],yy[l],zz[1-h]);
  l := (t+3) and 3; f2 := factor(xx[l],yy[l],zz[1-h]);
  if (zlmode<2) then ff := min(f1,f2) else ff := max(f1,f2);
  if (ff>f0) then f0 := ff;
  f0 := f0 * (ch*6+imsize)/imsize;
  f0 := f0 * size3d; if (f0<1) then f0 := 1;
  inittext3d(f0,xmi,xma,ymi,yma,ok);
  if not ok then labels3d := false;
end;

procedure plotzlabels(t,h,stm,mode:integer;dotext,doline,doxyz:boolean); {XXXXX}
 {tekent z-as labels voor cartesisch en cylindrisch frame}
var i,t1,t2,t3,tz,nd,l,ml,zh,dmode,d,cw,ch,tl: integer;
    x0,y0,x1,y1,x2,y2,xt,yt,xmn,xmx,ymn,ymx : integer;
    gr,p : single;
    s : string;
const c = '|';
begin
  if doline then dmode := 0 else dmode := 8;
  if zlvert and dotext and (not labels3d) then
    for i := length(zlabel) downto 2 do insert(c,zlabel,i);
  cw := xycanvas.textwidth('0'); ch := xycanvas.textheight('0');
  if odd(oct) then
       begin t1 := (t+3) mod 4; t2 := t; t3 := (t+1) mod 4; end
  else begin t1 := (t+1) mod 4; t2 := t; t3 := (t+3) mod 4; end;
  position(xx[t1],yy[t1],zz[0],x1,y1); position(xx[t1],yy[t1],zz[1],x2,y2);
  if (zlmode<2) then tz := t1 else tz := t3;
  if odd(oct) then zh := slope(x1,y1,x2,y2) else zh := slope(x2,y2,x1,y1);
  if labels3d then
    begin
      if odd(zlmode) then d := round(prm(121)*ch) else d := round(prm(120)*cw);
      verdeling3d(zma-zmi,round((zma-zmi)*pixz3d),d,gr,nd,stm,3);
    end
  else
    begin
     if zlvert and (zlmode=2) then d := round(prm(121)*ch)
         else d := round(prm(120)*cw);
     l := round(sqrt(sqr(x2-x1)+sqr(y2-y1)));
     verdeling3d(zma-zmi,l,d,gr,nd,stm,3);
     tl := round(prm(122)*ch);
    end;
  if fs3d[3].go then p := fs3d[3].min else p := gr*trunc(zmi/gr-1);
  ml := 0; xmx := max(x1,x2); xmn := min(x1,x2); ymx := max(y1,y2); ymn := min(y1,y2);
  repeat
    position(xx[t1],yy[t1],p,xt,yt);
    if  ( (xt<=xmx) and (xt>=xmn) and (yt<=ymx) and (yt>=ymn) ) then
      begin
        position(xx[t2],yy[t2],p,x0,y0);
        if not dotext then s := '' else
          if fs3d[3].go then s := formtext(p,3) else str(p:1:nd,s);
        if (length(s)>ml) then ml := length(s);
        if labels3d then text3d(s,xx[tz],yy[tz],p,zlmode+9,false,oct,ml,zlvert,prm(122))
        else
        if (zlmode=2) and zlvert then text3z(s,xt,yt,x0,y0,tl,doline)
              else text2(s,xt,yt,x0,y0,tl,zh,zlmode+dmode,true);
        if (mode=4) and doline then
          begin
            xyline(-1,xt,yt,x0,y0);
            position(xx[t3],yy[t3],p,x0,y0);
            if dowmf then drawwmf(x0,y0) else xycanvas.lineto(x0,y0);
          end;
      end;
    p := p + gr;
  until (p>zma+gr);
  if doxyz and dotext then
    if labels3d then text3d(zlabel,xx[tz],yy[tz],zmid,zlmode+9,true,oct,ml,zlvert,prm(122))
    else begin
      position(xx[t1],yy[t1],zmid,xt,yt); position(xx[t2],yy[t2],zmid,x0,y0);
      if not zlvert then      text1(zlabel,xt,yt,x0,y0,tl+cw*(ml+2), zh,zlmode)
      else if (zlmode=2) then text1(zlabel,xt,yt,x0,y0,tl+cw*(ml+2), zh,4)
                         else text1(zlabel,xt,yt,x0,y0,tl+cw*(ml+2),-90,4);
    end;
end;

procedure plotrlabels(t,h,stm:integer;dotext,doline,doxyz:boolean); {XXXXXXXXXX}
 {tekent r-as labels voor sferisch en cylindrisch frame}
var t1,t2,t3,xh,cw,ch,d,nd,l,ml,i,tl : integer;
    x0,y0,x1,y1,x2,y2,xt,yt : integer;
    f,r1,r2,gr,p,pp : single;
    s : string;
begin
  cw := xycanvas.textwidth('0'); ch := xycanvas.textheight('0');
  case oct of
    2,3 : t1 := 1; 4,5 : t1 := 2;
    6,7 : t1 := 3; 8,1 : t1 := 0;
  end;
  t2 := (t1 + 3) mod 4; t3 := (t1+2) mod 4;
  if (rmi>rma) then f := -1 else f := 1;
  r1 := rmi*f; r2 := rma*f;
  position(xx[t1],yy[t1],zz[h],x1,y1); position(xx[t2],yy[t2],zz[h],x2,y2);
  xh := slope(x2,y2,x1,y1);
  if labels3d then
    begin
      if odd(xlmode) then d := round(prm(131)*ch) else d := round(prm(130)*cw);
      verdeling3d(r2-r1,round((r2-r1)*pixx3d),d,gr,nd,stm,4)
    end
   else
    begin
      l := round(sqrt(sqr(x2-x1)+sqr(y2-y1))/2);
      verdeling3d(r2-r1,l,round(prm(130)*cw),gr,nd,stm,4);
      tl := round(prm(132)*ch);
    end;

  if fs3d[4].go then p := fs3d[4].min*f else p := gr*trunc(r1/gr-1); ml := 0;
  repeat
    pp := (p-r1)/(r2-r1);
    if (pp>-0.001) and (pp<1.001) then
    for i := 0 to 1 do
    begin
      if (i=1) then pp := -pp;
      if not dotext then s := '' else
        if fs3d[4].go then s := formtext(p*f,4) else str(p*f:1:nd,s);
      if (length(s)>ml) then ml := length(s);
      if oct in [1,4,5,8] then begin
        if labels3d then text3d(s,pp,-yy[t],zz[h],xlmode+1,false,oct,ml,zlvert,prm(132))
        else
          begin
             position(pp,-yy[t],zz[h],xt,yt); position(pp,yy[t],zz[h],x0,y0);
             text2(s,xt,yt,x0,y0,tl,xh,xlmode,false); end; end
      else begin
        if labels3d then text3d(s,-xx[t],pp,zz[h],xlmode+5,false,oct,ml,zlvert,prm(132))
        else
          begin position(-xx[t],pp,zz[h],xt,yt); position(xx[t],pp,zz[h],x0,y0);
            text2(s,xt,yt,x0,y0,tl,xh,xlmode,false); end; end;
    end;
    p := p + gr;
  until (p>r2+gr); if (xlmode>1) and not labels3d then ml := 3;

  if doxyz and dotext then
   if oct in [1,4,5,8] then
    if labels3d then text3d(xlabel,0,-yy[t],zz[h],xlmode+1,true,oct,ml,zlvert,prm(132))
    else begin position(0,-yy[t],zz[h],xt,yt); position(0,yy[t],zz[h],x0,y0);
       text1(xlabel,xt,yt,x0,y0,tl+cw*(ml+2),xh,xlmode); end
   else
    if labels3d then text3d(xlabel,-xx[t],0,zz[h],xlmode+5,true,oct,ml,zlvert,prm(132))
    else begin position(-xx[t],0,zz[h],xt,yt); position(xx[t],0,zz[h],x0,y0);
       text1(xlabel,xt,yt,x0,y0,tl+cw*(ml+2),xh,xlmode); end;
end;

procedure showspherframe(mode:integer;frcol,xycol,xzcol,yzcol:Tcolor;opt:integer);
var t,h,t1,t2,t3,h2,x1,x2,y1,y2,xt,yt,x0,y0,ch,cw,l,nd,ml,i,j,f,stm : integer;
    xh,dmode,m : integer;
    s : string;
    f0, ff, f2, f1 : single;
    gr,p,pp,r1,r2,x,y,z: single;
    grid,doxyz,dothick,dodead,st,doline,dotext : boolean;
    zero : Tpoint3D;
const a2 : array[0..3] of string = ('90','0','270','180');
      a3 : array[0..3] of string = ('270','0','90','180');
begin
  if stereo then begin xycol := -1; xzcol := -1; yzcol := -1; end;

  doxyz := (opt and 1 =0); dothick := (opt and 2 =0); dodead := (opt and 4 =0);
  frame := false; zero := point3D(0,0,0);
  xx[0] := xmi; xx[1] := xmi; xx[2] := xma; xx[3] := xma;
  yy[0] := ymi; yy[1] := yma; yy[2] := yma; yy[3] := ymi;
  zz[0] := zmi; zz[1] := zma;
  if (xlabel='X') then xlabel := 'R';
  if (xrlabel='X') or (xrlabel='-X') then xrlabel := 'R';
  case oct of
    1,2 : t := 2; 3,4 : t := 3;
    5,6 : t := 0; 7,8 : t := 1;
  end;
  if (viewz>=zmid) then h := 0 else h := 1;
  with framexyz do begin x := 0; y := 0; z := 0; end;


  with xycanvas do begin

  ch := textheight('0'); cw := textwidth('0');
  xylinewidth(1); polartype := 0;

  if not stereo then m := 1 else if nostlabels or labels3d then m := 2 else m := 3;
  for stm := 1 to m do begin

  doline := (stm<3); dotext := (stm=3) or (not stereo) or labels3d;
  if stereo then
    begin setstereo(stm); frcol := stereocol[stm]; end;
  if doline then dmode := 0 else dmode := 8;

  sttel := 0;
  if (mode>1) and doline then {teken zijvlakken en initieer endframe}
    begin
      rectxy(xmi,ymi,xma,yma,zz[h],frcol,-1,false,false);
      rectxz(xmi,zmi,xma,zma,yy[t],frcol,-1,false,false);
      rectyz(ymi,zmi,yma,zma,xx[t],frcol,-1,false,false);
      st := stereo; stereo := false;
      xy3dcircle(point3d(0,yy[t],0),zero,1,0,frcol,xzcol,false,false,0);
      xy3dcircle(point3d(0,0,zz[h]),zero,1,0,frcol,xycol,false,false,0);
      xy3dcircle(point3d(xx[t],0,0),zero,1,0,frcol,yzcol,false,false,0);
      stereo := st;

      if (frcol<0) then endframe[stm,0] := frontcolor else endframe[stm,0] := frcol;
      frame := true; t2 := (t+2) mod 4; h2 := 1-h;
      position(xx[t2],yy[t2],zz[h2],endframe[stm,1],endframe[stm,2]);
      position(xx[t2],yy[t2],zz[h],endframe[stm,3],endframe[stm,4]);
      position(xx[t2],yy[t],zz[h2],endframe[stm,5],endframe[stm,6]);
      position(xx[t],yy[t2],zz[h2],endframe[stm,7],endframe[stm,8]);
    end;

  if (frcol<0) then xypen.color := frontcolor else xypen.color := frcol;
  xyfont.color := xypen.color; xybrush.color := backcolor;
  xybrush.style := bsclear;
  if labels3d then init3dlabels(t,h,ch);

  {teken (dikke) XYZ assen}
  if (mode<3) then xyfont.style := [fsbold]; if dothick then xylinewidth(2);

  if (mode<3) then
    begin
      position(xx[t],yy[t],zz[h],x2,y2);
      t2 := (t + 1) mod 4; position(xx[t2],yy[t2],zz[h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if polrev then s := a3[t] else s := a2[t];
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,ylmode);

      position(xx[t],yy[t],zz[h],x2,y2);
      t2 := (t + 3) mod 4; position(xx[t2],yy[t2],zz[h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1); t3 := (t+1) mod 4;
      if polrev then s := a3[t3] else s := a2[t3];
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,ylmode);

      if doline then line3d(xx[t],yy[t],zz[h], xx[t],yy[t],zz[1-h]);
    end
  else
   begin
      position(0,yy[t],zz[h],x2,y2); position(0,yy[t],zz[1-h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if doxyz and dotext then
        begin
         if (oct in [1,2,7,8]) xor polrev then s := '90' else s := '270';
         if labels3d then text3d(s,0,yy[t],zz[1-h],ylmode+13,true,oct,ml,zlvert,1)
          else text3(s,x1,y1,x2,y2,ch,ylmode);
        end;

      position(xx[t],0,zz[h],x2,y2); position(xx[t],0,zz[1-h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if doxyz and dotext then
        begin
         if oct in [1,2,3,4] then s := ' 0 ' else s := '180';
         if labels3d then text3d(s,xx[t],0,zz[1-h],13+ylmode,true,oct,ml,zlvert,1)
         else text3(s,x1,y1,x2,y2,ch,ylmode);
        end;

      if doline then
       begin
        position(xx[t],yy[t],0,x2,y2);
        t2 := (t + 1) mod 4; position(xx[t2],yy[t2],0,x1,y1); xyline(-1,x2,y2,x1,y1);
        t2 := (t + 3) mod 4; position(xx[t2],yy[t2],0,x1,y1); xyline(-1,x2,y2,x1,y1);
        position(0,yy[t],zz[h],x2,y2); position(0,-yy[t],zz[h],x1,y1); xyline(-1,x2,y2,x1,y1);
        position(xx[t],0,zz[h],x2,y2); position(-xx[t],0,zz[h],x1,y1); xyline(-1,x2,y2,x1,y1);
      end;
    end;
  xyfont.style := []; xylinewidth(1);

  if (mode>2) then begin {teken schaal verdeling}

  plotrlabels(t,h,stm,dotext,doline,doxyz);

  if (mode>3) and doline then
    begin
      p := 0.5; pp := sqrt(0.75); z := zz[h]; x := xx[t]; y:= yy[t];
      line3d(p,pp,z, -p,-pp,z); line3d(p,-pp,z, -p,pp,z);
      line3d(pp,p,z, -pp,-p,z); line3d(pp,-p,z, -pp,p,z);
      st := stereo; stereo := false;
      xy3dcircle(point3d(0,0,z),zero,pp,0,frcol,-1,false,false,0);
      xy3dcircle(point3d(0,0,z),zero,p,0,frcol,-1,false,false,0);
      stereo := st; if dowmf then checkwmfpen;

      for j := 1 to 2 do
        begin
          if (j=1) then ff := p else ff := pp;
          position(x,si15[0]*ff,co15[0],xt,yt);
          if dowmf then movewmf(xt,yt) else moveto(xt,yt);
          for i := 1 to 24 do
            begin position(x,si15[i]*ff,co15[i],xt,yt);
            if dowmf then drawwmf(xt,yt) else lineto(xt,yt); end;
          position(si15[0]*ff,y,co15[0],xt,yt);
          if dowmf then movewmf(xt,yt) else moveto(xt,yt);
          for i := 1 to 24 do
            begin position(si15[i]*ff,y,co15[i],xt,yt);
            if dowmf then drawwmf(xt,yt) else lineto(xt,yt); end;
        end;
      line3d(p,y,pp, -p,y,pp);  line3d(pp,y,p,  -pp,y,p);
      line3d(p,y,-pp,-p,y,-pp); line3d(pp,y,-p, -pp,y,-p);
      line3d(x,p,pp, x,-p,pp);  line3d(x,pp,p,  x,-pp,p);
      line3d(x,p,-pp,x,-p,-pp); line3d(x,pp,-p, x,-pp,-p);
    end;

  end;

  end; end;
  polartype := 4;
  if labels3d then exittext3d;
  autoreset(3);
  for i := 1 to 4 do fs3d[i].go := false;
end;

procedure showcylframe(mode:integer;frcol,xycol,xzcol,yzcol:Tcolor;opt:integer);
var t,h,t1,t2,t3,h2,x1,x2,y1,y2,xt,yt,x0,y0,ch,cw,l,nd,ml,i,f,stm : integer;
    zh,xh,dmode,m,tz : integer;
    s : string;
    gr,p,pp,r1,r2: single;
    f0 ,ff,f1,f2 : single;
    grid,doxyz,dothick,dodead,st,doline,dotext : boolean;
const a2 : array[0..3] of string = ('90','0','270','180');
      a3 : array[0..3] of string = ('270','0','90','180');
const c : char = '|';
begin
  doxyz := (opt and 1 =0); dothick := (opt and 2 =0);
  dodead := (opt and 4 =0) and not stereo;
  if stereo then begin xycol := -1; xzcol := -1; yzcol := -1; end;
  frame := false;
  xx[0] := xmi; xx[1] := xmi; xx[2] := xma; xx[3] := xma;
  yy[0] := ymi; yy[1] := yma; yy[2] := yma; yy[3] := ymi;
  zz[0] := zmi; zz[1] := zma;
  if (xlabel='X') then xlabel := 'R';
  if (xrlabel='X') or (xrlabel='-X') then xrlabel := 'R';
  case oct of
    1,2 : t := 2; 3,4 : t := 3;
    5,6 : t := 0; 7,8 : t := 1;
  end;
  if (viewz>=zmid) then h := 0 else h := 1;
  with framexyz do begin x := 0; y := 0; z := zz[h]; end;

  with xycanvas do begin

  ch := textheight('0'); cw := textwidth('0');
  ticx := ch/pixx3d; ticy := ticx; ticz := ch/pixz3d;
  ticx2 := cw/pixx3d; ticy2 := ticx2; ticz2 := cw/pixz3d;
  xylinewidth(1);

  if dodead then if (dead>0) then
    begin
      l := round(facx*fac3d);
      x1 := xpc-l; x2 := xpc+l; y1 := ypc-l; y2 := ypc+l;
      restartgraph(x1,x2,y1,y2,false);
      if (frcol<0) then frcol := frontcolor;
      if (xycol>=0) then xycircle(frcol,xycol,xpc,ypc,l);
      xysetgridlines(1,1,1,1); grid := (mode=4);
      if (dead=13) then
        xypolargraph(frcol,rmi,rma,kijkhoek,0,polrev,false,grid,false)
      else
        xypolargraph(frcol,rmi,rma,-kijkhoek,0,not polrev,false,grid,false);
      exit;
    end;

  if not stereo then m := 1 else if nostlabels or labels3d then m := 2 else m := 3;
  for stm := 1 to m do begin

  doline := (stm<3); dotext := (stm=3) or (not stereo) or labels3d;
  if stereo then
    begin setstereo(stm); frcol := stereocol[stm]; end;
  if doline then dmode := 0 else dmode := 8;

  polartype := 0; sttel := 0;
  if (mode>1) and doline then {teken zijvlakken en initieer endframe}
    begin
      rectxy(xmi,ymi,xma,yma,zz[h],frcol,-1,false,false);
      st := stereo; stereo := false;
      xy3dcircle(point3d(0,0,zz[h]),point3d(0,0,zmid),1,0,frcol,xycol,false,false,0);
      stereo := st;
      rectxz(xmi,zmi,xma,zma,yy[t],frcol,xzcol,false,false);
      rectyz(ymi,zmi,yma,zma,xx[t],frcol,yzcol,false,false);
      if (frcol<0) then endframe[stm,0] := frontcolor else endframe[stm,0] := frcol;
      frame := true; t2 := (t+2) mod 4; h2 := 1-h;
      position(xx[t2],yy[t2],zz[h2],endframe[stm,1],endframe[stm,2]);
      position(xx[t2],yy[t2],zz[h], endframe[stm,3],endframe[stm,4]);
      position(xx[t2],yy[t],zz[h2], endframe[stm,5],endframe[stm,6]);
      position(xx[t],yy[t2],zz[h2], endframe[stm,7],endframe[stm,8]);
    end;

  if (frcol<0) then xypen.color := frontcolor else xypen.color := frcol;
  xyfont.color := xypen.color; xybrush.color := backcolor;
  xybrush.style := bsclear;
  if labels3d then init3dlabels(t,h,ch);

  {teken (dikke) XYZ assen}
  if (mode<3) then xyfont.style := [fsbold]; if dothick then xylinewidth(2);
  if (h=0) then s := zlabel else s := zrlabel;
  if (mode>1) then {twee Z lijnen}
    begin
      position(0,yy[t],zz[h],x2,y2); position(0,yy[t],zz[1-h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if dotext then if doxyz then if (mode>2) then
        begin
          if (oct in [1,2,7,8]) xor polrev then s := '90' else s := '270';
          if labels3d then text3d(s,0,yy[t],zz[1-h],ylmode+13,true,oct,ml,zlvert,1)
          else text3(s,x1,y1,x2,y2,ch,ylmode) {<<XZ}
        end
      else text3(s,x1,y1,x2,y2,ch,zlmode); {Z,XZ}

      position(xx[t],0,zz[h],x2,y2); position(xx[t],0,zz[1-h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if doxyz and dotext then if (mode>2) then
        begin
          if oct in [1,2,3,4] then s := ' 0 ' else s := '180';
          if labels3d then text3d(s,xx[t],0,zz[1-h],13+ylmode,true,oct,ml,zlvert,1)
          else text3(s,x1,y1,x2,y2,ch,ylmode); {<<YZ}
        end
      else text3(s,x1,y1,x2,y2,ch,zlmode); {Z-YZ}
    end
  else
    begin {een Z lijn}
      position(xx[t],yy[t],zz[h],x2,y2); position(xx[t],yy[t],zz[1-h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,zlmode);
    end;

  {teken X en Y lijnen}
  if (mode>1) then {lijnen in midden}
    begin
      position(0,yy[t],zz[h],x2,y2); position(0,-yy[t],zz[h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if (t in [1,2]) xor polrev then s := '270' else s := '90';
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,ylmode); {<<XY}

      position(xx[t],0,zz[h],x2,y2); position(-xx[t],0,zz[h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if (t in [2,3]) then s := '180' else s := '0';
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,ylmode); {<<XY}
    end
  else
    begin
      position(xx[t],yy[t],zz[h],x2,y2);
      t2 := (t + 1) mod 4; position(xx[t2],yy[t2],zz[h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1);
      if polrev then s := a3[t] else s := a2[t];
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,ylmode);

      position(xx[t],yy[t],zz[h],x2,y2);
      t2 := (t + 3) mod 4; position(xx[t2],yy[t2],zz[h],x1,y1);
      if doline then xyline(-1,x2,y2,x1,y1); t3 := (t+1) mod 4;
      if polrev then s := a3[t3] else s := a2[t3];
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,ylmode);
    end;
  xyfont.style := []; xylinewidth(1);

  if (mode>2) then begin {teken schaal verdeling}

  plotrlabels(t,h,stm,dotext,doline,doxyz);

  if (mode>3) and doline then
    begin
      p := sqrt(0.5);
      line3d(p,p,zz[h],-p,-p,zz[h]);
      line3d(p,-p,zz[h],-p,p,zz[h]);
    end;

  {Z-AS} if (r3d>0) or (abs(kijkhoogte)<>90) then
    plotzlabels(t,h,stm,mode,dotext,doline,doxyz);

  end;

  end; end;
  polartype := 3;
  if labels3d then exittext3d;
  autoreset(3);
  for i := 1 to 4 do fs3d[i].go := false;
end;

procedure xy3dshowframe(mode:integer;frcol,xycol,xzcol,yzcol:Tcolor;opt:integer);
 { mode: 0=geen, 1=assen 2=vlakken 3=vlaken met assen 4=3 met rasterlijnen
   frcol : kleur frame
   xycol,xzcol,yzcol : kleur vlakken
   opt : bit 0 set = geen X Y en Z labelling
         bit 1 set = geen dikke lijnen
         bit 2 set = geen dead point view
   }
var t,h,t1,t2,t3,h2,x1,x2,y1,y2,xt,yt,x0,y0,ch,cw,l,nd,ml,stm,tl: integer;
    xh,yh,zh,i,m,mm,dmode,tz,d,xmn,xmx,ymn,ymx : integer;
    s : string;
    gr,p,f,ff,f1,f2 : single;
    grid,doxyz,dothick,dodead,doline,dotext : boolean;
    col : Tcolor;
    a : array[0..3] of string;
const c : char = '|';
begin
  if (not ok3d) or (mode<=0) or (mode>4) then exit;
  if (polartype=3) then
    begin showcylframe(mode,frcol,xycol,xzcol,yzcol,opt); exit; end;
  if (polartype=4) then
    begin showspherframe(mode,frcol,xycol,xzcol,yzcol,opt); exit; end;

  if stereo then begin xycol := -1; xzcol := -1; yzcol := -1; end;
  a[0] := ylabel; a[1] := xlabel; a[2] := yrlabel; a[3] := xrlabel;

  xypen.color := frcol;  

  doxyz := (opt and 1 =0); dothick := (opt and 2 =0);
  dodead := (opt and 4 =0) and not stereo;
  frame := false;
  xx[0] := xmi; xx[1] := xmi; xx[2] := xma; xx[3] := xma;
  yy[0] := ymi; yy[1] := yma; yy[2] := yma; yy[3] := ymi;
  zz[0] := zmi; zz[1] := zma;
  case oct of
    1,2 : t := 2; 3,4 : t := 3;
    5,6 : t := 0; 7,8 : t := 1;
  end;
  if (viewz>=zmid) then h := 0 else h := 1;
  with framexyz do begin x := xx[t]; y := yy[t]; z := zz[h]; end;

  with xycanvas do begin

  ch := textheight('0'); cw := textwidth('0');
  ticx := ch/pixx3d; ticy := ch/pixy3d; ticz := ch/pixz3d;
  ticx2 := cw/pixx3d; ticy2 := cw/pixy3d; ticz2 := cw/pixz3d;
  xylinewidth(1);

  if dodead then if (dead>0) then
    begin
      position(xmi,ymi,zmi,x1,y1); position(xma,yma,zma,x2,y2);
      if (x1>x2) then begin t := x1; x1 := x2; x2 := t; end;
      if (y1>y2) then begin t := y1; y1 := y2; y2 := t; end;
      restartgraph(x1,x2,y1,y2,false);
      xysetgridlines(4,-1,4,-1);
      if (frcol<0) then frcol := frontcolor;
      case dead of
        5,7 : col := xzcol; 6,8 : col := yzcol;
      else col := xycol; end;
      xypen.color := frcol;
      grid := (mode=4);
      if (col>=0) then xybrush.color := col else xybrush.style := bsclear;
      if (mode>1) then
         if dowmf then
           begin checkwmfpen; checkwmfbrush; blokwmf(x1,y1,x2-1,y2-1); end
         else xycanvas.Rectangle(x1,y1,x2+1,y2+1);
      xybrush.style := bssolid; xybrush.color := backcolor;
      if (mode>2) then case dead of
        1,5,9:  xyxaxis(frcol,xmi,xma,0,0,xlabel,grid,false,false);
        3,7,11: xyxaxis(frcol,xma,xmi,0,0,xlabel,grid,false,false);
        4,8,12: xyxaxis(frcol,ymi,yma,0,0,ylabel,grid,false,false);
        2,6,10: xyxaxis(frcol,yma,ymi,0,0,ylabel,grid,false,false);
       end;
      if (mode>2) then case dead of
        4,10: xyyaxis(frcol,xmi,xma,0,0,xlabel,6,grid,false,false);
        2,12: xyyaxis(frcol,xma,xmi,0,0,xlabel,6,grid,false,false);
        3,9:  xyyaxis(frcol,ymi,yma,0,0,ylabel,6,grid,false,false);
        1,11: xyyaxis(frcol,yma,ymi,0,0,ylabel,6,grid,false,false);
        5..8: xyyaxis(frcol,zmi,zma,0,0,zlabel,6,grid,false,false);
       end;
      exit;
    end;

  if labels3d then init3dlabels(t,h,ch);

  if not stereo then mm := 1
    else if labels3d or nostlabels then mm := 2 else mm := 3;

  for stm := 1 to mm do begin

  doline := (stm<3); dotext := (stm=3) or (not stereo) or labels3d;
  if stereo then
    begin setstereo(stm); frcol := stereocol[stm]; end;
  if doline then dmode := 0 else dmode := 8;

  sttel := 0;
  if (mode>1) and doline then {teken zijvlakken en initieer endframe}
    begin
      rectxy(xmi,ymi,xma,yma,zz[h],frcol,xycol,false,false);
      rectxz(xmi,zmi,xma,zma,yy[t],frcol,xzcol,false,false);
      rectyz(ymi,zmi,yma,zma,xx[t],frcol,yzcol,false,false);
      if (frcol<0) then endframe[stm,0] := frontcolor else endframe[stm,0] := frcol;
      frame := true; t2 := (t+2) mod 4; h2 := 1-h;
      position(xx[t2],yy[t2],zz[h2],endframe[stm,1],endframe[stm,2]);
      position(xx[t2],yy[t2],zz[h],endframe[stm,3],endframe[stm,4]);
      position(xx[t2],yy[t],zz[h2],endframe[stm,5],endframe[stm,6]);
      position(xx[t],yy[t2],zz[h2],endframe[stm,7],endframe[stm,8]);
    end;

  if (frcol<0) then xypen.color := frontcolor else xypen.color := frcol;
  xyfont.color := xypen.color; xybrush.color := backcolor;
  xybrush.style := bsclear;

  if (mode in [1,2]) or ( (frcol>=0) and dothick) then {teken (dikke) XYZ assen}
    begin
      xyfont.style := [fsbold]; if dothick then xylinewidth(2);

      if (h=0) then s := zlabel else s := zrlabel;
      position(xx[t],yy[t],zz[h],x2,y2); position(xx[t],yy[t],zz[1-h],x1,y1);
      xh := slope(x1,y1,x2,y2);
      if doline then xyline(-1,x2,y2,x1,y1);
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,zlmode);

      position(xx[t],yy[t],zz[h],x2,y2);
      t2 := (t + 1) mod 4; position(xx[t2],yy[t2],zz[h],x1,y1);
      xh := slope(x1,y1,x2,y2); if (odd(t)) then m := xlmode else m := ylmode;
      if doline then xyline(-1,x2,y2,x1,y1); s := a[t];
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,m);

      position(xx[t],yy[t],zz[h],x2,y2);
      t2 := (t + 3) mod 4; position(xx[t2],yy[t2],zz[h],x1,y1);
      xh := slope(x2,y2,x1,y1); if (odd(t)) then m := ylmode else m := xlmode;
      if doline then xyline(-1,x2,y2,x1,y1); t3 := (t+1) mod 4; s := a[t3];
      if doxyz and (mode<3) and dotext then text3(s,x1,y1,x2,y2,ch,m);
      xyfont.style := []; xylinewidth(1);
    end;

  if (mode>2) then begin {teken schaal verdeling}

  {X-AS} if (r3d>0) or (kijkhoogte<>0) or (kijkhoek mod 180 <>90) then
  begin
  if oct in [1,2,7,8] then begin t1 := 0; t2 := 3; t3 := 1; end
    else begin t1 := 2; t2 := 1; t3 := 3; end;
  position(xx[t1],yy[t1],zz[h],x1,y1); position(xx[t2],yy[t2],zz[h],x2,y2);
  xh := slope(x2,y2,x1,y1);
  if labels3d then
    begin
      if odd(xlmode) then d := round(prm(101)*ch) else d := round(prm(100)*cw);
      verdeling3d(xma-xmi,round((xma-xmi)*pixx3d),d,gr,nd,stm,1);
    end
  else
    begin
      l := round(sqrt(sqr(x2-x1)+sqr(y2-y1)));
      verdeling3d(xma-xmi,l,round(prm(100)*cw),gr,nd,stm,1);
      tl := round(prm(102)*ch);
    end;
  xmx := max(x1,x2); xmn := min(x1,x2); ymx := max(y1,y2); ymn := min(y1,y2);
  if fs3d[1].go then p := fs3d[1].min else p := gr*trunc(xmi/gr-1); ml := 0;
  repeat
    position(p,yy[t1],zz[h],xt,yt);
    if  ( (xt<=xmx) and (xt>=xmn) and (yt<=ymx) and (yt>=ymn) ) then
      begin
        position(p,yy[t3],zz[h],x0,y0);
        if not dotext then s := '' else
          if fs3d[1].go then s := formtext(p,1) else str(p:1:nd,s);
        if (length(s)>ml) then ml := length(s);
        if labels3d then text3d(s,p,yy[t1],zz[h],xlmode+1,false,oct,ml,zlvert,prm(102))
        else text2(s,xt,yt,x0,y0,tl,xh,xlmode+dmode,false);
        if (mode=4) and doline then
          begin
            xyline(-1,xt,yt,x0,y0);
            position(p,yy[t3],zz[1-h],x0,y0);
            if dowmf then drawwmf(x0,y0) else lineto(x0,y0);
          end;
      end;
    p := p + gr;
  until (p>xma+gr); if not labels3d then if (xlmode>1) then ml := 2;

  if dotext and doxyz then
    if labels3d then
     begin
      text3d(xlabel,xmid,yy[t1],zz[h],xlmode+1,true,oct,ml,zlvert,prm(102));
     end
    else
    begin
      position(xmid,yy[t1],zz[h],xt,yt); position(xmid,yy[t3],zz[h],x0,y0);
      text1(xlabel,xt,yt,x0,y0,tl+cw*(ml+3),xh,xlmode);
    end;
  end;  

  {Y-AS} if (r3d>0) or (kijkhoogte<>0) or (kijkhoek mod 180 <>0) then
  begin
  if oct in [1,2,3,4] then begin t1 := 0; t2 := 1; t3 := 3; end
    else begin t1 := 2; t2 := 3; t3 := 1; end;
  position(xx[t1],yy[t1],zz[h],x1,y1); position(xx[t2],yy[t2],zz[h],x2,y2);
  yh := slope(x1,y1,x2,y2);
  if labels3d then
    begin
      if odd(ylmode) then d := round(prm(111)*ch) else d := round(prm(110)*cw);
      verdeling3d(yma-ymi,round((yma-ymi)*pixy3d),d,gr,nd,stm,2);
    end
  else
    begin
      l := round(sqrt(sqr(x2-x1)+sqr(y2-y1)));
      verdeling3d(yma-ymi,l,round(prm(110)*cw),gr,nd,stm,2);
      tl := round(prm(112)*ch);
    end;
  xmx := max(x1,x2); xmn := min(x1,x2); ymx := max(y1,y2); ymn := min(y1,y2);
  if fs3d[2].go then p := fs3d[2].min else p := gr*trunc(ymi/gr-1); ml := 0;
  repeat
    position(xx[t1],p,zz[h],xt,yt); 
    if  ( (xt<=xmx) and (xt>=xmn) and (yt<=ymx) and (yt>=ymn) ) then
      begin
        position(xx[t3],p,zz[h],x0,y0);
        if not dotext then s := '' else
          if fs3d[2].go then s := formtext(p,2) else str(p:1:nd,s);
        if (length(s)>ml) then ml := length(s);
        if labels3d then text3d(s,xx[t1],p,zz[h],ylmode+5,false,oct,ml,zlvert,prm(112)) else
        text2(s,xt,yt,x0,y0,tl,yh,ylmode+dmode,false);
        if (mode=4) and doline then
          begin
            xyline(-1,xt,yt,x0,y0);
            position(xx[t3],p,zz[1-h],x0,y0);
            if dowmf then drawwmf(x0,y0) else lineto(x0,y0);
          end;
      end;
    p := p + gr;
  until (p>yma+gr); if not labels3d then if (ylmode>1) then ml := 2;
  if doxyz and dotext then
    if labels3d then text3d(ylabel,xx[t1],ymid,zz[h],ylmode+5,true,oct,ml,zlvert,prm(112)) else
    begin
      position(xx[t1],ymid,zz[h],xt,yt); position(xx[t3],ymid,zz[h],x0,y0);
      text1(ylabel,xt,yt,x0,y0,tl+cw*(ml+3),yh,ylmode);
    end;
  end;

  {Z-AS} if (r3d>0) or (abs(kijkhoogte)<>90) then
     plotzlabels(t,h,stm,mode,dotext,doline,doxyz);

  end; end;

  end;
  if labels3d then exittext3d;
  autoreset(3);
  for i := 1 to 4 do fs3d[i].go := false;
end;

procedure xy3dcloseframe; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i,stm : integer;
begin
  if not frame then exit;
  xylinewidth(1);
  for stm := 1 to 2 do if (stm=1) or stereo then begin
  if stereo then setstereo(stm) else xypen.color := endframe[stm,0];
  with xycanvas do for i := 1 to 3 do
    xyline(-1,endframe[stm,1],endframe[stm,2],
          endframe[stm,i*2+1],endframe[stm,i*2+2]);
  end;
end;

procedure xy3dsetsun(pos,height:integer;contrast:single;mode:integer); {XXXXXXX}
var i,t : integer;
    zh,zp : single;
begin
  zonmode := mode;
  for i := 0 to 255 do
    begin
      t := round((i-127.5)*contrast+127.5);
      if (t<0) then t := 0 else if (t>255) then t := 255;
      licht[i] := t;
    end;

  zh := height * grtorad; zp := pos * grtorad;
  cszz := sin(zh); csxx := -cos(zh)*sin(zp); csyy := -cos(zh)*cos(zp);
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

function xy3dinfront(x,y,z:single):boolean;
var cs0,h1,h2,h3,ch:single;
    s1,c1,s2,c2 : extended;
begin
  if (not ok3d) or (polartype<3) then result := true
  else
    begin
      { bepaal hoek tussen punt en voetpunt van kijklijn }
      if (polartype=3) {cyl} then
        begin
          ch := cos( (y-xy3dviewa)*grtorad );
        end
      else {spher, via bol driehoek met Pool}
       { cos(c) = cos(a)cos(b) + sin(a)sin(b)cos(C) }
        begin
          if sphcor then
            begin h1 := y; h2 := xy3dviewh; h3 := z-xy3dviewa; end
          else
            begin h1 := 90-z; h2 := 90-xy3dviewh; h3 := y-xy3dviewa; end;
          h1 := h1 * grtorad; h2 := h2 * grtorad; h3 := h3 * grtorad;
          sincos(h1,s1,c1); sincos(h2,s2,c2);
          ch := c1*c2 + s1*s2*cos(h3);
        end;
      {bepaal grenswaarde voor cosinus cs0}
      if (r3d=0) then cs0 := 0 else cs0 := abs((x-rmi)/(xy3dviewr-rmi));
      result := (ch>cs0);
    end;
end;

procedure xy3dmove(x,y,z:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xp,yp,stm : integer;
begin
  if not ok3d then exit;
  lastx := x; lasty := y; lastz := z;
  if stereo then
    for stm := 1 to 2 do with stereopos[stm] do
      begin setstereo(stm); position(x,y,z,sxp,syp); end
  else
    begin position(x,y,z,xp,yp); xymove(xp,yp); end;
end;

procedure xy3ddraw(x,y,z:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xp,yp,stm : integer;
begin
  if not ok3d then exit;
      lastx := x; lasty := y; lastz := z;
  if stereo then
    for stm := 1 to 2 do with stereopos[stm] do
      begin
        setstereo(stm); position(x,y,z,xp,yp);
        xyline(-1,sxp,syp,xp,yp);
        sxp := xp; syp := yp;
      end
  else
    begin position(x,y,z,xp,yp); xydraw(xp,yp); end;
end;

procedure xy3dbar(p1,p2:Tpoint3d;frcol,cx1,cx2,cy1,cy2,cz1,cz2:Tcolor; {XXXXXXX}
  light,trans:boolean);
var t,x1,x2,y1,y2,z1,z2 : single;
    col,colf : Tcolor;
    cl : array[1..6] of Tcolor;
    i,stm : integer;
    showback : boolean;
begin
  if (not ok3d) or (polartype>0) then exit;
  cl[1] := cx1; cl[2] := cx2; cl[3] := cy1; cl[4] := cy2;
  cl[5] := cz1; cl[6] := cz2;
  col := -1; for i := 6 downto 1 do if (cl[i]>=0) then col := cl[i];
  if (col<0) and (frcol<0) then frcol := frontcolor;
  if stereo then begin frcol := stereocol[0]; for i := 1 to 6 do cl[i] := -1; col := -1; end;
  with lastsymbol do
   begin
     xp := 0; yp := 0; cd3d := 29; size := 0; fill := 0; width := xypen.width;
     color := frcol; cl1 := cl[1]; cl2 := cl[3]; cl3 := cl[5];
     if (col>=0) then fill := 3;
   end;

  if transoff then trans := false;
  showback := (transmode in [1,2]) and (transopt and 1>0);
  x1 := p1.x; x2 := p2.x; y1 := p1.y; y2 := p2.y; z1 := p1.z; z2 := p2.z;
  if (x1=x2) or (y1=y2) or (z1=z2) then exit;

  if (x1>x2) then begin t := x1; x1 := x2; x2 := t; end;
  if (y1>y2) then begin t := y1; y1 := y2; y2 := t; end;
  if (z1>z2) then begin t := z1; z1 := z2; z2 := t; end;

  for stm := 1 to 2 do if (stm=1) or stereo then begin

  if stereo then
    begin setstereo(stm); frcol := stereocol[stm]; col := -1; trans := false; end;

  if (col<0) or (trans and not showback) then {teken draadmodel}
    begin
      colf := frcol;
      if trans and (colf<0) then colf := col;
      rectxy(x1,y1,x2,y2,z1,colf,-1,false,false);
      rectxy(x1,y1,x2,y2,z2,colf,-1,false,false);
      line3d(x1,y1,z1, x1,y1,z2); line3d(x1,y2,z1, x1,y2,z2);
      line3d(x2,y2,z1, x2,y2,z2); line3d(x2,y1,z1, x2,y1,z2);
    end;

  if (col>=0) then
    begin {massief}
      for i := 1 to 6 do if (cl[i]<0) then cl[i] := col else col := cl[i];
      if trans and showback then {teken achterwanden}
      begin
      if (viewy>=y1) then rectxz(x1,z1,x2,z2,y1,frcol,cl[1],light,trans) ;
      if (viewy<=y2) then rectxz(x1,z1,x2,z2,y2,frcol,cl[2],light,trans) ;
      if (viewx>=x1) then rectyz(y1,z1,y2,z2,x1,frcol,cl[3],light,trans) ;
      if (viewx<=x2) then rectyz(y1,z1,y2,z2,x2,frcol,cl[4],light,trans) ;
      if (viewz>=z1) then rectxy(x1,y1,x2,y2,z1,frcol,cl[5],light,trans) ;
      if (viewz<=z2) then rectxy(x1,y1,x2,y2,z2,frcol,cl[6],light,trans) ;
      end;
      if (viewy<y1) then rectxz(x1,z1,x2,z2,y1,frcol,cl[1],light,trans) else
      if (viewy>y2) then rectxz(x1,z1,x2,z2,y2,frcol,cl[2],light,trans) ;
      if (viewx<x1) then rectyz(y1,z1,y2,z2,x1,frcol,cl[3],light,trans) else
      if (viewx>x2) then rectyz(y1,z1,y2,z2,x2,frcol,cl[4],light,trans) ;
      if (viewz<z1) then rectxy(x1,y1,x2,y2,z1,frcol,cl[5],light,trans) else
      if (viewz>z2) then rectxy(x1,y1,x2,y2,z2,frcol,cl[6],light,trans) ;
    end;

  end;
end;

procedure xy3dsymbol(x,y,z:single;st,sz:integer;col1,col2:Tcolor; {XXXXXXXXXXXX}
  light,trans:boolean;opt:integer);
  {st: 0=punt 1=cirkel 2=kubus 3=3lijnen+ 4=4lijnenX}
var l,d,dx,dy,dz : single;
    xp,yp,dd,topt,pt,stm : integer;
begin
  if not ok3d then exit;
  if not (st in [0..4]) then exit;
  if (sz=0) then
    if doprint then sz := round(xyfont.size*ffac)
               else sz := xyfont.size
  else sz := sz * res;
  d := sz/2;
  if stereo then begin col1 := stereocol[0]; col2 := -1; light := false; end;
  case st of
    1,2 : if (col2<0) and (col1<0) then col1 := frontcolor;
   else if (col1<0) then col1 := frontcolor;
  end;
  if (col2<0) then trans := false;
  with lastsymbol do
   begin
     xp := 0; yp := 0; style := 0; size := 0; fill := 0; cd3d := 0;
     width := xypen.width; color := col1; cl1 := col2; cl2 := -1; cl3 := -1;
     if (col2>=0) then fill := 3;
     case st of
       1 : cd3d := 28; 2 : cd3d := 29;
       3 : style := 7; 4 : style := 8;
     end;
   end;
  if (sz<0) then exit;
  if (opt and 1 >0) then begin x := lastx; y := lasty; z := lastz; end;

  if (st=0) then if not stereo then
    begin position(x,y,z,xp,yp); xypixel(xp,yp,col1); setcursor(xp,yp); end
  else
    for stm := 1 to 2 do
       begin setstereo(stm); position(x,y,z,xp,yp); xypixel(xp,yp,stereocol[stm]); end;
  if (st=0) then exit;

  pt := polartype; polartype := 0;
  if (pt=3) then poltocart(x,y) else
  if (pt=4) then sphtocart(x,y,z);

  if (st=1) then
    begin
      if (r3d>0) then
        begin
          dx := (viewx-x)*facx; dy := (viewy-y)*facy; dz := (viewz-z)*facz;
          l := sqrt(sqr(dx)+sqr(dy)+sqr(dz)); if (l=0) then exit;
          d := d*r3d/l;
        end;
      dd := round(d*1.2);
      xypen.color := col1;
      if (col2<0) then xybrush.style := bsclear else xybrush.color := col2;
      if not stereo then
        begin
          position(x,y,z,xp,yp);
          if trans and not transoff then transcircle(-1,-1,xp,yp,dd)
            else xycircle(-1,-1,xp,yp,dd);
          setcursor(xp,yp);
        end
      else
       for stm := 1 to 2 do
         begin setstereo(stm); position(x,y,z,xp,yp);
           xycircle(stereocol[stm],-1,xp,yp,dd); end;

      xybrush.style := bssolid;
      polartype := pt; exit;
    end;

  dx := d/(facx*fac3d); dy := d/(facy*fac3d); dz := d/(facz*fac3d);

  if (st=2) then
    begin
      topt := transopt; transopt := 0;
      xy3dbar(point3d(x-dx,y-dy,z-dz),point3d(x+dx,y+dy,z+dz),
           col1,col2,-1,-1,-1,-1,-1,light,trans);
      transopt := topt;
    end
  else
    begin
      for stm := 1 to 2 do if (stm=1) or stereo then  begin
      if stereo then setstereo(stm) else xypen.color := col1;
      case st of
        3 : begin
              line3d(x-dx,y,z, x+dx,y,z); line3d(x,y-dy,z, x,y+dy,z);
              line3d(x,y,z-dz, x,y,z+dz);
            end;
        4 : begin
              line3d(x-dx,y-dy,z-dz, x+dx,y+dy,z+dz); line3d(x+dx,y-dy,z-dz, x-dx,y+dy,z+dz);
              line3d(x-dx,y+dy,z-dz, x+dx,y-dy,z+dz); line3d(x-dx,y-dy,z+dz, x+dx,y+dy,z-dz);
            end;
    end; end; end;

  if not stereo then
    begin position(x,y,z,xp,yp); setcursor(xp,yp); end;
  polartype := pt;
end;

procedure xy3dtriangle(p1,p2,p3:Tpoint3D;col1,col2:Tcolor;light,trans:boolean);
begin
  if (mode3d and not ok3d) or (polartype>0) then exit;
  xy3dpolygon(col1,col2,[p1,p2,p3],light,trans);
end;

procedure xy3dquad(p1,p2,p3:Tpoint3D;col1,col2:Tcolor;light,trans:boolean);
var p4 : tpoint3d;
begin
  if (mode3d and not ok3d) or (polartype>0) then exit;
  p4.x := p1.x+p3.x-p2.x; p4.y := p1.y+p3.y-p2.y; p4.z := p1.z+p3.z-p2.z;
  xy3dpolygon(col1,col2,[p1,p2,p3,p4],light,trans);
end;

procedure xy3dusertoabs(x,y,z:single;var xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXX}
var stm : integer;
begin
  if stereo then begin stm := stereomode; setstereo(0); end;
  position(x,y,z,xp,yp);
  xp := round((xp-xoff)/res); yp := round((yp-xoff)/res);
  if stereo then setstereo(stm);
end;

procedure xy3dpolygon(col1,col2:Tcolor;poly:array of Tpoint3D; {XXXXXXXXXXXXXXX}
     light,trans:boolean);
var p : array of Tpoint;
    i,n,stm : integer;
begin
  if not mode3d then
    begin polygon2d(col1,col2,poly,trans and (not transoff)); exit; end;
  if not ok3d then exit;
  n := length(poly); if (n<3) then exit; setlength(p,n);
  if stereo then begin col2 := -1; light := false; trans := false; end;

  for stm := 1 to 2 do if (stm=1) or stereo then  begin

  if stereo then begin setstereo(stm); col1 := xypen.color; end;

  if light and (zonmode>0) and (col2>=0) then
    col2 := color3(poly[0],poly[1],poly[2],col2);
  for i := 0 to n-1 do p[i] := topoint2(poly[i]);
  drawpoly(col1,col2,p,trans);

  end;
end;

procedure xy3dcircle(p1,p2:Tpoint3D;r:single;np:integer; {XXXXXXXXXXXXXXXXXXXXX}
     col1,col2:Tcolor;light,trans:boolean;opt:integer);
var dx,dy,dz,r1,r2,sih,coh,sip,cop,t : single;
    x1,x2,x3,y1,y2,y3,z1,z2,z3,d,h1,h2 : single;
    i,pt,stm : integer;
    poly : array of Tpoint3d;
    b,spokes,rel : boolean;
const zerop : Tpoint3d = (x:0;y:0;z:0);
function equal(p1,p2:Tpoint3D):boolean; {--------------------------------------}
begin result := (p1.x=p2.x) and (p1.y=p2.y) and (p1.z=p2.z); end;
begin
  if (mode3d and not ok3d) then exit;
  if (np=0) then np := 24;
  b := (np<0); np := abs(np); if (np<3) then np := 3;
  if (col1<0) and (col2<0) then col1 := frontcolor;
  rel := (opt and 2 > 0);

  if (polartype=3) then
    begin
      r := (r-rmi)/(rma-rmi); rel := false;
      if (r<0) then r := 0 else if (r>1) then r := 1;
      p1.x := 0; p1.y := 0;
      p2.x := 0; p2.y := 0; p2.z := p1.z+1;
    end;
  if (polartype=4) then
    begin
      if sphcor then
        begin
          with p1 do begin t := y; y := z; z := 90-t; end;
          with p2 do begin t := y; y := z; z := 90-t; end;
        end;
      r := (r-rmi)/(rma-rmi); rel := false;
      if (r<0) then r := 0 else if (r>1) then r := 1;
      if abs(p1.z)<1 then
        begin
          h1 := p2.y*grtorad; h2:= p2.z*grtorad;
          p1.x := 0; p1.y := 0; p1.z := 0;
          p2.x := cos(h1)*cos(h2); p2.y := sin(h1)*cos(h2);
          p2.z := sin(h2);
        end
      else
        begin
          h1 := p1.z*grtorad;
          p1.x := 0; p1.y := 0; p1.z := r*sin(h1);
          p2.x := 0; p2.y := 0; p2.z := 2;
          r := r * cos(h1);
        end;
    end;

  r := abs(r); if (r=0) then exit;
  setlength(poly,np); spokes := (opt and 1 > 0);
  if b then d := pi/np else d := 0;

  if not mode3d then {2D circle}
    begin
      for i := 0 to np-1 do
        begin
          x3 := r*cos(i/np*2*pi+d); y3 := r*sin(i/np*2*pi+d);
          poly[i].x := x3+p1.x; poly[i].y := y3+p1.y; poly[i].z := 0;
        end;
     xy3dpolygon(col1,col2,poly,light,trans);
     if spokes then
        for i := 0 to np-1 do with poly[i] do
          begin xymove(p1.x,p1.y); xydraw(x,y); end;
     exit;
    end;

  if rel then
    begin
      if equal(p1,p2) then
        if equal(p1,zerop) then p2 := point3d(0,0,1) else p2 := zerop;
      dx := p2.x-p1.x; dy := p2.y-p1.y; dz := p2.z-p1.z;
    end
  else
    begin
      if equal(p2,zerop) then
        if equal(p1,zerop) then p2 := point3d(0,0,1) else p2 := p1;
      dx := p2.x; dy := p2.y; dz := p2.z;
    end;  
  r1 := sqrt(sqr(dx)+sqr(dy)+sqr(dz)); if (r1=0) then exit;
  r2 := sqrt(sqr(dx)+sqr(dy)); sih := -r2/r1; coh := dz/r1;
  if (r2=0) then begin sip := 0; cop := 1; end
    else begin sip := -dx/r2; cop := dy/r2; end;

  for i := 0 to np-1 do
    begin
      x1 := r*cos(i/np*2*pi+d); y1 := r*sin(i/np*2*pi+d); z1 := 0;
      x2 := x1; y2 := y1*coh; z2 := y1*sih; {kantelen}
      x3 := x2*cop - y2*sip; y3 := x2*sip + y2*cop; z3 := z2; {draaien}
      poly[i].x := x3+p1.x; poly[i].y := y3+p1.y; poly[i].z := z3+p1.z;
    end;

  if stereo then begin light := false; trans := false; end;

  pt := polartype; polartype := 0;
  xy3dpolygon(col1,col2,poly,light,trans);

  if spokes then
    for stm := 1 to 2 do if (stm=1) or stereo then
    begin
      if stereo then setstereo(stm);
      for i := 0 to np-1 do with poly[i] do line3d(x,y,z,p1.x,p1.y,p1.z);
    end;

  polartype := pt;
end;

procedure xy3dcylinder(p1,p2:Tpoint3D;ra1,ra2:single;np:integer; {XXXXXXXXXXXXX}
   colfr,colbo,colf1,colf2:Tcolor;light,trans:boolean;opt:integer);
var dx,dy,dz,d1,d2,r1,r2,x,y,z,sih,coh,sip,cop,dh : single;
    d,dd : double;
    i,i0,n,cl1,cl2,pt,stm,j : integer;
    vis1,vis2,fac1,fac2,b,dir,spokes : boolean;
    colbf : tcolor;
    poly : array[1..2] of array of Tpoint3d;
    pol4 : array[0..3] of Tpoint3d;
procedure berekenpunt(i:integer;r:single); {-----------------------------------}
var x1,x2,y1,y2,z1,z2 : single;
begin
  x1 := r*cos(i/np*2*pi+dh); y1 := r*sin(i/np*2*pi+dh); z1 := 0;
  x2 := x1; y2 := y1*coh; z2 := y1*sih; {kantelen}
  x := x2*cop - y2*sip; y := x2*sip + y2*cop; z := z2; {draaien}
end;
function afstand(n:integer):double; {------------------------------------------}
var x,y,z:single;
begin
  n := (n+np) mod np;
  x := (poly[1,n].x + poly[1,n+1].x)/2;
  y := (poly[1,n].y + poly[1,n+1].y)/2;
  z := (poly[1,n].z + poly[1,n+1].z)/2;
  result := sqrt(sqr(viewx-x)+sqr(viewy-y)+sqr(viewz-z));
end;
procedure vlak(n:integer); {---------------------------------------------------}
var i : integer;
    p : Tpoint3D;
begin
  xy3dpolygon(colfr,colf1,poly[n],light,trans);
  if (n=1) then p := p1 else p := p2;
  if spokes then for i := 0 to np-1 do with poly[n,i] do
     line3d(x,y,z,p.x,p.y,p.z);
end;
begin {------------------------------------------------------------------------}
  if (not ok3d) or (polartype>3) then exit;
  if (colfr<0) and (colbo<0) and (colf1<0) and (colf2<0)
    then colfr := frontcolor;

  with lastsymbol do
   begin
     xp := 0; yp := 0; cd3d := 30; size := 0; fill := 0; width := xypen.width;
     color := colfr; cl1 := colbo; cl2 := colf1; cl3 := colf2;
     if (colbo>=0) then fill := 3;
   end;

  if (polartype=3) then
    begin
      ra1 := (ra1-rmi)/(rma-rmi); ra2 := (ra2-rmi)/(rma-rmi);
      if (ra1<0) then ra1 := 0 else if (ra1>1) then ra1 := 1;
      if (ra2<0) then ra2 := 0 else if (ra2>1) then ra2 := 1;
      p1.x := 0; p1.y := 0;   p2.x := 0; p2.y := 0;
    end;

  ra1 := abs(ra1); ra2 := abs(ra2);
  if ( (ra1=0) and (ra2=0) ) then exit;

  spokes := (opt and 1 > 0); if (np=0) then np := 24;
  b := (np<0); np := abs(np); if (np<3) then np := 3;
  setlength(poly[1],np+1); setlength(poly[2],np+1);

  dx := (p2.x-p1.x); dy := (p2.y-p1.y); dz := (p2.z-p1.z);
  if (dx=0) and (dy=0) and (dz=0) then exit;

  {bereken afstanden tot vlakken (*factor)}
  d1 := dx*(viewx-p1.x) + dy*(viewy-p1.y) + dz*(viewz-p1.z);
  d2 := dx*(viewx-p2.x) + dy*(viewy-p2.y) + dz*(viewz-p2.z);
  {afstanden: als d1>0 en d2>0 dan vlak 2 voor;
              als d1<0 en d2<0 dan vlak 1 voor}
  vis1 := (d1<0) and (d2<0); vis2 := (d1>0) and (d2>0);
  {geven aan welke vlakken zichtbaar zijn}

  r1 := sqrt(sqr(dx)+sqr(dy)+sqr(dz)); if (r1=0) then exit;
  r2 := sqrt(sqr(dx)+sqr(dy)); sih := -r2/r1; coh := dz/r1;
  if (r2=0) then begin sip := 0; cop := 1; end
    else begin sip := -dx/r2; cop := dy/r2; end;
  if b then d := pi/np else d := 0;
  {bereken alle punten}
  for i := 0 to np-1 do
    begin
      berekenpunt(i,ra1); poly[1,i].x := x+p1.x;
      poly[1,i].y := y+p1.y; poly[1,i].z := z+p1.z;
      berekenpunt(i,ra2); poly[2,i].x := x+p2.x;
      poly[2,i].y := y+p2.y; poly[2,i].z := z+p2.z;
    end;
  poly[1,np] := poly[1,0]; poly[2,np] := poly[2,0];

  {if (colbo>=0) then colbf := -1 else} colbf := colfr;
  fac1 := (ra1>0) and ( (colf1>=0) or (colfr<>colbf) or spokes);
  fac2 := (ra2>0) and ( (colf2>=0) or (colfr<>colbf) or spokes);

  {bepaal achterste vlak}
  d := 0; for i := 0 to np-1 do
    begin
      dd := afstand(i); if (dd>d) then begin d := dd; i0 := i; end;
    end;
  dir := (afstand(i0+1)<afstand(i0-1)); {linksom of rechtsom}

  pt := polartype; polartype := 0;
  {teken de niet zichtbare zijvlakken}
  if not stereo then
    begin
      if fac1 then if not vis1 then vlak(1);
      if fac2 then if not vis2 then vlak(2);
    end;

  {teken de rompvlakken}
  for i := 0 to np-1 do
    begin
      if dir then
        if odd(i) then n := i0 - (i+1) div 2
          else n := i0 + i div 2
      else
        if odd(i) then n := i0 + (i+1) div 2
          else n := i0 - i div 2;
      n := (n + np) mod np;
      pol4[0] := poly[1,n]; pol4[1] := poly[1,n+1];
      pol4[2] := poly[2,n+1]; pol4[3] := poly[2,n];
      xy3dpolygon(colbf,colbo,pol4,light,trans);
    end;

  {teken de wel zichtbare zijvlakken}
  if not stereo then
    begin
      if fac1 then if vis1 then vlak(1);
      if fac2 then if vis2 then vlak(2);
    end
  else if spokes then
    for stm := 1 to 2 do
    begin
      setstereo(stm);
      for i := 0 to np-1 do with poly[1,i] do line3d(x,y,z,p1.x,p1.y,p1.z);
      for i := 0 to np-1 do with poly[2,i] do line3d(x,y,z,p2.x,p2.y,p2.z);
    end;

  polartype := pt;
end;

{MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM}

procedure copyzoom3d(n:integer;x0,x1,y0,y1:single); {XXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with graphs3d[n] do
    begin xf0 := x0; xf1 := x1; yf0 := y0; yf1 := y1; end;
end;

function checkrul3d:boolean; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  result := rul3d; if not rul3d then exit;
  with graphs3d[igraph] do
    begin datap := dp; zmi := z1; zma := z2; hmode := hm; drmode := drm; end;
  if (datap<>nil) then
    begin nx := length(datap^); ny := length(datap^[1]); end;
end;

procedure xy3dinitruler(cl:Tcolor;xp,yp,j:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXX}
 {initieert lineaal}
begin
  rul3d := true;
  xyinitruler(cl,xp,yp,j,0);
end;

procedure xyclearbuffer3d(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i,l : integer;
begin
  if (n<0) then begin setlength(buffer3d,0); exit; end;
  l := length(buffer3d);
  for i := n to l-2 do buffer3d[i] := buffer3d[i+1];
  setlength(buffer3d,l-1);
end;

procedure xyputbuffer3d(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (length(buffer3d)<=n) then setlength(buffer3d,n+1);
  with buffer3d[n] do
    begin
      rl3d := rul3d; bmp := hasbmp;
      graphdat3d := graphs3d;
    end;
end;

procedure xygetbuffer3d(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with buffer3d[n] do
    begin
      rul3d := rl3d; hasbmp := bmp;
      graphs3d := graphdat3d;
    end;
end;

procedure initxygraph3d; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
begin
  clearcrosssection; xy3dsetcrosssection(0,4); 
  for i := 0 to 24 do
    begin si15[i] := sin(i*pi/12);
          co15[i] := cos(i*pi/12); end;
  xy3dsetcolors([clblack,clwhite]);
end;

end.
