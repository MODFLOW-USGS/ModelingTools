unit xygraph4d;

{=========================================================================}
{ See Copyright notes at XYGRAPH.                                         }
{=========================================================================}

{ This unit is  self-containing. You need it only if you use 4D procedures,
  otherwise it can be discarded }

interface

{$R+}

uses Windows , {Messages,} SysUtils, Classes, Graphics, {Controls, Forms, Dialogs,
    {StdCtrls, Spin, ExtCtrls, Clipbrd, AppEvnts,} math,
     xycommon, xygraph, xygraph3d;

procedure xy4dsetarray(var data:T4dtype;nx,ny,nz:integer);
procedure xy4dloadvolume(var dat:T4dtype;var ok : boolean;opt:integer);
procedure xy4dsetscale(tmin,tmax:single;cs:array of Tcolor;crs:integer;
  rev,log:boolean);
procedure xy4dshowvolume(p1,p2,p3:Tpoint3D;mode1:integer;
  frcol,fccol,lncol,f2col:Tcolor;
  mode2,split:integer;fresh:Tprocedure;opt:integer); overload;
procedure xy4dshowvolume(p1,p2,p3:Tpoint3D;mode1:integer;
  frcol,fccol,lncol,f2col:Tcolor;mode2,split:integer;opt:integer); overload;
procedure xy4dcalcplane(p1,p2,p3:Tpoint3D;var a,b,c,d:single; var ok:boolean);
procedure xy4dsectvolume(a,b,c,d:single;p1,p2:Tpoint3d;
  frcol,fccol,lncol,f2col:Tcolor;mode:integer;opt:integer);
procedure xy4disovolume(h:single;p1,p2:Tpoint3d;frcol,fccol:Tcolor;
  mode:integer;light,trans:boolean;opt:integer);
procedure xy4dheightscale(x1,y1,x2,y2:integer;txt:string;opt:integer);

procedure initxygraph4d;

implementation

var  datap4d,crsdp : data4dpointer;         {pointer naar data}

     nx,ny,nz : integer;                    {aantallen punten in blok}
     tmi,tma : single;                      {min max t schaal}
     point4,pp4,ps4 : array[0..3] of Tpoint;
     point3,pp3,ps3 : array[0..2] of Tpoint;
     point6 : array[0..5] of Tpoint;
     xps,yps,zps : array of single;         {x,y en z waarden}
     colres : integer;                      {kleuren resolutie}
     colrev : boolean;                      {omgekeerde kleuren schaal}
     tlog : boolean;                        {log kleuren schaal}
     zw : boolean;                          {zwart/wit schaal}
     ltmi,ltma,colfac : single;             {factoren}
     ascol : integer;                       {kleur van as bij hoogteschaal}
     pixok : boolean;                       {ok voor abs -> user}

type nodetype = record x,y,t :smallint; end;
const maxsplit = 32;
var  nodes: array of array of nodetype;
     subnodes: array[0..maxsplit] of array[0..maxsplit] of nodetype;

type dim4type = record xmi,xma,ymi,yma,zmi,zma,tmi,tma : single; end;
var  blk : dim4type = (xmi:0;xma:0;ymi:0;yma:0;zmi:0;zma:0;tmi:0;tma:0);

type regel = array[0..8000] of integer;
     pregel = ^regel;
var  buf : pregel;
     bm : Tbitmap;
     init : boolean = true;

     rgnbuf : array[1..8000] of longword;

type polytype = array[0..23] of Tpoint3d;
var  pl : polytype;

{------------------------------------------------------------------------------}

function topoint(x,y,z:single):Tpoint; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xp,yp : integer;
begin position0(x,y,z,xp,yp); result := point(xp,yp); end;

procedure line3d(x1,y1,z1,x2,y2,z2:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xp1,yp1, xp2,yp2:integer;
begin
  position0(x1,y1,z1,xp1,yp1); position0(x2,y2,z2,xp2,yp2);
  xyline(-1,xp1,yp1,xp2,yp2);
end;

function equal(p1,p2:Tpoint3d):boolean; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin result := (p1.x=p2.x) and (p1.y=p2.y) and (p1.z=p2.z); end;

function topoint2(p:Tpoint3D):Tpoint; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var xp,yp : integer;
begin position0(p.x,p.y,p.z,xp,yp); result := point(xp,yp); end;

procedure dorectxy(x1,y1,x2,y2,z:single;col1,col2:Tcolor); {XXXXXXXXXXXXXXXXXXX}
begin
  point4[0] := topoint(x1,y1,z); point4[1] := topoint(x1,y2,z);
  point4[2] := topoint(x2,y2,z); point4[3] := topoint(x2,y1,z);
  drawpoly(col1,col2,point4,false);
end;

procedure dorectxz(x1,z1,x2,z2,y:single;col1,col2:Tcolor);{XXXXXXXXXXXXXXXXXXXX}
begin
  point4[0] := topoint(x1,y,z1); point4[1] := topoint(x1,y,z2);
  point4[2] := topoint(x2,y,z2); point4[3] := topoint(x2,y,z1);
  drawpoly(col1,col2,point4,false);
end;

procedure dorectyz(y1,z1,y2,z2,x:single;col1,col2:Tcolor); {XXXXXXXXXXXXXXXXXXX}
begin
  point4[0] := topoint(x,y1,z1); point4[1] := topoint(x,y2,z1);
  point4[2] := topoint(x,y2,z2); point4[3] := topoint(x,y1,z2);
  drawpoly(col1,col2,point4,false);
end;

{------------------------------------------------------------------------------}

{XXXXXXXXXX USER -> ABS en omgekeerd XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

(*ROUTINE VOOR USER -> ABS
  x := (x-xmid)*facx; y := (y-ymid)*facy; z := (z-zmid)*facz;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp; {z1 := z}    {draaiing om z-as}
  x2 := x1; y2 := y1*sinh + z*cosh; {z2 = y1*cosh - z1*sinh} {draaiing om as in xy vlak}
 [x3 := x2*cosr - y2*sinr; y3 := x2*sinr + y2*cosr;] {draaiing om z-as bij stereo}
  if (r3d=0) then ff := fac3d else
    ff := fac3d*r3d/(r3d+y1*cosh-z*sinh);              {factor voor perspectief}
  xp := round(xpc+ff*x2); yp := round(ypc-ff*y2);

  INVERSE ROUTINES - ALS X, Y of Z BEKEND:

  --- R3D = 0:
  voor constante x, y of z: w := (w-wmid)*facw
  x2 := (xp-xpc)/fac3d; y2 := -(yp-ypc)/fac3d;
  x1 := x2; y2 := y1*sinh + z*cosh;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp;
  oplossen afh van bekende: x, y of z;
  - bereken y2 en x1=x2;
  - als z bekend: bereken y1 uit y2, dan inverse draaiing van x1 en y1 geeft x,y
    (x := x1*cosp + y1*sinp; y := -x1*sinp+ y1*cosp;)
  - als y bekend: bereken x uit x1 en y, bereken y1 uit x en y, bereken z uit y1 en y2
  - als x bekend: bereken y uit x1 en y, bereken y1 uit x en y, bereken z uit y1 en y2
  x := x/facx+xmid; y := y/facy+ymid; z := z/facz+zmid;

  --- R3D =/= 0:
  voor constante x, y of z: w := (w-wmid)*facw
  x2 :=  (xp-xpc)/fac3d*(1+y1*cosh/r3d-z*sinh/r3d);
  y2 := -(yp-ypc)/fac3d*(1+y1*cosh/r3d-z*sinh/r3d);
  x1 := x2; y2 := y1*sinh + z*cosh;
  --> twee vergelijkingen in x1, y1 en z1=z:  f1(x1,y1,z1)=0 en f2(y1,z1=0)
  - als z=z1 bekend: bereken y1 uit f2, bereken x1 uit f1, inverse draaiing
    (x := x1*cosp + y1*sinp; y := -x1*sinp+ y1*cosp;)
  - als x of y bekend: -> elimineer y of x -> f3(x1,y1)=0
    drie vgl in x1,y1,z1 : los op, bereken y uit x of x uit y
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp;
  oplossen afh van constante: x, y of z;
  x := x/facx+xmid; y := y/facy+ymid; z := z/facz+zmid;

  c1 := -(yp-ypc)/fac3d; c2 := c1/r3d;
  f2: y1*sinh + z*cosh = c1 + c2*y1*cosh - c2*z*sinh
      y1*(sinh-c2*cosh) + z*(cosh+c2*sinh) = c1
  c3 := (xp-xpc)/fac3d;  c4 := c3/r3d;
  f1: x1 = c3 + c4*y1*cosh - c4*z*sinh
      x1 + y1*(-c4*cosh) + z*(c4*sinh) = c3;
  f3 : x1*cosp + y1*sinp = x   {x bekend}
  f3 : x1*sinp - y1*cosp = -y  {y bekend}
  ax + by + cz =  d
  f3: a*(x1*cosp + y1*sinp) + b*(-x1*sinp+ y1*cosp) + c*z1 = d
      x1(a*cosp-b*sinp) + y1*(a*sinp+b*cosp) + z1*c = d

  a1.x + b1.y + c1.z = d1 (f1)
    0  + b2.y + c2.z = d2 (f2)
  a3.x + b3.y +  0   = d3 {f3)
  D  = (b1.c2.a3-a1.c2.b3-c1.b2.a3);
  Dx = (b1.c2.d3+c1.d2.b3-d1.c2.b3-c1.b2.d3};
  Dy = (d1.c2.a3-a1.c2.d3-c1.d2.a3);
  Dz = (a1.b2.d3+b1.d2.a3-a1.d2.b3-d1.b2.a3);
  x = Dx/D; y = Dy/D; z = Dz/D;

  INVERSE ROUTINES - IHA X, Y EN Z LIGGEN OP EEN VLAK

  --- R3D = 0:
  x2 := (xp-xpc)/fac3d; y2 := -(yp-ypc)/fac3d;
  x1 := x2; y2 := y1*sinh + z*cosh;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp;
  --> twee vergelijkingen in x, y en z=z1:  f1(x,y,z)=0 en f2(y,z=0)
  ax + by + cz = d
  --> drie vergelijkingen

  x*sinp.sinh + y*cosp.sinh + z.cosh = y2
  x*cosp      + -y*sinp              = x2
  x.a         + y*b         + z*c    = d

  --- R3D =/= 0:
  x2 :=  (xp-xpc)/fac3d*(1+y1*cosh/r3d-z*sinh/r3d);
  y2 := -(yp-ypc)/fac3d*(1+y1*cosh/r3d-z*sinh/r3d);
  x1 := x2; y2 := y1*sinh + z*cosh;
  --> twee vergelijkingen in x1, y1 en z1=z:  f1(x1,y1,z1)=0 en f2(y1,z1=0)
  algemeen: ax + by + cz = d
  vertaal naar x1, y1 en z1=z, geeft derde vergelijking f3(x1,y1,z1)=0
  f3: a*(x1*cosp + y1*sinp) + b*(-x1*sinp+ y1*cosp) + c*z1 = d
      x1(a*cosp-b*sinp) + y1*(a*sinp+b*cosp) + z1*c = d
   drie vergelijkingen, los op; inverse draaiing geeft x en y

  c1 := -(yp-ypc)/fac3d; c2 := c1/r3d;
  f2: y1*sinh + z*cosh = c1 + c2*y1*cosh - c2*z*sinh
      y1*(sinh-c2*cosh) + z*(cosh+c2*sinh) = c1
  c3 := (xp-xpc)/fac3d;  c4 := c3/r3d;
  f1: x1 = c3 + c4*y1*cosh - c4*z*sinh
      x1 + y1*(-c4*cosh) + z*(c4*sinh) = c3;
  f3: a*(x1*cosp + y1*sinp) + b*(-x1*sinp+ y1*cosp) + c*z1 = d
      x1(a*cosp-b*sinp) + y1*(a*sinp+b*cosp) + z1*c = d

  x1                 + y1*(-c4*cosh)      + z*(c4*sinh)      = c3
                       y1*(sinh-c2*cosh)  + z*(cosh+c2*sinh) = c1
  x1*(a*cosp-b*sinp) + y1*(a*sinp+b*cosp) + z1*c             = d

  a1.x + b1.y + c1.z = d1 (f1)
  a2.x + b2.y + c2.z = d2 (f2)
  a3.x + b3.y + c3*z = d3 {f3)
  D  = (a1.b2.c3+b1.c2.a3+c1.a2.b3-a1.c2.b3-b1.a2.c3-c1.b2.a3);
  Dx = (d1.b2.c3+b1.c2.d3+c1.d2.b3-d1.c2.b3-b1.d2.c3-c1.b2.d3);
  Dy = (a1.d2.c3+d1.c2.a3+c1.a2.d3-a1.c2.d3-b1.a2.c3-c1.d2.a3);
  Dz = (a1.b2.d3+b1.d2.a3+d1.a2.b3-a1.d2.b3-b1.a2.d3-d1.b2.a3);
  x = Dx/D; y = Dy/D; z = Dz/D;

*)

procedure revpos(xp,yp:integer; var x,y,z:single; a,b,c,d : single); {XXXXXXXXX}
var a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3 : single;
    x0,y0,z0,p1,p2,p3,p4,x2,y2: single;
    ok : boolean;
procedure solve;
var det,idet:single;
begin
   det := a1*(b2*c3-c2*b3)+a2*(b3*c1-b1*c3)+a3*(b1*c2-c1*b2);
   if (det=0) then begin ok := false; exit; end else idet := 1/det;
   x0 := d1*(b2*c3-c2*b3)+d2*(b3*c1-b1*c3)+d3*(b1*c2-c1*b2);
   y0 := a1*(d2*c3-c2*d3)+a2*(d3*c1-d1*c3)+a3*(d1*c2-c1*d2);
   z0 := a1*(b2*d3-d2*b3)+a2*(b3*d1-b1*d3)+a3*(b1*d2-d1*b2);
   x0 := x0*idet; y0 := y0*idet; z0 := z0*idet; ok := true;
end;
begin
  y2 := -(yp-ypc)*ifac3d; x2 := (xp-xpc)*ifac3d; pixok := true;
  if (r3d=0) then
    begin
      a1 := sinp*sina; b1 := cosp*sina; c1 := cosa; d1 := y2;
      a2 := cosp;      b2 := -sinp;     c2 := 0;    d2 := x2;
      a3 := a;         b3 := b;         c3 := c;    d3 := d;
      solve; if not ok then
         begin pixok := false; x := 0; y := 0; z := 0; exit; end;
      x := x0; y := y0; z := z0;
    end
  else
    begin
      p1 := -(yp-ypc)*ifac3d; p3 := (xp-xpc)*ifac3d;
      p2 := p1*ir3d; p4 := p3*ir3d;
      a1 := 1;             b1 := -p4*cosa;      c1 :=p4*sina;      d1 := p3;
      a2 := 0;             b2 := sina-p2*cosa;  c2 :=cosa+p2*sina; d2 := p1;
      a3 := a*cosp-b*sinp; b3 := a*sinp+b*cosp; c3 := c;           d3 := d;
      solve; if not ok then
         begin pixok := false; x := 0; y := 0; z := 0; exit; end;
      x := x0*cosp + y0*sinp; y := -x0*sinp+ y0*cosp; z := z0;
    end;
  x := x*ifacx+xmid; y := y*ifacy+ymid; z := z*ifacz+zmid;
end;

procedure solve0(xp,yp,m:integer;t:single;var x1,y1,z1:single); {XXXXXXXXXXXXXX}
var p1,p2,p3,p4 : single;
    a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3 : single;
    det, idet : single;
begin
   p1 := -(yp-ypc)*ifac3d; p2 := p1*ir3d;
   p3 :=  (xp-xpc)*ifac3d; p4 := p3*ir3d;

   {a1 := 1;}  b1 := -p4*cosa;     c1 := p4*sina;      d1 := p3;
   {a2 := 0;}  b2 := sina-p2*cosa; c2 := cosa+p2*sina; d2 := p1;
   if (m=0) then begin
   a3 := cosp; b3 := sinp;        {c3 := 0;}           d3 := t;
   end else begin
   a3 := -sinp; b3 := cosp;       {c3 := 0;}           d3 := t;
   end;

   det := a3*(b1*c2-c1*b2)-c2*b3;
   if (det=0) then begin pixok := false; exit; end else idet := 1/det;
   x1 := c1*(d2*b3-d3*b2)+c2*(b1*d3-b3*d1);
   y1 := a3*(d1*c2-d2*c1)-c2*d3;
   z1 := a3*(b1*d2-d1*b2)+b2*d3-b3*d2;
   x1 := x1*idet; y1 := y1*idet; z1 := z1*idet;
end;

procedure revposx(xp,yp:integer;x:single; var y,z:single); {XXXXXXXXXXXXXXXXXXX}
var x1,x2,y1,y2,z1 : single;
begin
  x := (x-xmid)*facx; if (r3d=0) then
  begin
    if (sinp=0) or (cosa=0) then begin pixok := false; exit; end;
    x2 := (xp-xpc)*ifac3d; y2 := -(yp-ypc)*ifac3d;
    x1 := x2; y := -(x1-x*cosp)*isinp;
    y1 := x*sinp + y*cosp; z := (y2-y1*sina)*icosa;
  end else begin
    solve0(xp,yp,0,x,x1,y1,z1);
    z := z1; y := -x1*sinp+ y1*cosp;
  end;
  y := y*ifacy+ymid; z := z*ifacz+zmid;
end;

procedure revposy(xp,yp:integer;y:single; var x,z:single); {XXXXXXXXXXXXXXXXXXX}
var x1,x2,y1,y2,z1 : single;
begin
  y := (y-ymid)*facy; if (r3d=0) then
  begin
    if (cosp=0) or (cosa=0) then begin pixok := false; exit; end;
    x2 := (xp-xpc)*ifac3d; y2 := -(yp-ypc)*ifac3d;
    x1 := x2; x := (x1+y*sinp)*icosp;
    y1 := x*sinp + y*cosp; z := (y2-y1*sina)*icosa;
  end else begin
    solve0(xp,yp,1,y,x1,y1,z1);
    z := z1; x := x1*cosp + y1*sinp;
  end;
  x := x*ifacx+xmid; z := z*ifacz+zmid;
end;

procedure revposz(xp,yp:integer;z:single; var x,y:single); {XXXXXXXXXXXXXXXXXXX}
var x1,x2,y1,y2 : single;
    c1,c2,c3,c4,cc : single;
begin
  z := (z-zmid)*facz;
  if (r3d=0) then
  begin
    if (sina=0) then begin pixok := false; exit; end;
    x2 := (xp-xpc)*ifac3d; y2 := -(yp-ypc)*ifac3d;
    x1 := x2; y1 := (y2-z*cosa)*isina;
  end else begin
    c1 := -(yp-ypc)*ifac3d; c2 := c1*ir3d;
    cc := (c2*cosa-sina); if (cc=0) then begin pixok := false; exit; end;
    c3 := (xp-xpc)*ifac3d; c4 := c3*ir3d;
    y1 := (z*(cosa+c2*sina)-c1)/cc;
    x1 := c3 + c4*(y1*cosa - z*sina);
  end;
  x := x1*cosp + y1*sinp; y := -x1*sinp+ y1*cosp;
  x := x*ifacx+xmid; y := y*ifacy+ymid;
end;

{------------------------------------------------------------------------------}
function color(n:integer):Tcolor; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (n<0) then n := 0 else if (n>255) then n := 255;
  if colrev then n := 255 - n;
  if zw then
    if odd(n div colres) then result := 0 else result := $ffffff
  else result := colors[resscale[n]];
end;

function revcolor(n:integer):Tcolor; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (n<0) then n := 0 else if (n>255) then n := 255;
  if colrev then n := 255 - n;
  if zw then
    if odd(n div colres) then result := 0 else result := $ffffff
  else result := revcolors[resscale[n]];
end;

function hcolor(h:single;rev:boolean):Tcolor; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var t : integer;
begin
  if tlog then t := round((ln(h)-ltmi)*colfac)
   else t := round((h-tmi)*colfac);
  if rev then result := revcolor(t) else result := color(t);
end;

procedure setrange(tmin,tmax:single;log:boolean); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var tt : single;
begin
  if (tmax<tmin) then begin tt := tmax; tmax := tmin; tmin := tt; end;
  if (tmin=0) and (tmax=0) then begin tmi := blk.tmi; tma := blk.tma; end
  else begin tmi := tmin; tma := tmax; end;
  tlog := log; if (tmi<=0) or (tma<=0) then tlog := false;
  if tlog then
    begin if (tma=tmi) then begin tmi := tmi/1.4; tma := tmi*2; end;
      ltmi := ln(tmi); ltma := ln(tma); colfac := 255/(ltma-ltmi); end
  else
    begin if (tma=tmi) then begin tmi := tmi-0.5; tma := tmi+1; end;
      colfac := 255/(tma-tmi); end;
end;

procedure xy4dsetscale(tmin,tmax:single;cs:array of Tcolor;crs:integer; {XXXXXX}
  rev,log:boolean);
begin
  xy3dsetcolors(cs);
  setrange(tmin,tmax,log);
  colres := crs; if (colres<1) then colres := 1;
  setresscale(colres);
  colrev := rev;
end;

procedure xy4dheightscale(x1,y1,x2,y2:integer;txt:string;opt:integer); {XXXXXXX}
  { x1,x2,y1,y2 { gebied op scherm
    txt : text bij as
    opt : opties bit 0 set = schaal links ipv rechts
                 bit 1 set = text sideways
                 bit 2 set = generic
    }
var t,y,p,clh,clh0,c0,y0,yy,pw,h,col : integer;
    oldlog, gen : boolean;
begin
  if not mode3d or stereo then exit;
  gen := (opt and 4 > 0);
  if not gen then
   if not (vol or graphs3d[igraph].vl) then exit;
  if (x1=x2) or (y1=y2) then exit;
  if (x1>x2) then begin t := x1; x1 := x2; x2 := t; end;
  if (y1>y2) then begin t := y1; y1 := y2; y2 := t; end;
  x1 := x1*res+xoff; x2 := x2*res+xoff;
  y1 := y1*res+yoff; y2 := y2*res+yoff;
  sx1 := x1; sx2 := x2; sy1 := y1; sy2 := y2;
  case opt and 3 of 0:p:=4; 1:p:=1; 2:p:=7; 3:p:=5; end;
  if gen then col := xypen.color else col := ascol;
  restartgraph(x1,x2,y1,y2,true);
  copyzoom(igraph,0,1,0,1);
  xyyaxis(col,tmi,tma,0,0,txt,p,false,tlog,false);
  oldlog := tlog; tlog := false;
  {with graphs3d[igraph] do copyzoom(igraph,xf0,xf1,yf0,yf1);}

  y0 := y1;
  pw := xypen.width; xypen.width := res;
  for y := y1 to y2 do
   begin
     h := round((y2-y)/(y2-y1)*255);
     clh := color(h);
     if (y=y1) then clh0 := clh else
     if (clh<>clh0) or (y=y2) then
       begin
        if (y=y2) then yy := y2+1 else yy := y;
        c0 := clh0; xyrectangle(c0,c0,x1,y0,x2+1,yy);
        clh0 := clh; y0 := y;
       end;
   end;
  xypen.width := pw;
  tlog := oldlog;
  {restartgraph(xp1,xp2,yp1,yp2,false); scale := true;}
  if not gen then
    with graphs3d[igraph] do
      begin xs1 := sx1; xs2 := sx2; ys1 := sy1; ys2 := sy2; scl := true; end;
end;

procedure xy4dsetarray(var data:T4dtype;nx,ny,nz:integer); {XXXXXXXXXXXXXXXXXXX}
var i,j : integer;
begin
  setlength(data,nx+1);
  for i := 0 to nx do
    begin
      setlength(data[i],ny+1);
      for j := 0 to ny do setlength(data[i,j],nz+1);
    end;
  data[0,0,0] := 0;
end;

procedure xy4dloadvolume(var dat:T4dtype;var ok : boolean;opt:integer); {XXXXXX}
var i,j,k : integer; t,tmin,tmax : single;
   {opt : opties; bit 0 set = bereken min en max}
procedure scale(t:single);
begin if (t<tmin) then tmin := t else if (t>tmax) then tmax := t; end;
begin
  if init then initxygraph4d;
  datap4d := @dat; ok := true; vol := false; {graphs3d[igraph].dp := nil;}
  nx := length(dat)-1; ny := length(dat[1])-1; nz := length(dat[1,1])-1;
  if (nx<2) or (ny<2) or (nz<2) then ok := false;
  for i := 2 to nx do if (dat[i,0,0]<=dat[i-1,0,0]) then ok := false;
  for i := 2 to ny do if (dat[0,i,0]<=dat[0,i-1,0]) then ok := false;
  for i := 2 to nz do if (dat[0,0,i]<=dat[0,0,i-1]) then ok := false;
  if not ok then exit;

  tmin := dat[(1+nx) div 2,(1+ny) div 2,(1+nz) div 2]; tmax := tmin;
  if (opt and 1 >0) then
  for i := 1 to nx do for j := 1 to ny do for k := 1 to nz do
    begin t := dat[i,j,k]; if (t<tmin) then tmin := t
       else if (t>tmax) then tmax := t; end
  else
    begin
      scale(dat[ 1, 1, 1]); scale(dat[nx, 1, 1]); scale(dat[ 1,ny, 1]);
      scale(dat[ 1, 1,nz]); scale(dat[nx,ny, 1]); scale(dat[ 1,ny,nz]);
      scale(dat[nx, 1,nz]); scale(dat[nx,ny,nz]);
    end;

  blk.xmi := dat[1,0,0]; blk.xma := dat[nx,0,0];
  blk.ymi := dat[0,1,0]; blk.yma := dat[0,ny,0];
  blk.zmi := dat[0,0,1]; blk.zma := dat[0,0,nz];
  blk.tmi := tmin;       blk.tma := tmax;

  setlength(xps,nx+1); for i := 1 to nx do xps[i] := datap4d^[i,0,0];
  setlength(yps,ny+1); for i := 1 to ny do yps[i] := datap4d^[0,i,0];
  setlength(zps,nz+1); for i := 1 to nz do zps[i] := datap4d^[0,0,i];

  vol := true;
  xy4dsetscale(0,0,[clblack,clwhite],16,false,false);
  {with plotfield do with contcall do
    begin ccxp1 := x1; ccxp2 := x2; ccyp1 := y1; ccyp2 := y2; end;}
end;

function color4p(h:single):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{bepaalt kleur bij bepaalde waarde}
var n : integer;
begin
  if (h<tmi) then n := 0 else
  if (h>tma) then n := 255 else
  if tlog then n := round((ln(h)-ltmi)*colfac)
   else n := round((h-tmi)*colfac);
  result := n;
end;

procedure xy4dcalcplane(p1,p2,p3:Tpoint3D;var a,b,c,d:single; var ok:boolean);
  { vlak A.x + B.y + C.z + D = 0 door (x1,y1,z1), (x2,y2,z2), (x3,y3,z3): }
  { |  x   y   z  1 |     }
  { | x1  y1  z1  1 |     }
  { | x2  y2  z2  1 | = 0 }
  { | x3  y3  z3  1 |     }
var norm : boolean;
    p : single;
function det3(a1,b1,c1,a2,b2,c2,a3,b3,c3:single):single;
begin result := a1*(b2*c3-c2*b3) + b1*(c2*a3-a2*c3) + c1*(a2*b3-b2*a3); end;
begin
  norm := ok;
  a :=  det3(p1.y,p1.z,1, p2.y,p2.z,1, p3.y,p3.z,1);
  b := -det3(p1.x,p1.z,1, p2.x,p2.z,1, p3.x,p3.z,1);
  c :=  det3(p1.x,p1.y,1, p2.x,p2.y,1, p3.x,p3.y,1);
  d :=  det3(p1.x,p1.y,p1.z, p2.z,p2.y,p2.z, p3.x,p3.y,p3.z); {d = -D !}
  ok := (a<>0) or (b<>0) or (c<>0);
  if not ok then d := 0 else
  if norm then
    begin
      p := sqrt(a*a+b*b+c*c);
      if (d<0) then p := -p else
      if (d=0) then
        if (c<0) then p := -p else
        if (c=0) then
          if (b<0) then p := -p;
      a := a/p; b := b/p; c := c/p; d := d/p;
    end;
end;

procedure constructpolygon(var tel : integer; var pol : polytype; {XXXXXXXXXXXX}
  x1,x2,y1,y2,z1,z2 : single);
  {construeert een nette polygon uit een reeks punten}
var i,j,t,t2 : integer;
    pl2 : polytype;
    tp,tpp : Tpoint3d;
    ok : boolean;
    dm,dt,idx2,idy2,idz2 : single;
function distance(p1,p2:Tpoint3d):single; {------------------------------------}
begin result := sqr(p1.x-p2.x)*idx2 + sqr(p1.y-p2.y)*idy2 + sqr(p1.z-p2.z)*idz2; end;
begin {------------------------------------------------------------------------}
  idx2 := 1/sqr(x2-x1); idy2 := 1/sqr(y2-y1); idz2 := 1/sqr(z2-z1);

  {verwijder dubbelingen}
  pl2[0] := pol[0]; t2 := 1;
  for i := 1 to tel-1 do
    begin
      tp := pol[i]; ok := true;
      for j := 0 to i-1 do if equal(tp,pol[j]) then ok := false;
      if ok then begin inc(t2); pl2[t2-1] := tp; end;
    end;
  tel := t2; for i := 0 to tel-1 do pol[i] := pl2[i];
  if (tel<4) then exit; {bij 3 punten is de volgorde niet belangrijk}

  {sorteer op volgorde}
  for i := 0 to tel-1 do
    begin
      dm := 1e38; t := 0; tp := pol[i];
      {kijk bij punten in hetzelfde vlak}
      for j := i+1 to tel-1 do
        begin
          tpp := pol[j]; {kijk of ze in een vlak liggen}
          if ( (tp.x=x1) and (tpp.x=x1) ) or ( (tp.x=x2) and (tpp.x=x2) )
          or ( (tp.y=y1) and (tpp.y=y1) ) or ( (tp.y=y2) and (tpp.y=y2) )
          or ( (tp.z=z1) and (tpp.z=z1) ) or ( (tp.z=z2) and (tpp.z=z2) ) then
          begin
            dt := distance(tp,tpp);
            if (dt<dm) then begin dm := dt; t := j; end;
          end;
        end;
      if (t=0) then {kijk bij alle punten}
      for j := i+1 to tel-1 do
        begin
          tpp := pol[j]; dt := distance(tp,tpp);
          if (dt<dm) then begin dm := dt; t := j; end;
        end;
      if (t>0) then begin tp := pol[i+1]; pol[i+1] := pol[t]; pol[t] := tp; end;
    end;
end;

procedure showframe1(col:Tcolor); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with blk do begin
    if (xy3dviewx>xmi) then dorectyz(ymi,zmi,yma,zma,xmi,col,-1);
    if (xy3dviewx<xma) then dorectyz(ymi,zmi,yma,zma,xma,col,-1);
    if (xy3dviewy>ymi) then dorectxz(xmi,xmi,zma,zma,ymi,col,-1);
    if (xy3dviewy<yma) then dorectxz(xmi,xmi,zma,zma,yma,col,-1);
    if (xy3dviewz>zmi) then dorectxy(xmi,xmi,yma,yma,zmi,col,-1);
    if (xy3dviewz<zma) then dorectxy(xmi,xmi,yma,yma,zma,col,-1);
  end;
end;

procedure showframe2(col:Tcolor); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with blk do begin
    if (xy3dviewx<xmi) then dorectyz(ymi,zmi,yma,zma,xmi,col,-1);
    if (xy3dviewx>xma) then dorectyz(ymi,zmi,yma,zma,xma,col,-1);
    if (xy3dviewy<ymi) then dorectxz(xmi,xmi,zma,zma,ymi,col,-1);
    if (xy3dviewy>yma) then dorectxz(xmi,xmi,zma,zma,yma,col,-1);
    if (xy3dviewz<zmi) then dorectxy(xmi,xmi,yma,yma,zmi,col,-1);
    if (xy3dviewz>zma) then dorectxy(xmi,xmi,yma,yma,zma,col,-1);
  end;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

function zoeknode(x:single; var px : array of single):integer; {XXXXXXXXXXXXXXX}
var n,np,t1,t2 : integer;
begin
  np := length(px)-1;
  if (x<=px[1]) then n := 1 else
  if (x>=px[np]) then n := np else
    begin t1 := 1; t2 := nx; while (t2-t1>1) do begin n := (t2+t1) div 2;
         if (px[n]>x) then t2 := n else t1 := n; end;
      n := t2; if (x-px[n-1])<(px[n]-x) then dec(n); end;
  result := n;
end;

function range(x:single;mx:integer):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var n : integer;
begin n := round(x); if (n<1) then n := 1 else if (n>mx) then n := mx; result := n; end;

procedure subplane(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var x,y,tm : integer;
begin
  for x := 1 to n-1 do
    begin
      subnodes[x,0].x := (x*subnodes[n,0].x + (n-x)*subnodes[0,0].x) div n;
      subnodes[x,0].y := (x*subnodes[n,0].y + (n-x)*subnodes[0,0].y) div n;
      subnodes[x,0].t := (x*subnodes[n,0].t + (n-x)*subnodes[0,0].t) div n;
      subnodes[x,n].x := (x*subnodes[n,n].x + (n-x)*subnodes[0,n].x) div n;
      subnodes[x,n].y := (x*subnodes[n,n].y + (n-x)*subnodes[0,n].y) div n;
      subnodes[x,n].t := (x*subnodes[n,n].t + (n-x)*subnodes[0,n].t) div n;
    end;
  for x := 0 to n do for y := 1 to n-1 do
    begin
      subnodes[x,y].x := (y*subnodes[x,n].x + (n-y)*subnodes[x,0].x) div n;
      subnodes[x,y].y := (y*subnodes[x,n].y + (n-y)*subnodes[x,0].y) div n;
      subnodes[x,y].t := (y*subnodes[x,n].t + (n-y)*subnodes[x,0].t) div n;
    end;

  for x := 0 to n-1 do for y := 0 to n-1 do
    begin
      pp4[0].x := subnodes[x,y].x;     pp4[0].y := subnodes[x,y].y;
      pp4[1].x := subnodes[x,y+1].x;   pp4[1].y := subnodes[x,y+1].y;
      pp4[2].x := subnodes[x+1,y+1].x; pp4[2].y := subnodes[x+1,y+1].y;
      pp4[3].x := subnodes[x+1,y].x;   pp4[3].y := subnodes[x+1,y].y;
      tm := (subnodes[x,y].t     + subnodes[x,y+1].t +
             subnodes[x+1,y+1].t + subnodes[x+1,y].t) div 4;
      xycanvas.brush.color := color(tm);
      xycanvas.pen.color := xycanvas.brush.color;
      xypolygon(-1,-1,pp4);
    end;
end;

procedure makeplane(m,s:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var x,y,nx,ny,tm : integer;
begin
  if (m=2) and (s<2) then m := 1;
  if (s>maxsplit) then s := maxsplit;
  nx := length(nodes); if (nx<2) then exit;
  ny := length(nodes[0]); if (ny<2) then exit;
  for x := 0 to nx-2 do for y := 0 to ny-2 do
    begin
      if (m=1) then begin
        pp4[0].x := nodes[x,y].x;     pp4[0].y := nodes[x,y].y;
        pp4[1].x := nodes[x,y+1].x;   pp4[1].y := nodes[x,y+1].y;
        pp4[2].x := nodes[x+1,y+1].x; pp4[2].y := nodes[x+1,y+1].y;
        pp4[3].x := nodes[x+1,y].x;   pp4[3].y := nodes[x+1,y].y;
        tm := (nodes[x,y].t+nodes[x,y+1].t+nodes[x+1,y+1].t+nodes[x+1,y].t) div 4;
        xycanvas.brush.color := color(tm);
        xycanvas.pen.color := xycanvas.brush.color;
        xypolygon(-1,-1,pp4); end;
      if (m=2) then begin
        subnodes[0,0] := nodes[x,y]; subnodes[0,s] := nodes[x,y+1];
        subnodes[s,s] := nodes[x+1,y+1]; subnodes[s,0] := nodes[x+1,y];
        subplane(s); end;
      if (m=3) then begin end;
    end;
end;

procedure pixelplane(m,x0,y0,nx,ny:integer;tw:single); {XXXXXXXXXXXXXXXXXXXXXXX}
 {m mode: 0 = xplane, 1 = yplane, 2 = zplane }
var h : Thandle;
    rect0,rectxy,rectbm : Trect;
    x,y,t0,t1,t2,t3,t,i,p : integer;
    xr,yr,xr0,xr1,yr0,yr1,xs0,xs1,ys0,ys1 : integer;
    xw,yw,zw,xmi,xma,dx,ymi,yma,dy,fx,fy,fx1,fy1 : single;
    wi,he : integer;
procedure dopoint; {-----------------------------------------------------------}
begin
  pixok := true; case m of
   0 : revposx(xr,yr,tw,xw,yw);
   1 : revposy(xr,yr,tw,xw,yw);
   2 : revposz(xr,yr,tw,xw,yw); end;
  if not pixok then buf[xr-xs0] := 0 else
    begin
      fx := (xw-xmi)/dx; fy := (yw-ymi)/dy; fx1 := 1-fx; fy1 := 1-fy;
      t := round(t0*fx1*fy1+t3*fx*fy1+t1*fx1*fy+t2*fx*fy);
      buf[xr-xs0] := revcolor(t);
   end;
 end;
begin {------------------------------------------------------------------------}
  pp4[0].x := nodes[0,0].x;   pp4[0].y := nodes[0,0].y;
  pp4[1].x := nodes[0,ny].x;  pp4[1].y := nodes[0,ny].y;
  pp4[2].x := nodes[nx,ny].x; pp4[2].y := nodes[nx,ny].y;
  pp4[3].x := nodes[nx,0].x;  pp4[3].y := nodes[nx,0].y;
  h := createpolygonrgn(pp4,length(pp4),ALTERNATE); if (h=0) then exit;
  t := getrgnbox(h,rect0); deleteobject(h); if (t=0) then exit;
  with rect0 do
      begin xs0 := left; ys0 := top; xs1 := right-1; ys1 := bottom-1; end;
  if (xs0<=0) then xs0 := 0 else dec(xs0);
  if (xs1>=cwidth-1) then xs1 := cwidth-1 else inc(xs1);
  if (ys0<=0) then ys0 := 0 else dec(ys0);
  if (ys1>=cheight-1) then ys1 := cheight-1 else inc(ys1);
  wi := xs1-xs0+1; he := ys1-ys0+1;
  if (bm.width<wi) then bm.width := wi;
  if (bm.height<he) then bm.height := he;
  rectbm := rect(0,0,wi-1,he-1); rectxy := rect(xs0,ys0,xs1,ys1);
  xycanvas.copymode := cmSrcCopy; bm.canvas.copymode := cmSrcCopy;
  bm.canvas.CopyRect(rectbm,xycanvas,rectxy);
  {bitblt(bm.canvas.handle,0,0,wi,he,xycanvas.handle,xs0,ys0,srccopy);}
  for x := 0 to nx-1 do for y := 0 to ny-1 do
    begin
      pp4[0].x := nodes[x,y].x;     pp4[0].y := nodes[x,y].y;
      pp4[1].x := nodes[x,y+1].x;   pp4[1].y := nodes[x,y+1].y;
      pp4[2].x := nodes[x+1,y+1].x; pp4[2].y := nodes[x+1,y+1].y;
      pp4[3].x := nodes[x+1,y].x;   pp4[3].y := nodes[x+1,y].y;
      t0 := nodes[x,y].t;   t1 := nodes[x,y+1].t;
      t3 := nodes[x+1,y].t; t2 := nodes[x+1,y+1].t;

      h := createpolygonrgn(pp4,length(pp4),ALTERNATE); if (h=0) then exit;
      t := getrgnbox(h,rect0); if (t=0) then exit;
      with rect0 do
        begin xr0 := left; yr0 := top; xr1 := right-1; yr1 := bottom-1; end;
      if (xr0<xs0) then xr0 := xs0; if (xr1>xs1) then xr1 := xs1;
      if (yr0<ys0) then yr0 := ys0; if (yr1>ys1) then yr1 := ys1;

      case m of
    0 : begin xmi := yps[x+x0]; xma := yps[x+x0+1]; ymi := zps[y+y0]; yma := zps[y+y0+1]; end;
    1 : begin xmi := xps[x+x0]; xma := xps[x+x0+1]; ymi := zps[y+y0]; yma := zps[y+y0+1]; end;
    2 : begin xmi := xps[x+x0]; xma := xps[x+x0+1]; ymi := yps[y+y0]; yma := yps[y+y0+1]; end;
      end;
      dx := xma-xmi; dy := yma-ymi;

    t := getregiondata(h,sizeof(rgnbuf),@rgnbuf);
    if (t=0) then {geen data}
    for yr := yr0 to yr1 do
      begin
        buf := bm.scanline[yr-ys0];
        for xr := xr0 to xr1 do
          if ptinregion(h,xr,yr) then dopoint;
      end
    else
    for i := 1 to rgnbuf[3] do
      begin
        p := i*4 + 5;
        xr0 := max(xs0,rgnbuf[p]); xr1 := min(xs1,rgnbuf[p+2]);
        yr0 := max(ys0,rgnbuf[p+1]); yr1 := min(ys1,rgnbuf[p+3]);
        for yr := yr0 to yr1 do
          begin
            buf := bm.scanline[yr-ys0];
            for xr := xr0 to xr1 do dopoint;
          end;
      end;

      deleteobject(h);
    end;
  xycanvas.copyrect(rectxy,bm.canvas,rectbm);
  {bitblt(xycanvas.handle,xs0,ys0,wi,he,bm.canvas.handle,0,0,srccopy);}
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
procedure xy4dshowvolume(p1,p2,p3:Tpoint3D;mode1:integer; {XXXXXXXXXXXXXXXXXXXX}
  frcol,fccol,lncol,f2col:Tcolor;mode2,split:integer;fresh:Tprocedure;opt:integer);
 {p1,p2,p3 = punten; mode1 = split (0=geen, 1,2,3 = blok x,y,z, 4=hap}
 {frcol,fccol,lncol,f2col = kleuren van frame, zijde, raster, linen2 }
 {tmin tmax = t-schaal}
 {mode2 = show (0=draad, 1=blank, 2=raster, 3,4 = vlak, 5 = bitmap}
 {split = spitsing vlak mode 4; cres = kl.resolutie; rev = inverse kleuren}
 {opt = opties}
 {  1 = projecteer }
 {  2 = show bounding block }
 {  4 = show frame }
 {  8 = show frame en grid }
 { 16 = zwart/wit banden }
 { 32 = p1,p2,p3 in nodal numbers }

var n1x,n2x,n3x,n1y,n2y,n3y,n1z,n2z,n3z,m2 : integer;
    project,dofr,dogr,refresh,isnode,dobl : boolean;
    s:single;

procedure makearray(d1,d2:integer); {------------------------------------------}
var i : integer;
begin
   setlength(nodes,d1+1);
   for i := 0 to d1 do setlength(nodes[i],d2+1);
end;
procedure xvlak(ny1,nz1,ny2,nz2,nx:integer); {---------------------------------}
var x,y1,z1,y2,z2 : single;
    ny,nz,ymi,yma,zmi,zma,px,py : integer;
    frame : boolean;
procedure dogrid; var ny,nz : integer; { - - - - - - - - - - - - - - - - - - - }
begin for ny := ymi+1 to yma-1 do line3d(x,yps[ny],z1,x,yps[ny],z2);
  for nz := zmi+1 to zma-1 do line3d(x,y1,zps[nz],x,y2,zps[nz]); end;
begin
  frame := (nx>0); nx := abs(nx);
  if (nx=0) then x := framexyz.x else x := xps[nx];
  y1 := yps[ny1]; y2 := yps[ny2]; z1 := zps[nz1]; z2 := zps[nz2];
  point4[0] := topoint(x,y1,z1); point4[1] := topoint(x,y2,z1);
  point4[2] := topoint(x,y2,z2); point4[3] := topoint(x,y1,z2);
  ymi := min(ny1,ny2); yma := max(ny1,ny2);
  zmi := min(nz1,nz2); zma := max(nz1,nz2);
  case mode2 of
    0 : drawpoly(frcol,-1,point4,false);
    1 : drawpoly(frcol,fccol,point4,false);
    2 : begin drawpoly(lncol,fccol,point4,false); dogrid;
       if frame then drawpoly(frcol,-1,point4,false); end;
   3..5 : begin
        makearray(yma-ymi,zma-zmi);
        for ny := ymi to yma do for nz := zmi to zma do
        begin
         position0(x,yps[ny],zps[nz],px,py);
         nodes[ny-ymi,nz-zmi].x := px; nodes[ny-ymi,nz-zmi].y := py;
         nodes[ny-ymi,nz-zmi].t := color4p(datap4d^[nx,ny,nz]);
        end;
        if (mode2=5) then pixelplane(0,ymi,zmi,yma-ymi,zma-zmi,x)
                     else makeplane(mode2-2,split);
        if dogr then begin xypen.color := f2col; dogrid; end;
        if frame and dofr then drawpoly(f2col,-1,point4,false);
        end;
   end;
   if refresh then fresh;
end;
procedure yvlak(nx1,nz1,nx2,nz2,ny:integer); {---------------------------------}
var y,x1,x2,z1,z2:single;
    nx,nz,xmi,xma,zmi,zma,px,py : integer;
    frame : boolean;
procedure dogrid; var nx,nz : integer; { - - - - - - - - - - - - - - - - - - - }
begin for nx := xmi+1 to xma-1 do line3d(xps[nx],y,z1,xps[nx],y,z2);
  for nz := zmi+1 to zma-1 do line3d(x1,y,zps[nz],x2,y,zps[nz]); end;
begin
  frame := (ny>0) and dofr; ny := abs(ny);
  if (ny=0) then y := framexyz.y else y := yps[ny];
  x1 := xps[nx1]; x2 := xps[nx2]; z1 := zps[nz1]; z2 := zps[nz2];
  point4[0] := topoint(x1,y,z1); point4[1] := topoint(x1,y,z2);
  point4[2] := topoint(x2,y,z2); point4[3] := topoint(x2,y,z1);
  xmi := min(nx1,nx2); xma := max(nx1,nx2);
  zmi := min(nz1,nz2); zma := max(nz1,nz2);
  case mode2 of
    0 : drawpoly(frcol,-1,point4,false);
    1 : drawpoly(frcol,fccol,point4,false);
    2 : begin drawpoly(lncol,fccol,point4,false); dogrid;
      if frame then drawpoly(frcol,-1,point4,false); end;
   3..5 : begin
        makearray(xma-xmi,zma-zmi);
        for nx := xmi to xma do for nz := zmi to zma do
        begin
         position0(xps[nx],y,zps[nz],px,py);
         nodes[nx-xmi,nz-zmi].x := px; nodes[nx-xmi,nz-zmi].y := py;
         nodes[nx-xmi,nz-zmi].t := color4p(datap4d^[nx,ny,nz]);
        end;
        if (mode2=5) then pixelplane(1,xmi,zmi,xma-xmi,zma-zmi,y)
                     else makeplane(mode2-2,split);
        if dogr then begin xypen.color := f2col; dogrid; end;
        if frame and dofr then drawpoly(f2col,-1,point4,false);
        end;
   end;
   if refresh then fresh;
end;
procedure zvlak(nx1,ny1,nx2,ny2,nz:integer); {---------------------------------}
var z,x1,x2,y1,y2 : single;
    nx,ny,xmi,xma,ymi,yma,px,py : integer;
    frame : boolean;
procedure dogrid; var nx,ny : integer; { - - - - - - - - - - - - - - - - - - - }
begin for nx := xmi+1 to xma-1 do line3d(xps[nx],y1,z,xps[nx],y2,z);
   for ny := ymi+1 to yma-1 do line3d(x1,yps[ny],z,x2,yps[ny],z); end;
begin
  frame := (nz>0) and dofr; nz := abs(nz);
  if (nz=0) then z := framexyz.z else z := zps[nz];
  x1 := xps[nx1]; x2 := xps[nx2]; y1 := yps[ny1]; y2 := yps[ny2];
  point4[0] := topoint(x1,y1,z); point4[1] := topoint(x1,y2,z);
  point4[2] := topoint(x2,y2,z); point4[3] := topoint(x2,y1,z);
  ymi := min(ny1,ny2); yma := max(ny1,ny2);
  xmi := min(nx1,nx2); xma := max(nx1,nx2);
  case mode2 of
    0 : drawpoly(frcol,-1,point4,false);
    1 : drawpoly(frcol,fccol,point4,false);
    2 : begin drawpoly(lncol,fccol,point4,false); dogrid;
      if frame then drawpoly(frcol,-1,point4,false); end;
   3..5 : begin
        makearray(xma-xmi,yma-ymi);
        for nx := xmi to xma do for ny := ymi to yma do
        begin
         position0(xps[nx],yps[ny],z,px,py);
         nodes[nx-xmi,ny-ymi].x := px; nodes[nx-xmi,ny-ymi].y := py;
         nodes[nx-xmi,ny-ymi].t := color4p(datap4d^[nx,ny,nz]);
        end;
        if (mode2=5) then pixelplane(2,xmi,ymi,xma-xmi,yma-ymi,z)
                     else makeplane(mode2-2,split);
        if dogr then begin xypen.color := f2col; dogrid; end;
        if frame and dofr then drawpoly(f2col,-1,point4,false);
        end;
   end;
   if refresh then fresh;
end;
procedure xhoek(nx:integer); {-------------------------------------------------}
var x,y1,y2,y3,z1,z2,z3:single;
begin
  if (nx=0) then x := framexyz.x else x := xps[nx];
  y1 := yps[n1y]; y2 := yps[n2y]; y3 := yps[n3y];
  z1 := zps[n1z]; z2 := zps[n2z]; z3 := zps[n3z];
  point6[0] := topoint(x,y3,z3); point6[1] := topoint(x,y1,z3);
  point6[2] := topoint(x,y1,z2); point6[3] := topoint(x,y2,z2);
  point6[4] := topoint(x,y2,z1); point6[5] := topoint(x,y3,z1);
  case mode2 of
    0 : drawpoly(frcol,-1,point6,false);
    1 : drawpoly(frcol,fccol,point6,false);
   2..5 : begin
          xvlak(n2y,n2z,n3y,n1z,-nx); xvlak(n3y,n3z,n1y,n2z,-nx);
          if (mode2<3) then drawpoly(frcol,-1,point6,false) else
          if dofr then drawpoly(f2col,-1,point6,false);
          end;
  end;
end;
procedure yhoek(ny:integer); {-------------------------------------------------}
var y,x1,x2,x3,z1,z2,z3 : single;
begin
  if (ny=0) then y := framexyz.y else y := yps[ny];
  x1 := xps[n1x]; x2 := xps[n2x]; x3 := xps[n3x];
  z1 := zps[n1z]; z2 := zps[n2z]; z3 := zps[n3z];
  point6[0] := topoint(x3,y,z3); point6[1] := topoint(x1,y,z3);
  point6[2] := topoint(x1,y,z2); point6[3] := topoint(x2,y,z2);
  point6[4] := topoint(x2,y,z1); point6[5] := topoint(x3,y,z1);
  case mode2 of
    0 : drawpoly(frcol,-1,point6,false);
    1 : drawpoly(frcol,fccol,point6,false);
   2..5 : begin
         yvlak(n2x,n2z,n3x,n1z,-ny); yvlak(n3x,n3z,n1x,n2z,-ny);
         if (mode2<3) then drawpoly(frcol,-1,point6,false) else
         if dofr then drawpoly(f2col,-1,point6,false);
          end;
  end;
end;
procedure zhoek(nz:integer); {-------------------------------------------------}
var z,x1,x2,x3,y1,y2,y3 : single;
begin
  if (nz=0) then z := framexyz.z else z := zps[nz];
  x1 := xps[n1x]; x2 := xps[n2x]; x3 := xps[n3x];
  y1 := yps[n1y]; y2 := yps[n2y]; y3 := yps[n3y];
  point6[0] := topoint(x3,y3,z); point6[1] := topoint(x1,y3,z);
  point6[2] := topoint(x1,y2,z); point6[3] := topoint(x2,y2,z);
  point6[4] := topoint(x2,y1,z); point6[5] := topoint(x3,y1,z);
  case mode2 of
    0 : drawpoly(frcol,-1,point6,false);
    1 : drawpoly(frcol,fccol,point6,false);
   2..5 : begin
          zvlak(n2x,n2y,n3x,n1y,-nz); zvlak(n3x,n3y,n1x,n2y,-nz);
          if (mode2<3) then drawpoly(frcol,-1,point6,false) else
          if dofr then drawpoly(f2col,-1,point6,false);
          end;
  end;
end;

{------------------------------------------------------------------------------}
begin {------------------------------------------------------------------------}
 {  1 = projecteer }
 {  2 = show bounding block }
 {  4 = show frame }
 {  8 = show frame en grid }
 { 16 = zwart/wit banden }
 { 32 = p1,p2,p3 in nodal numbers }
  if init then initxygraph4d;
  if not mode3d or (polartype>0) or stereo or not vol then exit;
  if (mode1<0) or (mode1>4) then mode1 := 0;
  if (mode2<0) then mode2 := 0 else if (mode2>5) then mode2 := 5;
  if (mode2=3) then split := 1;
  if (split<1) then split := 1 else if (split>maxsplit) then split := maxsplit;
  if (frcol<0) then frcol := frontcolor;
  if (fccol<0) then fccol := backcolor;
  if (lncol<0) then lncol := frcol;
  if (f2col<0) then f2col := frcol;
  ascol := frcol;
  project := (opt and 1)>0; dobl := (opt and 2)>0;
  dofr := (opt and 12)>0;   dogr := (opt and 8)>0;
  zw := (opt and 16)>0;     isnode := (opt and 32)>0;
  refresh := cvmode and (mode2>3);
  if (mode2=5) then if dowmf or doprint then mode2 := 3
            else begin graphs3d[igraph].bmp4d := true;
            xybuffers[0].b4d := true; end;

  if isnode then {nodal points}
   begin
     n1x := range(p1.x,nx); n1y := range(p1.y,ny); n1z := range(p1.z,nz);
     n2x := range(p2.x,nx); n2y := range(p2.y,ny); n2z := range(p2.z,nz);
     n3x := range(p3.x,nx); n3y := range(p3.y,ny); n3z := range(p3.z,nz);
   end
  else {user co-ordinates}
    begin
     n1x := zoeknode(p1.x,xps); n1y := zoeknode(p1.y,yps); n1z := zoeknode(p1.z,zps);
     n2x := zoeknode(p2.x,xps); n2y := zoeknode(p2.y,yps); n2z := zoeknode(p2.z,zps);
     n3x := zoeknode(p3.x,xps); n3y := zoeknode(p3.y,yps); n3z := zoeknode(p3.z,zps);
    end;
  if (n1x=n2x) or (n1y=n2y) or (n1z=n2z) then exit;

  if (abs(n1x-n2x)=1) then
    begin if mode1 in [2,3] then mode1 := 0 else if (mode1=4) then mode1 := 1; end
  else if (abs(n1x-n2x)=2) then n3x := (n1x+n2x) div 2 else
  if (n1x>n2x) then
    begin if (n3x>=n1x) then n3x := n1x-1 else if (n3x<=n2x) then n3x := n2x+1 end
  else if (n3x>=n2x) then n3x := n2x-1 else if (n3x<=n1x) then n3x := n1x+1;

  if (abs(n1y-n2y)=1) then
    begin if mode1 in [1,3] then mode1 := 0 else if (mode1=4) then mode1 := 2; end
  else if (abs(n1y-n2y)=2) then n3y := (n1y+n2y) div 2 else;
  if (n1y>n2y) then
    begin if (n3y>=n1y) then n3y := n1y-1 else if (n3y<=n2y) then n3y := n2y+1 end
  else if (n3y>=n2y) then n3y := n2y-1 else if (n3y<=n1y) then n3y := n1y+1;

  if (abs(n1z-n2z)=1) then
    begin if mode1 in [1,2] then mode1 := 0 else if (mode1=4) then mode1 := 3; end
  else if (abs(n1z-n2z)=2) then n3z := (n1z+n2z) div 2 else
  if (n1z>n2z) then
    begin if (n3z>=n1z) then n3z := n1z-1 else if (n3z<=n2z) then n3z := n2z+1 end
  else if (n3z>=n2z) then n3z := n2z-1 else if (n3z<=n1z) then n3z := n1z+1;

  p1 := point3d(xps[n1x],yps[n1y],zps[n1z]);
  p2 := point3d(xps[n2x],yps[n2y],zps[n2z]);
  p3 := point3d(xps[n3x],yps[n3y],zps[n3z]);

  {-------------------}

  m2 := mode2; if (m2>1) then mode2 := 1; if project then
  case mode1 of
     0 : begin xvlak(n1y,n1z,n2y,n2z,0); yvlak(n1x,n1z,n2x,n2z,0);
         zvlak(n1x,n1y,n2x,n2y,0); end;
     1 : begin yvlak(n1x,n1z,n2x,n2z,0); yvlak(n2x,n3z,n1x,n2z,0);
               zvlak(n1x,n1y,n2x,n2y,0); zvlak(n2x,n3y,n1x,n2y,0);
         xhoek(0); end;
     2 : begin xvlak(n1y,n1z,n2y,n2z,0); xvlak(n2y,n3z,n1y,n2z,0);
               zvlak(n1x,n1y,n2x,n2y,0); zvlak(n3x,n2y,n2x,n1y,0);
         yhoek(0); end;
     3 : begin xvlak(n1y,n1z,n2y,n2z,0); xvlak(n2y,n1z,n3y,n2z,0);
               yvlak(n1x,n1z,n2x,n2z,0); yvlak(n2x,n1z,n3x,n2z,0);
         zhoek(0); end;
     4 : begin xvlak(n1y,n1z,n2y,n2z,0); xvlak(n1y,n1z,n3y,n3z,0);
               yvlak(n1x,n1z,n2x,n2z,0); yvlak(n1x,n1z,n3x,n3z,0);
               zvlak(n1x,n1y,n2x,n2y,0); zvlak(n1x,n1y,n3x,n3y,0);end;
   end;
  mode2 := m2;

  if dobl then showframe1(frcol);

  case mode1 of
     0 : begin
       if ((p1.x>p2.x)and(xy3dviewx>p1.x)) or ((p1.x<p2.x)and(xy3dviewx<p1.x))
         or (mode2=0) then xvlak(n1y,n1z,n2y,n2z,n1x);
       if ((p1.x>p2.x)and(xy3dviewx<p2.x)) or ((p1.x<p2.x)and(xy3dviewx>p2.x))
         or (mode2=0) then xvlak(n1y,n1z,n2y,n2z,n2x);

       if ((p1.y>p2.y)and(xy3dviewy>p1.y)) or ((p1.y<p2.y)and(xy3dviewy<p1.y))
         or (mode2=0) then yvlak(n1x,n1z,n2x,n2z,n1y);
       if ((p1.y>p2.y)and(xy3dviewy<p2.y)) or ((p1.y<p2.y)and(xy3dviewy>p2.y))
         or (mode2=0) then yvlak(n1x,n1z,n2x,n2z,n2y);

       if ((p1.z>p2.z)and(xy3dviewz>p1.z)) or ((p1.z<p2.z)and(xy3dviewz<p1.z))
         or (mode2=0) then zvlak(n1x,n1y,n2x,n2y,n1z);
       if ((p1.z>p2.z)and(xy3dviewz<p2.z)) or ((p1.z<p2.z)and(xy3dviewz>p2.z))
         or (mode2=0) then zvlak(n1x,n1y,n2x,n2y,n2z);
         end;

     1 : begin
       if ((p1.y<p2.y)and(xy3dviewy<p3.y)) or ((p1.y>p2.y)and(xy3dviewy>p3.y))
          or (mode2=0) then yvlak(n2x,n3z,n1x,n1z,n3y); {midden vlak}
       if ((p1.z<p2.z)and(xy3dviewz<p3.z)) or ((p1.z>p2.z)and(xy3dviewz>p3.z))
          or (mode2=0) then zvlak(n2x,n3y,n1x,n1y,n3z); {midden vlak}

       if ((p1.x>p2.x)and(xy3dviewx>p1.x)) or ((p1.x<p2.x)and(xy3dviewx<p1.x))
          or (mode2=0) then xhoek(n1x); {hoek}
       if ((p1.x>p2.x)and(xy3dviewx<p2.x)) or ((p1.x<p2.x)and(xy3dviewx>p2.x))
          or (mode2=0) then xhoek(n2x); {hoek}

       if ((p1.y<p2.y)and(xy3dviewy>p2.y)) or ((p1.y>p2.y)and(xy3dviewy<p2.y))
          or (mode2=0) then yvlak(n1x,n1z,n2x,n2z,n2y); {groot vlak}
       if ((p1.y<p2.y)and(xy3dviewy<p1.y)) or ((p1.y>p2.y)and(xy3dviewy>p1.y))
          or (mode2=0) then yvlak(n2x,n3z,n1x,n2z,n1y); {klein vlak}

       if ((p1.z<p2.z)and(xy3dviewz>p2.z)) or ((p1.z>p2.z)and(xy3dviewz<p2.z))
          or (mode2=0) then zvlak(n1x,n1y,n2x,n2y,n2z); {groot vlak}
       if ((p1.z<p2.z)and(xy3dviewz<p1.z)) or ((p1.z>p2.z)and(xy3dviewz>p1.z))
          or (mode2=0) then zvlak(n2x,n3y,n1x,n2y,n1z); {klein vlak}
         end;

      2 : begin
       if ((p1.x<p2.x)and(xy3dviewx<p3.x)) or ((p1.x>p2.x)and(xy3dviewx>p3.x))
          or (mode2=0) then xvlak(n2y,n3z,n1y,n1z,n3x); {midden vlak}
       if ((p1.z<p2.z)and(xy3dviewz<p3.z)) or ((p1.z>p2.z)and(xy3dviewz>p3.z))
          or (mode2=0) then zvlak(n3x,n2y,n1x,n1y,n3z); {midden vlak}

       if ((p1.y>p2.y)and(xy3dviewy>p1.y)) or ((p1.y<p2.y)and(xy3dviewy<p1.y))
          or (mode2=0) then yhoek(n1y); {hoek}
       if ((p1.y>p2.y)and(xy3dviewy<p2.y)) or ((p1.y<p2.y)and(xy3dviewy>p2.y))
          or (mode2=0) then yhoek(n2y); {hoek}

       if ((p1.x<p2.x)and(xy3dviewx>p2.x)) or ((p1.x>p2.x)and(xy3dviewx<p2.x))
          or (mode2=0) then xvlak(n1y,n1z,n2y,n2z,n2x); {groot vlak}
       if ((p1.x<p2.x)and(xy3dviewx<p1.x)) or ((p1.x>p2.x)and(xy3dviewx>p1.x))
          or (mode2=0) then xvlak(n2y,n3z,n1y,n2z,n1x); {klein vlak}

       if ((p1.z<p2.z)and(xy3dviewz>p2.z)) or ((p1.z>p2.z)and(xy3dviewz<p2.z))
          or (mode2=0) then zvlak(n1x,n1y,n2x,n2y,n2z); {groot vlak}
       if ((p1.z<p2.z)and(xy3dviewz<p1.z)) or ((p1.z>p2.z)and(xy3dviewz>p1.z))
          or (mode2=0) then zvlak(n3x,n2y,n2x,n1y,n1z); {klein vlak}
         end;

      3 : begin
       if ((p1.x>p2.x)and(xy3dviewx>p3.x)) or ((p1.x<p2.x)and(xy3dviewx<p3.x))
           or (mode2=0) then xvlak(n1y,n1z,n3y,n2z,n3x); {midden vlak}
       if ((p1.y>p2.y)and(xy3dviewy>p3.y)) or ((p1.y<p2.y)and(xy3dviewy<p3.y))
           or (mode2=0) then yvlak(n1x,n1z,n3x,n2z,n3y); {midden vlak}

       if ((p1.z>p2.z)and(xy3dviewz>p1.z)) or ((p1.z<p2.z)and(xy3dviewz<p1.z))
           or (mode2=0) then zhoek(n1z); {hoek}
       if ((p1.z>p2.z)and(xy3dviewz<p2.z)) or ((p1.z<p2.z)and(xy3dviewz>p2.z))
           or (mode2=0) then zhoek(n2z); {hoek}

       if ((p1.x>p2.x)and(xy3dviewx<p2.x)) or ((p1.x<p2.x)and(xy3dviewx>p2.x))
           or (mode2=0) then xvlak(n1y,n1z,n2y,n2z,n2x); {groot vlak}
       if ((p1.x>p2.x)and(xy3dviewx>p1.x)) or ((p1.x<p2.x)and(xy3dviewx<p1.x))
           or (mode2=0)  then xvlak(n2y,n1z,n3y,n2z,n1x); {klein vlak}

       if ((p1.y>p2.y)and(xy3dviewy<p2.y)) or ((p1.y<p2.y)and(xy3dviewy>p2.y))
           or (mode2=0) then yvlak(n1x,n1z,n2x,n2z,n2y); {groot vlak}
       if ((p1.y>p2.y)and(xy3dviewy>p1.y)) or ((p1.y<p2.y)and(xy3dviewy<p1.y))
           or (mode2=0) then yvlak(n2x,n1z,n3x,n2z,n1y); {klein vlak}
         end;

      4 : begin
       if ((p1.x>p2.x)and(xy3dviewx>p3.x)) or ((p1.x<p2.x)and(xy3dviewx<p3.x))
           or (mode2=0) then xvlak(n1y,n1z,n3y,n3z,n3x); {midden vlak}
       if ((p1.y>p2.y)and(xy3dviewy>p3.y)) or ((p1.y<p2.y)and(xy3dviewy<p3.y))
           or (mode2=0) then yvlak(n1x,n1z,n3x,n3z,n3y); {midden vlak}
       if ((p1.z<p2.z)and(xy3dviewz<p3.z)) or ((p1.z>p2.z)and(xy3dviewz>p3.z))
           or (mode2=0) then zvlak(n1x,n1y,n3x,n3y,n3z); {midden vlak}

       if ((p1.x>p2.x)and(xy3dviewx<p2.x)) or ((p1.x<p2.x)and(xy3dviewx>p2.x))
           or (mode2=0) then xvlak(n1y,n1z,n2y,n2z,n2x); {groot vlak}
       if ((p1.y>p2.y)and(xy3dviewy<p2.y)) or ((p1.y<p2.y)and(xy3dviewy>p2.y))
           or (mode2=0) then yvlak(n1x,n1z,n2x,n2z,n2y); {groot vlak}
       if ((p1.z<p2.z)and(xy3dviewz>p2.z)) or ((p1.z>p2.z)and(xy3dviewz<p2.z))
           or (mode2=0) then zvlak(n1x,n1y,n2x,n2y,n2z); {groot vlak}

       if ((p1.x>p2.x)and(xy3dviewx>p1.x)) or ((p1.x<p2.x)and(xy3dviewx<p1.x))
          or (mode2=0) then xhoek(n1x); {hoek}
       if ((p1.y>p2.y)and(xy3dviewy>p1.y)) or ((p1.y<p2.y)and(xy3dviewy<p1.y))
          or (mode2=0) then yhoek(n1y); {hoek}
       if ((p1.z>p2.z)and(xy3dviewz>p1.z)) or ((p1.z<p2.z)and(xy3dviewz<p1.z))
          or (mode2=0) then zhoek(n1z); {hoek}
        end;
    end;

  if dobl then showframe2(frcol);
  with graphs3d[igraph] do
    begin z1 := tmi; z2 := tma; vl := true; dp4d := datap4d; end;
end;

procedure xy4dshowvolume(p1,p2,p3:Tpoint3D;mode1:integer; {XXXXXXXXXXXXXXXXXXXX}
  frcol,fccol,lncol,f2col:Tcolor;mode2,split:integer;opt:integer);
begin xy4dshowvolume(p1,p2,p3,mode1,frcol,fccol,lncol,f2col,
      mode2,split,dummy,opt); end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure findelement(x,y,z:single;var tx,ty,tz:integer; var fx,fy,fz,h:single);
var fx1,fy1,fz1 : single;
begin
  if (x<xps[tx]) or (x>=xps[tx+1]) then
    if (x<=xps[1]) then begin tx := 1; fx := 0; end else
    if (x>=xps[nx]) then begin tx := nx-1; fx := 1; end else
     begin tx := nx; repeat dec(tx) until (xps[tx]<x);
       fx := (x-xps[tx])/(xps[tx+1]-xps[tx]); end
  else fx := (x-xps[tx])/(xps[tx+1]-xps[tx]);

  if (y<yps[ty]) or (y>=yps[ty+1]) then
    if (y<=yps[1]) then begin ty := 1; fy := 0; end else
    if (y>=xps[ny]) then begin ty := ny-1; fy := 1; end else
     begin ty := ny; repeat dec(ty) until (yps[ty]<y);
       fy := (y-yps[ty])/(yps[ty+1]-yps[ty]); end
  else fy := (y-yps[ty])/(yps[ty+1]-yps[ty]);

  if (z<zps[tz]) or (z>=zps[tz+1]) then
    if (z<=zps[1]) then begin tz := 1; fz := 0; end else
    if (z>=zps[nz]) then begin tz := nz-1; fz := 1; end else
     begin tz := nz; repeat dec(tz) until (zps[tz]<z);
       fz := (z-zps[tz])/(zps[tz+1]-zps[tz]); end
  else fz := (z-zps[tz])/(zps[tz+1]-zps[tz]);

  fx1 := 1-fx; fy1 := 1-fy; fz1 := 1-fz;
  h := datap4d^[tx  ,ty  ,tz  ]*fx1*fy1*fz1 +
       datap4d^[tx+1,ty+1,tz+1]*fx *fy *fz  +
       datap4d^[tx+1,ty  ,tz  ]*fx *fy1*fz1 +
       datap4d^[tx  ,ty+1,tz+1]*fx1*fy *fz  +
       datap4d^[tx  ,ty+1,tz  ]*fx1*fy *fz1 +
       datap4d^[tx+1,ty  ,tz+1]*fx *fy1*fz  +
       datap4d^[tx  ,ty  ,tz+1]*fx1*fy1*fz  +
       datap4d^[tx+1,ty+1,tz  ]*fx *fy *fz1 ;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
procedure xy4dsectvolume(a,b,c,d:single;p1,p2:Tpoint3d; {XXXXXXXXXXXXXXXXXXXXXX}
   frcol,fccol,lncol,f2col:Tcolor;mode:integer;opt:integer);
var pol,pol2 : polytype;
    tp, tp2 : Tpoint3d;
    pp, ppf, pfr : array of Tpoint;
    tpol,i,t: integer;
    tt,n,fact : single;
    xmi,xma,ymi,yma,zmi,zma,x,y,z,aa,bb,cc,dd,fx,fy,fz,fx1,fy1,fz1: single;
    ok,project,dofr,dobl,dogr : boolean;
    h : Thandle;
    rectxy,rectbm : Trect;
    xr0,yr0,xr1,yr1,xr,yr,wi,he,tx,ty,tz,p : integer;
    x1,y1,x2,y2,ix,iy,iz : integer;
    pxmi,pxma,pymi,pyma,pzmi,pzma : single;

procedure section(x1,x2,y1,y2,z1,z2:single); {---------------------------------}
  {bepaalt snijpunten van vlak met block}
var dx,dy,dz,ddx,ddy,ddz,xp,yp,zp : single;
procedure add; { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
begin inc(tpol); with pol[tpol-1] do begin x := xp; y := yp; z := zp; end; end;
procedure testx(y,z:single); {- - - - - - - - - - - - - - - - - - - - - - - - -}
begin
  xp := (d-b*y-c*z)/a; if (xp<x1-ddx) or (xp>x2+ddx) then exit;
  if (xp<x1+ddx) then xp := x1; if (xp>x2-ddx) then xp := x2;
  yp := y; zp := z; add;
end;
procedure testy(x,z:single); {- - - - - - - - - - - - - - - - - - - - - - - - -}
begin
  yp := (d-a*x-c*z)/b; if (yp<y1-ddy) or (yp>y2+ddy) then exit;
  if (yp<y1+ddy) then yp := y1; if (yp>y2-ddy) then yp := y2;
  xp := x; zp := z; add;
end;
procedure testz(x,y:single); {- - - - - - - - - - - - - - - - - - - - - - - - -}
begin
  zp := (d-a*x-b*y)/c; if (zp<z1-ddz) or (zp>z2+ddz) then exit;
  if (zp<z1+ddz) then zp := z1; if (zp>z2-ddz) then zp := z2;
  xp := x; yp := y; add;
end;
begin {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  dx := x2-x1; ddx := dx*fact; dy := y2-y1; ddy := dy*fact; dz := z2-z1; ddz := dz*fact;
  tpol := 0; {bereken snijpunten}
  if (a<>0) then begin testx(y1,z1); testx(y2,z1); testx(y1,z2); testx(y2,z2); end;
  if (b<>0) then begin testy(x1,z1); testy(x2,z1); testy(x1,z2); testy(x2,z2); end;
  if (c<>0) then begin testz(x1,y1); testz(x2,y1); testz(x1,y2); testz(x2,y2); end;
end;

procedure doelement(ix,iy,iz:integer); {---------------------------------------}
var x0,x1,x2,dx,y0,y1,y2,dy,z0,z1,z2,dz,d0 : single;
    fx,fy,fz,fx1,fy1,fz1,h0,tt,t0 : single;
    i,j,t : integer;
    h : array[0..23] of single;
    col : Tcolor;
    pol0 : Tpoint3d;
    p0 : Tpoint;
begin
  x1 := xps[ix]; x2 := xps[ix+1]; dx := x2-x1; x0 := (x1+x2)/2;
  y1 := yps[iy]; y2 := yps[iy+1]; dy := y2-y1; y0 := (y1+y2)/2;
  z1 := zps[iz]; z2 := zps[iz+1]; dz := z2-z1; z0 := (z1+z2)/2;
  d0:= a*x0+b*y0+c*z0-d;
  if (sqr(d0)*4>sqr(dx)+sqr(dy)+sqr(dz)) then exit;

  fact := 1/100; section(x1,x2,y1,y2,z1,z2);
  if (tpol<3) then exit;

  constructpolygon(tpol,pol,x1,x2,y1,y2,z1,z2);
  if (tpol<3) then exit;

  for i := 0 to tpol-1 do with pol[i] do
    begin
      fx := (x-x1)/dx; fy := (y-y1)/dy; fz := (z-z1)/dz;
      fx1 := 1-fx; fy1 := 1-fy; fz1 := 1-fz;
      h[i] := datap4d^[ix  ,iy  ,iz  ]*fx1*fy1*fz1 +
              datap4d^[ix+1,iy+1,iz+1]*fx *fy *fz  +
              datap4d^[ix+1,iy  ,iz  ]*fx *fy1*fz1 +
              datap4d^[ix  ,iy+1,iz+1]*fx1*fy *fz  +
              datap4d^[ix  ,iy+1,iz  ]*fx1*fy *fz1 +
              datap4d^[ix+1,iy  ,iz+1]*fx *fy1*fz  +
              datap4d^[ix  ,iy  ,iz+1]*fx1*fy1*fz  +
              datap4d^[ix+1,iy+1,iz  ]*fx *fy *fz1 ;
    end;
  t0 := 0; pol0 := point3d(0,0,0);
  with pol0 do begin
  for i := 0 to tpol-1 do
    begin
      x := x + pol[i].x; y := y + pol[i].y;
      z := z + pol[i].z; t0 := t0 + h[i];
    end;
  t0 := t0/tpol; x := x/tpol; y := y/tpol; z := z/tpol;
  p0 := topoint2(pol0);
  end;
  col := hcolor(t0,false);

  setlength(pp,tpol); for i := 0 to tpol-1 do pp[i] := topoint2(pol[i]);
  if (mode=3) then drawpoly(-1,col,pp,false)
  else
  begin
    for i := 0 to tpol-1 do
    begin
      j := i+1; if (j=tpol) then j := 0;
      tt := (t0+h[i]+h[j])/3; col := hcolor(tt,false);
      drawpoly(-1,col,[p0,pp[i],pp[j]],false);
    end;
  end;
end;

procedure dopoint(xr,yr:integer); {--------------------------------------------}
begin
  revpos(xr,yr,x,y,z,aa,bb,cc,dd);
  if not pixok then begin buf[xr-xr0] := 0; exit; end;
  findelement(x,y,z,tx,ty,tz,fx,fy,fz,tt);
  buf[xr-xr0] := hcolor(tt,true);
end;
procedure grid(col:Tcolor); {--------------------------------------------------}
var i : integer;
    x1,y1,z1,x2,y2,z2 : single;
begin
  xypen.color := col;
  if (b<>0) or (c<>0) then for i := 1 to nx do
    begin
      x := xps[i]; if (x>pxmi) and (x<pxma) then
        begin
          if (c=0) then
            begin z1 := zmi; z2 := zma; y1 := (d-a*x)/b; y2 := y1; end
          else
            begin
              y1 := ymi; z1 := (d-a*x-b*y1)/c;
              if (b<>0) then if (z1<zmi) then begin z1 := zmi; y1 := (d-a*x-c*z1)/b; end;
              if (b<>0) then if (z1>zma) then begin z1 := zma; y1 := (d-a*x-c*z1)/b; end;
              y2 := yma; z2 := (d-a*x-b*y2)/c;
              if (b<>0) then if (z2<zmi) then begin z2 := zmi; y2 := (d-a*x-c*z2)/b; end;
              if (b<>0) then if (z2>zma) then begin z2 := zma; y2 := (d-a*x-c*z2)/b; end;
            end;
          line3d(x,y1,z1,x,y2,z2);
        end;
    end;
  if (a<>0) or (c<>0) then for i := 1 to ny do
    begin
      y := yps[i]; if (y>pymi) and (y<pyma) then
        begin
          if (c=0) then
            begin z1 := zmi; z2 := zma; x1 := (d-b*y)/a; x2 := x1; end
          else
            begin
              x1 := xmi; z1 := (d-a*x1-b*y)/c;
              if (a<>0) then if (z1<zmi) then begin z1 := zmi; x1 := (d-b*y-c*z1)/a; end;
              if (a<>0) then if (z1>zma) then begin z1 := zma; x1 := (d-b*y-c*z1)/a; end;
              x2 := xma; z2 := (d-a*x2-b*y)/c;
              if (a<>0) then if (z2<zmi) then begin z2 := zmi; x2 := (d-b*y-c*z2)/a; end;
              if (a<>0) then if (z2>zma) then begin z2 := zma; x2 := (d-b*y-c*z2)/a; end;
            end;
          line3d(x1,y,z1,x2,y,z2);
        end;
    end;
  if (a<>0) or (b<>0) then for i := 1 to nz do
    begin
      z := zps[i]; if (z>pzmi) and (z<pzma) then
        begin
          if (b=0) then
            begin y1 := ymi; y2 := yma; x1 := (d-c*z)/a; x2 := x1; end
          else
            begin
              x1 := xmi; y1 := (d-a*x1-c*z)/b;
              if (a<>0) then if (y1<ymi) then begin y1 := ymi; x1 := (d-b*y1-c*z)/a; end;
              if (a<>0) then if (y1>yma) then begin y1 := yma; x1 := (d-b*y1-c*z)/a; end;
              x2 := xma; y2 := (d-a*x2-c*z)/b;
              if (a<>0) then if (y2<ymi) then begin y2 := ymi; x2 := (d-b*y2-c*z)/a; end;
              if (a<>0) then if (y2>yma) then begin y2 := yma; x2 := (d-b*y2-c*z)/a; end;
            end;
          line3d(x1,y1,z,x2,y2,z);
        end;
    end; 
end;

begin {------------------------------------------------------------------------}
 {  1 = projecteer }
 {  2 = show block }
 {  4 = show frame }
 {  8 = show frame en grid }
 { 16 = zwart/wit banden }
  if init then initxygraph4d;
  if not mode3d or (polartype>0) or stereo or not vol then exit;
  if (a=0) and (b=0) and (c=0) then exit;
  if (frcol<0) then frcol := frontcolor;
  if (fccol<0) then fccol := backcolor;
  if (lncol<0) then lncol := frcol;
  if (f2col<0) then f2col := frcol;
  project := (opt and 1)>0; dobl := (opt and 2)>0;
  dofr := (opt and 4)>0;    dogr := (opt and 8)>0;
  zw := (opt and 16)>0;
  ascol := frcol;
  if (mode<0) then mode := 0 else if (mode>5) then mode := 5;
  if (mode=5) then if dowmf or doprint then mode := 3
   else begin graphs3d[igraph].bmp4d := true; xybuffers[0].b4d := true; end;

  n := sqrt(sqr(a)+sqr(b)+sqr(c));
  a := a/n; b := b/n; c := c/n; d := d/n;
  xmi := blk.xmi; ymi := blk.ymi; zmi := blk.zmi;
  xma := blk.xma; yma := blk.yma; zma := blk.zma;
  if not equal(p1,p2) then
    begin
      x := min(p1.x,p2.x); if (x<xma) then xmi := max(x,xmi);
      x := max(p1.x,p2.x); if (x>xmi) then xma := min(x,xma);
      y := min(p1.y,p2.y); if (y<yma) then ymi := max(y,ymi);
      y := max(p1.y,p2.y); if (y>ymi) then yma := min(y,yma);
      z := min(p1.z,p2.z); if (z<zma) then zmi := max(z,zmi);
      z := max(p1.z,p2.z); if (z>zmi) then zma := min(z,zma);
    end;

  fact := 1/1000; section(xmi,xma,ymi,yma,zmi,zma);
  if (tpol<3) then exit;

  constructpolygon(tpol,pol,xmi,xma,ymi,yma,zmi,zma);
  if (tpol<3) then exit;

  pxmi := pol[0].x; pxma := pxmi; pymi := pol[0].y; pyma := pymi;
  pzmi := pol[0].z; pzma := pzmi; {bepaal grenzen polygon}
  for i := 1 to tpol-1 do with pol[i] do
    begin
      if (x<pxmi) then pxmi := x else if (x>pxma) then pxma := x;
      if (y<pymi) then pymi := y else if (y>pyma) then pyma := y;
      if (z<pzmi) then pzmi := z else if (z>pzma) then pzma := z;
    end;

  setlength(pfr,tpol); setlength(ppf,tpol);
  for i := 0 to tpol-1 do pfr[i] := topoint2(pol[i]);

  if project then with framexyz do
    begin
      point4[0] := topoint(xmi,ymi,z); point4[1] := topoint(xmi,xma,z);
      point4[2] := topoint(xma,xma,z); point4[3] := topoint(xma,xmi,z);
      for i := 0 to tpol-1 do
        begin pol2[i] := pol[i]; pol2[i].z := z; ppf[i] := topoint2(pol2[i]); end;
      drawpoly(frcol,fccol,point4,false); drawpoly(frcol,-1,ppf,false);
      point4[0] := topoint(xmi,y,zmi); point4[1] := topoint(xmi,y,zma);
      point4[2] := topoint(xma,y,zma); point4[3] := topoint(xma,y,zmi);
      for i := 0 to tpol-1 do
        begin pol2[i] := pol[i]; pol2[i].y := y; ppf[i] := topoint2(pol2[i]); end;
      drawpoly(frcol,fccol,point4,false); drawpoly(frcol,-1,ppf,false);
      point4[0] := topoint(x,ymi,zmi); point4[1] := topoint(x,ymi,zma);
      point4[2] := topoint(x,yma,zma); point4[3] := topoint(x,yma,zmi);
      for i := 0 to tpol-1 do
        begin pol2[i] := pol[i]; pol2[i].x := x; ppf[i] := topoint2(pol2[i]); end;
      drawpoly(frcol,fccol,point4,false); drawpoly(frcol,-1,ppf,false);
    end;

  if dobl then showframe1(frcol);

  case mode of
   0 : drawpoly(frcol,-1,pfr,false);
   1 : drawpoly(frcol,fccol,pfr,false);
   2 : begin drawpoly(frcol,fccol,pfr,false); grid(lncol); end;
  3,4 : begin
         for ix := 1 to nx-1 do
           for iy := 1 to ny-1 do
             for iz := 1 to nz-1 do
               doelement(ix,iy,iz);
         if dogr then grid(f2col);
         if dofr then drawpoly(f2col,-1,pfr,false);
       end;
   5 : begin
    h := createpolygonrgn(pfr[0],tpol,ALTERNATE); if (h=0) then exit;
    combinergn(h,h,clip0,RGN_AND); if (h=0) then exit;
    t := getrgnbox(h,rectxy); if (t=0) then begin deleteobject(h); exit; end;
    with rectxy do
      begin xr0 := left; yr0 := top; xr1 := right; yr1 := bottom; end;
    wi := xr1-xr0+1; he := yr1-yr0+1;
    if (bm.width<wi) then bm.width := wi;
    if (bm.height<he) then bm.height := he;
    rectbm := rect(0,0,wi-1,he-1);
    xycanvas.copymode := cmSrcCopy; bm.canvas.copymode := cmSrcCopy;
    bm.canvas.CopyRect(rectbm,xycanvas,rectxy);

    dd := d - a*xmid - b*ymid - c*zmid;
    aa := a*ifacx; bb := b*ifacy; cc := c*ifacz;

    tx := 1; ty := 1; tz := 1;
    t := getregiondata(h,sizeof(rgnbuf),@rgnbuf);
    if (t=0) then {geen data}
    for yr := yr0 to yr1 do
      begin
        buf := bm.scanline[yr-yr0];
        for xr := xr0 to xr1 do
          if ptinregion(h,xr,yr) then dopoint(xr,yr);
      end
    else
    for i := 1 to rgnbuf[3] do
      begin
        p := i*4 + 5;
        x1 := rgnbuf[p]; y1 := rgnbuf[p+1]; x2 := rgnbuf[p+2]; y2 := rgnbuf[p+3];
        for yr := y1 to y2-1 do
          begin
            buf := bm.scanline[yr-yr0];
            for xr := x1 to x2-1 do dopoint(xr,yr);
          end;
      end;
    deleteobject(h);
    xycanvas.copyrect(rectxy,bm.canvas,rectbm);
    if dogr then grid(f2col);
    if dofr then drawpoly(f2col,-1,pfr,false);
   end;
  end;

  if dobl then showframe2(frcol);
  with graphs3d[igraph] do
    begin z1 := tmi; z2 := tma; vl := true; dp4d := datap4d; end;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure xy4disovolume(h:single;p1,p2:Tpoint3d;frcol,fccol:Tcolor; {XXXXXXXXXX}
  mode:integer;light,trans:boolean;opt:integer);
var nx0,ny0,nz0,nxmi,nxma,nymi,nyma,nzmi,nzma : integer;
    xi,yi,zi,t,stm : integer;
    x,xmi,xma,y,ymi,yma,z,zmi,zma : single;
    dobl : boolean;
    frcol0,hcol : Tcolor;

procedure doelement(x,y,z:integer); {------------------------------------------}
var x1,x2,dx,y1,y2,dy,z1,z2,dz:single;
    h1,h2,h3,h4,h5,h6,h7,h8,hmin,hmax : single;
    pol : polytype;
    pp : array of Tpoint;
    tel,i : integer;
    col : Tcolor;
const fac = 1/1e6;
procedure add(xp,yp,zp:single); { - - - - - - - - - - - - - - - - - - - - - - -}
begin inc(tel); with pol[tel-1] do begin x := xp; y := yp; z := zp; end; end;
procedure checkx(x1,x2,h1,h2,y,z:single); { - - - - - - - - - - - - - - - - - -}
var x : single;
begin
  if (h1=h) or (h2=h) then
    begin if (h1=h) then add(x1,y,z); if (h2=h) then add(x2,y,z); end
  else
  if ( (h1<h) and (h2>h) ) or ( (h1>h) and (h2<h) ) then
    begin x := x1 + (h-h1)*(x2-x1)/(h2-h1); add(x,y,z); end;
end;
procedure checky(y1,y2,h1,h2,x,z:single); { - - - - - - - - - - - - - - - - - -}
var y : single;
begin
  if (h1=h) or (h2=h) then
    begin if (h1=h) then add(x,y1,z); if (h2=h) then add(x,y2,z); end
  else
  if ( (h1<h) and (h2>h) ) or ( (h1>h) and (h2<h) ) then
    begin y := y1 + (h-h1)*(y2-y1)/(h2-h1); add(x,y,z); end;
end;
procedure checkz(z1,z2,h1,h2,x,y:single); { - - - - - - - - - - - - - - - - - -}
var z : single;
begin
  if (h1=h) or (h2=h) then
    begin if (h1=h) then add(x,y,z1); if (h2=h) then add(x,y,z2); end
  else
  if ( (h1<h) and (h2>h) ) or ( (h1>h) and (h2<h) ) then
    begin z := z1 + (h-h1)*(z2-z1)/(h2-h1); add(x,y,z); end;
end;
begin { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  h1 := datap4d^[x,y,  z];     h2 := datap4d^[x+1,y,  z];
  h3 := datap4d^[x,y+1,z];     h4 := datap4d^[x+1,y+1,z];
  h5 := datap4d^[x,y,  z+1];   h6 := datap4d^[x+1,y,  z+1];
  h7 := datap4d^[x,y+1,z+1];   h8 := datap4d^[x+1,y+1,z+1];
  hmin := minvalue([h1,h2,h3,h4,h5,h6,h7,h8]);
  hmax := maxvalue([h1,h2,h3,h4,h5,h6,h7,h8]);
  if (hmin>=h) or (hmax<=h) then exit;

  x1 := xps[x]; x2 := xps[x+1]; dx := x2-x1;
  y1 := yps[y]; y2 := yps[y+1]; dy := y2-y1;
  z1 := zps[z]; z2 := zps[z+1]; dz := z2-z1;

  tel := 0;
  checkx(x1,x2,h1,h2,y1,z1); checkx(x1,x2,h3,h4,y2,z1);
  checkx(x1,x2,h5,h6,y1,z2); checkx(x1,x2,h7,h8,y2,z2);
  checky(y1,y2,h1,h3,x1,z1); checky(y1,y2,h2,h4,x2,z1);
  checky(y1,y2,h5,h7,x1,z2); checky(y1,y2,h6,h8,x2,z2);
  checkz(z1,z2,h1,h5,x1,y1); checkz(z1,z2,h2,h6,x2,y1);
  checkz(z1,z2,h3,h7,x1,y2); checkz(z1,z2,h4,h8,x2,y2);
  if (tel<3) then exit;

  constructpolygon(tel,pol,x1,x2,y1,y2,z1,z2);
  if (tel<3) then exit;

  if light and (zonmode>0) and (mode>0) then
    begin
      t := round(tel/3);
      col := color3(pol[0],pol[t],pol[t*2],fccol);
    end
  else col := fccol;
  setlength(pp,tel); for i := 0 to tel-1 do pp[i] := topoint2(pol[i]);
  case mode of
    0 : drawpoly(frcol, -1,pp,trans);
    1 : drawpoly(frcol,col,pp,trans);
    2 : drawpoly( -1,  col,pp,trans);
   end;  
end;

procedure dozplane(z:integer); {-----------------------------------------------}
var x,y : integer;
begin
  for x := nxmi to nx0-1 do     for y := nymi to ny0-1 do     doelement(x,y,z);
  for x := nxma-1 downto nx0 do for y := nymi to ny0-1 do     doelement(x,y,z);
  for x := nxmi to nx0-1 do     for y := nyma-1 downto ny0 do doelement(x,y,z);
  for x := nxma-1 downto nx0 do for y := nyma-1 downto ny0 do doelement(x,y,z);
end;
procedure doyplane(y:integer); {-----------------------------------------------}
var x,z : integer;
begin
  for x := nxmi to nx0-1 do     for z := nzmi to nz0-1 do     doelement(x,y,z);
  for x := nxma-1 downto nx0 do for z := nzmi to nz0-1 do     doelement(x,y,z);
  for x := nxmi to nx0-1 do     for z := nzma-1 downto nz0 do doelement(x,y,z);
  for x := nxma-1 downto nx0 do for z := nzma-1 downto nz0 do doelement(x,y,z);
end;
procedure doxplane(x:integer); {-----------------------------------------------}
var y,z : integer;
begin
  for y := nymi to ny0-1 do     for z := nzmi to nz0-1 do     doelement(x,y,z);
  for y := nyma-1 downto ny0 do for z := nzmi to nz0-1 do     doelement(x,y,z);
  for y := nymi to ny0-1 do     for z := nzma-1 downto nz0 do doelement(x,y,z);
  for y := nyma-1 downto ny0 do for z := nzma-1 downto nz0 do doelement(x,y,z);
end;
begin {------------------------------------------------------------------------}
 {  2 = show block }
 { 32 = lijn in kleur }
 { 64 = vlak in kleur }
  if init then initxygraph4d;
  if not mode3d or (polartype>0) or not vol then exit;
  if stereo then mode := 0 else if (mode>2) then mode := 2;
  if (h<=blk.tmi) or (h>=blk.tma) then exit;
  if (frcol<0) then frcol := frontcolor;
  if (fccol<0) then fccol := backcolor;
  frcol0 := frcol; hcol := hcolor(h,false);
  if (opt and 32 > 0) then frcol := hcol;
  if (opt and 64 > 0) then fccol := hcol;
  ascol := frcol;

  dobl := (opt and 2 > 0);

  xmi := blk.xmi-1; ymi := blk.ymi-1; zmi := blk.zmi-1;
  xma := blk.xma+1; yma := blk.yma+1; zma := blk.zma+1;
  if not equal(p1,p2) then
    begin
      x := min(p1.x,p2.x); if (x<xma) then xmi := max(x,xmi);
      x := max(p1.x,p2.x); if (x>xmi) then xma := min(x,xma);
      y := min(p1.y,p2.y); if (y<yma) then ymi := max(y,ymi);
      y := max(p1.y,p2.y); if (y>ymi) then yma := min(y,yma);
      z := min(p1.z,p2.z); if (z<zma) then zmi := max(z,zmi);
      z := max(p1.z,p2.z); if (z>zmi) then zma := min(z,zma);
    end;
  nxmi := 0; for xi := nx downto 1 do if (xps[xi]>=xmi) then nxmi := xi; if (nxmi=0) then exit;
  nxma := 0; for xi := 1 to nx do     if (xps[xi]<=xma) then nxma := xi; if (nxma=0) then exit;
  nymi := 0; for yi := ny downto 1 do if (yps[yi]>=ymi) then nymi := yi; if (nymi=0) then exit;
  nyma := 0; for yi := 1 to ny do     if (yps[yi]<=yma) then nyma := yi; if (nyma=0) then exit;
  nzmi := 0; for zi := nz downto 1 do if (zps[zi]>=zmi) then nzmi := zi; if (nzmi=0) then exit;
  nzma := 0; for zi := 1 to nz do     if (zps[zi]<=zma) then nzma := zi; if (nzma=0) then exit;
  if (nxmi>=nxma) or (nymi>=nyma) or (nzmi>=nzma) then exit;

  if (xy3dviewx<=xps[nxmi]) then nx0 := nxmi else
  if (xy3dviewx>=xps[nxma]) then nx0 := nxma-1 else
    begin nx0 := nxma; repeat dec(nx0) until (xps[nx0]<xy3dviewx); end;
  if (xy3dviewy<=yps[nymi]) then ny0 := nymi else
  if (xy3dviewy>=yps[nyma]) then ny0 := nyma-1 else
    begin ny0 := nyma; repeat dec(ny0) until (yps[ny0]<xy3dviewy); end;
  if (xy3dviewz<=zps[nzmi]) then nz0 := nzmi else
  if (xy3dviewz>=zps[nzma]) then nz0 := nzma-1 else
    begin nz0 := nzma; repeat dec(nz0) until (zps[nz0]<xy3dviewz); end;

  for stm := 1 to 2 do if (stm=1) or stereo then begin

  if stereo then
    begin setstereo(stm); frcol := stereocol[stm]; frcol0 := frcol; end;

  if dobl then showframe1(frcol0);

  if (kijkhoogte>45)  then for zi := nzmi to nzma-1     do dozplane(zi) else
  if (kijkhoogte<-45) then for zi := nzma-1 downto nzmi do dozplane(zi) else
  if (kijkhoek<45)    then for yi := nyma-1 downto nymi do doyplane(yi) else
  if (kijkhoek<135)   then for xi := nxma-1 downto nxmi do doxplane(xi) else
  if (kijkhoek<225)   then for yi := nymi to nyma-1     do doyplane(yi) else
  if (kijkhoek<315)   then for xi := nxmi to nxma-1     do doxplane(xi) else
                           for yi := nyma-1 downto nymi do doyplane(yi);

  if dobl then showframe2(frcol0);

  end;

  with graphs3d[igraph] do
    begin z1 := tmi; z2 := tma; vl := true; dp4d := datap4d; end;
end;

procedure shutdownxygraph4d; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin bm.free; end;

procedure initxygraph4d; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  bm := Tbitmap.create; bm.pixelformat := pf32bit; init := false;
  closing := shutdownxygraph4d;
end;

end.
