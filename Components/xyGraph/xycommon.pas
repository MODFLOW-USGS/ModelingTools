unit xycommon;

{=========================================================================}
{ See Copyright notes at XYGRAPH.                                         }
{=========================================================================}

interface

{$R+}

uses Windows , {Messages,} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    {StdCtrls, Spin,} ExtCtrls, Clipbrd{, AppEvnts}, xygraph;

{ these procedures and variables are to be used by XYGRAPH and XYGRAPH3D}
{ do not call them directly }

type polytype = array of Tpoint;
     polypoint = ^polytype;

procedure setauton(n:integer;v:single;a:integer);
procedure setn(n:integer;v:single);
procedure resetn(n:integer);
procedure resetall;
procedure autoreset(code:integer);
function getn(n:integer):single;
function prm(n:integer):single;
function prmi(n:integer):integer;

function mixcolor(cl1,cl2:Tcolor;f1,f2 : integer):Tcolor;
function reversecolor(cl:integer):integer;
procedure setresscale(res:integer);

procedure copyscreen(mode:integer);
procedure restorescreen;

procedure plotrect(p1,p2,p3,p4:Tpoint;c1,c2,c3,c4,mode:integer);

procedure position0(x,y,z:single;var xp,yp:integer);

procedure printpart(x1,y1,x2,y2:integer);
procedure store;
procedure restore;
procedure setcursor(xp,yp:integer);
procedure xypixel(xp,yp:integer;col:Tcolor);
procedure xyrectangle(col1,col2,x1,y1,x2,y2:integer);
procedure xyline(col,x1,y1,x2,y2:integer);
procedure xypolygon(col1,col2:integer;poly:array of Tpoint);
procedure transpolygon(col1,col2:integer;poly:array of Tpoint);
procedure xycircle(col1,col2,x,y,r:integer);
procedure transcircle(col1,col2,x,y,r:integer);
procedure drawpoly(col1,col2:tcolor;poly:array of Tpoint;trans:boolean);
procedure clippoly(var p:polypoint;x1,y1,x2,y2:integer);
procedure hatchbar(x1,x2,y1,y2:integer;bcol,hcol,fcol:Tcolor;style,wi1,wi2:integer);

procedure simpletext(cl:Tcolor;txt:string;xp,yp,xjust,yjust:integer); 
procedure bigtext(cl:Tcolor;txt:string;xp0,yp0,xjust,yjust,mode,angle,code:integer);
procedure labelangle(cl:Tcolor;txt:string;mode,bi,xp,yp,ang:integer;var dl:integer);

procedure initwmf;
procedure openwmf(var s:string;var ok:boolean);
procedure closewmf(var ok:boolean);
procedure initgrid;
procedure dogrid(p1,p2:integer;xas:boolean;col:Tcolor);
procedure checkwmfrgpen;
procedure checkwmfpen;
procedure wmfredpen;
procedure wmfgrnpen;
procedure checkwmfbrush;
procedure checkwmffont;
procedure linewmf(x1,y1,x2,y2:integer);
procedure blokwmf(x1,y1,x2,y2:integer);
procedure ellipsewmf(x1,y1,x2,y2:integer);
procedure rop2wmf(n:integer);
procedure polygonwmf(points: array of Tpoint);
procedure addfine(p:integer);
procedure addcoarse(p:integer);
procedure pixelwmf(x,y:integer; col:Tcolor);
procedure movewmf(x,y:integer);
procedure drawwmf(x,y:integer);
procedure textwmfalign(n:integer);
procedure textwmfout(x,y:integer;s:string);
procedure bitmapwmf(x1,y1,x2,y2:integer);

procedure inittext3d(f,x1,x2,y1,y2:single;var ok:boolean);
procedure exittext3d; 
procedure text3d(s:string;x,y,z:single;m1:integer;lbl:boolean;
       oct,ml:integer;zlvert:boolean;f:single);

procedure initxycommon;

var xycanvas : Tcanvas;
    xypaintbox : Tpaintbox;
    frontcolor, backcolor : Tcolor;   {default kleuren}
    oldfcolor, oldbcolor : Tcolor;    { idem }
    dowmf, doprint : boolean;         {bezig met metafile of print}
    useroff : boolean;                {user coord uitgeschakeld}
    mode3d : boolean;                 {3D mode}
    igraph, ngraph : integer;         {actuele grafiek, aantal grafieken}
    res : integer;                    {resolutie}
    oldpen : Tpen;                    {vorige pen}
    oldbrush : Tbrush;                {vorige brush}
    cwidth, cheight : integer;        {afmetingen canvas}
    cvmode : boolean;                 {canvas mode}
    vol : boolean;                    {er is een volume}
    xyfontangle : integer;            {hoek van font}
    transmode : integer;              {transparancy mode}
    transval : single;                {transparancy level}
    transopt : integer;               {transparancy options}
    prxoff,pryoff : integer;          {printer offset}
    prfac :integer;                   {printer factor}
    prshow : boolean;                 {toon voortgang}
    crsok : boolean;                  {mag crs maken}
    polartype : integer;              {polaire coordinaten: 0 = cart
                                        1=pol 2=radar 3-cyl 4-spher}
    stereo : boolean;                 {stereo aan rood/groen}
    ffac,prcor : single;              {correctiefactor fonts}
    xoff,yoff : integer;              {offset}
    linestyle : integer;              {line style, <0=none, 0=solid, >0 = dash}
    stereocol : array[0..3] of Tcolor; {kleur}
    framexyz : record  x,y,z : single; end; {coordinaten van 3D frame}
    colors, revcolors : array[0..255] of Tcolor; {kleurenschaal en reverse}
    resscale : array[0..255] of Tcolor; {resolutieschaal}
    sx1,sx2,sy1,sy2 : integer;        {x/y van hoogteschaal}
    clip0 : Thandle = 0;                  {overall clip}
    wmfbk : boolean;                  {achtergrond in wmf}
    setscale : boolean;               {set schaal vast}
    scale3d : single;                 {overall schaal factor}

type scaletype = record               {vaste schaal instellingen}
               min,max,fm,fac : single;
               nc,nf,ndec : integer;
               fix,go : boolean;
               exp : string;
            end;

var fscale : scaletype;
    fs3d : array[1..4] of scaletype;

type symboltype = record
                   xp,yp:integer;
                   style,size,fill,width,cd3d : word;
                   color,cl1,cl2,cl3:Tcolor;
                 end;
var  lastsymbol : symboltype;         {laatst gemelde xysymbol}

     screencopy : Tbitmap;            {kopie van beeldscherm}

var graphfield, plotfield : record x1,x2,y1,y2 : integer; end;

type datapointer = ^Tdatatype;
type data4dpointer = ^T4dtype;

type graftype3d = record                       {3d uitbreidingen van buf}
                  dp,cp : datapointer;         {data tabel,kleuren tabel}
                  dp4d : data4dpointer;        {4d data tabel}
                  m3d,sf,vl,bmp4d : boolean;   {3d mode, er is een surface, vol, 4d bmp}
                  scl,cnt,isbmp : boolean;     {er is een schaal/contour, cnt is bmp}
                  xs1,xs2,ys1,ys2 : integer;   {scherm pos schaal}
                  xc1,xc2,yc1,yc2 : integer;   {scherm pos contour}
                  z1,z2,c1,c2 : single;        {hoogtes}
                  xf0,xf1,yf0,yf1 : single;    {zoom factoren}
                  s1,s2,s3,s4,s5,s6 : single;  {surface waarden}
                  hm,drm : integer;            {hoogtemode, driehmode}
                  f3dsize : integer;           {font size ivm 3d text}
                  empt : single;               {leeg veld}
                  hasempt : boolean;           {er is een leegveld}
                  end;
     grafstype3d = array[1..maxgraf] of graftype3d;

var graphs3d : grafstype3d;
    xmid,ymid,zmid : single;      {middens van assen}
    facx,facy,facz : single;      {schaal factoren voor assen}
    ifacx,ifacy,ifacz : single;   {inverse factoren}
    sinp,cosp,sina,cosa : single; {sin en cos van kijkhoek}
    isinp,icosp,isina,icosa : single; {inverse factoren}
    sinr,cosr : single;           {sin en cos van draaiing}
    fac3d, r3d : single;          {reken factoren}
    ifac3d, ir3d : single;        {inverse factoren}
    xpc, xpc0, ypc : integer;     {quasi midden vam beeldscherm}
    xc0, yc0 : integer;           {echte midden van beeldscherm}
    pixx3d, pixy3d, pixz3d : single; {aantal pixels voor lengte 1}
    kijkhoek,kijkhoogte : integer;{-id-}
    viewx,viewy,viewz : single;   {kijkpunt}
    empty : single;               {lege waarde bij surface}
    hasempty : boolean;           {er is een lege waarde}
    antial : boolean;             {anti alias bij 3D text}
    zonmode : integer;            {modus zonlicht}
    zoomc : boolean;              {zoom op centrum}
    stereomode : integer;

    fr : single;

type fonttype = record name: string; size:integer; style:Tfontstyles; end;
procedure storefont (var f:fonttype);
procedure recallfont(var f:fonttype);

implementation

{$I-}

uses printers, math;

var ff : file;
    textbm, textbm2 : Tbitmap;    {tekst in achtergrond}
    textbuf : array of array of boolean;
    textfac : single;

    ticx, ticy, ticz : single;    {tics in user coord}
    ticx2, ticy2, ticz2 : single; {tics in user coord}
    xmi,xma,ymi,yma : single;     {bereik}

const pixs : array[0..15] of byte = (0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4);
var cols3d : array[0..4] of Tcolor;

type regel = array[0..8000] of integer;
     pregel = ^regel;
var  bufx : pregel;
     charbuf : array of array of byte;

var setval : array of single;     {actuele waarden}
    setnrs : array of byte;       {vertaal tabel}
    setaut : array of shortint;   {auto reset waarden}
    maxsetnr : integer;           {hoogste parameter nummer}
    setperm : integer;            {parameter autoreset}

type setting = record             {instelling van obscure parameters}
               nr : integer;      {codenr}
               def : single;      {act waarde, default waarde}
               min,max : single;  {min en max bereik}
               isint : boolean;   {integer waarde of niet}
               txt:string;        {beschrijving}
               end;

const settings : array[1..41] of setting = (
   {(nr:0 ; def:0;  min:0;  max:3;  isint:true; txt:'auto reset') , quasi entry }
    (nr:1;  def:1;  min:0;  max:1;  isint:true; txt:'antialias font') ,
    (nr:2;  def:20; min:10; max:100;isint:true; txt:'legend breedte') ,
    (nr:3;  def:0;  min:0;  max:1;  isint:true; txt:'mode colour scale') ,
    (nr:4;  def:0;  min:0;  max:1;  isint:true; txt:'symbol exponent') ,
    (nr:5;  def:0;  min:0;  max:1;  isint:true; txt:'disable zoom autoredraw') ,
    (nr:10; def:1e3;min:1;  max:1e5;isint:false;txt:'max zoom depth') ,
    (nr:11; def:2;  min:1;  max:100;isint:false;txt:'min log ratio') ,

    (nr:20; def:5;  min:1;  max:100;isint:false;txt:'label dist X float') ,
    (nr:21; def:8;  min:1;  max:100;isint:false;txt:'label dist X exp') ,
    (nr:22; def:1;  min:0.2;max:5;  isint:false;txt:'fineness log X') ,
    (nr:23; def:1;  min:0;  max:3;  isint:false;txt:'major ticsize X') ,
    (nr:24; def:0.6;min:0;  max:1;  isint:false;txt:'minor ticsize X') ,
    (nr:25; def:0;  min:-90;max:90; isint:true; txt:'angle text X') ,
    (nr:26; def:0;  min:0;  max:0;  isint:false;txt:'format sci labels X') ,
    (nr:27; def:0;  min:0;  max:1;  isint:true; txt:'do not plot 0 X') ,

    (nr:40; def:2;  min:1;  max:100;isint:false;txt:'label dist Y') ,
    (nr:41; def:0;  min:0;  max:1;  isint:true; txt:'rotate vert caption Y') ,
    (nr:42; def:1;  min:0.2;max:5;  isint:false;txt:'fineness log Y'),
    (nr:43; def:1;  min:0;  max:3;  isint:false;txt:'major ticsize Y') ,
    (nr:44; def:0.6;min:0;  max:1;  isint:false;txt:'minor ticsize Y') ,
    (nr:45; def:0;  min:-90;max:90; isint:true; txt:'angle text Y') ,
    (nr:46; def:0;  min:0;  max:0;  isint:false;txt:'format sci labels Y') ,
    (nr:47; def:0;  min:0;  max:1;  isint:true; txt:'do not plot 0 Y' ) ,

    (nr:60; def:2;  min:1;  max:100;isint:false;txt:'label dist R') ,
    (nr:61; def:20; min:1;  max:1e3;isint:false;txt:'max zoom depth R') ,
    (nr:62; def:1;  min:0.1;max:10; isint:false;txt:'fineness grid polar'),
    (nr:63; def:1;  min:0;  max:3;  isint:false;txt:'major ticsize R') ,
    (nr:64; def:0.6;min:0;  max:1;  isint:false;txt:'minor ticsize R') ,
    (nr:66; def:0;  min:0;  max:0;  isint:false;txt:'format sci labels R') ,

    (nr:100;def:5;  min:1;  max:100;isint:false;txt:'label dist X 1') ,
    (nr:101;def:2;  min:1;  max:10; isint:false;txt:'label dist X 2') ,
    (nr:102;def:1;  min:0;  max:3;  isint:false;txt:'ticsize X') ,
    (nr:110;def:5;  min:1;  max:100;isint:false;txt:'label dist Y 1') ,
    (nr:111;def:2;  min:1;  max:100;isint:false;txt:'label dist Y 2') ,
    (nr:112;def:1;  min:0;  max:3;  isint:false;txt:'ticsize Y') ,
    (nr:120;def:5;  min:1;  max:100;isint:false;txt:'label dist Z 1') ,
    (nr:121;def:2;  min:1;  max:100;isint:false;txt:'label dist Z 2') ,
    (nr:122;def:0.6;min:0;  max:3;  isint:false;txt:'ticsize Z') ,
    (nr:130;def:5;  min:1;  max:100;isint:false;txt:'label dist R 1') ,
    (nr:131;def:2;  min:1;  max:100;isint:false;txt:'label dist R 2') ,
    (nr:132;def:1;  min:0;  max:3;  isint:false;txt:'ticsize R') );


function getn(n:integer):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (n=0) then begin result := setperm; exit; end;
  if (n<1) or (n>maxsetnr) then result := setval[0]
  else result := prm(n);
end;

procedure setn(n:integer;v:single); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {set een waarde}
var t : integer;
begin
  if (n=0) then
    begin if (v<0) then v := 0 else if (v>3) then v := 3;
      setperm := round(v); exit; end;
  if (n<1) or (n>maxsetnr) then exit;
  t := setnrs[n]; if (t=0) then exit;
  with settings[t] do
    begin
      if (min<>0) and (max<>0) then
        begin
          if (v<min) then v := min;
          if (v>max) then v := max;
        end;
      if isint then if (v>=0) then v := int(v+0.5)
        else v := -int(-v+0.5);
    end;
  setval[t] := v;
end;

procedure setauton(n:integer;v:single;a:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var t : integer;
begin
  if (n=0) then begin setn(0,v); exit; end;
  if (n<1) or (n>maxsetnr) then exit;
  t := setnrs[n]; if (t=0) then exit;
  if (a>3) or (a<0) then a := -1;
  setaut[t] := a; setn(n,v);
end;

procedure resetn(n:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {reset een waarde}
var t : integer;
begin
  if (n=0) then begin setperm := 0; exit; end;
  if (n<1) or (n>maxsetnr) then exit;
  t := setnrs[n]; if (t=0) then exit;
  setval[t] := settings[t].def;
  setaut[t] := -1;
end;

procedure autoreset(code:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {reset alle setwaarden}
var i : integer;
begin
  if (setperm=code) then
     for i := 1 to length(settings) do
       if (setaut[i]=-1) then setval[i] := settings[i].def;
  for i := 1 to length(settings) do
     if (setaut[i]=code) then setval[i] := settings[i].def;
end;

procedure resetall; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i : integer;
begin
  for i := 1 to length(settings) do
    begin setval[i] := settings[i].def; setaut[i] := -1; end;
end;

procedure initsettings; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
 {initialiseert setwaarden}
var i,n,m,t : integer;
begin
  n := length(settings);
  m := 0; for i := 1 to n do
    if (settings[i].nr>m) then m := settings[i].nr;
  maxsetnr := m;
  setlength(setval,n+1); for i := 1 to n do setval[i] := settings[i].def;
  setlength(setaut,n+1); for i := 0 to n do setaut[i] := -1;
  setlength(setnrs,m+1); for i := 0 to m do setnrs[i] := 0;
  for i := 1 to n do
    begin t := settings[i].nr; setnrs[t] := i; end;
  resetn(0); setval[0] := -1; {resultaat bij fout nummer}
  setresscale(1);
end;

function prm(n:integer):single; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin result := setval[setnrs[n]]; end;

function prmi(n:integer):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin result := round(setval[setnrs[n]]); end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure plotrect(p1,p2,p3,p4:Tpoint;c1,c2,c3,c4,mode:integer); {XXXXXXXXXXXXX}
const maxsplit = 8;
var nodes : array[-1..maxsplit+1,-1..maxsplit+1] of Tpoint;
    colrs : array[-1..maxsplit+1,-1..maxsplit+1] of integer;
    n,x,y,cl,i : integer;
begin
  if (mode<0) then n := 2*(-mode)-2 else n := mode;
  if (n>maxsplit) then n := maxsplit;
  if (n<2) then
    begin
      cl := (c1+c2+c3+c4) div 4;
      xycanvas.brush.color := colors[resscale[cl]];
      xycanvas.pen.color := xycanvas.brush.color;
      xypolygon(-1,-1,[p1,p2,p3,p4]); exit;
    end;
  nodes[0,0] := p1; nodes[0,n] := p2; nodes[n,n] := p3; nodes[n,0] := p4;
  colrs[0,0] := c1; colrs[0,n] := c2; colrs[n,n] := c3; colrs[n,0] := c4;
  for x := 1 to n-1 do
    begin
      nodes[x,0].x := (x*nodes[n,0].x + (n-x)*nodes[0,0].x) div n;
      nodes[x,0].y := (x*nodes[n,0].y + (n-x)*nodes[0,0].y) div n;
      colrs[x,0]   := (x*colrs[n,0]   + (n-x)*colrs[0,0])   div n;
      nodes[x,n].x := (x*nodes[n,n].x + (n-x)*nodes[0,n].x) div n;
      nodes[x,n].y := (x*nodes[n,n].y + (n-x)*nodes[0,n].y) div n;
      colrs[x,n]   := (x*colrs[n,n]   + (n-x)*colrs[0,n])   div n;
    end;
  for x := 0 to n do for y := 1 to n-1 do
    begin
      nodes[x,y].x := (y*nodes[x,n].x + (n-y)*nodes[x,0].x) div n;
      nodes[x,y].y := (y*nodes[x,n].y + (n-y)*nodes[x,0].y) div n;
      colrs[x,y]   := (y*colrs[x,n]   + (n-y)*colrs[x,0]  ) div n;
    end;
  if (mode<0) then
    begin
      for i := 0 to n do begin
        nodes[-1,i] := nodes[0,i]; nodes[n+1,i] := nodes[n,i];
        nodes[i,-1] := nodes[i,0]; nodes[i,n+1] := nodes[i,n]; end;
      for x := 0 to n do for y := 0 to n do
      if (odd(x) and odd(y)) or ( (not odd(x)) and (not odd(y)) ) then
        begin
         p1 := nodes[x-1,y]; p2 := nodes[x,y-1]; p3 := nodes[x+1,y]; p4 := nodes[x,y+1];
         cl := colrs[x,y];
         xycanvas.brush.color := colors[resscale[cl]];
         xycanvas.pen.color := xycanvas.brush.color;
         xypolygon(-1,-1,[p1,p2,p3,p4]);
       end;
    end
  else
    for x := 0 to n-1 do for y := 0 to n-1 do
      begin
        p1 := nodes[x,y]; p2 := nodes[x,y+1]; p3 := nodes[x+1,y+1]; p4 := nodes[x+1,y];
        cl := (colrs[x,y] + colrs[x,y+1] + colrs[x+1,y+1] + colrs[x+1,y]) div 4;
        xycanvas.brush.color := colors[resscale[cl]];
        xycanvas.pen.color := xycanvas.brush.color;
        xypolygon(-1,-1,[p1,p2,p3,p4]);
      end;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure storefont(var f:fonttype);
begin with xyfont do begin f.name := name; f.size := size; f.style := style; end; end;

procedure recallfont(var f:fonttype);
begin with xyfont do begin name := f.name; size := f.size; style := f.style; end; end;

function align(xj,yj:integer):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
  (*   0   6   2 top
      24  30  26 baseline!
       8  14  10 bottom *)
begin
  if (xj=-1) then result := 2 else
  if (xj= 0) then result := 6 else
                  result := 0;
  if (yj=-1) then result := result + 8;
end;

procedure labelangle(cl:Tcolor;txt:string;mode,bi,xp,yp,ang:integer;var dl:integer);
 {mode: 1=X; 2=Y; bi = richting}
var tw,th,dth,aa : integer;
    si,co,dh,dw : single;
begin
  if (txt='') then exit; aa := abs(ang);
  th := xycharheight; tw := xycanvas.textwidth(txt); dth := th div 6;
  si := sin(aa*pi/180); co := cos(ang*pi/180);
  dh := tw*si + th*co; dw := tw*co + th*si;
  if (mode=1) then {X}
    begin
      yp := yp + round(th*co/2)*bi;
      if (aa=90) then yp := yp + dth*bi;
      if (ang>0) then bigtext(cl,txt,xp,yp,-bi,0,0,ang,1)
                 else bigtext(cl,txt,xp,yp, bi,0,0,ang,1);
      dl := round(dh); if (aa=90) then dl := dl + dth;
    end
  else {Y}
    begin
      if (ang=90)  then bigtext(cl,txt,xp,yp,0, bi,0,ang,1) else
      if (ang=-90) then bigtext(cl,txt,xp,yp,0,-bi,0,ang,1) else
      begin
        xp := xp + round(th*si/2)*bi;
        bigtext(cl,txt,xp,yp,bi,0,0,ang,1);
      end;
      dl := round(dw);
    end;
end;

procedure simpletext(cl:Tcolor;txt:string;xp,yp,xjust,yjust:integer); {XXXXXXXX}
begin
  if (txt='') then exit;
  if (xp<xoff) or (xp>=cwidth+xoff) or (yp<yoff) or (yp>=cheight+yoff) then exit;
  if (yjust=0) then
     begin yp := yp - xycharheight div 2; yjust := 1; end;
  settextalign(xycanvas.handle,align(xjust,yjust));
  if stereo then xyfont.color := stereocol[0]
    else if (cl>=0) then xyfont.color := cl;
  if dowmf then
    begin checkwmffont; textwmfout(xp,yp,txt) end
  else xycanvas.textout(xp,yp,txt);
  settextalign(xycanvas.handle,0);
end;

procedure bigtext(cl:Tcolor;txt:string;xp0,yp0,xjust,yjust,mode,angle,code:integer);
 { mode 0=trans,noframe; 1=transp,frame; 2=obl,noframe; 3 = obl,frame
  code: 1=no compensation; 2 = reduced height; 4 = existing colors;}
var lines : array of string;
    nlines,p,wi,he,w,i,xp1,yp1,d,xo,yo,xp,yp,xx,yy,cl1,cl2,lhe,dhe,xpt : integer;
    ang1, ang2 : integer;
    red, col, nocomp : boolean;
    si,co : single;
    LogRec: TLOGFONT;
    OldFontHandle, NewFontHandle: HFONT;
    point4 : array[0..3] of Tpoint;
const del = '|';
      dd = 3;
procedure rotate(var x,y : integer);
var dw,dh :integer;
begin
   dw := xo-x; dh := yo-y;
   x := xo-round(dh*si+dw*co);
   y := yo-round(dh*co-dw*si);
end;
begin
  nocomp := (code and 1 >0); {geen breedte compensatie}
  red := (code and 2 >0);    {gereduceerde hoogte}
  col := (code and 4 >0);    {existing colors}
  mode := mode and 3;

  if (angle=0) and (mode=0) and (pos(del,txt)=0) then
    begin
      if col then cl := -1;
      if not nocomp then
        if (xjust=1)  then xp0 := xp0 + dd*res else
        if (xjust=-1) then xp0 := xp0 - dd*res;
      xybrush.style := bsclear;
      simpletext(cl,txt,xp0,yp0,xjust,yjust);
      exit;
    end;

  if (txt='') then exit;
  if (xp0<xoff) or (xp0>=cwidth+xoff) or (yp0<yoff) or (yp0>=cheight+yoff) then exit;

  try setgraphicsmode(xycanvas.handle,gm_compatible); except end;
  while (angle<0)   do inc(angle,360);
  while (angle>359) do dec(angle,360);
  ang1 := angle * 10; ang2 := ang1;

  if (angle=0) then begin si := 0; co := 1 end else
  begin si := sin(angle/180*pi); co := cos(angle/180*pi); end;
  xo := xp0; yo := yp0;
  if red then lhe := round(xycharheight*0.75) else
              lhe := round(xycharheight*0.95);
  dhe := xycharheight-lhe;

  if nocomp then d := 0 else d := dd * res;
  setlength(lines,1); p := pos(del,txt); nlines := 0;
  if (p=0) then
    begin lines[0] := txt; nlines := 1; end
  else
    begin
      repeat
        inc(nlines); setlength(lines,nlines);
        lines[nlines-1] := copy(txt,1,p-1);
        txt := copy(txt,p+1,length(txt));
        p := pos(del,txt);
      until (p=0);
      inc(nlines); setlength(lines,nlines);
      lines[nlines-1] := txt;
    end;
  he := nlines*lhe + dhe; wi := 0;
  for i := 0 to nlines-1 do
    begin
      w := xycanvas.textwidth(lines[i]);
      if (w>wi) then wi := w;
    end;

  wi := wi + d+d; xpt := xp0;
  if (xjust=0) then xp0 := xp0-wi div 2 else
   if (xjust<0) then xp0 := xp0 - wi;
  if (yjust=0) then yp0 := yp0-he div 2 else
   if (yjust<0) then yp0 := yp0 - he;

  xp1 := xp0+wi; yp1 := yp0+he+1; xypen.width := res;
  xx := xp0; yy := yp0; rotate(xx,yy); point4[0] := point(xx,yy);
  xx := xp1; yy := yp0; rotate(xx,yy); point4[1] := point(xx,yy);
  xx := xp1; yy := yp1; rotate(xx,yy); point4[2] := point(xx,yy);
  xx := xp0; yy := yp1; rotate(xx,yy); point4[3] := point(xx,yy);

  if stereo then cl := stereocol[0] else if (cl<0) then cl := xyfont.color;
  if col then begin cl1 := xypen.color; cl2 := xybrush.color; end
    else begin cl1 := cl; cl2 := backcolor; end;
  case mode of
    1 : drawpoly(cl1,-1 ,point4,false);
    2 : drawpoly(cl2,cl2,point4,false);
    3 : drawpoly(cl1,cl2,point4,false);
  end;

  with xycanvas do
    begin
      if not col then font.color := cl;
      brush.style := bsclear;
      if (ang1<>0) or (ang2<>0) then
        begin
          setgraphicsmode(handle,2);
          GetObject(Font.Handle,SizeOf(LogRec),Addr(LogRec));
          LogRec.lfEscapement := ang1;
          LogRec.lfOrientation := ang2;
          if (prmi(1)=1) then LogRec.lfQuality := 5 {=anti aliasing}
                         else LogRec.lfQuality := 0;
          NewFontHandle := CreateFontIndirect(LogRec);
          OldFontHandle := SelectObject(Handle,NewFontHandle);
          xyfontangle := angle;
        end;

      settextalign(xycanvas.handle,align(xjust,1));
      if dowmf then checkwmffont;
      if (xjust=1)  then xpt := xpt+d else
      if (xjust=-1) then xpt := xpt-d;
      for i := 0 to nlines-1 do
        begin
          xp := xpt; yp := yp0+i*lhe;
          if (angle<>0) then rotate(xp,yp);
          if dowmf then textwmfout(xp,yp,lines[i])
                      else textout(xp,yp,lines[i]);
       end;

      if (angle<>0) or (ang2<>0) then
        begin
          NewFontHandle := SelectObject(Handle,OldFontHandle);
          DeleteObject(NewFontHandle);
        end;
      xyfontangle := 0;
    end;
  settextalign(xycanvas.handle,0);
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXX EX COMMON XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

function red(cl:Tcolor):integer;
begin result := colortorgb(cl) shr 0 and $FF; end;

function green(cl:Tcolor):integer;
begin result := colortorgb(cl) shr 8 and $FF; end;

function blue(cl:Tcolor):integer;
begin result := colortorgb(cl) shr 16 and $FF; end;

function mixcolor(cl1,cl2:Tcolor;f1,f2 : integer):Tcolor;
begin
  if (f1<0) or (f2<0) or (f1+f2=0) then begin result := clblack; exit; end;
  result :=
   ( ( red(cl1)   * f1 + red(cl2)   * f2 ) div (f1+f2) ) and $FF shl 0 +
   ( ( green(cl1) * f1 + green(cl2) * f2 ) div (f1+f2) ) and $FF shl 8 +
   ( ( blue(cl1)  * f1 + blue(cl2)  * f2 ) div (f1+f2) ) and $FF shl 16;
end;

function reversecolor(cl:integer):integer; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{kleur omkeer RGB -> BGR voor direct mem}
begin
  result := (cl and $FF00)
         or (cl and $FF) shl 16
         or (cl and $FF0000) shr 16 ;
end;

procedure setresscale(res:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var i,n : integer;
begin
  if (res<1) then res := 1 else if (res>255) then res := 255;
  if (res=1) then for i := 0 to 255 do resscale[i] := i
  else if prmi(3) = 0 then
  for i := 0 to 255 do
    begin n := (i div res) * res; resscale[i] := n; end
  else
  for i := 0 to 255 do
    begin n := round(i/res)*res; if (n>255) then n := 255; resscale[i] := n; end;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{maskblt bitblt stretchblt setrop2 getrgnbox selectcliprgn}

procedure transparent(h:Thandle;col:tcolor;edge:boolean); {XXXXXXXXXXXXXXXXXXXX}
var r,g,b,tr,f1,f2,x0,y0,x1,y1,wi,he,x,y,yy,xp,t : integer;
    rectxy, rectbm : Trect;
    bm : Tbitmap;
    p : Pbytearray;
    oy : boolean;
begin
  r := col and $ff; g := col shr 8 and $ff; b := col shr 16 and $ff;

  tr := round(transval*8);
  if (tr<0) then tr := 0 else if (tr>8) then tr := 8;
  f1 := 256*(8-tr); f2 := tr;

  t := getrgnbox(h,rectxy); if (t=0) or (t=nullregion) then exit;
  with rectxy do
    begin x0 := left; y0 := top; x1 := right-1; y1 := bottom-1; end;
  {if edge then begin inc(x0); inc(y0); end;}
  if (x0>xoff+cwidth) or (y0>yoff+cheight) or (x1<xoff) or (y1<yoff) then exit;
  if (x0<xoff) then x0 := xoff; if (x1>xoff+cwidth)  then x1 := xoff+cwidth;
  if (y0<yoff) then y0 := yoff; if (y1>yoff+cheight) then y1 := yoff+cheight;
  he := y1-y0+1; wi := x1-x0+1;
  if (he<1) or (wi<1) then exit;

  bm := Tbitmap.create; bm.pixelformat := pf24bit;
  bm.width := wi; bm.height := he;
  rectbm := rect(0,0,wi,he); rectxy := rect(x0,y0,x1+1,y1+1);
  xycanvas.copymode := cmSrcCopy; bm.canvas.copymode := cmSrcCopy;
  bm.canvas.CopyRect(rectbm,xycanvas,rectxy);

  if (transmode=2) then {echte transparancy}

  for y := 0 to he-1 do
    begin
      p := bm.scanline[y]; yy := y+y0; xp := 0;
      for x := x0 to x0+wi-1 do if ptinregion(h,x,yy) then
        begin
          p[xp] := (b * (f1+p[xp]*f2)) shr 11; inc(xp);
          p[xp] := (g * (f1+p[xp]*f2)) shr 11; inc(xp);
          p[xp] := (r * (f1+p[xp]*f2)) shr 11; inc(xp);
        end else inc(xp,3);
    end

  else {quasi transparancy}

  for y := 0 to he-1 do
    begin
      p := bm.scanline[y]; yy := y+y0; xp := 0; oy := odd(yy);
      for x := x0 to x0+wi-1 do
       if ((odd(x) xor oy) and ptinregion(h,x,yy) ) then
         begin
           p[xp] := b; inc(xp);
           p[xp] := g; inc(xp);
           p[xp] := r; inc(xp);
         end else inc(xp,3);
    end;

  xycanvas.copyrect(rectxy,bm.canvas,rectbm);
  bm.free;
end;

procedure drawtransparent(x,y,r:integer;poly:array of Tpoint); {XXXXXXXXXXXXXXX}
var mode,col1,col2 : integer;
    h : Thandle;
    edge : boolean;
begin
  if length(poly)>1 then mode := 2
  else if (r>0) then mode := 1
  else exit;

  col2 := xybrush.color; col1 := xypen.color; h := 0;
  edge := (col1<>col2) or (transopt and 2 > 0);

  if (dowmf) then {alleen transmode=1}
    begin
      checkwmfpen; checkwmfbrush;
      rop2wmf(r2_maskpen);
      if (mode=2) then polygonwmf(poly)
        else ellipsewmf(x-r,y-r,x+r+1,y+r+1);
      rop2wmf(r2_copypen);
      if edge then
        begin
          xybrush.style := bsclear;
          checkwmfbrush;
          if (mode=2) then polygonwmf(poly)
            else ellipsewmf(x-r,y-r,x+r+1,y+r+1);
          xybrush.style := bssolid;
        end;
      exit;
    end;

  if (transmode>1) then
    begin
      if (mode=2) then h := createpolygonrgn(poly,length(poly),ALTERNATE)
        else h := createellipticrgn(x-r,y-r,x+r+1,y+r+1);
      if (h<>0) then begin transparent(h,col2,edge); deleteobject(h); end;
    end;

  if (transmode=1) or (h=0) then
    begin
      setrop2(xycanvas.handle,r2_maskpen);
      if (mode=2) then xycanvas.polygon(poly)
       else xycanvas.ellipse(x-r,y-r,x+r+1,y+r+1);
      setrop2(xycanvas.handle,r2_copypen);
    end;

  if edge then
    begin
      xybrush.style := bsclear;
      if (mode=2) then xycanvas.polygon(poly)
       else xycanvas.ellipse(x-r,y-r,x+r+1,y+r+1);
      xybrush.style := bssolid;
    end;
  xybrush.color := col2;
end;

procedure position0(x,y,z:single;var xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXXXXXX}
var x1,x2,y1,y2,ff,t,f : single;
begin
  x := (x-xmid)*facx; y := (y-ymid)*facy; z := (z-zmid)*facz;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp; {z1 := z}    {draaiing om z-as}
  x2 := x1; y2 := y1*sina + z*cosa; {z2 = y1*cosh - z1*sinh} {draaiing om as in xy vlak}
  if stereo then {draaiing om z-as bij stereo}
    begin t := x2; x2 := x2*cosr - y2*sinr; y2 := t*sinr + y2*cosr; end;
  if (r3d=0) then ff := fac3d else
    ff := fac3d*r3d/(r3d+y1*cosa-z*sina);       {factor voor perspectief}
  if zoomc then f := scale3d else f := 1;
  xp := round(xc0+(xpc-xc0)*f+ff*x2);
  yp := round(yc0+(ypc-yc0)*f-ff*y2);
  {xp := round(xpc+ff*x2); yp := round(ypc-ff*y2);}
end;

procedure positionr(x,y,z:single;var xs,ys:single); {XXXXXXXXXXXXXXXXXXXXXXXXX}
var x1,x2,y1,y2,ff,t : single;
begin
  x := (x-xmid)*facx; y := (y-ymid)*facy; z := (z-zmid)*facz;
  x1 := x*cosp - y*sinp; y1 := x*sinp + y*cosp; {z1 := z}    {draaiing om z-as}
  x2 := x1; y2 := y1*sina + z*cosa; {z2 = y1*cosh - z1*sinh} {draaiing om as in xy vlak}
  if stereo then {draaiing om z-as bij stereo}
    begin t := x2; x2 := x2*cosr - y2*sinr; y2 := t*sinr + y2*cosr; end;
  if (r3d=0) then ff := fac3d else
    ff := fac3d*r3d/(r3d+y1*cosa-z*sina);       {factor voor perspectief}
  xs := (xpc+ff*x2+0.5); ys := (ypc-ff*y2+0.5);
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure printpart(x1,y1,x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
type regel = array[0..8000] of integer;
     pregel = ^regel;
var x,y,xp,yp,xx,xxp,pw,xp0,yp0 : integer;
    col : Tcolor;
    buf : pregel;
begin
  pw := xypen.width; xypen.width := 1;
  xp0 := prxoff - prfac div 2; yp0 := pryoff - prfac div 2;
  if (not doprint) and prshow then xypen.mode := pmnot;
  for y := y1 to y2 do
    begin
     if not doprint then
       if prshow then with xycanvas do begin moveto(x1,y); lineto(x2,y); end;
     buf := screencopy.scanline[y]; yp := yp0 + y * prfac; x := x1;
     while (x<=x2) do
      begin
       col := buf[x];
       if (col<>clwhite) then
         begin
          xx := x; while(xx<x2) and (buf[xx+1]=col) do inc(xx);
          xp := xp0 + x * prfac; xxp := xp0 + xx * prfac;
          printer.canvas.brush.color := reversecolor(col);
          printer.canvas.pen.color := printer.canvas.brush.color;
          printer.canvas.rectangle(xp,yp,xxp+prfac,yp+prfac);
          x := xx;
         end;
       inc(x);
     end;
   end;
   xypen.mode := pmcopy;
   xypen.width := pw;
end;

procedure copyscreen(mode:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var w,h : integer;
    crect : Trect;
begin
  screencopy := Tbitmap.create;
  w := cwidth; h := cheight;
  with screencopy do
    begin
      if (mode=1) then pixelformat := pf24bit else
      if (mode=2) then pixelformat := pf32bit;
      width := w; height := h;
      crect := rect(0,0,w,h);
      canvas.copymode := cmSrcCopy;
      canvas.copyrect(crect,xycanvas,crect);
    end;
end;

procedure restorescreen; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var crect : Trect;
begin
   with screencopy do crect := rect(0,0,width,height);
   xycanvas.copyrect(crect,screencopy.canvas,crect);
   screencopy.free;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

var col3d : array[0..15] of tcolor;

procedure inittext3d(f,x1,x2,y1,y2:single;var ok:boolean); {XXXXXXXXXXXXXXXXXXX}
var h,i,ch,l : integer;
begin
  ch := xycanvas.textheight('0'); l := ch*5;
  textbm := Tbitmap.create; {hierin wordt de dummy tekst gemaakt}
  with textbm do
    begin
      pixelformat := pf8bit;
      canvas.pen.color := clwhite;
      canvas.brush.color := clwhite;
    end;
  textbm2 := Tbitmap.create; {hierin wordt ev. de echte 3D tekst geplot}
  with textbm2 do
    begin
      pixelformat := pf32bit;
      canvas.copymode := cmSrcCopy;
      width := l;
      height := width;
    end;

  textfac := f; if antial then textfac := textfac * 2;
  ok := true;
  with textbm.canvas.font do
    begin
      assign(xycanvas.font);
      color := clblack;
      with graphs3d[igraph] do
      if not doprint then
        begin
          f3dsize := size;
          size := round(xycanvas.font.size*textfac);
          textfac := size/xycanvas.font.size;
        end
      else
        if (f3dsize=0) then begin ok := false; textbm.free; exit; end
        else size := round(f3dsize*res*textfac*prcor);
    end;
  h := textbm.canvas.textheight('0'); textbm.height := h;
  setlength(textbuf,h); for i := 0 to h-1 do setlength(textbuf[i],40);
  setlength(charbuf,l+1); for i := 0 to l do setlength(charbuf[i],l+2);

  ticx := ch/pixx3d; ticy := ch/pixy3d; ticz := ch/pixz3d;
  ticx2 := ticx/2;   ticy2 := ticy/2;   ticz2 := ticz/2;
  xmi := x1;  xma := x2; ymi := y1; yma := y2;
end;

procedure exittext3d; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  textbm.free; textbm2.free;
  setlength(charbuf,1);
  setlength(charbuf[0],1);
end;

procedure showtext3d(s:string;col:Tcolor;x,y,z:single; {XXXXXXXXXXXXXXXXXXXXXXX}
         m1,m2,m3:integer;kp,av,cl:boolean);
  {m1: 0=xy vlak; 1 = xz vlak; 2 = yz vlak}
  {m2: 0=0, 1=90, 2=180, 3=270}
  {m3: just: 0=bo, 1=li, 2=on, 3=re}
  {kp= extra op de kop; av=extra achterstevoren; cl = gedraaid}
var tw,th,tw0,th0,xr,yr,xp,yp,xt,yt,c1,c2,i,j,t : integer;
    u0,v0,du,dv,uw,vw,u,v : single;
    ipx,ipy,ipz,d,xs,ys : single;
    ipu,ipv,fu,fv,dipu,dipv : single;
    rot : boolean;
    xa,ya,xb,yb,xc,yc,xd,yd : integer;
    x1,y1,x2,y2,dx,dy,p1,p2 : integer;
    rectbm, rectxy : Trect;
const orient : array[0..3] of integer = (1,0,2,3);

procedure setstring(m:integer); {----------------------------------------------}
var i,j,w,h,n,d,w2 : integer;
    p : pbytearray;
    ss : string;
    LogRec: TLOGFONT;
    OldFontHandle, NewFontHandle: HFONT;
  {m: 0=normaal; 1=op de kop; 2 = achterstevoren; 3 = gedraaid}
begin
  th := textbm.height;
  if cl then
   begin
    d := round(th*0.9); n := length(s); tw := th+(n-1)*d;
    with textbm do with canvas do  begin if (width<tw) then width := tw;
      rectangle(0,0,tw,th); brush.style := bsclear;
      GetObject(Font.Handle,SizeOf(LogRec),Addr(LogRec));
      LogRec.lfEscapement:= 900; {10th of a degree}
      LogRec.lfOrientation:= 900;
      NewFontHandle := CreateFontIndirect(LogRec);
      OldFontHandle := SelectObject(Handle,NewFontHandle);
      for i := 1 to n do begin ss := s[i]; w2 := textwidth(ss);
            textout((i-1)*d,(th+w2) div 2,s[i]); end;
      NewFontHandle := SelectObject(Handle,OldFontHandle);
      DeleteObject(NewFontHandle);
      brush.style := bssolid;
      end;
   end
  else
   begin
    tw := textbm.canvas.textwidth(s);
    with textbm do with canvas do begin if (width<tw) then width := tw;
      rectangle(0,0,tw,th); textout(0,0,s);end;
   end;
  h := th-1; w := tw-1;
  if (length(textbuf[0])<tw) then for i := 0 to h do setlength(textbuf[i],tw);
  case m of
    0 : for i := 0 to h do begin p := textbm.scanline[i];
      for j := 0 to w do textbuf[i,j] := (p^[j]=0); end;
    1 : for i := 0 to h do begin p := textbm.scanline[h-i];
      for j := 0 to w do textbuf[i,j] := (p^[j]=0); end;
    2 : for i := 0 to h do begin p := textbm.scanline[i];
      for j := 0 to w do textbuf[i,j] := (p^[w-j]=0); end;
    3 : for i := 0 to h do begin p := textbm.scanline[h-i];
      for j := 0 to w do textbuf[i,j] := (p^[w-j]=0); end;
   end;
end;
procedure position(u,v:single;var xr,yr: integer); {---------------------------}
begin
  case m1 of 0 : position0(u,v,z,xr,yr);
             1 : position0(u,y,v,xr,yr);
             2 : position0(x,u,v,xr,yr); end;
end;
begin {------------------------------------------------------------------------}
  if (m1<0) or (m1>2) then exit;
  position0(x,y,z,xr,yr);
    if (xr<xoff) or (xr>=xoff+cwidth) or (yr<yoff) or (yr>=yoff+cheight) then exit;
  m2 := m2 and 3; m3 := m3 and 3; rot := odd(m2);
  c1 := orient[m2]; if kp then c1 := c1 xor 1; if av then c1 := c1 xor 2;
  setstring(c1);  c2 := (m2+m3) and 3;
  tw0 := trunc(tw/textfac)+1; th0 := trunc(th/textfac)+1;
  ipx := 1/pixx3d; ipy := 1/pixy3d; ipz := 1/pixz3d;
  d := th0/6; if (m3=3) then d := -d;
  if antial then {maak mengkleuren}
    begin
      cols3d[0] := backcolor;  cols3d[4] := col;
      cols3d[1] := mixcolor(backcolor,col,3,1);
      cols3d[2] := mixcolor(backcolor,col,1,1);
      cols3d[3] := mixcolor(backcolor,col,1,3);
      if not (dowmf or doprint) then
        for i := 0 to 4 do cols3d[i] := reversecolor(cols3d[i]);
      for i := 0 to 15 do col3d[i] := cols3d[pixs[i]];
    end
  else if (dowmf or doprint) then col3d[15] := col
       else col3d[15] := reversecolor(col);

  case m1 of
   0 {XY}: begin u := x; v := y; ipu := ipx; ipv := ipy; end;
   1 {XZ}: begin u := x; v := z; ipu := ipx; ipv := ipz; end;
   2 {YZ}: begin u := y; v := z; ipu := ipy; ipv := ipz; end;
  end;
  fu := ipu/textfac; fv := ipv/textfac; dipu := d*ipu; dipv := d*ipv;
  if rot then begin du := th0*ipu; dv := tw0*ipv; end   
         else begin du := tw0*ipu; dv := th0*ipv; end;
  case c2 of
    0 : begin u0 := u-du/2; v0 := v-dv;   end;
    1 : begin u0 := u;      v0 := v-dv/2; end;
    2 : begin u0 := u-du/2; v0 := v;      end;
    3 : begin u0 := u-du;   v0 := v-dv/2; end;
  end;
  if (m3 in [1,3]) then
     case m2 of 0 : u0 := u0+dipu; 1 : v0 := v0+dipv;
                2 : u0 := u0-dipu; 3 : v0 := v0-dipv; end;
  position(u0,v0,xa,ya);    position(u0+du,v0,xb,yb);
  position(u0,v0+dv,xc,yc); position(u0+du,v0+dv,xd,yd);
  x1 := minintvalue([xa,xb,xc,xd]); x2 := maxintvalue([xa,xb,xc,xd]); dx := x2-x1;
  y1 := minintvalue([ya,yb,yc,yd]); y2 := maxintvalue([ya,yb,yc,yd]); dy := y2-y1;
  {xycanvas.rectangle(x1,y1,x2,y2);}
  if (length(charbuf)<(dx+1)) then setlength(charbuf,dx+1);
  for i := 0 to dx do
    begin
      if (length(charbuf[i])<dy+2) then setlength(charbuf[i],dy+2);
      fillchar(charbuf[i,0],dy+2,0);
    end;

  for xp := 0 to tw-1 do for yp := 0 to th-1 do if textbuf[yp,xp] then
    begin
      if rot then begin uw := u0+yp*fu; vw := v0 + xp*fv; end
             else begin uw := u0+xp*fu; vw := v0 + yp*fv; end;
      case m1 of 0 : positionr(uw,vw,z,xs,ys);
                 1 : positionr(uw,y,vw,xs,ys);
                 2 : positionr(x,uw,vw,xs,ys);
               end;
      xr := trunc(xs); yr := trunc(ys); xt := xr-x1; yt := yr-y1;
      if (xt>=0) and (xt<=dx) and (yt>=0) and (yt<=dy) then
      if antial then {maak mengpixels}
        begin
          t := 1; if round(xs)>xr then t := 2; if round(ys)>yr then t := t * 4;
          charbuf[xt,yt] := charbuf[xt,yt] or t;
        end
      else charbuf[xt,yt] := 15;
    end;

  if doprint then {kopieer de pixels}
    for xp := 0 to dx do for yp := 0 to dy do
      begin t := charbuf[xp,yp]; if (t>0) then
         xycanvas.pixels[xp+x1,yp+y1] := col3d[t]; end
  else
  if dowmf then
    if antial then {kopieer de pixels}
      for xp := 0 to dx do for yp := 0 to dy do
        begin t := charbuf[xp,yp]; if (t>0) then
          pixelwmf(xp+x1,yp+y1,col3d[t]); end
    else {maak lijnen}
      begin
        store; xypen.width := 1; xypen.color := col; checkwmfpen;
        for xp := 0 to x2-x1 do
         begin
           p1 := -1;
             repeat
             repeat inc(p1) until (charbuf[xp,p1]>0) or (p1>dy);
             if (p1<=dy) then
               begin
                 p2 := p1-1; repeat inc(p2) until charbuf[xp,p2]=0;
                 if (p2-p1=1) then pixelwmf(xp+x1,p1+y1,col) {xycanvas.pixels[xp+x1,p1+y1] := col}
                 else linewmf(xp+x1,p1+y1,xp+x1,p2+y1);      {xyline(-1,xp+x1,p1+y1,xp+x1,p2+y1);}
                p1 := p2;
              end;
            until (p1>dy);
         end;
        restore;
      end
  else {op beeldscherm}
    begin
      rectxy := rect(x1,y1,x2,y2); rectbm := rect(0,0,dx,dy);
      with textbm2 do
        begin
          if (width<dx+1) then width := dx+1;
          if (height<dy+1) then height := dy+1; end;
      textbm2.canvas.CopyRect(rectbm,xycanvas,rectxy);
      for yp := 0 to dy do
        begin
          bufx := textbm2.scanline[yp];
          if stereo and (stereomode=2) then
            for xp := 0 to dx do begin t := charbuf[xp,yp];
            if (t>0) then bufx[xp] := bufx[xp] or col3d[t]; end
          else
            for xp := 0 to dx do begin t := charbuf[xp,yp];
             if (t>0) then bufx[xp] := col3d[t]; end;
        end;
      xycanvas.copyrect(rectxy,textbm2.canvas,rectbm);
    end;
end;

procedure text3d(s:string;x,y,z:single;m1:integer;lbl:boolean; {XXXXXXXXXXXXXXX}
       oct,ml:integer;zlvert:boolean;f:single);
  {m1 :  1-4  = X-as, 5-8  = Y-as, 9-12 = Z-as, 13-16 = hoeken}
var x2,y2,z2,d : single;
    kp,av,cl : boolean;
    vl,ri,ju : integer;
    xp1,xp2,yp1,yp2:integer;
begin
  if lbl and (m1<13) then if odd(m1) then ml := -1 else dec(m1);
  kp := false; av := false; x2 := x; y2 := y; z2 := z; ju := 0; cl := false;
  if lbl and (m1 in [9..12]) then cl := zlvert;
  case m1 of
   1 : begin
      kp := (kijkhoogte<0); vl := 0;
      if not lbl then d := f*ticy else if (ml<0) then d := f*ticy + ticy
        else d := f*ticy + ml*ticy2;
      if (y<ymid) then d := -d; y2 := y+d;
      if (y>ymid) then ri := 2 else ri := 0;
    end;
   2 : begin
      kp := (kijkhoogte<0); vl := 0;
      if oct in [1,2,3,4] then ri := 3 else ri := 1;
      if oct in [3,4,7,8] then ju := 3 else ju := 1;
      if (y<ymid) then y2 := y - f*ticy else y2 := y + f*ticy;
    end;
   3 : begin
      if (viewy>ymi) and (viewy<yma) then exit;
      if not lbl then d := f*ticz else if (ml<0) then d := f*ticz + ticz
        else d := f*ticz + ml*ticz2;
      if (kijkhoogte<0) then begin d := -d; ju := 2; end;
      z2 := z-d; ri := 0; vl := 1; av := (y>ymid);
    end;
   4: begin
      if (viewy>ymi) and (viewy<yma) then exit;
      if oct in [1,2,5,6] then ri := 3 else ri := 1;
      ju := 4-ri; kp := (oct in [3,4,5,6]);
      if (kijkhoogte<0) then z2 := z + f*ticz else z2 := z - f*ticz;
      vl := 1; if (kijkhoogte<0) then ri := (ri + 2) and 3;
    end;

  5 : begin
      kp := (kijkhoogte<0); vl := 0;
      if not lbl then d := f*ticx else if (ml<0) then d := f*ticx + ticx
        else d := f*ticx + ml*ticx2;
      if (x<xmid) then d := -d; x2 := x+d;
      if (x>xmid) then ri := 1 else ri := 3;
    end;
   6 : begin
      kp := (kijkhoogte<0); vl := 0;
      if (oct in [3,4,5,6]) then ri := 2 else ri := 0;
      if (oct in [1,2,5,6]) then ju := 3 else ju := 1;
      if (x<xmid) then x2 := x - f*ticx else x2 := x + f*ticx;
    end;
  7 : begin
      if (viewx>xmi) and (viewx<xma) then exit;
      if not lbl then d := f*ticz else if (ml<0) then d := f*ticz + ticz
        else d := f*ticz + ml*ticz2;
      if (kijkhoogte<0) then begin d := -d; ju := 2; end;
      z2 := z-d; ri := 0; vl := 2; av := (x<xmid);
    end;
   8: begin
      if (viewx>xmi) and (viewx<xma) then exit;
      if (oct in [1,2,7,8]) then ri := 3 else ri := 1;
      ju := 4-ri; av := (oct in [1,2,3,4]);
      if (kijkhoogte<0) then z2 := z + f*ticz else z2 := z - f*ticz;
      vl := 2; if (kijkhoogte<0) then ri := (ri + 2) and 3;
    end;

  9,11:  begin
      { 1,4,5,8 = aan y-as; 1,2,4,7 = aan neg.zijde}
      if oct in [1,4,5,8] then
        if not lbl then d := f*ticx else if (ml<0) then d := f*ticx+ticx
              else d := f*ticx + ml*ticx2
      else
        if not lbl then d := f*ticy else if (ml<0) then d := f*ticy+ticy
              else d := f*ticy + ml*ticy2;
      if (oct in [1,2,4,7]) xor (m1=11) then begin d := -d; ri := 3; end else ri := 1;
      if oct in [1,4,5,8] then begin x2 := x+d; vl := 1; end
                          else begin y2 := y+d; vl := 2; end;
      av := oct in [2,3,4,5];
      if (kijkhoogte<0) and not cl then begin kp := true; av := not av; end;
      if cl then if oct in [2,4,6,8] then
        begin kp := true; av := not av; end;
      if (m1=11) and cl then begin kp := not kp; av := not av; end;
    end;
  10,12:  begin
      if oct in [1,4,5,8] then d := f*ticx else d := f*ticy;
      ri := 0;
      if (oct in [1,2,4,7]) xor (m1=12) then
           begin d := -d; ju := 3; end else ju := 1;
      if oct in [1,4,5,8] then
        begin x2 := x+d; vl := 1; end else begin y2 := y+d; vl := 2; end;
      av := oct in [2,3,4,5];
    end;

  13,15 : begin
         if (x=0) then begin vl := 1; av := (oct in [3,4,5,6]); end
                  else begin vl := 2; av := (oct in [1,2,3,4]); end;
         if (kijkhoogte<0) then begin ju := 0; z2 := z-ticz2; end
          else begin ju := 2; z2 := z+ticz2; end; ri := 0;
       end;
  14,16 : begin
         if (x=0) then
            begin vl := 1; kp := (oct in [3,4,5,6]);
            if oct in [1,2,5,6] then ri := 3 else ri := 1; end
         else
            begin vl := 2; kp := (oct in [1,2,3,4]);
            if oct in [1,2,5,6] then ri := 1 else ri := 3; end;
         if (kijkhoogte<0) then z2 := z-ticz2 else z2 := z+ticz2;
         ju := ri; if (kijkhoogte<0) then ri := (ri + 2) and 3;
         end;

    end;
  if not lbl then
    begin
      position0(x, y, z, xp1,yp1);
      position0(x2,y2,z2,xp2,yp2);
      xyline(-1,xp1,yp1,xp2,yp2);
    end;
  showtext3d(s,xyfont.color,x2,y2,z2,vl,ri,ju,kp,av,cl);
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXX EX XYGRAF XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure store; begin oldpen.assign(xypen); oldbrush.assign(xybrush); end;
procedure restore; begin xypen.assign(oldpen); xybrush.assign(oldbrush); end;

procedure setcursor(xp,yp:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  {xp := (xp*res)+xoff; yp := (yp*res)+yoff;}
  if dowmf then movewmf(xp,yp) else xycanvas.moveto(xp,yp);
end;

procedure setorigin(x,y:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  with xycanvas do
    begin
      unrealizeobject(brush.handle);
      setbrushorgex(handle,x,y,nil);
      selectobject(handle,brush.handle);
    end;
end;

procedure xypixel(xp,yp:integer;col:Tcolor); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
var df1,df2 : integer;
begin
  if (res=1) then
    if dowmf then pixelwmf(xp,yp,col)
         else xycanvas.pixels[xp,yp] := col
  else
    begin
      store; xypen.width := 1; xybrush.style := bssolid;
      df1 := res div 2; df2 := res-df1;
      xyrectangle(col,col,xp-df1,yp-df1,xp+df2,yp+df2);
      restore;
    end;
end;

procedure xyrectangle(col1,col2,x1,y1,x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end;
  if dowmf then
    begin checkwmfpen; checkwmfbrush; blokwmf(x1,y1,x2,y2); end
  else xycanvas.rectangle(x1,y1,x2,y2);
end;

procedure xypolygon(col1,col2:integer;poly:array of Tpoint); {XXXXXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end;
  if dowmf then
    begin checkwmfpen; checkwmfbrush; polygonwmf(poly); end
  else xycanvas.polygon(poly);
end;

procedure transpolygon(col1,col2:integer;poly:array of Tpoint); {XXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end;
  drawtransparent(0,0,0,poly);
end;

procedure xycircle(col1,col2,x,y,r:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end
  else if (col1>=0) then xypen.color := col1;
  if dowmf then
    begin checkwmfpen; checkwmfbrush; ellipsewmf(x-r,y-r,x+r+1,y+r+1); end
  else xycanvas.ellipse(x-r,y-r,x+r+1,y+r+1);
end;

procedure transcircle(col1,col2,x,y,r:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (col2>=0) then
    begin
     xybrush.color := col2;
     if (col1<0) then xypen.color := col2 else xypen.color := col1;
    end;
  drawtransparent(x,y,r,[]);
end;

procedure xyline(col,x1,y1,x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  if (col>=0) then xypen.color := col;
  if dowmf then begin checkwmfpen; linewmf(x1,y1,x2,y2); end
  else begin xycanvas.moveto(x1,y1); xycanvas.lineto(x2,y2); end;
end;

procedure drawpoly(col1,col2:tcolor;poly:array of Tpoint;trans:boolean); {XXXXX}
var i : integer; 
begin
  if (col1<0) and (col2<0) then col1 := frontcolor;
  with xycanvas do
  if (col2<0) then
    begin
      pen.color := col1;
      if dowmf then
        begin checkwmfpen; movewmf(poly[0].x,poly[0].y);
          for i := 1 to length(poly)-1 do drawwmf(poly[i].x,poly[i].y);
          drawwmf(poly[0].x,poly[0].y);
        end
      else
        begin
          moveto(poly[0].x,poly[0].y);
          for i := 1 to length(poly)-1 do lineto(poly[i].x,poly[i].y);
          lineto(poly[0].x,poly[0].y);
        end;
    end
  else
    begin
      brush.color := col2; if (col1<0) then col1 := col2; pen.color := col1;
      if trans then transpolygon(-1,-1,poly)
        else xypolygon(-1,-1,poly);
    end;
end;

function equal(p1,p2:Tpoint):boolean; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin result := (p1.x=p2.x) and (p1.y=p2.y); end;

procedure clippoly(var p:polypoint;x1,y1,x2,y2:integer); {XXXXXXXXXXXXXXXXXXXXX}
var i,t,n,m,l,n3,xa,ya,xb,yb,x0,y0 : integer;
    h0,h : Thandle;
    ok : boolean;
    pp : array of Tpoint;
    ps : array of integer;
    {0 = buiten; 1 = binnen; 2,3,4,5 = op rand; 6 = hoek; -1 = doet niet mee}
    { rand 2=on; 3=li; 4=bo; 5=re }
    reom, liom : array[2..5] of Tpoint;
function binnen : boolean; {---------------------------------------------------}
begin
  if (x0<x1) or (x0>x2) or (y0<y1) or (y0>y2) then
     begin result := false; exit; end;
  if (xa<>xb) then
    if (xa<xb) then result := (x0>=xa) and (x0<=xb)
               else result := (x0>=xb) and (x0<=xa)
  else
    if (ya<xb) then result := (y0>=ya) and (y0<=yb)
               else result := (y0>=yb) and (y0<=ya);
end;
procedure clip(n1,n2,nt:integer); {--------------------------------------------}
var xi,yi,xo,yo,x0,y0 : integer;
begin
   xi := pp[n1].x; yi := pp[n1].y; xo := pp[n2].x; yo := pp[n2].y;
   if (xo<xi) then {check rand 3 = li} begin
       y0 := yi+round((yo-yi)/(xo-xi)*(x1-xi)); if (y0>=y1) and (y0<=y2) then
          begin pp[nt].x := x1; pp[nt].y := y0; ps[nt] := 3; exit; end; end;
   if (xo>xi) then {check rand 5 = re} begin
       y0 := yi+round((yo-yi)/(xo-xi)*(x2-xi)); if (y0>=y1) and (y0<=y2) then
          begin pp[nt].x := x2; pp[nt].y := y0; ps[nt] := 5; exit; end; end;
   if (yo<yi) then {check rand 4 = bo} begin
       x0 := xi+round((xo-xi)/(yo-yi)*(y1-yi)); if (x0>=x1) and (x0<=x2) then
          begin pp[nt].x := x0; pp[nt].y := y1; ps[nt] := 4; exit; end; end;
   if (yo>yi) then {check rand 2 = on} begin
       x0 := xi+round((xo-xi)/(yo-yi)*(y2-yi)); if (x0>=x1) and (x0<=x2) then
          begin pp[nt].x := x0; pp[nt].y := y2; ps[nt] := 2; exit; end; end;
end;
procedure clip2(n1,n2:integer); {----------------------------------------------}
var t,l1,l2 : integer;
    p1,p2 : Tpoint;
procedure store(l:integer); { - - - - - - - - - - - - - - - - - - - - - - - - -}
begin
  if (l in [2,4]) then
    begin if (x0=x1) then x0 := x1+1 else if (x0=x2) then x0 := x2-1; end
  else
    begin if (y0=y1) then y0 := y1+1 else if (y0=y2) then y0 := y2-1; end;
  case t of
    0 : begin p1.x := x0; p1.y := y0; l1 := l; inc(t); end;
    1 : begin p2.x := x0; p2.y := y0; l2 := l; inc(t); end;
  end;
end;
begin
   if equal(pp[n1],pp[n2]) then exit;
   xa := pp[n1].x; ya := pp[n1].y; xb := pp[n2].x; yb := pp[n2].y;
   t := 0;
   if (xa<>xb) then
    begin
     x0 := x1; y0 := yb+round((ya-yb)/(xa-xb)*(x0-xb)); if binnen then store(3);
     x0 := x2; y0 := yb+round((ya-yb)/(xa-xb)*(x0-xb)); if binnen then store(5);
    end;
   if (ya<>yb) then
    begin
     y0 := y1; x0 := xb+round((xa-xb)/(ya-yb)*(y0-yb)); if binnen then store(4);
     y0 := y2; x0 := xb+round((xa-xb)/(ya-yb)*(y0-yb)); if binnen then store(2);
    end;
   if (t=0) then exit;
   if (t=1) or equal(p1,p2) then begin pp[n1+1] := p1; ps[n1+1] := l1; end;
   if (t=2) then
     if ( (xa<xb) and (p1.x<p2.x) )
     or ( (xa>xb) and (p1.x>p2.x) )
     or ( (ya<yb) and (p1.y<p2.y) )
     or ( (ya>yb) and (p1.y>p2.y) ) then
       begin pp[n1+1] := p1; ps[n1+1] := l1; pp[n1+2] := p2; ps[n1+2] := l2; end
     else
       begin pp[n1+1] := p2; ps[n1+1] := l2; pp[n1+2] := p1; ps[n1+2] := l1; end;
end;
begin {------------------------------------------------------------------------}
   if (x1>x2)then begin t := x1; x1 := x2; x2 := t; end;
   if (y1>y2)then begin t := y1; y1 := y2; y2 := t; end;
   if (x1=x2) or (y1=y2) then exit;
   reom[2] := point(x1,y2); reom[3] := point(x1,y1); reom[4] := point(x2,y1);
   reom[5] := point(x2,y2);
   liom[2] := reom[5]; liom[3] := reom[2]; liom[4] := reom[3]; liom[5] := reom[4];
   n := length(p^);
   n3 := n*3;
   h0 := createrectrgn(x1,y1,x2+1,y2+1); if (h0=0) then exit;
   h  := createpolygonrgn(p^[0],n,ALTERNATE);
     if (h=0) then begin deleteobject(h0); exit; end;
   {kijk of p helemaal in rect ligt}
   ok := true;
   for i := 0 to n-1 do if not ptinregion(h0,p^[i].x,p^[i].y) then ok := false;
   if ok then {er hoeft niets te worden gedaan}
     begin deleteobject(h); deleteobject(h0); exit; end;
   setlength(pp,n3); setlength(ps,n3);
   for i := 0 to n3-1 do
     if (i mod 3 = 0) then
       begin
         pp[i] := p^[i div 3];
         if ptinregion(h0,pp[i].x,pp[i].y)
           then ps[i] := 1 else ps[i] := 0;
       end
     else
       begin pp[i] := point(0,0); ps[i] := -1; end;
   for i := 0 to n3-1 do if (i mod 3 = 0) then
     begin
       n := (i+3) mod n3;
       if (ps[i]=0) and (ps[n]=1) then clip(n,i,i+2);
       if (ps[i]=1) and (ps[n]=0) then clip(i,n,i+1);
       if (ps[i]=0) and (ps[n]=0) then clip2(i,n);
     end;

   for i := 0 to n3-1 do if (ps[i] in [2..5]) then
     begin
       m := i; repeat inc(m); n := m mod n3; until (ps[n]>0);
       if (n<>i) then if (m>i+1) then if (ps[n] in [2..5]) then if (ps[n]<>ps[i]) then
         begin
           l := ps[i];
           {kijk rechtsom} if ptinregion(h,reom[l].x,reom[l].y) then
             begin
               t := i;
               repeat inc(t); t := t mod n3; pp[t] := reom[l]; ps[t] := 6;
               inc(l); if (l=6) then l := 2; until l = ps[n];
             end else
           {kijk linksom} if ptinregion(h,liom[l].x,liom[l].y) then
             begin
               t := i;
               repeat inc(t); t := t mod n3; pp[t] := liom[l]; ps[t] := 6;
               dec(l); if (l=1) then l := 5; until l = ps[n];
             end;
         end
     end;

   t := 0; for i := 0 to n3-1 do if (ps[i]>0) then inc(t);
   if (t=0) then {geen snijpunten: helemaal buiten of omheen}
   if ptinregion(h,x1,x2) then {helemaal omheen}
     begin
       setlength(p^,4); p^[0] := point(x1,y1); p^[1] := point(x1,y2);
       p^[2] := point(x2,y2); p^[3] := point(x2,y1);
     end
   else {helemaal er buiten}
     begin
       setlength(p^,1); p^[0] := point(0,0);
     end
   else {er zijn wel snijpunten}
     begin
       setlength(p^,t);
       t := 0; for i := 0 to n3-1 do if (ps[i]>0) then
         begin p^[t]:= pp[i]; inc(t); end;
     end;
  deleteobject(h); deleteobject(h0);
end;

procedure hatchbar(x1,x2,y1,y2:integer;bcol,hcol,fcol:Tcolor;style,wi1,wi2:integer);
var pw,p1,p2,dy,ofs,wt,pt,dp,xc,yc,p0,wi0,dl,dx,px,py,dxt,dyt : integer;
    p : polytype;
    h : Thandle;
    pp : polypoint;
    m3d : boolean;
const wm = 2;
procedure schuin(up:boolean); {schuine hatch ----------------------------------}
var bi,y11,y22,dp,xt1,xt2,yt1,yt2 : integer;
begin
  if up then {5 = bdiagonal}
    begin ofs := (x1+y1) mod wt + wi1 div 2; bi := -1; y11 := y2; y22 := y1; end
  else {4 = fdiagonaal}
    begin
      ofs := (x1 mod wt)-(y2 mod wt); if (ofs<0) then ofs := ofs + wt;
      ofs := ofs + wi1 div 2; bi := 1; y11 := y1; y22 := y2;
    end;
  p1 := x1-dy-ofs;
  repeat
  if (wi1<=wm) then {met lijnen}
   for dp := 0 to wi1-1 do
    begin
      xt1 := p1+dp; xt2 := p1+dy+1+dp; yt1 := y11; yt2 := y22+bi;
      if (xt2>x1) then
        begin
          if (xt1<x1)   then begin yt1 := yt1+(x1-xt1)*bi;   xt1 := x1;   end;
          if (xt2>x2+1) then begin yt2 := yt2-(xt2-x2-1)*bi; xt2 := x2+1; end;
          xyline(hcol,xt1,yt1,xt2,yt2);
        end;
    end
  else {met polygons}
    begin
      p2 := p1+wi1-1; setlength(p,4);
      p[0] := point(p1,y11);    p[1] := point(p2,y11);
      p[2] := point(p2+dy,y22); p[3] := point(p1+dy,y22);
      if (p2+dy+1>x1) then
        begin
          if (h=0) {geen region} then clippoly(pp,x1,y1,x2,y2);
          if (length(p)>2) then drawpoly(hcol,hcol,p,false);
        end;
    end;
  p1 := p1 + wt;
  until (p1>x2);
end;
begin {------------------------------------------------------------------------}
  wi1 := wi1 * res; wi2 := wi2 * res; wt := wi1+wi2;
  dx := x2-x1; dy := y2-y1; dxt := dx+1; dyt := dy+1;
  pp := @p; dl := round(sqrt(sqr(dx)+sqr(dy)));
  xc := (x1+x2) div 2; yc := (y1+y2) div 2;

  pw := xypen.width; xypen.width := 1;
  xypen.style := pssolid; xybrush.style := bssolid;
  if (bcol>=0) then xyrectangle(bcol,bcol,x1,y1,x2,y2);

  if style in [2,6] then {hatch horizontaal}
    begin
      ofs := y1 mod wt + wi1 div 2; pt := y1-ofs;
      repeat
      p1 := pt; if (p1<y1) then p1 := y1;
      p2 := pt+wi1-1; if (p2>y2) then p2 := y2;
      if (p2<p1+1) then for p0 := p1 to p2 do xyline(hcol,x1,p0,x2+1,p0)
      else xyrectangle(hcol,hcol,x1,p1,x2,p2);
      pt := pt + wt;
      until (pt>y2);
    end;
  if style in [3,6] then {hatch verticaal}
    begin
      ofs := x1 mod wt + wi1 div 2; pt := x1-ofs;
      repeat
      p1 := pt;if (p1<x1) then p1 := x1;
      p2 := pt+wi1-1; if (p2>x2) then p2 := x2;
      if (p2<p1+1) then for p0 := p1 to p2 do xyline(hcol,p0,y1,p0,y2+1)
      else xyrectangle(hcol,hcol,p1,y1,p2,y2);
      pt := pt + wt;
      until (pt>x2);
    end;

  if style in [4,5,7] then {maak schuine hatches}
    begin
     h := 0;
     if (wi1>wm) then if not dowmf then
       begin h := createrectrgn(x1,y1,x2+1,y2+1);
         if (h<>0) then selectcliprgn(xycanvas.handle,h); end;
     if style in [4,7] then schuin(false);
     if style in [5,7] then schuin(true);
     if (h<>0) then
       begin deleteobject(h); selectcliprgn(xycanvas.handle,0); end;
  end;

  if style in [8,10,12,14] then
  begin
    px := round(dx*wi1/100); py := round(dy*wi1/100);
    case style of
     8 : xyrectangle(hcol,hcol,x2-px,y1,x2,y2);
    10 : xyrectangle(hcol,hcol,x1,y1,x2,y1+py);
    12 : xyrectangle(hcol,hcol,x1,y1,x1+px,y2);
    14 : xyrectangle(hcol,hcol,x1,y2-py,x2,y2);
    end;
  end;

  if style in [9,11,13,15] then
  if (wi1=100) then xyrectangle(hcol,hcol,x1,y1,x2,y2)
  else
  if (wi1<=50) then
    begin
    px := round(dx*wi1/50); py := round(dy*wi1/50); setlength(p,3);
    case style of
     9 : begin p[0] := point(x2-px,y1); p[1] := point(x2,y1); p[2] := point(x2,y1+py); end;
    11 : begin p[0] := point(x1+px,y1); p[1] := point(x1,y1); p[2] := point(x1,y1+py); end;
    13 : begin p[0] := point(x1+px,y2); p[1] := point(x1,y2); p[2] := point(x1,y2-py); end;
    15 : begin p[0] := point(x2-px,y2); p[1] := point(x2,y2); p[2] := point(x2,y2-py); end;
    end;
    xypolygon(hcol,hcol,p);
    end
  else
    begin
    px := round(dx*(wi1-50)/50); py := round(dy*(wi1-50)/50); setlength(p,5);
    case style of
     9 : begin p[0] := point(x1,y1+py); p[1] := point(x1,y1); p[2] := point(x2,y1);
           p[3] := point(x2,y2); p[4] := point(x2-px,y2); end;
    11 : begin p[0] := point(x1+px,y2); p[1] := point(x1,y2); p[2] := point(x1,y1);
           p[3] := point(x2,y1); p[4] := point(x2,y1+py); end;
    13 : begin p[0] := point(x2,y2-py); p[1] := point(x2,y2); p[2] := point(x1,y2);
           p[3] := point(x1,y1); p[4] := point(x1+px,y1); end;
    15 : begin p[0] := point(x1,y2-py); p[1] := point(x1,y2); p[2] := point(x2,y2);
           p[3] := point(x2,y1); p[4] := point(x2-px,y1); end;
    end;
    xypolygon(hcol,hcol,p);
    end;

  if (style in [16,18]) and (wi1>0) then
    begin
      if (wi2=0) then wi0 := wi1 else
      if (wi2=1) then wi0 := round(min(dxt,dyt)*wi1/100) else
      if (wi2=2) then wi0 := round(dxt*wi1/100);
      p1 := (x1+x2+1-wi0) div 2; p2 := p1+wi0-1; if (p2<p1) then p2 := p1;
      if (p1<x1) then p1 := x1; if (p2>x2) then p2 := x2;
      xyrectangle(hcol,hcol,p1,y1,p2,y2);
    end;
  if (style in [17,18]) and (wi1>0)  then
    begin
      if (wi2=0) then wi0 := wi1 else
      if (wi2=1) then wi0 := round(min(dxt,dyt)*wi1/100) else
      if (wi2=2) then wi0 := round(dyt*wi1/100);
      p1 := (y1+y2+1-wi0) div 2; p2 := p1+wi0-1; if (p2<p1) then p2 := p1;
      if (p1<y1) then p1 := y1; if (p2>y2) then p2 := y2;
      xyrectangle(hcol,hcol,x1,p1,x2,p2);
    end;

  if style in [19,20,21] then if (dx>0) and (dy>0) and (wi1>0) then
    begin
      if (wi2=0) then
        begin px := round(wi1/2*dl/dy)-1; py := round(wi1/2*dl/dx)-1; end
      else
        begin px := round(dxt*wi1/100)-1; py := round(dyt*wi1/100)-1; end;
      if (px>=dx) or (py>=dy) then xyrectangle(hcol,hcol,x1,y1,x2,y2)
      else
        begin
          if (px<0) then px := 0; if (py<0) then py := 0; setlength(p,6);
          if (style in [19,21]) then
            begin
              p[0] := point(x1,y1); p[1] := point(x1+px,y1); p[2] := point(x2,y2-py);
              p[3] := point(x2,y2); p[4] := point(x2-px,y2); p[5] := point(x1,y1+py);
              xypolygon(hcol,hcol,p);
            end;
          if (style in [20,21]) then
            begin
              p[0] := point(x1,y2); p[1] := point(x1+px,y2); p[2] := point(x2,y1+py);
              p[3] := point(x2,y1); p[4] := point(x2-px,y1); p[5] := point(x1,y2-py);
              xypolygon(hcol,hcol,p);
            end;
        end;
    end;

  if (style=22) and (wi1>0) then
    begin
      if (wi2=0) then begin px := wi1-1; py := wi1-1; end else
      if (wi2=1) then begin px := round(min(dxt,dyt)*wi1/100)-1; py := px; end else
      if (wi2=2) then begin px := round(dxt*wi1/100)-1; py := round(dyt*wi1/100)-1; end;
      if (x1+px>=x2-px) or (y1+py>=y2-py) then xyrectangle(hcol,hcol,x1,y1,x2,y2)
      else
        begin
          xyrectangle(hcol,hcol,x1,y1,x1+px,y2);
          xyrectangle(hcol,hcol,x2,y1,x2-px,y2);
          xyrectangle(hcol,hcol,x1+px,y1,x2-px,y1+py);
          xyrectangle(hcol,hcol,x1+px,y2,x2-px,y2-py);
        end;
    end;

  xypen.width := pw; {xypen.style := pssolid;}
  if (fcol>=0) then
    begin
     m3d := mode3d; mode3d := true; xypen.color := fcol; xymove(x1,y1);
     xydraw(x1,y2); xydraw(x2,y2); xydraw(x2,y1); xydraw(x1,y1);
     mode3d := m3d;
    end;
  if dowmf then checkwmfpen;
end;

(*
Procedure CanvasTextOutAngle(C:TCanvas; X,Y:Integer; Angle:Word; S:String);
Var
  LogRec: TLOGFONT;
  OldFontHandle,
  NewFontHandle: HFONT;
Begin
  GetObject(C.Font.Handle,SizeOf(LogRec),Addr(LogRec));
  LogRec.lfEscapement:=Angle*10; {10th of a degree}
  LogRec.lfOrientation:=Angle*10;
  NewFontHandle := CreateFontIndirect(LogRec);
  OldFontHandle := SelectObject(C.Handle,NewFontHandle);
  C.Brush.Style := bsClear;
  C.TextOut(X,Y,S);
  NewFontHandle := SelectObject(C.Handle,OldFontHandle);
  DeleteObject(NewFontHandle);
End; *)

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXX EX WMF XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

const buflen = 64;

var fn : file;
    buf : array[1..buflen] of word;
    bufd : array[1..buflen div 2] of cardinal absolute buf;
    flsize, maxlen, nobj, ppx, ppy : integer;
    fnaam : string;
    wmfalign : integer;

    ok,lastmove : boolean;

procedure dosize(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  inc(flsize,n);
  if (n>maxlen) then maxlen := n;
end;

procedure dorecord; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var domove : boolean;
begin
  domove := (buf[3]=$0214);
  if (domove and lastmove) then {overschrijf vorige}
    begin
      seek(fn,filepos(fn)-bufd[1]);
      blockwrite(fn,buf,bufd[1]);
    end
  else
    begin
      blockwrite(fn,buf,bufd[1]);
      dosize(bufd[1]); {ppx := -1;}
    end;
  lastmove := domove;
end;

procedure fillintext(s:string; pos:integer; nul:boolean); {WWWWWWWWWWWWWWWWWWWW}
var i : integer;
begin
  if (s='') then s := ' ';
  if nul then s := s + #0;
  if odd(length(s)) then s := s + #32;
  for i := 1 to length(s) div 2 do
    begin
      buf[pos] := ord(s[i*2-1]) + ord(s[i*2]) shl 8;
      bufd[1] := pos;
      inc(pos);
    end;
end;

procedure fillincolor(color:Tcolor; pos:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  buf[pos] := red(color) + green(color) shl 8;
  buf[pos+1] := blue(color) + 2 shl 8;
  buf[pos+2] := 0;
end;

procedure startwmf(var s:string;var ok : boolean); {WWWWWWWWWWWWWWWWWWWWWWWWWWW}
var i : integer;
begin
  s := changefileext(s,'.wmf');
  fnaam := s;
  assignfile(fn,s); rewrite(fn,2);
  ok := (ioresult=0);  
  if ok then
   begin
     for i := 1  to 20 do buf[i] := 0;
     blockwrite(fn,buf,11+9);
     flsize := 20; maxlen := 0; nobj := 5;
    end;
  lastmove := false;
end;

procedure bitmap(name:string; xp,yp:integer; fac:single); {WWWWWWWWWWWWWWWWWWWW}
var width,height : integer;
    p1,p2,t,len : integer;
begin
  assignfile(ff,name); reset(ff,2); if (ioresult<>0) then exit;
  seek(ff,8); blockread(ff,buf,4); width := buf[2]; height := buf[4];
  seek(ff,7);

(*  bufd[1] := 4; buf[3] := $0107; buf[4] := 3; dorecord; {SetStretchBltMode} *)

  p1 := filepos(fn);
  bufd[1] := 00; buf[3] := $0F43;  {StretchDIBits}
  buf[4] := 32; buf[5] := 204; buf[6] := 0; {standaard header?}
  buf[7] := height; buf[8] := width;
  buf[9] := 0; buf[10] := 0;
  if (fac<1.5) then
    begin buf[11] := round(height*fac); buf[12] := round(width*fac); end
  else
  begin buf[11] := round(height*fac-fac/2); buf[12] := round(width*fac-fac/2); end;
  buf[13] := yp; buf[14] := xp;
  blockwrite(fn,buf,14);

  repeat blockread(ff,buf,buflen,t); blockwrite(fn,buf,t) until (t<buflen);

  p2 := filepos(fn); len := (p2-p1);
  seek(fn,p1); bufd[1] := len; blockwrite(fn,buf,2); seek(fn,p2);
  inc(flsize,len); if (len>maxlen) then maxlen := len;

  closefile(ff);
end;

procedure finishwmf(x,y,inch:integer;var ok:boolean); {WWWWWWWWWWWWWWWWWWWWWWWW}
var csum,i  : integer;
begin
  bufd[1] := 3; buf[3] := 0; dorecord;  {EOF}

  bufd[1] := $9AC6CDD7;    {Aldus header}
  buf[3] := 0;
  buf[4] := 0; buf[5] := 0;
  buf[6] := x; buf[7] := y;
  buf[8] := inch;
  buf[9] := 0; buf[10] := 0;
  csum := 0; for i := 1 to 10 do csum := csum xor buf[i];
  buf[11] := csum;
  seek(fn,0); blockwrite(fn,buf,11);

  buf[1] := 1;             {standard header}
  buf[2] := 9;
  buf[3] := $300;
  buf[4] := flsize and 65535;  buf[5] := flsize shr 16;
  buf[6] := nobj;
  buf[7] := maxlen and 65535; buf[8] := maxlen shr 16;
  buf[9] := 0;
  blockwrite(fn,buf,9);
  closefile(fn);

  ok := fileexists(fnaam);
  if ok then
    begin
      assignfile(fn,fnaam); reset(fn,1);
      ok := (filesize(fn)=flsize*2); closefile(fn);
    end;
end;

procedure setsize(x,y:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $0103; buf[4] := 8; dorecord; {Mapmode};
  bufd[1] := 5; buf[3] := $020B; buf[4] := 0; buf[5] := 0; dorecord; {Windoworg}
  bufd[1] := 5; buf[3] := $020C; buf[4] := y; buf[5] := x; dorecord; {Windowext}
end;

procedure setemptybrush; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $02FC; buf[4] := 1;
  buf[5] := 0; buf[6] := 0; buf[7] := 0;
  dorecord; {createbrushindirect}
end;

procedure setbrush(style:integer;color:Tcolor); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
 { bssolid=0 bsclear=1 bshorizontal=2 bsvertical=3
  bsfdiagonal=4 bsbdiagonal=5 bscross=6 bsdiagcross=7}
 { BS_SOLID = 0; BS_NULL = 1; BS_HATCHED = 2; BS_PATTERN  = 3; }
 { HS_HORIZONTAL = 0; HS_VERTICAL = 1; HS_FDIAGONAL  = 2;
   HS_BDIAGONAL  = 3; HS_CROSS = 4; HS_DIAGCROSS  = 5; }
var bs,hs : integer;
begin
  if (style<2) then begin bs := style; hs := 0; end
               else begin bs := 2; hs := style-2; end;
  bufd[1] := 7; buf[3] := $02FC; buf[4] := bs;
  fillincolor(color,5); buf[7] := hs; dorecord; {createbrushindirect}
end;

procedure textcolor(color:Tcolor); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 5; buf[3] := $0209;
  fillincolor(color,4); dorecord; {settextcolor}
end;

procedure setfont(he,wi,an:integer;font:Tfont); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var s : string;
begin
  bufd[1] := 0; buf[3] := $02FB;
  buf[4] := word(he); buf[5] := wi;
  buf[6] := an*10; buf[7] := an*10;
  if fsbold in font.style then buf[8] := 700 else buf[8] := 400;
  buf[9] := 0; if fsitalic in font.style then buf[9] := 1;
  if fsunderline in font.style then buf[9] := buf[9] + 256;
  buf[10] := 0; if fsstrikeout in font.style then buf[10] := 1;
  buf[10] := buf[10] + 1*256;
  buf[11] := 0 + 0*256;
  if (prmi(1)=1) then buf[12] := 5 + 0*256 {5 = anti aliasing}
                 else buf[12] := 0 + 0*256;
  s := font.name; fillintext(s,13,true);
  dorecord; {createfontindirect}

  textcolor(font.color);
end;

procedure extrachar(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $0108; buf[4] := n; dorecord; {settextcharextra}
end;

procedure textalign(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
  (*   0   6   2
      24  30  26
       8  14  10 *)
begin
  bufd[1] := 4; buf[3] := $012E; buf[4] := n; dorecord; {settextalign}
  wmfalign := n;
end;

procedure textout(x,y:integer;txt:string); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 0; buf[3] := $0521; buf[4] := length(txt);
  fillintext(txt,5,false); inc(bufd[1],2);
  buf[buf[1]-1] := y; buf[bufd[1]] := x; dorecord; {textout}

  exit;
  bufd[1] := 0; buf[3] := $0A32; buf[4] := y; buf[5] := x;
  buf[6] := length(txt); buf[7] := 0; fillintext(txt,8,false);
  dorecord; {exttextout}
end;

procedure setpen(style,wi:integer; color:Tcolor); {WWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 9; buf[3] := $02FA; buf[4] := style; buf[5] := wi; buf[6] := 0;
  fillincolor(color,7); dorecord; {createpenindirect}
end;

procedure selectobject(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $012D; buf[4] := n; dorecord; {selectobject}
end;

procedure deleteobject(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $01F0; buf[4] := n; dorecord; {deleteobject}
end;

procedure blokwmf(x1,y1,x2,y2:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $041B;
  buf[4] := y2; buf[5] := x2; buf[6] := y1; buf[7] := x1;
  dorecord; {rectangle};
end;

procedure ellipse(x1,y1,x2,y2:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $0418;
  buf[4] := y2; buf[5] := x2; buf[6] := y1; buf[7] := x1;
  dorecord; {ellipse};
end;

procedure bkmode(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $0102; buf[4] := n; dorecord; {setbkmode}
end;

procedure rop2(n:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 4; buf[3] := $0104; buf[4] := n; dorecord; {setrop2}
end;

procedure comment(txt:string); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 0; buf[3] := $0626; buf[4] := 15;
  buf[5] := length(txt); fillintext(txt,6,false);
  dorecord; {escape};
end;

procedure movewmf(x,y:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  if (x>32760) then x := 32760 else if (x<0) then x := 0;
  if (y>32760) then y := 32760 else if (y<0) then y := 0;
  if (ppx=x) and (ppy=y) then exit;
  bufd[1] := 5; buf[3] := $0214; buf[4] := y; buf[5] := x;
  dorecord; {moveto};
  ppx := x; ppy := y;
end;

procedure drawwmf(x,y:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  if (x>32760) then x := 32760 else if (x<0) then x := 0;
  if (y>32760) then y := 32760 else if (y<0) then y := 0;
  if (ppx=x) and (ppy=y) then exit;
  bufd[1] := 5; buf[3] := $0213; buf[4] := y; buf[5] := x;
  dorecord; {lineto};
  ppx := x; ppy := y;
end;

procedure pixel(x,y:integer;color:Tcolor); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  bufd[1] := 7; buf[3] := $041F; fillincolor(color,4);
  buf[6] := y; buf[7] := x; dorecord; {setpixel};
end;

procedure polygon(points: array of Tpoint); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var i,n : integer;
begin
  n := length(points);
  bufd[1] := n*2+4; buf[3] := $0324; buf[4] := n;
  for i := 1 to n do
    begin
      buf[i*2+3] := points[i-1].x;
      buf[i*2+4] := points[i-1].y;
    end;
  dorecord; {polygon};
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXX EX XYWMF XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

var penwidth : integer;
    penstyle : integer;
    pencolor,redcolor,grncolor : Tcolor;
    fontcolor : Tcolor;
    fontname : string;
    fontsize : integer;
    fontstyle: Tfontstyles;
    fontangle : integer;
    brushcolor : Tcolor;
    brushstyle : integer;

    cgrid,fgrid : array of word;
    ncgrid,nfgrid : integer;

procedure initgrid; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  setlength(cgrid,0); setlength(fgrid,0);
  ncgrid := 0; nfgrid := 0;
end;

procedure addcoarse(p:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  inc(ncgrid); setlength(cgrid,ncgrid);
  cgrid[ncgrid-1] := p;
end;

procedure addfine(p:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  inc(nfgrid); setlength(fgrid,nfgrid);
  fgrid[nfgrid-1] := p;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure checkwmfpen; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  if stereo then exit;
  with xycanvas.pen do
    if (color<>pencolor) or (integer(style)<>penstyle) or (width<>penwidth) then
      begin
        deleteobject(1);
        setpen(integer(style),width,color);
        selectobject(1);
        pencolor := color;
        penstyle := integer(style);
        penwidth := width;
      end;
end;

procedure checkwmfrgpen; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  with xycanvas.pen do
    if (redcolor<>stereocol[1]) or (grncolor<>stereocol[2])
     or (integer(style)<>penstyle) or (width<>penwidth) then
      begin
        deleteobject(4);
        setpen(integer(style),width,stereocol[2]);
        selectobject(4);
        deleteobject(3);
        setpen(integer(style),width,stereocol[1]);
        selectobject(3);
        redcolor := stereocol[1]; grncolor := stereocol[2];
        penstyle := integer(style);
        penwidth := width;
      end
   else selectobject(3);
end;

procedure wmfredpen; begin selectobject(3); end;
procedure wmfgrnpen; begin selectobject(4); end;

procedure fontwmf; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var h,w,e,a : integer;
    tm : textmetric;
    tt : boolean; {= true type font}
begin
  h := xycanvas.font.height; e := 0;
  if not gettextmetrics(xycanvas.handle,tm) then {veronderstel TT font}
    begin w := round(xycanvas.textwidth('0')*0.85); e := 0; end
  else
    begin
      tt := (tm.tmPitchAndFamily and 4 > 0); {TT font}
      if tt then
        begin
          h := tm.tmheight; w := tm.tmAveCharWidth;
          a := xyfontangle mod 180; if (a>90) then a := 180-a;
          e := round(h*a/14/90); end
      else
        begin w := 0; e := 0; end;
    end;
  setfont(h,w,xyfontangle,xycanvas.font);
  extrachar(e);
  {e := size div 3; if fsbold in style then inc(e);}
end;

procedure checkwmffont; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var ta : integer;
begin
  with xycanvas.font do
    if (style<>fontstyle) or (size<>fontsize) or (name<>fontname)
      or (xyfontangle<>fontangle) then
      begin
        deleteobject(2);
        fontwmf;
        selectobject(2);
        fontcolor := color;
        fontstyle := style;
        fontsize := size;
        fontname := name;
        fontangle := xyfontangle;
      end
    else if (color<>fontcolor) then
      begin
        textcolor(color);
        fontcolor := color;
      end;
  ta := gettextalign(xycanvas.handle);
  if (wmfalign<>ta) then textalign(ta);
end;

procedure checkwmfbrush; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  with xycanvas.brush do
    if (color<>brushcolor) or (integer(style)<>brushstyle) then
      begin
        deleteobject(0);
        setbrush(integer(style),color);
        selectobject(0);
        brushcolor := color;
        brushstyle := integer(style);
      end;
end;

{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure openwmf(var s:string;var ok:boolean); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
begin
  if (s='') then s := 'xygraf.wmf';
  startwmf(s,ok);
  dowmf := ok; ppx := -1;
end;

procedure initwmf; {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var x,y : integer;
begin
  x := cwidth; y := cheight;
  redcolor := $0000ff; grncolor := $00ff00;
  comment('File created by XYGRAPH');
  comment('(C) Wilko C Emmens 2002');
  setsize(x,y);
  setbrush(0,backcolor);
    brushcolor := backcolor; brushstyle := 0;
  selectobject(0);
  setpen(0,res,frontcolor);
    pencolor := frontcolor; penstyle := 0; penwidth := res;
  selectobject(1);
  if wmfbk then blokwmf(res div 2,res div 2,x,y);
  fontwmf;
    fontname := xycanvas.font.name;
    fontsize := xycanvas.font.size;
    fontcolor := xycanvas.font.color;
    fontstyle := xycanvas.font.style;
    wmfalign := 0;
  selectobject(2);
  setpen(0,1,redcolor); selectobject(3);
  setpen(0,1,grncolor); selectobject(4);
  selectobject(1);
  bkmode(1);
end;

procedure pixelwmf(x,y:integer; col:Tcolor); begin pixel(x,y,col); end;
procedure linewmf(x1,y1,x2,y2:integer); begin movewmf(x1,y1); drawwmf(x2,y2); end;

procedure textwmfalign(n:integer); begin textalign(n); end;
procedure textwmfout(x,y:integer;s:string); begin textout(x,y,s); end;

procedure ellipsewmf(x1,y1,x2,y2:integer); begin ellipse(x1,y1,x2,y2); end;

procedure rop2wmf(n:integer); begin rop2(n); end;

procedure polygonwmf(points: array of Tpoint);
begin polygon(points); end;

procedure closewmf(var ok:boolean); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var x,y : integer;
begin
  x := cwidth; y := cheight;
  finishwmf(x,y,screen.pixelsperinch,ok);
  dowmf := false;
end;

procedure dogrid(p1,p2:integer;xas:boolean;col:Tcolor); {WWWWWWWWWWWWWWWWWWWWWW}
var i : integer;
    oldcol : Tcolor;
begin
  xycanvas.pen.style := psdot; oldcol := xycanvas.pen.color;
  if (nfgrid>0) then
    begin
      xycanvas.pen.color := mixcolor(col,backcolor,1,3);
      checkwmfpen;
      for i := 0 to nfgrid-1 do
        if xas then linewmf(fgrid[i],p1,fgrid[i],p2)
               else linewmf(p1,fgrid[i],p2,fgrid[i]);
    end;
  if (ncgrid>0) then
    begin
      xycanvas.pen.color := col;
      checkwmfpen;
      for i := 0 to ncgrid-1 do
        if xas then linewmf(cgrid[i],p1,cgrid[i],p2)
               else linewmf(p1,cgrid[i],p2,cgrid[i]);
    end;
  xycanvas.pen.style := pssolid;
  xycanvas.pen.color := oldcol;
end;

procedure bitmapwmf(x1,y1,x2,y2:integer); {WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
var bmp : Tbitmap;
    crect1,crect2 : Trect;
    ok : boolean;
    s : string;
begin
  s := 'tempfile.bmp';
  screen.cursor := crhourglass;
  bmp := Tbitmap.create; ok := true;
  with bmp do
    begin
      pixelformat := screencopy.pixelformat;
      width := x2-x1+1; height := y2-y1+1;
      crect1 := rect(x1,y1,x2+1,y2+1);
      crect2 := rect(0,0,x2-x1+1,y2-y1+1);
      canvas.copymode := cmSrcCopy;
      canvas.copyrect(crect2,screencopy.canvas,crect1);
      try    savetofile(s);
      except ok := false;
      end;
      free;
    end;
  if ok then
    begin
      bitmap(s,x1*res,y1*res,res);
      deletefile(s);
    end;
  screen.cursor := crdefault;
end;

procedure initxycommon; {XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}
begin
  xyfontangle := 0;
  initsettings;
end;

end.
