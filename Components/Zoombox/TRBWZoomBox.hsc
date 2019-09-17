HelpScribble project file.
10
Yrbaneq S. Xbavxbj-050Q35
48
1
TRbwZoomBox



TRUE

D:\PROGRA~1\HELPSC~1,D:\PROGRA~1\Borland\Delphi5\COMPON~1\ZoomBox
1
BrowseButtons()
0
FALSE

FALSE
186
1000
Scribble1000
RBWZoomBox unit
RBWZoomBox unit;RBWZoomBox


rbwzoombox:001000
Writing



FALSE
48
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b RbwZoomBox unit
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs22\b Components
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0 
\par 
\par \plain\f3\fs22\b Classes
\par \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20\cf0 
\par 
\par \plain\f3\fs22\b Exceptions
\par \plain\f3\fs20\cf1\strike EInvalidZoomLevel\plain\f3\fs20\cf3 \{linkID=1010\}\plain\f3\fs20\cf0 
\par 
\par \plain\f3\fs22\b Types
\par \plain\f3\fs20\cf1\strike TZBArray\plain\f3\fs20\cf3 \{linkID=1800\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20 
\par     TRbwZoomBox provides methods for converting real-number coordinates to screen coordinates so that they may be easily displayed.
\par 
\par     By default, the positive directions for the real-number coordinate system are to the right (X-axis) and upward (Y-axis).  However, either coordinate system may be reversed.  By default, the vertical exaggeration is 1 but the vertical exaggeration may be set to any positive real number.  However, if the vertical exaggeration is set to too high or too low a value, EInvalidOp may be raised when calculating the screen coordinates.
\par 
\par     TRbwZoomBox has two embedded components, a TPaintBox and a TShape;  The TPaintBox provides the drawing surface. The TShape is only visible during zooming operations.  Methods or properties that merely surface methods or properties of the TPaintBox and TShape have the same name as the methods in the TPaintBox and TShape with a prefix of "PB" or "S".
\par 
\par     Zooming methods:
\par       \plain\f3\fs20\cf1\strike AbortZoom\plain\f3\fs20\cf3 \{linkID=1435\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike BeginZoom\plain\f3\fs20\cf3 \{linkID=1445\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike ContinueZoom\plain\f3\fs20\cf3 \{linkID=1450\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike FinishZoom\plain\f3\fs20\cf3 \{linkID=1460\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike ZoomBy\plain\f3\fs20\cf3 \{linkID=1555\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike ZoomByAt\plain\f3\fs20\cf3 \{linkID=1560\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike ZoomOut\plain\f3\fs20\cf3 \{linkID=1565\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike SetZoom\plain\f3\fs20\cf3 \{linkID=1410\}\plain\f3\fs20 
\par 
\par     Panning methods:
\par       \plain\f3\fs20\cf1\strike BeginPan\plain\f3\fs20\cf3 \{linkID=1440\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike EndPan\plain\f3\fs20\cf3 \{linkID=1456\}\plain\f3\fs20 
\par 
\par     Coordinate Conversion methods
\par       \plain\f3\fs20\cf1\strike MouseToCoordinates\plain\f3\fs20\cf3 \{linkID=1475\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike X\plain\f3\fs20\cf3 \{linkID=1415\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike XCoord\plain\f3\fs20\cf3 \{linkID=1420\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike Y\plain\f3\fs20\cf3 \{linkID=1425\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf1\strike YCoord\plain\f3\fs20\cf3 \{linkID=1430\}\plain\f3\fs20 
\par 
\par     \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20  is a helper class that store the real-number coordinates.
\par     Many of the zooming operations require that \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20  be used to store the coordinates.
\par \plain\f3\fs20\cf0 
\par \plain\f3\fs20 
\par }
1010
Scribble1010
EInvalidZoomLevel exception
EInvalidZoomLevel
class_EInvalidZoomLevel

rbwzoombox:001010
Writing


EInvalidZoomLevel_Object;EInvalidZoomLevel
FALSE
12
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b EInvalidZoomLevel exception
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Unit
\par \plain\f3\fs20\cf2\strike RbwZoomBox\plain\f3\fs20\cf1 \{linkID=1000\}\plain\f3\fs20\cf0 
\par 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 EInvalidZoomLevel is raised if there is an attempt to set a zoom level <= 0 in TRbwZoomBox.\plain\f3\fs20\cf2\strike SetZoom\plain\f3\fs20\cf1 \{linkID=1410\}\plain\f3\fs20 
\par 
\par }
1100
Scribble1100
TRBWZoomBox component
TRBWZoomBox
class_TRBWZoomBox

rbwzoombox:001100
Writing


TRBWZoomBox_Object;TRBWZoomBox
FALSE
43
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs20\cf1 \{bml RbwZoomBox.bmp\}\tab \plain\f3\fs32\cf2\b TRbwZoomBox component\plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf3\ul See also\plain\f3\fs16\cf1 \{linkID=%1101\}\tab \plain\f3\fs16\cf3\strike Properties\plain\f3\fs16\cf1 \{linkID=1102>sidebar\}\tab \plain\f3\fs16\cf3\strike Methods\plain\f3\fs16\cf1 \{linkID=1103>sidebar\}\tab \plain\f3\fs16\cf3\strike Events\plain\f3\fs16\cf1 \{linkID=1104>sidebar\}\tab \plain\f3\fs16\cf3\strike Tasks\plain\f3\fs16\cf1 \{linkID=%1105\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Unit
\par \plain\f3\fs20\cf3\strike RbwZoomBox\plain\f3\fs20\cf1 \{linkID=1000\}\plain\f3\fs20\cf0 
\par 
\par \plain\f3\fs22\b Declaration\plain\f3\fs20\cf0 
\par \pard\plain\f4\fs20 TRbwZoomBox = \plain\f4\fs20\b class\plain\f4\fs20 (TScrollBox)\plain\f3\fs22\b 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs22\b 
\par Description
\par \plain\f3\fs20     TRbwZoomBox provides methods for converting real-number coordinates to screen coordinates so that they may be easily displayed.
\par 
\par     By default, the positive directions for the real-number coordinate system are to the right (X-axis) and upward (Y-axis).  However, either coordinate system may be reversed.  By default, the vertical exaggeration is 1 but the vertical exaggeration may be set to any positive real number.  However, if the vertical exaggeration is set to too high or too low a value, EInvalidOp may be raised when calculating the screen coordinates.
\par 
\par     TRbwZoomBox has two embedded components, a TPaintBox and a TShape;  The TPaintBox provides the drawing surface. The TShape is only visible during zooming operations.  Methods or properties that merely surface methods or properties of the TPaintBox and TShape have the same name as the methods in the TPaintBox and TShape with a prefix of "PB" or "S".
\par 
\par     Zooming methods:
\par       \plain\f3\fs20\cf3\strike AbortZoom\plain\f3\fs20\cf1 \{linkID=1435\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike BeginZoom\plain\f3\fs20\cf1 \{linkID=1445\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike ContinueZoom\plain\f3\fs20\cf1 \{linkID=1450\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike FinishZoom\plain\f3\fs20\cf1 \{linkID=1460\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike ZoomBy\plain\f3\fs20\cf1 \{linkID=1555\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike ZoomByAt\plain\f3\fs20\cf1 \{linkID=1560\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike ZoomOut\plain\f3\fs20\cf1 \{linkID=1565\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike SetZoom\plain\f3\fs20\cf1 \{linkID=1410\}\plain\f3\fs20 
\par 
\par     Panning methods:
\par       \plain\f3\fs20\cf3\strike BeginPan\plain\f3\fs20\cf1 \{linkID=1440\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike EndPan\plain\f3\fs20\cf1 \{linkID=1456\}\plain\f3\fs20 
\par 
\par     Coordinate Conversion methods
\par       \plain\f3\fs20\cf3\strike MouseToCoordinates\plain\f3\fs20\cf1 \{linkID=1475\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike X\plain\f3\fs20\cf1 \{linkID=1415\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike XCoord\plain\f3\fs20\cf1 \{linkID=1420\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike Y\plain\f3\fs20\cf1 \{linkID=1425\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf3\strike YCoord\plain\f3\fs20\cf1 \{linkID=1430\}\plain\f3\fs20 
\par 
\par     \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20  is a helper class that store the real-number coordinates.
\par     Many of the zooming operations require that \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20  be used to store the coordinates.
\par 
\par 
\par }
1101
Scribble1101
TRBWZoomBox component - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike TRbwZoomPoint class\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike TZBArray type\plain\f3\fs20\cf2 \{linkID=1800\}\plain\f3\fs20 
\par 
\par }
1102
Scribble1102
TRBWZoomBox - Properties




Writing
sidebar


FALSE
55
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b Properties
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100>main\}\plain\f3\fs20\cf2\b \{keepn\}\plain\f3\fs20\cf1\b 
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\plain\f3\fs20\cf0  Run-time only\tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\plain\f3\fs20\cf0  Key properties
\par \pard\tx200\tx640\plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike BottomMargin\plain\f3\fs20\cf2 \{linkID=1110>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike DefaultMultiplier\plain\f3\fs20\cf2 \{linkID=1115>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike LeftMargin\plain\f3\fs20\cf2 \{linkID=1120>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike MaxX\plain\f3\fs20\cf2 \{linkID=1125>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike MaxY\plain\f3\fs20\cf2 \{linkID=1130>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike MinX\plain\f3\fs20\cf2 \{linkID=1135>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike MinY\plain\f3\fs20\cf2 \{linkID=1140>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike Multiplier\plain\f3\fs20\cf2 \{linkID=1145>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike PBCanvas\plain\f3\fs20\cf2 \{linkID=1150>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBClientHeight\plain\f3\fs20\cf2 \{linkID=1155>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBClientOrigin\plain\f3\fs20\cf2 \{linkID=1160>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBClientRect\plain\f3\fs20\cf2 \{linkID=1165>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBClientWidth\plain\f3\fs20\cf2 \{linkID=1170>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBColor\plain\f3\fs20\cf2 \{linkID=1175>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBComObject\plain\f3\fs20\cf2 \{linkID=1180>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBComponentCount\plain\f3\fs20\cf2 \{linkID=1185>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBComponentIndex\plain\f3\fs20\cf2 \{linkID=1190>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBComponents\plain\f3\fs20\cf2 \{linkID=1195>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBControlState\plain\f3\fs20\cf2 \{linkID=1200>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBControlStyle\plain\f3\fs20\cf2 \{linkID=1205>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBCursor\plain\f3\fs20\cf2 \{linkID=1210>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBDragCursor\plain\f3\fs20\cf2 \{linkID=1215>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBDragKind\plain\f3\fs20\cf2 \{linkID=1220>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBDragMode\plain\f3\fs20\cf2 \{linkID=1225>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBEnabled\plain\f3\fs20\cf2 \{linkID=1230>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBFont\plain\f3\fs20\cf2 \{linkID=1235>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBHeight\plain\f3\fs20\cf2 \{linkID=1240>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBHint\plain\f3\fs20\cf2 \{linkID=1245>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBLeft\plain\f3\fs20\cf2 \{linkID=1250>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBPopupMenu\plain\f3\fs20\cf2 \{linkID=1255>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBShowHint\plain\f3\fs20\cf2 \{linkID=1260>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBTag\plain\f3\fs20\cf2 \{linkID=1265>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBTop\plain\f3\fs20\cf2 \{linkID=1270>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBVisible\plain\f3\fs20\cf2 \{linkID=1275>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBWidth\plain\f3\fs20\cf2 \{linkID=1280>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike PBWindowProc\plain\f3\fs20\cf2 \{linkID=1285>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike RightMargin\plain\f3\fs20\cf2 \{linkID=1290>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike SBoundsRect\plain\f3\fs20\cf2 \{linkID=1295>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike SBrush\plain\f3\fs20\cf2 \{linkID=1300>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike SCursor\plain\f3\fs20\cf2 \{linkID=1305>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike SDragCursor\plain\f3\fs20\cf2 \{linkID=1310>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \tab \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SelectionWidth\plain\f3\fs20\cf2 \{linkID=1315>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike SPen\plain\f3\fs20\cf2 \{linkID=1320>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike TopMargin\plain\f3\fs20\cf2 \{linkID=1325>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike VerticalExaggeration\plain\f3\fs20\cf2 \{linkID=1330>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike XPositive\plain\f3\fs20\cf2 \{linkID=1335>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike YPositive\plain\f3\fs20\cf2 \{linkID=1340>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20 
\par }
1103
Scribble1103
TRBWZoomBox - Methods




Writing
sidebar


FALSE
53
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx200\tx640\plain\f3\fs20\cf1\b Methods
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100>main\}\plain\f3\fs20\cf2\b \{keepn\}
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2 \{bmct key.bmp\}\plain\f3\fs20\cf0  Key methods
\par \tab \tab \plain\f3\fs20\cf3\strike Create\plain\f3\fs20\cf2 \{linkID=1345>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike Destroy\plain\f3\fs20\cf2 \{linkID=1350>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike IsPointInside\plain\f3\fs20\cf2 \{linkID=1355>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBClientToScreen\plain\f3\fs20\cf2 \{linkID=1360>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBDragging\plain\f3\fs20\cf2 \{linkID=1365>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBGetTextBuf\plain\f3\fs20\cf2 \{linkID=1370>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBGetTextLen\plain\f3\fs20\cf2 \{linkID=1375>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBPerform\plain\f3\fs20\cf2 \{linkID=1380>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBScreenToClient\plain\f3\fs20\cf2 \{linkID=1385>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SelectPoint\plain\f3\fs20\cf2 \{linkID=1390>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SelectPolygon\plain\f3\fs20\cf2 \{linkID=1395>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SelectPolyLine\plain\f3\fs20\cf2 \{linkID=1400>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SelectSegment\plain\f3\fs20\cf2 \{linkID=1405>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SetZoom\plain\f3\fs20\cf2 \{linkID=1410>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike X\plain\f3\fs20\cf2 \{linkID=1415>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike XCoord\plain\f3\fs20\cf2 \{linkID=1420>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike Y\plain\f3\fs20\cf2 \{linkID=1425>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike YCoord\plain\f3\fs20\cf2 \{linkID=1430>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike AbortZoom\plain\f3\fs20\cf2 \{linkID=1435>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike BeginPan\plain\f3\fs20\cf2 \{linkID=1440>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike BeginZoom\plain\f3\fs20\cf2 \{linkID=1445>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike ContinueZoom\plain\f3\fs20\cf2 \{linkID=1450>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike EndPan\plain\f3\fs20\cf2 \{linkID=1455>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike FinishZoom\plain\f3\fs20\cf2 \{linkID=1460>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike GetMinMax\plain\f3\fs20\cf2 \{linkID=1465>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike Invalidate\plain\f3\fs20\cf2 \{linkID=1470>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike MouseToCoordinates\plain\f3\fs20\cf2 \{linkID=1475>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBBeginDrag\plain\f3\fs20\cf2 \{linkID=1480>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBBringToFront\plain\f3\fs20\cf2 \{linkID=1485>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBDragDrop\plain\f3\fs20\cf2 \{linkID=1490>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBEndDrag\plain\f3\fs20\cf2 \{linkID=1495>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBHide\plain\f3\fs20\cf2 \{linkID=1500>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PbRefresh\plain\f3\fs20\cf2 \{linkID=1505>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBRepaint\plain\f3\fs20\cf2 \{linkID=1510>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBSendToBack\plain\f3\fs20\cf2 \{linkID=1515>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBSetBounds\plain\f3\fs20\cf2 \{linkID=1520>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBSetTextBuf\plain\f3\fs20\cf2 \{linkID=1525>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBShow\plain\f3\fs20\cf2 \{linkID=1530>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf3\strike PBUpdate\plain\f3\fs20\cf2 \{linkID=1535>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SetRange\plain\f3\fs20\cf2 \{linkID=1540>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SetXRange\plain\f3\fs20\cf2 \{linkID=1545>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike SetYRange\plain\f3\fs20\cf2 \{linkID=1550>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike ZoomBy\plain\f3\fs20\cf2 \{linkID=1555>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike ZoomByAt\plain\f3\fs20\cf2 \{linkID=1560>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike ZoomOut\plain\f3\fs20\cf2 \{linkID=1565>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20 
\par }
1104
Scribble1104
TRBWZoomBox - Events




Writing
sidebar


FALSE
21
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\tx200\tx640\plain\f3\fs20\cf1\b Events
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100>main\}\plain\f3\fs20\cf3\b \{keepn\}
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3 \{bmct key.bmp\}\plain\f3\fs20\cf0  Key events
\par \tab \tab \plain\f3\fs20\cf2\strike OnClick\plain\f3\fs20\cf3 \{linkID=1570>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf2\strike OnDblClick\plain\f3\fs20\cf3 \{linkID=1575>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf2\strike OnDragDrop\plain\f3\fs20\cf3 \{linkID=1580>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf2\strike OnDragOver\plain\f3\fs20\cf3 \{linkID=1585>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf2\strike OnEndDrag\plain\f3\fs20\cf3 \{linkID=1590>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf2\strike OnKeyDown\plain\f3\fs20\cf3 \{linkID=1600>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf2\strike OnKeyPress\plain\f3\fs20\cf3 \{linkID=1605>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf2\strike OnKeyUp\plain\f3\fs20\cf3 \{linkID=1610>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf3 \{bmct key.bmp\}\tab \plain\f3\fs20\cf2\strike OnMouseDown\plain\f3\fs20\cf3 \{linkID=1615>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf3 \{bmct key.bmp\}\tab \plain\f3\fs20\cf2\strike OnMouseMove\plain\f3\fs20\cf3 \{linkID=1620>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf3 \{bmct key.bmp\}\tab \plain\f3\fs20\cf2\strike OnMouseUp\plain\f3\fs20\cf3 \{linkID=1625>main\}\plain\f3\fs20\cf0 
\par \tab \plain\f3\fs20\cf3 \{bmct key.bmp\}\tab \plain\f3\fs20\cf2\strike OnPaint\plain\f3\fs20\cf3 \{linkID=1630>main\}\plain\f3\fs20\cf0 
\par \tab \tab \plain\f3\fs20\cf2\strike OnStartDrag\plain\f3\fs20\cf3 \{linkID=1635>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20 
\par }
1105
Scribble1105
About the TRBWZoomBox component
TRBWZoomBox component


rbwzoombox:001105
Writing



FALSE
43
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs20\cf1 \{bml RbwZoomBox.bmp\}\tab \plain\f3\fs32\cf3\b About the TRbwZoomBox component\plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf1 \{linkID=%1106\}\tab \plain\f3\fs16\cf2\strike TRbwZoomBox reference\plain\f3\fs16\cf1 \{linkID=%1100\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Purpose\plain\f3\fs20 
\par     TRbwZoomBox provides methods for converting real-number coordinates to screen coordinates so that they may be easily displayed.
\par 
\par     By default, the positive directions for the real-number coordinate system are to the right (X-axis) and upward (Y-axis).  However, either coordinate system may be reversed.  By default, the vertical exaggeration is 1 but the vertical exaggeration may be set to any positive real number.  However, if the vertical exaggeration is set to too high or too low a value, EInvalidOp may be raised when calculating the screen coordinates.
\par 
\par     TRbwZoomBox has two embedded components, a TPaintBox and a TShape;  The TPaintBox provides the drawing surface. The TShape is only visible during zooming operations.  Methods or properties that merely surface methods or properties of the TPaintBox and TShape have the same name as the methods in the TPaintBox and TShape with a prefix of "PB" or "S".
\par 
\par     Zooming methods:
\par       \plain\f3\fs20\cf2\strike AbortZoom\plain\f3\fs20\cf1 \{linkID=1435\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike BeginZoom\plain\f3\fs20\cf1 \{linkID=1445\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike ContinueZoom\plain\f3\fs20\cf1 \{linkID=1450\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike FinishZoom\plain\f3\fs20\cf1 \{linkID=1460\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike ZoomBy\plain\f3\fs20\cf1 \{linkID=1555\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike ZoomByAt\plain\f3\fs20\cf1 \{linkID=1560\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike ZoomOut\plain\f3\fs20\cf1 \{linkID=1565\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike SetZoom\plain\f3\fs20\cf1 \{linkID=1410\}\plain\f3\fs20 
\par 
\par     Panning methods:
\par       \plain\f3\fs20\cf2\strike BeginPan\plain\f3\fs20\cf1 \{linkID=1440\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike EndPan\plain\f3\fs20\cf1 \{linkID=1456\}\plain\f3\fs20 
\par 
\par     Coordinate Conversion methods
\par       \plain\f3\fs20\cf2\strike MouseToCoordinates\plain\f3\fs20\cf1 \{linkID=1475\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike X\plain\f3\fs20\cf1 \{linkID=1415\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike XCoord\plain\f3\fs20\cf1 \{linkID=1420\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike Y\plain\f3\fs20\cf1 \{linkID=1425\}\plain\f3\fs20 
\par       \plain\f3\fs20\cf2\strike YCoord\plain\f3\fs20\cf1 \{linkID=1430\}\plain\f3\fs20 
\par 
\par 
\par     \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20  is a helper class that store the real-number coordinates.
\par     Many of the zooming operations require that \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20  be used to store the coordinates.
\par     
\par 
\par \plain\f3\fs22\b Tasks
\par \pard\plain\f3\fs20     At design time, set the \plain\f3\fs20\cf2\strike LeftMargin\plain\f3\fs20\cf1 \{linkID=1120\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike RightMargin\plain\f3\fs20\cf1 \{linkID=1290\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike TopMargin\plain\f3\fs20\cf1 \{linkID=1325\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike BottomMargin\plain\f3\fs20\cf1 \{linkID=1110\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike VerticalExaggeration\plain\f3\fs20\cf1 \{linkID=1330\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike XPositive\plain\f3\fs20\cf1 \{linkID=1335\}\plain\f3\fs20 , and \plain\f3\fs20\cf2\strike YPositive\plain\f3\fs20\cf1 \{linkID=1340\}\plain\f3\fs20  properties to their desired values.
\par   At runtime, create a series of \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20 's and assign them have real-number \plain\f3\fs20\cf2\strike X\plain\f3\fs20\cf1 \{linkID=1715\}\plain\f3\fs20  and \plain\f3\fs20\cf2\strike Y\plain\f3\fs20\cf1 \{linkID=1725\}\plain\f3\fs20  coordinates.  In the event handler for the \plain\f3\fs20\cf2\strike OnPaint event\plain\f3\fs20\cf1 \{linkID=1630\}\plain\f3\fs20 , use the screen coordinates of those \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20 's (\plain\f3\fs20\cf2\strike XCoord\plain\f3\fs20\cf1 \{linkID=1720\}\plain\f3\fs20  and \plain\f3\fs20\cf2\strike YCoord\plain\f3\fs20\cf1 \{linkID=1730\}\plain\f3\fs20 ) to draw the object on the \plain\f3\fs20\cf2\strike PBCanvas\plain\f3\fs20\cf1 \{linkID=1150\}\plain\f3\fs20 .
\par   The \plain\f3\fs20\cf2\strike SelectPoint\plain\f3\fs20\cf1 \{linkID=1390\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike SelectSegment\plain\f3\fs20\cf1 \{linkID=1405\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike SelectPolyLine\plain\f3\fs20\cf1 \{linkID=1400\}\plain\f3\fs20 , and \plain\f3\fs20\cf2\strike SelectPolygon\plain\f3\fs20\cf1 \{linkID=1395\}\plain\f3\fs20  methods may be helpful for selecting objects containing \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20 's using the mouse coordinates.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1106
Scribble1106
About the TRBWZoomBox component - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2\strike TZBArray\plain\f3\fs20\cf1 \{linkID=1800\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20 
\par }
1110
Scribble1110
TRBWZoomBox.BottomMargin property
BottomMargin,TRBWZoomBox;TRBWZoomBox,BottomMargin
prop_TRBWZoomBoxBottomMargin

rbwzoombox:001110
Writing


TRBWZoomBox_BottomMargin;BottomMargin_Property;BottomMargin
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.BottomMargin property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1111\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  BottomMargin: integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 BottomMargin is the width of the space in pixels between the bottom edge of the embedded TPaintBox client area and the \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f3\fs20  whose screen Y-coordinate was the highest (furthest down on the screen) at the time of the last call to \plain\f3\fs20\cf1\strike GetMinMax\plain\f3\fs20\cf2 \{linkID=1465\}\plain\f3\fs20 .
\par 
\par }
1111
Scribble1111
BottomMargin property - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike LeftMargin property\plain\f3\fs20\cf3 \{linkID=1120\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike RightMargin property\plain\f3\fs20\cf3 \{linkID=1290\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike TopMargin property\plain\f3\fs20\cf3 \{linkID=1325\}\plain\f3\fs20 
\par }
1115
Scribble1115
TRBWZoomBox.DefaultMultiplier property
DefaultMultiplier,TRBWZoomBox;TRBWZoomBox,DefaultMultiplier
prop_TRBWZoomBoxDefaultMultiplier

rbwzoombox:001115
Writing


TRBWZoomBox_DefaultMultiplier;DefaultMultiplier_Property;DefaultMultiplier
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.DefaultMultiplier property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf1 \{linkID=%1116\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  DefaultMultiplier: Extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 DefaultMultiplier is the zoom level that would display all the \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20 s with a blank area surrounding them \plain\f3\fs20\cf2\strike Margin\plain\f3\fs20\cf1 \{linkID=1120\}\plain\f3\fs20  pixels wide that were in existence when the last \plain\f3\fs20\cf2\strike GetMinMax\plain\f3\fs20\cf1 \{linkID=1465\}\plain\f3\fs20  occurred.
\par Run-time only
\par Read-only
\par 
\par }
1116
Scribble1116
DefaultMultiplier property - See also




Writing



FALSE
6
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike Multiplier property\plain\f3\fs20\cf1 \{linkID=1145\}\plain\f3\fs20 
\par }
1120
Scribble1120
TRBWZoomBox.LeftMargin property
LeftMargin,TRBWZoomBox;TRBWZoomBox,LeftMargin
prop_TRBWZoomBoxLeftMargin

rbwzoombox:001120
Writing


TRBWZoomBox_LeftMargin;LeftMargin_Property;LeftMargin
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.LeftMargin property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf3 \{linkID=%1121\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  LeftMargin: integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 LeftMargin is the width of the space in pixels between the left edge of the embedded TPaintBox client area and the \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20  whose screen X-coordinate was the lowest (furthest to the left on the screen) at the time of the last call to \plain\f3\fs20\cf2\strike GetMinMax\plain\f3\fs20\cf3 \{linkID=1465\}\plain\f3\fs20 .
\par 
\par }
1121
Scribble1121
LeftMargin property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike BottomMargin property\plain\f3\fs20\cf1 \{linkID=1110\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike RightMargin property\plain\f3\fs20\cf1 \{linkID=1290\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike TopMargin property\plain\f3\fs20\cf1 \{linkID=1325\}\plain\f3\fs20 
\par 
\par }
1125
Scribble1125
TRBWZoomBox.MaxX property
MaxX,TRBWZoomBox;TRBWZoomBox,MaxX
prop_TRBWZoomBoxMaxX

rbwzoombox:001125
Writing


TRBWZoomBox_MaxX;MaxX_Property;MaxX
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.MaxX property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf3 \{linkID=%1126\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  MaxX: extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 MaxX is the maximum X value of all the \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20 s that were associated with the TRbwZoomBox and for which \plain\f3\fs20\cf1\strike UseForZoomOut\plain\f3\fs20\cf3 \{linkID=1710\}\plain\f3\fs20  was true at the last time \plain\f3\fs20\cf1\strike GetMinMax\plain\f3\fs20\cf3 \{linkID=1465\}\plain\f3\fs20  was called. If no \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20 s were in existence, it is set to a default value.
\par 
\par }
1126
Scribble1126
MaxX property - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike MaxY property\plain\f3\fs20\cf2 \{linkID=1130\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike MinX property\plain\f3\fs20\cf2 \{linkID=1135\}
\par \plain\f3\fs20\cf1\strike MinY property\plain\f3\fs20\cf2 \{linkID=1140\}\plain\f3\fs20 
\par }
1130
Scribble1130
TRBWZoomBox.MaxY property
MaxY,TRBWZoomBox;TRBWZoomBox,MaxY
prop_TRBWZoomBoxMaxY

rbwzoombox:001130
Writing


TRBWZoomBox_MaxY;MaxY_Property;MaxY
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.MaxY property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1131\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  MaxY: extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 MaxY is the maximum Y value of all the \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f3\fs20 s that were associated with the TRbwZoomBox and for which \plain\f3\fs20\cf1\strike UseForZoomOut\plain\f3\fs20\cf2 \{linkID=1710\}\plain\f3\fs20  was true at the last time \plain\f3\fs20\cf1\strike GetMinMax\plain\f3\fs20\cf2 \{linkID=1465\}\plain\f3\fs20  was called. If no \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f3\fs20 s were in existence, it is set to a default value.
\par 
\par }
1131
Scribble1131
MaxY property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike MaxX property\plain\f3\fs20\cf3 \{linkID=1125\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike MinX property\plain\f3\fs20\cf3 \{linkID=1135\}
\par \plain\f3\fs20\cf1\strike MinY property\plain\f3\fs20\cf3 \{linkID=1140\}\plain\f3\fs20 
\par 
\par }
1135
Scribble1135
TRBWZoomBox.MinX property
MinX,TRBWZoomBox;TRBWZoomBox,MinX
prop_TRBWZoomBoxMinX

rbwzoombox:001135
Writing


TRBWZoomBox_MinX;MinX_Property;MinX
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.MinX property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf1 \{linkID=%1136\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  MinX: extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 MinX is the minimum X value of all the \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20 s that were associated with the TRbwZoomBox and for which \plain\f3\fs20\cf2\strike UseForZoomOut\plain\f3\fs20\cf1 \{linkID=1710\}\plain\f3\fs20  was true at the last time \plain\f3\fs20\cf2\strike GetMinMax\plain\f3\fs20\cf1 \{linkID=1465\}\plain\f3\fs20  was called. If no \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20 s were in existence, it is set to a default value.
\par 
\par }
1136
Scribble1136
MinX property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike MaxX property\plain\f3\fs20\cf1 \{linkID=1125\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike MaxY property\plain\f3\fs20\cf1 \{linkID=1130\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike MinY property\plain\f3\fs20\cf1 \{linkID=1140\}\plain\f3\fs20 
\par 
\par }
1140
Scribble1140
TRBWZoomBox.MinY property
MinY,TRBWZoomBox;TRBWZoomBox,MinY
prop_TRBWZoomBoxMinY

rbwzoombox:001140
Writing


TRBWZoomBox_MinY;MinY_Property;MinY
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.MinY property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf3 \{linkID=%1141\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  MinY: extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 MinY is the minimum Y value of all the \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20 s that were associated with the TRbwZoomBox and for which \plain\f3\fs20\cf2\strike UseForZoomOut\plain\f3\fs20\cf3 \{linkID=1710\}\plain\f3\fs20  was true at the last time \plain\f3\fs20\cf2\strike GetMinMax\plain\f3\fs20\cf3 \{linkID=1465\}\plain\f3\fs20  was called. If no \plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20 s were in existence, it is set to a default value.
\par 
\par }
1141
Scribble1141
MinY property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike MaxX property\plain\f3\fs20\cf1 \{linkID=1125\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike MaxY property\plain\f3\fs20\cf1 \{linkID=1130\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike MinX property\plain\f3\fs20\cf1 \{linkID=1135\}
\par \plain\f3\fs20 
\par }
1145
Scribble1145
TRBWZoomBox.Multiplier property
Multiplier,TRBWZoomBox;TRBWZoomBox,Multiplier
prop_TRBWZoomBoxMultiplier

rbwzoombox:001145
Writing


TRBWZoomBox_Multiplier;Multiplier_Property;Multiplier
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.Multiplier property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf3 \{linkID=%1146\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  Multiplier: Extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 Multiplier is the current zoom level.
\par 
\par }
1146
Scribble1146
Multiplier property - See also




Writing



FALSE
6
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike DefaultMultiplier property\plain\f3\fs20\cf1 \{linkID=1115\}\plain\f3\fs20\cf2\strike 
\par }
1150
Scribble1150
TRBWZoomBox.PBCanvas property
PBCanvas,TRBWZoomBox;TRBWZoomBox,PBCanvas
prop_TRBWZoomBoxPBCanvas

rbwzoombox:001150
Writing


TRBWZoomBox_PBCanvas;PBCanvas_Property;PBCanvas
FALSE
18
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBCanvas property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBCanvas: TCanvas;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Canvas of the embedded paintbox.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 See \plain\f3\fs20\cf2\strike TGraphicControl\plain\f3\fs20\cf3 \{linkDelphi=TGraphicControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Canvas\plain\f3\fs20\cf3 \{linkDelphi=Canvas_Property\}\plain\f3\fs20 .
\par Run-time only
\par Read-only
\par 
\par }
1155
Scribble1155
TRBWZoomBox.PBClientHeight property
PBClientHeight,TRBWZoomBox;TRBWZoomBox,PBClientHeight
prop_TRBWZoomBoxPBClientHeight

rbwzoombox:001155
Writing


TRBWZoomBox_PBClientHeight;PBClientHeight_Property;PBClientHeight
FALSE
18
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBClientHeight property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBClientHeight: Integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ClientHeight of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ClientHeight\plain\f3\fs20\cf3 \{linkDelphi=ClientHeight_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par Read-only
\par 
\par }
1160
Scribble1160
TRBWZoomBox.PBClientOrigin property
PBClientOrigin,TRBWZoomBox;TRBWZoomBox,PBClientOrigin
prop_TRBWZoomBoxPBClientOrigin

rbwzoombox:001160
Writing


TRBWZoomBox_PBClientOrigin;PBClientOrigin_Property;PBClientOrigin
FALSE
18
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBClientOrigin property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBClientOrigin: TPoint;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ClientOrigin of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ClientOrigin\plain\f3\fs20\cf3 \{linkDelphi=ClientOrigin_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par Read-only
\par 
\par }
1165
Scribble1165
TRBWZoomBox.PBClientRect property
PBClientRect,TRBWZoomBox;TRBWZoomBox,PBClientRect
prop_TRBWZoomBoxPBClientRect

rbwzoombox:001165
Writing


TRBWZoomBox_PBClientRect;PBClientRect_Property;PBClientRect
FALSE
18
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBClientRect property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBClientRect: TRect;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ClientRect of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ClientRect\plain\f3\fs20\cf3 \{linkDelphi=ClientRect_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par Read-only
\par 
\par }
1170
Scribble1170
TRBWZoomBox.PBClientWidth property
PBClientWidth,TRBWZoomBox;TRBWZoomBox,PBClientWidth
prop_TRBWZoomBoxPBClientWidth

rbwzoombox:001170
Writing


TRBWZoomBox_PBClientWidth;PBClientWidth_Property;PBClientWidth
FALSE
18
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBClientWidth property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBClientWidth: Integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ClientWidth of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ClientWidth\plain\f3\fs20\cf3 \{linkDelphi=ClientWidth_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par Read-only
\par 
\par }
1175
Scribble1175
TRBWZoomBox.PBColor property
PBColor,TRBWZoomBox;TRBWZoomBox,PBColor
prop_TRBWZoomBoxPBColor

rbwzoombox:001175
Writing


TRBWZoomBox_PBColor;PBColor_Property;PBColor
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBColor property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBColor: TColor;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Color of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Color\plain\f3\fs20\cf3 \{linkDelphi=Color_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1180
Scribble1180
TRBWZoomBox.PBComObject property
PBComObject,TRBWZoomBox;TRBWZoomBox,PBComObject
prop_TRBWZoomBoxPBComObject

rbwzoombox:001180
Writing


TRBWZoomBox_PBComObject;PBComObject_Property;PBComObject
FALSE
18
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBComObject property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBComObject: IUnknown;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ComObject of the embedded paintbox.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 See \plain\f3\fs20\cf2\strike TComponent\plain\f3\fs20\cf3 \{linkDelphi=TComponent_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ComObject\plain\f3\fs20\cf3 \{linkDelphi=ComObject_Property\}\plain\f3\fs20 .
\par Run-time only
\par Read-only
\par 
\par }
1185
Scribble1185
TRBWZoomBox.PBComponentCount property
PBComponentCount,TRBWZoomBox;TRBWZoomBox,PBComponentCount
prop_TRBWZoomBoxPBComponentCount

rbwzoombox:001185
Writing


TRBWZoomBox_PBComponentCount;PBComponentCount_Property;PBComponentCount
FALSE
18
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBComponentCount property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBComponentCount: integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ComponentCount of the embedded paintbox.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 See \plain\f3\fs20\cf2\strike TComponent\plain\f3\fs20\cf3 \{linkDelphi=TComponent_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ComponentCount\plain\f3\fs20\cf3 \{linkDelphi=ComponentCount_Property\}\plain\f3\fs20 .
\par Run-time only
\par Read-only
\par 
\par }
1190
Scribble1190
TRBWZoomBox.PBComponentIndex property
PBComponentIndex,TRBWZoomBox;TRBWZoomBox,PBComponentIndex
prop_TRBWZoomBoxPBComponentIndex

rbwzoombox:001190
Writing


TRBWZoomBox_PBComponentIndex;PBComponentIndex_Property;PBComponentIndex
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBComponentIndex property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBComponentIndex: integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ComponentIndex of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TComponent\plain\f3\fs20\cf3 \{linkDelphi=TComponent_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ComponentIndex\plain\f3\fs20\cf3 \{linkDelphi=ComponentIndex_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par 
\par }
1195
Scribble1195
TRBWZoomBox.PBComponents property
PBComponents,TRBWZoomBox;TRBWZoomBox,PBComponents
prop_TRBWZoomBoxPBComponents

rbwzoombox:001195
Writing


TRBWZoomBox_PBComponents;PBComponents_Property;PBComponents
FALSE
18
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBComponents property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBComponents[Index: Integer: TComponent;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Components of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TComponent\plain\f3\fs20\cf3 \{linkDelphi=TComponent_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Components\plain\f3\fs20\cf3 \{linkDelphi=Components_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par Read-only
\par 
\par }
1200
Scribble1200
TRBWZoomBox.PBControlState property
PBControlState,TRBWZoomBox;TRBWZoomBox,PBControlState
prop_TRBWZoomBoxPBControlState

rbwzoombox:001200
Writing


TRBWZoomBox_PBControlState;PBControlState_Property;PBControlState
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBControlState property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBControlState: TControlState;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ControlState of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ControlState\plain\f3\fs20\cf3 \{linkDelphi=ControlState_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par 
\par }
1205
Scribble1205
TRBWZoomBox.PBControlStyle property
PBControlStyle,TRBWZoomBox;TRBWZoomBox,PBControlStyle
prop_TRBWZoomBoxPBControlStyle

rbwzoombox:001205
Writing


TRBWZoomBox_PBControlStyle;PBControlStyle_Property;PBControlStyle
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBControlStyle property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBControlStyle: TControlStyle;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ControlStyle of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ControlStyle\plain\f3\fs20\cf3 \{linkDelphi=ControlStyle_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par 
\par }
1210
Scribble1210
TRBWZoomBox.PBCursor property
PBCursor,TRBWZoomBox;TRBWZoomBox,PBCursor
prop_TRBWZoomBoxPBCursor

rbwzoombox:001210
Writing


TRBWZoomBox_PBCursor;PBCursor_Property;PBCursor
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBCursor property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBCursor: TCursor;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Cursor of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Cursor\plain\f3\fs20\cf3 \{linkDelphi=Cursor_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1215
Scribble1215
TRBWZoomBox.PBDragCursor property
PBDragCursor,TRBWZoomBox;TRBWZoomBox,PBDragCursor
prop_TRBWZoomBoxPBDragCursor

rbwzoombox:001215
Writing


TRBWZoomBox_PBDragCursor;PBDragCursor_Property;PBDragCursor
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBDragCursor property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBDragCursor: TCursor;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 DragCursor of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike DragCursor\plain\f3\fs20\cf3 \{linkDelphi=DragCursor_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1220
Scribble1220
TRBWZoomBox.PBDragKind property
PBDragKind,TRBWZoomBox;TRBWZoomBox,PBDragKind
prop_TRBWZoomBoxPBDragKind

rbwzoombox:001220
Writing


TRBWZoomBox_PBDragKind;PBDragKind_Property;PBDragKind
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBDragKind property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBDragKind: TDragKind;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 DragKind of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike DragKind\plain\f3\fs20\cf3 \{linkDelphi=DragKind_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1225
Scribble1225
TRBWZoomBox.PBDragMode property
PBDragMode,TRBWZoomBox;TRBWZoomBox,PBDragMode
prop_TRBWZoomBoxPBDragMode

rbwzoombox:001225
Writing


TRBWZoomBox_PBDragMode;PBDragMode_Property;PBDragMode
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBDragMode property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBDragMode: TDragMode;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 DragMode of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike DragMode\plain\f3\fs20\cf3 \{linkDelphi=DragMode_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1230
Scribble1230
TRBWZoomBox.PBEnabled property
PBEnabled,TRBWZoomBox;TRBWZoomBox,PBEnabled
prop_TRBWZoomBoxPBEnabled

rbwzoombox:001230
Writing


TRBWZoomBox_PBEnabled;PBEnabled_Property;PBEnabled
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBEnabled property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBEnabled: Boolean;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Enabled of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Enabled\plain\f3\fs20\cf3 \{linkDelphi=Enabled_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1235
Scribble1235
TRBWZoomBox.PBFont property
PBFont,TRBWZoomBox;TRBWZoomBox,PBFont
prop_TRBWZoomBoxPBFont

rbwzoombox:001235
Writing


TRBWZoomBox_PBFont;PBFont_Property;PBFont
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBFont property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBFont: TFont;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Font of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Font\plain\f3\fs20\cf3 \{linkDelphi=Font_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1240
Scribble1240
TRBWZoomBox.PBHeight property
PBHeight,TRBWZoomBox;TRBWZoomBox,PBHeight
prop_TRBWZoomBoxPBHeight

rbwzoombox:001240
Writing


TRBWZoomBox_PBHeight;PBHeight_Property;PBHeight
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBHeight property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBHeight: Integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Height of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Height\plain\f3\fs20\cf3 \{linkDelphi=Height_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1245
Scribble1245
TRBWZoomBox.PBHint property
PBHint,TRBWZoomBox;TRBWZoomBox,PBHint
prop_TRBWZoomBoxPBHint

rbwzoombox:001245
Writing


TRBWZoomBox_PBHint;PBHint_Property;PBHint
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBHint property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBHint: \plain\f4\fs20\b string\plain\f4\fs20 ;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Hint of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Hint\plain\f3\fs20\cf3 \{linkDelphi=Hint_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1250
Scribble1250
TRBWZoomBox.PBLeft property
PBLeft,TRBWZoomBox;TRBWZoomBox,PBLeft
prop_TRBWZoomBoxPBLeft

rbwzoombox:001250
Writing


TRBWZoomBox_PBLeft;PBLeft_Property;PBLeft
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBLeft property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBLeft: integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Left of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Left\plain\f3\fs20\cf3 \{linkDelphi=Left_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1255
Scribble1255
TRBWZoomBox.PBPopupMenu property
PBPopupMenu,TRBWZoomBox;TRBWZoomBox,PBPopupMenu
prop_TRBWZoomBoxPBPopupMenu

rbwzoombox:001255
Writing


TRBWZoomBox_PBPopupMenu;PBPopupMenu_Property;PBPopupMenu
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBPopupMenu property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBPopupMenu: TPopupMenu;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 PopupMenu of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike PopupMenu\plain\f3\fs20\cf3 \{linkDelphi=PopupMenu_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1260
Scribble1260
TRBWZoomBox.PBShowHint property
PBShowHint,TRBWZoomBox;TRBWZoomBox,PBShowHint
prop_TRBWZoomBoxPBShowHint

rbwzoombox:001260
Writing


TRBWZoomBox_PBShowHint;PBShowHint_Property;PBShowHint
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBShowHint property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBShowHint: Boolean;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ShowHint of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike ShowHint\plain\f3\fs20\cf3 \{linkDelphi=ShowHint_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1265
Scribble1265
TRBWZoomBox.PBTag property
PBTag,TRBWZoomBox;TRBWZoomBox,PBTag
prop_TRBWZoomBoxPBTag

rbwzoombox:001265
Writing


TRBWZoomBox_PBTag;PBTag_Property;PBTag
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBTag property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBTag: Longint;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Tag of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TComponent\plain\f3\fs20\cf3 \{linkDelphi=TComponent_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Tag\plain\f3\fs20\cf3 \{linkDelphi=Tag_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1270
Scribble1270
TRBWZoomBox.PBTop property
PBTop,TRBWZoomBox;TRBWZoomBox,PBTop
prop_TRBWZoomBoxPBTop

rbwzoombox:001270
Writing


TRBWZoomBox_PBTop;PBTop_Property;PBTop
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBTop property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBTop: Integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Top of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Top\plain\f3\fs20\cf3 \{linkDelphi=Top_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1275
Scribble1275
TRBWZoomBox.PBVisible property
PBVisible,TRBWZoomBox;TRBWZoomBox,PBVisible
prop_TRBWZoomBoxPBVisible

rbwzoombox:001275
Writing


TRBWZoomBox_PBVisible;PBVisible_Property;PBVisible
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBVisible property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBVisible: Boolean;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Visible of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Visible\plain\f3\fs20\cf3 \{linkDelphi=Visible_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1280
Scribble1280
TRBWZoomBox.PBWidth property
PBWidth,TRBWZoomBox;TRBWZoomBox,PBWidth
prop_TRBWZoomBoxPBWidth

rbwzoombox:001280
Writing


TRBWZoomBox_PBWidth;PBWidth_Property;PBWidth
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBWidth property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBWidth: Integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Width of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Width\plain\f3\fs20\cf3 \{linkDelphi=Width_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1285
Scribble1285
TRBWZoomBox.PBWindowProc property
PBWindowProc,TRBWZoomBox;TRBWZoomBox,PBWindowProc
prop_TRBWZoomBoxPBWindowProc

rbwzoombox:001285
Writing


TRBWZoomBox_PBWindowProc;PBWindowProc_Property;PBWindowProc
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBWindowProc property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  PBWindowProc: TWndMethod;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 WindowProc of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf3 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike WindowProc\plain\f3\fs20\cf3 \{linkDelphi=WindowProc_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par 
\par }
1290
Scribble1290
TRBWZoomBox.RightMargin property
RightMargin,TRBWZoomBox;TRBWZoomBox,RightMargin
prop_TRBWZoomBoxRightMargin

rbwzoombox:001290
Writing


TRBWZoomBox_RightMargin;RightMargin_Property;RightMargin
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f4\fs32\cf2\b TRbwZoomBox.RightMargin property
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf1\ul See also\plain\f4\fs16\cf3 \{linkID=%1291\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf1\strike TRbwZoomBox\plain\f4\fs20\cf3 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b property\plain\f3\fs20  RightMargin: integer;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \plain\f4\fs20 RightMargin is the width of the space in pixels between the right edge of the embedded TPaintBox client area and the \plain\f4\fs20\cf1\strike TRbwZoomPoint\plain\f4\fs20\cf3 \{linkID=1700\}\plain\f4\fs20  whose screen X-coordinate was the highest (furthest to the right on the screen) at the time of the last call to \plain\f4\fs20\cf1\strike GetMinMax\plain\f4\fs20\cf3 \{linkID=1465\}\plain\f4\fs20 .
\par 
\par }
1291
Scribble1291
RightMargin property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike BottomMargin property\plain\f3\fs20\cf3 \{linkID=1110\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike LeftMargin property\plain\f3\fs20\cf3 \{linkID=1120\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike TopMargin property\plain\f3\fs20\cf3 \{linkID=1325\}\plain\f3\fs20 
\par 
\par }
1295
Scribble1295
TRBWZoomBox.SBoundsRect property
SBoundsRect,TRBWZoomBox;TRBWZoomBox,SBoundsRect
prop_TRBWZoomBoxSBoundsRect

rbwzoombox:001295
Writing


TRBWZoomBox_SBoundsRect;SBoundsRect_Property;SBoundsRect
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.SBoundsRect property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  SBoundsRect: TRect;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 BoundsRect of the embedded shape.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike BoundsRect\plain\f3\fs20\cf1 \{linkDelphi=BoundsRect_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 Run-time only
\par 
\par }
1300
Scribble1300
TRBWZoomBox.SBrush property
SBrush,TRBWZoomBox;TRBWZoomBox,SBrush
prop_TRBWZoomBoxSBrush

rbwzoombox:001300
Writing


TRBWZoomBox_SBrush;SBrush_Property;SBrush
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.SBrush property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  SBrush: TBrush;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Brush of the embedded shape.
\par See \plain\f3\fs20\cf1\strike TShape\plain\f3\fs20\cf2 \{linkDelphi=TShape_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf1\strike Brush\plain\f3\fs20\cf2 \{linkDelphi=Brush_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1305
Scribble1305
TRBWZoomBox.SCursor property
SCursor,TRBWZoomBox;TRBWZoomBox,SCursor
prop_TRBWZoomBoxSCursor

rbwzoombox:001305
Writing


TRBWZoomBox_SCursor;SCursor_Property;SCursor
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.SCursor property
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b property\plain\f3\fs20  SCursor: TCursor;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 Cursor of the embedded shape.
\par See \plain\f4\fs20\cf2\strike TControl\plain\f4\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf2\strike Cursor\plain\f4\fs20\cf1 \{linkDelphi=Cursor_Property\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1310
Scribble1310
TRBWZoomBox.SDragCursor property
SDragCursor,TRBWZoomBox;TRBWZoomBox,SDragCursor
prop_TRBWZoomBoxSDragCursor

rbwzoombox:001310
Writing


TRBWZoomBox_SDragCursor;SDragCursor_Property;SDragCursor
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.SDragCursor property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  SDragCursor: TCursor;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 DragCursor of the embedded shape.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike DragCursor\plain\f3\fs20\cf1 \{linkDelphi=DragCursor_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1315
Scribble1315
TRBWZoomBox.SelectionWidth property
SelectionWidth,TRBWZoomBox;TRBWZoomBox,SelectionWidth
prop_TRBWZoomBoxSelectionWidth

rbwzoombox:001315
Writing


TRBWZoomBox_SelectionWidth;SelectionWidth_Property;SelectionWidth
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.SelectionWidth property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1316\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  SelectionWidth: integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 SelectionWidth is used in \plain\f3\fs20\cf1\strike SelectPoint\plain\f3\fs20\cf2 \{linkID=1390\}\plain\f3\fs20 , \plain\f3\fs20\cf1\strike SelectSegment\plain\f3\fs20\cf2 \{linkID=1405\}\plain\f3\fs20 , \plain\f3\fs20\cf1\strike SelectPolyLine\plain\f3\fs20\cf2 \{linkID=1400\}\plain\f3\fs20 , and \plain\f3\fs20\cf1\strike SelectPolygon\plain\f3\fs20\cf2 \{linkID=1395\}\plain\f3\fs20  to determine how far away from the object a location can be and still allow the object to be selected.
\par Published declarations
\par }
1316
Scribble1316
SelectionWidth property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectPoint\plain\f3\fs20\cf3 \{linkID=1390\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectSegment\plain\f3\fs20\cf3 \{linkID=1405\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectPolyLine\plain\f3\fs20\cf3 \{linkID=1400\}\plain\f3\fs20  
\par \plain\f3\fs20\cf2\strike SelectPolygon\plain\f3\fs20\cf3 \{linkID=1395\}\plain\f3\fs20 
\par }
1320
Scribble1320
TRBWZoomBox.SPen property
SPen,TRBWZoomBox;TRBWZoomBox,SPen
prop_TRBWZoomBoxSPen

rbwzoombox:001320
Writing


TRBWZoomBox_SPen;SPen_Property;SPen
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.SPen property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  SPen: TPen;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Pen of the embedded shape.
\par See \plain\f3\fs20\cf3\strike TShape\plain\f3\fs20\cf1 \{linkDelphi=TShape_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf3\strike Pen\plain\f3\fs20\cf1 \{linkDelphi=Pen_Property\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1325
Scribble1325
TRBWZoomBox.TopMargin property
TopMargin,TRBWZoomBox;TRBWZoomBox,TopMargin
prop_TRBWZoomBoxTopMargin

rbwzoombox:001325
Writing


TRBWZoomBox_TopMargin;TopMargin_Property;TopMargin
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.TopMargin property
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf2\ul See also\plain\f4\fs16\cf1 \{linkID=%1326\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b property\plain\f3\fs20  TopMargin: integer;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \plain\f4\fs20 TopMargin is the width of the space in pixels between the top edge of the embedded TPaintBox client area and the \plain\f4\fs20\cf2\strike TRbwZoomPoint\plain\f4\fs20\cf1 \{linkID=1700\}\plain\f4\fs20  whose screen Y-coordinate was the lowest (highest up on the screen) at the time of the last call to \plain\f4\fs20\cf2\strike GetMinMax\plain\f4\fs20\cf1 \{linkID=1465\}\plain\f4\fs20 .
\par Published declarations
\par }
1326
Scribble1326
TopMargin property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike BottomMargin property\plain\f3\fs20\cf2 \{linkID=1110\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike LeftMargin property\plain\f3\fs20\cf2 \{linkID=1120\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike RightMargin property\plain\f3\fs20\cf2 \{linkID=1290\}\plain\f3\fs20 
\par 
\par }
1330
Scribble1330
TRBWZoomBox.VerticalExaggeration property
VerticalExaggeration,TRBWZoomBox;TRBWZoomBox,VerticalExaggeration
prop_TRBWZoomBoxVerticalExaggeration

rbwzoombox:001330
Writing


TRBWZoomBox_VerticalExaggeration;VerticalExaggeration_Property;VerticalExaggeration
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.VerticalExaggeration property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  VerticalExaggeration: double;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 VerticalExaggeration represents the degree of vertical exaggeration that will be used in converting real-number Y-coordinates to the screen Y-coordinates.
\par Published declarations
\par 
\par }
1335
Scribble1335
TRBWZoomBox.XPositive property
XPositive,TRBWZoomBox;TRBWZoomBox,XPositive
prop_TRBWZoomBoxXPositive

rbwzoombox:001335
Writing


TRBWZoomBox_XPositive;XPositive_Property;XPositive
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.XPositive property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1336\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  XPositive: boolean;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 XPositive is true if the direction of the real-number X-coordinate axis is positive to the right.
\par 
\par }
1336
Scribble1336
XPositive property - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike YPositive property\plain\f3\fs20\cf2 \{linkID=1340\}\plain\f3\fs20 
\par 
\par }
1340
Scribble1340
TRBWZoomBox.YPositive property
YPositive,TRBWZoomBox;TRBWZoomBox,YPositive
prop_TRBWZoomBoxYPositive

rbwzoombox:001340
Writing


TRBWZoomBox_YPositive;YPositive_Property;YPositive
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.YPositive property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf3 \{linkID=%1341\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  YPositive: boolean;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 YPositive is true if the direction of the real-number Y-coordinate axis is positive upward.
\par 
\par }
1341
Scribble1341
YPositive property - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike XPositive property\plain\f3\fs20\cf2 \{linkID=1335\}\plain\f3\fs20 
\par 
\par }
1345
Scribble1345
TRBWZoomBox.Create method
Create,TRBWZoomBox;TRBWZoomBox,Create


rbwzoombox:001345
Writing


TRBWZoomBox_Create;Create_Method;Create
FALSE
25
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.Create method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b constructor\plain\f4\fs20  Create(AOwner: TComponent); \plain\f4\fs20\b override\plain\f4\fs20 ;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 Creates and initializes a new TRbwZoomBox object. 
\par 
\par Create performs the following tasks: 
\par Calls the TScrollBox Create method, passing it AOwner. 
\par Creates a TPaintbox and TShape passing itself as AOwner and Parent. 
\par Creates a list of \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f3\fs20 . 
\par Sets the \plain\f3\fs20\cf3\strike Margins\plain\f3\fs20\cf2 \{linkID=1120\}\plain\f3\fs20  to 20. 
\par Sets \plain\f3\fs20\cf3\strike Multiplier\plain\f3\fs20\cf2 \{linkID=1145\}\plain\f3\fs20  to 1. 
\par Sets the \plain\f3\fs20\cf3\strike vertical exaggeration\plain\f3\fs20\cf2 \{linkID=1330\}\plain\f3\fs20  to 1. 
\par Sets \plain\f3\fs20\cf3\strike XPositive\plain\f3\fs20\cf2 \{linkID=1335\}\plain\f3\fs20  and \plain\f3\fs20\cf3\strike YPositive\plain\f3\fs20\cf2 \{linkID=1340\}\plain\f3\fs20  to True.
\par Sets the \plain\f3\fs20\cf3\strike SelectionWidth\plain\f3\fs20\cf2 \{linkID=1315\}\plain\f3\fs20  to 3.
\par 
\par }
1350
Scribble1350
TRBWZoomBox.Destroy method
Destroy,TRBWZoomBox;TRBWZoomBox,Destroy


rbwzoombox:001350
Writing


TRBWZoomBox_Destroy;Destroy_Method;Destroy
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.Destroy method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b destructor\plain\f4\fs20  Destroy; \plain\f4\fs20\b override\plain\f4\fs20 ;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 Removes the TScrollingWinControl object from memory. Description Do not call Destroy. Instead, call Free, which calls Destroy if the \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  \plain\f3\fs20 object is not nil. Destroy frees the TPaintbox and TShape and the list of TRbwZoomPoint's and then calls TScrollBox.Destroy. It does \plain\f3\fs20\b not\plain\f3\fs20  destroy the TRbwZoomPoint's in the list.
\par 
\par }
1355
Scribble1355
TRBWZoomBox.IsPointInside method
IsPointInside,TRBWZoomBox;TRBWZoomBox,IsPointInside


rbwzoombox:001355
Writing


TRBWZoomBox_IsPointInside;IsPointInside_Method;IsPointInside
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f4\fs32\cf2\b TRbwZoomBox.IsPointInside method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf1\ul See also\plain\f4\fs16\cf3 \{linkID=%1356\}\tab \plain\f4\fs16\cf1\strike Example\plain\f4\fs16\cf3 \{linkID=%1357\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf1\strike TRbwZoomBox\plain\f4\fs20\cf3 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b function\plain\f3\fs20  IsPointInside(\plain\f3\fs20\b const\plain\f3\fs20  X, Y: extended; \plain\f3\fs20\b const\plain\f3\fs20  ZoomPointArray: \plain\f3\fs20\cf1\strike TZBArray\plain\f3\fs20\cf3 \{linkID=1800\}\plain\f3\fs20 ): boolean;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \plain\f4\fs20 IsPointInside tests whether a point with real-number coordinates X and Y is inside a polygon defined by a series of \plain\f4\fs20\cf1\strike TRbwZoomPoint\plain\f4\fs20\cf3 \{linkID=1700\}\plain\f4\fs20  in ZoomPointArray. The first and last \plain\f4\fs20\cf1\strike TRbwZoomPoint\plain\f4\fs20\cf3 \{linkID=1700\}\plain\f4\fs20 s in ZoomPointArray must be at the same location for IsPointInside to return true.
\par 
\par }
1356
Scribble1356
IsPointInside method - See also




Writing



FALSE
11
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs20\cf3\b See also\plain\f3\fs20 
\par 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\strike SelectPoint method\plain\f3\fs20\cf1 \{linkID=1390\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectSegment method\plain\f3\fs20\cf1 \{linkID=1405\}
\par \plain\f3\fs20\cf2\strike SelectPolyLine method\plain\f3\fs20\cf1 \{linkID=1400\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectPolygon method\plain\f3\fs20\cf1 \{linkID=1395\}\plain\f3\fs20 
\par 
\par \pard\plain\f3\fs20 
\par }
1357
Scribble1357
IsPointInside method - Example




Writing



FALSE
30
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b IsPointInside method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b function\plain\f3\fs20  TRbwZoomBox.SelectPolygon(\plain\f3\fs20\b const\plain\f3\fs20  X, Y: integer;
\par   \plain\f3\fs20\b const\plain\f3\fs20  ZoomPointArray: TZBArray): boolean;
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par   result := SelectPolyLine(X, Y, ZoomPointArray);
\par   \plain\f3\fs20\b if\plain\f3\fs20  result \plain\f3\fs20\b then\plain\f3\fs20  Exit;
\par   result := IsPointInside(self.X(X), self.Y(Y), ZoomPointArray);
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par 
\par \plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseUp(Sender: TObject; Button: TMouseButton;
\par   Shift: TShiftState; X, Y: Integer);
\par \plain\f3\fs20\b var\plain\f3\fs20 
\par   Index : integer;
\par   ScreenObject : TScreenObject;
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par     \plain\f3\fs20\b for\plain\f3\fs20  Index := 0 \plain\f3\fs20\b to\plain\f3\fs20  ScreenObjectList.Count -1 \plain\f3\fs20\b do\plain\f3\fs20 
\par     \plain\f3\fs20\b begin\plain\f3\fs20 
\par       ScreenObject := ScreenObjectList[Index] \plain\f3\fs20\b as\plain\f3\fs20  TScreenObject;
\par       ScreenObject.Selected := False;
\par       \plain\f3\fs20\b if\plain\f3\fs20  RbwZoomBox1.SelectPolygon(X, Y, ScreenObject.Points) \plain\f3\fs20\b then\plain\f3\fs20 
\par       \plain\f3\fs20\b begin\plain\f3\fs20 
\par         ScreenObject.Selected := True;
\par       \plain\f3\fs20\b end\plain\f3\fs20 ;
\par     \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1360
Scribble1360
TRBWZoomBox.PBClientToScreen method
PBClientToScreen,TRBWZoomBox;TRBWZoomBox,PBClientToScreen


rbwzoombox:001360
Writing


TRBWZoomBox_PBClientToScreen;PBClientToScreen_Method;PBClientToScreen
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBClientToScreen method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  PBClientToScreen(\plain\f4\fs20\b const\plain\f4\fs20  Point: TPoint): TPoint;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 ClientToScreen of the embedded paintbox.
\par See \plain\f3\fs20\cf3\strike TControl\plain\f3\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf3\strike ClientToScreen\plain\f3\fs20\cf2 \{linkDelphi=ClientToScreen_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1365
Scribble1365
TRBWZoomBox.PBDragging method
PBDragging,TRBWZoomBox;TRBWZoomBox,PBDragging


rbwzoombox:001365
Writing


TRBWZoomBox_PBDragging;PBDragging_Method;PBDragging
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f4\fs32\cf1\b TRbwZoomBox.PBDragging method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf3\strike TRbwZoomBox\plain\f4\fs20\cf2 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b function\plain\f3\fs20  PBDragging: Boolean;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 Dragging of the embedded paintbox.
\par See \plain\f4\fs20\cf3\strike TControl\plain\f4\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf3\strike Dragging\plain\f4\fs20\cf2 \{linkDelphi=Dragging_Method\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1370
Scribble1370
TRBWZoomBox.PBGetTextBuf method
PBGetTextBuf,TRBWZoomBox;TRBWZoomBox,PBGetTextBuf


rbwzoombox:001370
Writing


TRBWZoomBox_PBGetTextBuf;PBGetTextBuf_Method;PBGetTextBuf
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBGetTextBuf method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  PBGetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 GetTextBuf of the embedded paintbox.
\par See \plain\f3\fs20\cf3\strike TControl\plain\f3\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf3\strike GetTextBuf\plain\f3\fs20\cf2 \{linkDelphi=GetTextBuf_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1375
Scribble1375
TRBWZoomBox.PBGetTextLen method
PBGetTextLen,TRBWZoomBox;TRBWZoomBox,PBGetTextLen


rbwzoombox:001375
Writing


TRBWZoomBox_PBGetTextLen;PBGetTextLen_Method;PBGetTextLen
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f4\fs32\cf1\b TRbwZoomBox.PBGetTextLen method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf3\strike TRbwZoomBox\plain\f4\fs20\cf2 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b function\plain\f3\fs20  PBGetTextLen: Integer;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 GetTextLen of the embedded paintbox.
\par See \plain\f4\fs20\cf3\strike TControl\plain\f4\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf3\strike GetTextLen\plain\f4\fs20\cf2 \{linkDelphi=GetTextLen_Method\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1380
Scribble1380
TRBWZoomBox.PBPerform method
PBPerform,TRBWZoomBox;TRBWZoomBox,PBPerform


rbwzoombox:001380
Writing


TRBWZoomBox_PBPerform;PBPerform_Method;PBPerform
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.PBPerform method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  PBPerform(Msg: Cardinal; WParam, LParam: Longint): Longint;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Perform of the embedded paintbox.
\par See \plain\f3\fs20\cf3\strike TControl\plain\f3\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf3\strike Perform\plain\f3\fs20\cf2 \{linkDelphi=Perform_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1385
Scribble1385
TRBWZoomBox.PBScreenToClient method
PBScreenToClient,TRBWZoomBox;TRBWZoomBox,PBScreenToClient


rbwzoombox:001385
Writing


TRBWZoomBox_PBScreenToClient;PBScreenToClient_Method;PBScreenToClient
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f4\fs32\cf1\b TRbwZoomBox.PBScreenToClient method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf3\strike TRbwZoomBox\plain\f4\fs20\cf2 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b function\plain\f3\fs20  PBScreenToClient(\plain\f3\fs20\b const\plain\f3\fs20  Point: TPoint): TPoint;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 ScreenToClient of the embedded paintbox.
\par See \plain\f4\fs20\cf3\strike TControl\plain\f4\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf3\strike ScreenToClient\plain\f4\fs20\cf2 \{linkDelphi=ScreenToClient_Method\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1390
Scribble1390
TRBWZoomBox.SelectPoint method
SelectPoint,TRBWZoomBox;TRBWZoomBox,SelectPoint


rbwzoombox:001390
Writing


TRBWZoomBox_SelectPoint;SelectPoint_Method;SelectPoint
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.SelectPoint method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf3 \{linkID=%1391\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf3 \{linkID=%1392\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  SelectPoint(\plain\f4\fs20\b const\plain\f4\fs20  X, Y: integer; \plain\f4\fs20\b const\plain\f4\fs20  AZoomPoint: \plain\f4\fs20\cf1\strike TRbwZoomPoint\plain\f4\fs20\cf3 \{linkID=1700\}\plain\f4\fs20 ): boolean; \plain\f4\fs20\b virtual\plain\f4\fs20 ;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f4\fs20 SelectPoint\plain\f3\fs20  returns true only if the screen coordinates of AZoomPoint are within \plain\f3\fs20\cf1\strike SelectionWidth\plain\f3\fs20\cf3 \{linkID=1315\}\plain\f3\fs20  in both the X and Y directions.
\par 
\par }
1391
Scribble1391
SelectPoint method - See also




Writing



FALSE
10
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike IsPointInside method\plain\f3\fs20\cf2 \{linkID=1355\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike SelectSegment method\plain\f3\fs20\cf2 \{linkID=1405\}
\par \plain\f3\fs20\cf3\strike SelectPolyLine method\plain\f3\fs20\cf2 \{linkID=1400\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike SelectPolygon method\plain\f3\fs20\cf2 \{linkID=1395\}\plain\f3\fs20 
\par 
\par }
1392
Scribble1392
SelectPoint method - Example




Writing



FALSE
45
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b SelectPoint method example
\par \plain\f3\fs20 
\par \pard\plain\f4\fs20\b procedure\plain\f4\fs20  TForm1.RbwZoomBox1MouseUp(Sender: TObject; Button: TMouseButton;
\par   Shift: TShiftState; X, Y: Integer);
\par \plain\f4\fs20\b var\plain\f4\fs20 
\par   Index : integer;
\par   APoint : TRbwZoomPoint;
\par \plain\f4\fs20\b begin\plain\f4\fs20 
\par     \plain\f4\fs20\b for\plain\f4\fs20  Index := 0 \plain\f4\fs20\b to\plain\f4\fs20  PointsList.Count -1 \plain\f4\fs20\b do\plain\f4\fs20 
\par     \plain\f4\fs20\b begin\plain\f4\fs20 
\par       APoint := PointsList[Index] \plain\f4\fs20\b as\plain\f4\fs20  TRbwZoomPoint;
\par       \plain\f4\fs20\b if\plain\f4\fs20  RbwZoomBox1.SelectPoint(X, Y, APoint) \plain\f4\fs20\b then\plain\f4\fs20 
\par       \plain\f4\fs20\b begin\plain\f4\fs20 
\par         ShowMessage('Selected');
\par       \plain\f4\fs20\b end\plain\f4\fs20 ;
\par     \plain\f4\fs20\b end\plain\f4\fs20 ;
\par \plain\f4\fs20\b end\plain\f4\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par \pard\plain\f4\fs20\b function\plain\f4\fs20  TRbwZoomBox.SelectPolyLine(\plain\f4\fs20\b const\plain\f4\fs20  X, Y: integer;
\par   \plain\f4\fs20\b const\plain\f4\fs20  ZoomPointArray: TZBArray): boolean;
\par \plain\f4\fs20\b Var\plain\f4\fs20 
\par   Index : integer;
\par   ZoomPoint1, ZoomPoint2 :TRbwZoomPoint;
\par \plain\f4\fs20\b begin\plain\f4\fs20 
\par   result := False;
\par   \plain\f4\fs20\b for\plain\f4\fs20  Index := 0 \plain\f4\fs20\b to\plain\f4\fs20  Length(ZoomPointArray) -1 \plain\f4\fs20\b do\plain\f4\fs20 
\par   \plain\f4\fs20\b begin\plain\f4\fs20 
\par     ZoomPoint1 := ZoomPointArray[Index];
\par     result := self.SelectPoint(X, Y, ZoomPoint1);
\par     \plain\f4\fs20\b if\plain\f4\fs20  Result \plain\f4\fs20\b then\plain\f4\fs20  Exit;
\par   \plain\f4\fs20\b end\plain\f4\fs20 ;
\par 
\par   \plain\f4\fs20\b for\plain\f4\fs20  Index := 0 \plain\f4\fs20\b to\plain\f4\fs20  Length(ZoomPointArray) -2 \plain\f4\fs20\b do\plain\f4\fs20 
\par   \plain\f4\fs20\b begin\plain\f4\fs20 
\par     ZoomPoint1 := ZoomPointArray[Index];
\par     ZoomPoint2 := ZoomPointArray[Index+1];
\par     result := SelectSegment(X, Y, ZoomPoint1, ZoomPoint2);
\par     \plain\f4\fs20\b if\plain\f4\fs20  result \plain\f4\fs20\b then\plain\f4\fs20  Exit;
\par   \plain\f4\fs20\b end\plain\f4\fs20 ;
\par \plain\f4\fs20\b end\plain\f4\fs20 ;
\par 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1395
Scribble1395
TRBWZoomBox.SelectPolygon method
SelectPolygon,TRBWZoomBox;TRBWZoomBox,SelectPolygon


rbwzoombox:001395
Writing


TRBWZoomBox_SelectPolygon;SelectPolygon_Method;SelectPolygon
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.SelectPolygon method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1396\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf2 \{linkID=%1397\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  SelectPolygon(\plain\f4\fs20\b const\plain\f4\fs20  X, Y: integer; \plain\f4\fs20\b const\plain\f4\fs20  ZoomPointArray: \plain\f4\fs20\cf1\strike TZBArray\plain\f4\fs20\cf2 \{linkID=1800\}\plain\f4\fs20 ): boolean; \plain\f4\fs20\b virtual\plain\f4\fs20 ;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 SelectPolygon returns true if the screen coordinates X and Y are close to or inside the polygon defined by the series of points in ZoomPointArray. The last point in ZoomPointArray should have the same coordinates as the first point in ZoomPointArray to properly define a polygon. If the coordinates are not identical, SelectPolygon returns the same result as \plain\f3\fs20\cf1\strike SelectPolyLine\plain\f3\fs20\cf2 \{linkID=1400\}\plain\f3\fs20 . SelectPolygon first calls \plain\f3\fs20\cf1\strike SelectPolyLine\plain\f3\fs20\cf2 \{linkID=1400\}\plain\f3\fs20  and returns True if \plain\f3\fs20\cf1\strike SelectPolyLine\plain\f3\fs20\cf2 \{linkID=1400\}\plain\f3\fs20  returns true. Otherwise it then calls \plain\f3\fs20\cf1\strike IsPointInside\plain\f3\fs20\cf2 \{linkID=1355\}\plain\f3\fs20  using the real-number coordinates corresponding to X and Y and returns the result of \plain\f3\fs20\cf1\strike IsPointInside\plain\f3\fs20\cf2 \{linkID=1355\}\plain\f3\fs20 .
\par 
\par }
1396
Scribble1396
SelectPolygon method - See also




Writing



FALSE
10
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike IsPointInside method\plain\f3\fs20\cf1 \{linkID=1355\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectPoint method\plain\f3\fs20\cf1 \{linkID=1390\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectSegment method\plain\f3\fs20\cf1 \{linkID=1405\}
\par \plain\f3\fs20\cf2\strike SelectPolyLine method\plain\f3\fs20\cf1 \{linkID=1400\}\plain\f3\fs20 
\par 
\par }
1397
Scribble1397
SelectPolygon method - Example




Writing



FALSE
23
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b SelectPolygon method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseUp(Sender: TObject; Button: TMouseButton;
\par   Shift: TShiftState; X, Y: Integer);
\par \plain\f3\fs20\b var\plain\f3\fs20 
\par   Index : integer;
\par   ScreenObject : TScreenObject;
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par     \plain\f3\fs20\b for\plain\f3\fs20  Index := 0 \plain\f3\fs20\b to\plain\f3\fs20  ScreenObjectList.Count -1 \plain\f3\fs20\b do\plain\f3\fs20 
\par     \plain\f3\fs20\b begin\plain\f3\fs20 
\par       ScreenObject := ScreenObjectList[Index] \plain\f3\fs20\b as\plain\f3\fs20  TScreenObject;
\par       ScreenObject.Selected := False;
\par       \plain\f3\fs20\b if\plain\f3\fs20  RbwZoomBox1.SelectPolygon(X, Y, ScreenObject.Points) \plain\f3\fs20\b then\plain\f3\fs20 
\par       \plain\f3\fs20\b begin\plain\f3\fs20 
\par         ScreenObject.Selected := True;
\par       \plain\f3\fs20\b end\plain\f3\fs20 ;
\par     \plain\f3\fs20\b end\plain\f3\fs20 ;
\par     RbwZoomBox1.Invalidate;
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par 
\par }
1400
Scribble1400
TRBWZoomBox.SelectPolyLine method
SelectPolyLine,TRBWZoomBox;TRBWZoomBox,SelectPolyLine


rbwzoombox:001400
Writing


TRBWZoomBox_SelectPolyLine;SelectPolyLine_Method;SelectPolyLine
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.SelectPolyLine method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf3 \{linkID=%1401\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf3 \{linkID=%1402\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  SelectPolyLine(\plain\f4\fs20\b const\plain\f4\fs20  X, Y: integer; \plain\f4\fs20\b const\plain\f4\fs20  ZoomPointArray: \plain\f4\fs20\cf1\strike TZBArray\plain\f4\fs20\cf3 \{linkID=1800\}\plain\f4\fs20 ): boolean; \plain\f4\fs20\b virtual\plain\f4\fs20 ;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 SelectPolyLine returns true if the screen coordinates X and Y are close to the line defined by the series of points in ZoomPointArray. SelectPolyLine calls \plain\f3\fs20\cf1\strike SelectPoint\plain\f3\fs20\cf3 \{linkID=1390\}\plain\f3\fs20  for each \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20  in \plain\f3\fs20\cf1\strike TZBArray\plain\f3\fs20\cf3 \{linkID=1800\}\plain\f3\fs20  and then \plain\f3\fs20\cf1\strike SelectSegment\plain\f3\fs20\cf3 \{linkID=1405\}\plain\f3\fs20  for each adjacent pair of \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20 's in \plain\f3\fs20\cf1\strike TZBArray\plain\f3\fs20\cf3 \{linkID=1800\}\plain\f3\fs20  until a value of True has been returned or all members of \plain\f3\fs20\cf1\strike TZBArray\plain\f3\fs20\cf3 \{linkID=1800\}\plain\f3\fs20  have been tested.
\par 
\par }
1401
Scribble1401
SelectPolyLine method - See also




Writing



FALSE
10
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike IsPointInside method\plain\f3\fs20\cf2 \{linkID=1355\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike SelectPoint method\plain\f3\fs20\cf2 \{linkID=1390\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike SelectSegment method\plain\f3\fs20\cf2 \{linkID=1405\}
\par \plain\f3\fs20\cf1\strike SelectPolygon method\plain\f3\fs20\cf2 \{linkID=1395\}\plain\f3\fs20 
\par 
\par }
1402
Scribble1402
SelectPolyLine method - Example




Writing



FALSE
23
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b SelectPolyLine method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseUp(Sender: TObject; Button: TMouseButton;
\par   Shift: TShiftState; X, Y: Integer);
\par \plain\f3\fs20\b var\plain\f3\fs20 
\par   Index : integer;
\par   ScreenObject : TScreenObject;
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par     \plain\f3\fs20\b for\plain\f3\fs20  Index := 0 \plain\f3\fs20\b to\plain\f3\fs20  ScreenObjectList.Count -1 \plain\f3\fs20\b do\plain\f3\fs20 
\par     \plain\f3\fs20\b begin\plain\f3\fs20 
\par       ScreenObject := ScreenObjectList[Index] \plain\f3\fs20\b as\plain\f3\fs20  TScreenObject;
\par       ScreenObject.Selected := False;
\par       \plain\f3\fs20\b if\plain\f3\fs20  RbwZoomBox1.SelectPolyLine(X, Y, ScreenObject.Points) \plain\f3\fs20\b then\plain\f3\fs20 
\par       \plain\f3\fs20\b begin\plain\f3\fs20 
\par         ScreenObject.Selected := True;
\par       \plain\f3\fs20\b end\plain\f3\fs20 ;
\par     \plain\f3\fs20\b end\plain\f3\fs20 ;
\par     RbwZoomBox1.Invalidate;
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1405
Scribble1405
TRBWZoomBox.SelectSegment method
SelectSegment,TRBWZoomBox;TRBWZoomBox,SelectSegment


rbwzoombox:001405
Writing


TRBWZoomBox_SelectSegment;SelectSegment_Method;SelectSegment
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f4\fs32\cf2\b TRbwZoomBox.SelectSegment method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf1\ul See also\plain\f4\fs16\cf3 \{linkID=%1406\}\tab \plain\f4\fs16\cf1\strike Example\plain\f4\fs16\cf3 \{linkID=%1407\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf1\strike TRbwZoomBox\plain\f4\fs20\cf3 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b function\plain\f3\fs20  SelectSegment(\plain\f3\fs20\b const\plain\f3\fs20  X, Y: integer; \plain\f3\fs20\b const\plain\f3\fs20  ZoomPoint1, ZoomPoint2: \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20 ): boolean; \plain\f3\fs20\b virtual\plain\f3\fs20 ;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \plain\f4\fs20 SelectSegment will return true if the point with screen coordinates X, Y is within \plain\f4\fs20\cf1\strike SelectionWidth\plain\f4\fs20\cf3 \{linkID=1315\}\plain\f4\fs20  of the line segment defined by ZoomPoint1 and ZoomPoint2. The distance is measured either horizontally or vertically depending on whether or not the slope of the line connecting ZoomPoint1 and ZoomPoint2 is closer to the vertical or the horizontal in the screen coordinates. SelectSegment can be used to decide whether to select a particular line segment with the mouse.
\par 
\par }
1406
Scribble1406
SelectSegment method - See also




Writing



FALSE
10
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike IsPointInside method\plain\f3\fs20\cf1 \{linkID=1355\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectPoint method\plain\f3\fs20\cf1 \{linkID=1390\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectPolyLine method\plain\f3\fs20\cf1 \{linkID=1400\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SelectPolygon method\plain\f3\fs20\cf1 \{linkID=1395\}\plain\f3\fs20 
\par 
\par }
1407
Scribble1407
SelectSegment method - Example




Writing



FALSE
44
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b SelectSegment method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseUp(Sender: TObject; Button: TMouseButton;
\par   Shift: TShiftState; X, Y: Integer);
\par \plain\f3\fs20\b var\plain\f3\fs20 
\par   Index : integer;
\par   Point1, Point2 : TRbwZoomPoint;
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par     \plain\f3\fs20\b for\plain\f3\fs20  Index := 0 \plain\f3\fs20\b to\plain\f3\fs20  PointsList.Count -2 \plain\f3\fs20\b do\plain\f3\fs20 
\par     \plain\f3\fs20\b begin\plain\f3\fs20 
\par       Point1 := PointsList[Index] \plain\f3\fs20\b as\plain\f3\fs20  TRbwZoomPoint;
\par       Point2 := PointsList[Index+1] \plain\f3\fs20\b as\plain\f3\fs20  TRbwZoomPoint;
\par       \plain\f3\fs20\b if\plain\f3\fs20  RbwZoomBox1.SelectSegment (X, Y, Point1, Point2) \plain\f3\fs20\b then\plain\f3\fs20 
\par       \plain\f3\fs20\b begin\plain\f3\fs20 
\par         ShowMessage('Selected');
\par       \plain\f3\fs20\b end\plain\f3\fs20 ;
\par     \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par \pard\plain\f3\fs20\b function\plain\f3\fs20  TRbwZoomBox.SelectPolyLine(\plain\f3\fs20\b const\plain\f3\fs20  X, Y: integer;
\par   \plain\f3\fs20\b const\plain\f3\fs20  ZoomPointArray: TZBArray): boolean;
\par \plain\f3\fs20\b Var\plain\f3\fs20 
\par   Index : integer;
\par   ZoomPoint1, ZoomPoint2 :TRbwZoomPoint;
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par   result := False;
\par   \plain\f3\fs20\b for\plain\f3\fs20  Index := 0 \plain\f3\fs20\b to\plain\f3\fs20  Length(ZoomPointArray) -1 \plain\f3\fs20\b do\plain\f3\fs20 
\par   \plain\f3\fs20\b begin\plain\f3\fs20 
\par     ZoomPoint1 := ZoomPointArray[Index];
\par     result := self.SelectPoint(X, Y, ZoomPoint1);
\par     \plain\f3\fs20\b if\plain\f3\fs20  Result \plain\f3\fs20\b then\plain\f3\fs20  Exit;
\par   \plain\f3\fs20\b end\plain\f3\fs20 ;
\par 
\par   \plain\f3\fs20\b for\plain\f3\fs20  Index := 0 \plain\f3\fs20\b to\plain\f3\fs20  Length(ZoomPointArray) -2 \plain\f3\fs20\b do\plain\f3\fs20 
\par   \plain\f3\fs20\b begin\plain\f3\fs20 
\par     ZoomPoint1 := ZoomPointArray[Index];
\par     ZoomPoint2 := ZoomPointArray[Index+1];
\par     result := SelectSegment(X, Y, ZoomPoint1, ZoomPoint2);
\par     \plain\f3\fs20\b if\plain\f3\fs20  result \plain\f3\fs20\b then\plain\f3\fs20  Exit;
\par   \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \plain\f3\fs20\b end\plain\f3\fs20 ;\plain\f4\fs20 
\par }
1410
Scribble1410
TRBWZoomBox.SetZoom method
SetZoom,TRBWZoomBox;TRBWZoomBox,SetZoom


rbwzoombox:001410
Writing


TRBWZoomBox_SetZoom;SetZoom_Method;SetZoom
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.SetZoom method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  SetZoom(AZoomLevel: Extended): Extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 SetZoom changes the degree of zooming. AZoomLevel should be the degree of zooming that you wish to set. If AZoomLevel is set equal to DefaultMultiplier, this is equivalent to ZoomOut. The zoom level that is actually set will generally differ slightly from that specified by AZoomLevel. However, the displayed region will differ from that requested by one pixel or less. The actual zoom level that is set is the result of the SetZoom function.
\par 
\par }
1415
Scribble1415
TRBWZoomBox.X method
X,TRBWZoomBox;TRBWZoomBox,X


rbwzoombox:001415
Writing


TRBWZoomBox_X;X_Method;X
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.X method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1416\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf2 \{linkID=%1417\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  X(\plain\f4\fs20\b const\plain\f4\fs20  XCoord: integer): extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 X converts a screen coordinate into a real-number X coordinate.
\par 
\par }
1416
Scribble1416
X method - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike XCoord method\plain\f3\fs20\cf2 \{linkID=1420\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike Y method\plain\f3\fs20\cf2 \{linkID=1425\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike YCoord method\plain\f3\fs20\cf2 \{linkID=1430\}
\par \plain\f3\fs20 
\par }
1417
Scribble1417
X method - Example




Writing



FALSE
12
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b X method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TRbwZoomBox.MouseToCoordinates(\plain\f3\fs20\b const\plain\f3\fs20  AnXCoord, AYCoord: integer;
\par   \plain\f3\fs20\b var\plain\f3\fs20  AnX, AY: extended);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par   AnX := X(AnXCoord);
\par   AY  := Y(AYCoord);
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1420
Scribble1420
TRBWZoomBox.XCoord method
XCoord,TRBWZoomBox;TRBWZoomBox,XCoord


rbwzoombox:001420
Writing


TRBWZoomBox_XCoord;XCoord_Method;XCoord
FALSE
14
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f4\fs32\cf1\b TRbwZoomBox.XCoord method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs22\b Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf3 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b function\plain\f3\fs20  XCoord(\plain\f3\fs20\b const\plain\f3\fs20  X: extended): integer;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \plain\f4\fs20 XCoord converts a real-number X coordinate into a screen coordinate.
\par 
\par }
1421
Scribble1421
XCoord method - See also




Writing



FALSE
10
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike X method\plain\f3\fs20\cf1 \{linkID=1415\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike Y method\plain\f3\fs20\cf1 \{linkID=1425\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike YCoord method\plain\f3\fs20\cf1 \{linkID=1430\}
\par \plain\f3\fs20\cf2\strike MouseToCoordinates method\plain\f3\fs20\cf1 \{linkID=1475\}\plain\f3\fs20 
\par 
\par }
1425
Scribble1425
TRBWZoomBox.Y method
Y,TRBWZoomBox;TRBWZoomBox,Y


rbwzoombox:001425
Writing


TRBWZoomBox_Y;Y_Method;Y
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.Y method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf3\ul See also\plain\f3\fs16\cf2 \{linkID=%1426\}\tab \plain\f3\fs16\cf3\strike Example\plain\f3\fs16\cf2 \{linkID=%1427\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  Y(\plain\f4\fs20\b const\plain\f4\fs20  YCoord: integer): extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 Y converts a screen coordinate into a real-number Y coordinate.
\par 
\par }
1426
Scribble1426
Y method - See also




Writing



FALSE
10
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike X method\plain\f3\fs20\cf3 \{linkID=1415\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike XCoord method\plain\f3\fs20\cf3 \{linkID=1420\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike YCoord method\plain\f3\fs20\cf3 \{linkID=1430\}
\par \plain\f3\fs20 
\par 
\par }
1427
Scribble1427
Y method - Example




Writing



FALSE
12
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b Y method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TRbwZoomBox.MouseToCoordinates(\plain\f3\fs20\b const\plain\f3\fs20  AnXCoord, AYCoord: integer;
\par   \plain\f3\fs20\b var\plain\f3\fs20  AnX, AY: extended);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par   AnX := X(AnXCoord);
\par   AY  := Y(AYCoord);
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1430
Scribble1430
TRBWZoomBox.YCoord method
YCoord,TRBWZoomBox;TRBWZoomBox,YCoord


rbwzoombox:001430
Writing


TRBWZoomBox_YCoord;YCoord_Method;YCoord
FALSE
14
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomBox.YCoord method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs22\b Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b function\plain\f4\fs20  YCoord(\plain\f4\fs20\b const\plain\f4\fs20  Y: extended): integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 YCoord converts a real-number Y coordinate into a screen coordinate.
\par 
\par }
1431
Scribble1431
YCoord method - See also




Writing



FALSE
10
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike X method\plain\f3\fs20\cf1 \{linkID=1415\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike XCoord method\plain\f3\fs20\cf1 \{linkID=1420\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike Y method\plain\f3\fs20\cf1 \{linkID=1425\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike MouseToCoordinates method\plain\f3\fs20\cf1 \{linkID=1475\}\plain\f3\fs20 
\par 
\par }
1435
Scribble1435
TRBWZoomBox.AbortZoom method
AbortZoom,TRBWZoomBox;TRBWZoomBox,AbortZoom


rbwzoombox:001435
Writing


TRBWZoomBox_AbortZoom;AbortZoom_Method;AbortZoom
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.AbortZoom method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1436\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  AbortZoom;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 If for some reason you want to terminate a zoom operation without doing the zoom, call AbortZoom instead of FinishZoom.
\par 
\par }
1436
Scribble1436
AbortZoom method - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike BeginZoom method\plain\f3\fs20\cf2 \{linkID=1445\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike ContinueZoom method\plain\f3\fs20\cf2 \{linkID=1450\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike FinishZoom method\plain\f3\fs20\cf2 \{linkID=1460\}\plain\f3\fs20 
\par }
1440
Scribble1440
TRBWZoomBox.BeginPan method
BeginPan,TRBWZoomBox;TRBWZoomBox,BeginPan


rbwzoombox:001440
Writing


TRBWZoomBox_BeginPan;BeginPan_Method;BeginPan
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.BeginPan method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1441\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf2 \{linkID=%1442\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  BeginPan;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 BeginPan stores the current mouse and scroll bar positions and sets Panning to True. This causes scroll bars to change position to keep the current position under the mouse until \plain\f3\fs20\cf1\strike EndPan\plain\f3\fs20\cf2 \{linkID=1455\}\plain\f3\fs20  is called.
\par 
\par }
1441
Scribble1441
BeginPan method - See also




Writing



FALSE
6
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike EndPan method\plain\f3\fs20\cf3 \{linkID=1455\}\plain\f3\fs20 
\par }
1442
Scribble1442
BeginPan method - Example




Writing



FALSE
11
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b BeginPan method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseDown(Sender: TObject;
\par   Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par     RbwZoomBox1.BeginPan;
\par \plain\f3\fs20\b end\plain\f3\fs20 ;\plain\f4\fs20 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1445
Scribble1445
TRBWZoomBox.BeginZoom method
BeginZoom,TRBWZoomBox;TRBWZoomBox,BeginZoom


rbwzoombox:001445
Writing


TRBWZoomBox_BeginZoom;BeginZoom_Method;BeginZoom
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.BeginZoom method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf1 \{linkID=%1446\}\tab \plain\f3\fs16\cf2\strike Example\plain\f3\fs16\cf1 \{linkID=%1447\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  BeginZoom(X, Y: Integer);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Begin Zoom starts a zooming operation. X, and Y are the X and Y screen coordinates of the Paintbox in the TRbwZoomBox. You can get those in \plain\f3\fs20\cf2\strike OnMouseDown\plain\f3\fs20\cf1 \{linkID=1615\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1446
Scribble1446
BeginZoom method - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike AbortZoom method\plain\f3\fs20\cf1 \{linkID=1435\}\plain\f3\fs20\cf3\strike 
\par ContinueZoom method\plain\f3\fs20\cf1 \{linkID=1450\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike FinishZoom method\plain\f3\fs20\cf1 \{linkID=1460\}\plain\f3\fs20 
\par 
\par }
1447
Scribble1447
BeginZoom method - Example




Writing



FALSE
11
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b BeginZoom method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseDown(Sender: TObject;
\par   Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par     RbwZoomBox1.BeginZoom(X,Y);
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1450
Scribble1450
TRBWZoomBox.ContinueZoom method
ContinueZoom,TRBWZoomBox;TRBWZoomBox,ContinueZoom


rbwzoombox:001450
Writing


TRBWZoomBox_ContinueZoom;ContinueZoom_Method;ContinueZoom
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.ContinueZoom method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf3 \{linkID=%1451\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf3 \{linkID=%1452\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  ContinueZoom(X, Y: Integer);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 If BeginZoom has been called, and neither \plain\f3\fs20\cf1\strike AbortZoom\plain\f3\fs20\cf3 \{linkID=1435\}\plain\f3\fs20  nor \plain\f3\fs20\cf1\strike FinishZoom\plain\f3\fs20\cf3 \{linkID=1460\}\plain\f3\fs20  have yet been called, ContinueZoom will draw a shape on the paintbox. One of the corners of the shape will be at the location where the zoom began. The other will be at X, and Y where are the X and Y screen coordinates of the Paintbox in the TRbwZoomBox. You can those from OnMouseMove.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1451
Scribble1451
ContinueZoom method - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike AbortZoom method\plain\f3\fs20\cf2 \{linkID=1435\}\plain\f3\fs20\cf3\strike 
\par BeginZoom method\plain\f3\fs20\cf2 \{linkID=1445\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike FinishZoom method\plain\f3\fs20\cf2 \{linkID=1460\}\plain\f3\fs20 
\par 
\par }
1452
Scribble1452
ContinueZoom method - Example




Writing



FALSE
13
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b ContinueZoom method example
\par \plain\f3\fs20 
\par \pard\plain\f4\fs20\b procedure\plain\f4\fs20  TForm1.RbwZoomBox1MouseMove(Sender: TObject; Shift: TShiftState;
\par   X, Y: Integer);
\par \plain\f4\fs20\b var\plain\f4\fs20 
\par   RealX, RealY : extended;
\par \plain\f4\fs20\b begin\plain\f4\fs20 
\par     RbwZoomBox1.ContinueZoom(X,Y)
\par \plain\f4\fs20\b end\plain\f4\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1455
Scribble1455
TRBWZoomBox.EndPan method
EndPan,TRBWZoomBox;TRBWZoomBox,EndPan


rbwzoombox:001455
Writing


TRBWZoomBox_EndPan;EndPan_Method;EndPan
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.EndPan method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1456\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf2 \{linkID=%1457\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  EndPan;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 This sets Panning to False so that the scroll bars cease to change position to keep the current position under the mouse
\par 
\par }
1456
Scribble1456
EndPan method - See also




Writing



FALSE
6
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike BeginPan method\plain\f3\fs20\cf3 \{linkID=1440\}\plain\f3\fs20 
\par }
1457
Scribble1457
EndPan method - Example




Writing



FALSE
11
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b EndPan method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseUp(Sender: TObject; Button: TMouseButton;
\par   Shift: TShiftState; X, Y: Integer);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par     RbwZoomBox1.EndPan;
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1460
Scribble1460
TRBWZoomBox.FinishZoom method
FinishZoom,TRBWZoomBox;TRBWZoomBox,FinishZoom


rbwzoombox:001460
Writing


TRBWZoomBox_FinishZoom;FinishZoom_Method;FinishZoom
FALSE
14
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.FinishZoom method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf1\ul See also\plain\f4\fs16\cf2 \{linkID=%1461\}\tab \plain\f4\fs16\cf1\strike Example\plain\f4\fs16\cf2 \{linkID=%1462\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf1\strike TRbwZoomBox\plain\f4\fs20\cf2 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b procedure\plain\f3\fs20  FinishZoom(X, Y: Integer);
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \plain\f4\fs20 FinishZoom terminates a zoom operation. X, and Y are the X and Y screen coordinates of the Paintbox in the TRbwZoomBox. You can get those in OnMouseUp. FinishZoom sets the zoom level so that the area outlined by the X, and Y parameters in BeginZoom and the X, and Y parameters in FinishZoom are displayed in the TRbwZoomBox.
\par }
1461
Scribble1461
FinishZoom method - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike AbortZoom method\plain\f3\fs20\cf3 \{linkID=1435\}\plain\f3\fs20\cf1\strike 
\par BeginZoom method\plain\f3\fs20\cf3 \{linkID=1445\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike ContinueZoom method\plain\f3\fs20\cf3 \{linkID=1450\}\plain\f3\fs20 
\par 
\par }
1462
Scribble1462
FinishZoom method - Example




Writing



FALSE
11
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b FinishZoom method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseUp(Sender: TObject; Button: TMouseButton;
\par   Shift: TShiftState; X, Y: Integer);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par     RbwZoomBox1.FinishZoom(X,Y);
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1465
Scribble1465
TRBWZoomBox.GetMinMax method
GetMinMax,TRBWZoomBox;TRBWZoomBox,GetMinMax


rbwzoombox:001465
Writing


TRBWZoomBox_GetMinMax;GetMinMax_Method;GetMinMax
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.GetMinMax method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf2\ul See also\plain\f4\fs16\cf1 \{linkID=%1466\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b procedure\plain\f3\fs20  GetMinMax;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 GetMinMax determines the minimum and maximum coordinates of the currently existing \plain\f4\fs20\cf2\strike TRbwZoomPoint\plain\f4\fs20\cf1 \{linkID=1700\}\plain\f4\fs20 s associated with the \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20  for which \plain\f4\fs20\cf2\strike UseForZoomOut\plain\f4\fs20\cf1 \{linkID=1710\}\plain\f4\fs20  is true and sets the \plain\f4\fs20\cf2\strike DefaultMultiplier\plain\f4\fs20\cf1 \{linkID=1115\}\plain\f4\fs20  to a value sufficient to display all the \plain\f4\fs20\cf2\strike TRbwZoomPoints\plain\f4\fs20\cf1 \{linkID=1700\}\plain\f4\fs20  and still leave a blank area equal to the \plain\f4\fs20\cf2\strike TopMargin\plain\f4\fs20\cf1 \{linkID=1325\}\plain\f4\fs20 , \plain\f4\fs20\cf2\strike BottomMargin\plain\f4\fs20\cf1 \{linkID=1110\}\plain\f4\fs20 , \plain\f4\fs20\cf2\strike LeftMargin\plain\f4\fs20\cf1 \{linkID=1120\}\plain\f4\fs20  and \plain\f4\fs20\cf2\strike RightMargin\plain\f4\fs20\cf1 \{linkID=1290\}\plain\f4\fs20  pixels on their respective sides.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1466
Scribble1466
GetMinMax method - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike SetRange method\plain\f3\fs20\cf2 \{linkID=1540\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike SetXRange method\plain\f3\fs20\cf2 \{linkID=1545\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike SetYRange method\plain\f3\fs20\cf2 \{linkID=1550\}\plain\f3\fs20 
\par }
1470
Scribble1470
TRBWZoomBox.Invalidate method
Invalidate,TRBWZoomBox;TRBWZoomBox,Invalidate


rbwzoombox:001470
Writing


TRBWZoomBox_Invalidate;Invalidate_Method;Invalidate
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.Invalidate method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  Invalidate; \plain\f4\fs20\b override\plain\f4\fs20 ;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 Invalidate calls the inherited Invalidate and then calls Invalidate of the embedded TPaintBox.
\par 
\par }
1475
Scribble1475
TRBWZoomBox.MouseToCoordinates method
MouseToCoordinates,TRBWZoomBox;TRBWZoomBox,MouseToCoordinates


rbwzoombox:001475
Writing


TRBWZoomBox_MouseToCoordinates;MouseToCoordinates_Method;MouseToCoordinates
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.MouseToCoordinates method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1476\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf2 \{linkID=%1477\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  MouseToCoordinates(\plain\f4\fs20\b const\plain\f4\fs20  AnXCoord, AYCoord: integer; \plain\f4\fs20\b var\plain\f4\fs20  AnX, AY: extended);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 MouseToCoordinates changes the screen coordinates AnXCoord and AYCoord to the floating point coordinates AnX and AY.
\par 
\par }
1476
Scribble1476
MouseToCoordinates method - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike XCoord property\plain\f3\fs20\cf2 \{linkID=1720\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike YCoord property\plain\f3\fs20\cf2 \{linkID=1730\}\plain\f3\fs20 
\par }
1477
Scribble1477
MouseToCoordinates method - Example




Writing



FALSE
14
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b MouseToCoordinates method example
\par \plain\f4\fs20 
\par \pard\plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.RbwZoomBox1MouseMove(Sender: TObject; Shift: TShiftState;
\par   X, Y: Integer);
\par \plain\f3\fs20\b var\plain\f3\fs20 
\par   RealX, RealY : extended;
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par   RbwZoomBox1.MouseToCoordinates(X, Y, RealX, RealY);
\par   StatusBar1.SimpleText := Format('X: %g, Y: %g', [RealX, RealY]);
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1480
Scribble1480
TRBWZoomBox.PBBeginDrag method
PBBeginDrag,TRBWZoomBox;TRBWZoomBox,PBBeginDrag


rbwzoombox:001480
Writing


TRBWZoomBox_PBBeginDrag;PBBeginDrag_Method;PBBeginDrag
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.PBBeginDrag method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs22\b Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b procedure\plain\f3\fs20  PBBeginDrag(Immediate: Boolean; Threshold: Integer = - 1);
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 BeginDrag of the embedded paintbox.
\par See \plain\f4\fs20\cf2\strike TControl\plain\f4\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf2\strike BeginDrag\plain\f4\fs20\cf1 \{linkDelphi=BeginDrag_Method\}\plain\f4\fs20  
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1485
Scribble1485
TRBWZoomBox.PBBringToFront method
PBBringToFront,TRBWZoomBox;TRBWZoomBox,PBBringToFront


rbwzoombox:001485
Writing


TRBWZoomBox_PBBringToFront;PBBringToFront_Method;PBBringToFront
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.PBBringToFront method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  PBBringToFront;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 BringToFront of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike BringToFront\plain\f3\fs20\cf1 \{linkDelphi=BringToFront_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1490
Scribble1490
TRBWZoomBox.PBDragDrop method
PBDragDrop,TRBWZoomBox;TRBWZoomBox,PBDragDrop


rbwzoombox:001490
Writing


TRBWZoomBox_PBDragDrop;PBDragDrop_Method;PBDragDrop
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.PBDragDrop method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  PBDragDrop(Source: TObject; X, Y: Integer);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 DragDrop of the embedded paintbox.
\par See \plain\f3\fs20\cf1\strike TControl\plain\f3\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf1\strike DragDrop\plain\f3\fs20\cf2 \{linkDelphi=DragDrop_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1495
Scribble1495
TRBWZoomBox.PBEndDrag method
PBEndDrag,TRBWZoomBox;TRBWZoomBox,PBEndDrag


rbwzoombox:001495
Writing


TRBWZoomBox_PBEndDrag;PBEndDrag_Method;PBEndDrag
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.PBEndDrag method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b procedure\plain\f3\fs20  PBEndDrag(Drop: Boolean);
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 EndDrag of the embedded paintbox.
\par See \plain\f4\fs20\cf2\strike TControl\plain\f4\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf2\strike EndDrag\plain\f4\fs20\cf1 \{linkDelphi=EndDrag_Method\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1500
Scribble1500
TRBWZoomBox.PBHide method
PBHide,TRBWZoomBox;TRBWZoomBox,PBHide


rbwzoombox:001500
Writing


TRBWZoomBox_PBHide;PBHide_Method;PBHide
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.PBHide method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  PBHide;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Hide of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Hide\plain\f3\fs20\cf1 \{linkDelphi=Hide_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1505
Scribble1505
TRBWZoomBox.PbRefresh method
PbRefresh,TRBWZoomBox;TRBWZoomBox,PbRefresh


rbwzoombox:001505
Writing


TRBWZoomBox_PbRefresh;PbRefresh_Method;PbRefresh
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.PbRefresh method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  PbRefresh;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Refresh of the embedded paintbox.
\par See \plain\f3\fs20\cf1\strike TControl\plain\f3\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf1\strike Refresh\plain\f3\fs20\cf2 \{linkDelphi=Refresh_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1510
Scribble1510
TRBWZoomBox.PBRepaint method
PBRepaint,TRBWZoomBox;TRBWZoomBox,PBRepaint


rbwzoombox:001510
Writing


TRBWZoomBox_PBRepaint;PBRepaint_Method;PBRepaint
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.PBRepaint method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b procedure\plain\f3\fs20  PBRepaint;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 Repaint of the embedded paintbox.
\par See \plain\f4\fs20\cf2\strike TControl\plain\f4\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf2\strike Repaint\plain\f4\fs20\cf1 \{linkDelphi=Repaint_Method\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1515
Scribble1515
TRBWZoomBox.PBSendToBack method
PBSendToBack,TRBWZoomBox;TRBWZoomBox,PBSendToBack


rbwzoombox:001515
Writing


TRBWZoomBox_PBSendToBack;PBSendToBack_Method;PBSendToBack
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.PBSendToBack method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  PBSendToBack;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 SendToBack of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike SendToBack\plain\f3\fs20\cf1 \{linkDelphi=SendToBack_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1520
Scribble1520
TRBWZoomBox.PBSetBounds method
PBSetBounds,TRBWZoomBox;TRBWZoomBox,PBSetBounds


rbwzoombox:001520
Writing


TRBWZoomBox_PBSetBounds;PBSetBounds_Method;PBSetBounds
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.PBSetBounds method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  PBSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 SetBounds of the embedded paintbox.
\par See \plain\f3\fs20\cf1\strike TControl\plain\f3\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf1\strike SetBounds\plain\f3\fs20\cf2 \{linkDelphi=SetBounds_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1525
Scribble1525
TRBWZoomBox.PBSetTextBuf method
PBSetTextBuf,TRBWZoomBox;TRBWZoomBox,PBSetTextBuf


rbwzoombox:001525
Writing


TRBWZoomBox_PBSetTextBuf;PBSetTextBuf_Method;PBSetTextBuf
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.PBSetTextBuf method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b procedure\plain\f3\fs20  PBSetTextBuf(Buffer: PChar);
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 SetTextBuf of the embedded paintbox.
\par See \plain\f4\fs20\cf2\strike TControl\plain\f4\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf2\strike SetTextBuf\plain\f4\fs20\cf1 \{linkDelphi=SetTextBuf_Method\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1530
Scribble1530
TRBWZoomBox.PBShow method
PBShow,TRBWZoomBox;TRBWZoomBox,PBShow


rbwzoombox:001530
Writing


TRBWZoomBox_PBShow;PBShow_Method;PBShow
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.PBShow method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  PBShow;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Show of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike Show\plain\f3\fs20\cf1 \{linkDelphi=Show_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1535
Scribble1535
TRBWZoomBox.PBUpdate method
PBUpdate,TRBWZoomBox;TRBWZoomBox,PBUpdate


rbwzoombox:001535
Writing


TRBWZoomBox_PBUpdate;PBUpdate_Method;PBUpdate
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.PBUpdate method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  PBUpdate;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Update of the embedded paintbox.
\par See \plain\f3\fs20\cf1\strike TControl\plain\f3\fs20\cf2 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf1\strike Update\plain\f3\fs20\cf2 \{linkDelphi=Update_Method\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1540
Scribble1540
TRBWZoomBox.SetRange method
SetRange,TRBWZoomBox;TRBWZoomBox,SetRange


rbwzoombox:001540
Writing


TRBWZoomBox_SetRange;SetRange_Method;SetRange
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.SetRange method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf1 \{linkID=%1541\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  SetRange(MinimumX, MaximumX, MinimumY, MaximumY: Extended);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 SetRange sets the minimum and maximum real-number coordinates of the \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20  and sets the \plain\f3\fs20\cf2\strike Multiplier\plain\f3\fs20\cf1 \{linkID=1145\}\plain\f3\fs20  to a value sufficient all values within that range and still leave a blank area equal to the \plain\f3\fs20\cf2\strike TopMargin\plain\f3\fs20\cf1 \{linkID=1325\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike BottomMargin\plain\f3\fs20\cf1 \{linkID=1110\}\plain\f3\fs20 , \plain\f3\fs20\cf2\strike LeftMargin\plain\f3\fs20\cf1 \{linkID=1120\}\plain\f3\fs20  and \plain\f3\fs20\cf2\strike RightMargin\plain\f3\fs20\cf1 \{linkID=1290\}\plain\f3\fs20  pixels on their respective sides.
\par 
\par }
1541
Scribble1541
SetRange method - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike GetMinMax method\plain\f3\fs20\cf3 \{linkID=1465\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SetXRange method\plain\f3\fs20\cf3 \{linkID=1545\}
\par \plain\f3\fs20\cf2\strike SetYRange method\plain\f3\fs20\cf3 \{linkID=1550\}\plain\f3\fs20\cf2\strike 
\par }
1545
Scribble1545
TRBWZoomBox.SetXRange method
SetXRange,TRBWZoomBox;TRBWZoomBox,SetXRange


rbwzoombox:001545
Writing


TRBWZoomBox_SetXRange;SetXRange_Method;SetXRange
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.SetXRange method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf3 \{linkID=%1546\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  SetXRange(MinimumX, MaximumX: Extended);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 SetRange sets the minimum and maximum real-number coordinates of the X-axis of the \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20  and sets the \plain\f3\fs20\cf1\strike Multiplier\plain\f3\fs20\cf3 \{linkID=1145\}\plain\f3\fs20  to a value sufficient all values within that range and still leave a blank area equal to the \plain\f3\fs20\cf1\strike TopMargin\plain\f3\fs20\cf3 \{linkID=1325\}\plain\f3\fs20 , \plain\f3\fs20\cf1\strike BottomMargin\plain\f3\fs20\cf3 \{linkID=1110\}\plain\f3\fs20 , \plain\f3\fs20\cf1\strike LeftMargin\plain\f3\fs20\cf3 \{linkID=1120\}\plain\f3\fs20  and \plain\f3\fs20\cf1\strike RightMargin\plain\f3\fs20\cf3 \{linkID=1290\}\plain\f3\fs20  pixels on their respective sides.
\par 
\par }
1546
Scribble1546
SetXRange method - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike GetMinMax method\plain\f3\fs20\cf3 \{linkID=1465\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike SetRange method\plain\f3\fs20\cf3 \{linkID=1540\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike SetYRange method\plain\f3\fs20\cf3 \{linkID=1550\}\plain\f3\fs20\cf1\strike 
\par \plain\f3\fs20 
\par }
1550
Scribble1550
TRBWZoomBox.SetYRange method
SetYRange,TRBWZoomBox;TRBWZoomBox,SetYRange


rbwzoombox:001550
Writing


TRBWZoomBox_SetYRange;SetYRange_Method;SetYRange
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\plain\f4\fs32\cf1\b TRbwZoomBox.SetYRange method
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf2\ul See also\plain\f4\fs16\cf3 \{linkID=%1551\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf3 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b procedure\plain\f3\fs20  SetYRange(MinimumY, MaximumY: Extended);
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \plain\f4\fs20 SetRange sets the minimum and maximum real-number coordinates of the Y-axis of the \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf3 \{linkID=1100\}\plain\f4\fs20  and sets the \plain\f4\fs20\cf2\strike Multiplier\plain\f4\fs20\cf3 \{linkID=1145\}\plain\f4\fs20  to a value sufficient all values within that range and still leave a blank area equal to the \plain\f4\fs20\cf2\strike TopMargin\plain\f4\fs20\cf3 \{linkID=1325\}\plain\f4\fs20 , \plain\f4\fs20\cf2\strike BottomMargin\plain\f4\fs20\cf3 \{linkID=1110\}\plain\f4\fs20 , \plain\f4\fs20\cf2\strike LeftMargin\plain\f4\fs20\cf3 \{linkID=1120\}\plain\f4\fs20  and \plain\f4\fs20\cf2\strike RightMargin\plain\f4\fs20\cf3 \{linkID=1290\}\plain\f4\fs20  pixels on their respective sides.
\par 
\par }
1551
Scribble1551
SetYRange method - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike GetMinMax method\plain\f3\fs20\cf1 \{linkID=1465\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SetRange method\plain\f3\fs20\cf1 \{linkID=1540\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike SetXRange method\plain\f3\fs20\cf1 \{linkID=1545\}
\par \plain\f3\fs20 
\par }
1555
Scribble1555
TRBWZoomBox.ZoomBy method
ZoomBy,TRBWZoomBox;TRBWZoomBox,ZoomBy


rbwzoombox:001555
Writing


TRBWZoomBox_ZoomBy;ZoomBy_Method;ZoomBy
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.ZoomBy method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf3\ul See also\plain\f3\fs16\cf1 \{linkID=%1556\}\tab \plain\f3\fs16\cf3\strike Example\plain\f3\fs16\cf1 \{linkID=%1557\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  ZoomBy(ZoomFactor: Extended);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 ZoomBy changes the level of zoom by a factor specified by ZoomFactor. The real-number coordinates are not changed by ZoomBy.
\par 
\par }
1556
Scribble1556
ZoomBy method - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike ZoomByAt method\plain\f3\fs20\cf2 \{linkID=1560\}
\par \plain\f3\fs20\cf1\strike ZoomOut method\plain\f3\fs20\cf2 \{linkID=1565\}\plain\f3\fs20 
\par }
1557
Scribble1557
ZoomBy method - Example




Writing



FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b ZoomBy method example
\par \pard\plain\f3\fs20 
\par \plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.sbZoomInClick(Sender: TObject);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par   RbwZoomBox1.ZoomBy(2);
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par 
\par \plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.sbZoomOutClick(Sender: TObject);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par   RbwZoomBox1.ZoomBy(0.5);
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1560
Scribble1560
TRBWZoomBox.ZoomByAt method
ZoomByAt,TRBWZoomBox;TRBWZoomBox,ZoomByAt


rbwzoombox:001560
Writing


TRBWZoomBox_ZoomByAt;ZoomByAt_Method;ZoomByAt
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomBox.ZoomByAt method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf3 \{linkID=%1561\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  ZoomByAt(ZoomFactor, X, Y: Extended);
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 ZoomByAt calls ZoomBy and passes it ZoomFactor. It then centers the position indicated by the floating point coordinates X and Y.
\par 
\par }
1561
Scribble1561
ZoomByAt method - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike ZoomBy method\plain\f3\fs20\cf3 \{linkID=1555\}
\par \plain\f3\fs20\cf1\strike ZoomOut method\plain\f3\fs20\cf3 \{linkID=1565\}\plain\f3\fs20 
\par }
1565
Scribble1565
TRBWZoomBox.ZoomOut method
ZoomOut,TRBWZoomBox;TRBWZoomBox,ZoomOut


rbwzoombox:001565
Writing


TRBWZoomBox_ZoomOut;ZoomOut_Method;ZoomOut
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.ZoomOut method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1566\}\tab \plain\f3\fs16\cf1\strike Example\plain\f3\fs16\cf2 \{linkID=%1567\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomBox\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b procedure\plain\f4\fs20  ZoomOut;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 ZoomOut sets the zoom level so that all the \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f3\fs20 's that had been created at the time of the previous call to \plain\f3\fs20\cf1\strike GetMinMax\plain\f3\fs20\cf2 \{linkID=1465\}\plain\f3\fs20  and for which UseForZoomOut was true will be displayed with blank areas at the edges with a width equal to \plain\f3\fs20\cf1\strike TopMargin\plain\f3\fs20\cf2 \{linkID=1325\}\plain\f3\fs20 , \plain\f3\fs20\cf1\strike BottomMargin\plain\f3\fs20\cf2 \{linkID=1110\}\plain\f3\fs20 , \plain\f3\fs20\cf1\strike LeftMargin\plain\f3\fs20\cf2 \{linkID=1120\}\plain\f3\fs20  and \plain\f3\fs20\cf1\strike RightMargin\plain\f3\fs20\cf2 \{linkID=1290\}\plain\f3\fs20 .
\par Public declarations
\par 
\par }
1566
Scribble1566
ZoomOut method - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike ZoomBy method\plain\f3\fs20\cf1 \{linkID=1555\}
\par \plain\f3\fs20\cf3\strike ZoomByAt method\plain\f3\fs20\cf1 \{linkID=1560\}\plain\f3\fs20 
\par }
1567
Scribble1567
ZoomOut method - Example




Writing



FALSE
10
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20\cf1\b ZoomOut method example
\par \pard\plain\f3\fs20 
\par \plain\f3\fs20\b procedure\plain\f3\fs20  TForm1.sbZoomAllTheWayOutClick(Sender: TObject);
\par \plain\f3\fs20\b begin\plain\f3\fs20 
\par   RbwZoomBox1.ZoomOut;
\par \plain\f3\fs20\b end\plain\f3\fs20 ;
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1570
Scribble1570
TRBWZoomBox.OnClick event
OnClick,TRBWZoomBox;TRBWZoomBox,OnClick
event_TRBWZoomBoxOnClick

rbwzoombox:001570
Writing


TRBWZoomBox_OnClick;OnClick_Event;OnClick
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnClick event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnClick: TNotifyEvent;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 OnClick of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnClick\plain\f3\fs20\cf1 \{linkDelphi=OnClick_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1575
Scribble1575
TRBWZoomBox.OnDblClick event
OnDblClick,TRBWZoomBox;TRBWZoomBox,OnDblClick
event_TRBWZoomBoxOnDblClick

rbwzoombox:001575
Writing


TRBWZoomBox_OnDblClick;OnDblClick_Event;OnDblClick
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnDblClick event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnDblClick: TNotifyEvent;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 OnDblClick of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnDblClick\plain\f3\fs20\cf1 \{linkDelphi=OnDblClick_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1580
Scribble1580
TRBWZoomBox.OnDragDrop event
OnDragDrop,TRBWZoomBox;TRBWZoomBox,OnDragDrop
event_TRBWZoomBoxOnDragDrop

rbwzoombox:001580
Writing


TRBWZoomBox_OnDragDrop;OnDragDrop_Event;OnDragDrop
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.OnDragDrop event
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b property\plain\f3\fs20  OnDragDrop: TDragDropEvent;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 OnDragDrop of the embedded paintbox.
\par See \plain\f4\fs20\cf2\strike TControl\plain\f4\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf2\strike OnDragDrop\plain\f4\fs20\cf1 \{linkDelphi=OnDragDrop_Event\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1585
Scribble1585
TRBWZoomBox.OnDragOver event
OnDragOver,TRBWZoomBox;TRBWZoomBox,OnDragOver
event_TRBWZoomBoxOnDragOver

rbwzoombox:001585
Writing


TRBWZoomBox_OnDragOver;OnDragOver_Event;OnDragOver
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnDragOver event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnDragOver: TDragOverEvent;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 OnDragOver of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnDragOver\plain\f3\fs20\cf1 \{linkDelphi=OnDragOver_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1590
Scribble1590
TRBWZoomBox.OnEndDrag event
OnEndDrag,TRBWZoomBox;TRBWZoomBox,OnEndDrag
event_TRBWZoomBoxOnEndDrag

rbwzoombox:001590
Writing


TRBWZoomBox_OnEndDrag;OnEndDrag_Event;OnEndDrag
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnEndDrag event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnEndDrag: TEndDragEvent;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 OnEndDrag of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnEndDrag\plain\f3\fs20\cf1 \{linkDelphi=OnEndDrag_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1600
Scribble1600
TRBWZoomBox.OnKeyDown event
OnKeyDown,TRBWZoomBox;TRBWZoomBox,OnKeyDown
event_TRBWZoomBoxOnKeyDown

rbwzoombox:001600
Writing


TRBWZoomBox_OnKeyDown;OnKeyDown_Event;OnKeyDown
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.OnKeyDown event
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf2\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b property\plain\f3\fs20  OnKeyDown;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 See \plain\f4\fs20\cf2\strike TWinControl\plain\f4\fs20\cf1 \{linkDelphi=TWinControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf2\strike OnKeyDown\plain\f4\fs20\cf1 \{linkDelphi=OnKeyDown_Event\}\plain\f4\fs20 .
\par 
\par You may have to call SetFocus before the TRbwZoomBox will respond to the OnKeyDown event.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1605
Scribble1605
TRBWZoomBox.OnKeyPress event
OnKeyPress,TRBWZoomBox;TRBWZoomBox,OnKeyPress
event_TRBWZoomBoxOnKeyPress

rbwzoombox:001605
Writing


TRBWZoomBox_OnKeyPress;OnKeyPress_Event;OnKeyPress
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnKeyPress event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnKeyPress;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 See \plain\f3\fs20\cf2\strike TWinControl\plain\f3\fs20\cf1 \{linkDelphi=TWinControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnKeyPress\plain\f3\fs20\cf1 \{linkDelphi=OnKeyPress_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par \pard\plain\f3\fs20 You may have to call SetFocus before the TRbwZoomBox will respond to the OnKeyPress event.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1610
Scribble1610
TRBWZoomBox.OnKeyUp event
OnKeyUp,TRBWZoomBox;TRBWZoomBox,OnKeyUp
event_TRBWZoomBoxOnKeyUp

rbwzoombox:001610
Writing


TRBWZoomBox_OnKeyUp;OnKeyUp_Event;OnKeyUp
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnKeyUp event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnKeyUp;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 See \plain\f3\fs20\cf2\strike TWinControl\plain\f3\fs20\cf1 \{linkDelphi=TWinControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnKeyUp\plain\f3\fs20\cf1 \{linkDelphi=OnKeyUp_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par \pard\plain\f3\fs20 You may have to call SetFocus before the TRbwZoomBox will respond to the OnKeyUp event.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1615
Scribble1615
TRBWZoomBox.OnMouseDown event
OnMouseDown,TRBWZoomBox;TRBWZoomBox,OnMouseDown
event_TRBWZoomBoxOnMouseDown

rbwzoombox:001615
Writing


TRBWZoomBox_OnMouseDown;OnMouseDown_Event;OnMouseDown
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnMouseDown event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf1 \{linkID=%1616\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnMouseDown: TMouseEvent;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 OnMouseDown of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnMouseDown\plain\f3\fs20\cf1 \{linkDelphi=OnMouseDown_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1616
Scribble1616
OnMouseDown event - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike OnMouseMove event\plain\f3\fs20\cf3 \{linkID=1620\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike OnMouseUp event\plain\f3\fs20\cf3 \{linkID=1625\}\plain\f3\fs20 
\par }
1620
Scribble1620
TRBWZoomBox.OnMouseMove event
OnMouseMove,TRBWZoomBox;TRBWZoomBox,OnMouseMove
event_TRBWZoomBoxOnMouseMove

rbwzoombox:001620
Writing


TRBWZoomBox_OnMouseMove;OnMouseMove_Event;OnMouseMove
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\plain\f4\fs32\cf2\b TRbwZoomBox.OnMouseMove event
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf3\ul See also\plain\f4\fs16\cf1 \{linkID=%1621\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf3\strike TRbwZoomBox\plain\f4\fs20\cf1 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b property\plain\f3\fs20  OnMouseMove: TMouseMoveEvent;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 OnMouseMove of the embedded paintbox.
\par See \plain\f4\fs20\cf3\strike TControl\plain\f4\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf3\strike OnMouseMove\plain\f4\fs20\cf1 \{linkDelphi=OnMouseMove_Event\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1621
Scribble1621
OnMouseMove event - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike OnMouseDown event\plain\f3\fs20\cf1 \{linkID=1615\}\plain\f3\fs20\cf3\strike 
\par OnMouseUp event\plain\f3\fs20\cf1 \{linkID=1625\}\plain\f3\fs20 
\par 
\par }
1625
Scribble1625
TRBWZoomBox.OnMouseUp event
OnMouseUp,TRBWZoomBox;TRBWZoomBox,OnMouseUp
event_TRBWZoomBoxOnMouseUp

rbwzoombox:001625
Writing


TRBWZoomBox_OnMouseUp;OnMouseUp_Event;OnMouseUp
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnMouseUp event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf2\ul See also\plain\f3\fs16\cf1 \{linkID=%1626\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnMouseUp: TMouseEvent;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 OnMouseUp of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnMouseUp\plain\f3\fs20\cf1 \{linkDelphi=OnMouseUp_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1626
Scribble1626
OnMouseUp event - See also




Writing



FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike OnMouseDown event\plain\f3\fs20\cf3 \{linkID=1615\}\plain\f3\fs20\cf1\strike 
\par OnMouseMove event\plain\f3\fs20\cf3 \{linkID=1620\}\plain\f3\fs20 
\par 
\par }
1630
Scribble1630
TRBWZoomBox.OnPaint event
OnPaint,TRBWZoomBox;TRBWZoomBox,OnPaint
event_TRBWZoomBoxOnPaint

rbwzoombox:001630
Writing


TRBWZoomBox_OnPaint;OnPaint_Event;OnPaint
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomBox.OnPaint event
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs22\b Applies to
\par \plain\f4\fs20\cf1\strike TRbwZoomBox\plain\f4\fs20\cf2 \{linkID=1100\}\plain\f4\fs20\cf0  component
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b property\plain\f3\fs20  OnPaint: TNotifyEvent;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f4\fs20 OnPaint of the embedded paintbox.
\par See \plain\f4\fs20\cf1\strike TPaintBox\plain\f4\fs20\cf2 \{linkDelphi=TPaintBox_Object\}\plain\f4\fs20 .\plain\f4\fs20\cf1\strike OnPaint\plain\f4\fs20\cf2 \{linkDelphi=OnPaint_Event\}\plain\f4\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
1635
Scribble1635
TRBWZoomBox.OnStartDrag event
OnStartDrag,TRBWZoomBox;TRBWZoomBox,OnStartDrag
event_TRBWZoomBoxOnStartDrag

rbwzoombox:001635
Writing


TRBWZoomBox_OnStartDrag;OnStartDrag_Event;OnStartDrag
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomBox.OnStartDrag event
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf2\strike TRbwZoomBox\plain\f3\fs20\cf1 \{linkID=1100\}\plain\f3\fs20\cf0  component
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  OnStartDrag: TStartDragEvent;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 OnStartDrag of the embedded paintbox.
\par See \plain\f3\fs20\cf2\strike TControl\plain\f3\fs20\cf1 \{linkDelphi=TControl_Object\}\plain\f3\fs20 .\plain\f3\fs20\cf2\strike OnStartDrag\plain\f3\fs20\cf1 \{linkDelphi=OnStartDrag_Event\}\plain\f3\fs20 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1700
Scribble1700
TRBWZoomPoint class
TRBWZoomPoint
class_TRBWZoomPoint

rbwzoombox:001700
Writing


TRBWZoomPoint_Object;TRBWZoomPoint
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b TRbwZoomPoint class
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1701\}\tab \plain\f3\fs16\cf1\strike Properties\plain\f3\fs16\cf2 \{linkID=1702>sidebar\}\tab \plain\f3\fs16\cf1\strike Methods\plain\f3\fs16\cf2 \{linkID=1703>sidebar\}\tab \plain\f3\fs16\cf1\strike Tasks\plain\f3\fs16\cf2 \{linkID=%1705\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Unit
\par \plain\f3\fs20\cf1\strike RbwZoomBox\plain\f3\fs20\cf2 \{linkID=1000\}\plain\f3\fs20\cf0 
\par 
\par \plain\f3\fs22\b Declaration\plain\f3\fs20\cf0 
\par \pard\plain\f4\fs20 TRbwZoomPoint = \plain\f4\fs20\b class\plain\f4\fs20 (TObject)\plain\f3\fs22\b 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs22\b 
\par Description
\par \pard\plain\f3\fs20 TRbwZoomPoint stores real-number X and Y coordinates which it can then convert to screen coordinates.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1701
Scribble1701
TRBWZoomPoint class - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike TRbwZoomBox component\plain\f3\fs20\cf2 \{linkID=1100\}\plain\f3\fs20 
\par 
\par }
1702
Scribble1702
TRBWZoomPoint - Properties




Writing
sidebar


FALSE
14
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b Properties
\par \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700>main\}\{keepn\}
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\plain\f3\fs20\cf0  Run-time only\tab \plain\f3\fs20\cf2 \{bmct key.bmp\}\plain\f3\fs20\cf0  Key properties
\par \pard\tx200\tx640\plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike UseForZoomOut\plain\f3\fs20\cf2 \{linkID=1710>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike X\plain\f3\fs20\cf2 \{linkID=1715>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike XCoord\plain\f3\fs20\cf2 \{linkID=1720>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike Y\plain\f3\fs20\cf2 \{linkID=1725>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \{bmct key.bmp\}\tab \plain\f3\fs20\cf3\strike YCoord\plain\f3\fs20\cf2 \{linkID=1730>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20\cf2 \{bmct runtime.bmp\}\tab \plain\f3\fs20\cf0 \tab \plain\f3\fs20\cf3\strike ZoomBox\plain\f3\fs20\cf2 \{linkID=1735>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20 
\par }
1703
Scribble1703
TRBWZoomPoint - Methods




Writing
sidebar


FALSE
8
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\tx200\tx640\plain\f3\fs20\cf1\b Methods
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700>main\}\{keepn\}
\par \pard\tx200\tx640\plain\f3\fs20 
\par \plain\f3\fs20\cf0 \tab \tab \plain\f3\fs20\cf2\strike Create\plain\f3\fs20\cf3 \{linkID=1740>main\}\plain\f3\fs20\cf0 
\par \plain\f3\fs20 
\par }
1705
Scribble1705
About the TRBWZoomPoint class
TRBWZoomPoint class


rbwzoombox:001705
Writing



FALSE
12
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f3\fs32\cf3\b About the TRbwZoomPoint class
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf2 \{linkID=%1706\}\tab \plain\f3\fs16\cf1\strike TRbwZoomPoint reference\plain\f3\fs16\cf2 \{linkID=%1700\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Purpose\plain\f3\fs20 
\par TRbwZoomPoint stores real-number X and Y coordinates which it can then convert to screen coordinates.
\par 
\par \plain\f3\fs22\b Tasks
\par \plain\f3\fs20 Assign value to either the X and Y real-number properties or the XCoord and YCoord properties to assign the location of the point.
\par 
\par }
1706
Scribble1706
About the TRBWZoomPoint class - See also




Writing



FALSE
7
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike TRbwZoomBox component\plain\f3\fs20\cf3 \{linkID=1100\}\plain\f3\fs20 
\par 
\par }
1710
Scribble1710
TRBWZoomPoint.UseForZoomOut property
UseForZoomOut,TRBWZoomPoint;TRBWZoomPoint,UseForZoomOut
prop_TRBWZoomPointUseForZoomOut

rbwzoombox:001710
Writing


TRBWZoomPoint_UseForZoomOut;UseForZoomOut_Property;UseForZoomOut
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomPoint.UseForZoomOut property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20\cf0  class
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  UseForZoomOut: boolean;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 If UseForZoomOut is true, when \plain\f3\fs20\cf1\strike GetMinMax\plain\f3\fs20\cf3 \{linkID=1465\}\plain\f3\fs20  is performed on the TRbwZoomBox, the \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20  will be used to determine the range of points that should be displayed.
\par Run-time only
\par 
\par }
1715
Scribble1715
TRBWZoomPoint.X property
X,TRBWZoomPoint;TRBWZoomPoint,X
prop_TRBWZoomPointX

rbwzoombox:001715
Writing


TRBWZoomPoint_X;X_Property;X
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomPoint.X property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf1\ul See also\plain\f3\fs16\cf3 \{linkID=%1716\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20\cf0  class
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  X: Extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 X is the real number X-coordinate of the \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf3 \{linkID=1700\}\plain\f3\fs20 
\par Run-time only
\par 
\par }
1716
Scribble1716
X property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;\red128\green0\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike XCoord property\plain\f3\fs20\cf3 \{linkID=1720\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike Y property\plain\f3\fs20\cf3 \{linkID=1725\}\plain\f3\fs20 
\par \plain\f3\fs20\cf1\strike YCoord property\plain\f3\fs20\cf3 \{linkID=1730\}\plain\f3\fs20 
\par 
\par }
1720
Scribble1720
TRBWZoomPoint.XCoord property
XCoord,TRBWZoomPoint;TRBWZoomPoint,XCoord
prop_TRBWZoomPointXCoord

rbwzoombox:001720
Writing


TRBWZoomPoint_XCoord;XCoord_Property;XCoord
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TRbwZoomPoint.XCoord property
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16\cf1\ul See also\plain\f4\fs16\cf2 \{linkID=%1721\}\plain\f4\fs16\cf0 \{keepn\}\plain\f4\fs22\b 
\par Applies to
\par \plain\f4\fs20\cf1\strike TRbwZoomPoint\plain\f4\fs20\cf2 \{linkID=1700\}\plain\f4\fs20\cf0  class
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b property\plain\f3\fs20  XCoord: Integer;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \plain\f4\fs20 XCoord is the screen number X-coordinate of the \plain\f4\fs20\cf1\strike TRbwZoomPoint\plain\f4\fs20\cf2 \{linkID=1700\}\plain\f4\fs20 
\par Run-time only
\par 
\par }
1721
Scribble1721
XCoord property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green128\blue0;\red0\green0\blue255;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf3\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike X property\plain\f3\fs20\cf1 \{linkID=1715\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike Y property\plain\f3\fs20\cf1 \{linkID=1725\}\plain\f3\fs20 
\par \plain\f3\fs20\cf2\strike YCoord property\plain\f3\fs20\cf1 \{linkID=1730\}\plain\f3\fs20 
\par 
\par }
1725
Scribble1725
TRBWZoomPoint.Y property
Y,TRBWZoomPoint;TRBWZoomPoint,Y
prop_TRBWZoomPointY

rbwzoombox:001725
Writing


TRBWZoomPoint_Y;Y_Property;Y
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomPoint.Y property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf3\ul See also\plain\f3\fs16\cf1 \{linkID=%1726\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20\cf0  class
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  Y: Extended;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 Y is the real number Y-coordinate of the \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20 
\par Run-time only
\par 
\par }
1726
Scribble1726
Y property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf2\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike X property\plain\f3\fs20\cf1 \{linkID=1715\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike XCoord property\plain\f3\fs20\cf1 \{linkID=1720\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike YCoord property\plain\f3\fs20\cf1 \{linkID=1730\}\plain\f3\fs20 
\par 
\par }
1730
Scribble1730
TRBWZoomPoint.YCoord property
YCoord,TRBWZoomPoint;TRBWZoomPoint,YCoord
prop_TRBWZoomPointYCoord

rbwzoombox:001730
Writing


TRBWZoomPoint_YCoord;YCoord_Property;YCoord
FALSE
16
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomPoint.YCoord property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16\cf3\ul See also\plain\f3\fs16\cf1 \{linkID=%1731\}\plain\f3\fs16\cf0 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20\cf0  class
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  YCoord: Integer;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 YCoord is the screen number Y-coordinate of the \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20 
\par Run-time only
\par 
\par }
1731
Scribble1731
YCoord property - See also




Writing



FALSE
9
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20\cf1\b See also
\par \plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike X property\plain\f3\fs20\cf2 \{linkID=1715\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike XCoord property\plain\f3\fs20\cf2 \{linkID=1720\}\plain\f3\fs20 
\par \plain\f3\fs20\cf3\strike Y property\plain\f3\fs20\cf2 \{linkID=1725\}\plain\f3\fs20 
\par 
\par }
1735
Scribble1735
TRBWZoomPoint.ZoomBox property
ZoomBox,TRBWZoomPoint;TRBWZoomPoint,ZoomBox
prop_TRBWZoomPointZoomBox

rbwzoombox:001735
Writing


TRBWZoomPoint_ZoomBox;ZoomBox_Property;ZoomBox
FALSE
17
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red128\green0\blue0;\red0\green0\blue255;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf2\b TRbwZoomPoint.ZoomBox property
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20\cf0  class
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b property\plain\f4\fs20  ZoomBox: TRbwZoomBox;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \plain\f3\fs20 ZoomBox is the TRbwZoomBox with which the \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf1 \{linkID=1700\}\plain\f3\fs20  is associated.
\par Run-time only
\par Read-only
\par 
\par }
1740
Scribble1740
TRBWZoomPoint.Create method
Create,TRBWZoomPoint;TRBWZoomPoint,Create


rbwzoombox:001740
Writing


TRBWZoomPoint_Create;Create_Method;Create
FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fswiss\fcharset1 Arial;}{\f4\fmodern\fcharset1 Courier New;}}
{\colortbl\red0\green0\blue0;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\deflang1033\pard\plain\f3\fs32\cf1\b TRbwZoomPoint.Create method
\par \plain\f3\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs16 \{keepn\}\plain\f3\fs22\b 
\par Applies to
\par \plain\f3\fs20\cf3\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f3\fs20\cf0  class
\par 
\par \plain\f3\fs22\b Declaration
\par \plain\f4\fs20\b constructor\plain\f4\fs20  Create(\plain\f4\fs20\b const\plain\f4\fs20  An_RbwZoomBox: TRbwZoomBox); \plain\f4\fs20\b virtual\plain\f4\fs20 ;
\par \plain\f3\fs20 
\par \plain\f3\fs22\b Description
\par \pard\plain\f3\fs20 Creates sets the \plain\f3\fs20\cf3\strike ZoomBox\plain\f3\fs20\cf2 \{linkID=1735\}\plain\f3\fs20  to RbwZoomBox and sets \plain\f3\fs20\cf3\strike UseForZoomOut property\plain\f3\fs20\cf2 \{linkID=1710\}\plain\f3\fs20  to True.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f3\fs20 
\par }
1800
Scribble1800
TZBArray type
TZBArray


rbwzoombox:001800
Writing



FALSE
15
{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fswiss MS Sans Serif;}{\f1\froman\fcharset2 Symbol;}{\f2\fswiss\fcharset1 Arial;}{\f3\fmodern\fcharset1 Courier New;}{\f4\fswiss\fcharset1 Arial;}}
{\colortbl\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue255;}
\deflang1033\pard\plain\f4\fs32\cf3\b TZBArray type
\par \plain\f4\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs16 \{keepn\}\plain\f4\fs22\b 
\par Unit
\par \plain\f4\fs20\cf1\strike RbwZoomBox\plain\f4\fs20\cf2 \{linkID=1000\}\plain\f4\fs20\cf0 
\par 
\par \plain\f4\fs22\b Declaration
\par \plain\f3\fs20\b type\plain\f3\fs20  TZBArray = \plain\f3\fs20\b array\plain\f3\fs20  \plain\f3\fs20\b of\plain\f3\fs20  \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f3\fs20 ;
\par \plain\f4\fs20 
\par \plain\f4\fs22\b Description
\par \pard\plain\f3\fs20 TZBArray is a dynamic array of \plain\f3\fs20\cf1\strike TRbwZoomPoint\plain\f3\fs20\cf2 \{linkID=1700\}\plain\f4\fs20 .  Initially all members of a \plain\f3\fs20 TZBArray will be nil.\plain\f4\fs20 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\plain\f4\fs20 
\par }
2
main="",(512,0,511,1023),0,,,0
sidebar="",(293,43,216,927),0,,,0
0
0
123
1 EInvalidZoomLevel exception
2 EInvalidZoomLevel exception Reference=Scribble1010
1 TRBWZoomBox component
2 TRBWZoomBox component Reference=Scribble1100
2 Properties
3 BottomMargin=Scribble1110
3 DefaultMultiplier=Scribble1115
3 LeftMargin=Scribble1120
3 MaxX=Scribble1125
3 MaxY=Scribble1130
3 MinX=Scribble1135
3 MinY=Scribble1140
3 Multiplier=Scribble1145
3 PBCanvas=Scribble1150
3 PBClientHeight=Scribble1155
3 PBClientOrigin=Scribble1160
3 PBClientRect=Scribble1165
3 PBClientWidth=Scribble1170
3 PBColor=Scribble1175
3 PBComObject=Scribble1180
3 PBComponentCount=Scribble1185
3 PBComponentIndex=Scribble1190
3 PBComponents=Scribble1195
3 PBControlState=Scribble1200
3 PBControlStyle=Scribble1205
3 PBCursor=Scribble1210
3 PBDragCursor=Scribble1215
3 PBDragKind=Scribble1220
3 PBDragMode=Scribble1225
3 PBEnabled=Scribble1230
3 PBFont=Scribble1235
3 PBHeight=Scribble1240
3 PBHint=Scribble1245
3 PBLeft=Scribble1250
3 PBPopupMenu=Scribble1255
3 PBShowHint=Scribble1260
3 PBTag=Scribble1265
3 PBTop=Scribble1270
3 PBVisible=Scribble1275
3 PBWidth=Scribble1280
3 PBWindowProc=Scribble1285
3 RightMargin=Scribble1290
3 SBoundsRect=Scribble1295
3 SBrush=Scribble1300
3 SCursor=Scribble1305
3 SDragCursor=Scribble1310
3 SelectionWidth=Scribble1315
3 SPen=Scribble1320
3 TopMargin=Scribble1325
3 VerticalExaggeration=Scribble1330
3 XPositive=Scribble1335
3 YPositive=Scribble1340
2 Methods
3 Create=Scribble1345
3 Destroy=Scribble1350
3 IsPointInside=Scribble1355
3 PBClientToScreen=Scribble1360
3 PBDragging=Scribble1365
3 PBGetTextBuf=Scribble1370
3 PBGetTextLen=Scribble1375
3 PBPerform=Scribble1380
3 PBScreenToClient=Scribble1385
3 SelectPoint=Scribble1390
3 SelectPolygon=Scribble1395
3 SelectPolyLine=Scribble1400
3 SelectSegment=Scribble1405
3 SetZoom=Scribble1410
3 X=Scribble1415
3 XCoord=Scribble1420
3 Y=Scribble1425
3 YCoord=Scribble1430
3 AbortZoom=Scribble1435
3 BeginPan=Scribble1440
3 BeginZoom=Scribble1445
3 ContinueZoom=Scribble1450
3 EndPan=Scribble1455
3 FinishZoom=Scribble1460
3 GetMinMax=Scribble1465
3 Invalidate=Scribble1470
3 MouseToCoordinates=Scribble1475
3 PBBeginDrag=Scribble1480
3 PBBringToFront=Scribble1485
3 PBDragDrop=Scribble1490
3 PBEndDrag=Scribble1495
3 PBHide=Scribble1500
3 PbRefresh=Scribble1505
3 PBRepaint=Scribble1510
3 PBSendToBack=Scribble1515
3 PBSetBounds=Scribble1520
3 PBSetTextBuf=Scribble1525
3 PBShow=Scribble1530
3 PBUpdate=Scribble1535
3 SetRange=Scribble1540
3 SetXRange=Scribble1545
3 SetYRange=Scribble1550
3 ZoomBy=Scribble1555
3 ZoomByAt=Scribble1560
3 ZoomOut=Scribble1565
2 Events
3 OnClick=Scribble1570
3 OnDblClick=Scribble1575
3 OnDragDrop=Scribble1580
3 OnDragOver=Scribble1585
3 OnEndDrag=Scribble1590
3 OnKeyDown=Scribble1600
3 OnKeyPress=Scribble1605
3 OnKeyUp=Scribble1610
3 OnMouseDown=Scribble1615
3 OnMouseMove=Scribble1620
3 OnMouseUp=Scribble1625
3 OnPaint=Scribble1630
3 OnStartDrag=Scribble1635
1 TRBWZoomPoint class
2 TRBWZoomPoint class Reference=Scribble1700
2 Properties
3 UseForZoomOut=Scribble1710
3 X=Scribble1715
3 XCoord=Scribble1720
3 Y=Scribble1725
3 YCoord=Scribble1730
3 ZoomBox=Scribble1735
2 Methods
3 Create=Scribble1740
9
*InternetLink
-2147483640
Times New Roman
0
12
1
....
0
0
0
0
0
0
*ParagraphTitle
-2147483640
Arial
0
11
1
B...
0
0
0
0
0
0
*PopupLink
-2147483640
Arial
0
8
1
....
0
0
0
0
0
0
*PopupTopicTitle
16711680
Arial
0
10
1
B...
0
0
0
0
0
0
*SourceCode
-2147483640
Courier New
0
10
1
....
0
0
0
0
0
0
*TopicText
-2147483640
Arial
0
10
1
....
0
0
0
0
0
0
*TopicTitle
16711680
Arial
0
16
1
B...
0
0
0
0
0
0
Link
32768
Arial
0
10
1
...S
0
0
0
0
0
0
LinkTo
128
Arial
0
10
1
....
0
0
0
0
0
0
