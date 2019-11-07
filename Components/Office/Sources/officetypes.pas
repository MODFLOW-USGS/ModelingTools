
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeTypes;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes;

type

  TksoBorderStyle = (
    kbsNone,
    kbsFlat,
    kbsSingle,
    kbsSolid,
    kbsEtched,
    kbsBump,
    kbsSunken,
    kbsRaised,
    kbsDown,
    kbsUp,
    kbsOuterRaised,
    kbsOuterSunken
  );

implementation {===============================================================}

end.
