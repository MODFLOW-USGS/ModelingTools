{******************************************************************************}
{                                                                              }
{ Unit name: JediMath.inc                                                      }
{      for the Jedi Math Alpha 1.04 release                                    }
{ Project JEDI Math  http://sourceforge.net/projects/jedimath/                 }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{ or see the file MPL-1.1.txt included in this package.                        }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This file defines various generic compiler directives. The directives in     }
{ this file are of generic nature and consist mostly of mappings from the      }
{ VERXXX directives defined by Delphi, Kylix and C++ Builder to friendly       }
{ names such as DELPHI5 and SUPPORTS_WIDESTRING. These friendly names are      }
{ subsequently in JEDI Math to test for compiler versions and/or whether the   }
{ compiler supports certain features (such as widestring's or 64 bit integers. }
{ JEDI Math contains an additional include file - JediMath.inc. That file      }
{ should be included in source files instead of this one (which is pulled in   }
{ automatically by JediMath.inc.                                               }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Unit owner: Patrick van Laake                                                }
{ Last modified:                                                               }
{      Feb. 16, 2006 by Ralph K. Muench (ralphkmuench@users.sourceforge.net)   }
{      for the Jedi Math Alpha 1.04 release                                    }
{                                                                              }
{******************************************************************************}

{$B-} // Boolean shortcut evaluation
{$H+} // Long strings
{$J-} // Read-only typed constants
{$T-} // Type checked pointers off

// Pull in the JmJedi.inc shared directives
//----------------------------------------------------------------------
{$INCLUDE JmJedi.inc}


// Math precision selection; these are mutually exclusive; currently
// only single and double precision are supported
//----------------------------------------------------------------------
{.$DEFINE jmDoublePrecision}
{.$DEFINE jmSinglePrecision}
{$DEFINE jmExtendedPrecision}


// Math angle selection, mutually exclusive
// it is strongly recommended not to select any other directive!
//----------------------------------------------------------------------
{.$DEFINE MATH_ANGLES_DEGREES}
{.$DEFINE MATH_ANGLES_GRADS}
{$DEFINE MATH_ANGLES_RADIANS}


// optimized code
//----------------------------------------------------------------------
{$DEFINE jmUseAssemblerForSpeed}


// Free Pascal specific
//----------------------------------------------------------------------
{$DEFINE UseRegAsm}
{$DEFINE UseAsm}
{$IFDEF FPC}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_OVERLOAD}
  {$ASMMODE Intel}
  {$UNDEF UseRegAsm}
{$ENDIF}


// Delphi version-specific definitions
//----------------------------------------------------------------------
{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}  // Platform warnings off
{$ENDIF}


// Establishing the compiler version and the default component base. For D6 and
// D7 the default component base is VCL. You can change this to CLX by enabling
// the $DEFINE below, or by using the Project|Options|Conditionals dialog.
//----------------------------------------------------------------------
{$DEFINE OLDVER}
{$IFDEF VER320}
  {$DEFINE DELPHI7}
  {.$DEFINE CLX}
  {$UNDEF OLDVER}
{$ENDIF}

{$IFDEF VER330}
  {$DEFINE DELPHI7}
  {.$DEFINE CLX}
  {$UNDEF OLDVER}
{$ENDIF}



{$IFDEF VER150}
  {$DEFINE DELPHI7}
  {.$DEFINE CLX}
  {$UNDEF OLDVER}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE DELPHI6}
  {.$DEFINE CLX}
  {$UNDEF OLDVER}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE DELPHI5}
  {$UNDEF CLX}
  {$UNDEF OLDVER}
{$ENDIF}

{$IFDEF LINUX}
 {$DEFINE CLX}
{$ENDIF}

{$IFDEF FPC}
  {$UNDEF CLX}
  {$UNDEF OLDVER}
{$ENDIF}

{$IFDEF OLDVER}
DSpatial will only compile successfully with Delphi 5-7, or Kylix.
You may be able to patch the code to work with older versions of
Delphi, but this is not supported.
{$ENDIF}

