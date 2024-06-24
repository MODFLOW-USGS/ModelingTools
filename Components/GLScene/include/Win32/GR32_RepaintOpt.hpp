// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_RepaintOpt.pas' rev: 35.00 (Windows)

#ifndef Gr32_repaintoptHPP
#define Gr32_repaintoptHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GR32.hpp>
#include <GR32_LowLevel.hpp>
#include <GR32_Containers.hpp>
#include <GR32_Layers.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_repaintopt
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomRepaintOptimizer;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomRepaintOptimizer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FEnabled;
	System::Classes::TList* FLayerCollections;
	Gr32_containers::TRectList* FInvalidRects;
	Gr32::TBitmap32* FBuffer;
	
protected:
	virtual bool __fastcall GetEnabled();
	virtual void __fastcall SetEnabled(const bool Value);
	__property System::Classes::TList* LayerCollections = {read=FLayerCollections, write=FLayerCollections};
	__property Gr32::TBitmap32* Buffer = {read=FBuffer, write=FBuffer};
	__property Gr32_containers::TRectList* InvalidRects = {read=FInvalidRects, write=FInvalidRects};
	virtual void __fastcall LayerCollectionNotifyHandler(Gr32_layers::TLayerCollection* Sender, Gr32_layers::TLayerListNotification Action, Gr32_layers::TCustomLayer* Layer, int Index) = 0 ;
	
public:
	__fastcall virtual TCustomRepaintOptimizer(Gr32::TBitmap32* Buffer, Gr32_containers::TRectList* InvalidRects);
	__fastcall virtual ~TCustomRepaintOptimizer();
	virtual void __fastcall RegisterLayerCollection(Gr32_layers::TLayerCollection* Layers);
	virtual void __fastcall UnregisterLayerCollection(Gr32_layers::TLayerCollection* Layers);
	virtual void __fastcall BeginPaint();
	virtual void __fastcall EndPaint();
	virtual void __fastcall BeginPaintBuffer();
	virtual void __fastcall EndPaintBuffer();
	virtual void __fastcall Reset() = 0 ;
	virtual bool __fastcall UpdatesAvailable() = 0 ;
	virtual void __fastcall PerformOptimization() = 0 ;
	virtual void __fastcall AreaUpdateHandler(System::TObject* Sender, const System::Types::TRect &Area, const unsigned Info) = 0 ;
	virtual void __fastcall LayerUpdateHandler(System::TObject* Sender, Gr32_layers::TCustomLayer* Layer) = 0 ;
	virtual void __fastcall BufferResizedHandler(const int NewWidth, const int NewHeight) = 0 ;
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, nodefault};
};

#pragma pack(pop)

typedef System::TMetaClass* TCustomRepaintOptimizerClass;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall InflateArea(System::Types::TRect &Area, int Dx, int Dy);
}	/* namespace Gr32_repaintopt */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_REPAINTOPT)
using namespace Gr32_repaintopt;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gr32_repaintoptHPP
