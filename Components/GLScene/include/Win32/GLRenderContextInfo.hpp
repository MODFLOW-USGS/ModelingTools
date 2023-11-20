// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLRenderContextInfo.pas' rev: 35.00 (Windows)

#ifndef GlrendercontextinfoHPP
#define GlrendercontextinfoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLState.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLColor.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glrendercontextinfo
{
//-- forward type declarations -----------------------------------------------
struct TGLSize;
struct TRenderContextClippingInfo;
struct TGLRenderContextInfo;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDrawState : unsigned char { dsRendering, dsPicking, dsPrinting };

struct DECLSPEC_DRECORD TGLSize
{
public:
	int cx;
	int cy;
};


enum DECLSPEC_DENUM TGLObjectsSorting : unsigned char { osInherited, osNone, osRenderFarthestFirst, osRenderBlendedLast, osRenderNearestFirst };

enum DECLSPEC_DENUM TGLVisibilityCulling : unsigned char { vcInherited, vcNone, vcObjectBased, vcHierarchical };

struct DECLSPEC_DRECORD TRenderContextClippingInfo
{
public:
	Glvectortypes::TVector4f origin;
	Glvectortypes::TVector4f clippingDirection;
	float viewPortRadius;
	float nearClippingDistance;
	float farClippingDistance;
	Glvectorgeometry::TFrustum frustum;
};


struct DECLSPEC_DRECORD TGLRenderContextInfo
{
public:
	System::TObject* scene;
	System::TObject* buffer;
	Glvectortypes::TVector4f cameraPosition;
	Glvectortypes::TVector4f cameraDirection;
	Glvectortypes::TVector4f cameraUp;
	TGLSize viewPortSize;
	int renderDPI;
	System::TObject* materialLibrary;
	System::TObject* lightmapLibrary;
	int fogDisabledCounter;
	TDrawState drawState;
	TGLObjectsSorting objectsSorting;
	TGLVisibilityCulling visibilityCulling;
	Glstate::TGLStateCache* GLStates;
	Glpipelinetransformation::TGLTransformation* PipelineTransformation;
	TRenderContextClippingInfo rcci;
	Glvectortypes::TVector4f sceneAmbientColor;
	bool bufferFaceCull;
	bool bufferLighting;
	bool bufferFog;
	bool bufferDepthTest;
	bool proxySubObject;
	bool ignoreMaterials;
	bool ignoreBlendingRequests;
	bool ignoreDepthRequests;
	bool amalgamating;
	Glpersistentclasses::TPersistentObjectList* lights;
	Glpersistentclasses::TPersistentObjectList* afterRenderEffects;
	Glstate::TGLMaterialLevel currentMaterialLevel;
	Glstate::TGLMeshPrimitives primitiveMask;
	int orderCounter;
};


typedef TGLRenderContextInfo *PRenderContextInfo;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glrendercontextinfo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLRENDERCONTEXTINFO)
using namespace Glrendercontextinfo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlrendercontextinfoHPP
