// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLFile3DS.pas' rev: 34.00 (Windows)

#ifndef Glfile3dsHPP
#define Glfile3dsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <OpenGLTokens.hpp>
#include <GLStrings.hpp>
#include <GLScene.hpp>
#include <GLObjects.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLTexture.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLVectorGeometry.hpp>
#include <File3DS.hpp>
#include <Types3DS.hpp>
#include <GLContext.hpp>
#include <GLPersistentClasses.hpp>
#include <GLFile3DSSceneObjects.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorLists.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLMaterial.hpp>
#include <GLBaseClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glfile3ds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EGLFile3DS;
struct TGLFile3DSAnimationData;
class DELPHICLASS TGLFile3DSAnimationKeys;
class DELPHICLASS TGLFile3DSScaleAnimationKeys;
class DELPHICLASS TGLFile3DSRotationAnimationKeys;
class DELPHICLASS TGLFile3DSPositionAnimationKeys;
class DELPHICLASS TGLFile3DSColorAnimationKeys;
class DELPHICLASS TTGLFile3DSPositionAnimationKeys;
class DELPHICLASS TGLFile3DSSpotLightCutOffAnimationKeys;
class DELPHICLASS TGLFile3DSLightHotSpotAnimationKeys;
class DELPHICLASS TGLFile3DSRollAnimationKeys;
class DELPHICLASS TGLFile3DSAnimationKeyList;
class DELPHICLASS TGLFile3DSDummyObject;
class DELPHICLASS TGLFile3DSMeshObject;
class DELPHICLASS TGLFile3DSOmniLightObject;
class DELPHICLASS TGLFile3DSSpotLightObject;
class DELPHICLASS TGLFile3DSCameraObject;
class DELPHICLASS TGL3DSVectorFile;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION EGLFile3DS : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EGLFile3DS(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLFile3DS(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLFile3DS(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLFile3DS(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLFile3DS(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLFile3DS(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLFile3DS() { }
	
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TGLFile3DSAnimationData
{
public:
	Glvectortypes::TMatrix4f ModelMatrix;
	Glvectortypes::TVector4f Color;
	Glvectortypes::TVector3f TargetPos;
	float SpotLightCutOff;
	float HotSpot;
	float Roll;
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TGLFile3DSAnimationKeys : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
	
private:
	typedef System::DynamicArray<Types3ds::TKeyHeader3DS> _TGLFile3DSAnimationKeys__1;
	
	
private:
	int FNumKeys;
	_TGLFile3DSAnimationKeys__1 FKeys;
	void __fastcall InterpolateFrame(int &I, double &w, const double AFrame);
	
protected:
	float __fastcall InterpolateValue(const float *AValues, const int AValues_High, const double AFrame)/* overload */;
	Glvectortypes::TVector3f __fastcall InterpolateValue(const Glvectortypes::TVector3f *AValues, const int AValues_High, const double AFrame)/* overload */;
	Glvectortypes::TMatrix4f __fastcall InterpolateValue(const Types3ds::TKFRotKey3DS *AValues, const int AValues_High, const double AFrame)/* overload */;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame) = 0 ;
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSAnimationKeys() : Glpersistentclasses::TPersistentObject() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSScaleAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Glvectortypes::TVector3f> _TGLFile3DSScaleAnimationKeys__1;
	
	
private:
	_TGLFile3DSScaleAnimationKeys__1 FScale;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSScaleAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSScaleAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSScaleAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSRotationAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Types3ds::TKFRotKey3DS> _TGLFile3DSRotationAnimationKeys__1;
	
	
private:
	_TGLFile3DSRotationAnimationKeys__1 FRot;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSRotationAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSRotationAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSRotationAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSPositionAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Glvectortypes::TVector3f> _TGLFile3DSPositionAnimationKeys__1;
	
	
private:
	_TGLFile3DSPositionAnimationKeys__1 FPos;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSPositionAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSPositionAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSPositionAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSColorAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Glvectortypes::TVector3f> _TGLFile3DSColorAnimationKeys__1;
	
	
private:
	_TGLFile3DSColorAnimationKeys__1 FCol;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSColorAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSColorAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSColorAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TTGLFile3DSPositionAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<Glvectortypes::TVector3f> _TTGLFile3DSPositionAnimationKeys__1;
	
	
private:
	_TTGLFile3DSPositionAnimationKeys__1 FTPos;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TTGLFile3DSPositionAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TTGLFile3DSPositionAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TTGLFile3DSPositionAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSSpotLightCutOffAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSSpotLightCutOffAnimationKeys__1;
	
	
private:
	_TGLFile3DSSpotLightCutOffAnimationKeys__1 FFall;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSSpotLightCutOffAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSSpotLightCutOffAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSSpotLightCutOffAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSLightHotSpotAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSLightHotSpotAnimationKeys__1;
	
	
private:
	_TGLFile3DSLightHotSpotAnimationKeys__1 FHot;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSLightHotSpotAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSLightHotSpotAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSLightHotSpotAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSRollAnimationKeys : public TGLFile3DSAnimationKeys
{
	typedef TGLFile3DSAnimationKeys inherited;
	
	
private:
	typedef System::DynamicArray<float> _TGLFile3DSRollAnimationKeys__1;
	
	
private:
	_TGLFile3DSRollAnimationKeys__1 FRoll;
	
public:
	virtual void __fastcall LoadData(const int ANumKeys, const Types3ds::PKeyHeaderList Keys, const void * AData);
	virtual void __fastcall Apply(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSRollAnimationKeys() : TGLFile3DSAnimationKeys() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSRollAnimationKeys(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSAnimationKeys(reader) { }
	/* TPersistentObject.Destroy */ inline __fastcall virtual ~TGLFile3DSRollAnimationKeys() { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSAnimationKeyList : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
	
private:
	typedef System::DynamicArray<TGLFile3DSAnimationKeys*> _TGLFile3DSAnimationKeyList__1;
	
	
private:
	_TGLFile3DSAnimationKeyList__1 FAnimKeysList;
	
protected:
	void __fastcall ApplyAnimKeys(TGLFile3DSAnimationData &DataTransf, const double AFrame);
	
public:
	void __fastcall AddKeys(TGLFile3DSAnimationKeys* const AItem);
	void __fastcall ClearKeys();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSAnimationKeyList();
public:
	/* TPersistentObject.Create */ inline __fastcall virtual TGLFile3DSAnimationKeyList() : Glpersistentclasses::TPersistentObject() { }
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSAnimationKeyList(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	
};


enum DECLSPEC_DENUM TGLFile3DSAnimKeysClassType : unsigned char { ctScale, ctRot, ctPos, ctCol, ctTPos, ctFall, ctHot, ctRoll };

class PASCALIMPLEMENTATION TGLFile3DSDummyObject : public Glvectorfileobjects::TGLMorphableMeshObject
{
	typedef Glvectorfileobjects::TGLMorphableMeshObject inherited;
	
private:
	TGLFile3DSAnimationKeyList* FAnimList;
	void *FAnimData;
	TGLFile3DSAnimationData FRefTranf;
	TGLFile3DSAnimationData FAnimTransf;
	TGLFile3DSDummyObject* FParent;
	Types3ds::String64 FParentName;
	bool FStatic;
	
public:
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	virtual void __fastcall MorphTo(int morphTargetIndex);
	virtual void __fastcall Lerp(int morphTargetIndex1, int morphTargetIndex2, float lerpFactor);
	virtual void __fastcall GetExtents(/* out */ Glvectortypes::TVector3f &min, /* out */ Glvectortypes::TVector3f &max)/* overload */;
	virtual Glvectorlists::TAffineVectorList* __fastcall ExtractTriangles(Glvectorlists::TAffineVectorList* texCoords = (Glvectorlists::TAffineVectorList*)(0x0), Glvectorlists::TAffineVectorList* normals = (Glvectorlists::TAffineVectorList*)(0x0));
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLFile3DSDummyObject();
	__fastcall virtual ~TGLFile3DSDummyObject();
	__property TGLFile3DSAnimationKeyList* AnimList = {read=FAnimList};
	__property TGLFile3DSDummyObject* Parent = {read=FParent, write=FParent};
	__property TGLFile3DSAnimationData RefrenceTransf = {read=FRefTranf, write=FRefTranf};
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSDummyObject(Glvectorfileobjects::TGLMeshObjectList* AOwner) : Glvectorfileobjects::TGLMorphableMeshObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSDummyObject(Glpersistentclasses::TVirtualReader* reader) : Glvectorfileobjects::TGLMorphableMeshObject(reader) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  GetExtents(/* out */ Glgeometrybb::TAABB &aabb){ Glvectorfileobjects::TMeshObject::GetExtents(aabb); }
	
};


class PASCALIMPLEMENTATION TGLFile3DSMeshObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
public:
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &ARci);
public:
	/* TGLFile3DSDummyObject.Create */ inline __fastcall virtual TGLFile3DSMeshObject() : TGLFile3DSDummyObject() { }
	/* TGLFile3DSDummyObject.Destroy */ inline __fastcall virtual ~TGLFile3DSMeshObject() { }
	
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSMeshObject(Glvectorfileobjects::TGLMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSMeshObject(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSOmniLightObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
private:
	Glfile3dssceneobjects::TGLFile3DSLight* FLightSrc;
	Types3ds::String64 FLightSrcName;
	
public:
	__fastcall virtual TGLFile3DSOmniLightObject();
	virtual void __fastcall LoadData(Glvectorfileobjects::TGLBaseMesh* const AOwner, const Types3ds::PLight3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSOmniLightObject();
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSOmniLightObject(Glvectorfileobjects::TGLMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSOmniLightObject(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSSpotLightObject : public TGLFile3DSOmniLightObject
{
	typedef TGLFile3DSOmniLightObject inherited;
	
public:
	virtual void __fastcall LoadData(Glvectorfileobjects::TGLBaseMesh* const AOwner, const Types3ds::PLight3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
public:
	/* TGLFile3DSOmniLightObject.Create */ inline __fastcall virtual TGLFile3DSSpotLightObject() : TGLFile3DSOmniLightObject() { }
	/* TGLFile3DSOmniLightObject.Destroy */ inline __fastcall virtual ~TGLFile3DSSpotLightObject() { }
	
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSSpotLightObject(Glvectorfileobjects::TGLMeshObjectList* AOwner) : TGLFile3DSOmniLightObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSSpotLightObject(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSOmniLightObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGLFile3DSCameraObject : public TGLFile3DSDummyObject
{
	typedef TGLFile3DSDummyObject inherited;
	
private:
	Globjects::TGLDummyCube* FTargetObj;
	Glfile3dssceneobjects::TGLFile3DSCamera* FCameraSrc;
	Types3ds::String64 FCameraSrcName;
	
public:
	__fastcall virtual TGLFile3DSCameraObject();
	void __fastcall LoadData(Glvectorfileobjects::TGLBaseMesh* Owner, Types3ds::PCamera3DS AData);
	virtual void __fastcall LoadAnimation(const void * AData);
	virtual void __fastcall SetFrame(const double AFrame);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* Writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* Reader);
	__fastcall virtual ~TGLFile3DSCameraObject();
public:
	/* TMeshObject.CreateOwned */ inline __fastcall TGLFile3DSCameraObject(Glvectorfileobjects::TGLMeshObjectList* AOwner) : TGLFile3DSDummyObject(AOwner) { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLFile3DSCameraObject(Glpersistentclasses::TVirtualReader* reader) : TGLFile3DSDummyObject(reader) { }
	
};


class PASCALIMPLEMENTATION TGL3DSVectorFile : public Glvectorfileobjects::TGLVectorFile
{
	typedef Glvectorfileobjects::TGLVectorFile inherited;
	
public:
	__classmethod virtual Glapplicationfileio::TGLDataFileCapabilities __fastcall Capabilities();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
public:
	/* TGLVectorFile.Create */ inline __fastcall virtual TGL3DSVectorFile(System::Classes::TPersistent* AOwner) : Glvectorfileobjects::TGLVectorFile(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGL3DSVectorFile() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool vGLFile3DS_UseTextureEx;
extern DELPHI_PACKAGE bool vGLFile3DS_EnableAnimation;
extern DELPHI_PACKAGE bool vGLFile3DS_FixDefaultUpAxisY;
extern DELPHI_PACKAGE int vGLFile3DS_LoadedStaticFrame;
}	/* namespace Glfile3ds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLFILE3DS)
using namespace Glfile3ds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glfile3dsHPP
