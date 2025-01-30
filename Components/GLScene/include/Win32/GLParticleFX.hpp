// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLParticleFX.pas' rev: 36.00 (Windows)

#ifndef GLParticleFXHPP
#define GLParticleFXHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.Math.hpp>
#include <OpenGLTokens.hpp>
#include <GLScene.hpp>
#include <GLCrossPlatform.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLState.hpp>
#include <GLVectorTypes.hpp>
#include <GLPersistentClasses.hpp>
#include <GLVectorGeometry.hpp>
#include <GLXCollection.hpp>
#include <GLMaterial.hpp>
#include <GLCadencer.hpp>
#include <GLVectorLists.hpp>
#include <GLGraphics.hpp>
#include <GLContext.hpp>
#include <GLColor.hpp>
#include <GLBaseClasses.hpp>
#include <GLCoordinates.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLManager.hpp>
#include <GLTextureFormat.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glparticlefx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLParticle;
class DELPHICLASS TGLParticleList;
class DELPHICLASS TGLParticleFXManager;
class DELPHICLASS TGLParticleFXEffect;
struct TParticleReference;
struct TPFXRegion;
class DELPHICLASS TGLParticleFXRenderer;
class DELPHICLASS TGLSourcePFXEffect;
class DELPHICLASS TGLDynamicPFXManager;
class DELPHICLASS TPFXLifeColor;
class DELPHICLASS TPFXLifeColors;
class DELPHICLASS TGLLifeColoredPFXManager;
class DELPHICLASS TGLCustomPFXManager;
class DELPHICLASS TGLPolygonPFXManager;
class DELPHICLASS TGLBaseSpritePFXManager;
class DELPHICLASS TGLCustomSpritePFXManager;
class DELPHICLASS TGLPointLightPFXManager;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TGLParticle : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
private:
	int FID;
	int FTag;
	TGLParticleFXManager* FManager;
	Glvectorgeometry::TAffineVector FPosition;
	Glvectorgeometry::TAffineVector FVelocity;
	float FRotation;
	double FCreationTime;
	float FEffectScale;
	float __fastcall GetPosition(const int Index);
	void __fastcall WritePosition(const int Index, const float aValue);
	float __fastcall GetVelocity(const int Index);
	void __fastcall WriteVelocity(const int Index, const float aValue);
	
public:
	__fastcall virtual TGLParticle();
	__fastcall virtual ~TGLParticle();
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	__property TGLParticleFXManager* Manager = {read=FManager, write=FManager};
	__property int ID = {read=FID, nodefault};
	__property Glvectorgeometry::TAffineVector Position = {read=FPosition, write=FPosition};
	__property Glvectorgeometry::TAffineVector Velocity = {read=FVelocity, write=FVelocity};
	__property double CreationTime = {read=FCreationTime, write=FCreationTime};
	__property float PosX = {read=GetPosition, write=WritePosition, index=0};
	__property float PosY = {read=GetPosition, write=WritePosition, index=1};
	__property float PosZ = {read=GetPosition, write=WritePosition, index=2};
	__property float VelX = {read=GetVelocity, write=WriteVelocity, index=0};
	__property float VelY = {read=GetVelocity, write=WriteVelocity, index=1};
	__property float VelZ = {read=GetVelocity, write=WriteVelocity, index=2};
	__property int Tag = {read=FTag, write=FTag, nodefault};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLParticle(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	
};


typedef System::TMetaClass* TGLParticleClass;

typedef System::StaticArray<TGLParticle*, 134217728> TGLParticleArray;

typedef TGLParticleArray *PGLParticleArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLParticleList : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
public:
	TGLParticle* operator[](int index) { return this->Items[index]; }
	
private:
	TGLParticleFXManager* FOwner;
	Glpersistentclasses::TPersistentObjectList* FItemList;
	PGLParticleArray FDirectList;
	
protected:
	TGLParticle* __fastcall GetItems(int index);
	void __fastcall SetItems(int index, TGLParticle* val);
	void __fastcall AfterItemCreated(System::TObject* Sender);
	
public:
	__fastcall virtual TGLParticleList();
	__fastcall virtual ~TGLParticleList();
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	__property TGLParticleFXManager* Owner = {read=FOwner, write=FOwner};
	__property TGLParticle* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	int __fastcall ItemCount();
	int __fastcall AddItem(TGLParticle* aItem);
	void __fastcall RemoveAndFreeItem(TGLParticle* aItem);
	int __fastcall IndexOfItem(TGLParticle* aItem);
	void __fastcall Pack();
	__property PGLParticleArray List = {read=FDirectList};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TGLParticleList(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TPFXCreateParticleEvent)(System::TObject* Sender, TGLParticle* aParticle);

class PASCALIMPLEMENTATION TGLParticleFXManager : public Glcadencer::TGLCadencedComponent
{
	typedef Glcadencer::TGLCadencedComponent inherited;
	
private:
	Glmaterial::TGLBlendingMode FBlendingMode;
	TGLParticleFXRenderer* FRenderer;
	TGLParticleList* FParticles;
	int FNextID;
	TPFXCreateParticleEvent FOnCreateParticle;
	bool FAutoFreeWhenEmpty;
	System::Classes::TList* FUsers;
	
protected:
	void __fastcall SetRenderer(TGLParticleFXRenderer* const val);
	void __fastcall SetParticles(TGLParticleList* const aParticles);
	virtual unsigned __fastcall TexturingMode() = 0 ;
	virtual void __fastcall InitializeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci) = 0 ;
	virtual void __fastcall BeginParticles(Glrendercontextinfo::TGLRenderContextInfo &rci) = 0 ;
	virtual void __fastcall RenderParticle(Glrendercontextinfo::TGLRenderContextInfo &rci, TGLParticle* aParticle) = 0 ;
	virtual void __fastcall EndParticles(Glrendercontextinfo::TGLRenderContextInfo &rci) = 0 ;
	virtual void __fastcall FinalizeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci) = 0 ;
	__property int NextID = {read=FNextID, write=FNextID, nodefault};
	__property Glmaterial::TGLBlendingMode BlendingMode = {read=FBlendingMode, write=FBlendingMode, nodefault};
	void __fastcall ApplyBlendingMode(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall UnapplyBlendingMode(Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall registerUser(TGLParticleFXEffect* obj);
	void __fastcall unregisterUser(TGLParticleFXEffect* obj);
	
public:
	__fastcall virtual TGLParticleFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLParticleFXManager();
	virtual void __fastcall NotifyChange(System::TObject* Sender);
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	__classmethod virtual TGLParticleClass __fastcall ParticlesClass();
	virtual TGLParticle* __fastcall CreateParticle();
	void __fastcall CreateParticles(int nbParticles);
	__property TGLParticleList* Particles = {read=FParticles, write=SetParticles};
	virtual int __fastcall ParticleCount();
	__property bool AutoFreeWhenEmpty = {read=FAutoFreeWhenEmpty, write=FAutoFreeWhenEmpty, nodefault};
	
__published:
	__property TGLParticleFXRenderer* Renderer = {read=FRenderer, write=SetRenderer};
	__property TPFXCreateParticleEvent OnCreateParticle = {read=FOnCreateParticle, write=FOnCreateParticle};
	__property Cadencer;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLParticleFXEffect : public Glscene::TGLObjectPostEffect
{
	typedef Glscene::TGLObjectPostEffect inherited;
	
private:
	TGLParticleFXManager* FManager;
	System::UnicodeString FManagerName;
	float FEffectScale;
	void __fastcall SetEffectScale(const float Value);
	
protected:
	void __fastcall SetManager(TGLParticleFXManager* val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	DYNAMIC void __fastcall Loaded();
	void __fastcall managerNotification(TGLParticleFXManager* aManager, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TGLParticleFXEffect(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLParticleFXEffect();
	
__published:
	__property TGLParticleFXManager* Manager = {read=FManager, write=SetManager};
	__property float EffectScale = {read=FEffectScale, write=SetEffectScale};
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD TParticleReference
{
public:
	TGLParticle* particle;
	int distance;
};
#pragma pack(pop)


typedef TParticleReference *PParticleReference;

typedef System::StaticArray<TParticleReference, 8388607> TParticleReferenceArray;

typedef TParticleReferenceArray *PParticleReferenceArray;

typedef System::StaticArray<void *, 8388607> TFXPointerList;

typedef TFXPointerList *PFXPointerList;

struct DECLSPEC_DRECORD TPFXRegion
{
public:
	int count;
	int capacity;
	PParticleReferenceArray particleRef;
	PFXPointerList particleOrder;
};


typedef TPFXRegion *PPFXRegion;

enum DECLSPEC_DENUM TPFXSortAccuracy : unsigned char { saLow, saOneTenth, saOneThird, saOneHalf, saHigh };

class PASCALIMPLEMENTATION TGLParticleFXRenderer : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	System::Classes::TList* FManagerList;
	double FLastSortTime;
	int FLastParticleCount;
	bool FZWrite;
	bool FZTest;
	bool FZCull;
	TPFXSortAccuracy FZSortAccuracy;
	float FZMaxDistance;
	Glmaterial::TGLBlendingMode FBlendingMode;
	System::StaticArray<TPFXRegion, 128> FRegions;
	
protected:
	bool __fastcall StoreZMaxDistance();
	void __fastcall RegisterManager(TGLParticleFXManager* aManager);
	void __fastcall UnRegisterManager(TGLParticleFXManager* aManager);
	void __fastcall UnRegisterAll();
	
public:
	__fastcall virtual TGLParticleFXRenderer(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLParticleFXRenderer();
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property double LastSortTime = {read=FLastSortTime};
	__property int LastParticleCount = {read=FLastParticleCount, nodefault};
	
__published:
	__property bool ZWrite = {read=FZWrite, write=FZWrite, default=0};
	__property bool ZTest = {read=FZTest, write=FZTest, default=1};
	__property bool ZCull = {read=FZCull, write=FZCull, default=1};
	__property TPFXSortAccuracy ZSortAccuracy = {read=FZSortAccuracy, write=FZSortAccuracy, default=4};
	__property float ZMaxDistance = {read=FZMaxDistance, write=FZMaxDistance, stored=StoreZMaxDistance};
	__property Glmaterial::TGLBlendingMode BlendingMode = {read=FBlendingMode, write=FBlendingMode, default=2};
	__property Visible = {default=1};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLParticleFXRenderer(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


enum DECLSPEC_DENUM TGLSourcePFXVelocityMode : unsigned char { svmAbsolute, svmRelative };

enum DECLSPEC_DENUM TGLSourcePFXPositionMode : unsigned char { spmAbsoluteOffset, spmRelative };

enum DECLSPEC_DENUM TGLSourcePFXDispersionMode : unsigned char { sdmFast, sdmIsotropic, sdmGaussian };

class PASCALIMPLEMENTATION TGLSourcePFXEffect : public TGLParticleFXEffect
{
	typedef TGLParticleFXEffect inherited;
	
private:
	Glcoordinates::TGLCoordinates* FInitialVelocity;
	Glcoordinates::TGLCoordinates* FInitialPosition;
	Glcoordinates::TGLCoordinates* FPositionDispersionRange;
	float FVelocityDispersion;
	float FPositionDispersion;
	float FParticleInterval;
	TGLSourcePFXVelocityMode FVelocityMode;
	TGLSourcePFXPositionMode FPositionMode;
	TGLSourcePFXDispersionMode FDispersionMode;
	bool FEnabled;
	bool FDisabledIfOwnerInvisible;
	double FTimeRemainder;
	float FRotationDispersion;
	
protected:
	void __fastcall SetInitialVelocity(Glcoordinates::TGLCoordinates* const val);
	void __fastcall SetInitialPosition(Glcoordinates::TGLCoordinates* const val);
	void __fastcall SetPositionDispersionRange(Glcoordinates::TGLCoordinates* const val);
	void __fastcall SetParticleInterval(const float val);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	Glvectorgeometry::TAffineVector __fastcall ParticleAbsoluteInitialPos();
	
public:
	__fastcall virtual TGLSourcePFXEffect(Glxcollection::TXCollection* aOwner);
	__fastcall virtual ~TGLSourcePFXEffect();
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	void __fastcall Burst(double time, int nb);
	void __fastcall RingExplosion(double time, float minInitialSpeed, float maxInitialSpeed, int nbParticles);
	
__published:
	__property Glcoordinates::TGLCoordinates* InitialVelocity = {read=FInitialVelocity, write=SetInitialVelocity};
	__property float VelocityDispersion = {read=FVelocityDispersion, write=FVelocityDispersion};
	__property Glcoordinates::TGLCoordinates* InitialPosition = {read=FInitialPosition, write=SetInitialPosition};
	__property float PositionDispersion = {read=FPositionDispersion, write=FPositionDispersion};
	__property Glcoordinates::TGLCoordinates* PositionDispersionRange = {read=FPositionDispersionRange, write=SetPositionDispersionRange};
	__property float ParticleInterval = {read=FParticleInterval, write=SetParticleInterval};
	__property TGLSourcePFXVelocityMode VelocityMode = {read=FVelocityMode, write=FVelocityMode, default=0};
	__property TGLSourcePFXPositionMode PositionMode = {read=FPositionMode, write=FPositionMode, default=0};
	__property TGLSourcePFXDispersionMode DispersionMode = {read=FDispersionMode, write=FDispersionMode, default=0};
	__property float RotationDispersion = {read=FRotationDispersion, write=FRotationDispersion};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property bool DisabledIfOwnerInvisible = {read=FDisabledIfOwnerInvisible, write=FDisabledIfOwnerInvisible, nodefault};
};


class PASCALIMPLEMENTATION TGLDynamicPFXManager : public TGLParticleFXManager
{
	typedef TGLParticleFXManager inherited;
	
private:
	Glcoordinates::TGLCoordinates* FAcceleration;
	float FFriction;
	double FCurrentTime;
	
protected:
	void __fastcall SetAcceleration(Glcoordinates::TGLCoordinates* const val);
	virtual float __fastcall MaxParticleAge() = 0 ;
	__property double CurrentTime = {read=FCurrentTime};
	
public:
	__fastcall virtual TGLDynamicPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLDynamicPFXManager();
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	
__published:
	__property Glcoordinates::TGLCoordinates* Acceleration = {read=FAcceleration, write=SetAcceleration};
	__property float Friction = {read=FFriction, write=FFriction};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TPFXLifeColor : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glcolor::TGLColor* FColorInner;
	Glcolor::TGLColor* FColorOuter;
	float FLifeTime;
	float FInvLifeTime;
	float FIntervalRatio;
	float FSizeScale;
	bool FDoScale;
	bool FDoRotate;
	float FRotateAngle;
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	void __fastcall SetColorInner(Glcolor::TGLColor* const val);
	void __fastcall SetColorOuter(Glcolor::TGLColor* const val);
	void __fastcall SetLifeTime(const float val);
	void __fastcall SetSizeScale(const float val);
	void __fastcall SetRotateAngle(const float Value);
	
public:
	__fastcall virtual TPFXLifeColor(System::Classes::TCollection* Collection);
	__fastcall virtual ~TPFXLifeColor();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property float InvLifeTime = {read=FInvLifeTime};
	__property float InvIntervalRatio = {read=FIntervalRatio};
	
__published:
	__property Glcolor::TGLColor* ColorInner = {read=FColorInner, write=SetColorInner};
	__property Glcolor::TGLColor* ColorOuter = {read=FColorOuter, write=SetColorOuter};
	__property float LifeTime = {read=FLifeTime, write=SetLifeTime};
	__property float SizeScale = {read=FSizeScale, write=SetSizeScale};
	__property float RotateAngle = {read=FRotateAngle, write=SetRotateAngle};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TPFXLifeColors : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TPFXLifeColor* operator[](int index) { return this->Items[index]; }
	
protected:
	void __fastcall SetItems(int index, TPFXLifeColor* const val);
	TPFXLifeColor* __fastcall GetItems(int index);
	
public:
	__fastcall TPFXLifeColors(System::Classes::TPersistent* AOwner);
	HIDESBASE TPFXLifeColor* __fastcall Add();
	HIDESBASE TPFXLifeColor* __fastcall FindItemID(int ID);
	__property TPFXLifeColor* Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	double __fastcall MaxLifeTime();
	bool __fastcall RotationsDefined();
	bool __fastcall ScalingDefined();
	void __fastcall PrepareIntervalRatios();
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TPFXLifeColors() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLLifeColoredPFXManager : public TGLDynamicPFXManager
{
	typedef TGLDynamicPFXManager inherited;
	
private:
	TPFXLifeColors* FLifeColors;
	System::Classes::TList* FLifeColorsLookup;
	bool FLifeRotations;
	bool FLifeScaling;
	Glcolor::TGLColor* FColorInner;
	Glcolor::TGLColor* FColorOuter;
	float FParticleSize;
	
protected:
	void __fastcall SetLifeColors(TPFXLifeColors* const val);
	void __fastcall SetColorInner(Glcolor::TGLColor* const val);
	void __fastcall SetColorOuter(Glcolor::TGLColor* const val);
	virtual void __fastcall InitializeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual float __fastcall MaxParticleAge();
	void __fastcall ComputeColors(float &lifeTime, Glcolor::TColorVector &inner, Glcolor::TColorVector &outer);
	void __fastcall ComputeInnerColor(float &lifeTime, Glcolor::TColorVector &inner);
	void __fastcall ComputeOuterColor(float &lifeTime, Glcolor::TColorVector &outer);
	bool __fastcall ComputeSizeScale(float &lifeTime, float &sizeScale);
	bool __fastcall ComputeRotateAngle(float &lifeTime, float &rotateAngle);
	void __fastcall RotateVertexBuf(Glvectorlists::TAffineVectorList* buf, float lifeTime, const Glvectorgeometry::TAffineVector &axis, float offsetAngle);
	
public:
	__fastcall virtual TGLLifeColoredPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLLifeColoredPFXManager();
	__property float ParticleSize = {read=FParticleSize, write=FParticleSize};
	__property Glcolor::TGLColor* ColorInner = {read=FColorInner, write=SetColorInner};
	__property Glcolor::TGLColor* ColorOuter = {read=FColorOuter, write=SetColorOuter};
	__property TPFXLifeColors* LifeColors = {read=FLifeColors, write=SetLifeColors};
	
__published:
	__property BlendingMode = {default=2};
};


typedef void __fastcall (__closure *TPFXDirectRenderEvent)(System::TObject* Sender, TGLParticle* aParticle, Glrendercontextinfo::TGLRenderContextInfo &rci);

typedef void __fastcall (__closure *TPFXProgressEvent)(System::TObject* Sender, const Glbaseclasses::TGLProgressTimes &progressTime, bool &defaultProgress);

typedef void __fastcall (__closure *TPFXParticleProgress)(System::TObject* Sender, const Glbaseclasses::TGLProgressTimes &progressTime, TGLParticle* aParticle, bool &killParticle);

typedef int __fastcall (__closure *TPFXGetParticleCountEvent)(System::TObject* Sender);

class PASCALIMPLEMENTATION TGLCustomPFXManager : public TGLLifeColoredPFXManager
{
	typedef TGLLifeColoredPFXManager inherited;
	
private:
	Glscene::TGLDirectRenderEvent FOnInitializeRendering;
	Glscene::TGLDirectRenderEvent FOnBeginParticles;
	TPFXDirectRenderEvent FOnRenderParticle;
	Glscene::TGLDirectRenderEvent FOnEndParticles;
	Glscene::TGLDirectRenderEvent FOnFinalizeRendering;
	TPFXProgressEvent FOnProgress;
	TPFXParticleProgress FOnParticleProgress;
	TPFXGetParticleCountEvent FOnGetParticleCountEvent;
	
protected:
	virtual unsigned __fastcall TexturingMode();
	virtual void __fastcall InitializeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall BeginParticles(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall RenderParticle(Glrendercontextinfo::TGLRenderContextInfo &rci, TGLParticle* aParticle);
	virtual void __fastcall EndParticles(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	virtual void __fastcall DoProgress(const Glbaseclasses::TGLProgressTimes &progressTime);
	virtual int __fastcall ParticleCount();
	
__published:
	__property Glscene::TGLDirectRenderEvent OnInitializeRendering = {read=FOnInitializeRendering, write=FOnInitializeRendering};
	__property Glscene::TGLDirectRenderEvent OnBeginParticles = {read=FOnBeginParticles, write=FOnBeginParticles};
	__property TPFXDirectRenderEvent OnRenderParticle = {read=FOnRenderParticle, write=FOnRenderParticle};
	__property Glscene::TGLDirectRenderEvent OnEndParticles = {read=FOnEndParticles, write=FOnEndParticles};
	__property Glscene::TGLDirectRenderEvent OnFinalizeRendering = {read=FOnFinalizeRendering, write=FOnFinalizeRendering};
	__property TPFXProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property TPFXParticleProgress OnParticleProgress = {read=FOnParticleProgress, write=FOnParticleProgress};
	__property TPFXGetParticleCountEvent OnGetParticleCountEvent = {read=FOnGetParticleCountEvent, write=FOnGetParticleCountEvent};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
public:
	/* TGLLifeColoredPFXManager.Create */ inline __fastcall virtual TGLCustomPFXManager(System::Classes::TComponent* aOwner) : TGLLifeColoredPFXManager(aOwner) { }
	/* TGLLifeColoredPFXManager.Destroy */ inline __fastcall virtual ~TGLCustomPFXManager() { }
	
};


class PASCALIMPLEMENTATION TGLPolygonPFXManager : public TGLLifeColoredPFXManager
{
	typedef TGLLifeColoredPFXManager inherited;
	
private:
	int FNbSides;
	Glvectorgeometry::TAffineVector Fvx;
	Glvectorgeometry::TAffineVector Fvy;
	Glvectorlists::TAffineVectorList* FVertices;
	Glvectorlists::TAffineVectorList* FVertBuf;
	
protected:
	void __fastcall SetNbSides(const int val);
	virtual unsigned __fastcall TexturingMode();
	virtual void __fastcall InitializeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall BeginParticles(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall RenderParticle(Glrendercontextinfo::TGLRenderContextInfo &rci, TGLParticle* aParticle);
	virtual void __fastcall EndParticles(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	
public:
	__fastcall virtual TGLPolygonPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLPolygonPFXManager();
	
__published:
	__property int NbSides = {read=FNbSides, write=SetNbSides, default=6};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


enum DECLSPEC_DENUM TSpriteColorMode : unsigned char { scmFade, scmInner, scmOuter, scmNone };

enum DECLSPEC_DENUM TSpritesPerTexture : unsigned char { sptOne, sptFour };

class PASCALIMPLEMENTATION TGLBaseSpritePFXManager : public TGLLifeColoredPFXManager
{
	typedef TGLLifeColoredPFXManager inherited;
	
private:
	Glcontext::TGLTextureHandle* FTexHandle;
	Glvectorgeometry::TAffineVector Fvx;
	Glvectorgeometry::TAffineVector Fvy;
	Glvectorgeometry::TAffineVector Fvz;
	Glvectorlists::TAffineVectorList* FVertices;
	Glvectorlists::TAffineVectorList* FVertBuf;
	float FAspectRatio;
	float FRotation;
	TGLBaseSpritePFXManager* FShareSprites;
	TSpritesPerTexture FSpritesPerTexture;
	TSpriteColorMode FColorMode;
	
protected:
	virtual void __fastcall PrepareImage(Glgraphics::TGLBitmap32* bmp32, int &texFormat) = 0 ;
	void __fastcall BindTexture(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall SetSpritesPerTexture(const TSpritesPerTexture val);
	void __fastcall SetColorMode(const TSpriteColorMode val);
	void __fastcall SetAspectRatio(const float val);
	bool __fastcall StoreAspectRatio();
	void __fastcall SetRotation(const float val);
	void __fastcall SetShareSprites(TGLBaseSpritePFXManager* const val);
	virtual unsigned __fastcall TexturingMode();
	virtual void __fastcall InitializeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall BeginParticles(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall RenderParticle(Glrendercontextinfo::TGLRenderContextInfo &rci, TGLParticle* aParticle);
	virtual void __fastcall EndParticles(Glrendercontextinfo::TGLRenderContextInfo &rci);
	virtual void __fastcall FinalizeRendering(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property TSpritesPerTexture SpritesPerTexture = {read=FSpritesPerTexture, write=SetSpritesPerTexture, nodefault};
	
public:
	__fastcall virtual TGLBaseSpritePFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLBaseSpritePFXManager();
	__property TSpriteColorMode ColorMode = {read=FColorMode, write=SetColorMode, nodefault};
	
__published:
	__property float AspectRatio = {read=FAspectRatio, write=SetAspectRatio, stored=StoreAspectRatio};
	__property float Rotation = {read=FRotation, write=SetRotation};
	__property TGLBaseSpritePFXManager* ShareSprites = {read=FShareSprites, write=FShareSprites};
};


typedef void __fastcall (__closure *TPFXPrepareTextureImageEvent)(System::TObject* Sender, Glgraphics::TGLBitmap32* destBmp32, int &texFormat);

class PASCALIMPLEMENTATION TGLCustomSpritePFXManager : public TGLBaseSpritePFXManager
{
	typedef TGLBaseSpritePFXManager inherited;
	
private:
	TPFXPrepareTextureImageEvent FOnPrepareTextureImage;
	
protected:
	virtual void __fastcall PrepareImage(Glgraphics::TGLBitmap32* bmp32, int &texFormat);
	
public:
	__fastcall virtual TGLCustomSpritePFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLCustomSpritePFXManager();
	
__published:
	__property TPFXPrepareTextureImageEvent OnPrepareTextureImage = {read=FOnPrepareTextureImage, write=FOnPrepareTextureImage};
	__property ColorMode = {default=1};
	__property SpritesPerTexture = {default=0};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


class PASCALIMPLEMENTATION TGLPointLightPFXManager : public TGLBaseSpritePFXManager
{
	typedef TGLBaseSpritePFXManager inherited;
	
private:
	int FTexMapSize;
	
protected:
	virtual void __fastcall PrepareImage(Glgraphics::TGLBitmap32* bmp32, int &texFormat);
	void __fastcall SetTexMapSize(const int val);
	
public:
	__fastcall virtual TGLPointLightPFXManager(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TGLPointLightPFXManager();
	
__published:
	__property int TexMapSize = {read=FTexMapSize, write=SetTexMapSize, default=5};
	__property ColorMode = {default=1};
	__property ParticleSize = {default=0};
	__property ColorInner;
	__property ColorOuter;
	__property LifeColors;
};


//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Byte cPFXNbRegions = System::Byte(0x80);
static _DELPHI_CONST System::Byte cPFXGranularity = System::Byte(0x80);
extern DELPHI_PACKAGE TGLSourcePFXEffect* __fastcall GetOrCreateSourcePFX(Glscene::TGLBaseSceneObject* obj, const System::UnicodeString name = System::UnicodeString());
}	/* namespace Glparticlefx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLPARTICLEFX)
using namespace Glparticlefx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GLParticleFXHPP
