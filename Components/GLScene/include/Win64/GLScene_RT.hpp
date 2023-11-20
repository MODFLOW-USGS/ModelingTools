// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLScene_RT.dpk' rev: 35.00 (Windows)

#ifndef Glscene_rtHPP
#define Glscene_rtHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <Const3DS.hpp>
#include <DDSImage.hpp>
#include <DXTC.hpp>
#include <File3DS.hpp>
#include <FileB3D.hpp>
#include <FileGL2.hpp>
#include <FileMD2.hpp>
#include <FileMD3.hpp>
#include <FileOCT.hpp>
#include <FileTGA.hpp>
#include <FileX.hpp>
#include <GLAVIRecorder.hpp>
#include <GLAnimatedSprite.hpp>
#include <GLAnimationUtils.hpp>
#include <GLApplicationFileIO.hpp>
#include <GLAsmShader.hpp>
#include <GLAsyncHDS.hpp>
#include <GLAsyncTimer.hpp>
#include <GLAtmosphere.hpp>
#include <GLBSP.hpp>
#include <GLBaseClasses.hpp>
#include <GLBaseMeshSilhouette.hpp>
#include <GLBehaviours.hpp>
#include <GLBitmapFont.hpp>
#include <GLBlur.hpp>
#include <GLBumpMapping.hpp>
#include <GLBumpShader.hpp>
#include <GLBumpmapHDS.hpp>
#include <GLCadencer.hpp>
#include <GLCameraController.hpp>
#include <GLCanvas.hpp>
#include <GLCelShader.hpp>
#include <GLCollision.hpp>
#include <GLColor.hpp>
#include <GLCompositeImage.hpp>
#include <GLConsole.hpp>
#include <GLContext.hpp>
#include <GLCoordinates.hpp>
#include <GLCrossPlatform.hpp>
#include <GLCurvesAndSurfaces.hpp>
#include <GLCustomShader.hpp>
#include <GLDCE.hpp>
#include <GLDCEMisc.hpp>
#include <GLDynamicTexture.hpp>
#include <GLEParticleMasksManager.hpp>
#include <GLEllipseCollision.hpp>
#include <GLExplosionFx.hpp>
#include <GLExtrusion.hpp>
#include <GLFBO.hpp>
#include <GLFBORenderer.hpp>
#include <GLFPSMovement.hpp>
#include <GLFeedback.hpp>
#include <GLFile3DPDF.hpp>
#include <GLFile3DS.hpp>
#include <GLFile3DSSceneObjects.hpp>
#include <GLFileASE.hpp>
#include <GLFileB3D.hpp>
#include <GLFileBMP.hpp>
#include <GLFileDDS.hpp>
#include <GLFileDXF.hpp>
#include <GLFileGL2.hpp>
#include <GLFileGLB.hpp>
#include <GLFileGLTF.hpp>
#include <GLFileGRD.hpp>
#include <GLFileGTS.hpp>
#include <GLFileHDR.hpp>
#include <GLFileJPEG.hpp>
#include <GLFileLMTS.hpp>
#include <GLFileLWO.hpp>
#include <GLFileMD2.hpp>
#include <GLFileMD3.hpp>
#include <GLFileMD5.hpp>
#include <GLFileMDC.hpp>
#include <GLFileMP3.hpp>
#include <GLFileMS3D.hpp>
#include <GLFileNMF.hpp>
#include <GLFileNurbs.hpp>
#include <GLFileO3TC.hpp>
#include <GLFileOBJ.hpp>
#include <GLFileOCT.hpp>
#include <GLFilePAK.hpp>
#include <GLFilePLY.hpp>
#include <GLFilePNG.hpp>
#include <GLFileQ3BSP.hpp>
#include <GLFileSMD.hpp>
#include <GLFileSTL.hpp>
#include <GLFileTGA.hpp>
#include <GLFileTIN.hpp>
#include <GLFileVRML.hpp>
#include <GLFileWAV.hpp>
#include <GLFileX.hpp>
#include <GLFileZLIB.hpp>
#include <GLFireFX.hpp>
#include <GLFullScreenViewer.hpp>
#include <GLGLUTesselation.hpp>
#include <GLGameMenu.hpp>
#include <GLGeomObjects.hpp>
#include <GLGeometryBB.hpp>
#include <GLGeometryCoordinates.hpp>
#include <GLGizmo.hpp>
#include <GLGizmoEx.hpp>
#include <GLGraph.hpp>
#include <GLGraphics.hpp>
#include <GLGui.hpp>
#include <GLHUDObjects.hpp>
#include <GLHeightData.hpp>
#include <GLHeightTileFile.hpp>
#include <GLHeightTileFileHDS.hpp>
#include <GLHiddenLineShader.hpp>
#include <GLImageUtils.hpp>
#include <GLImposter.hpp>
#include <GLIsolines.hpp>
#include <GLIsosurface.hpp>
#include <GLJoystick.hpp>
#include <GLKeyboard.hpp>
#include <GLLensFlare.hpp>
#include <GLLinePFX.hpp>
#include <GLManager.hpp>
#include <GLMaterial.hpp>
#include <GLMaterialEx.hpp>
#include <GLMaterialMultiProxy.hpp>
#include <GLMaterialScript.hpp>
#include <GLMesh.hpp>
#include <GLMeshBuilder.hpp>
#include <GLMeshCSG.hpp>
#include <GLMeshLines.hpp>
#include <GLMeshOptimizer.hpp>
#include <GLMeshUtils.hpp>
#include <GLMirror.hpp>
#include <GLMovement.hpp>
#include <GLMultiMaterialShader.hpp>
#include <GLMultiPolygon.hpp>
#include <GLMultiProxy.hpp>
#include <GLMultisampleImage.hpp>
#include <GLNavigator.hpp>
#include <GLNodes.hpp>
#include <GLObjectManager.hpp>
#include <GLObjects.hpp>
#include <GLOctree.hpp>
#include <GLOutlineShader.hpp>
#include <GLParametricSurfaces.hpp>
#include <GLParticleFX.hpp>
#include <GLParticles.hpp>
#include <GLPerlin.hpp>
#include <GLPerlinBase.hpp>
#include <GLPerlinNoise3D.hpp>
#include <GLPerlinPFX.hpp>
#include <GLPersistentClasses.hpp>
#include <GLPhongShader.hpp>
#include <GLPictureRegisteredFormats.hpp>
#include <GLPipelineTransformation.hpp>
#include <GLPlugInManager.hpp>
#include <GLPlugInIntf.hpp>
#include <GLPolyhedron.hpp>
#include <GLPolynomials.hpp>
#include <GLPortal.hpp>
#include <GLPostEffects.hpp>
#include <GLProcTextures.hpp>
#include <GLProjectedTextures.hpp>
#include <GLProxyObjects.hpp>
#include <GLROAMPatch.hpp>
#include <GLRagdoll.hpp>
#include <GLRandomHDS.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLSArchiveManager.hpp>
#include <GLSCrossXML.hpp>
#include <GLSGenerics.hpp>
#include <GLSLBumpShader.hpp>
#include <GLSLDiffuseSpecularShader.hpp>
#include <GLSLErosionShader.hpp>
#include <GLSLFurShader.hpp>
#include <GLSLGlassShader.hpp>
#include <GLSLGoochShader.hpp>
#include <GLSLIvoryShader.hpp>
#include <GLSLLatticeShader.hpp>
#include <GLSLParameter.hpp>
#include <GLSLPostShaders.hpp>
#include <GLSLProjectedTextures.hpp>
#include <GLSLSemShader.hpp>
#include <GLSLShader.hpp>
#include <GLSLToonShader.hpp>
#include <GLSLanguage.hpp>
#include <GLSLog.hpp>
#include <GLSMemo.hpp>
#include <GLSRGBE.hpp>
#include <GLSRedBlackTree.hpp>
#include <GLSVfw.hpp>
#include <GLScene.hpp>
#include <GLSceneForm.hpp>
#include <GLScreen.hpp>
#include <GLScreenSaver.hpp>
#include <GLSelection.hpp>
#include <GLShaderCombiner.hpp>
#include <GLShadowHDS.hpp>
#include <GLShadowPlane.hpp>
#include <GLShadowVolume.hpp>
#include <GLSilhouette.hpp>
#include <GLSimpleNavigation.hpp>
#include <GLSkyBox.hpp>
#include <GLSkydome.hpp>
#include <GLSLVertexDisplacementShader.hpp>
#include <GLSmoothNavigator.hpp>
#include <GLSound.hpp>
#include <GLSoundFileObjects.hpp>
#include <GLSpacePartition.hpp>
#include <GLSpaceText.hpp>
#include <GLSpatialPartitioning.hpp>
#include <GLSpline.hpp>
#include <GLState.hpp>
#include <GLStrings.hpp>
#include <GLTeapot.hpp>
#include <GLTerrainRenderer.hpp>
#include <GLTexCombineShader.hpp>
#include <GLTexLensFlare.hpp>
#include <GLTexture.hpp>
#include <GLTextureCombiners.hpp>
#include <GLTextureFormat.hpp>
#include <GLTextureImageEditors.hpp>
#include <GLTextureSharingShader.hpp>
#include <GLTexturedHDS.hpp>
#include <GLThorFX.hpp>
#include <GLTilePlane.hpp>
#include <GLTimeEventsMgr.hpp>
#include <GLTrail.hpp>
#include <GLTree.hpp>
#include <GLTriangulation.hpp>
#include <GLTypes.hpp>
#include <GLUserShader.hpp>
#include <GLUtils.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLVectorGeometry.hpp>
#include <GLVectorLists.hpp>
#include <GLVectorTypes.hpp>
#include <GLVerletClothify.hpp>
#include <GLVerletHairClasses.hpp>
#include <GLVerletSkeletonColliders.hpp>
#include <GLVerletTypes.hpp>
#include <GLVfsPAK.hpp>
#include <GLWaterPlane.hpp>
#include <GLWin32Context.hpp>
#include <GLWin32Viewer.hpp>
#include <GLWindows.hpp>
#include <GLWindowsFont.hpp>
#include <GLXCollection.hpp>
#include <GLzBuffer.hpp>
#include <HDRImage.hpp>
#include <LWObjects.hpp>
#include <O3TCImage.hpp>
#include <OpenGL1x.hpp>
#include <OpenGLAdapter.hpp>
#include <OpenGLTokens.hpp>
#include <Q3BSP.hpp>
#include <Q3MD3.hpp>
#include <Types3DS.hpp>
#include <Utils3DS.hpp>
#include <VRMLParser.hpp>
#include <XOpenGL.hpp>
#include <System.Types.hpp>	// (rtl)
#include <System.UITypes.hpp>	// (rtl)
#include <Winapi.Windows.hpp>	// (rtl)
#include <Winapi.Messages.hpp>	// (rtl)
#include <System.SysConst.hpp>	// (rtl)
#include <Winapi.ImageHlp.hpp>	// (rtl)
#include <Winapi.SHFolder.hpp>	// (rtl)
#include <Winapi.PsAPI.hpp>	// (rtl)
#include <System.RTLConsts.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <Winapi.ActiveX.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <System.Hash.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.Generics.Collections.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.Diagnostics.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <Winapi.Wincodec.hpp>	// (rtl)
#include <Winapi.CommCtrl.hpp>	// (rtl)
#include <Winapi.Qos.hpp>	// (rtl)
#include <Winapi.Winsock2.hpp>	// (rtl)
#include <Winapi.IpExport.hpp>	// (rtl)
#include <Winapi.ShellAPI.hpp>	// (rtl)
#include <Winapi.RegStr.hpp>	// (rtl)
#include <Winapi.WinInet.hpp>	// (rtl)
#include <Winapi.UrlMon.hpp>	// (rtl)
#include <Winapi.ObjectArray.hpp>	// (rtl)
#include <Winapi.StructuredQueryCondition.hpp>	// (rtl)
#include <Winapi.PropSys.hpp>	// (rtl)
#include <Winapi.MSXMLIntf.hpp>	// (rtl)
#include <Winapi.ShlObj.hpp>	// (rtl)
#include <Winapi.KnownFolders.hpp>	// (rtl)
#include <System.Masks.hpp>	// (rtl)
#include <System.StrUtils.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <System.UIConsts.hpp>	// (rtl)
#include <Vcl.Consts.hpp>	// (vcl)
#include <Vcl.Graphics.hpp>	// (vcl)
#include <System.SyncObjs.hpp>	// (rtl)
#include <System.Win.Crtl.hpp>	// (rtl)
#include <System.ZLib.hpp>	// (rtl)
#include <Vcl.Imaging.pnglang.hpp>	// (vclimg)
#include <Vcl.Imaging.pngimage.hpp>	// (vclimg)
#include <Winapi.MultiMon.hpp>	// (rtl)
#include <System.Contnrs.hpp>	// (rtl)
#include <System.ImageList.hpp>	// (rtl)
#include <System.Messaging.hpp>	// (rtl)
#include <System.Actions.hpp>	// (rtl)
#include <Winapi.Imm.hpp>	// (rtl)
#include <Vcl.ActnList.hpp>	// (vcl)
#include <System.HelpIntfs.hpp>	// (rtl)
#include <Winapi.UxTheme.hpp>	// (rtl)
#include <Winapi.Dwmapi.hpp>	// (rtl)
#include <Vcl.GraphUtil.hpp>	// (vcl)
#include <Vcl.StdCtrls.hpp>	// (vcl)
#include <System.MaskUtils.hpp>	// (rtl)
#include <Vcl.Clipbrd.hpp>	// (vcl)
#include <Vcl.Mask.hpp>	// (vcl)
#include <Winapi.CommDlg.hpp>	// (rtl)
#include <Winapi.WinSpool.hpp>	// (rtl)
#include <Vcl.Printers.hpp>	// (vcl)
#include <Winapi.RichEdit.hpp>	// (rtl)
#include <Vcl.ToolWin.hpp>	// (vcl)
#include <Vcl.ListActns.hpp>	// (vcl)
#include <Vcl.ComStrs.hpp>	// (vcl)
#include <Vcl.StdActns.hpp>	// (vcl)
#include <Vcl.ComCtrls.hpp>	// (vcl)
#include <System.WideStrUtils.hpp>	// (rtl)
#include <Winapi.Dlgs.hpp>	// (rtl)
#include <Vcl.Dialogs.hpp>	// (vcl)
#include <Vcl.ExtCtrls.hpp>	// (vcl)
#include <Vcl.Themes.hpp>	// (vcl)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <System.Win.ComConst.hpp>	// (rtl)
#include <System.Win.ComObj.hpp>	// (rtl)
#include <System.Win.Taskbar.hpp>	// (rtl)
#include <System.Win.TaskbarCore.hpp>	// (rtl)
#include <Winapi.ShellScaling.hpp>	// (rtl)
#include <Winapi.FlatSB.hpp>	// (rtl)
#include <Vcl.Forms.hpp>	// (vcl)
#include <Vcl.ImgList.hpp>	// (vcl)
#include <Vcl.Menus.hpp>	// (vcl)
#include <Winapi.TpcShrd.hpp>	// (rtl)
#include <Winapi.MsCTF.hpp>	// (rtl)
#include <Winapi.MsInkAut.hpp>	// (rtl)
#include <Winapi.PenInputPanel.hpp>	// (rtl)
#include <Vcl.Controls.hpp>	// (vcl)
#include <GR32_System.hpp>
#include <GR32_Bindings.hpp>
#include <GR32_Blend.hpp>
#include <GR32_Math.hpp>
#include <GR32_LowLevel.hpp>
#include <GR32_Filters.hpp>
#include <GR32_VectorMaps.hpp>
#include <GR32_OrdinalMaps.hpp>
#include <GR32_Containers.hpp>
#include <GR32_Rasterizers.hpp>
#include <GR32_Transforms.hpp>
#include <GR32_Resamplers.hpp>
#include <GR32_RepaintOpt.hpp>
#include <GR32_Layers.hpp>
#include <GR32_XPThemes.hpp>
#include <GR32_RangeBars.hpp>
#include <GR32_MicroTiles.hpp>
#include <GR32_Image.hpp>
#include <GR32_Backends.hpp>
#include <GR32_Backends_Generic.hpp>
#include <GR32_Backends_VCL.hpp>
#include <GR32_DrawingEx.hpp>
#include <GR32.hpp>
#include <Vcl.Buttons.hpp>	// (vcl)
#include <Vcl.ExtDlgs.hpp>	// (vcl)
#include <Winapi.MMSystem.hpp>	// (rtl)
#include <Vcl.Imaging.JConsts.hpp>	// (vclimg)
#include <Vcl.Imaging.jpeg.hpp>	// (vclimg)
#include <System.Math.Vectors.hpp>	// (rtl)
#include <Winapi.OpenGL.hpp>	// (rtl)
#include <Xml.XMLConst.hpp>	// (xmlrtl)
#include <Winapi.msxml.hpp>	// (rtl)
#include <Xml.Win.msxmldom.hpp>	// (xmlrtl)
#include <Xml.xmldom.hpp>	// (xmlrtl)
#include <Xml.XMLIntf.hpp>	// (xmlrtl)
#include <Xml.XMLSchemaTags.hpp>	// (xmlrtl)
#include <Xml.XMLSchema.hpp>	// (xmlrtl)
#include <Xml.xmlutil.hpp>	// (xmlrtl)
#include <Xml.XMLDoc.hpp>	// (xmlrtl)
// PRG_EXT: .bpl
// BPI_DIR: ..\..\lib\Win64
// OBJ_DIR: ..\..\lib\Win64
// OBJ_EXT: .o

//-- user supplied -----------------------------------------------------------

namespace Glscene_rt
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glscene_rt */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCENE_RT)
using namespace Glscene_rt;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Glscene_rtHPP
