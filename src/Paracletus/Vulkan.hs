{-# LANGUAGE Strict #-}
module Paracletus.Vulkan where
-- vulkan specific calls are made
import Prelude()
import UPrelude
import Control.Monad (forM_, when)
import Control.Monad.State.Class (gets)
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Anamnesis
import Anamnesis.Data
import Anamnesis.Event
import Anamnesis.Foreign
import Anamnesis.Util
import Artos.Except
import Artos.Var
import Paracletus.Data
import Paracletus.Oblatum
import qualified Paracletus.Oblatum.GLFW as GLFW
import Paracletus.Vulkan.Buffer
import Paracletus.Vulkan.Command
import Paracletus.Vulkan.Desc
import Paracletus.Vulkan.Device
import Paracletus.Vulkan.Draw
import Paracletus.Vulkan.Foreign
import Paracletus.Vulkan.Instance
import Paracletus.Vulkan.Pipeline
import Paracletus.Vulkan.Pres
import Paracletus.Vulkan.Shader
import Paracletus.Vulkan.Tile
import Paracletus.Vulkan.Texture
import Paracletus.Vulkan.Trans
import Paracletus.Vulkan.Vertex

runParacVulkan ∷ Anamnesis ε σ ()
runParacVulkan = do
  windowSizeChanged ← liftIO $ atomically $ newTVar False
  logInfo $ "beginning paracletus"
  window ← initGLFWWindow 1280 720 "paracletus" windowSizeChanged
  vulkanInstance ← createGLFWVulkanInstance "paracletus-instance"
  vulkanSurface ← createSurface vulkanInstance window
  logDebug $ "created surface: " ⧺ show vulkanSurface
  -- forks GLFW as parent
  glfwWaitEventsMeanwhile $ do
    (_, pdev) ← pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logDebug $ "selected physical device: " ⧺ show pdev
    msaaSamples ← getMaxUsableSampleCount pdev
    (dev, queues) ← createGraphicsDevice pdev vulkanSurface
    logDebug $ "created device: " ⧺ show dev
    logDebug $ "created queues: " ⧺ show queues
    (shaderVert, shaderFrag) ← makeShader dev
    logDebug $ "created vertex shader module: " ⧺ show shaderVert
    logDebug $ "created fragment shader module: " ⧺ show shaderFrag
    frameIndexRef      ← liftIO $ atomically $ newTVar 0
    renderFinishedSems ← createFrameSemaphores dev
    imageAvailableSems ← createFrameSemaphores dev
    inFlightFences     ← createFrameFences     dev
    commandPool        ← createCommandPool     dev queues
    logDebug $ "created command pool: " ⧺ show commandPool
    imgIndexPtr ← mallocRes
    let nimages = 3
    descriptorSetLayout ← createDescriptorSetLayout dev nimages
    pipelineLayout ← createPipelineLayout dev descriptorSetLayout
    let tex1Path = "dat/tex/texture1.png"
        tex2Path = "dat/tex/texture2.png"
        texAlph  = "dat/tex/alph.png"
    (textureView1, mipLevels1) ← createTextureImageView pdev dev commandPool (graphicsQueue queues) tex1Path
    (textureView2, mipLevels2) ← createTextureImageView pdev dev commandPool (graphicsQueue queues) tex2Path
    (texViewAlph, mipLevelsAlph) ← createTextureImageView pdev dev commandPool (graphicsQueue queues) texAlph
    textureSampler1 ← createTextureSampler dev mipLevels1
    textureSampler2 ← createTextureSampler dev mipLevels2
    texSamplerAlph  ← createTextureSampler dev mipLevelsAlph
    descriptorTextureInfo ← textureImageInfos [textureView1, textureView2, texViewAlph] [textureSampler1, textureSampler2, texSamplerAlph]
    depthFormat ← findDepthFormat pdev
    -- wait when minimized
    let beforeSwapchainCreation ∷ Anamnesis ε σ ()
        beforeSwapchainCreation = liftIO $ atomically $ writeTVar windowSizeChanged False
    loop $ do
      ds ← gets drawSt
      logDebug "creating new swapchain..."
      scsd ← querySwapchainSupport pdev vulkanSurface
      beforeSwapchainCreation
      swapInfo ← createSwapchain dev scsd queues vulkanSurface
      let swapchainLen = length (swapImgs swapInfo)
      (transObjMems, transObjBufs) ← unzip ⊚ createTransObjBuffers pdev dev swapchainLen
      descriptorBufferInfos ← mapM transObjBufferInfo transObjBufs
      descriptorPool ← createDescriptorPool dev swapchainLen nimages
      descriptorSetLayouts ← newArrayRes $ replicate swapchainLen descriptorSetLayout
      descriptorSets ← createDescriptorSets dev descriptorPool swapchainLen descriptorSetLayouts
      forM_ (zip descriptorBufferInfos descriptorSets) $ \(bufInfo, dSet) → prepareDescriptorSet dev bufInfo descriptorTextureInfo dSet nimages
      transObjMemories ← newArrayRes transObjMems
      imgViews ← mapM (\image → createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
      logDebug $ "created image views: " ⧺ show imgViews
      renderPass ← createRenderPass dev swapInfo depthFormat msaaSamples
      logDebug $ "created renderpass: " ⧺ show renderPass
      graphicsPipeline ← createGraphicsPipeline dev swapInfo vertIBD vertIADs [shaderVert, shaderFrag] renderPass pipelineLayout msaaSamples
      logDebug $ "created pipeline: " ⧺ show graphicsPipeline
      colorAttImgView ← createColorAttImgView pdev dev commandPool (graphicsQueue queues) (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
      depthAttImgView ← createDepthAttImgView pdev dev commandPool (graphicsQueue queues) (swapExtent swapInfo) msaaSamples
      framebuffers ← createFramebuffers dev renderPass swapInfo imgViews depthAttImgView colorAttImgView
      logDebug $ "created framebuffers: " ⧺ show framebuffers
      let (verts, inds) = calcVertices ds
      vertexBuffer ← createVertexBuffer pdev dev commandPool (graphicsQueue queues) verts
      indexBuffer ← createIndexBuffer pdev dev commandPool (graphicsQueue queues) inds

      cmdBuffersPtr0 ← createCommandBuffers dev graphicsPipeline commandPool renderPass pipelineLayout swapInfo vertexBuffer (dfLen inds, indexBuffer) framebuffers descriptorSets
      cmdBuffers ← peekArray swapchainLen cmdBuffersPtr0
      logDebug $ "created command buffers: " ⧺ show cmdBuffers
      shouldExit ← glfwMainLoop window $ do
        -- changes command buffer when
        -- the state changes
        cmdBP ← do
            dsNew ← gets drawSt
            let (verts0, inds0) = calcVertices dsNew
            vertexBufferNew ← createVertexBuffer pdev dev commandPool (graphicsQueue queues) verts0
            indexBufferNew ← createIndexBuffer pdev dev commandPool (graphicsQueue queues) inds0
            newCmdBP ← createCommandBuffers dev graphicsPipeline commandPool renderPass pipelineLayout swapInfo vertexBufferNew (dfLen inds0, indexBufferNew) framebuffers descriptorSets
            -- for now just recreate command
            -- buffers every frame
            --modify' $ \s → s { stateChanged = False }
            return newCmdBP
        let rdata = RenderData { dev
                               , swapInfo
                               , queues
                               , imgIndexPtr
                               , frameIndexRef
                               , renderFinishedSems
                               , imageAvailableSems
                               , inFlightFences
                               , cmdBuffersPtr = cmdBP
                               , memories = transObjMemories
                               , memoryMutator = updateTransObj dev (swapExtent swapInfo) }
        liftIO $ GLFW.pollEvents
        needRecreation ← drawFrame rdata `catchError` (\err → case (testEx err VK_ERROR_OUT_OF_DATE_KHR) of
          -- when khr out of date,
          -- recreate swapchain
          True → do
            _ ← logDebug $ "vulkan khr out of date"
            return True
          _    → logExcept ParacError ExParacletus "unknown drawFrame error" )
          -- _    → logExcept err ExParacletus "unknown drawFrame error" )
        sizeChanged ← liftIO $ atomically $ readTVar windowSizeChanged
        when sizeChanged $ logDebug "glfw window size callback"
        -- this is for key input
        processEvents
        runVk $ vkDeviceWaitIdle dev
        return $ if needRecreation ∨ sizeChanged then AbortLoop else ContinueLoop
      -- loop ends, now deallocate
      return $ if shouldExit then AbortLoop else ContinueLoop
  return ()
