{-# LANGUAGE Strict #-}
module Paracletus.Vulkan where
-- vulkan specific calls are made
import Prelude()
import UPrelude
import Control.Monad (forM_, when)
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Numeric.DataFrame
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
    vertexBuffer ← createVertexBuffer pdev dev commandPool (graphicsQueue queues) vertices
    indexBuffer ← createIndexBuffer pdev dev commandPool (graphicsQueue queues) indices
    pcPtr ← createPushConstants 1
    descriptorSetLayout ← createDescriptorSetLayout dev
    pipelineLayout ← createPipelineLayout dev descriptorSetLayout
    let tex1Path = "dat/tex/texture1.png"
        tex2Path = "dat/tex/texture2.png"
    (textureView1, mipLevels1) ← createTextureImageView pdev dev commandPool (graphicsQueue queues) tex1Path
    (textureView2, mipLevels2) ← createTextureImageView pdev dev commandPool (graphicsQueue queues) tex2Path
    textureSampler1 ← createTextureSampler dev mipLevels1
    textureSampler2 ← createTextureSampler dev mipLevels2
    descriptorTextureInfo ← textureImageInfo textureView1 textureSampler1 textureView2 textureSampler2
    depthFormat ← findDepthFormat pdev
    -- wait when minimized
    let beforeSwapchainCreation ∷ Anamnesis ε σ ()
        beforeSwapchainCreation = liftIO $ atomically $ writeTVar windowSizeChanged False
    loop $ do
      logDebug "creating new swapchain..."
      scsd ← querySwapchainSupport pdev vulkanSurface
      beforeSwapchainCreation
      swapInfo ← createSwapchain dev scsd queues vulkanSurface
      let swapchainLen = length (swapImgs swapInfo)
      (transObjMems, transObjBufs) ← unzip ⊚ createTransObjBuffers pdev dev swapchainLen
      descriptorBufferInfos ← mapM transObjBufferInfo transObjBufs
      descriptorPool ← createDescriptorPool dev swapchainLen
      descriptorSetLayouts ← newArrayRes $ replicate swapchainLen descriptorSetLayout
      descriptorSets ← createDescriptorSets dev descriptorPool swapchainLen descriptorSetLayouts
      forM_ (zip descriptorBufferInfos descriptorSets) $ \(bufInfo, dSet) → prepareDescriptorSet dev bufInfo descriptorTextureInfo dSet
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
      cmdBuffersPtr ← createCommandBuffers dev graphicsPipeline commandPool renderPass pipelineLayout swapInfo vertexBuffer (dfLen indices, indexBuffer) pcPtr framebuffers descriptorSets
      let rdata = RenderData { dev
                             , swapInfo
                             , queues
                             , imgIndexPtr
                             , frameIndexRef
                             , renderFinishedSems
                             , imageAvailableSems
                             , inFlightFences
                             , cmdBuffersPtr
                             , memories = transObjMemories
                             , memoryMutator = updateTransObj dev (swapExtent swapInfo) }
      cmdBuffers ← peekArray swapchainLen cmdBuffersPtr
      logDebug $ "created command buffers: " ⧺ show cmdBuffers
      shouldExit ← glfwMainLoop window $ do
        -- logic
        st ← get
        let rdata' = rdata { memoryMutator = updateTransObj dev (swapExtent swapInfo) }
        liftIO $ GLFW.pollEvents
        needRecreation ← drawFrame rdata' `catchError` (\err → case (testEx err VK_ERROR_OUT_OF_DATE_KHR) of
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
        return $ if needRecreation ∨ sizeChanged then AbortLoop else ContinueLoop
      -- loop ends, now deallocate
      runVk $ vkDeviceWaitIdle dev
      return $ if shouldExit then AbortLoop else ContinueLoop
  return ()

-- these is placeholder data
vertices ∷ DataFrame Vertex '[XN 3]
vertices = XFrame $ square `appendDF` withPos (+ vec4 2 0 0 0) square `appendDF` withPos (+ vec4 0 2 0 0) square
  where square ∷ Vector Vertex 4
        square = fromFlatList (D4 :* U) (Vertex 0 0 0)
          [ Vertex (vec3 (-1) (-1) 0) (vec3 1 0 0) (vec2 0 1)
          , Vertex (vec3   1  (-1) 0) (vec3 0 1 0) (vec2 1 1)
          , Vertex (vec3   1    1  0) (vec3 0 0 1) (vec2 1 0)
          , Vertex (vec3 (-1)   1  0) (vec3 1 1 1) (vec2 0 0) ]
        withPos ∷ (Vec4f → Vec4f) → Vector Vertex 4 → Vector Vertex 4
        withPos f = ewmap (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
indices ∷ DataFrame Word32 '[XN 3]
indices = atLeastThree $ fromList $ oneRectIndices ⧺ map (+4) oneRectIndices ⧺ map (+8) oneRectIndices
  where oneRectIndices = [0,3,2,2,1,0]
