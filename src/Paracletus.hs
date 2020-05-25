module Paracletus where
-- a graphics layer is chosen,
-- a GLFW instance is begun
import Prelude()
import UPrelude
import Control.Monad.Reader.Class (asks)
import Artos.Log
import Anamnesis
import Anamnesis.Data
import Numeric.DataFrame
import Paracletus.Data
import Paracletus.Util
import Paracletus.GLFW
import Paracletus.Vulkan
import Paracletus.Vulkan.Command
import Paracletus.Vulkan.Device
import Paracletus.Vulkan.Surface
import Paracletus.Vulkan.Shader
import Paracletus.Vulkan.Vertex
-- a generic action is run in a
-- MProg context, returning ()
runParacletus ∷ GraphicsLayer → Anamnesis ret env state ()
runParacletus Vulkan = do
  inputQueue ← asks envEventsChan
  logInfo $ "beginning paracletus..."
  window ← initGLFWWindow Vulkan 1280 720 "paracletus" inputQueue
  vulkanInstance ← createGLFWVulkanInstance "paracletus-instance"
  vulkanSurface ← createSurface vulkanInstance window
  logDebug $ "createdSurface: " ⧺ show vulkanSurface
  -- fork thread for GLFW
  glfwWaitEventsMeanwhile $ do
    logDebug $ "glfw thread begun..."
    (_, pdev) ← pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logDebug $ "selected physical device: " ⧺ show pdev
    msaaSamples ← getMaxUsableSampleCount pdev
    (dev, queues) ← createGraphicsDevice pdev vulkanSurface
    logDebug $ "created device: " ⧺ show dev
    logDebug $ "created queues: " ⧺ show queues
    (shaderVert, shaderFrag) ← makeShader dev
    logDebug $ "created vertex shader module: " ⧺ show shaderVert
    logDebug $ "created fragment shader module: " ⧺ show shaderFrag
    commandPool ← createCommandPool dev queues
    logDebug $ "created command pool: " ⧺ show commandPool
--    vertexBuffer ← pdev dev commandPool (graphicsQueue queues) vertices
--    indexBuffer ← createIndexBuffer pdev dev commandPool (graphicsQueue queues) indices
--    descriptorSetLayout ← createDescriptorSetLayout dev
--    pipelineLayout ← createPipelineLayout dev descriptorSetLayout
--    texturePath = "dat/tex/texture.jpg"
--    (textureView, mipLevels) ← createTextureImageView pdev dev commandPool (graphicsQueue queues) texturePath
--    textureSampler ← createTextureSampler dev mipLevels
--    descriptorTextureInfo ← textureImageInfo textureView textureSampler
--    depthFormat ← findDepthFormat
--    loop $ do
--      logDebug $ "creating new swapchain..."
--      scsd ← querySwapchainSupport pdev vulkanSurface
--      swapInfo ← createSwapchain dev scsd queues vulkanSurface
--      let swapchainLen = length $ swapImgs swapInfo
--      (transObjMems, transObjBufs) ← unzip ⊚ createTransObjBuffers pdev dev swapchainLen
--      descriptorBufferInfos ← mapM transObjBufferInfo transObjBufs
--      descriptorPool ← createDescriptorPool dev swapchainLen
--      descriptorSetLayout ← newArrayRes $ replicate swapchainLen descriptorSetLayout
--      descriptorSets ← createDescriptorSets dev descriptorPool swapchainLen descriptorSetLayouts
--      forM_ (zip descriptorBufferInfos descriptorSets) $ \(bufInfo, dSet) → prepareDescriptorSet dev bufInfo descriptorTextureInfo dSet
--      transObjMemories ← newArrayRes transObjMems
--      imgViews ← mapM (\image → createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
--      logDebug $ "created image views " ⧺ show imgViews
--
--      renderPass ← createRenderPass dev swapInfo depthFormat msaaSamples
--      logDebug $ "created renderpass: " ⧺ show renderPass
--      graphicsPipeline ← createGraphicsPipeline dev swapInfo vertIBD vertIADs [shaderVert, shaderFrag] renderPass pipelineLayout msaaSamples
--      logDebug $ "created pipeline: " ⧺ show graphicsPipeline
--      colorAttImgView ← createColorAttImgView pdev commandPool (graphicsQueue queues) (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
--      depthAttImgView ← createDepthAttImgView pdev dev commandPool (graphicsQueue queues) (swapExtent swapInfo) msaaSamples
--      framebuffers ← createFramebuffers dev renderPass swapInfo imgViews depthAttImgView colorAttImgView
--      logDebug $ "created framebuffers: " ⧺ show framebuffers
--      cmdBuffersPtr ← createCommandBuffers dev graphicsPipeline commandPool renderPass pipelineLayout commandPool vertexBuffer (dfLen indices, indexBuffer) framebuffers descriptorSets
--      cmdBuffers ← peekArray swapchainLen cmdBuffersPtr
--      logDebug $ " created commandBuffers: " ⧺ show cmdBuffers
--      shouldExit ← glfwMainLoop window $ do
--        --logic
--        needRecreation ← drawFrame rdata `catchError` ( \err@(VulkanException ecode _) → case ecode of
--          Just VK_ERROR_OUT_OF_DATE_KHR → do
--            logError $ "vulkan khr out of date"
--            return True
--          _ → throwError err
--        )
--        return $ if needRecreation ∨ sizeChanged then AbortLoop else ContinueLoop
--      runVk $ vkDeviceWaitIdle dev
--      return $ if shouldExit then AbortLoop else ContinueLoop
  return ()
runParacletus _ = logExcept ParacError $ "unsuzzpported graphics layer..."
-- placeholder helper functions
vertices ∷ DataFrame Vertex '[XN 3]
vertices = XFrame $ square `appendDF` withPos (+ vec4 0 0 0.5 0) square `appendDF` withPos (\p → p %* rotateX (π/2) + vec4 0 0 (-0.5) 0) square
  where square ∷ Vector Vertex 4
        square = fromFlatList (D4 :* U) (Vertex 0 0 0)
          [ Vertex (vec3 (-0.5) (-0.5) 0) (vec3 1 0 0) (vec2 0 0)
          , Vertex (vec3   0.4  (-0.5) 0) (vec3 0 1 0) (vec2 1 0)
          , Vertex (vec3   0.4    0.4  0) (vec3 0 0 1) (vec2 1 1)
          , Vertex (vec3 (-0.5)   0.4  0) (vec3 1 1 1) (vec2 0 1) ]
        withPos ∷ (Vec4f → Vec4f) → Vector Vertex 4 → Vector Vertex 4
        withPos f = ewmap (\(S v) → S v { pos = fromHom ∘ f ∘ toHomPoint $ pos v })
