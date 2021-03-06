{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan where
-- vulkan specific calls are made
import Prelude()
import UPrelude
import Control.Concurrent (forkIO)
import Control.Monad (forM_, when)
import Control.Monad.State.Class (gets, modify)
import Data.List (zip4)
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Anamnesis
import Anamnesis.Data
import Anamnesis.Event
import Anamnesis.Foreign
import Anamnesis.Util
import Artos.Data
import Artos.Except
import Artos.Var
import Artos.Queue
import Epiklesis.Data
import Epiklesis.Lua
import Paracletus.Data
import Paracletus.Load
import Paracletus.Oblatum
import qualified Paracletus.Oblatum.GLFW as GLFW
import Paracletus.Vulkan.Buffer
import Paracletus.Vulkan.Calc
import Paracletus.Vulkan.Command
import Paracletus.Vulkan.Data
import Paracletus.Vulkan.Desc
import Paracletus.Vulkan.Device
import Paracletus.Vulkan.Draw
import Paracletus.Vulkan.Foreign
import Paracletus.Vulkan.Instance
import Paracletus.Vulkan.Load
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
  modify $ \s → s { windowSt = Just window }
  vulkanInstance ← createGLFWVulkanInstance "paracletus-instance"
  vulkanSurface ← createSurface vulkanInstance window
  --logDebug $ "created surface: " ⧺ show vulkanSurface
  -- forks GLFW as parent
  glfwWaitEventsMeanwhile $ do
    (_, pdev) ← pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    --logDebug $ "selected physical device: " ⧺ show pdev
    msaaSamples ← getMaxUsableSampleCount pdev
    (dev, queues) ← createGraphicsDevice pdev vulkanSurface
    --logDebug $ "created device: " ⧺ show dev
    --logDebug $ "created queues: " ⧺ show queues
    (shaderVert, shaderFrag) ← makeShader dev
    --(tshaderVert, tshaderFrag) ← makeTShader dev
    --logDebug $ "created vertex shader module: " ⧺ show shaderVert
    --logDebug $ "created fragment shader module: " ⧺ show shaderFrag
    frameIndexRef      ← liftIO $ atomically $ newTVar 0
    renderFinishedSems ← createFrameSemaphores dev
    imageAvailableSems ← createFrameSemaphores dev
    inFlightFences     ← createFrameFences     dev
    commandPool        ← createCommandPool     dev queues
    --logDebug $ "created command pool: " ⧺ show commandPool
    imgIndexPtr ← mallocRes
    let gqdata = GQData pdev dev commandPool (graphicsQueue queues)
    texData ← loadVulkanTextures gqdata []
    -- this is where we load all the lua data
    -- forked as child so that we can continue
    -- to draw the loading screen
    env ← ask
    st  ← get
    _ ← liftIO $ forkIO $ loadState env st
    -- this function updates the world grid
    -- as we change the camera.
    -- TODO: the aspect ratio should
    -- not be hardcoded
    -- _ ← liftIO $ forkIO $ updateWorld env 0 ((0.0,0.0),(12,8)) [] TStop
    -- this thread helps load in new command buffers
    -- and texture data
    _ ← liftIO $ forkIO $ loadParacVulkan env
    liftIO $ atomically $ writeChan (envLTimerChan env) TStart
        -- wait when minimized
    let beforeSwapchainCreation ∷ Anamnesis ε σ ()
        beforeSwapchainCreation = liftIO $ atomically $ writeTVar windowSizeChanged False
    loop $ do
      firstTick ← liftIO getCurTick
      --logDebug "creating new swapchain..."
      scsd ← querySwapchainSupport pdev vulkanSurface
      beforeSwapchainCreation
      rec ← gets sRecreate
      case rec of
        True → do
          newst ← get
          let ls          = luaSt newst
              windows     = luaWindows ls
              thiswin     = windows !! (luaCurrWin ls)
              wintextures = findReqTextures thiswin
          newTexData ← loadVulkanTextures gqdata wintextures
          modify $ \s → s { sRecreate  = False
                          , sVertCache = Nothing
                          , sTick      = Just firstTick }
          let vulkLoopData' = VulkanLoopData {..}
              vulkLoopData  = vulkLoopData' { texData = newTexData }
          vulkLoop vulkLoopData
        False → do
          let vulkLoopData = VulkanLoopData {..}
          vulkLoop vulkLoopData

vulkLoop ∷ VulkanLoopData → Anamnesis ε σ (LoopControl)
vulkLoop (VulkanLoopData (GQData pdev dev commandPool _) queues scsd window vulkanSurface texData msaaSamples shaderVert shaderFrag imgIndexPtr windowSizeChanged frameIndexRef renderFinishedSems imageAvailableSems inFlightFences) = do
  env ← ask
  swapInfo ← createSwapchain dev scsd queues vulkanSurface
  let swapchainLen = length (swapImgs swapInfo)
  (transObjMems, transObjBufs) ← unzip ⊚ createTransObjBuffers pdev dev swapchainLen
  let nDynObjs = 32
  (transDynMems, transDynBufs) ← unzip ⊚ createTransDynBuffers pdev dev swapchainLen nDynObjs
  (transTexMems, transTexBufs) ← unzip ⊚ createTransTexBuffers pdev dev swapchainLen nDynObjs
  descriptorBufferInfos ← mapM transObjBufferInfo transObjBufs
  dynDescBufInfos ← mapM (transDynBufferInfo nDynObjs) transDynBufs
  dynTexDescBufInfos ← mapM (transTexBufferInfo nDynObjs) transTexBufs
  descriptorPool ← createDescriptorPool dev swapchainLen (nimages texData)
  descriptorSetLayouts ← newArrayRes $ replicate swapchainLen (descSetLayout texData)
  descriptorSets ← createDescriptorSets dev descriptorPool swapchainLen descriptorSetLayouts
  forM_ (zip4 descriptorBufferInfos dynDescBufInfos dynTexDescBufInfos descriptorSets) $ \(bufInfo, dynBufInfo, dynTexDescBufInfo, dSet) → prepareDescriptorSet dev bufInfo dynBufInfo dynTexDescBufInfo (descTexInfo texData) dSet (nimages texData)
  transObjMemories ← newArrayRes transObjMems
  transDynMemories ← newArrayRes transDynMems
  transTexMemories ← newArrayRes transTexMems
  imgViews ← mapM (\image → createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
  --logDebug $ "created image views: " ⧺ show imgViews
  renderPass ← createRenderPass dev swapInfo (depthFormat texData) msaaSamples
  --logDebug $ "created renderpass: " ⧺ show renderPass
  graphicsPipeline ← createGraphicsPipeline dev swapInfo vertIBD vertIADs [shaderVert, shaderFrag] renderPass (pipelineLayout texData) msaaSamples
  --logDebug $ "created pipeline: " ⧺ show graphicsPipeline
  colorAttImgView ← createColorAttImgView pdev dev commandPool (graphicsQueue queues) (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
  depthAttImgView ← createDepthAttImgView pdev dev commandPool (graphicsQueue queues) (swapExtent swapInfo) msaaSamples
  framebuffers ← createFramebuffers dev renderPass swapInfo imgViews depthAttImgView colorAttImgView
  --logDebug $ "created framebuffers: " ⧺ show framebuffers
  -- dumb fps counter
  frameCount ← liftIO $ atomically $ newTVar @Int 0
  currentSec ← liftIO $ atomically $ newTVar @Int 0
  shouldExit ← loadLoop window $ do
    cmdBP0 ← genCommandBuffs dev pdev commandPool queues graphicsPipeline renderPass texData swapInfo framebuffers descriptorSets
    -- cache any new tiles that have been created
    -- currently only caches the first recreate
    vertcache ← gets sVertCache
    case vertcache of
      Just (Verts _) → modify $ \s → s { sReload = False }
      Nothing → do
        modify $ \s → s { sReload = False }
    -- if the size is changing, halt all draw ops
    sizeChanged0 ← liftIO $ atomically $ readTVar windowSizeChanged
    if sizeChanged0 then return AbortLoop
    else do
      shouldLoad ← glfwMainLoop window $ do
        stNew ← get
        let lsNew = luaSt stNew
            camNew = if ((luaCurrWin lsNew) > 0) then (winCursor $ (luaWindows lsNew) !! (luaCurrWin lsNew)) else (0.0,0.0,(-1.0))
            nDynData = dsDyns $ drawSt stNew
            nDynNew = length nDynData
        let rdata = RenderData { dev
                               , swapInfo
                               , queues
                               , imgIndexPtr
                               , frameIndexRef
                               , renderFinishedSems
                               , imageAvailableSems
                               , inFlightFences
                               , cmdBuffersPtr = cmdBP0
                               , memories = transObjMemories
                               , dynMemories = transDynMemories
                               , texMemories = transTexMemories
                               , memoryMutator = updateTransObj camNew dev (swapExtent swapInfo)
                               , dynMemoryMutator = updateTransDyn nDynNew nDynData dev (swapExtent swapInfo)
                               , texMemoryMutator = updateTransTex nDynNew nDynData dev (swapExtent swapInfo) }
        liftIO $ GLFW.pollEvents
        needRecreation ← drawFrame rdata `catchError` (\err → case (testEx err VK_ERROR_OUT_OF_DATE_KHR) of
          -- when khr out of date,
          -- recreate swapchain
          True → do
            _ ← logDebug $ "vulkan khr out of date"
            return True
          _    → logExcept ParacError ExParacletus "unknown drawFrame error" )
        sizeChanged ← liftIO $ atomically $ readTVar windowSizeChanged
        -- this is for the vaious events
        -- such as key input and state changes
        processEvents
        -- this is for input calculations
        processInput
        st ← get
        let newLS = luaSt st
        -- dumb fps counter
        seconds ← getTime
        cur ← liftIO $ atomically $ readTVar currentSec
        if floor seconds /= cur
        then do
          count ← liftIO $ atomically $ readTVar frameCount
          let cmdQ = envLCmdChan env
          when (cur /= 0) $ do
            let newLS' = setFPS newLS count
            modify $ \s → s { luaSt = newLS'
                            , sFPS  = (fst (sFPS st),count) }
            --liftIO $ atomically $ writeQueue cmdQ $ LoadCmdWin newLS'
          liftIO $ do
            atomically $ writeTVar currentSec (floor seconds)
            atomically $ writeTVar frameCount 0
        else liftIO $ atomically $ modifyTVar' frameCount succ
        -- wait idle because it seems needed
        -- at least once every frame
        runVk $ vkDeviceWaitIdle dev
        -- recreation check
        stateRecreate ← gets sRecreate
        stateReload   ← gets sReload
        return $ if needRecreation ∨ sizeChanged ∨ stateRecreate ∨ stateReload then AbortLoop else ContinueLoop
      -- loop ends, now deallocate
      stateRecreate ← gets sRecreate
      return $ if shouldLoad ∨ stateRecreate then AbortLoop else ContinueLoop
  return $ if shouldExit then AbortLoop else ContinueLoop

genCommandBuffs ∷ VkDevice → VkPhysicalDevice → VkCommandPool → DevQueues → VkPipeline → VkRenderPass → TextureData → SwapchainInfo → [VkFramebuffer] → [VkDescriptorSet] → Anamnesis ε σ (Ptr VkCommandBuffer)
genCommandBuffs dev pdev commandPool queues graphicsPipeline renderPass texData swapInfo framebuffers descriptorSets = do
      stNew ← get
      let dsNew = drawSt stNew
          (verts0, inds0) = case (sVertCache stNew) of
            Just (Verts verts) → verts
            Nothing            → calcVertices $ dsTiles dsNew
      vertexBufferNew ← createVertexBuffer pdev dev commandPool (graphicsQueue queues) verts0
      indexBufferNew ← createIndexBuffer pdev dev commandPool (graphicsQueue queues) inds0
      newCmdBP ← createCommandBuffers dev graphicsPipeline commandPool renderPass (pipelineLayout texData) swapInfo vertexBufferNew (dfLen inds0, indexBufferNew) framebuffers descriptorSets
      -- for now just recreate command
      -- buffers every frame
      return newCmdBP
