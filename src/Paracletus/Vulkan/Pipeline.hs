{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
module Paracletus.Vulkan.Pipeline where
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame
import Numeric.DataFrame
import Numeric.Dimensions
import Anamnesis
import Anamnesis.Foreign
import Anamnesis.Util
import Paracletus.Vulkan.Data
import Paracletus.Vulkan.Foreign

createGraphicsPipeline ∷ KnownDim (n ∷ Nat) ⇒ VkDevice → SwapchainInfo → VkVertexInputBindingDescription → Vector VkVertexInputAttributeDescription n → [VkPipelineShaderStageCreateInfo] → VkRenderPass → VkPipelineLayout → VkSampleCountFlagBits → Anamnesis ε σ VkPipeline
createGraphicsPipeline dev SwapchainInfo{swapExtent} bindDesc attrDescs shaderDescs renderPass pipelineLayout msaaSamples = allocResource
    (\gp → liftIO $ vkDestroyPipeline dev gp VK_NULL) $
    withVkPtr gpCreateInfo $ \gpciPtr → allocaPeek $ runVk ∘ vkCreateGraphicsPipelines dev VK_NULL 1 gpciPtr VK_NULL
  where gpCreateInfo = createVk @VkGraphicsPipelineCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"stageCount" (fromIntegral $ length shaderDescs)
          &* setListRef @"pStages" shaderDescs
          &* setVkRef @"pVertexInputState" vertexInputInfo
          &* setVkRef @"pInputAssemblyState" inputAssembly
          &* set @"pTessellationState" VK_NULL
          &* setVkRef @"pViewportState" viewPortState
          &* setVkRef @"pRasterizationState" rasterizer
          &* setVkRef @"pMultisampleState" multisampling
          &* setVkRef @"pDepthStencilState" depthStencilState
          &* setVkRef @"pColorBlendState" colorBlending
          &* set @"pDynamicState" VK_NULL
          &* set @"layout" pipelineLayout
          &* set @"renderPass" renderPass
          &* set @"subpass" 0
          &* set @"basePipelineHandle" VK_NULL_HANDLE
          &* set @"basePipelineIndex" (-1)
        vertexInputInfo = createVk @VkPipelineVertexInputStateCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"vertexBindingDescriptionCount" 1
          &* setDFRef @"pVertexBindingDescriptions" (scalar bindDesc)
          &* set @"vertexAttributeDescriptionCount" (fromIntegral ∘ totalDim $ dims `inSpaceOf` attrDescs)
          &* setDFRef @"pVertexAttributeDescriptions" attrDescs
        inputAssembly = createVk @VkPipelineInputAssemblyStateCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          &* set @"primitiveRestartEnable" VK_FALSE
        viewPort = createVk @VkViewport
          $  set @"x" 0
          &* set @"y" 0
          &* set @"width" (fromIntegral $ getField @"width" swapExtent)
          &* set @"height" (fromIntegral $ getField @"height" swapExtent)
          &* set @"minDepth" 0
          &* set @"maxDepth" 1
        scissor = createVk @VkRect2D
          $  set @"extent" swapExtent
          &* setVk @"offset" (set @"x" 0 &* set @"y" 0)
        viewPortState = createVk @VkPipelineViewportStateCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"viewportCount" 1
          &* setVkRef @"pViewports" viewPort
          &* set @"scissorCount" 1
          &* setVkRef @"pScissors" scissor
        rasterizer = createVk @VkPipelineRasterizationStateCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"depthClampEnable" VK_FALSE
          &* set @"rasterizerDiscardEnable" VK_FALSE
          &* set @"polygonMode" VK_POLYGON_MODE_FILL
          &* set @"cullMode" VK_CULL_MODE_BACK_BIT
          &* set @"frontFace" VK_FRONT_FACE_CLOCKWISE
          &* set @"depthBiasEnable" VK_FALSE
          &* set @"depthBiasConstantFactor" 0
          &* set @"depthBiasClamp" 0
          &* set @"depthBiasSlopeFactor" 0
          &* set @"lineWidth" 1.0
        multisampling = createVk @VkPipelineMultisampleStateCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"sampleShadingEnable" VK_FALSE
          &* set @"rasterizationSamples" msaaSamples
          &* set @"minSampleShading" 1.0
          &* set @"pSampleMask" VK_NULL
          &* set @"alphaToCoverageEnable" VK_FALSE
          &* set @"alphaToOneEnable" VK_FALSE
        colorBlendAttachment = createVk @VkPipelineColorBlendAttachmentState
          $  set @"colorWriteMask" (VK_COLOR_COMPONENT_R_BIT ⌄ VK_COLOR_COMPONENT_G_BIT ⌄ VK_COLOR_COMPONENT_B_BIT ⌄ VK_COLOR_COMPONENT_A_BIT)
          &* set @"blendEnable" VK_TRUE
          &* set @"srcColorBlendFactor" VK_BLEND_FACTOR_SRC_ALPHA
          &* set @"dstColorBlendFactor" VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
          &* set @"colorBlendOp" VK_BLEND_OP_ADD
          &* set @"srcAlphaBlendFactor" VK_BLEND_FACTOR_SRC_ALPHA
          &* set @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
          &* set @"alphaBlendOp" VK_BLEND_OP_SUBTRACT
        colorBlending = createVk @VkPipelineColorBlendStateCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"logicOpEnable" VK_FALSE
          &* set @"logicOp" VK_LOGIC_OP_COPY
          &* set @"attachmentCount" 1
          &* setVkRef @"pAttachments" colorBlendAttachment
          &* setAt @"blendConstants" @0 0.0
          &* setAt @"blendConstants" @1 0.0
          &* setAt @"blendConstants" @2 0.0
          &* setAt @"blendConstants" @3 0.0
        depthStencilState = createVk @VkPipelineDepthStencilStateCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
          &* set @"depthTestEnable" VK_TRUE
          &* set @"depthWriteEnable" VK_FALSE
          &* set @"depthCompareOp" VK_COMPARE_OP_LESS
          &* set @"depthBoundsTestEnable" VK_FALSE
          &* set @"minDepthBounds" 0.0
          &* set @"maxDepthBounds" 1.0
          &* set @"stencilTestEnable" VK_FALSE
          &* setVk @"front"
              (  set @"failOp" VK_STENCIL_OP_KEEP
              &* set @"passOp" VK_STENCIL_OP_KEEP
              &* set @"depthFailOp" VK_STENCIL_OP_KEEP
              &* set @"compareOp" VK_COMPARE_OP_NEVER
              &* set @"compareMask" 0
              &* set @"writeMask" 0
              &* set @"reference" 0 )
          &* setVk @"back"
              (  set @"failOp" VK_STENCIL_OP_KEEP
              &* set @"passOp" VK_STENCIL_OP_KEEP
              &* set @"depthFailOp" VK_STENCIL_OP_KEEP
              &* set @"compareOp" VK_COMPARE_OP_NEVER
              &* set @"compareMask" 0
              &* set @"writeMask" 0
              &* set @"reference" 0 )

createPipelineLayout ∷ VkDevice → VkDescriptorSetLayout → Anamnesis ε σ VkPipelineLayout
createPipelineLayout dev dsl = do
  let plCreateInfo = createVk @VkPipelineLayoutCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"setLayoutCount" 1
        &* setListRef @"pSetLayouts" [dsl]
        &* set @"pushConstantRangeCount" 0
        &* set @"pPushConstantRanges" VK_NULL
     --   &* set @"pushConstantRangeCount" 1
     --   &* setListRef @"pPushConstantRanges" [pcr]
     -- pcr = createVk @VkPushConstantRange
     --   $  set @"stageFlags" VK_SHADER_STAGE_VERTEX_BIT
     --   &* set @"offset" 0
     --   &* set @"size" (bSizeOf @PushConstantData undefined)
  allocResource
    (\pl → liftIO $ vkDestroyPipelineLayout dev pl VK_NULL) $
    withVkPtr plCreateInfo $ \plciPtr → allocaPeek $ runVk ∘ vkCreatePipelineLayout dev plciPtr VK_NULL

createRenderPass ∷ VkDevice → SwapchainInfo → VkFormat → VkSampleCountFlagBits → Anamnesis ε σ VkRenderPass
createRenderPass dev SwapchainInfo{swapImgFormat} depthFormat samples = allocResource (\rp → liftIO $ vkDestroyRenderPass dev rp VK_NULL) $ withVkPtr rpCreateInfo $ \rpciPtr → allocaPeek $ runVk ∘ vkCreateRenderPass dev rpciPtr VK_NULL
  where colorAttachment = createVk @VkAttachmentDescription
          $  set @"flags" VK_ZERO_FLAGS
          &* set @"format" swapImgFormat
          &* set @"samples" samples
          &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
          &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
          &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
          &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
          &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
          &* set @"finalLayout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL 
        depthAttachment = createVk @VkAttachmentDescription
          $  set @"flags" VK_ZERO_FLAGS
          &* set @"format" depthFormat
          &* set @"samples" samples
          &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
          &* set @"storeOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
          &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
          &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
          &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
          &* set @"finalLayout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        colorAttachmentResolve = createVk @VkAttachmentDescription
          $  set @"flags" VK_ZERO_FLAGS
          &* set @"format" swapImgFormat
          &* set @"samples" VK_SAMPLE_COUNT_1_BIT
          &* set @"loadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
          &* set @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
          &* set @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
          &* set @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
          &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
          &* set @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
        colorAttachmentRef = createVk @VkAttachmentReference
          $  set @"attachment" 0
          &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        depthAttachmentRef = createVk @VkAttachmentReference
          $  set @"attachment" 1
          &* set @"layout" VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        colorAttachmentResolveRef = createVk @VkAttachmentReference
          $  set @"attachment" 2
          &* set @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        subpass = createVk @VkSubpassDescription
          $  set @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
          &* set @"colorAttachmentCount" 1
          &* setVkRef @"pColorAttachments" colorAttachmentRef
          &* setVkRef @"pDepthStencilAttachment" depthAttachmentRef
          &* setVkRef @"pResolveAttachments" colorAttachmentResolveRef
          &* set @"pPreserveAttachments" VK_NULL
          &* set @"pInputAttachments" VK_NULL
        dependency = createVk @VkSubpassDependency
          $  set @"srcSubpass" VK_SUBPASS_EXTERNAL
          &* set @"dstSubpass" 0
          &* set @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          &* set @"srcAccessMask" VK_ZERO_FLAGS
          &* set @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          &* set @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT ⌄ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
        rpCreateInfo = createVk @VkRenderPassCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* setListCountAndRef @"attachmentCount" @"pAttachments" [colorAttachment, depthAttachment, colorAttachmentResolve]
          &* set @"subpassCount" 1
          &* setVkRef @"pSubpasses" subpass
          &* set @"dependencyCount" 1
          &* setVkRef @"pDependencies" dependency
