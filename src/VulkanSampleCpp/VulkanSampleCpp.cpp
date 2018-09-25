#ifdef _WIN32

#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

#include "VmaUsage.h"

#include <vulkan/vulkan.hpp>

#define ERR_GUARD_VULKAN(Expr) do { VkResult res__ = (Expr); if (res__ < 0) throw std::runtime_error("Vulkan error: " #Expr); } while(0)

static const wchar_t* const WINDOW_CLASS_NAME = L"VULKAN_MEMORY_ALLOCATOR_SAMPLE_CPP";
static const char* const VALIDATION_LAYER_NAME = "VK_LAYER_LUNARG_standard_validation";
static const char* const APP_TITLE_A =     "Vulkan Memory Allocator Sample C++ 1.0";
static const wchar_t* const APP_TITLE_W = L"Vulkan Memory Allocator Sample C++ 1.0";

static const uint32_t COMMAND_BUFFER_COUNT = 2;

static HINSTANCE g_hAppInstance;
static HWND g_hWnd;
static LONG g_SizeX = 1280, g_SizeY = 720;
static bool g_EnableValidationLayer = true;
static bool g_MemoryAliasingWarningEnabled = true;
static bool VK_KHR_get_memory_requirements2_enabled = false;
static bool VK_KHR_dedicated_allocation_enabled = false;

static bool IsLayerSupported(const vk::LayerProperties* pProps, size_t propCount, const char* pLayerName)
{
    const vk::LayerProperties* propsEnd = pProps + propCount;
    return std::find_if(
        pProps,
        propsEnd,
        [pLayerName](const vk::LayerProperties& prop) -> bool {
            return strcmp(pLayerName, prop.layerName) == 0;
        }) != propsEnd;
}

enum class CONSOLE_COLOR
{
    INFO,
    NORMAL,
    WARNING,
    ERROR_,
    COUNT
};

static void SetConsoleColor(CONSOLE_COLOR color)
{
    WORD attr = 0;
    switch(color)
    {
    case CONSOLE_COLOR::INFO:
        attr = FOREGROUND_INTENSITY;;
        break;
    case CONSOLE_COLOR::NORMAL:
        attr = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
        break;
    case CONSOLE_COLOR::WARNING:
        attr = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY;
        break;
    case CONSOLE_COLOR::ERROR_:
        attr = FOREGROUND_RED | FOREGROUND_INTENSITY;
        break;
    default:
        assert(0);
    }

    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleTextAttribute(out, attr);
}

static VKAPI_ATTR VkBool32 VKAPI_CALL MyDebugReportCallback(
    VkDebugReportFlagsEXT flags,
    VkDebugReportObjectTypeEXT objectType,
    uint64_t object,
    size_t location,
    int32_t messageCode,
    const char* pLayerPrefix,
    const char* pMessage,
    void* pUserData)
{
    // "Non-linear image 0xebc91 is aliased with linear buffer 0xeb8e4 which may indicate a bug."
    if(!g_MemoryAliasingWarningEnabled && flags == VK_DEBUG_REPORT_WARNING_BIT_EXT &&
        (strstr(pMessage, " is aliased with non-linear ") || strstr(pMessage, " is aliased with linear ")))
    {
        return VK_FALSE;
    }

    // Ignoring because when VK_KHR_dedicated_allocation extension is enabled,
    // vkGetBufferMemoryRequirements2KHR function is used instead, while Validation
    // Layer seems to be unaware of it.
    if (strstr(pMessage, "but vkGetBufferMemoryRequirements() has not been called on that buffer") != nullptr)
    {
        return VK_FALSE;
    }
    if (strstr(pMessage, "but vkGetImageMemoryRequirements() has not been called on that image") != nullptr)
    {
        return VK_FALSE;
    }

    /*
    "Mapping an image with layout VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL can result in undefined behavior if this memory is used by the device. Only GENERAL or PREINITIALIZED should be used."
    Ignoring because we map entire VkDeviceMemory blocks, where different types of
    images and buffers may end up together, especially on GPUs with unified memory
    like Intel.
    */
    if(strstr(pMessage, "Mapping an image with layout") != nullptr &&
        strstr(pMessage, "can result in undefined behavior if this memory is used by the device") != nullptr)
    {
        return VK_FALSE;
    }
    
    switch(flags)
    {
    case VK_DEBUG_REPORT_WARNING_BIT_EXT:
        SetConsoleColor(CONSOLE_COLOR::WARNING);
        break;
    case VK_DEBUG_REPORT_ERROR_BIT_EXT:
        SetConsoleColor(CONSOLE_COLOR::ERROR_);
        break;
    default:
        SetConsoleColor(CONSOLE_COLOR::INFO);
    }

    printf("%s \xBA %s\n", pLayerPrefix, pMessage);

    SetConsoleColor(CONSOLE_COLOR::NORMAL);

    if(flags == VK_DEBUG_REPORT_WARNING_BIT_EXT ||
        flags == VK_DEBUG_REPORT_ERROR_BIT_EXT)
    {
        OutputDebugStringA(pMessage);
        OutputDebugStringA("\n");
    }

    return VK_FALSE;
}

class Sample
{
public:
    Sample();
    void Init();
    ~Sample();
    void HandlePossibleSizeChange();
    void DrawFrame();
    void PrintAllocatorStats();

private:
    vk::UniqueInstance m_VulkanInstance;
    PFN_vkCreateDebugReportCallbackEXT m_pvkCreateDebugReportCallbackEXT = nullptr;
    PFN_vkDebugReportMessageEXT m_pvkDebugReportMessageEXT = nullptr;
    PFN_vkDestroyDebugReportCallbackEXT m_pvkDestroyDebugReportCallbackEXT = nullptr;
    VkDebugReportCallbackEXT m_hCallback = nullptr;
    vk::UniqueSurfaceKHR m_Surface;
    vk::PhysicalDevice m_PhysicalDevice;
    uint32_t m_GraphicsQueueFamilyIndex = UINT_MAX;
    uint32_t m_PresentQueueFamilyIndex = UINT_MAX;
    vk::UniqueDevice m_Device;
    VmaAllocator m_hAllocator = nullptr;
    vk::Queue m_GraphicsQueue;
    vk::Queue m_PresentQueue;
    vk::UniqueCommandPool m_CommandPool;
    vk::UniqueCommandBuffer m_MainCommandBuffers[COMMAND_BUFFER_COUNT];
    vk::UniqueFence m_MainCommandBufferExecutedFances[COMMAND_BUFFER_COUNT];
    vk::UniqueCommandBuffer m_TemporaryCommandBuffer;
    vk::UniqueSampler m_Sampler;

    void RegisterDebugCallbacks();
    void RecreateSwapChain();
};

Sample::Sample()
{
}

void Sample::Init()
{
    std::vector<vk::LayerProperties> instanceLayerProps = vk::enumerateInstanceLayerProperties();

    if(g_EnableValidationLayer)
    {
        if(IsLayerSupported(instanceLayerProps.data(), instanceLayerProps.size(), VALIDATION_LAYER_NAME) == false)
        {
            printf("Layer \"%s\" not supported.", VALIDATION_LAYER_NAME);
            g_EnableValidationLayer = false;
        }
    }

    std::vector<const char*> instanceExtensions;
    instanceExtensions.push_back(VK_KHR_SURFACE_EXTENSION_NAME);
    instanceExtensions.push_back(VK_KHR_WIN32_SURFACE_EXTENSION_NAME);
    
    std::vector<const char*> instanceLayers;
    if(g_EnableValidationLayer)
    {
        instanceLayers.push_back(VALIDATION_LAYER_NAME);
        instanceExtensions.push_back("VK_EXT_debug_report");
    }

    vk::ApplicationInfo appInfo = {
        APP_TITLE_A,
        VK_MAKE_VERSION(1, 0, 0),
        "Adam Sawicki Engine",
        VK_MAKE_VERSION(1, 0, 0),
        VK_API_VERSION_1_0 };
    vk::InstanceCreateInfo instInfo = {
        {},
        &appInfo,
        static_cast<uint32_t>(instanceLayers.size()),
        instanceExtensions.data(),
        static_cast<uint32_t>(instanceExtensions.size()),
        instanceLayers.data() };
    m_VulkanInstance.reset(vk::createInstance(instInfo));

    if(g_EnableValidationLayer)
    {
        RegisterDebugCallbacks();
    }

    vk::Win32SurfaceCreateInfoKHR surfaceInfo = {
        {},
        g_hAppInstance,
        g_hWnd };
    m_Surface.reset(m_VulkanInstance->createWin32SurfaceKHR(surfaceInfo));

    std::vector<vk::PhysicalDevice> physicalDevices = m_VulkanInstance->enumeratePhysicalDevices();
    assert(!physicalDevices.empty());
    m_PhysicalDevice = physicalDevices[0];

    // Query for features

    vk::PhysicalDeviceProperties physicalDeviceProperties = m_PhysicalDevice.getProperties();
    //vk::PhysicalDeviceFeatures physicalDeviceFeatures = m_PhysicalDevice.getFeatures();

    // Find queue family index

    std::vector<vk::QueueFamilyProperties> queueFamilies = m_PhysicalDevice.getQueueFamilyProperties();
    for(uint32_t i = 0, count = (uint32_t)queueFamilies.size();
        (i < count) &&
            (m_GraphicsQueueFamilyIndex == UINT_MAX || m_PresentQueueFamilyIndex == UINT_MAX);
        ++i)
    {
        if(queueFamilies[i].queueCount > 0)
        {
            if((m_GraphicsQueueFamilyIndex != 0) &&
                ((queueFamilies[i].queueFlags & vk::QueueFlagBits::eGraphics) != vk::QueueFlagBits{}))
            {
                m_GraphicsQueueFamilyIndex = i;
            }

            vk::Bool32 surfaceSupported = 0;
            vk::Result res = m_PhysicalDevice.getSurfaceSupportKHR(i, m_Surface.get(), &surfaceSupported);
            if((res >= vk::Result::eSuccess) && (surfaceSupported == VK_TRUE))
            {
                m_PresentQueueFamilyIndex = i;
            }
        }
    }
    assert(m_GraphicsQueueFamilyIndex != UINT_MAX);

    // Create logical device

    const float queuePriority = 1.f;

    std::array<vk::DeviceQueueCreateInfo, 2> deviceQueueCreateInfo = {
        vk::DeviceQueueCreateInfo{ {}, m_GraphicsQueueFamilyIndex, 1, &queuePriority },
        vk::DeviceQueueCreateInfo{ {}, m_PresentQueueFamilyIndex, 1, &queuePriority },
    };

    vk::PhysicalDeviceFeatures deviceFeatures = {};
    deviceFeatures.fillModeNonSolid = VK_TRUE;
    deviceFeatures.samplerAnisotropy = VK_TRUE;

    // Determine list of device extensions to enable.
    std::vector<const char*> enabledDeviceExtensions;
    enabledDeviceExtensions.push_back(VK_KHR_SWAPCHAIN_EXTENSION_NAME);
    {
        std::vector<vk::ExtensionProperties> properties = m_PhysicalDevice.enumerateDeviceExtensionProperties();
        for(uint32_t i = 0, count = (uint32_t)properties.size(); i < count; ++i)
        {
            if(strcmp(properties[i].extensionName, VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME) == 0)
            {
                enabledDeviceExtensions.push_back(VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME);
                VK_KHR_get_memory_requirements2_enabled = true;
            }
            else if(strcmp(properties[i].extensionName, VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME) == 0)
            {
                enabledDeviceExtensions.push_back(VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME);
                VK_KHR_dedicated_allocation_enabled = true;
            }
        }
    }

    vk::DeviceCreateInfo deviceCreateInfo = {
        {}, // flags
        m_PresentQueueFamilyIndex != m_GraphicsQueueFamilyIndex ? 2u : 1u, // queueCreateInfoCount
        deviceQueueCreateInfo.data(), // pQueueCreateInfos
        0, // enabledLayerCount
        nullptr, // ppEnabledLayerNames
        (uint32_t)enabledDeviceExtensions.size(), // enabledExtensionCount
        !enabledDeviceExtensions.empty() ? enabledDeviceExtensions.data() : nullptr, // ppEnabledExtensionNames
        &deviceFeatures }; // pEnabledFeatures
    m_Device.reset(m_PhysicalDevice.createDevice(deviceCreateInfo));

    // Create memory allocator

    VmaAllocatorCreateInfo allocatorInfo = {};
    allocatorInfo.physicalDevice = m_PhysicalDevice;
    allocatorInfo.device = m_Device.get();

    if(VK_KHR_dedicated_allocation_enabled)
    {
        allocatorInfo.flags |= VMA_ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT;
    }

    ERR_GUARD_VULKAN( vmaCreateAllocator(&allocatorInfo, &m_hAllocator) );

    // Retrieve queue (doesn't need to be destroyed)
    m_GraphicsQueue = m_Device->getQueue(m_GraphicsQueueFamilyIndex, 0);
    m_PresentQueue = m_Device->getQueue(m_PresentQueueFamilyIndex, 0);
    assert(m_GraphicsQueue);
    assert(m_PresentQueue);

    // Create command pool

    vk::CommandPoolCreateInfo commandPoolInfo = {
        vk::CommandPoolCreateFlagBits::eResetCommandBuffer, // flags
        m_GraphicsQueueFamilyIndex }; // queueFamilyIndex
    m_CommandPool.reset(m_Device->createCommandPool(commandPoolInfo));

    vk::CommandBufferAllocateInfo commandBufferInfo = {
        m_CommandPool.get(), // commandPool
        vk::CommandBufferLevel::ePrimary, // level
        1 }; // commandBufferCount
    vk::FenceCreateInfo fenceInfo = {
        vk::FenceCreateFlagBits::eSignaled }; // flags
    vk::CommandBuffer cmdBuf;
    for(uint32_t i = 0; i < COMMAND_BUFFER_COUNT; ++i)
    {
        m_Device->allocateCommandBuffers(&commandBufferInfo, &cmdBuf);
        m_MainCommandBuffers[i].reset(cmdBuf);

        m_MainCommandBufferExecutedFances[i].reset(m_Device->createFence(fenceInfo));
    }

    commandBufferInfo.commandBufferCount = 1;
    m_Device->allocateCommandBuffers(&commandBufferInfo, &cmdBuf);
    m_TemporaryCommandBuffer.reset(cmdBuf);

    // Create texture sampler

    vk::SamplerCreateInfo samplerInfo = {
        {}, // flags
        vk::Filter::eLinear, // magFilter
        vk::Filter::eLinear, // minFilter
        vk::SamplerMipmapMode::eLinear, //mipmapMode
        vk::SamplerAddressMode::eRepeat, // addressModeU
        vk::SamplerAddressMode::eRepeat, // addressModeV
        vk::SamplerAddressMode::eRepeat, // addressModeW
        0.f, // mipLodBias
        VK_TRUE, // anisotropyEnable
        16, // maxAnisotropy
        VK_FALSE, // compareEnable
        vk::CompareOp::eAlways, // compareOp
        0.f, // minLod
        FLT_MAX, // maxLod
        vk::BorderColor::eIntOpaqueBlack, // borderColor
        VK_FALSE }; // unnormalizedCoordinates
    m_Sampler.reset(m_Device->createSampler(samplerInfo));

#if 0
    CreateTexture(128, 128);
    CreateMesh();

    VkDescriptorSetLayoutBinding samplerLayoutBinding = {};
    samplerLayoutBinding.binding = 1;
    samplerLayoutBinding.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    samplerLayoutBinding.descriptorCount = 1;
    samplerLayoutBinding.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;

    VkDescriptorSetLayoutCreateInfo descriptorSetLayoutInfo = { VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO };
    descriptorSetLayoutInfo.bindingCount = 1;
    descriptorSetLayoutInfo.pBindings = &samplerLayoutBinding;
    ERR_GUARD_VULKAN( vkCreateDescriptorSetLayout(g_hDevice, &descriptorSetLayoutInfo, nullptr, &g_hDescriptorSetLayout) );

    // Create descriptor pool

    VkDescriptorPoolSize descriptorPoolSizes[2];
    ZeroMemory(descriptorPoolSizes, sizeof(descriptorPoolSizes));
    descriptorPoolSizes[0].type = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
    descriptorPoolSizes[0].descriptorCount = 1;
    descriptorPoolSizes[1].type = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    descriptorPoolSizes[1].descriptorCount = 1;

    VkDescriptorPoolCreateInfo descriptorPoolInfo = { VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO };
    descriptorPoolInfo.poolSizeCount = (uint32_t)_countof(descriptorPoolSizes);
    descriptorPoolInfo.pPoolSizes = descriptorPoolSizes;
    descriptorPoolInfo.maxSets = 1;
    ERR_GUARD_VULKAN( vkCreateDescriptorPool(g_hDevice, &descriptorPoolInfo, nullptr, &g_hDescriptorPool) );

    // Create descriptor set layout

    VkDescriptorSetLayout descriptorSetLayouts[] = { g_hDescriptorSetLayout };
    VkDescriptorSetAllocateInfo descriptorSetInfo = { VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO };
    descriptorSetInfo.descriptorPool = g_hDescriptorPool;
    descriptorSetInfo.descriptorSetCount = 1;
    descriptorSetInfo.pSetLayouts = descriptorSetLayouts;
    ERR_GUARD_VULKAN( vkAllocateDescriptorSets(g_hDevice, &descriptorSetInfo, &g_hDescriptorSet) );

    VkDescriptorImageInfo descriptorImageInfo = {};
    descriptorImageInfo.imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    descriptorImageInfo.imageView = g_hTextureImageView;
    descriptorImageInfo.sampler = g_hSampler;

    VkWriteDescriptorSet writeDescriptorSet = { VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET };
    writeDescriptorSet.dstSet = g_hDescriptorSet;
    writeDescriptorSet.dstBinding = 1;
    writeDescriptorSet.dstArrayElement = 0;
    writeDescriptorSet.descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    writeDescriptorSet.descriptorCount = 1;
    writeDescriptorSet.pImageInfo = &descriptorImageInfo;

    vkUpdateDescriptorSets(g_hDevice, 1, &writeDescriptorSet, 0, nullptr);

    CreateSwapchain();
#endif
}

Sample::~Sample()
{
#if 0
    vkDeviceWaitIdle(g_hDevice);

    DestroySwapchain(true);

    if(g_hDescriptorPool != VK_NULL_HANDLE)
    {
        vkDestroyDescriptorPool(g_hDevice, g_hDescriptorPool, nullptr);
        g_hDescriptorPool = VK_NULL_HANDLE;
    }

    if(g_hDescriptorSetLayout != VK_NULL_HANDLE)
    {
        vkDestroyDescriptorSetLayout(g_hDevice, g_hDescriptorSetLayout, nullptr);
        g_hDescriptorSetLayout = VK_NULL_HANDLE;
    }

    if(g_hTextureImageView != VK_NULL_HANDLE)
    {
        vkDestroyImageView(g_hDevice, g_hTextureImageView, nullptr);
        g_hTextureImageView = VK_NULL_HANDLE;
    }
    if(g_hTextureImage != VK_NULL_HANDLE)
    {
        vmaDestroyImage(g_hAllocator, g_hTextureImage, g_hTextureImageAlloc);
        g_hTextureImage = VK_NULL_HANDLE;
    }

    if(g_hIndexBuffer != VK_NULL_HANDLE)
    {
        vmaDestroyBuffer(g_hAllocator, g_hIndexBuffer, g_hIndexBufferAlloc);
        g_hIndexBuffer = VK_NULL_HANDLE;
    }
    if(g_hVertexBuffer != VK_NULL_HANDLE)
    {
        vmaDestroyBuffer(g_hAllocator, g_hVertexBuffer, g_hVertexBufferAlloc);
        g_hVertexBuffer = VK_NULL_HANDLE;
    }

#endif

    m_Sampler.reset(nullptr);

    m_TemporaryCommandBuffer.reset(nullptr);

    // Too bad I can't destroy them all with single call to m_Device->freeCommandBuffers because this is an array of vk::UniqueCommandBuffer.
    for(uint32_t i = COMMAND_BUFFER_COUNT; i--; )
    {
        m_MainCommandBufferExecutedFances[i].reset(nullptr);
        m_MainCommandBuffers[i].reset(nullptr);
    }

    m_CommandPool.reset(nullptr);

    if(m_hAllocator != VK_NULL_HANDLE)
    {
        vmaDestroyAllocator(m_hAllocator);
        m_hAllocator = nullptr;
    }

    m_Device.reset(nullptr);

    m_Surface.reset(nullptr);

    if(m_pvkDestroyDebugReportCallbackEXT && m_hCallback)
    {
        m_pvkDestroyDebugReportCallbackEXT(m_VulkanInstance.get(), m_hCallback, nullptr);
        m_hCallback = VK_NULL_HANDLE;
    }

    m_VulkanInstance.reset(nullptr);
}

void Sample::RegisterDebugCallbacks()
{
    m_pvkCreateDebugReportCallbackEXT =
        reinterpret_cast<PFN_vkCreateDebugReportCallbackEXT>
            (vkGetInstanceProcAddr(m_VulkanInstance.get(), "vkCreateDebugReportCallbackEXT"));
    m_pvkDebugReportMessageEXT =
        reinterpret_cast<PFN_vkDebugReportMessageEXT>
            (vkGetInstanceProcAddr(m_VulkanInstance.get(), "vkDebugReportMessageEXT"));
    m_pvkDestroyDebugReportCallbackEXT =
        reinterpret_cast<PFN_vkDestroyDebugReportCallbackEXT>
            (vkGetInstanceProcAddr(m_VulkanInstance.get(), "vkDestroyDebugReportCallbackEXT"));
    assert(m_pvkCreateDebugReportCallbackEXT);
    assert(m_pvkDebugReportMessageEXT);
    assert(m_pvkDestroyDebugReportCallbackEXT);

    vk::DebugReportCallbackCreateInfoEXT callbackCreateInfo = {
            //vk::DebugReportFlagBitsEXT::eInformation |
            vk::DebugReportFlagBitsEXT::eError |
            vk::DebugReportFlagBitsEXT::eWarning |
            vk::DebugReportFlagBitsEXT::ePerformanceWarning /*|
            vk::DebugReportFlagBitsEXT::eDebug*/,
        &MyDebugReportCallback,
        nullptr };
    ERR_GUARD_VULKAN( m_pvkCreateDebugReportCallbackEXT(
        m_VulkanInstance.get(),
        &(const VkDebugReportCallbackCreateInfoEXT&)callbackCreateInfo,
        nullptr,
        &m_hCallback) );
}

void Sample::RecreateSwapChain()
{
#if 0
    vkDeviceWaitIdle(g_hDevice);
    DestroySwapchain(false);
    CreateSwapchain();
#endif
}

static Sample* g_Sample;

void Sample::HandlePossibleSizeChange()
{
    RECT clientRect;
    GetClientRect(g_hWnd, &clientRect);
    LONG newSizeX = clientRect.right - clientRect.left;
    LONG newSizeY = clientRect.bottom - clientRect.top;
    if((newSizeX > 0) &&
        (newSizeY > 0) &&
        ((newSizeX != g_SizeX) || (newSizeY != g_SizeY)))
    {
        g_SizeX = newSizeX;
        g_SizeY = newSizeY;

        RecreateSwapChain();
    }
}

void Sample::DrawFrame()
{
    if(!m_Device)
    {
        return;
    }

#if 0
    // Begin main command buffer
    size_t cmdBufIndex = (g_NextCommandBufferIndex++) % COMMAND_BUFFER_COUNT;
    VkCommandBuffer hCommandBuffer = g_MainCommandBuffers[cmdBufIndex];
    VkFence hCommandBufferExecutedFence = g_MainCommandBufferExecutedFances[cmdBufIndex];

    ERR_GUARD_VULKAN( vkWaitForFences(g_hDevice, 1, &hCommandBufferExecutedFence, VK_TRUE, UINT64_MAX) );
    ERR_GUARD_VULKAN( vkResetFences(g_hDevice, 1, &hCommandBufferExecutedFence) );

    VkCommandBufferBeginInfo commandBufferBeginInfo = { VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO };
    commandBufferBeginInfo.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
    ERR_GUARD_VULKAN( vkBeginCommandBuffer(hCommandBuffer, &commandBufferBeginInfo) );
    
    // Acquire swapchain image
    uint32_t imageIndex = 0;
    VkResult res = vkAcquireNextImageKHR(g_hDevice, g_hSwapchain, UINT64_MAX, g_hImageAvailableSemaphore, VK_NULL_HANDLE, &imageIndex);
    if(res == VK_ERROR_OUT_OF_DATE_KHR)
    {
        RecreateSwapChain();
        return;
    }
    else if(res < 0)
    {
        ERR_GUARD_VULKAN(res);
    }

    // Record geometry pass

    VkClearValue clearValues[2];
    ZeroMemory(clearValues, sizeof(clearValues));
    clearValues[0].color.float32[0] = 0.25f;
    clearValues[0].color.float32[1] = 0.25f;
    clearValues[0].color.float32[2] = 0.5f;
    clearValues[0].color.float32[3] = 1.0f;
    clearValues[1].depthStencil.depth = 1.0f;

    VkRenderPassBeginInfo renderPassBeginInfo = { VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO };
    renderPassBeginInfo.renderPass = g_hRenderPass;
    renderPassBeginInfo.framebuffer = g_Framebuffers[imageIndex];
    renderPassBeginInfo.renderArea.offset.x = 0;
    renderPassBeginInfo.renderArea.offset.y = 0;
    renderPassBeginInfo.renderArea.extent = g_Extent;
    renderPassBeginInfo.clearValueCount = (uint32_t)_countof(clearValues);
    renderPassBeginInfo.pClearValues = clearValues;
    vkCmdBeginRenderPass(hCommandBuffer, &renderPassBeginInfo, VK_SUBPASS_CONTENTS_INLINE);
    
    vkCmdBindPipeline(
        hCommandBuffer,
        VK_PIPELINE_BIND_POINT_GRAPHICS,
        g_hPipeline);

    mat4 view = mat4::LookAt(
        vec3(0.f, 0.f, 0.f),
        vec3(0.f, -2.f, 4.f),
        vec3(0.f, 1.f, 0.f));
    mat4 proj = mat4::Perspective(
        1.0471975511966f, // 60 degrees
        (float)g_Extent.width / (float)g_Extent.height,
        0.1f,
        1000.f);
    mat4 viewProj = view * proj;

    vkCmdBindDescriptorSets(
        hCommandBuffer,
        VK_PIPELINE_BIND_POINT_GRAPHICS,
        g_hPipelineLayout,
        0,
        1,
        &g_hDescriptorSet,
        0,
        nullptr);

    float rotationAngle = (float)GetTickCount() * 0.001f * (float)PI * 0.2f;
    mat4 model = mat4::RotationY(rotationAngle);

    UniformBufferObject ubo = {};
    ubo.ModelViewProj = model * viewProj;
    vkCmdPushConstants(hCommandBuffer, g_hPipelineLayout, VK_SHADER_STAGE_VERTEX_BIT, 0, sizeof(UniformBufferObject), &ubo);

    VkBuffer vertexBuffers[] = { g_hVertexBuffer };
    VkDeviceSize offsets[] = { 0 };
    vkCmdBindVertexBuffers(hCommandBuffer, 0, 1, vertexBuffers, offsets);

    vkCmdBindIndexBuffer(hCommandBuffer, g_hIndexBuffer, 0, VK_INDEX_TYPE_UINT16);

    vkCmdDrawIndexed(hCommandBuffer, g_IndexCount, 1, 0, 0, 0);

    vkCmdEndRenderPass(hCommandBuffer);
    
    vkEndCommandBuffer(hCommandBuffer);

    // Submit command buffer
    
    VkSemaphore submitWaitSemaphores[] = { g_hImageAvailableSemaphore };
    VkPipelineStageFlags submitWaitStages[] = { VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT };
    VkSemaphore submitSignalSemaphores[] = { g_hRenderFinishedSemaphore };
    VkSubmitInfo submitInfo = { VK_STRUCTURE_TYPE_SUBMIT_INFO };
    submitInfo.waitSemaphoreCount = 1;
    submitInfo.pWaitSemaphores = submitWaitSemaphores;
    submitInfo.pWaitDstStageMask = submitWaitStages;
    submitInfo.commandBufferCount = 1;
    submitInfo.pCommandBuffers = &hCommandBuffer;
    submitInfo.signalSemaphoreCount = _countof(submitSignalSemaphores);
    submitInfo.pSignalSemaphores = submitSignalSemaphores;
    ERR_GUARD_VULKAN( vkQueueSubmit(g_hGraphicsQueue, 1, &submitInfo, hCommandBufferExecutedFence) );

    VkSemaphore presentWaitSemaphores[] = { g_hRenderFinishedSemaphore };

    VkSwapchainKHR swapchains[] = { g_hSwapchain };
    VkPresentInfoKHR presentInfo = { VK_STRUCTURE_TYPE_PRESENT_INFO_KHR };
    presentInfo.waitSemaphoreCount = _countof(presentWaitSemaphores);
    presentInfo.pWaitSemaphores = presentWaitSemaphores;
    presentInfo.swapchainCount = 1;
    presentInfo.pSwapchains = swapchains;
    presentInfo.pImageIndices = &imageIndex;
    presentInfo.pResults = nullptr;
    res = vkQueuePresentKHR(g_hPresentQueue, &presentInfo);
    if(res == VK_ERROR_OUT_OF_DATE_KHR)
    {
        RecreateSwapChain();
    }
    else
        ERR_GUARD_VULKAN(res);
#endif
}

void Sample::PrintAllocatorStats()
{
#if 0
#if VMA_STATS_STRING_ENABLED
    char* statsString = nullptr;
    vmaBuildStatsString(g_hAllocator, &statsString, true);
    printf("%s\n", statsString);
    vmaFreeStatsString(g_hAllocator, statsString);
#endif
#endif
}

static LRESULT WINAPI WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch(msg)
    {
    case WM_CREATE:
        // This is intentionally assigned here because we are now inside CreateWindow, before it returns.
        g_hWnd = hWnd;
        g_Sample = new Sample();
        g_Sample->PrintAllocatorStats();
        return 0;

    case WM_DESTROY:
        delete g_Sample;
        g_Sample = nullptr;
        PostQuitMessage(0);
        return 0;

    // This prevents app from freezing when left Alt is pressed
    // (which normally enters modal menu loop).
    case WM_SYSKEYDOWN:
    case WM_SYSKEYUP:
        return 0;

    case WM_SIZE:
        if(g_Sample && (wParam == SIZE_MAXIMIZED || wParam == SIZE_RESTORED))
        {
            g_Sample->HandlePossibleSizeChange();
        }
        return 0;

    case WM_EXITSIZEMOVE:
        if(g_Sample)
        {
            g_Sample->HandlePossibleSizeChange();
        }
        return 0;

    case WM_KEYDOWN:
        switch(wParam)
        {
        case VK_ESCAPE:
            PostMessage(hWnd, WM_CLOSE, 0, 0);
            break;
        }
        return 0;

    default:
        break;
    }

    return DefWindowProc(hWnd, msg, wParam, lParam);
}

int main()
{
    g_hAppInstance = (HINSTANCE)GetModuleHandle(NULL);

    WNDCLASSEX wndClassDesc = { sizeof(WNDCLASSEX) };
    wndClassDesc.style = CS_VREDRAW | CS_HREDRAW | CS_DBLCLKS;
    wndClassDesc.hbrBackground = NULL;
    wndClassDesc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wndClassDesc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wndClassDesc.hInstance = g_hAppInstance;
    wndClassDesc.lpfnWndProc = WndProc;
    wndClassDesc.lpszClassName = WINDOW_CLASS_NAME;
    
    const ATOM hWndClass = RegisterClassEx(&wndClassDesc);
    assert(hWndClass);

    const DWORD style = WS_VISIBLE | WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_THICKFRAME;
    const DWORD exStyle = 0;

    RECT rect = { 0, 0, g_SizeX, g_SizeY };
    AdjustWindowRectEx(&rect, style, FALSE, exStyle);

    CreateWindowEx(
        exStyle, WINDOW_CLASS_NAME, APP_TITLE_W, style,
        CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
        NULL, NULL, g_hAppInstance, NULL);

    MSG msg;
    for(;;)
    {
        if(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
        {
            if(msg.message == WM_QUIT)
                break;
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
        if(g_Sample)
        {
            g_Sample->DrawFrame();
        }
    }

    return 0;
}

#else // #ifdef _WIN32

#ifdef __clang__
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wtautological-compare" // comparison of unsigned expression < 0 is always false
#endif

#include "VmaUsage.h"

#ifdef __clang__
    #pragma clang diagnostic pop
#endif

int main()
{
    // TODO
}

#endif // #ifdef _WIN32
