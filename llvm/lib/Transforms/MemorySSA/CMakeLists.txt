add_llvm_component_library(LLVMMemorySSA
        Construction.cpp

        ADDITIONAL_HEADER_DIRS
        ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms

        DEPENDS
        intrinsics_gen

        COMPONENT_NAME
        MemorySSA

        LINK_COMPONENTS
        Analysis
        Core
        Support
        TransformUtils
)
