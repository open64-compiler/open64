pathscale_defs.h:
echo '#ifndef __pathscale_defs_h' > $@
echo '#define __pathscale_defs_h' >> $@
echo '#define OPEN64_NAME_PREFIX "${OPEN64_NAME_PREFIX}"' >> $@
echo '#define OPEN64_MAJOR_VERSION_NUM ${OPEN64_MAJOR_VERSION}' >> $@
echo '#define OPEN64_MINOR_VERSION_NUM ${OPEN64_MINOR_VERSION}' >> $@
echo '#define OPEN64_MAJOR_VERSION "${OPEN64_MAJOR_VERSION}"' >> $@
echo '#define OPEN64_MINOR_VERSION "${OPEN64_MINOR_VERSION}"' >> $@
echo '#define OPEN64_FULL_VERSION "${OPEN64_FULL_VERSION}"' >> $@
echo '#define OPEN64_GCC_VERSION "${OPEN64_GCC_VERSION}"' >> $@
echo '#define OPEN64_GCC40_VERSION "${OPEN64_GCC40_VERSION}"' >> $@
echo '#define OPEN64_GCC42_VERSION "${OPEN64_GCC42_VERSION}"' >> $@
echo '#define OPEN64_INSTALL_PREFIX "${OPEN64_INSTALL_PREFIX}"' >> $@
echo '#define OPEN64_TARGET "${OPEN64_TARGET}"' >> $@
echo '#define OPEN64_PATCH_LEVEL "${OPEN64_PATCH_LEVEL}"' >> $@
echo "#endif" >> $@