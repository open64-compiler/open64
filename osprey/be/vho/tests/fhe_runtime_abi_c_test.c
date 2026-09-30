#include "open64_fhe_runtime_abi.h"

int
main(void)
{
  open64_fhe_broker_desc_v1 desc = {
    OPEN64_FHE_ABI_VERSION_V1,
    sizeof(open64_fhe_broker_desc_v1),
    0,
    0
  };
  return desc.abi_version == OPEN64_FHE_ABI_VERSION_V1 ? 0 : 1;
}
