set(COMMON_COM_TARG_SRC )

# CROSS BUILD
set(COM_UTILS_TARG_SRC
  c_qwmultu.c
)

set(COM_UTILS_H_SRC
  #libcomutil.h 
  bitset.h 
  errors.h 
  file_util.h 
  flags.h 
  linklist.h 
  mempool.h 
  cxx_memory.h 
  quad.h 
  quadsim.h 
  priority_queue.h 
  resource.h 
  mstack.h 
  tracing.h 
  util.h 
  vcg.h
  vstring.h
)

set(COM_UTILS_C_SRC
  bitset.c 
  file_util.c 
  flags.c 
  linklist.c 
  memory.c 
  priority_queue.c 
  resource.c 
  mstack.c 
  tracing.c 
  util.c 
  vstring.c
  c_a_to_q.c 
  c_q_add.c 
  c_q_div.c 
  c_q_mul.c 
  c_q_neg.c 
  c_q_rel.c 
  c_q_sqrt.c 
  c_q_sub.c 
  c_q_to_a.c 
  c_qtenscale.c 
  quadsim.c 
  )

set(COM_UTILS_CXX_SRC
  cxx_memory.cxx
  errors.cxx 
  vcg.cxx
  )

foreach(ONE_C_FILE IN LISTS COM_UTILS_TARG_SRC)
  get_osprey_file_path(${ONE_C_FILE} ${COMMON_UTIL_DIR}/x8664 COM_UTILS_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS COM_UTILS_C_SRC)
  get_osprey_file_path(${ONE_C_FILE} ${COMMON_UTIL_DIR} COM_UTILS_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS COM_UTILS_CXX_SRC)
  get_osprey_file_path(${ONE_C_FILE} ${COMMON_UTIL_DIR} "COM_UTILS_SRCS")
endforeach()

foreach(ONE_C_FILE IN LISTS COM_UTILS_H_SRC)
  get_osprey_file_path(${ONE_C_FILE} ${COMMON_UTIL_DIR} "COM_UTILS_SRCS")
endforeach()