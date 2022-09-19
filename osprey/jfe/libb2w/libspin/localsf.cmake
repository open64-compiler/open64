

SET(LIBSPIN_SOURCES
  gspin-tree.c 
  gspin-assert.c 
  gspin-alloc.c 
  gspin-mempool.c 
  gspin-io.c 
  gspin-list.c 
  gspin-tel.c
)

SET(LIBSPIN_HEADERS
  gspin-tree.h 
  gspin-assert.h 
  gspin-base-types.h 
  gspin-alloc.h 
  gspin-mempool.h 
  gspin-io.h 
  gspin-list.h 
  gspin-tel.h
)

foreach(ONE_C_FILE IN LISTS LIBSPIN_SOURCES)
  list(APPEND LIBSPIN_SRC ${LIBSPIN_DIR}/${ONE_C_FILE})
endforeach()


foreach(ONE_C_FILE IN LISTS LIBSPIN_HEADERS)
  list(APPEND LIBSPIN_SRC ${LIBSPIN_DIR}/${ONE_C_FILE})
endforeach()


