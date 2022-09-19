SET(WHIRL2C_CXX_SRCS 
  init2c.cxx            
  PUinfo.cxx            
  st2c.cxx              
  stab_attr.cxx         
  tcon2c.cxx            
  token_buffer.cxx      
  ty2c.cxx              
  w2c_driver.cxx        
  w2cf_parentize.cxx    
  w2cf_symtab.cxx       
  whirl2c_common.cxx    
  wn_attr.cxx           
  wn2c.cxx              
  wn2c_pragma.cxx       
  diagnostics.cxx)

SET(WHIRL2C_DRIVER_SRCS
	whirl2c.c)
SET(WHIRL2C_MAIN_SRCS
	whirl2c_main.cxx)

foreach(ONE_C_FILE IN LISTS WHIRL2C_CXX_SRCS)
  list(APPEND W2C_SRC ${BE_WHIRL2C_DIR}/${ONE_C_FILE})
endforeach()

foreach(ONE_C_FILE IN LISTS WHIRL2C_DRIVER_SRCS)
  list(APPEND W2C_SRC ${BE_WHIRL2C_DIR}/${ONE_C_FILE})
endforeach()

foreach(ONE_C_FILE IN LISTS WHIRL2C_MAIN_SRCS)
  list(APPEND W2C_SRC ${BE_WHIRL2C_DIR}/${ONE_C_FILE})
endforeach()


