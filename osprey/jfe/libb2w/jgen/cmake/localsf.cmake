SET(JGEN_SRCS 
  json2whirl.cxx
  json_reader.cxx
  json_ir_decl.cxx
        # JGEN GLobals
  jgen_global.cpp
  jgen_visitor.cpp
  jgen_node.cpp
        #JGEN Type/ST/Decl
  jgen_type.cpp
  jgen_st.cpp
  json_symboltree.cxx
  json_typetree.cxx

#wgen_decl.cxx
)

SET(JREAD_SRCS 
  json_reader.cxx
  test.cxx
  jgen_global.cpp
  jgen_node.cpp
  jgen_visitor.cpp
  json_ir_decl.cxx
  jgen_type.cpp
  jgen_st.cpp
  json_symboltree.cxx
  json_typetree.cxx
#wgen_decl.cxx
)

SET(JSON_SRCS
  jsoncpp.cpp)

SET(FE_C_SRCS              
#    main.c         	
    varasm.c		
    c_int_model.c       
)

SET(FE_CXX_SRCS            
#    wgen_misc.cxx       
#    wgen_expr.cxx       
#    wgen_stmt.cxx       
#    wgen_decl.cxx
#    wgen_dst.cxx        
#    wgen_spin_symbol.cxx 
#    wgen_tracing.cxx
)
  
SET(OPENMP_SRCS            
#    omp_directive.cxx   
#    wgen_omp_check_stack.cxx 
#    wgen_omp_directives.cxx
)
  
SET(COMMON_COM_CXX_SRCS    
   config.cxx           
   const.cxx            
   controls.cxx         
   dwarf_DST.cxx                
   dwarf_DST_dump.cxx   
   dwarf_DST_mem.cxx    
   dwarf_DST_producer.cxx 
   err_host.cxx         
   glob.cxx             
   intrn_info.cxx       
   ir_bcom.cxx          
   ir_bwrite.cxx                
   ir_reader.cxx                
   irbdata.cxx          
   mtypes.cxx           
   opcode.cxx           
   opcode_core.cxx      
   pu_info.cxx          
   strtab.cxx           
   symtab.cxx           
   symtab_verify.cxx    
   ttype.cxx            
   wn.cxx               
   wn_map.cxx           
   wn_pragmas.cxx       
   wn_simp.cxx          
#   wn_util.cxx          
   wutil.cxx            
   xstats.cxx
)

set(WGENUTIL_O_CXX_SRC
  wn_util.cxx)

SET(COMMON_COM_TARG_SRCS   
   config_host.c                
   config_platform.c
)

SET(COMMON_COM_TARG_CXX_SRCS  
   config_targ.cxx      
   config_elf_targ.cxx  
   targ_const.cxx       
   targ_sim.cxx
 )

set(BE_COM_SRC
  wssa_mgr.cxx
  )


foreach(ONE_C_FILE IN LISTS FE_C_SRCS )
  list(APPEND COMMON_WN_FUNCS ${WGEN_DIR}/${ONE_C_FILE})
endforeach()

foreach(ONE_C_FILE IN LISTS FE_CXX_SRCS )
  list(APPEND COMMON_WN_FUNCS ${WGEN_DIR}/${ONE_C_FILE})
endforeach()

foreach(ONE_C_FILE IN LISTS OPENMP_SRCS )
  list(APPEND COMMON_WN_FUNCS ${WGEN_DIR}/${ONE_C_FILE})
endforeach()

foreach(ONE_C_FILE IN LISTS COMMON_COM_CXX_SRCS )
  list(APPEND COMMON_WN_FUNCS ${COMMON_COM_DIR}/${ONE_C_FILE})
endforeach()

foreach(ONE_C_FILE IN LISTS COMMON_COM_TARG_SRCS )
  list(APPEND COMMON_WN_FUNCS ${COMMON_COM_DIR}/${BUILD_TARGET_PREFIX}/${ONE_C_FILE})
endforeach()

foreach(ONE_C_FILE IN LISTS COMMON_COM_TARG_CXX_SRCS )
  list(APPEND COMMON_WN_FUNCS ${COMMON_COM_DIR}/${BUILD_TARGET_PREFIX}/${ONE_C_FILE})
endforeach()


foreach(ONE_C_FILE IN LISTS WGENUTIL_O_CXX_SRC)
  list(APPEND WGENUTILO_SRC ${COMMON_COM_DIR}/${ONE_C_FILE})
endforeach()  

foreach(ONE_C_FILE IN LISTS JGEN_SRCS)
  list(APPEND J2WGEN_SRC ${J2WGEN_SRC_DIR}/${ONE_C_FILE})
endforeach()  

foreach(ONE_C_FILE IN LISTS JREAD_SRCS)
  list(APPEND JREAD_SRC ${J2WGEN_SRC_DIR}/${ONE_C_FILE})
endforeach()  

foreach(ONE_C_FILE IN LISTS JSON_SRCS)
  list(APPEND JREAD_SRC ${JSON_SRC_DIR}/${ONE_C_FILE})
  list(APPEND J2WGEN_SRC ${JSON_SRC_DIR}/${ONE_C_FILE})
endforeach()

