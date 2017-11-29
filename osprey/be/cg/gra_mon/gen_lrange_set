#!/BIN/CSH -F
### ====================================================================
### ====================================================================
###
### MODULE: GEN_TN_SET
### $REVISION: 3.1 $
### $DATE: 1993/08/21 02:12:44 $
### $AUTHOR: WSJ $
### $SOURCE: /NET/SAHARA_152.MTI/CMPLRS.SRC/V7.00/BE/CG/RCS/GEN_TN_SET,V $
### REVISION HISTORY:
###   5-MAY-92 - ORIGINAL VERSION
###
### USAGE:      GEN_TN_SET UTILDIR OPTS
###
###     GENERATE THE TN_SET.[CH] MODULE.  'UTILDIR' IS WHERE TO FIND
###     THE GEN_X_SET STUFF. 'OPTS' IS PASSED AS TEH FIRST ARGUMENT
###     TO GEN_X_SET TO ALLOW SPECIFICATION OF FUNCTIONAL OR MACRO
###     INTERFACES.
###
###     WE DO THIS IN A FILE SO THE MAKE RULE CAN DEPEND ON IT AND
###     THE MODULES CAN BE REBUILT WHEN THE PROCEDURE CHANGES
###
### ====================================================================
### ====================================================================


perl $1/gen_x_set $2 -s lrange_set 'LRANGE*' LRANGE_SET                 \
                   LRANGE_INT INT_LRANGE                                \
                   LRANGE_Universe_ID_S INT_LRANGE_Sub                  \
                   'defs.h'                                             \
                   'gra_bb.h'                                       	\
                   'gra_lrange.h'                                       \
                   'gra_lrange_subuniverse.h'                           \
                   'mempool.h'
