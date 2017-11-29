/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libu/ffio/set_layer_options.c	92.1	06/29/99 13:16:47"



#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <ffio.h>
#include <string.h>
#include <liberrno.h>

#include "layer_def.h"
#include "spec_parse.h"

#define CONV_MULT 1
#define CONV_ERROR 2
#define CONV_DIVIDE 3

#define NUMERIC_OVERWRITE 'O'
#define NUMERIC_ADD       '+'
#define NUMERIC_SUB       '-'
#define NUMERIC_MULT      '*'
#define NUMERIC_DVD       '/'
#define NUMERIC_OR        '|'
#define NUMERIC_AND       '&'


#if (_MIPS_SZLONG == 32)
#define STRTOVAL strtoll
#else
#define STRTOVAL strtol
#endif

extern int _get_ffio_rt_defaults();

/*
 * Returns 0 if OK, -1 if error
 */
int
_str_to_int( 
char *token_inp,
numerictype *value,
int *units_flag )
{
int base ;
char valid_chars[16] = "0123456789abcdef" ;
int nc ;
int i, j ;
numerictype this_value ;
char token_store[80] ;
char *token ;
int new_units_flag ;

    token = token_store ;
    nc = strlen( token_inp ) ;
    strcpy( token, token_inp ) ;

    /* The following ensures that the user uses consistent units */
    /* It is an error to specify .pages_size=64b+512w */

    if(      token[nc-1] == 'w' || token[nc-1] == 'W' )
        new_units_flag = 'w' ;	/* words */
    else if( token[nc-1] == 'l' || token[nc-1] == 'l' )
        new_units_flag = 'l' ;	/* blocks (4096-bytes) */
    else if( token[nc-1] == 'y' || token[nc-1] == 'Y' )
        new_units_flag = 'y' ;	/* bytes */
    else if( token[nc-1] == 'n' || token[nc-1] == 'N' )
        new_units_flag = 'n' ;	/* none */
    else
        new_units_flag = 0 ;

    if( new_units_flag && *units_flag && ( new_units_flag != *units_flag ) ) {
        return(ERR);
    }
    else if( new_units_flag ) {
        *units_flag = new_units_flag ;
        token[--nc] = '\0' ;
    }

    if( token[nc-1] == 'k'  || token[nc-1] == 'K' ) {
        *value = 1024 ;
        token[--nc] = 0 ;
    }
    else if( token[nc-1] == 'm'  || token[nc-1] == 'M' )  {
        *value = 1048576 ;
        token[--nc] = 0 ;
    }
    else {
        *value = 1 ;
    }

    if( *token == '-' ) {
        /* negative sign */
        *value = *value * -1 ;
        token ++ ;
    } 
    else if( *token == '+' ){
        /* positive sign */
        token++ ;
    }

    if( *token == '0' && (*(token+1) == 'x' || *(token+1) == 'X') ) {
        /* base 16 */
        token += 2 ;
        base = 16 ;
    }
    else if( *token == '0' && (*(token+1) == 'o' || *(token+1) == 'O'))  {
        /* base 8  */
        token += 2;
        base = 8 ;
    }
    else if( *token == '@' ){
        /* base 2  */
        token++ ;
        base = 2 ;
    }
    else  {
        /* base 10   default */ 
        base = 10 ;
    }
  
    for(i=0;i<strlen(token);i++){
        for(j=0;j<base;j++) 
            if( token[i] == valid_chars[j] )
                break ;
        if( j==base )
            return(ERR) ;
    }

    this_value = STRTOVAL(token, NULL, base);
    *value = *value * this_value ;

    return(0) ;

}

static int
_get_numeric_value(
struct LAYER_NUMERICS *numeric_form ,
const char *token_inp,
numerictype *value ,
struct LAYER_ALIAS *alias ,
int num_alias ,
int level ,
int warnmode,
int errmode
)
{
    int status ;
    int  j ;
    numerictype this_value ;
    char *this_token=NULL ;
    const char *token_inp_start ;
    char token_store[80] ;
    int token_len ;
    int units_flag=0 ;
    int operation ;

    level++ ;
    token_inp_start = token_inp ;

    *value = 0 ;
    while(token_inp) {
        switch (*token_inp) {
            case NUMERIC_MULT :
            case NUMERIC_DVD  :
            case NUMERIC_ADD  :
            case NUMERIC_SUB  :
            case NUMERIC_OR   :
            case NUMERIC_AND  :
                operation = *token_inp ;
                token_inp++ ;
                break ;
            default :
                operation = NUMERIC_ADD ;
        }

        if( *token_inp == '(' ) {
            token_inp++ ;
            this_token = token_store ;
            token_len = _get_next_token( &token_inp, this_token, ")" , 
                1 , 1, 80, errmode ) ;
            if( token_len )  {
                token_inp++ ; /* skip past the ")" */
                status = _get_numeric_value( 
                    numeric_form, this_token, &this_value, alias,
                    num_alias , level, warnmode, errmode ) ;
                if( status == ERR )
                    return( ERR ) ;
            }
            else  /* did not find the matching ) */
                return(ERR) ;
            goto do_op ;
        }

        this_token = token_store ;
        token_len = _get_next_token( &token_inp, this_token, "*+-|&/" ,
            1 , 1, 80, errmode ) ;

        if( token_len ) {
            for(j=0;j<num_alias;j++) {
                if( strcmp( alias[j].name, this_token ) == 0 ) {
                    this_token = alias[j].numeric_string ;
                    break ;
                }
            }

            status = _str_to_int( this_token, &this_value , &units_flag ) ;
            if( status == ERR )
                return(ERR) ;

do_op :
            switch( operation )  {
                case NUMERIC_MULT :
                   *value = *value * this_value ;
                   break ;
                case NUMERIC_DVD :
                   if( this_value == 0 )
                       return(ERR) ;
                   *value = *value / this_value ;
                   break ;
                case NUMERIC_ADD :
                   *value = *value + this_value ;
                   break ;
                case NUMERIC_SUB :
                   *value = *value - this_value ;
                   break ;
                case NUMERIC_OR :
                   *value = *value | this_value ;
                   break ;
                case NUMERIC_AND :
                   *value = *value & this_value ;
                   break ;
             }
        }
        else {
            break ;
        }
    }
   
    if( status == ERR ) {
        *value = 0 ;
        if (errmode)
            _lerror(_LELVL_MSG, ERAS_BADSYNT , token_inp_start);
        return(ERR) ;
    }


    {                        /* UNIT CONVERSION */
        struct _UNIT_CONV {
            int mode ;
            int factor ;
        } ;
        struct _UNIT_CONV unit_conv[4][4] = {   /*[output][input] */

    /*         none             bytes                 words                block   */

/* none */ { {CONV_MULT , 1} , {CONV_ERROR  ,   1} , {CONV_ERROR ,   1} , {CONV_ERROR,   1} } ,
/* bytes*/ { {CONV_MULT , 1} , {CONV_MULT   ,   1} , {CONV_MULT  ,   8} , {CONV_MULT ,4096} } ,
/* words*/ { {CONV_MULT , 1} , {CONV_DIVIDE ,   8} , {CONV_MULT  ,   1} , {CONV_MULT , 512} } ,
/* block*/ { {CONV_MULT , 1} , {CONV_DIVIDE ,4096} , {CONV_DIVIDE, 512} , {CONV_MULT ,   1} } };

        int input_units, output_units ;
        int factor ;
        int mode ;

        if( units_flag == 0 || units_flag == 'n' )
            input_units = 0 ;
        else if( units_flag == 'y' )
            input_units = 1 ;
        else if( units_flag == 'w' )
            input_units = 2 ;
        else if( units_flag == 'l' )
            input_units = 3 ;

        switch( numeric_form->unit ) {
            case 'n' :         /* no units allowed */
            default :
                output_units = 0 ;
                break ;
            case 'y' : /* bytes */
                output_units = 1 ;
                break ;
            case 'w' : /* (8-byte) words */
                output_units = 2 ;
                break ;
            case 'b' :  /* old code for blocks */
            case 'l' :  /* new code for blocks */
                output_units = 3 ;
                break ;
        }

        factor = unit_conv[output_units][input_units].factor ;
        mode   = unit_conv[output_units][input_units].mode ;
        switch( mode ) {
            case CONV_MULT :
                *value = (*value)*factor ;
                break ;
            case CONV_DIVIDE :
                *value = ((*value)+(factor-1))/factor ;
                break ;
            case CONV_ERROR  :
                if (errmode)
                    _lerror(_LELVL_MSG, ERAS_TOKUNI , token_inp_start);
                return(ERR) ;
            default :
                if (errmode)
                    _lerror(_LELVL_MSG,FDC_ERR_INTERR);
                return(ERR) ;
        }
    }

/* check the min and max range */
    if( level == 1 ) {
        if ( numeric_form->check_min && (*value < numeric_form->min_value )) {
            if (errmode) {
                    _lerror(_LELVL_MSG, ERAS_TOOSMALL ,
                    *value,numeric_form->min_value,numeric_form->name);
            }
            return(ERR);
        }
        if( numeric_form->check_max && (*value > numeric_form->max_value)) {
            if (errmode) {
                    _lerror(_LELVL_MSG, ERAS_TOOBIG,
                    *value,numeric_form->max_value,numeric_form->name);
            }
            return(ERR);
        }
    }

    return(0) ;
}

static int
_parse_layer_string(
struct LAYER_DATA *table,
union spec_u *opt_flag, 
union spec_u *opt_ints,
const char *str,
int warnmode,
int errmode,
union spec_u *user_opt_flag) 
{
    char token[80] ;
    char area ;
    int token_len ;
    int i , j ;
    int nints ;
    numerictype value ;
    struct LAYER_OPTS  *opts ;
    struct LAYER_ALIAS *alias ;
    int nopts ;
    int  ret ;
    int  numeric_mode ;

    opts = table->opts ;
    alias = table->alias ;
    nopts = table->num_opts ;
    nints = 0 ;
    while( *str && ( *str == '.' || *str == ':' ) ) {
        area = *str ;
        str++ ;
        token_len = _get_next_token( &str , token , ".:,|=" , 1 , 1, 80, errmode ) ;
process_alias :

        numeric_mode = NUMERIC_OVERWRITE ;
        if( *str == '=' )  {
            /* We found a .name=value specification */
            if( area != '.' ) {
                /* Should not be :name= */
               if (errmode)
                   _lerror(_LELVL_MSG, ERAS_BADSYNT, token);
               return(ERR);
            }
            if(      token[token_len-1] == '+' )        /* name+=value */
                numeric_mode = NUMERIC_ADD ;
            else if( token[token_len-1] == '-' )        /* name-=value */
                numeric_mode = NUMERIC_SUB ;
            else if( token[token_len-1] == '*' )        /* name*=value */
                numeric_mode = NUMERIC_MULT ;
            else if( token[token_len-1] == '/' )        /* name/=value */
                numeric_mode = NUMERIC_DVD  ;
            else if( token[token_len-1] == '|' )        /* name|=value */
                numeric_mode = NUMERIC_OR ;
            else if( token[token_len-1] == '&' )        /* name&=value */
                numeric_mode = NUMERIC_AND ;
            if( numeric_mode != NUMERIC_OVERWRITE ) {
                /* strip the last char if name+ name- name* ... */
                token[--token_len] = 0 ;
            }
        }

        for(i=0;i<table->num_alias;i++) {
            /* look for alias */
            if( strcmp( token, alias[i].name ) == 0 ) {
                if ( alias[i].inuse ) {
                    if (errmode)
                        _lerror(_LELVL_MSG, FDC_ERR_INTERR);
                    return(ERR);
                }
                if( area == '.' ) {
                    alias[i].inuse ++ ;
                    if( ret = _parse_layer_string( table, opt_flag,
                        opt_ints, alias[i].option_string, warnmode,
                        errmode, user_opt_flag) == ERR )
                        return( ERR ) ;
                    alias[i].inuse -- ;
                    goto alias_break ;
                }
                else if( area == ':' ) {
                    strcpy( token, alias[i].numeric_string ) ;
                    goto process_alias ;
                }
            }
        }

        if( area == '.' ) {
            int match=0 ;
            for(i=0;i<table->num_numerics;i++) {
                if(strcmp( token, table->numerics[i].name ) == 0 ) {
                    numerictype old_value ;
                    match = 1 ;
                    token[0] = '\0' ;
                    for(j=0;j<i+1;j++)
                        strcat(token , ":" ) ;
                    str++ ;
                    token_len = _get_next_token( &str , token+(i+1) ,
                        ".:,|" , 1 , 1, 80 - (i+1), errmode ) ;
                    old_value = opt_ints[i].info.quan ;

                    if( ret = _parse_layer_string( table, opt_flag, 
                        opt_ints, token, warnmode, errmode, user_opt_flag )
                        == ERR )
                        return( ERR ) ;

                    if(     numeric_mode == NUMERIC_ADD )
                        opt_ints[i].info.quan += old_value ;
                    else if(numeric_mode == NUMERIC_DVD ) {
                        if( opt_ints[i].info.quan )
                            opt_ints[i].info.quan = 
                                old_value/opt_ints[i].info.quan ;
                        else {
                            if (errmode)
                                _lerror(_LELVL_MSG,ERAS_BADSYNT , token);
                            return(ERR) ;
                        }
                    }
                    else if(numeric_mode == NUMERIC_SUB )
                        /* - is different order */
                        opt_ints[i].info.quan =
                            old_value-opt_ints[i].info.quan ;
                    else if(numeric_mode == NUMERIC_MULT  )
                        opt_ints[i].info.quan *= old_value ;
                    else if(numeric_mode == NUMERIC_OR  )
                        opt_ints[i].info.quan = 
                            opt_ints[i].info.quan | old_value ;
                    else if(numeric_mode == NUMERIC_AND  )
                        opt_ints[i].info.quan &= old_value ;
                }
            }

            if( match == 0 ) {
                char *opt_name ;
                /* Wasn't an alias or a numeric */
                for(i=0;i<nopts;i++) {
                    if(      opts[i].name[0] == '#' ) /* deactivated option */
                        continue ;
                    else if( opts[i].name[0] == '.' ) /* hidden option */
                        opt_name = opts[i].name+1 ;
                    else
                        opt_name = opts[i].name ;
                    if( strcmp( token, opt_name ) == 0 ) {
			/* Was a conflicting option already specified? */
                        if (user_opt_flag) {
                            if ((user_opt_flag->class_info.info1 &
                                opt_flag->class_info.info1 &
                                opts[i].exclusive_mask1) ||
                                (user_opt_flag->class_info.info2 & 
                                opt_flag->class_info.info2 &
                                opts[i].exclusive_mask2)) {
                                if (errmode)
                                    _lerror(_LELVL_MSG,ERAS_CONFLICT, opt_name); 
                                return(ERR);
                                }
                            user_opt_flag->class_info.info1 = 
                                user_opt_flag->class_info.info1 &
                                ( ~opts[i].info1_mask ) | opts[i].info1_val;
                            user_opt_flag->class_info.info2 =
                                user_opt_flag->class_info.info2 & 
                                ( ~opts[i].info2_mask ) | opts[i].info2_val;
                        }
                        opt_flag->class_info.class = opts[i].class;
                        opt_flag->class_info.info1 =
                            opt_flag->class_info.info1 &
                            ( ~opts[i].info1_mask ) | opts[i].info1_val;
                        opt_flag->class_info.info2 = 
                            opt_flag->class_info.info2 &
                            ( ~opts[i].info2_mask ) | opts[i].info2_val;

                        match = 1 ;
                        break ;
                    }
                }
            }
            if( match == 0 ) {
                if (errmode)
                    _lerror(_LELVL_MSG,ERAS_BADOPT , token, table->name);
                    return(ERR) ;
            }
        }
        else if( area == ':' ) {
            if( token_len > 0 ) {
                if((nints >= table->num_numerics) && user_opt_flag ) {
                    if (warnmode)
                        _lwarn( WNAS_NUMERIC , table->num_numerics);
                }
                else if( _get_numeric_value( &table->numerics[nints], token,
                    &value , alias, table->num_alias , 0, warnmode,
                    errmode) == 0 ) {
                    /* The user actually specified it */
                    opt_ints[nints].info.quan    = value ;
                    SET_VALID_BIT(opt_ints[nints].info, 1);
                }
                else {
                    if (errmode)
                        _lerror(_LELVL_MSG, ERAS_BADSYNT , token);
                    return(ERR) ;
                }
            }
            nints++ ;
        }
alias_break :
        continue ;
    }
    return(0) ;
}

int
_set_layer_options(
char *layer_name_inp,
char *options,
union spec_u *specs,
int spec_level,
int limit,
int *num_added,
int warnmode,
int errmode)
{
int ret_value ;
int i, j, nc=0 ;
union spec_u *opt_flag ,*opt_ints ;
struct LAYER_DATA *table ;
char layer_char[16] ;
char *layer = layer_char;
union spec_u user_opt_flag;

    user_opt_flag.wword = 0;
    *num_added = 0 ;

    while( *layer_name_inp ) {
        if( *layer_name_inp != ' ' ) layer[nc++] = *layer_name_inp ;
        layer_name_inp++ ;
    }
    layer[nc] = 0 ;

    table = NULL ;
    for(i=0;i<_num_layer_tables;i++) {
        if( _ffio_parse_tables[i] == NULL )
            continue ;
        if((_ffio_parse_tables[i]->name != NULL) && 
	  (strcmp(_ffio_parse_tables[i]->name,layer) == 0 )) {
            table = _ffio_parse_tables[i] ;
            break ;
        }
    }

    if( !table ) {
        if (errmode)
            _lerror(_LELVL_MSG,ERAS_BADCLASS,layer);
        ret_value = -1 ;
        goto all_exit_here ;
    }

    opt_flag = specs ;
    opt_ints = specs + table->num_flag_words ;

    if ((*num_added + table->num_flag_words + table->num_numerics) > limit) {
        if (errmode)
            _lerror(_LELVL_MSG,ERAS_TOOLAY );
        return(ERR);
    }
    for(i=0;i<table->num_flag_words;i++) {
        opt_flag[i].wword      = 0 ;
        opt_flag[i].info.ext   = 1 ;
        opt_flag[i].info.class = table->class_num ;
    }

    if( spec_level == 0 ) 
        SET_VALID_BIT(opt_flag[0].info,0) ; /* 0 in first word only */

    for(i=0;i<table->num_numerics;i++) {
        opt_ints[i].wword      = table->numerics[i].def_value ;
        opt_ints[i].info.ext   = 1 ;
        opt_ints[i].info.class = table->class_num ;
    }
    *num_added = table->num_flag_words + table->num_numerics ;
    if( *num_added  > 0 ) {
        opt_flag[(*num_added)-1].info.ext = 0 ;
    }

    if(  _parse_layer_string( table, opt_flag, opt_ints, 
        table->hard_coded_defaults, warnmode, errmode, NULL ) == ERR ) {
            ret_value = -1 ; 
            goto all_exit_here ;
    }
    {
	/* Applications has their own _get_ffio_rt_defaults routine. */
	/* This allows them to accept environment variables that */
	/* mean change the default for a given layer */
	char rt_defaults[256];
	int nc;
	nc = _get_ffio_rt_defaults(table->name, rt_defaults, 256);
	if (nc) {
		if (_parse_layer_string( table, opt_flag, opt_ints,
			rt_defaults, warnmode, errmode, NULL) == ERR) {
			ret_value = -1;;
			goto all_exit_here;
		}
	}
    }
    if( _parse_layer_string(table,  opt_flag, opt_ints, 
        options, warnmode, errmode, &user_opt_flag ) == ERR ) {
        ret_value = -1 ;
        goto all_exit_here ; 
    }

    if ((( table->non_zero_mask1 ) &&
        (( opt_flag->class_info.info1 & table->non_zero_mask1 ) == 0 )) ||
        (( table->non_zero_mask2 ) &&
        (( opt_flag->class_info.info2 & table->non_zero_mask2 ) == 0 ))) {
        if (errmode)
            _lerror(_LELVL_MSG,ERAS_OPTREQ , table->name);
        ret_value = -1 ;
        goto all_exit_here ;
    }

    for(j=0;j<table->num_opts;j++) {
        if( (opt_flag->class_info.class != table->opts[j].class )
	    || (opt_flag->class_info.info1 & table->opts[j].info1_mask !=
            table->opts[j].info1_val) ||
	    (opt_flag->class_info.info2 & table->opts[j].info2_mask !=
            table->opts[j].info2_val))
            continue;	/* option is not on */
        if( table->opts[j].num_mask == 0 )
            continue ;  /* this opt does not have a no-default numeric */
        for(i=0;i<table->num_numerics;i++) {
            if( GET_VALID_BIT(opt_ints[i].info) == 0 ) {
                if( (table->opts[j].num_mask >> i)  & 1 )  {
                    if (errmode)
                        _lerror(_LELVL_MSG, ERAS_NUMREQ ,
                        table->numerics[i].name,table->opts[j].name,
                        table->name);
                    ret_value = -1 ;
                    goto all_exit_here ; 
                }
            }
        }
    }
    /* If a routine was specified to check the total contents of the */
    /* specification, call it */
    if( table->chkrtn != NULL) {
        if (table->chkrtn(specs, table, *num_added, warnmode, errmode) == -1)
            return(ERR);
    }

/**************  BACKFIT  *******************/

    if ( table->type == FSS_TYPE ) {
        if ( table->num_numerics > 0 ) {
            opt_flag[0].fss.min     = opt_flag[1].info.quan ;
            for(i=1;i<table->num_numerics;i++) {
                opt_flag[i].info.quan   = opt_flag[i+1].info.quan ;
                SET_VALID_BIT(opt_flag[i].info,0) ;
            }
        }
        opt_flag[table->num_numerics-1].info.ext   = 0 ;
        *num_added = *num_added - 1 ; ;
    }

    if( table->type == FLD_TYPE || table->type == FLD_EXT_TYPE ) {
        if( table->num_numerics > 0 ) {
            int discard ;
            discard = 0 ;
            for(i=0;i<table->num_numerics;i++) {
                if( GET_VALID_BIT(opt_ints[i].info) )  {
                    discard = i+1 ;
                }
            }
            /* put first numeric in recsize */
            opt_flag[0].fld.recsize  = opt_flag[1].info.quan ;  
            if( table->num_numerics > 1 ) {
                /* put second numeric in mbs */
                opt_flag[0].fld.mbs  = opt_flag[2].info.quan ;
            }
            *num_added = discard+1;
            opt_flag[discard].info.ext = 0 ;
        }
    }

/************* END OF BACK FIT ************************/

    ret_value = 0 ;
all_exit_here :
    return(ret_value) ;
}

/*
 * Get the next token. Returns the length of the token.
 */
int
_get_next_token( 
const char **str_inp , 
char *token , 
char *separators , 
int strip_spaces , 
int lower_case ,
int maxlen,
int errmode )
{
int nc ;
const char *str ;
char *sep ;
int sqrbr_count ;
int quoted_string ;

    nc = 0 ;
    *token = 0 ;
    str = *str_inp ;
    quoted_string = 0 ;

    sqrbr_count = 0 ;
    if(*separators == ')') sqrbr_count =  1 ;
    if(*separators == '(') sqrbr_count = -1 ;

     while( *str ) {
        if( *str == '\n' ) {
            str++ ;
            continue ; 
        }
        else if(*str == '(' ) 
            sqrbr_count++ ;
        else if(*str == ')' ) 
            sqrbr_count-- ;
        else if(*str == '"' ) 
            quoted_string = (++quoted_string)&1 ;

        if(sqrbr_count<=0) {
            /* only check for separator if all braces are paired */
            for(sep=separators;*sep;sep++) {
                if( *sep==*str ) {
                    if( *str == '|' && *(str+1) == '=' )
                        break ;  /* special case |= */
                    if( *sep == ' ' && nc == 0 )
                        break ; /* skip any leading blanks */
                    goto out ;
                }
            }
        }

        if( *str == ' ' && !quoted_string ) {
            if( nc == 0 || strip_spaces ) {
                str++ ;
                continue ;
            }
        }
        *(token+nc) = *str ;
        if( lower_case && ( token[nc] > 64 ) && ( token[nc] < 91 ) ) 
            token[nc] += 32 ;
        nc++ ;
        if (nc > maxlen) {
            if (errmode)
               _lerror(_LELVL_MSG, ERAS_PSTRING, token);
            return(0);
        }
        str++ ;
        *(token+nc) = 0 ;
    }
out :

    if( sqrbr_count != 0 ) {
        if (errmode)
            _lerror(_LELVL_MSG, ERAS_BADSYNT , token);
        *str_inp     = str   ;
        *(token) = '\0' ;
        return(0) ;
    }

    *(token+nc) = 0 ;
    while( nc && (token[nc-1]== ' ') )
        token[--nc] = 0 ; /* strip trailing blanks from output */
    while(*str==' ')
        str++ ;           /* strip leading blanks from input */
    *str_inp   = str   ;
    return(nc) ;
}

