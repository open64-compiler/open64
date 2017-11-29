# Print routines to return constant name for associated value.
# The input is dwarf.h
# For each set of names with a common prefix, we create a routine
# to return the name given the value.
# Also print header file that gives prototypes of routines.
# To handle cases where there are multiple names for a single
# value (DW_AT_* has some due to ambiguities in the DWARF2 spec)
# we take the first of a given value as the definitive name.
# TAGs, Attributes, etc are given distinct checks.
BEGIN {
	prefix = "foo"
	prefix_id = "foo"
	prefix_len = length(prefix)
	dw_prefix = "DW_"
	dw_len = length(dw_prefix)
	start_routine = 0
	printf "#include \"globals.h\"\n\n"
	printf "#include \"makename.h\"\n\n"
	header = "dwarf_names.h"
	printf "/* automatically generated routines */\n" > header
	dup_arr["0"] = ""
}
{
	# want to emit #if defined(TARG_*) same as source,
	# but ignore #ifdef DWARF_H markers.
	# Warning:  this requires #if defined() not #ifdef,
	# also doesn't work for nested #if
	if ($1 == "#if") {
		inif = 1
		printf "%s\n", $0
		next
	}
	if ($1 == "#endif" && inif) {
		inif = 0
		printf "%s\n", $0
		next
	}
	if ($1 == "#define") {
		if (substr($2,1,prefix_len) != prefix) {
			# new prefix
			if (substr($2,1,dw_len) != dw_prefix) {
				# skip
				next
			} else if (substr($2,1,dw_len+3) == "DW_CFA") {
				# skip, cause numbers conflict
				# (have both high-order and low-order bits)
				next
			} else {
				# New prefix, empty the dup_arr
				for (k in dup_arr)
					dup_arr[k] = ""
				if (start_routine) {
					# end routine
					printf "\tdefault:\n"
printf "\t\t{ \n"
printf "\t\t    char buf[100]; \n"
printf "\t\t    char *n; \n"
printf "\t\t    sprintf(buf,\"<Unknown %s value 0x%%x>\",(int)val);\n",prefix_id
printf "\t\t fprintf(stderr,\"%s of %%d (0x%%x) is unknown to dwarfdump. \" \n ", prefix_id
printf "\t\t \"Continuing. \\n\",(int)val,(int)val );  \n"
printf "\t\t    n = makename(buf);\n"
printf "\t\t    return n; \n"
printf "\t\t} \n"
					printf "\t}\n"
					printf "/*NOTREACHED*/\n"
					printf "}\n\n"
				}
				start_routine = 1
				post_dw = substr($2,dw_len+1, length($2))
				second_underscore = index(post_dw,"_")
				prefix = substr($2,1,second_underscore+dw_len)
				prefix_len = length(prefix)
				# prefix id is unique part after DW_, e.g. LANG
				prefix_id = substr(prefix,dw_len+1,prefix_len-dw_len-1)
				printf "/* ARGSUSED */\n"
				printf "extern string\n"
				printf "get_%s_name (Dwarf_Debug dbg, Dwarf_Half val)\n", prefix_id
				printf "{\n"
				printf "\tswitch (val) {\n"
				printf "extern string get_%s_name (Dwarf_Debug dbg, Dwarf_Half val);\n\n", prefix_id >> header
			}
		}
		if (substr($2,1,prefix_len) == prefix) {
			if (substr($2,1,dw_len+8) == "DW_CHILDREN" \
			    || substr($2,1,dw_len+8) == "DW_children" \
			    || substr($2,1,dw_len+4) == "DW_ADDR") {
				main_part = substr($2,dw_len+1, length($2))
			}
			else {
				post_dw = substr($2,dw_len+1, length($2))
				second_underscore = index(post_dw,"_")
				main_part = substr($2,dw_len+second_underscore+1, length($2))
			}
			if( dup_arr[$3] != $3 ) {
			  # Take first of those with identical value,
			  # ignore others.
			  dup_arr[$3] = $3
			  printf "\tcase %s:\n", $2
			  printf "\t\tif (ellipsis)\n"
			  printf "\t\t\treturn \"%s\";\n", main_part
			  printf "\t\telse\n"
			  printf "\t\t\treturn \"%s\";\n", $2
		        }
		}
	}
}
END {
	if (start_routine) {
		printf "\tdefault:\n"
		printf "\t\tprint_error(dbg, \"get_%s_name unexpected value\",DW_DLV_OK, err);\n", prefix_id
		printf "\t}\n"
		printf "\t return \"unknown-name-dwarf-error\";\n"
		printf "}\n\n"
	}
}

