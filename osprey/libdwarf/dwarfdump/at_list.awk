# print code to return attribute name from list of attrs in dwarf.h (the input)
# In case of a duplicate value, accept the first as definitive.
# dwarf2 had a couple ambiguities/mistakes in attribute spelling.
BEGIN {
	printf "static int list_of_attrs[] = {\n"
	used_pref["0"] = "";
}
{
	prefix = "DW_AT_"
	prefix_len = length(prefix)
	if ($1 == "#define" && substr($2,1,prefix_len) == prefix) {
		if ( used_pref[ $3] != $3 )  {
		   printf "\t%s,\n", $2
		   used_pref [$3] = $3 ;
	        }
	}
}
END {
	printf "\t0\n"		# last value
	printf "};\n"
}

