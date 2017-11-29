#
#  Copyright (C) 2009, 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU Lesser General Public License as published
#  by the Free Software Foundation; either version 2.1 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with the Open64 Runtime Library; if not, write to the
#  Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
#  02111-1307 USA.
#

#
# extract.awk: generate C source for default error messages
#
# INPUT:  preprocessed source file of message definitions
# OUTPUT: C code definition of libu_defgets, a function that translates a
#         message identifier into its corresponding raw message string.
BEGIN {
    count = 0;
    printf "#include <stdio.h>\n";
    printf "\n"
    printf "static struct libu_msg_rec {\n";
    printf "\tint msg_id;\n";
    printf "\tchar *msg;\n";
    printf "} libu_default_msgs[] = {\n";
}
/^\$msg ([0-9]+) (.+)$/ {
    if (count > 0) {
        printf ",\n";
    }
    count++;

    # the message number is the text between the first and second spaces
    sp1 = index($0, " ")
    sp2 = sp1 + index(substr($0, sp1 + 1), " ")
    num = substr($0, sp1 + 1, sp2 - sp1 - 1)

    # the message is the remainder following the second space in the line
    msg = substr($0, sp2 + 1)

    # escape double-quotes in message text
    gsub(/"/, "\\\"", msg)

    printf "\t{ %s, \"%s\" }", num, msg
}
END {
    printf "\n};\n";
    printf "\n"
    printf "/*\n"
    printf " *\tlibu_defgets - return raw default error message text or NULL if none\n"
    printf " *\n"
    printf " *\t\tmsg_num\t\tMessage number\n"
    printf " */\n"
    printf "char *\n"
    printf "libu_defgets(int msg_num)\n"
    printf "{\n"
    printf "\tint i;\n"
    printf "\n"
    printf "\tfor (i = 0; i < %d; i++)\n", count
    printf "\t\tif (msg_num == libu_default_msgs[i].msg_id)\n"
    printf "\t\t\treturn libu_default_msgs[i].msg;\n"
    printf "\n"
    printf "\treturn NULL;\n"
    printf "}\n\n"
}
