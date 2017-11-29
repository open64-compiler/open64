readelf -S $1 | gawk "{ if (\$9~/AX/) { printf \"0x%s\n\",\$7; } }" | gawk "BEGIN { sum = 0; } { sum = sum + strtonum(\$1); } END { printf \"code size = %d 0x%x\n\",sum,sum;}"
