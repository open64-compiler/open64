BEGIN		{ print "$set 1" }
/^\$msg[ \t]/	{ print substr($0, index($0, $2)); }
