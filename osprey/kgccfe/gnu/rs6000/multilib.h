static const char *const multilib_raw[] = {
". !msoft-float;",
"nof msoft-float;",
NULL
};

static const char *const multilib_matches_raw[] = {
"mcpu=401 msoft-float;",
"mcpu=403 msoft-float;",
"mcpu=405 msoft-float;",
"mcpu=ec603e msoft-float;",
"mcpu=801 msoft-float;",
"mcpu=821 msoft-float;",
"mcpu=823 msoft-float;",
"mcpu=860 msoft-float;",
"msoft-float msoft-float;",
NULL
};

static const char *multilib_extra = "fPIC mstrict-align";

static const char *const multilib_exclusions_raw[] = {
NULL
};

static const char *multilib_options = "msoft-float";
