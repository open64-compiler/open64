struct timevar_def {
  unsigned used : 1
} timevars[];
timevar_push() {
  struct timevar_def *tv = &timevars[0];
  tv->used = 0;
}
enum { TV_INTEGRATION } expand_call() { timevar_push(TV_INTEGRATION); }
expand_expr() { expand_call(); }
ix86_expand_builtin() { expand_expr(); }
*x86_64_reg_class_name = ix86_expand_builtin;
