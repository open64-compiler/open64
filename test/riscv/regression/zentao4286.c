struct {
  int fld;
  int operand_loc[]
} recog_data;
insn_extract() {
  int **ro_loc = recog_data.operand_loc;
  ro2(recog_data.operand_loc[2]);
  ro2(ro_loc[2]);
}

