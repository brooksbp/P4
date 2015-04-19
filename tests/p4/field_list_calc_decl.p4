// field_list_calc_decl.p4

field_list_calculation flc1 {
  input {
    fl1;
    fl2;
  }
  algorithm: crc32;
  output_width: 0xFFFF;
}
