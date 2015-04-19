// header_type_decl.p4

header_type h1 {
  fields {
    f1 : 8;
  }
}

header_type h2 {
  fields {
    f1: 1 (signed);
    f2: 1 ( saturating , signed ) ;
  }
}

header_type len_t {
  fields { f1 : *; }
  length: f1 + (1 << 8);
}

header_type ml_t {
  fields { f1 : *; }
  max_length: 0x10;
}
