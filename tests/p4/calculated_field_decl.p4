// calculated_field_decl.p4

calculated_field h1.f1 {
  update flcn1;
  verify flcn1 if (valid(h1));
  verify flcn1 if (valid(h1[0]));
  verify flcn1 if (valid(h1.f2));
  update flcn1 if (h1[last].f3 == 0x1010);
}
