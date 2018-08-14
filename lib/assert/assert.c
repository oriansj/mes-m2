void
assert_fail (char* s)
{
  eputs ("assert fail: ");
  eputs (s);
  eputs ("\n");
  char *fail = s;
  fail = 0;
  *fail = 0;
}
