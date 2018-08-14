
int eputs (char const *);
struct function {
#if __M2_PLANET__
  FUNCTION *function;
#else
  int (*function) (void);
#endif
  int arity;
  char *name;
};

struct function fun_make_cell_;

void
make_cell_ ()
{
}

int
main ()
{
  eputs ("00\n");
#if __M2_PLANET__
  fun_make_cell_->function = make_cell_;
#else
  fun_make_cell_.function = make_cell_;
#endif
  eputs ("01\n");
}
